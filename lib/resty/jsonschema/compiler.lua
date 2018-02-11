-- Copyright (C) Jinzheng Zhang (tianchaijz).

local cjson = require "cjson.safe"

local type = type
local next = next
local pairs = pairs
local ipairs = ipairs
local tostring = tostring
local floor = math.floor
local string_rep = string.rep
local string_match = string.match
local string_format = string.format
local concat = table.concat
local table_remove = table.remove
local loadstring = loadstring
local setmetatable = setmetatable

local ngx = ngx
local re_find = ngx.re.find


local function is_str(obj) return type(obj) == "string" end
local function is_num(obj) return type(obj) == "number" end
local function is_tbl(obj) return type(obj) == "table" end
local function is_bool(obj) return type(obj) == "boolean" end
local function is_nil(obj) return obj == nil end
local function is_null(obj) return obj == cjson.null end
local function is_int(obj) return is_num(obj) and floor(obj) == obj end

local function table_type(obj)
    local max = 0
    local count = 0
    for k in pairs(obj) do
        if is_num(k) then
            if k > max then max = k end
            count = count + 1
        else
            return -1
        end
    end

    if max > count * 2 then
        return -1
    end

    return max
end

local function is_array(obj) return is_tbl(obj) and table_type(obj) >= 0 end
local function is_object(obj) return is_tbl(obj) and table_type(obj) <= 0 end


local Variable = {}
Variable.__index = Variable

Variable.__tostring = function(self)
    return self._alias or self._name
end

Variable.__call = function(self)
    return self._name
end


function Variable.new(name, alias, attributes)
    local var = { _name = name, _alias = alias }
    if attributes then
        for k, v in pairs(attributes) do
            var[k] = v
        end
    end
    return setmetatable(var, Variable)
end


function Variable.set_alias(self, alias)
    self._alias = alias
end


function Variable.len(self)
    return self.length or "#" .. self()
end


local Dump = {}


function Dump.value(v)
    if is_str(v) then
        return string_format("%q", v)
    elseif is_null(v) then
        return "cjson.null"
    elseif is_tbl(v) then
        return Dump.tostring(v)
    end

    return tostring(v)
end


function Dump.key(k)
    if is_str(k) and string_match(k, "^[_%a][_%a%d]*$") then
        return k
    end

    return "[" .. Dump.value(k) .. "]"
end


-- http://lua-users.org/wiki/TableUtils
function Dump.tostring(obj)
    if not is_tbl(obj) then
        return Dump.value(obj)
    end

    local result, done = {}, {}
    for k, v in ipairs(obj) do
        result[k] = Dump.value(v)
        done[k] = true
    end

    for k, v in pairs(obj) do
        if not done[k] then
            result[#result + 1] = Dump.key(k) .. "=" .. Dump.value(v)
        end
    end

    return "{" .. concat(result, ", ") .. "}"
end


local dump = Dump.tostring


local function table_len(t)
    local count = 0
    for _ in pairs(t) do
        count = count + 1
    end
    return count
end


local function equal(x, y)
    if type(x) ~= type(y) then
        return false
    end

    if is_tbl(x) then
        local keys = {}
        for k, v in pairs(x) do
            if y[k] == nil or not equal(v, y[k]) then
                return false
            end
            keys[k] = true
        end
        for k in pairs(y) do
            if not keys[k] then
                return false
            end
        end
        return true
    end

    return x == y
end


local type_order = {
    number   = 1,
    boolean  = 2,
    string   = 3,
    table    = 4,
    userdata = 5,
}


local function serialize(obj)
    return type_order[type(obj)] .. ":" .. dump(obj)
end


local _M = {
    _VERSION = "0.01",
}
local mt = { __index = _M }

local mapping = {
    { "type", "type" },
    { "enum", "enum" },
    { "allOf", "all_of" },
    { "anyOf", "any_of" },
    { "oneOf", "one_of" },
    { "not", "not" },
    {
        {
            { "minLength", "min_length" },
            { "maxLength", "max_length" },
            { "pattern", "pattern" },
        },
        "string"
    },
    {
        {
            { "minimum", "minimum" }, -- gt, ge, lt, le
            { "maximum", "maximum" },
            { "multipleOf", "multiple_of" },
        },
        "number"
    },
    {
        {
            { "items", "items" },
            { "minItems", "min_items" },
            { "maxItems", "max_items" },
            { "uniqueItems", "unique_items" },
        },
        "array"
    },
    {
        {
            { "properties", "properties" },
            { "required", "required" },
            { "minProperties", "min_properties" },
            { "maxProperties", "max_properties" },
            { "dependencies", "dependencies" },
        },
        "object"
    },
    { "validator", "validator" },
}

local Base = {
    is_nil = is_nil,
    is_null = is_null,
    is_string = is_str,
    is_number = is_num,
    is_integer = is_int,
    is_boolean = is_bool,
    is_table = is_tbl,
    is_array = is_array,
    is_object = is_object,
    equal = equal,
    serialize = serialize,
    re_find = re_find,
    table_len = table_len,
}

_M.Base = Base


local function _stmts(...) return concat({ ... }, "; ") end
local function _and(...) return concat({ ... }, " and ") end
local function _assign(lhs, rhs) return lhs .. " = " .. rhs end
local function _for(expr, ...) return "for " .. concat({ ... }, ", ") .. " in " .. expr .. " do" end
local function _for_assign(var, ...) return "for " .. var .. " = " .. concat({ ... }, ", ") .. " do" end
local function _call(func, ...) return func .. "(" .. concat({ ... }, ", ") .. ")" end
local function _list(l) return "{" .. concat(l, ", ") .. "}" end
local function _if(expr) return "if " .. expr .. " then" end
local function _not(expr) return "not " .. expr end
local function _op(lhs, op, rhs) return lhs .. " " .. op .. " " .. rhs end
local function _len(obj) return "#" .. obj end
local function _index(obj, k) return obj .. "[" .. k .. "]" end


function _M.new(schema, lib, name)
    name = name or "data"
    local m = {
        _indent = 0,
        _code = {},
        _lib = lib,
        _var = Variable.new(name),
        _name = name,
        _vars = 0,
        _pool = {},
        _variables = {},
        _schema = schema,
    }

    setmetatable(m, mt)
    m:generate_function()

    return m
end


function _M.code(self)
    if self._program then
        return self._program
    end

    if self._vars > 0 then
        local variables = {}
        for i = 1, self._vars do
            variables[i] = "_" .. i
        end
        self._code[3] = self._code[3] .. "local " .. concat(variables, ", ")
    else
        table_remove(self._code, 3)
    end

    self._program = concat(self._code, "\n")

    return self._program
end


function _M.compile(self)
    local code = self:code()
    local chunk, err = loadstring(code)
    if not chunk then
        ngx.log(ngx.ERR, "compile error: ", err)
    end

    local func = chunk()
    return function(data, ctx) return func(data, Base, self._lib, ctx) end
end


function _M.emit(self, l)
    self._code[#self._code + 1] = string_rep("  ", self._indent) .. l
end


function _M.generate_function(self)
    self:generate_code_block(
        "return function(data, base, lib, ctx)",
        function()
            self:generate_code_block(
                "local validate = function(data, base, lib, ctx)",
                function()
                    self:emit("") -- place holder for variables
                    self:generate()
                    self:emit("return true")
                end
            )
            self:emit("return validate(data, base, lib, ctx)")
        end
    )
end


function _M.generate_code_block(self, head, generator, done)
    local indent = self._indent

    self:emit(head)
    self._indent = indent + 1

    generator()

    local pool = self._pool[self._indent]
    if pool then
        for idx, alive in pairs(pool) do
            if alive then
                self:do_free_variable(idx)
            end
        end
    end

    self._indent = indent

    if done ~= false then
        self:emit("end")
    end
end


function _M.generate_error(self, ...)
    local err = concat({ ... }, " ")
    self:emit(string_format("return nil, %q", err))
end


function _M.ensure_type(self, typ)
    local is = "is_" .. typ
    if self._var[is] then
        return
    end

    self._var[is] = true
    self:generate_code_block(
        _if(_not(_call("base." .. is, self._var()))),
        function()
            self:generate_error(tostring(self._var), "must be", typ)
        end
    )
end


function _M.ignore_except_type(self, typ)
    local ignore = "ignore_except_" .. typ
    if self._var[ignore] then
        return
    end

    self._var[ignore] = true

    if self._schema.type ~= typ then
        self:generate_code_block(
            _if(_not(_call("base.is_" .. typ, self._var()))),
            function()
                self:emit("return true")
            end
        )
    end
end


function _M.get_variable(self, alias)
    local idx = self._variables[0]
    if idx then
        self._variables[0] = self._variables[idx]
    else
        idx = #self._variables + 1
        self._vars = idx
    end

    self._variables[idx] = true

    local pool = self._pool[self._indent]
    if not pool then
        pool = {}
        self._pool[self._indent] = pool
    end

    pool[idx] = true

    return Variable.new("_" .. idx, alias, { idx = idx })
end


function _M.do_free_variable(self, idx)
    self._variables[idx] = self._variables[0]
    self._variables[0] = idx

    local pool = self._pool[self._indent]
    if pool then
        pool[idx] = false
    end
end


function _M.free_variable(self, var)
    self:do_free_variable(var.idx)
end


function _M.set_variable_properties(self, var)
    var = var or self._var
    if not var.properties then
        local properties = self:get_variable()
        self:emit(_assign(properties(), _call("base.table_len", var())))
        var.properties = properties()
    end
end


function _M.set_variable_length(self, var)
    var = var or self._var
    if not var.length then
        local len = self:get_variable()
        self:emit(_assign(len(), _len(var())))
        var.length = len()
    end
end


function _M.generate_specific(self, entry, typ)
    local has = {}
    local schema = self._schema
    for _, k in ipairs(entry) do
        if schema[k[1]] then
            has[#has + 1] = k
        elseif (
            k[1] == "properties" and (
            schema.additionalProperties ~= nil or
            schema.patternProperties)
        ) or (
            k[1] == "items" and (
            schema.additionalItems ~= nil)
        ) then
            has[#has + 1] = k
        end
    end

    if #has == 0 then
        return
    end

    local function generate()
        for _, k in ipairs(has) do
            self["_generate_" .. k[2]](self, self._schema[k[1]])
        end
    end

    local ensure = self._var["is_" .. typ]
    if ensure then
        return generate()
    end

    self:generate_code_block(
        _if(_call("base.is_" .. typ, self._var())),
        function()
            generate()
        end
    )
end


function _M.generate(self, var, schema)
    local _schema, _var = self._schema, self._var

    if var then self._var = var end
    if schema then self._schema = schema end

    for _, v in ipairs(mapping) do
        if is_tbl(v[1]) then
            self:generate_specific(v[1], v[2])
        elseif self._schema[v[1]] then
            self["_generate_" .. v[2]](self, self._schema[v[1]])
        end
    end

    self._schema, self._var = _schema, _var
end


function _M._generate_not(self, schema)
    local ok = self:get_variable()
    local sub = self:get_variable()

    self:generate_code_block(
        _assign(sub(), "function()"),
        function()
            self:generate(nil, schema)
            self:emit("return true")
        end
    )

    self:emit(_assign(ok(), _call(sub())))
    self:generate_code_block(
        _if(ok()),
        function()
            self:generate_error(tostring(self._var),
                                "must not be valid by not schema")
        end
    )

    self:free_variable(ok)
    self:free_variable(sub)
end


function _M._generate_type(self, types)
    types = is_tbl(types) and types or { types }

    local validators = {}
    for _, typ in ipairs(types) do
        validators[#validators + 1] = "base.is_" .. typ
    end

    if #validators == 1 then
        self:ensure_type(types[1])
        return
    end

    local ok = self:get_variable()
    self:emit(_assign(ok(), "false"))
    self:generate_code_block(
        _for(_call("ipairs", _list(validators)), "_", "v"),
        function()
            self:generate_code_block(
                _if(_call("v", self._var())),
                function()
                    self:emit(_stmts(_assign(ok(), "true"), "break"))
                end
            )
        end
    )

    self:generate_code_block(
        _if(_not(ok())),
        function()
            self:generate_error(tostring(self._var), "must be",
                                concat(types, " or "))
        end
    )

    self:free_variable(ok)
end


function _M._generate_enum(self, enum)
    local ok = self:get_variable()

    enum = dump(enum)
    self:emit(_assign(ok(), "false"))

    self:generate_code_block(
        _for(_call("ipairs", enum), "_", "elem"),
        function()
            self:generate_code_block(
            _if(_call("base.equal", self._var(), "elem")),
            function()
                self:emit(_stmts(_assign(ok(), "true"), "break"))
            end)
        end
    )

    self:generate_code_block(
        _if(_not(ok())),
        function()
            self:generate_error(tostring(self._var), "must be one of", enum)
        end
    )

    self:free_variable(ok)
end


function _M._generate_min_length(self, n)
    self:generate_code_block(
        _if(_op(self._var:len(), "<", dump(n))),
        function()
            self:generate_error(tostring(self._var),
                                "be longer than or equal to", dump(n),
                                "characters")
        end
    )
end


function _M._generate_max_length(self, n)
    self:generate_code_block(
        _if(_op(self._var:len(), ">", dump(n))),
        function()
            self:generate_error(tostring(self._var),
                                "be shorter than or equal to", dump(n),
                                "characters")
        end
    )
end


function _M._generate_min_items(self, n)
    self:generate_code_block(
        _if(_op(self._var:len(), "<", dump(n))),
        function()
            self:generate_error(tostring(self._var),
                                "must contain at least", dump(n), "items")
        end
    )
end


function _M._generate_max_items(self, n)
    self:generate_code_block(
        _if(_op(self._var:len(), ">", dump(n))),
        function()
            self:generate_error(tostring(self._var),
                                "must contain less than or equal to", dump(n),
                                "items")
        end
    )
end


function _M._generate_unique_items(self, unique)
    if unique == false then
        return
    end

    local mem = self:get_variable()

    self:emit(_assign(mem(), "{}"))
    self:generate_code_block(
        _for(_call("ipairs", self._var()), "_", "k"),
        function()
            self:emit(_assign("local id", _call("base.serialize", "k")))
            self:generate_code_block(
                _if(_index(mem(), "id")),
                function()
                    self:generate_error(tostring(self._var),
                                        "must contain unique items")
                end,
                false
            )
            self:generate_code_block(
                "else",
                function()
                    self:emit(_assign(_index(mem(), "id"), "true"))
                end
            )
        end
    )
end


function _M._generate_required(self, required)
    required = dump(required)
    self:generate_code_block(
        _for(_call("ipairs", required), "_", "k"),
        function()
            self:generate_code_block(
                _if(_op(_index(self._var(), "k"), "==", "nil")),
                function()
                    self:generate_error(tostring(self._var), "must contain",
                                        required, "properties")
                end
            )
        end
    )
end


function _M._generate_dependencies(self, dependencies)
    local function generate_dependency(prop, dep)
        dep = dump(dep)
        self:generate_code_block(
            _if(_op(_index(self._var(), dep), "==", "nil")),
            function()
                self:generate_error(
                    prop, "depends on", _index(tostring(self._var), dep))
            end
        )
    end

    local function generate_list_dependencies(prop, dep)
        if #dep == 1 then
            return generate_dependency(prop, dep[1])
        end

        self:generate_code_block(
            _for(_call("ipairs", dump(dep)), "_", "k"),
            function()
                self:generate_code_block(
                    _if(_op(_index(self._var(), "k"), "==", "nil")),
                    function()
                        self:generate_error(
                            prop, "depends on",
                            tostring(self._var) .. dump(dep))
                    end
                )
            end
        )
    end

    local function generate_schema_dependencies(prop, schema)
        for k, v in pairs(schema) do
            local var = self:get_variable(_index(tostring(self._var), dump(k)))
            self:emit(_assign(var(), _index(self._var(), dump(k))))
            self:generate_code_block(
                _if(_op(var(), "==", "nil")),
                function()
                    self:generate_error(
                        prop, "depends on", tostring(var))
                end
            )
            self:generate(var, v)
            self:free_variable(var)
        end
    end

    for k, v in pairs(dependencies) do
        local prop = _index(tostring(self._var), dump(k))
        self:generate_code_block(
            _if(_op(_index(self._var(), dump(k)), "~=", "nil")),
            function()
                if is_array(v) then
                    generate_list_dependencies(prop, v)
                elseif is_tbl(v) and v.properties then
                    generate_schema_dependencies(prop, v.properties)
                else
                    generate_dependency(prop, v)
                end
            end
        )
    end
end


function _M._generate_minimum(self, min)
    local op, hint
    if self._schema.exclusiveMinimum then
        op, hint = "<=", "bigger than"
    else
        op, hint = "<", "bigger than or equal to"
    end

    self:generate_code_block(
        _if(_op(self._var(), op, dump(min))),
        function()
            self:generate_error(tostring(self._var),
                                "must be", hint, dump(min))
        end
    )
end


function _M._generate_maximum(self, min)
    local op, hint
    if self._schema.exclusiveMaximum then
        op, hint = ">=", "smaller than"
    else
        op, hint = ">", "smaller than or equal to"
    end

    self:generate_code_block(
        _if(_op(self._var(), op, dump(min))),
        function()
            self:generate_error(tostring(self._var),
                                "must be", hint, dump(min))
        end
    )
end


function _M._generate_pattern(self, pattern)
    local opt = self._schema.ignoreCase and "ijo" or "jo"
    pattern = dump(pattern)
    self:generate_code_block(
        _if(_not(_call("base.re_find", self._var(), pattern, dump(opt)))),
        function()
            self:generate_error(tostring(self._var),
                                "must match pattern", pattern)
        end
    )
end


function _M._generate_multiple_of(self, n)
    self:generate_code_block(
        _if(_not(_call("base.is_integer", _op(self._var(), "/", dump(n))))),
        function()
            self:generate_error(tostring(self._var),
                                "must be multiple of", dump(n))
        end
    )
end


function _M._generate_items(self, items)
    local parent = self._schema
    local root = self:get_variable(tostring(self._var))

    self:emit(_assign(root(), self._var()))
    self:set_variable_length(root)
    self._var.length = root.length

    if is_array(items) and #items > 0 then
        for i, schema in ipairs(items) do
            if next(schema) then
                self:generate_code_block(
                    _if(_op(root:len(), ">", dump(i - 1))),
                    function()
                        local var = Variable.new(
                            _index(root(), i), _index(tostring(root), i))
                        self:generate(var, schema)
                    end,
                    false
                )
                if schema.default ~= nil then
                    self:generate_code_block(
                        "else",
                        function()
                            self:emit(_assign(self._var(),
                                      dump(schema.default)))
                        end,
                        false
                    )
                end
                self:emit("end")
            end
        end
    elseif is_tbl(items) and next(items) then
        self:generate_code_block(
            _for(_call("ipairs", root()), "_", "elem"),
            function()
                local var = Variable.new("elem", _index(tostring(root), "?"))
                self:generate(var, items)
            end
        )
    else
        items = {}
    end

    if is_array(items) and #items > 0 and parent.additionalItems == false then
        self:generate_code_block(
            _if(_op(root:len(), ">", dump(#items))),
            function()
                self:generate_error(
                    tostring(self._var),
                    "must contain only specified items")
            end
        )
    elseif is_tbl(parent.additionalItems) and next(parent.additionalItems) then
        self:generate_code_block(
            _for_assign("i", #items + 1, root:len(), 1),
            function()
                local var = Variable.new(
                    _index(root(), "i"), _index(tostring(root), "?"))
                self:generate(var, parent.additionalItems)
            end
        )
    end

    self:free_variable(root)
end


function _M._generate_any_of(self, schemas)
    local ok = self:get_variable()
    self:generate_code_block(
        _assign(ok(), "function()"),
        function()
            for _, schema in ipairs(schemas) do
                local pass = self:get_variable()
                self:generate_code_block(
                    _assign(pass(), "function()"),
                    function()
                        self:generate(nil, schema)
                        self:emit("return true")
                    end
                )
                self:generate_code_block(
                    _if(_call(pass())),
                    function()
                        self:emit("return true")
                    end
                )
                self:free_variable(pass)
            end
        end,
        false
    )
    self:emit("end")
    self:generate_code_block(
        _if(_not(_call(ok()))),
        function()
            self:generate_error(tostring(self._var),
                                "must be valid by one of anyOf definition")
        end
    )
    self:free_variable(ok)
end


function _M._generate_all_of(self, schemas)
    for _, schema in ipairs(schemas) do
        self:generate(nil, schema)
    end
end


function _M._generate_one_of(self, schemas)
    local blank = self:get_variable()
    local count = self:get_variable()

    self:emit(_assign(count(), "0"))
    self:generate_code_block(
        _assign(blank(), "(function()"),
        function()
            self:emit("local _")
            for _, schema in ipairs(schemas) do
                self:generate_code_block(
                    _assign("_", "(function()"),
                    function()
                        self:generate(nil, schema)
                        self:emit(_assign(count(), _op(count(), "+", "1")))
                    end,
                    false
                )
                self:emit("end)()")
                self:generate_code_block(
                    _if(_op(count(), ">", "1")),
                    function()
                        self:emit("return")
                    end
                )
            end
        end,
        false
    )
    self:emit("end)()")
    self:generate_code_block(
        _if(_op(count(), "~=", "1")),
        function()
            self:generate_error(tostring(self._var),
                                "must be valid by one of oneOf definition")
        end
    )

    self:free_variable(blank)
    self:free_variable(count)
end


function _M._generate_min_properties(self, properties)
    self:set_variable_properties()
    self:generate_code_block(
        _if(_op(self._var.properties, "<", properties)),
        function()
            self:generate_error(tostring(self._var),
                "must contain at least", properties, "properties")
        end
    )
end


function _M._generate_max_properties(self, properties)
    self:set_variable_properties()
    self:generate_code_block(
        _if(_op(self._var.properties, ">", properties)),
        function()
            self:generate_error(tostring(self._var),
                "must contain less than or equal to", properties, "properties")
        end
    )
end


function _M._generate_properties(self, properties)
    local parent = self._schema
    local keys = self:get_variable()
    local is_root = tostring(self._var) == self._name
    local root

    if is_root then
        root = self._var
    else
        root = self:get_variable(tostring(self._var))
        self:emit(_assign(root(), self._var()))
    end

    self:emit(_assign(keys(), "{}"))
    self:generate_code_block(
        _for(_call("pairs", root()), "k"),
        function()
            self:emit(_assign(keys() .. "[k]", "true"))
        end
    )

    local function generate_properties(schema, var, key)
        self:generate_code_block(
            _if(_index(keys(), key)),
            function()
                self:emit(_assign(_index(keys(), key), "nil"))
                self:generate(var, schema)
            end,
            false
        )
        if schema.default ~= nil then
            self:generate_code_block(
                "else",
                function()
                    self:emit(_assign(self._var(), dump(schema.default)))
                end,
                false
            )
        end
        self:emit("end")
    end

    if parent.properties then
        for key, schema in pairs(properties) do
            local var = Variable.new(
                _index(root(), dump(key)), _index(tostring(root), dump(key)))
            generate_properties(schema, var, dump(key))
        end
    end

    if parent.patternProperties then
        properties = parent.patternProperties
        for pattern, schema in pairs(properties) do
            self:generate_code_block(
                _for(_call("pairs", root()), "k"),
                function()
                    self:generate_code_block(
                        _if(_and(_call("base.is_string", "k"),
                                 _call("base.re_find",
                                       "k", dump(pattern), dump("jo")))),
                        function()
                            local var = Variable.new(
                                _index(root(), "k"),
                                _index(tostring(root), dump(pattern)))
                            self:emit(_assign(_index(keys(), "k"), "nil"))
                            self:generate(var, schema)
                        end
                    )
                end
            )
        end
    end

    if parent.additionalProperties == false then
        self:generate_code_block(
            _if(_op(_call("next", keys()), "~=", "nil")),
            function()
                self:generate_error(tostring(self._var),
                                    "must contain only specified properties")
            end
        )
    elseif is_tbl(parent.additionalProperties) then
        local var = self:get_variable(_index(tostring(root), "?"))
        self:generate_code_block(
            _for(_call("pairs", keys()), "k"),
            function()
                self:emit(_assign(var(), _index(root(), "k")))
                self:generate(var, parent.additionalProperties)
            end
        )
        self:free_variable(var)
    end

    if not is_root then
        self:free_variable(root)
    end

    self:free_variable(keys)
end


function _M._generate_validator(self, validator)
    self:generate_code_block(
        _if(_not(_call("lib." .. validator, self._var(), "ctx"))),
        function()
            self:generate_error(
                tostring(self._var), "must be valid by", validator)
        end
    )
end


return _M
