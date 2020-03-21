-- https://github.com/jdesgats/ljsonschema/blob/master/bench.lua

local cjson = require "cjson.safe"
local clock = require("os").clock
local jsonschema = require "lib/resty/jsonschema/compiler"


local supported = {
    "spec/extra/sanity.json",
    "spec/extra/empty.json",
    "spec/extra/table.json",
    "spec/extra/dependencies.json",

    "spec/JSON-Schema-Test-Suite/tests/draft4/type.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/default.json",

    -- object
    "spec/JSON-Schema-Test-Suite/tests/draft4/properties.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/required.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/additionalProperties.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/patternProperties.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/minProperties.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/maxProperties.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/dependencies.json",

    -- string
    "spec/JSON-Schema-Test-Suite/tests/draft4/minLength.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/maxLength.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/pattern.json",

    -- number
    "spec/JSON-Schema-Test-Suite/tests/draft4/multipleOf.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/minimum.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/maximum.json",

    -- array
    "spec/JSON-Schema-Test-Suite/tests/draft4/items.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/additionalItems.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/minItems.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/maxItems.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/uniqueItems.json",

    -- enum
    "spec/JSON-Schema-Test-Suite/tests/draft4/enum.json",

    -- misc
    "spec/JSON-Schema-Test-Suite/tests/draft4/not.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/allOf.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/anyOf.json",
    "spec/JSON-Schema-Test-Suite/tests/draft4/oneOf.json",
}

local blacklist = {
    -- edge cases, not supported features
    ["object type matches objects"] = {
        ["an array is not an object"] = true, -- empty object/array confusion
    },
    ["array type matches arrays"] = {
        ["an object is not an array"] = true, -- empty object/array confusion
    },
    ["required validation"] = {
        ["ignores arrays"] = true,
    },
    ["minProperties validation"] = {
        ["ignores arrays"] = true,
    },
    ["maxProperties validation"] = {
        ["ignores arrays"] = true,
    },
    ["minLength validation"] = {
        ["one supplementary Unicode code point is not long enough"] = true, -- unicode handling
    },
    ["maxLength validation"] = {
        ["two supplementary Unicode code points is long enough"] = true, -- unicode handling
    },
    ["a schema given for items"] = {
        ["JavaScript pseudo-array is valid"] = true, -- pseudo array
    },
    ["an array of schemas for items"] = {
        ["JavaScript pseudo-array is valid"] = true, -- pseudo array
    },
}


local function bench(func, msg, iters, count)
    count = count or 1
    iters = iters or 1000
    local start = clock()
    local n = 0
    while n < iters do
        func()
        n = n + 1
    end
    local elapsed = clock() - start
    print(string.format("%s: elapsed time: %.6f s, per %.6f us",
                        msg, elapsed, 1000000 * elapsed / iters / count))
end


local function decode_descriptor(path)
    local f = assert(io.open(path))
    local testsuite = cjson.decode(assert(f:read("*a")))
    f:close()
    return ipairs(testsuite)
end


local function load_test()
    local cases, n = {}, 0
    for _, descriptor in ipairs(supported) do
        for _, suite in decode_descriptor(descriptor) do
            local skipped = blacklist[suite.description] or {}
            if skipped == true then
                print("skip suite: " .. suite.description)
            else
                print("add suite: " .. suite.description)
                local schema = cjson.encode(suite.schema)
                local ok, js = pcall(jsonschema.new, suite.schema)
                if not ok then
                    print("error: " .. js .. ", " ..
                          cjson.encode(suite.schema))
                    return
                end
                for _, case in ipairs(suite.tests) do
                    if skipped[case.description] then
                        print("skip suite case: " .. case.description)
                    else
                        n = n + 1
                        local ok, jv = pcall(js.compile, js)
                        if not ok then
                            print(js:code())
                        end
                        cases[n] = { jv, schema, case, js:code() }
                    end
                end
            end
        end
    end
    return cases, n
end


local cases, ncases = load_test()
local function test()
    for _, case in ipairs(cases) do
        local ok, pass, err = pcall(case[1], case[3].data)
        local valid = pass or false
        local expect = case[3].valid
        if valid ~= expect then
            print(string.format("expect %s, but got %s", expect, valid))
            print(case[2])
            print(cjson.encode(case[3]))
            print(case[4])
            if err then
                error(err)
            end
            return
        end
    end
end


bench(load_test, "load test cases", 1)

print("total test cases: " .. ncases)
bench(test, "run test cases", tonumber(arg[1]) or 1, ncases)


local schema = {
    properties = {
        num = {
            type = "number",
            validator = "positive"
        },
        x = {
            default = 2,
        }
    }
}

local js = jsonschema.new(schema, { positive = function(n) return n > 0 end }, "test")
local jv = js:compile()
local obj  = { num = 1 }
local ok, err = jv(obj)
assert(ok, err)
assert(obj.x == 2, obj.x)

obj.num = -1
obj.x = 3
ok, err = jv(obj)
assert(not ok, err)
assert(obj.x == 3)

schema = {
    items = {
        { type = "number" },
        { default = 2 },
        { enum = { cjson.null } },
    },
}

obj = { 1 }
js = jsonschema.new(schema)
jv = js:compile()

ok, err = jv(obj)
assert(ok)
assert(obj[2] == 2)

obj = { 1, 3, cjson.null }
ok, err = jv(obj)
assert(ok)
assert(obj[2] == 3)

schema = {
    type = "object",
    properties = {
        foo = {
            type = "object",
            patternProperties = {
                ["^bar"] = {
                    type = "object",
                    properties = {
                        baz = { type = "string", },
                        bazz = { type = "string", length = 1024 },
                    },
                    required = { "baz", },
                },
            },
        },
        unique_array = {
            type = "array",
            length = 2,
            uniqueItems = true,
        },
        object = {
            type = "object",
            properties = {
                foo = {},
                bar = {},
            },
            additionalProperties = false
        },
        object2 = {
            type = "object",
            properties = {
                foo = {},
                bar = {},
            },
            additionalProperties = {
                type = "boolean"
            }
        }
    },
}

obj = {
    foo = {
        bar1 = {
            baz = "hi",
            bazz = string.rep("x", 1024)
        }
    },
    unique_array = { 1, 2 },
    object = { foo = true, bar = {} },
    object2 = { foo = true, bar = {}, baz = true },
}

js = jsonschema.new(schema)

-- print(js:code())

jv = js:compile()

ok, err = jv(obj)
assert(ok, err)
