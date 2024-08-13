# mkjson

[![CI](https://github.com/mfussenegger/mkjson/workflows/CI/badge.svg)](https://github.com/mfussenegger/mkjson/actions)

`mkjson` is a CLI to generate JSON records.


Static JSON:

```
↪  mkjson x=10 y=20 foo=bar
{"foo":"bar","x":10,"y":20}
```

Generated random data:

```
↪  mkjson --num 3 id="uuid4()" x="randomInt(0, 10)"
{"id":"2c6ce42f-5f7d-4e65-a1a1-8b39f6cfce19","x":6}
{"id":"d4c1af69-3cdd-417d-98e0-d3774f5fa1be","x":8}
{"id":"8fc064c9-57ba-4d78-b205-57899776e757","x":6}
```

It takes `fieldName=value` or `fieldName=provider()` pairs as
argument. Each pair will result in a field within the JSON object.

To generate infinite records, use:

```
↪  mkjson --num Inf
```


### Examples

Using `fromFile` and `oneOf`:

```
↪  mkjson --num 3 w="oneOf(fromFile('/usr/share/dict/words'))"
{"w":"Karl"}
{"w":"demographic"}
{"w":"calumny's"}
```


Use dotted notation to create objects:

```
↪  mkjson point.x=10 point.y=20
{"point":{"x":10,"y":20}}
```

Or nest calls to create objects:

```
↪  mkjson obj=$(mkjson xs="array(randomInt(0, 4), randomInt(5, 9))")
{"obj":{"xs":[2,5]}}
```

Or use the `object` provider with various providers. Argument are `key, value [, ...]` pairs:

```
↪  mkjson obj="object(dt, randomDateTime(), type, fromRegex('[a-z]{4}-\d+'), xs, replicate(5, randomInt(0, 10)))"
{"obj":{"dt":"2099-09-11T16:33:41Z","xs":[6,8,5,2,0],"type":"ldwa-2667786160"}}
```

You may also want to read this [introduction and sample use case][1].


### Available providers

 - uuid1
 - uuid4
 - null
 - randomBool
 - randomChar
 - randomInt(lower, upper)
 - randomDouble(lower, upper)
 - randomDate [(lower, upper)]
 - randomDateTime [(lower, upper)]
 - array(expr [, ...])
 - oneOf(arrayExpr)
 - oneOf(expr, expr [, ...])
 - replicate(number, expr)
 - object(key, value [, ...])
 - fromFile(fileName)
 - fromRegex(pattern)
 - join(sep, val1, [, ...])
 - join(array)


## Installation

Get a pre-build binary from the
[releases](https://github.com/mfussenegger/mkjson/releases) page or build it
yourself:

Install stack:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Install mkjson:

```bash
git clone https://github.com/mfussenegger/mkjson.git
cd mkjson
stack install
```


## Alternatives

There are some alternatives if you're not so interested in the random-value
generation aspect of `mkjson`:

 - [jarg](https://github.com/jdp/jarg)
 - [jo](https://github.com/jpmens/jo)


[1]: https://zignar.net/2020/05/01/generating-data-sets-using-mkjson/
