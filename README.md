# mkjson

[![Build status](https://dev.azure.com/mfussenegger/mkjson/_apis/build/status/mkjson-CI?branchName=master)](https://dev.azure.com/mfussenegger/mkjson/_build/latest?definitionId=2)

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


### Available providers

 - uuid1
 - uuid4
 - ulid
 - null
 - randomBool
 - randomChar
 - randomInt(lower, upper)
 - randomDouble(lower, upper)
 - randomDate [(lower, upper)]
 - randomDateTime 
 - array(expr [, ...])
 - oneOf(arrayExpr)
 - oneOf(expr, expr [, ...])
 - replicate(number, expr)
 - object(key, value [, ...])
 - fromFile(fileName)
 - fromRegex(pattern)


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
