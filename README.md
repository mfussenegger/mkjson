# fake-json

[![Build status](https://dev.azure.com/mfussenegger/fake-json/_apis/build/status/fake-json-CI?branchName=master)](https://dev.azure.com/mfussenegger/fake-json/_build/latest?definitionId=2)

`fake-json` is a CLI to generate random JSON records. If invoked it will
generate and print 1 JSON object per line until cancelled:

```
↪  fake-json id=uuid4 x="randomInt(0, 10)" | head -n 3
{"id":"2c6ce42f-5f7d-4e65-a1a1-8b39f6cfce19","x":6}
{"id":"d4c1af69-3cdd-417d-98e0-d3774f5fa1be","x":8}
{"id":"8fc064c9-57ba-4d78-b205-57899776e757","x":6}
```

It takes `fieldName=valueProvider` pairs as argument. Each pair will result in
a field within the JSON object.

### Available providers

 - uuid1
 - uuid4
 - null
 - randomInt(lower, upper)
 - randomDouble(lower, upper)
 - array(expr [, ...])
 - oneOf(arrayExpr)
 - oneOf(expr, expr [, ...])
 - replicate(number, expr)
 - object(key, value [, ...])
 - fromFile(fileName)


## Installation

There might be pre-build binaries available at some point, until then:

Install stack:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Install fake-json:

```bash
git clone https://github.com/mfussenegger/fake-json.git
cd fake-json
stack install
```
