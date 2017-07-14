OCaml-webworker
===
A webworker to assist with OCaml editor experiences on the web

Warning
---
This is a work in progress, it's not easily reusable at the moment

How to install
---
I have no idea...

Available commands
---

- type

Used to type-check a piece of code, also needed for all the other tasks besides
`execute`. Note that always the last correctly typed version is stored.

Request:
```json
{
  "msgId": 123,
  "msgType": "type",
  "code": "let a = \"an example\""
}
```

Response in case of error:
```json
{
  "msgId": 123,
  "type": "SyntaxError|TypemodError|TypetexpError|TypecoreError",
  "subtype": "Something",
  "locations": [{
    "loc_end": {
      "pos_bol": 0,
      "pos_cnum": 5,
      "pos_fname": "",
      "pos_lnum": 1
    },
    "loc_start":{
      "pos_bol": 0,
      "pos_cnum": 17,
      "pos_fname": "",
      "pos_lnum": 1
    }
  }]
}
```
- execute
- complete_prefix
- locate
- outline
- shape
- documentation


Demo
---
Warning: experience is suboptimal, a better one will arrive later:

https://sanderspies.github.io/ocaml-webworker/test.html

Issues
---
- no proper error reporting with wrong input
- needs way more documentation

License
===
MIT
