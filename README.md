# yaml-protocol

CLOS YAML 1.2 encode/decode for [cl-stack](https://github.com/egao1980/cl-stack). Own repo.

**Extends** [`json-protocol`](https://github.com/egao1980/json-protocol) (`yaml-backend` ⊆ `json-backend`, `yaml-error` ⊆ `json-error`). Same Lisp mapping. JSON ⊂ YAML at the document level.

Default encode is **`:block`**. `:style :json` is optional (native emit; may delegate to a loaded JSON backend). YAML backends do **not** need `json-backend-jzon` / yason.

Anchors / aliases / `<<` merge. `:object-class` optionally initializes a CLOS instance (or a function of the mapping). Same options on serdes `:yaml`.

jzon stays the RFC 8259 `:json` path — `json-protocol` does **not** depend on this package.

| System | Role | OCI |
|--------|------|-----|
| `yaml-protocol` (`stack-yaml`) | YAML 1.2 + serdes `:yaml` | **0.1.2** |

```lisp
(asdf:load-system "yaml-protocol")
(yaml-protocol:decode "foo: 1")                 ; block YAML
(yaml-protocol:decode "{\"foo\":1}")            ; JSON ⊂ YAML (input)
(yaml-protocol:encode ht)                       ; default :block
(yaml-protocol:encode ht :style :json)          ; optional JSON-schema YAML
(yaml-protocol:decode "name: Ada" :object-class 'person)  ; optional CLOS init
```

YAML 1.2 Core scalars (`NO` is a string, not boolean).

## License

MIT — see [LICENSE](LICENSE).
