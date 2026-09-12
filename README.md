# yaml-protocol

CLOS YAML 1.2 encode/decode for [cl-stack](https://github.com/egao1980/cl-stack). Own repo. **Native Common Lisp** — no libyaml, no FFI.

**Extends** [`json-protocol`](https://github.com/egao1980/json-protocol) (`yaml-backend` ⊆ `json-backend`, `yaml-error` ⊆ `json-error`). Same Lisp mapping. JSON ⊂ YAML at the document level.

Default encode is **`:block`**. `:style :json` is optional (native emit; may delegate to a loaded JSON backend). YAML backends do **not** need `json-backend-jzon` / yason.

jzon stays the RFC 8259 `:json` path — `json-protocol` does **not** depend on this package.

| System | Role | version |
|--------|------|---------|
| `yaml-protocol` (`stack-yaml`) | YAML 1.2 events + compose + serdes `:yaml` | **0.2.0** |

```lisp
(asdf:load-system "yaml-protocol")
(yaml-protocol:decode "foo: 1")
(yaml-protocol:parse-events "foo: &a 1~%bar: *a")
(yaml-protocol:decode "name: Ada" :object-class 'person)
```

YAML 1.2 Core scalars (`NO` is a string, not boolean).

## Events

`parse-events` is the parser. It emits the [yaml-test-suite](https://github.com/yaml/yaml-test-suite) event DSL (`+STR` / `+DOC` / `+MAP` / `+SEQ` / `=VAL` / `=ALI` / …). Anchors, tags, and scalar style live on the event; aliases are **not** resolved in the stream.

`compose-events` builds the Lisp graph: Core schema on plain scalars, alias identity, `<<` merge (existing keys win).

`decode` = first document of `compose-events` ∘ `parse-events`. `decode-all` = every document → vector. Empty / comment-only stream: events are `+STR` `-STR`; `decode` → `:null`; `decode-all` → `#()`.

`format-events` prints the suite DSL (used by the conformance tests).

## Object creation

Compose **always** builds this Lisp graph (same as json-protocol):

| YAML | Lisp |
|------|------|
| mapping | hash-table (`equal`), string keys |
| sequence | vector |
| null | `:null` |
| false / true | `nil` / `t` |
| int / float | integer / double-float |
| string | string |
| alias | the anchored object (`eq`) |
| `<<` | compose-time merge; existing keys win |

`:object-class` is optional and runs **after** compose, **once per document root**:

| Value | Effect |
|-------|--------|
| `nil` (default) | leave the composed value |
| class designator | `make-instance` with initargs = each mapping key, `string-upcase` interned in `keyword`, + value. Nested mappings stay hash-tables. Non-mapping root → `yaml-parse-error`. |
| function (or fbound symbol that is not a class) | `(funcall fn composed)` — use this for graphs, nested classes, validation |

Same on `decode`, `decode-all` (per document), `decode-octets`, and serdes-protocol `:yaml`.

There is no recursive class map. Want a tree of CLOS objects → pass a function.

## Conformance

Vendored corpus: [yaml-test-suite](https://github.com/yaml/yaml-test-suite) tag **`data-2022-01-17`** (`tests/suite/`, MIT). Every `in.yaml` is a Rove case:

- `error` present → `parse-events` must signal `yaml-parse-error`
- else `format-events` must match `test.event`
- `in.json` present → composed value must match that JSON

## License

MIT — see [LICENSE](LICENSE). Suite fixtures remain MIT (yaml-test-suite).
