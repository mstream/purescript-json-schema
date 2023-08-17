# Examples

- [Validation](#validation)

---
## Validation
### A null value against a schema accepting only null values
A null value conforms to the schema.
#### Input
##### JSON schema
```json
{"type":["null"]}
```
##### JSON
```json
null
```
#### Output
> <no violations>

### A boolean value against a schema accepting only null values
A boolean value does not conform to the schema as only null values do.
#### Input
##### JSON schema
```json
{"type":["null"]}
```
##### JSON
```json
true
```
#### Output
> [{ description: "", path: "?" }]

