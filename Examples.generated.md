# Examples

- [Validation](#validation)

---
## Validation
### ► A null value against a schema accepting only null values
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
```
✓ no violations
```

### ► A boolean value against a schema accepting only null values
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
```
✗ Invalid type. Expected null but got boolean.
  Schema path: #/type
  JSON path: $
```

### ► A boolean value against a schema accepting only null and string values
A boolean value does not conform to the schema as only null or string values do.
#### Input
##### JSON schema
```json
{"type":["null","string"]}
```
##### JSON
```json
true
```
#### Output
```
✗ Invalid type. Expected null or string but got boolean.
  Schema path: #/type
  JSON path: $
```

