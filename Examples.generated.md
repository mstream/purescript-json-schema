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
✗
  Schema path:
  #/type
  JSON path:
  $
  Invalid type. Expected null but got boolean.
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
✗
  Schema path:
  #/type
  JSON path:
  $
  Invalid type. Expected null or string but got boolean.
```

### ► An array with 2 out of 5 items not matching the desired item type
When schema requires items to conform to a certain schema, every single value in the array has to.
#### Input
##### JSON schema
```json
{"items":{"type":["null"]},"type":["array"]}
```
##### JSON
```json
[null,false,null,true,null]
```
#### Output
```
✗
  Schema path:
  #
  JSON path:
  $
  Invalid array: 
  -
    Schema path:
    #/items/type
    JSON path:
    $[1]
    Invalid type. Expected null but got boolean.
  -
    Schema path:
    #/items/type
    JSON path:
    $[3]
    Invalid type. Expected null but got boolean.
```

