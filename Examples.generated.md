# Examples

- [Compatibility](#compatibility)
- [Diff](#diff)
- [Validation](#validation)

---
## Compatibility
### ► No JSON schema differences
When there is not JSON schema differences, schema change is fully compatible.
#### Input
##### JSON schema differences
```
no differences
```
#### Output
```
full
```

### ► Expected JSON value type changes from null to boolean
Because no boolean value can satisfy null JSON type constraint, and vice versa, such a change is incompatible.
#### Input
##### JSON schema differences
```
-
  Schema path: #
  Change of accepted JSON value types from 
  - null
  to
  - boolean
```
#### Output
```
none
```

### ► Expected JSON value type changes from integer to number
Because every integer is a number, but not vice versa, such a change is backward compatible.
#### Input
##### JSON schema differences
```
-
  Schema path: #
  Change of accepted JSON value types from 
  - integer
  to
  - number
```
#### Output
```
backward
```

### ► Expected JSON value type changes from number to integer
Because every integer is a number, but not vice versa, such a change is forward compatible.
#### Input
##### JSON schema differences
```
-
  Schema path: #
  Change of accepted JSON value types from 
  - number
  to
  - integer
```
#### Output
```
forward
```

### ► Expected JSON value types is extended
Because more value types than before are accepted, this change is backward compatible.
#### Input
##### JSON schema differences
```
-
  Schema path: #
  Change of accepted JSON value types from 
  - null
  to
  - boolean
  - null
```
#### Output
```
backward
```

### ► Expected JSON value types is reduced
Because less value types than before are accepted, this change is forward compatible.
#### Input
##### JSON schema differences
```
-
  Schema path: #
  Change of accepted JSON value types from 
  - boolean
  - null
  to
  - null
```
#### Output
```
forward
```

### ► Expected JSON value types including number is extended by integer
Because every integer is a number, such a change is fully compatible.
#### Input
##### JSON schema differences
```
-
  Schema path: #
  Change of accepted JSON value types from 
  - number
  to
  - integer
  - number
```
#### Output
```
full
```

### ► Expected JSON value types including integer is extended by number
Because not every integer is a number, such a change is backward compatible.
#### Input
##### JSON schema differences
```
-
  Schema path: #
  Change of accepted JSON value types from 
  - integer
  to
  - integer
  - number
```
#### Output
```
backward
```

### ► Expected JSON value types including integer and number is reduced by integer
Because every integer is a number, such a change is fully compatible.
#### Input
##### JSON schema differences
```
-
  Schema path: #
  Change of accepted JSON value types from 
  - integer
  - number
  to
  - number
```
#### Output
```
full
```

### ► Expected JSON value types including integer and number is reuced by number
Because not every integer is a number, such a change is forward compatible.
#### Input
##### JSON schema differences
```
-
  Schema path: #
  Change of accepted JSON value types from 
  - integer
  - number
  to
  - integer
```
#### Output
```
forward
```

---
## Diff
### ► Comparing identical schemata
When two identical schemata are compared, no difference should be found.
#### Input
##### Previous JSON schema
```json
false
```
##### Next JSON schema
```json
false
```
#### Output
```
no differences
```

### ► Changing expected JSON value type from null to boolean
Any change in expected JSON value type should be accounted as a difference.
#### Input
##### Previous JSON schema
```json
{"type":["null"]}
```
##### Next JSON schema
```json
{"type":["boolean"]}
```
#### Output
```
-
  Schema path: #
  Change of accepted JSON value types from 
  - null
  to
  - boolean
```

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
  Schema path: #/type
  JSON path: $
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
  Schema path: #/type
  JSON path: $
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
  Schema path: #
  JSON path: $
  Invalid array: 
  -
    Schema path: #/items/type
    JSON path: $[1]
    Invalid type. Expected null but got boolean.
  -
    Schema path: #/items/type
    JSON path: $[3]
    Invalid type. Expected null but got boolean.
```

### ► An array with forbidden duplicate value.
When schema requires items to be unique, any duplicate occurrence of any value will cause a validation failure.
#### Input
##### JSON schema
```json
{"uniqueItems":true}
```
##### JSON
```json
["a","b","b","c","d","d","e"]
```
#### Output
```
✗
  Schema path: #
  JSON path: $
  Invalid array: 
  -
    Schema path: #/uniqueItems
    JSON path: $[1]
    Non-unique array item.
  -
    Schema path: #/uniqueItems
    JSON path: $[2]
    Non-unique array item.
  -
    Schema path: #/uniqueItems
    JSON path: $[4]
    Non-unique array item.
  -
    Schema path: #/uniqueItems
    JSON path: $[5]
    Non-unique array item.
```

