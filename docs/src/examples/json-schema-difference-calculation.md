## JSON Schema Difference Calculation
TODO



Examples:

- [Comparing identical schemata](#comparing-identical-schemata)
- [Changing expected JSON value type from null to boolean](#changing-expected-json-value-type-from-null-to-boolean)
- [Changing multipleOf value](#changing-multipleof-value)
---
### Comparing identical schemata
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
---
### Changing expected JSON value type from null to boolean
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
### Changing multipleOf value
TODO

#### Input
##### Previous JSON schema
```json
{"multipleOf":2,"type":["number"]}
```
##### Next JSON schema
```json
{"multipleOf":4,"type":["number"]}
```
#### Output
```
-
  Schema path: #
  multipleOf changed from 2.0 to 4.0
```
