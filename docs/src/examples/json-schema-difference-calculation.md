## JSON Schema Difference Calculation
TODO



Properties:

- comparing two identical schemata should give no differences


Examples of comparing:

- [identical schemata](#identical-schemata)
- [schema expecting maximum value of 2 (exclusively) to schema expecting maximum value of 4 (exclusively)](#schema-expecting-maximum-value-of-2-(exclusively)-to-schema-expecting-maximum-value-of-4-(exclusively))
- [schema expecting maximum value of 2 (inclusively) to schema expecting maximum value of 4 (inclusively)](#schema-expecting-maximum-value-of-2-(inclusively)-to-schema-expecting-maximum-value-of-4-(inclusively))
- [schema expecting minimum value of 2 (exclusively) to schema expecting minimum value of 4 (exclusively)](#schema-expecting-minimum-value-of-2-(exclusively)-to-schema-expecting-minimum-value-of-4-(exclusively))
- [schema expecting minimum value of 2 (inclusively) to schema expecting minimum value of 4 (inclusively)](#schema-expecting-minimum-value-of-2-(inclusively)-to-schema-expecting-minimum-value-of-4-(inclusively))
- [schema expecting multiples of 2 to schema expecting multiples of 4](#schema-expecting-multiples-of-2-to-schema-expecting-multiples-of-4)
- [schema with expected type of null to schema with expected type of boolean](#schema-with-expected-type-of-null-to-schema-with-expected-type-of-boolean)
---
### schema with expected type of null to schema with expected type of boolean
Any change in expected JSON value type should be accounted as a difference.

#### Input
##### Previous JSON schema
```json
{
  "type": [
    "null"
  ]
}
```
##### Next JSON schema
```json
{
  "type": [
    "boolean"
  ]
}
```
#### Output
```
-
  Schema path: #/type
  change of accepted JSON value types from 
  - null
  to
  - boolean
```
---
### identical schemata
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
### schema expecting maximum value of 2 (exclusively) to schema expecting maximum value of 4 (exclusively)
changes of exclusiveMaximum keyword should be reported

#### Input
##### Previous JSON schema
```json
{
  "exclusiveMaximum": 2
}
```
##### Next JSON schema
```json
{
  "exclusiveMaximum": 4
}
```
#### Output
```
-
  Schema path: #/exclusiveMaximum
  change of exclusiveMaximum from 2.0 to 4.0
```
---
### schema expecting maximum value of 2 (inclusively) to schema expecting maximum value of 4 (inclusively)
changes of maximum keyword should be reported

#### Input
##### Previous JSON schema
```json
{
  "maximum": 2
}
```
##### Next JSON schema
```json
{
  "maximum": 4
}
```
#### Output
```
-
  Schema path: #/maximum
  change of maximum from 2.0 to 4.0
```
---
### schema expecting minimum value of 2 (exclusively) to schema expecting minimum value of 4 (exclusively)
changes of minimum keyword should be reported

#### Input
##### Previous JSON schema
```json
{
  "exclusiveMinimum": 2
}
```
##### Next JSON schema
```json
{
  "exclusiveMinimum": 4
}
```
#### Output
```
-
  Schema path: #/exclusiveMinimum
  change of exclusiveMinimum from 2.0 to 4.0
```
---
### schema expecting minimum value of 2 (inclusively) to schema expecting minimum value of 4 (inclusively)
changes of minimum keyword should be reported

#### Input
##### Previous JSON schema
```json
{
  "minimum": 2
}
```
##### Next JSON schema
```json
{
  "minimum": 4
}
```
#### Output
```
-
  Schema path: #/minimum
  change of minimum from 2.0 to 4.0
```
---
### schema expecting multiples of 2 to schema expecting multiples of 4
changes of multipleOf keyword should be reported

#### Input
##### Previous JSON schema
```json
{
  "multipleOf": 2
}
```
##### Next JSON schema
```json
{
  "multipleOf": 4
}
```
#### Output
```
-
  Schema path: #/multipleOf
  change of multipleOf from 2.0 to 4.0
```
