## JSON Values Validation
TODO



Examples of validation of:

- [a boolean value against a schema accepting only nulls](#a-boolean-value-against-a-schema-accepting-only-nulls)
- [a boolean value against a schema accepting only nulls and strings](#a-boolean-value-against-a-schema-accepting-only-nulls-and-strings)
- [a fractional number against a schema accepting only whole numbers](#a-fractional-number-against-a-schema-accepting-only-whole-numbers)
- [a number which is a multiple of the factor desired by the schema](#a-number-which-is-a-multiple-of-the-factor-desired-by-the-schema)
- [a number which is equal to the exclusive maximum value specified by the schema](#a-number-which-is-equal-to-the-exclusive-maximum-value-specified-by-the-schema)
- [a number which is equal to the exclusive minimum value specified by the schema](#a-number-which-is-equal-to-the-exclusive-minimum-value-specified-by-the-schema)
- [a number which is equal to the maximum value specified by the schema](#a-number-which-is-equal-to-the-maximum-value-specified-by-the-schema)
- [a number which is equal to the minimum value specified by the schema](#a-number-which-is-equal-to-the-minimum-value-specified-by-the-schema)
- [a number which is greater than the maximum value specified by the schema](#a-number-which-is-greater-than-the-maximum-value-specified-by-the-schema)
- [a number which is greater than the minimum value specified by the schema](#a-number-which-is-greater-than-the-minimum-value-specified-by-the-schema)
- [a number which is less than the exclusive maximum value specified by the schema](#a-number-which-is-less-than-the-exclusive-maximum-value-specified-by-the-schema)
- [a number which is less than the minimum value specified by the schema](#a-number-which-is-less-than-the-minimum-value-specified-by-the-schema)
- [a number which is not a multiple of the factor desired by the schema](#a-number-which-is-not-a-multiple-of-the-factor-desired-by-the-schema)
- [a whole number against a schema which accepts any numbers](#a-whole-number-against-a-schema-which-accepts-any-numbers)
- [an array containing a mixture of null and boolean values to a schema accepting only arrays of nulls](#an-array-containing-a-mixture-of-null-and-boolean-values-to-a-schema-accepting-only-arrays-of-nulls)
- [an array containing duplicated strings against a schema not accepting duplicates](#an-array-containing-duplicated-strings-against-a-schema-not-accepting-duplicates)
- [the null value against a schema accepting only nulls](#the-null-value-against-a-schema-accepting-only-nulls)
---
### a boolean value against a schema accepting only nulls and strings
A boolean value does not conform to the schema as only null or string values do.

#### Input
##### JSON schema
```json
{
  "type": [
    "null",
    "string"
  ]
}
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
---
### a boolean value against a schema accepting only nulls
A boolean value does not conform to the schema as only null values do.

#### Input
##### JSON schema
```json
{
  "type": [
    "null"
  ]
}
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
---
### the null value against a schema accepting only nulls
A null value conforms to the schema.

#### Input
##### JSON schema
```json
{
  "type": [
    "null"
  ]
}
```
##### JSON
```json
null
```
#### Output
```
✓ no violations
```
---
### a whole number against a schema which accepts any numbers
All whole number values conform to the schema as every integer is a number.

#### Input
##### JSON schema
```json
{
  "type": [
    "number"
  ]
}
```
##### JSON
```json
1
```
#### Output
```
✓ no violations
```
---
### a number which is equal to the maximum value specified by the schema
Because maximum constraint is inclusive, such a value is valid.

#### Input
##### JSON schema
```json
{
  "maximum": 4
}
```
##### JSON
```json
4
```
#### Output
```
✓ no violations
```
---
### a number which is equal to the exclusive maximum value specified by the schema
Because the exclusiveMaximum constraint is exclusive, such a value is invalid.

#### Input
##### JSON schema
```json
{
  "exclusiveMaximum": 4
}
```
##### JSON
```json
4
```
#### Output
```
✗
  Schema path: #/exclusiveMaximum
  JSON path: $
  4.0 is outside of the valid range of (-Infinity,4.0)
```
---
### a number which is equal to the exclusive minimum value specified by the schema
Because the exclusiveMinimum constraint is exclusive, such a value is not valid.

#### Input
##### JSON schema
```json
{
  "exclusiveMinimum": 4
}
```
##### JSON
```json
4
```
#### Output
```
✗
  Schema path: #/exclusiveMinimum
  JSON path: $
  4.0 is outside of the valid range of (4.0,Infinity)
```
---
### a number which is equal to the minimum value specified by the schema
Because the minimum constraint is inclusive, such a value is invalid.

#### Input
##### JSON schema
```json
{
  "minimum": 4
}
```
##### JSON
```json
4
```
#### Output
```
✓ no violations
```
---
### a number which is greater than the maximum value specified by the schema
Because the value is out of the valid range, it is invalid.

#### Input
##### JSON schema
```json
{
  "maximum": 4
}
```
##### JSON
```json
5
```
#### Output
```
✗
  Schema path: #/maximum
  JSON path: $
  5.0 is outside of the valid range of (-Infinity,4.0]
```
---
### a number which is less than the minimum value specified by the schema
Because the value is out of the valid range, it is invalid.

#### Input
##### JSON schema
```json
{
  "minimum": 4
}
```
##### JSON
```json
3
```
#### Output
```
✗
  Schema path: #/minimum
  JSON path: $
  3.0 is outside of the valid range of [4.0,Infinity)
```
---
### a number which is less than the exclusive maximum value specified by the schema
Because the value is within the valid range, it is valid.

#### Input
##### JSON schema
```json
{
  "exclusiveMaximum": 4
}
```
##### JSON
```json
3
```
#### Output
```
✓ no violations
```
---
### a number which is greater than the minimum value specified by the schema
Because the value is within the valid range, it is valid.

#### Input
##### JSON schema
```json
{
  "exclusiveMinimum": 4
}
```
##### JSON
```json
5
```
#### Output
```
✓ no violations
```
---
### a fractional number against a schema accepting only whole numbers
Not all number values conform to the schema as not every number is a integer.

#### Input
##### JSON schema
```json
{
  "type": [
    "integer"
  ]
}
```
##### JSON
```json
1.5
```
#### Output
```
✗
  Schema path: #/type
  JSON path: $
  Invalid type. Expected integer but got number.
```
---
### a number which is not a multiple of the factor desired by the schema
Number 7 does not conform the schema as 7.5 is not a multiple of 2.5.

#### Input
##### JSON schema
```json
{
  "multipleOf": 2.5,
  "type": [
    "number"
  ]
}
```
##### JSON
```json
7
```
#### Output
```
✗
  Schema path: #/multipleOf
  JSON path: $
  7.0 is not a multiple of 2.5
```
---
### a number which is a multiple of the factor desired by the schema
Number 7.5 conforms to the schema as 7.5 is 2.5 times 3.

#### Input
##### JSON schema
```json
{
  "multipleOf": 2.5,
  "type": [
    "number"
  ]
}
```
##### JSON
```json
7.5
```
#### Output
```
✓ no violations
```
---
### an array containing duplicated strings against a schema not accepting duplicates
When schema requires items to be unique, any duplicate occurrence of any value will cause a validation failure.

#### Input
##### JSON schema
```json
{
  "uniqueItems": true
}
```
##### JSON
```json
[
  "a",
  "b",
  "b",
  "c",
  "d",
  "d",
  "e"
]
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
---
### an array containing a mixture of null and boolean values to a schema accepting only arrays of nulls
When schema requires items to conform to a certain schema, every single value in the array has to.

#### Input
##### JSON schema
```json
{
  "items": {
    "type": [
      "null"
    ]
  },
  "type": [
    "array"
  ]
}
```
##### JSON
```json
[
  null,
  false,
  null,
  true,
  null
]
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
