## JSON Values Validation
TODO



Examples:

- [A null value against a schema accepting only null values](#a-null-value-against-a-schema-accepting-only-null-values)
- [A boolean value against a schema accepting only null values](#a-boolean-value-against-a-schema-accepting-only-null-values)
- [An whole number value against a schema accepting only numbers](#an-whole-number-value-against-a-schema-accepting-only-numbers)
- [A fractional number value against a schema accepting only integers](#a-fractional-number-value-against-a-schema-accepting-only-integers)
- [A boolean value against a schema accepting only null and string values](#a-boolean-value-against-a-schema-accepting-only-null-and-string-values)
- [An array with 2 out of 5 items not matching the desired item type](#an-array-with-2-out-of-5-items-not-matching-the-desired-item-type)
- [An array with forbidden duplicate value.](#an-array-with-forbidden-duplicate-value)
- [Number 7.5 against a schema accepting only multiples of 2.5](#number-75-against-a-schema-accepting-only-multiples-of-25)
- [Number 7 against a schema accepting only multiples of 2.5](#number-7-against-a-schema-accepting-only-multiples-of-25)
- [A JSON number value is matches exactly the maximum keyword value](#a-json-number-value-is-matches-exactly-the-maximum-keyword-value)
- [A JSON number value is greater than the maximum keyword value](#a-json-number-value-is-greater-than-the-maximum-keyword-value)
- [A JSON number value is less than the exclusiveMaximum keyword value](#a-json-number-value-is-less-than-the-exclusivemaximum-keyword-value)
- [A JSON number value is matches exactly the exclusiveMaximum keyword value](#a-json-number-value-is-matches-exactly-the-exclusivemaximum-keyword-value)
- [A JSON number value is matches exactly the minimum keyword value](#a-json-number-value-is-matches-exactly-the-minimum-keyword-value)
- [A JSON number value is less than the minimum keyword value](#a-json-number-value-is-less-than-the-minimum-keyword-value)
- [A JSON number value is greater than the exclusiveMinimum keyword value](#a-json-number-value-is-greater-than-the-exclusiveminimum-keyword-value)
- [A JSON number value is matches exactly the exclusiveMinimum keyword value](#a-json-number-value-is-matches-exactly-the-exclusiveminimum-keyword-value)
---
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
```
✓ no violations
```
---
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
```
✗
  Schema path: #/type
  JSON path: $
  Invalid type. Expected null but got boolean.
```
---
### An whole number value against a schema accepting only numbers
All whole number values conform to the schema as every integer is a number.

#### Input
##### JSON schema
```json
{"type":["number"]}
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
### A fractional number value against a schema accepting only integers
Not all number values conform to the schema as not every number is a integer.

#### Input
##### JSON schema
```json
{"type":["integer"]}
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
### A boolean value against a schema accepting only null and string values
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
---
### An array with 2 out of 5 items not matching the desired item type
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
---
### An array with forbidden duplicate value.
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
---
### Number 7.5 against a schema accepting only multiples of 2.5
Number 7.5 conforms to the schema as 7.5 is 2.5 times 3.

#### Input
##### JSON schema
```json
{"multipleOf":2.5,"type":["number"]}
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
### Number 7 against a schema accepting only multiples of 2.5
Number 7 does not conform the schema as 7.5 is not a multiple of 2.5.

#### Input
##### JSON schema
```json
{"multipleOf":2.5,"type":["number"]}
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
### A JSON number value is matches exactly the maximum keyword value
Because maximum constraint is inclusive, such a value is valid.

#### Input
##### JSON schema
```json
{"maximum":4}
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
### A JSON number value is greater than the maximum keyword value
Because the value is out of the valid range, it is invalid.

#### Input
##### JSON schema
```json
{"maximum":4}
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
### A JSON number value is less than the exclusiveMaximum keyword value
Because the value is within the valid range, it is valid.

#### Input
##### JSON schema
```json
{"exclusiveMaximum":4}
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
### A JSON number value is matches exactly the exclusiveMaximum keyword value
Because the exclusiveMaximum constraint is exclusive, such a value is invalid.

#### Input
##### JSON schema
```json
{"exclusiveMaximum":4}
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
### A JSON number value is matches exactly the minimum keyword value
Because the minimum constraint is inclusive, such a value is invalid.

#### Input
##### JSON schema
```json
{"minimum":4}
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
### A JSON number value is less than the minimum keyword value
Because the value is out of the valid range, it is invalid.

#### Input
##### JSON schema
```json
{"minimum":4}
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
### A JSON number value is greater than the exclusiveMinimum keyword value
Because the value is within the valid range, it is valid.

#### Input
##### JSON schema
```json
{"exclusiveMinimum":4}
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
### A JSON number value is matches exactly the exclusiveMinimum keyword value
Because the exclusiveMinimum constraint is exclusive, such a value is not valid.

#### Input
##### JSON schema
```json
{"exclusiveMinimum":4}
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
