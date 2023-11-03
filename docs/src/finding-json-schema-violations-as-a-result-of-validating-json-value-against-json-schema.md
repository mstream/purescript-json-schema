# Finding JSON schema violations as a result of validating JSON value against JSON schema

JSON validation is a specification for validating the structure and data
types of JSON values.\
It allows you to specify the required properties, the types of values,
the format of the data, and other constraints for a JSON object.\
This is useful for ensuring that the data received or sent in a JSON
format is as expected and can be processed correctly.\
It helps to catch errors early, improve data quality, and reduce the
amount of code needed for data validation.

## Properties

- 'true' JSON schema does not impose any constraints

- 'false' JSON schema rejects anything

- any JSON value passes validation against 'empty object' JSON schema

## Examples

- [finding JSON schema violations as a result of validating an array containing some duplicated strings against schema not accepting duplicates](#finding-json-schema-violations-as-a-result-of-validating-an-array-containing-some-duplicated-strings-against-schema-not-accepting-duplicates)

- [finding JSON schema violations as a result of validating a boolean value against schema accepting only nulls or strings](#finding-json-schema-violations-as-a-result-of-validating-a-boolean-value-against-schema-accepting-only-nulls-or-strings)

- [finding JSON schema violations as a result of validating JSON number value against JSON schema accepting only numbers](#finding-json-schema-violations-as-a-result-of-validating-json-number-value-against-json-schema-accepting-only-numbers)

- [finding JSON schema violations as a result of validating JSON null value against JSON schema accepting booleans, nulls and strings](#finding-json-schema-violations-as-a-result-of-validating-json-null-value-against-json-schema-accepting-booleans-nulls-and-strings)

- [finding JSON schema violations as a result of validating JSON number value which happens to be an integer against JSON schema accepting any numbers](#finding-json-schema-violations-as-a-result-of-validating-json-number-value-which-happens-to-be-an-integer-against-json-schema-accepting-any-numbers)

- [finding JSON schema violations as a result of validating a multiple of x against schema accepting only numbers which are multiples of x](#finding-json-schema-violations-as-a-result-of-validating-a-multiple-of-x-against-schema-accepting-only-numbers-which-are-multiples-of-x)

- [finding JSON schema violations as a result of validating a fractional number against schema accepting only whole numbers](#finding-json-schema-violations-as-a-result-of-validating-a-fractional-number-against-schema-accepting-only-whole-numbers)

- [finding JSON schema violations as a result of validating a multiple of 2.5 against a schema accepting only multiples of 2.5](#finding-json-schema-violations-as-a-result-of-validating-a-multiple-of-25-against-a-schema-accepting-only-multiples-of-25)

- [finding JSON schema violations as a result of validating an array containing a mixture of null and boolean values to a schema accepting only arrays of nulls against schema accepting only arrays of nulls](#finding-json-schema-violations-as-a-result-of-validating-an-array-containing-a-mixture-of-null-and-boolean-values-to-a-schema-accepting-only-arrays-of-nulls-against-schema-accepting-only-arrays-of-nulls)

- [finding JSON schema violations as a result of validating number at the schema's minimum allowed values boundary against a schema with a minimum exclusive allowed value set](#finding-json-schema-violations-as-a-result-of-validating-number-at-the-schemas-minimum-allowed-values-boundary-against-a-schema-with-a-minimum-exclusive-allowed-value-set)

- [finding JSON schema violations as a result of validating number at the schema's maximum allowed values boundary against a schema with a maximum exclusive allowed value set](#finding-json-schema-violations-as-a-result-of-validating-number-at-the-schemas-maximum-allowed-values-boundary-against-a-schema-with-a-maximum-exclusive-allowed-value-set)

- [finding JSON schema violations as a result of validating not a multiple of 2.5 against a schema accepting only multiples of 2.5](#finding-json-schema-violations-as-a-result-of-validating-not-a-multiple-of-25-against-a-schema-accepting-only-multiples-of-25)

- [finding JSON schema violations as a result of validating number at the schema's maximum allowed values boundary against a schema with a maximum inclusive allowed value set](#finding-json-schema-violations-as-a-result-of-validating-number-at-the-schemas-maximum-allowed-values-boundary-against-a-schema-with-a-maximum-inclusive-allowed-value-set)

- [finding JSON schema violations as a result of validating number below the schema's maximum allowed values boundary against a schema with a maximum exclusive allowed value set](#finding-json-schema-violations-as-a-result-of-validating-number-below-the-schemas-maximum-allowed-values-boundary-against-a-schema-with-a-maximum-exclusive-allowed-value-set)

- [finding JSON schema violations as a result of validating number exceeding the schema's maximum allowed values boundary against a schema with a maximum inclusive allowed value set](#finding-json-schema-violations-as-a-result-of-validating-number-exceeding-the-schemas-maximum-allowed-values-boundary-against-a-schema-with-a-maximum-inclusive-allowed-value-set)

- [finding JSON schema violations as a result of validating number at the schema's minimum allowed values boundary against a schema with a minimum inclusive allowed value set](#finding-json-schema-violations-as-a-result-of-validating-number-at-the-schemas-minimum-allowed-values-boundary-against-a-schema-with-a-minimum-inclusive-allowed-value-set)

- [finding JSON schema violations as a result of validating number below the schema's minimum allowed values boundary against a schema with a minimum exclusive allowed value set](#finding-json-schema-violations-as-a-result-of-validating-number-below-the-schemas-minimum-allowed-values-boundary-against-a-schema-with-a-minimum-exclusive-allowed-value-set)

- [finding JSON schema violations as a result of validating number exceeding the schema's minimum allowed values boundary against a schema with a minimum inclusive allowed value set](#finding-json-schema-violations-as-a-result-of-validating-number-exceeding-the-schemas-minimum-allowed-values-boundary-against-a-schema-with-a-minimum-inclusive-allowed-value-set)

---

### finding JSON schema violations as a result of validating an array containing some duplicated strings against schema not accepting duplicates

Because the schema requires items to be unique, and the value contains
duplicate occurrence, such a value is invalid.

_Input:_

an array containing some duplicated strings:

> ```json
> [
>   "a",
>   "b",
>   "b",
>   "c",
>   "d",
>   "d",
>   "e"
> ]
> ```

schema not accepting duplicates:

> ```json
> {
>   "uniqueItems": true
> }
> ```

_Output:_

an invalid array violation:

> - JSON value path: `$`\
>   JSON schema path: `#`
>
>   Invalid array:
>
>   - JSON value path: `$[1]`\
>     JSON schema path: `#/uniqueItems`
>
>     Non-unique array item.
>
>   - JSON value path: `$[2]`\
>     JSON schema path: `#/uniqueItems`
>
>     Non-unique array item.
>
>   - JSON value path: `$[4]`\
>     JSON schema path: `#/uniqueItems`
>
>     Non-unique array item.
>
>   - JSON value path: `$[5]`\
>     JSON schema path: `#/uniqueItems`
>
>     Non-unique array item.

---

### finding JSON schema violations as a result of validating a boolean value against schema accepting only nulls or strings

Because the value is neither null or string, such a value is invalid.

_Input:_

a boolean value:

> ```json
> true
> ```

schema accepting only nulls or strings:

> ```json
> {
>   "type": [
>     "null",
>     "string"
>   ]
> }
> ```

_Output:_

a type mismatch violation:

> - JSON value path: `$`\
>   JSON schema path: `#/type`
>
>   Invalid type. Expected nullorstring but got boolean.

---

### finding JSON schema violations as a result of validating JSON number value against JSON schema accepting only numbers

Because a JSON value directly matches schema's only 'type' keyword item,
such a value is valid.

_Input:_

JSON number value:

> ```json
> 2.5
> ```

JSON schema accepting only numbers:

> ```json
> {
>   "type": [
>     "number"
>   ]
> }
> ```

_Output:_

no violations:

> ∅

---

### finding JSON schema violations as a result of validating JSON null value against JSON schema accepting booleans, nulls and strings

Because a JSON value directly matches one of schema's 'type' keyword
items, such a value is valid.

_Input:_

JSON null value:

> ```json
> null
> ```

JSON schema accepting booleans, nulls and strings:

> ```json
> {
>   "type": [
>     "boolean",
>     "null",
>     "string"
>   ]
> }
> ```

_Output:_

no violations:

> ∅

---

### finding JSON schema violations as a result of validating JSON number value which happens to be an integer against JSON schema accepting any numbers

Because a JSON value indirectly matches schema's only 'type' keyword
item, such a value is valid.

_Input:_

JSON number value which happens to be an integer:

> ```json
> 1
> ```

JSON schema accepting any numbers:

> ```json
> {
>   "type": [
>     "number"
>   ]
> }
> ```

_Output:_

no violations:

> ∅

---

### finding JSON schema violations as a result of validating a multiple of x against schema accepting only numbers which are multiples of x

Because a JSON number value is a multiple of the factor desired by the
schema, such a value is valid.

_Input:_

a multiple of x:

> ```json
> 7.5
> ```

schema accepting only numbers which are multiples of x:

> ```json
> {
>   "multipleOf": 2.5,
>   "type": [
>     "number"
>   ]
> }
> ```

_Output:_

no violations:

> ∅

---

### finding JSON schema violations as a result of validating a fractional number against schema accepting only whole numbers

Because the schema accepts only whole numbers, such a value is invalid.

_Input:_

a fractional number:

> ```json
> 1.5
> ```

schema accepting only whole numbers:

> ```json
> {
>   "type": [
>     "integer"
>   ]
> }
> ```

_Output:_

a type mismatch violation:

> - JSON value path: `$`\
>   JSON schema path: `#/type`
>
>   Invalid type. Expected integer but got number.

---

### finding JSON schema violations as a result of validating a multiple of 2.5 against a schema accepting only multiples of 2.5

Because the schema accepts any multiples of 2.5, such a value is valid.

_Input:_

a multiple of 2.5:

> ```json
> 7.5
> ```

a schema accepting only multiples of 2.5:

> ```json
> {
>   "multipleOf": 2.5,
>   "type": [
>     "number"
>   ]
> }
> ```

_Output:_

no violations:

> ∅

---

### finding JSON schema violations as a result of validating an array containing a mixture of null and boolean values to a schema accepting only arrays of nulls against schema accepting only arrays of nulls

Because the schema requires items to conform to a certain schema and
this is not the case here, such a value is invalid.

_Input:_

an array containing a mixture of null and boolean values to a schema
accepting only arrays of nulls:

> ```json
> [
>   null,
>   false,
>   null,
>   true,
>   null
> ]
> ```

schema accepting only arrays of nulls:

> ```json
> {
>   "items": {
>     "type": [
>       "null"
>     ]
>   },
>   "type": [
>     "array"
>   ]
> }
> ```

_Output:_

an invalid array violation:

> - JSON value path: `$`\
>   JSON schema path: `#`
>
>   Invalid array:
>
>   - JSON value path: `$[1]`\
>     JSON schema path: `#/items/type`
>
>     Invalid type. Expected null but got boolean.
>
>   - JSON value path: `$[3]`\
>     JSON schema path: `#/items/type`
>
>     Invalid type. Expected null but got boolean.

---

### finding JSON schema violations as a result of validating number at the schema's minimum allowed values boundary against a schema with a minimum exclusive allowed value set

Because the minimum value constraint is exclusive, such a value is
invalid.

_Input:_

number at the schema's minimum allowed values boundary:

> ```json
> 4
> ```

a schema with a minimum exclusive allowed value set:

> ```json
> {
>   "exclusiveMinimum": 4
> }
> ```

_Output:_

an invalid range violation:

> - JSON value path: `$`\
>   JSON schema path: `#/exclusiveMinimum`
>
>   4.0 is outside of the valid range of (4.0,Infinity)

---

### finding JSON schema violations as a result of validating number at the schema's maximum allowed values boundary against a schema with a maximum exclusive allowed value set

Because the maximum value constraint is exclusive, such a value is
invalid.

_Input:_

number at the schema's maximum allowed values boundary:

> ```json
> 4
> ```

a schema with a maximum exclusive allowed value set:

> ```json
> {
>   "exclusiveMaximum": 4
> }
> ```

_Output:_

an invalid range violation:

> - JSON value path: `$`\
>   JSON schema path: `#/exclusiveMaximum`
>
>   4.0 is outside of the valid range of (-Infinity,4.0)

---

### finding JSON schema violations as a result of validating not a multiple of 2.5 against a schema accepting only multiples of 2.5

Because the schema accepts only multiples of 2.5, such a value is
invalid.

_Input:_

not a multiple of 2.5:

> ```json
> 7
> ```

a schema accepting only multiples of 2.5:

> ```json
> {
>   "multipleOf": 2.5,
>   "type": [
>     "number"
>   ]
> }
> ```

_Output:_

an invalid multiple violation:

> - JSON value path: `$`\
>   JSON schema path: `#/multipleOf`
>
>   7.0 is not a multiple of 2.5

---

### finding JSON schema violations as a result of validating number at the schema's maximum allowed values boundary against a schema with a maximum inclusive allowed value set

Because the maximum value constraint is inclusive, such a value is
valid.

_Input:_

number at the schema's maximum allowed values boundary:

> ```json
> 4
> ```

a schema with a maximum inclusive allowed value set:

> ```json
> {
>   "maximum": 4
> }
> ```

_Output:_

no violations:

> ∅

---

### finding JSON schema violations as a result of validating number below the schema's maximum allowed values boundary against a schema with a maximum exclusive allowed value set

Because the value is less than the maximum value constraint, such a
value is valid.

_Input:_

number below the schema's maximum allowed values boundary:

> ```json
> 3
> ```

a schema with a maximum exclusive allowed value set:

> ```json
> {
>   "exclusiveMaximum": 4
> }
> ```

_Output:_

no violations:

> ∅

---

### finding JSON schema violations as a result of validating number exceeding the schema's maximum allowed values boundary against a schema with a maximum inclusive allowed value set

Because the value is greater than the maximum value constraint is
exclusive, such a value is invalid.

_Input:_

number exceeding the schema's maximum allowed values boundary:

> ```json
> 5
> ```

a schema with a maximum inclusive allowed value set:

> ```json
> {
>   "maximum": 4
> }
> ```

_Output:_

an invalid range violation:

> - JSON value path: `$`\
>   JSON schema path: `#/maximum`
>
>   5.0 is outside of the valid range of (-Infinity,4.0]

---

### finding JSON schema violations as a result of validating number at the schema's minimum allowed values boundary against a schema with a minimum inclusive allowed value set

Because the minimum value constraint is inclusive, such a value is
valid.

_Input:_

number at the schema's minimum allowed values boundary:

> ```json
> 4
> ```

a schema with a minimum inclusive allowed value set:

> ```json
> {
>   "minimum": 4
> }
> ```

_Output:_

no violations:

> ∅

---

### finding JSON schema violations as a result of validating number below the schema's minimum allowed values boundary against a schema with a minimum exclusive allowed value set

Because the value is less than the minimum value constraint, such a
value is valid.

_Input:_

number below the schema's minimum allowed values boundary:

> ```json
> 5
> ```

a schema with a minimum exclusive allowed value set:

> ```json
> {
>   "exclusiveMinimum": 4
> }
> ```

_Output:_

no violations:

> ∅

---

### finding JSON schema violations as a result of validating number exceeding the schema's minimum allowed values boundary against a schema with a minimum inclusive allowed value set

Because the value is greater than the minimum value constraint is
exclusive, such a value is invalid.

_Input:_

number exceeding the schema's minimum allowed values boundary:

> ```json
> 3
> ```

a schema with a minimum inclusive allowed value set:

> ```json
> {
>   "minimum": 4
> }
> ```

_Output:_

an invalid range violation:

> - JSON value path: `$`\
>   JSON schema path: `#/minimum`
>
>   3.0 is outside of the valid range of [4.0,Infinity)
