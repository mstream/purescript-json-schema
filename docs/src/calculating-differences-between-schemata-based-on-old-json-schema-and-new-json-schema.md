# Calculating differences between schemata based on old JSON schema and new JSON schema

## Schema

```mermaid
flowchart LR
    subgraph inputs
        input_desc_0["new JSON schema"]
        input_desc_1["old JSON schema"]
    end
    subgraph output
        output_desc["differences between schemata"]
    end
    inputs --> output
```

## Context

Calculating JSON Schema Difference is a process used to identify the
changes between two JSON schemata.\
It is used to to see what has been added, removed, or changed.\
This is useful for tracking changes over time, understanding the impact
of changes, and managing versions of a schema.\
It can also be used to generate a diff report or to automate the process
of updating dependent systems or documentation when a schema changes.

## Properties

- comparing identical schemata yields no differences

- comparing different schemata yields differences

## Examples

- [calculating differences between schemata based on JSON schema accepting only number less than or equal to some number and JSON schema accepting only number less than or equal to other number](#calculating-differences-between-schemata-based-on-json-schema-accepting-only-number-less-than-or-equal-to-some-number-and-json-schema-accepting-only-number-less-than-or-equal-to-other-number)

- [calculating differences between schemata based on JSON schema accepting only number greater than or equal to some number and JSON schema accepting only number greater than or equal to other number](#calculating-differences-between-schemata-based-on-json-schema-accepting-only-number-greater-than-or-equal-to-some-number-and-json-schema-accepting-only-number-greater-than-or-equal-to-other-number)

- [calculating differences between schemata based on JSON schema accepting only multiples of a some number and JSON schema accepting only multiples of other number](#calculating-differences-between-schemata-based-on-json-schema-accepting-only-multiples-of-a-some-number-and-json-schema-accepting-only-multiples-of-other-number)

- [calculating differences between schemata based on JSON schema accepting only number greater than some number and JSON schema accepting only number greater than other number](#calculating-differences-between-schemata-based-on-json-schema-accepting-only-number-greater-than-some-number-and-json-schema-accepting-only-number-greater-than-other-number)

- [calculating differences between schemata based on JSON schema accepting only some type and JSON schema accepting only other type](#calculating-differences-between-schemata-based-on-json-schema-accepting-only-some-type-and-json-schema-accepting-only-other-type)

- [calculating differences between schemata based on JSON schema accepting only number less than some number and JSON schema accepting only number less than other number](#calculating-differences-between-schemata-based-on-json-schema-accepting-only-number-less-than-some-number-and-json-schema-accepting-only-number-less-than-other-number)

- [calculating differences between schemata based on same schema and some schema](#calculating-differences-between-schemata-based-on-same-schema-and-some-schema)

---

### calculating differences between schemata based on JSON schema accepting only number less than or equal to some number and JSON schema accepting only number less than or equal to other number

Any change of maximum inclusive keyword value  is considered a
difference.

_Input:_

JSON schema accepting only number less than or equal to other number:

> ```json
> {
>   "maximum": 4
> }
> ```

JSON schema accepting only number less than or equal to some number:

> ```json
> {
>   "maximum": 2
> }
> ```

_Output:_

a change in inclusive maximum value:

> - JSON schema path: #/maximum
>
>   change of maximum from 2.0 to 4.0

---

### calculating differences between schemata based on JSON schema accepting only number greater than or equal to some number and JSON schema accepting only number greater than or equal to other number

Any change of minimum inclusive keyword value is considered a
difference.

_Input:_

JSON schema accepting only number greater than or equal to other number:

> ```json
> {
>   "minimum": 4
> }
> ```

JSON schema accepting only number greater than or equal to some number:

> ```json
> {
>   "minimum": 2
> }
> ```

_Output:_

a change in inclusive minimum value:

> - JSON schema path: #/minimum
>
>   change of minimum from 2.0 to 4.0

---

### calculating differences between schemata based on JSON schema accepting only multiples of a some number and JSON schema accepting only multiples of other number

Any change of multipleOf keyword is considered a difference.

_Input:_

JSON schema accepting only multiples of other number:

> ```json
> {
>   "multipleOf": 4
> }
> ```

JSON schema accepting only multiples of a some number:

> ```json
> {
>   "multipleOf": 2
> }
> ```

_Output:_

a change in accepted value factor:

> - JSON schema path: #/multipleOf
>
>   change of multipleOf from 2.0 to 4.0

---

### calculating differences between schemata based on JSON schema accepting only number greater than some number and JSON schema accepting only number greater than other number

Any change of minimum exclusive keyword value  is considered a
difference.

_Input:_

JSON schema accepting only number greater than other number:

> ```json
> {
>   "exclusiveMinimum": 4
> }
> ```

JSON schema accepting only number greater than some number:

> ```json
> {
>   "exclusiveMinimum": 2
> }
> ```

_Output:_

a change in exclusive minimum value:

> - JSON schema path: #/exclusiveMinimum
>
>   change of exclusiveMinimum from 2.0 to 4.0

---

### calculating differences between schemata based on JSON schema accepting only some type and JSON schema accepting only other type

Any change of expected JSON value type  is considered a difference.

_Input:_

JSON schema accepting only other type:

> ```json
> {
>   "type": [
>     "boolean"
>   ]
> }
> ```

JSON schema accepting only some type:

> ```json
> {
>   "type": [
>     "null"
>   ]
> }
> ```

_Output:_

a change in accepted value type:

> - JSON schema path: #/type
>
>   change of accepted JSON value types from
>
>   - null
>
>   to
>
>   - boolean

---

### calculating differences between schemata based on JSON schema accepting only number less than some number and JSON schema accepting only number less than other number

Any change of maximum exclusive keyword value  is considered a
difference.

_Input:_

JSON schema accepting only number less than other number:

> ```json
> {
>   "exclusiveMaximum": 4
> }
> ```

JSON schema accepting only number less than some number:

> ```json
> {
>   "exclusiveMaximum": 2
> }
> ```

_Output:_

a change in exclusive maximum value:

> - JSON schema path: #/exclusiveMaximum
>
>   change of exclusiveMaximum from 2.0 to 4.0

---

### calculating differences between schemata based on same schema and some schema

Comparison of two identical schemata yields no differences.

_Input:_

some schema:

> ```json
> false
> ```

same schema:

> ```json
> false
> ```

_Output:_

no differences:

> ∅