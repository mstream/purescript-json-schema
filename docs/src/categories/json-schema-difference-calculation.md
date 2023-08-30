
# JSON Schema Difference Calculation

Calculating JSON Schema Difference is a process used to identify the changes between two JSON schemata.

It is used to to see what has been added, removed, or changed.

This is useful for tracking changes over time, understanding the impact of changes, and managing versions of a schema.

It can also be used to generate a diff report or to automate the process of updating dependent systems or documentation when a schema changes.




## Properties

- comparing two identical schemata should give no differences



## Examples of comparing

- [identical schemata](#identical-schemata)
- [schema expecting lower (exclusive) maximum value to schema expecting higher (exclusive) maximum value](#schema-expecting-lower-exclusive-maximum-value-to-schema-expecting-higher-exclusive-maximum-value)
- [schema expecting lower (exclusive) minimum value to schema expecting higher (exclusive) minimum value](#schema-expecting-lower-exclusive-minimum-value-to-schema-expecting-higher-exclusive-minimum-value)
- [schema expecting lower (inclusive) maximum value to schema expecting higher (inclusive) maximum value](#schema-expecting-lower-inclusive-maximum-value-to-schema-expecting-higher-inclusive-maximum-value)
- [schema expecting lower (inclusive) minimum value to schema expecting higher (inclusive) minimum value](#schema-expecting-lower-inclusive-minimum-value-to-schema-expecting-higher-inclusive-minimum-value)
- [schema expecting multiples of 2 to schema expecting multiples of 4](#schema-expecting-multiples-of-2-to-schema-expecting-multiples-of-4)
- [schema with expected type of null to schema with expected type of boolean](#schema-with-expected-type-of-null-to-schema-with-expected-type-of-boolean)
---

### schema with expected type of null to schema with expected type of boolean

Any change in expected JSON value type should be accounted as a difference.

**Input:**

*Previous JSON schema:*

```json
{
  "type": [
    "null"
  ]
}
```
*Next JSON schema:*

```json
{
  "type": [
    "boolean"
  ]
}
```
**Output:**

```text
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

**Input:**

*Previous JSON schema:*

```json
false
```
*Next JSON schema:*

```json
false
```
**Output:**

```text
no differences
```
---

### schema expecting lower (exclusive) maximum value to schema expecting higher (exclusive) maximum value

changes of exclusiveMaximum keyword should be reported

**Input:**

*Previous JSON schema:*

```json
{
  "exclusiveMaximum": 2
}
```
*Next JSON schema:*

```json
{
  "exclusiveMaximum": 4
}
```
**Output:**

```text
-
  Schema path: #/exclusiveMaximum
  change of exclusiveMaximum from 2.0 to 4.0
```
---

### schema expecting lower (inclusive) maximum value to schema expecting higher (inclusive) maximum value

changes of maximum keyword should be reported

**Input:**

*Previous JSON schema:*

```json
{
  "maximum": 2
}
```
*Next JSON schema:*

```json
{
  "maximum": 4
}
```
**Output:**

```text
-
  Schema path: #/maximum
  change of maximum from 2.0 to 4.0
```
---

### schema expecting lower (exclusive) minimum value to schema expecting higher (exclusive) minimum value

changes of minimum keyword should be reported

**Input:**

*Previous JSON schema:*

```json
{
  "exclusiveMinimum": 2
}
```
*Next JSON schema:*

```json
{
  "exclusiveMinimum": 4
}
```
**Output:**

```text
-
  Schema path: #/exclusiveMinimum
  change of exclusiveMinimum from 2.0 to 4.0
```
---

### schema expecting lower (inclusive) minimum value to schema expecting higher (inclusive) minimum value

changes of minimum keyword should be reported

**Input:**

*Previous JSON schema:*

```json
{
  "minimum": 2
}
```
*Next JSON schema:*

```json
{
  "minimum": 4
}
```
**Output:**

```text
-
  Schema path: #/minimum
  change of minimum from 2.0 to 4.0
```
---

### schema expecting multiples of 2 to schema expecting multiples of 4

changes of multipleOf keyword should be reported

**Input:**

*Previous JSON schema:*

```json
{
  "multipleOf": 2
}
```
*Next JSON schema:*

```json
{
  "multipleOf": 4
}
```
**Output:**

```text
-
  Schema path: #/multipleOf
  change of multipleOf from 2.0 to 4.0
```
