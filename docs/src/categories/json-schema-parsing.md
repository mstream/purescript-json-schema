
# JSON Schema Parsing

JSON schema is commonly expressed in a JSON format.

However, not every JSON is a valid JSON schema.




## Examples of parsing

- [A JSON object with 'required' property being array of strings](#a-json-object-with-required-property-being-array-of-strings)
- [A JSON object with 'type' property set to 'null' string](#a-json-object-with-type-property-set-to-null-string)
- [A JSON object with 'type' property set to an array with 'array', 'null' and 'string' strings inside it](#a-json-object-with-type-property-set-to-an-array-with-array-null-and-string-strings-inside-it)
- [A JSON object with 'type' property set to an array with a single 'null' string inside it](#a-json-object-with-type-property-set-to-an-array-with-a-single-null-string-inside-it)
- [A JSON object with 'type' property set to an empty array](#a-json-object-with-type-property-set-to-an-empty-array)
- [A JSON object with 'uniqueItems' property set to true](#a-json-object-with-uniqueitems-property-set-to-true)
- [a 'false' boolean value](#a-false-boolean-value)
- [a 'true' boolean value](#a-true-boolean-value)
- [a JSON object with 'not' property set to true](#a-json-object-with-not-property-set-to-true)
- [an JSON object with the 'items' property set to true](#an-json-object-with-the-items-property-set-to-true)
- [an empty JSON object](#an-empty-json-object)
---

### a 'false' boolean value

A boolean value of false is a valid schema which fails validation of any JSON value

**Input:**

*JSON:*


```json
false
```
**Output:**

```text
successfully parsed schema
```
---

### a 'true' boolean value

A boolean value of true is a valid schema which passes validation of any JSON value

**Input:**

*JSON:*


```json
true
```
**Output:**

```text
successfully parsed schema
```
---

### an empty JSON object

An empty JSON object is a valid schema which passes validation of any JSON value

**Input:**

*JSON:*


```json
{}
```
**Output:**

```text
successfully parsed schema
```
---

### an JSON object with the 'items' property set to true

The 'items' constrain makes sure than if a JSON value is an array, every item of that array conforms the schema defined by it.

**Input:**

*JSON:*


```json
{
  "items": true
}
```
**Output:**

```text
successfully parsed schema
```
---

### a JSON object with 'not' property set to true

The 'not' constrain rejects any JSON value which conform to schema defined by it.

**Input:**

*JSON:*


```json
{
  "not": true
}
```
**Output:**

```text
successfully parsed schema
```
---

### A JSON object with 'required' property being array of strings

The 'required' constrain rejects any JSON object not containing properties defined by it.

**Input:**

*JSON:*


```json
{
  "required": [
    "prop1",
    "prop2"
  ]
}
```
**Output:**

```text
successfully parsed schema
```
---

### A JSON object with 'type' property set to 'null' string

The 'type' keyword defines acceptable JSON types. It can be in a form of a single string.

**Input:**

*JSON:*


```json
{
  "type": "null"
}
```
**Output:**

```text
successfully parsed schema
```
---

### A JSON object with 'type' property set to an array with a single 'null' string inside it

The 'type' keyword defines acceptable JSON types. It can be in a form of an array of string. Here, only with one type defined.

**Input:**

*JSON:*


```json
{
  "type": [
    "null"
  ]
}
```
**Output:**

```text
successfully parsed schema
```
---

### A JSON object with 'type' property set to an empty array

The 'type' keyword defines acceptable JSON types. It can be in a form of an array of string. Here, with no types defined.

**Input:**

*JSON:*


```json
{
  "type": []
}
```
**Output:**

```text
successfully parsed schema
```
---

### A JSON object with 'type' property set to an array with 'array', 'null' and 'string' strings inside it

The 'type' keyword defines acceptable JSON types. It can be in a form of an array of string. Here, with three types defined.

**Input:**

*JSON:*


```json
{
  "type": [
    "array",
    "null",
    "string"
  ]
}
```
**Output:**

```text
successfully parsed schema
```
---

### A JSON object with 'uniqueItems' property set to true

The 'uniqueItems' keyword makes sure that if JSON value is an array, its items do not contain any duplicates

**Input:**

*JSON:*


```json
{
  "uniqueItems": true
}
```
**Output:**

```text
successfully parsed schema
```
