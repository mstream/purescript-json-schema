# Parsing JSON value

JSON schema is commonly expressed in a JSON format.However, not every JSON is a valid JSON schema.

## Examples

- [parsing a boolean JSON value](#parsing-a-boolean-json-value)

- [parsing A JSON object with 'type' property defined](#parsing-a-json-object-with-type-property-defined)

- [parsing A JSON object with 'type' property set to an empty array](#parsing-a-json-object-with-type-property-set-to-an-empty-array)

- [parsing A JSON object with 'required' property being array of strings](#parsing-a-json-object-with-required-property-being-array-of-strings)

- [parsing A JSON object with 'type' property set to an array with 'array', 'null' and 'string' strings inside it](#parsing-a-json-object-with-type-property-set-to-an-array-with-array-null-and-string-strings-inside-it)

- [parsing A JSON object with 'type' property set to an array with a single 'null' string inside it](#parsing-a-json-object-with-type-property-set-to-an-array-with-a-single-null-string-inside-it)

- [parsing A JSON object with 'uniqueItems' property set](#parsing-a-json-object-with-uniqueitems-property-set)

- [parsing a JSON value not being a boolean or object](#parsing-a-json-value-not-being-a-boolean-or-object)

- [parsing an JSON object with the 'not' property defined](#parsing-an-json-object-with-the-not-property-defined)

- [parsing an JSON object with the 'items' property defined](#parsing-an-json-object-with-the-items-property-defined)

- [parsing an empty JSON object](#parsing-an-empty-json-object)

---

### parsing a boolean JSON value

Because a boolean value of false is a valid schema which passes
validation of any JSON value, such a value represents a JSON schema.

_Input:_

a boolean JSON value:

> ```json
> false
> ```

_Output:_

a successfully parsed JSON schema:

> ```json
> false
> ```

---

### parsing A JSON object with 'type' property defined

Because the 'type' keyword defines acceptable JSON types. It can be in a
form of a single string, such a value represents a JSON schema.

_Input:_

A JSON object with 'type' property defined:

> ```json
> {
>   "type": "null"
> }
> ```

_Output:_

a successfully parsed JSON schema:

> ```json
> {
>   "type": [
>     "null"
>   ]
> }
> ```

---

### parsing A JSON object with 'type' property set to an empty array

Because the 'type' keyword defines acceptable JSON types. It can be in a
form of an array of string (here, with no types defined), such a value
represents a JSON schema.

_Input:_

A JSON object with 'type' property set to an empty array:

> ```json
> {
>   "type": []
> }
> ```

_Output:_

a successfully parsed JSON schema:

> ```json
> {
>   "type": []
> }
> ```

---

### parsing A JSON object with 'required' property being array of strings

Because the 'required' constrain rejects any JSON object not containing
properties defined by it, such a value represents a JSON schema.

_Input:_

A JSON object with 'required' property being array of strings:

> ```json
> {
>   "required": [
>     "prop1",
>     "prop2"
>   ]
> }
> ```

_Output:_

a successfully parsed JSON schema:

> ```json
> {
>   "required": [
>     "prop1",
>     "prop2"
>   ]
> }
> ```

---

### parsing A JSON object with 'type' property set to an array with 'array', 'null' and 'string' strings inside it

Because the 'type' keyword defines acceptable JSON types. It can be in a
form of an array of string (here, with three types defined), such a
value represents a JSON schema.

_Input:_

A JSON object with 'type' property set to an array with 'array', 'null'
and 'string' strings inside it:

> ```json
> {
>   "type": [
>     "array",
>     "null",
>     "string"
>   ]
> }
> ```

_Output:_

a successfully parsed JSON schema:

> ```json
> {
>   "type": [
>     "array",
>     "null",
>     "string"
>   ]
> }
> ```

---

### parsing A JSON object with 'type' property set to an array with a single 'null' string inside it

Because the 'type' keyword defines acceptable JSON types. It can be in a
form of an array of string (here, only with one type defined), such a
value represents a JSON schema.

_Input:_

A JSON object with 'type' property set to an array with a single 'null'
string inside it:

> ```json
> {
>   "type": [
>     "null"
>   ]
> }
> ```

_Output:_

a successfully parsed JSON schema:

> ```json
> {
>   "type": [
>     "null"
>   ]
> }
> ```

---

### parsing A JSON object with 'uniqueItems' property set

Because the 'uniqueItems' keyword makes sure that if JSON value is an
array, its items do not contain any duplicates, such a value represents
a JSON schema.

_Input:_

A JSON object with 'uniqueItems' property set:

> ```json
> {
>   "uniqueItems": true
> }
> ```

_Output:_

a successfully parsed JSON schema:

> ```json
> {
>   "uniqueItems": true
> }
> ```

---

### parsing a JSON value not being a boolean or object

Because booleans and objects are the only acceptable forms, such a value
does not represent a JSON schema.

_Input:_

a JSON value not being a boolean or object:

> ```json
> 0
> ```

_Output:_

a parsing error:

> an error:
>
> `"the JSON value is neither a boolean nor an object"`

---

### parsing an JSON object with the 'not' property defined

Because the 'not' constrain rejects any JSON value which conform to
schema defined by it, such a value represents a JSON schema.

_Input:_

an JSON object with the 'not' property defined:

> ```json
> {
>   "not": true
> }
> ```

_Output:_

a successfully parsed JSON schema:

> ```json
> {
>   "not": true
> }
> ```

---

### parsing an JSON object with the 'items' property defined

Because the 'items' constrain makes sure than if a JSON value is an
array, every item of that array conforms the schema defined by it, such
a value represents a JSON schema.

_Input:_

an JSON object with the 'items' property defined:

> ```json
> {
>   "items": true
> }
> ```

_Output:_

a successfully parsed JSON schema:

> ```json
> {
>   "items": true
> }
> ```

---

### parsing an empty JSON object

Because an empty JSON object is a valid schema which passes validation
of any JSON value, such a value represents a JSON schema.

_Input:_

an empty JSON object:

> ```json
> {}
> ```

_Output:_

a successfully parsed JSON schema:

> ```json
> {}
> ```
