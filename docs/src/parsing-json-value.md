# Parsing JSON value

JSON schema is commonly expressed in a JSON format.However, not every JSON is a valid JSON schema.

## Examples

- [parsing a boolean JSON value](#parsing-a-boolean-json-value)

- [parsing an JSON object with the 'not' property defined](#parsing-an-json-object-with-the-not-property-defined)

- [parsing a JSON value not being a boolean or object](#parsing-a-json-value-not-being-a-boolean-or-object)

- [parsing an JSON object with the 'items' property defined](#parsing-an-json-object-with-the-items-property-defined)

- [parsing an empty JSON object](#parsing-an-empty-json-object)

---

### parsing a boolean JSON value

A boolean value of false is a valid schema which passes validation of any JSON value

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

### parsing an JSON object with the 'not' property defined

The 'not' constrain rejects any JSON value which conform to schema defined by it.

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

### parsing a JSON value not being a boolean or object

Booleans and objects are the only acceptable forms of JSON schema.

_Input:_

a JSON value not being a boolean or object:

> ```json
> 0
> ```

_Output:_

a parsing error:

> an error:
>
> the JSON value is neither a boolean nor an object

---

### parsing an JSON object with the 'items' property defined

The 'items' constrain makes sure than if a JSON value is an array, every item of that array conforms the schema defined by it.

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

An empty JSON object is a valid schema which passes validation of any JSON value

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
