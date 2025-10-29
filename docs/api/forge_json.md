# JSON Support

Advanced JSON parsing, generation, and manipulation for ForGE Qt.

## Overview

ForGE provides comprehensive JSON support including parsing, serialization, document handling, and error reporting.

## QJsonValue

Variant type for JSON values.

### Methods

- `is_null()` - Check if null
- `is_bool()` - Check if boolean
- `is_number()` - Check if number
- `is_string()` - Check if string
- `is_array()` - Check if array
- `is_object()` - Check if object
- `to_bool(default)` - Convert to boolean
- `to_number(default)` - Convert to number
- `to_string(default)` - Convert to string
- `to_array()` - Convert to array
- `to_object()` - Convert to object
- `set_bool(value)` - Set boolean value
- `set_number(value)` - Set number value
- `set_string(value)` - Set string value
- `set_array(value)` - Set array value
- `set_object(value)` - Set object value

## QJsonObject

JSON object (key-value pairs).

### Methods

- `insert(key, value)` - Insert key-value pair
- `remove(key)` - Remove key-value pair
- `contains(key)` - Check if key exists
- `value(key)` - Get value for key
- `size()` - Get number of properties
- `keys()` - Get all keys
- `is_empty()` - Check if empty
- `begin()` - Iterator begin
- `end()` - Iterator end
- `key_at(index)` - Get key at index
- `value_at(index)` - Get value at index

## QJsonArray

JSON array.

### Methods

- `append(value)` - Append value
- `insert(index, value)` - Insert at position
- `remove(index)` - Remove at position
- `at(index)` - Get value at index
- `size()` - Get array size
- `is_empty()` - Check if empty
- `begin()` - Iterator begin
- `end()` - Iterator end

## QJsonDocument

JSON document container.

### Methods

- `is_array()` - Check if document is array
- `is_object()` - Check if document is object
- `is_empty()` - Check if document is empty
- `is_null()` - Check if document is null
- `array()` - Get as array
- `object()` - Get as object
- `set_array(array)` - Set from array
- `set_object(object)` - Set from object
- `to_json(format)` - Serialize to JSON string
- `from_json(json_string, error)` - Parse from JSON string

## QJsonParseError

JSON parsing error information.

### Methods

- `is_null()` - Check if no error
- `to_string()` - Get error message
- `error` - Error code
- `error_string` - Error message
- `offset` - Character offset of error

## QJsonSerializer

Custom JSON serialization.

### Methods

- `serialize(value)` - Serialize value
- `set_compact_format(compact)` - Set compact/expanded format
- `set_indent_size(size)` - Set indentation size
- `set_keep_null(keep)` - Keep null values in output

## Global Functions

- `parse_json(json_string)` - Parse JSON string to QJsonValue
- `parse_json_document(json_string, error)` - Parse JSON string to QJsonDocument
- `json_to_string(value, pretty)` - Convert QJsonValue to JSON string

## Example

```fortran
use forge_json

type(QJsonDocument) :: doc
type(QJsonObject) :: obj
type(QJsonArray) :: arr
type(QJsonParseError) :: error
logical :: success

! Parse JSON
success = doc%from_json('{"name": "John", "scores": [85, 92]}', error)

if (success) then
    obj = doc%object()
    print *, "Name:", obj%value("name")%to_string()

    arr = obj%value("scores")%to_array()
    print *, "First score:", arr%at(0)%to_number()
end if

! Create JSON
call obj%insert("age", QJsonValue())
call obj%value("age")%set_number(30.0)

call doc%set_object(obj)
print *, doc%to_json(.true.)
```

## Error Handling

All JSON operations include comprehensive error handling with detailed error messages and position information for parsing errors.