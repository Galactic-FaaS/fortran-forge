# QXmlStreamReader/QXmlStreamWriter

High-performance XML streaming parser and writer for ForGE Qt.

## Overview

The XML streaming classes provide fast, low-memory XML processing suitable for large documents or streaming scenarios.

## QXmlStreamReader

High-performance XML stream reader with token-based parsing.

### Methods

- `set_data(data)` - Set XML data to parse
- `read_next()` - Read next token
- `token_type()` - Get current token type
- `token_string()` - Get token type as string
- `name()` - Get element/attribute name
- `text()` - Get text content
- `attributes()` - Get element attributes
- `is_whitespace()` - Check if current token is whitespace
- `is_cdata()` - Check if current token is CDATA
- `at_end()` - Check if at end of document
- `has_error()` - Check if parsing error occurred
- `error_string()` - Get error message
- `line_number()` - Get current line number
- `column_number()` - Get current column number
- `character_offset()` - Get character offset

### Token Types

- `StartDocument` - XML declaration
- `EndDocument` - End of document
- `StartElement` - Start tag
- `EndElement` - End tag
- `Characters` - Text content
- `Comment` - XML comment
- `DTD` - Document type definition
- `EntityReference` - Entity reference
- `ProcessingInstruction` - Processing instruction

## QXmlStreamWriter

XML stream writer for generating XML documents.

### Methods

- `set_auto_formatting(enabled)` - Enable/disable automatic formatting
- `set_auto_formatting_enabled(enabled)` - Alternative method
- `write_start_document(version, encoding, standalone)` - Write XML declaration
- `write_end_document()` - End document
- `write_start_element(qualified_name)` - Write start tag
- `write_end_element()` - Write end tag
- `write_empty_element(qualified_name)` - Write self-closing tag
- `write_attribute(qualified_name, value)` - Write attribute
- `write_namespace(namespace_uri, prefix)` - Write namespace declaration
- `write_default_namespace(namespace_uri)` - Write default namespace
- `write_characters(text)` - Write text content
- `write_cdata(text)` - Write CDATA section
- `write_comment(text)` - Write comment
- `write_processing_instruction(target, data)` - Write processing instruction
- `write_dtd(dtd)` - Write DTD
- `write_entity_reference(name)` - Write entity reference
- `get_buffer()` - Get generated XML
- `has_error()` - Check for errors
- `error_string()` - Get error message

## Example

```fortran
use forge_xml_stream

type(QXmlStreamReader) :: reader
type(QXmlStreamWriter) :: writer

! Parse XML
call reader%set_data('<root><item id="1">Hello</item></root>')

do while (reader%read_next())
    if (reader%token_type() == StartElement) then
        print *, "Element:", reader%name()
    end if
end do

! Generate XML
call writer%set_auto_formatting(.true.)
call writer%write_start_document()
call writer%write_start_element("root")
call writer%write_start_element("item")
call writer%write_attribute("id", "1")
call writer%write_characters("Hello")
call writer%write_end_element()
call writer%write_end_element()
call writer%write_end_document()

print *, writer%get_buffer()
```

## Performance Notes

- Minimal memory usage for large documents
- Fast parsing suitable for streaming
- No DOM tree building
- Event-driven processing