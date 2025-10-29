# QDomDocument/QDomElement

Document Object Model (DOM) based XML processing for ForGE Qt.

## Overview

The DOM classes provide tree-based XML manipulation with full document navigation and modification capabilities.

## QDomNode

Base DOM node class.

### Node Types

- `ElementNode` - Element node
- `AttributeNode` - Attribute node
- `TextNode` - Text content node
- `CDATASectionNode` - CDATA section
- `EntityReferenceNode` - Entity reference
- `EntityNode` - Entity definition
- `ProcessingInstructionNode` - Processing instruction
- `CommentNode` - Comment node
- `DocumentNode` - Document root
- `DocumentTypeNode` - Document type
- `DocumentFragmentNode` - Document fragment
- `NotationNode` - Notation

### Methods

- `node_type()` - Get node type
- `node_name()` - Get node name
- `node_value()` - Get/set node value
- `set_node_value(value)` - Set node value
- `namespace_uri()` - Get namespace URI
- `prefix()` - Get namespace prefix
- `local_name()` - Get local name
- `has_child_nodes()` - Check for children
- `child_nodes()` - Get child nodes
- `first_child()` - Get first child
- `last_child()` - Get last child
- `next_sibling()` - Get next sibling
- `previous_sibling()` - Get previous sibling
- `parent_node()` - Get parent node
- `owner_document()` - Get owner document
- `insert_before(new, ref)` - Insert before reference
- `insert_after(new, ref)` - Insert after reference
- `replace_child(new, old)` - Replace child
- `remove_child(old)` - Remove child
- `append_child(new)` - Append child
- `clone_node(deep)` - Clone node
- `is_null()` - Check if null
- `is_element()` - Check if element
- `is_attr()` - Check if attribute
- `is_text()` - Check if text
- `is_cdata_section()` - Check if CDATA
- `is_entity_reference()` - Check if entity reference
- `is_entity()` - Check if entity
- `is_processing_instruction()` - Check if processing instruction
- `is_comment()` - Check if comment
- `is_document()` - Check if document
- `is_document_type()` - Check if document type
- `is_document_fragment()` - Check if document fragment
- `is_notation()` - Check if notation
- `to_element()` - Convert to element
- `to_attr()` - Convert to attribute
- `to_text()` - Convert to text
- `to_cdata_section()` - Convert to CDATA
- `to_processing_instruction()` - Convert to processing instruction
- `to_comment()` - Convert to comment
- `to_document()` - Convert to document
- `to_document_type()` - Convert to document type
- `to_document_fragment()` - Convert to document fragment
- `to_notation()` - Convert to notation
- `clear()` - Clear node

## QDomElement

DOM element node.

### Methods

- `tag_name()` - Get element name
- `set_tag_name(name)` - Set element name
- `attribute(name, default)` - Get attribute value
- `set_attribute(name, value)` - Set attribute
- `remove_attribute(name)` - Remove attribute
- `has_attribute(name)` - Check attribute exists
- `attribute_node(name)` - Get attribute node
- `set_attribute_node(new_attr)` - Set attribute node
- `remove_attribute_node(old_attr)` - Remove attribute node
- `attributes()` - Get all attributes
- `has_attributes()` - Check for attributes
- `namespace_uri()` - Get namespace URI
- `prefix()` - Get prefix
- `local_name()` - Get local name
- `set_prefix(prefix)` - Set prefix
- `elements_by_tag_name(tag_name)` - Find elements by tag
- `elements_by_tag_name_ns(ns_uri, local_name)` - Find elements by namespace
- `first_child_element(tag_name)` - Get first child element
- `last_child_element(tag_name)` - Get last child element
- `next_sibling_element(tag_name)` - Get next sibling element
- `previous_sibling_element(tag_name)` - Get previous sibling element
- `text()` - Get element text content
- `set_text(text)` - Set element text content
- `is_null()` - Check if null
- `node_type()` - Get node type
- `to_node()` - Convert to node

## QDomAttr

DOM attribute node.

### Methods

- `name()` - Get attribute name
- `value()` - Get attribute value
- `set_value(value)` - Set attribute value
- `namespace_uri()` - Get namespace URI
- `prefix()` - Get prefix
- `local_name()` - Get local name
- `specified()` - Check if specified

## QDomDocument

DOM document container.

### Methods

- `set_content(text, namespace_processing)` - Parse XML content
- `to_string(indent)` - Serialize to XML string
- `document_element()` - Get root element
- `create_element(tag_name)` - Create element
- `create_element_ns(ns_uri, q_name)` - Create element with namespace
- `create_attribute(name)` - Create attribute
- `create_attribute_ns(ns_uri, q_name)` - Create attribute with namespace
- `create_text_node(value)` - Create text node
- `create_comment(value)` - Create comment
- `create_cdata_section(value)` - Create CDATA section
- `create_processing_instruction(target, data)` - Create processing instruction
- `create_entity_reference(name)` - Create entity reference
- `import_node(imported_node, deep)` - Import node from another document
- `elements_by_tag_name(tag_name)` - Find elements by tag
- `elements_by_tag_name_ns(ns_uri, local_name)` - Find elements by namespace
- `element_by_id(element_id)` - Find element by ID
- `doctype()` - Get document type
- `implementation()` - Get DOM implementation
- `has_error()` - Check for errors
- `error_string()` - Get error message
- `clear()` - Clear document

## Example

```fortran
use forge_xml_dom

type(QDomDocument) :: doc
type(QDomElement) :: root, child
logical :: success

! Parse XML
success = doc%set_content('<root><item id="1">Hello</item></root>')

if (success) then
    root = doc%document_element()
    print *, "Root:", root%tag_name()

    ! Navigate
    child = root%first_child_element("item")
    print *, "ID:", child%attribute("id")
    print *, "Text:", child%text()

    ! Modify
    call child%set_attribute("id", "2")
    call child%set_text("Updated")

    ! Serialize
    print *, doc%to_string(2)
end if

! Create document
doc = QDomDocument()
root = doc%create_element("root")
child = doc%create_element("item")
call child%set_attribute("id", "3")
call root%append_child(child%to_node())

print *, doc%to_string(2)
```

## Memory Management

DOM trees can consume significant memory for large documents. Consider using streaming parsers for memory-constrained applications.