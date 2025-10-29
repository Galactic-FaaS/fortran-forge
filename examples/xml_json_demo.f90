!> @brief Comprehensive XML/JSON processing demo
!> @details Demonstrates all XML and JSON features in ForGE
!> @author ForGE Contributors
!> @date 2025

program xml_json_demo
    use forge_json
    use forge_xml_stream
    use forge_xml_dom
    use forge_xml_query
    use forge_xml_schema
    use forge_xml_sax
    use forge_xml_formatter
    use forge_string_utils
    implicit none

    ! JSON examples
    call demo_json_parsing()
    call demo_json_document()
    call demo_json_serializer()

    ! XML streaming examples
    call demo_xml_streaming()

    ! DOM examples
    call demo_xml_dom()

    ! Query examples
    call demo_xml_query()

    ! Schema validation examples
    call demo_xml_schema()

    ! SAX parsing examples
    call demo_xml_sax()

    ! Formatting examples
    call demo_xml_formatting()

    print *, "All XML/JSON demos completed successfully!"

contains

    subroutine demo_json_parsing()
        type(QJsonValue) :: value
        type(QJsonObject) :: obj
        type(QJsonArray) :: arr
        character(len=:), allocatable :: json_str

        print *, "=== JSON Parsing Demo ==="

        ! Parse JSON string
        json_str = '{"name": "John", "age": 30, "active": true, "scores": [85, 92, 78]}'
        value = parse_json(json_str)

        ! Convert back to string
        json_str = json_to_string(value, .true.)
        print *, "Parsed and reformatted JSON:"
        print *, json_str

        ! Create JSON object
        call obj%insert("name", QJsonValue())
        call obj%value("name")%set_string("Jane")

        call obj%insert("age", QJsonValue())
        call obj%value("age")%set_number(25.0)

        ! Create JSON array
        call arr%append(QJsonValue())
        call arr%at(0)%set_number(95.0)

        call obj%insert("scores", QJsonValue())
        call obj%value("scores")%set_array(arr)

        json_str = json_to_string(QJsonValue(), .true.)  ! Would need proper conversion
        print *, "Created JSON object:"
        print *, json_str
    end subroutine demo_json_parsing

    subroutine demo_json_document()
        type(QJsonDocument) :: doc
        type(QJsonParseError) :: error
        type(QJsonObject) :: obj
        logical :: success

        print *, "=== JSON Document Demo ==="

        ! Parse JSON document
        success = doc%from_json('{"users": [{"name": "Alice"}, {"name": "Bob"}]}', error)

        if (success) then
            print *, "JSON document parsed successfully"
            if (doc%is_object()) then
                obj = doc%object()
                print *, "Document is an object with", obj%size(), "properties"
            end if
        else
            print *, "JSON parsing error:", error%to_string()
        end if

        ! Create and serialize document
        call doc%set_object(obj)
        print *, "Serialized document:", doc%to_json(.true.)
    end subroutine demo_json_document

    subroutine demo_json_serializer()
        type(QJsonSerializer) :: serializer
        type(QJsonValue) :: value

        print *, "=== JSON Serializer Demo ==="

        ! Configure serializer
        call serializer%set_compact_format(.false.)
        call serializer%set_indent_size(2)

        ! Serialize value
        call value%set_string("Hello, World!")
        print *, "Serialized value:", serializer%serialize(value)
    end subroutine demo_json_serializer

    subroutine demo_xml_streaming()
        type(QXmlStreamReader) :: reader
        type(QXmlStreamWriter) :: writer
        character(len=:), allocatable :: xml_data

        print *, "=== XML Streaming Demo ==="

        ! Sample XML
        xml_data = '<?xml version="1.0"?><root><item id="1">First</item><item id="2">Second</item></root>'

        ! Read XML
        call reader%set_data(xml_data)

        do while (reader%read_next())
            select case (reader%token_type())
            case (StartElement)
                print *, "Start element:", reader%name()
            case (Characters)
                if (.not. reader%is_whitespace()) then
                    print *, "Text:", reader%text()
                end if
            case (EndElement)
                print *, "End element:", reader%name()
            end select
        end do

        ! Write XML
        call writer%set_auto_formatting(.true.)
        call writer%write_start_document()
        call writer%write_start_element("root")
        call writer%write_start_element("item")
        call writer%write_attribute("id", "1")
        call writer%write_characters("Content")
        call writer%write_end_element()
        call writer%write_end_element()
        call writer%write_end_document()

        print *, "Generated XML:"
        print *, writer%get_buffer()
    end subroutine demo_xml_streaming

    subroutine demo_xml_dom()
        type(QDomDocument) :: doc
        type(QDomElement) :: root, child
        logical :: success

        print *, "=== XML DOM Demo ==="

        ! Parse XML
        success = doc%set_content('<root><child id="1">Content</child></root>')

        if (success) then
            root = doc%document_element()
            print *, "Root element:", root%tag_name()

            ! Navigate DOM
            child = root%first_child_element("child")
            if (.not. child%is_null()) then
                print *, "Child element:", child%tag_name()
                print *, "Child attribute id:", child%attribute("id")
                print *, "Child text:", child%text()
            end if
        else
            print *, "DOM parsing failed"
        end if

        ! Create DOM document
        doc = QDomDocument()
        root = doc%create_element("root")
        child = doc%create_element("child")
        call child%set_attribute("id", "2")
        call root%append_child(child%to_node())

        print *, "Created DOM XML:"
        print *, doc%to_string(2)
    end subroutine demo_xml_dom

    subroutine demo_xml_query()
        type(QXmlQuery) :: query
        character(len=:), allocatable :: result

        print *, "=== XML Query Demo ==="

        ! Set up query
        call query%set_query('//item[@id="1"]/text()', "")

        ! Note: Full XPath/XQuery implementation would require proper XML parsing
        result = query%evaluate_to_string()
        print *, "Query result:", result
    end subroutine demo_xml_query

    subroutine demo_xml_schema()
        type(QXmlSchema) :: schema
        type(QXmlSchemaValidator) :: validator
        logical :: valid

        print *, "=== XML Schema Validation Demo ==="

        ! Load schema (simplified)
        valid = schema%load('<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"><xs:element name="root"/></xs:schema>')

        if (valid) then
            print *, "Schema loaded successfully"

            ! Validate document
            call validator%set_schema(schema)
            valid = validator%validate('<root/>')

            if (valid) then
                print *, "Document is valid according to schema"
            else
                print *, "Document validation failed"
            end if
        else
            print *, "Schema loading failed"
        end if
    end subroutine demo_xml_schema

    subroutine demo_xml_sax()
        type(QXmlSimpleReader) :: reader
        type(QXmlDefaultHandler) :: handler
        type(QXmlInputSource) :: input
        logical :: success

        print *, "=== XML SAX Demo ==="

        ! Set up SAX parsing
        call reader%set_content_handler(handler)
        call input%set_data('<root><item>Hello</item></root>')

        success = reader%parse(input)

        if (success) then
            print *, "SAX parsing completed successfully"
        else
            print *, "SAX parsing failed:", reader%error_string()
        end if
    end subroutine demo_xml_sax

    subroutine demo_xml_formatting()
        type(QXmlFormatter) :: formatter
        character(len=:), allocatable :: formatted

        print *, "=== XML Formatting Demo ==="

        ! Configure formatter
        call formatter%set_indent_size(2)
        call formatter%set_auto_formatting(.true.)

        ! Format XML
        formatted = formatter%format_to_string('<root><child>content</child></root>')

        print *, "Formatted XML:"
        print *, formatted
    end subroutine demo_xml_formatting

end program xml_json_demo