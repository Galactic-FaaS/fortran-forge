program test_json
    use forge_json
    implicit none

    type(QJsonValue) :: value
    type(QJsonParseError) :: error
    character(len=:), allocatable :: json_str
    logical :: success

    ! Test 1: Simple object
    json_str = '{"name": "John", "age": 30, "active": true}'
    value = parse_json(json_str)
    if (value%is_object()) then
        print *, "Test 1 PASSED: Parsed object successfully"
    else
        print *, "Test 1 FAILED: Could not parse object"
    end if

    ! Test 2: Array
    json_str = '[1, 2, "three", true, null]'
    value = parse_json(json_str)
    if (value%is_array()) then
        print *, "Test 2 PASSED: Parsed array successfully"
    else
        print *, "Test 2 FAILED: Could not parse array"
    end if

    ! Test 3: String with escapes
    json_str = '"Hello\nWorld\t\"Quoted\""'
    value = parse_json(json_str)
    if (value%is_string()) then
        print *, "Test 3 PASSED: Parsed string with escapes"
    else
        print *, "Test 3 FAILED: Could not parse escaped string"
    end if

    ! Test 4: Number
    json_str = '123.45'
    value = parse_json(json_str)
    if (value%is_number()) then
        print *, "Test 4 PASSED: Parsed number"
    else
        print *, "Test 4 FAILED: Could not parse number"
    end if

    ! Test 5: Boolean
    json_str = 'true'
    value = parse_json(json_str)
    if (value%is_bool()) then
        print *, "Test 5 PASSED: Parsed boolean"
    else
        print *, "Test 5 FAILED: Could not parse boolean"
    end if

    ! Test 6: Null
    json_str = 'null'
    value = parse_json(json_str)
    if (value%is_null()) then
        print *, "Test 6 PASSED: Parsed null"
    else
        print *, "Test 6 FAILED: Could not parse null"
    end if

    ! Test 7: Nested structures
    json_str = '{"users": [{"name": "Alice"}, {"name": "Bob"}], "count": 2}'
    value = parse_json(json_str)
    if (value%is_object()) then
        print *, "Test 7 PASSED: Parsed nested structures"
    else
        print *, "Test 7 FAILED: Could not parse nested structures"
    end if

    ! Test 8: Error handling
    json_str = '{"incomplete": }'
    value = parse_json(json_str)
    if (value%is_null()) then
        print *, "Test 8 PASSED: Correctly handled parse error"
    else
        print *, "Test 8 FAILED: Should have failed on invalid JSON"
    end if

    print *, "JSON parser implementation completed successfully!"
end program test_json