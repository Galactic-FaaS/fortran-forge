!> @brief DOM-based XML processing
!> @details QDomDocument/QDomElement for tree-based XML manipulation
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_xml_dom
    use forge_string_utils
    use forge_containers
    use forge_errors
    implicit none
    private

    public :: QDomDocument, QDomElement, QDomNode, QDomAttr
    public :: QDomNode_NodeType
    public :: ElementNode, AttributeNode, TextNode, CDATASectionNode
    public :: EntityReferenceNode, EntityNode, ProcessingInstructionNode
    public :: CommentNode, DocumentNode, DocumentTypeNode
    public :: NotationNode, DocumentFragmentNode

    !> Node types
    integer, parameter :: ElementNode = 1
    integer, parameter :: AttributeNode = 2
    integer, parameter :: TextNode = 3
    integer, parameter :: CDATASectionNode = 4
    integer, parameter :: EntityReferenceNode = 5
    integer, parameter :: EntityNode = 6
    integer, parameter :: ProcessingInstructionNode = 7
    integer, parameter :: CommentNode = 8
    integer, parameter :: DocumentNode = 9
    integer, parameter :: DocumentTypeNode = 10
    integer, parameter :: DocumentFragmentNode = 11
    integer, parameter :: NotationNode = 12

    !> @brief DOM node type information
    type :: QDomNode_NodeType
        integer :: node_type = ElementNode
        character(len=:), allocatable :: node_name
        character(len=:), allocatable :: node_value
        character(len=:), allocatable :: namespace_uri
        character(len=:), allocatable :: prefix
        character(len=:), allocatable :: local_name
    end type QDomNode_NodeType

    !> @brief DOM attribute
    type :: QDomAttr
        private
        character(len=:), allocatable :: name
        character(len=:), allocatable :: value
        character(len=:), allocatable :: namespace_uri
        character(len=:), allocatable :: prefix
        character(len=:), allocatable :: local_name
        logical :: specified = .true.
    contains
        procedure :: name => domattr_name
        procedure :: value => domattr_value
        procedure :: set_value => domattr_set_value
        procedure :: namespace_uri => domattr_namespace_uri
        procedure :: prefix => domattr_prefix
        procedure :: local_name => domattr_local_name
        procedure :: specified => domattr_specified
    end type QDomAttr

    !> @brief DOM node
    type :: QDomNode
        private
        type(QDomNode_NodeType) :: node_info
        type(QDomNode), pointer :: parent => null()
        type(QDomNode), dimension(:), allocatable :: children
        integer :: child_count = 0
        type(QHashMap) :: attributes
    contains
        procedure :: node_type => domnode_node_type
        procedure :: node_name => domnode_node_name
        procedure :: node_value => domnode_node_value
        procedure :: set_node_value => domnode_set_node_value
        procedure :: namespace_uri => domnode_namespace_uri
        procedure :: prefix => domnode_prefix
        procedure :: local_name => domnode_local_name
        procedure :: has_child_nodes => domnode_has_child_nodes
        procedure :: child_nodes => domnode_child_nodes
        procedure :: first_child => domnode_first_child
        procedure :: last_child => domnode_last_child
        procedure :: next_sibling => domnode_next_sibling
        procedure :: previous_sibling => domnode_previous_sibling
        procedure :: parent_node => domnode_parent_node
        procedure :: owner_document => domnode_owner_document
        procedure :: insert_before => domnode_insert_before
        procedure :: insert_after => domnode_insert_after
        procedure :: replace_child => domnode_replace_child
        procedure :: remove_child => domnode_remove_child
        procedure :: append_child => domnode_append_child
        procedure :: clone_node => domnode_clone_node
        procedure :: is_null => domnode_is_null
        procedure :: is_element => domnode_is_element
        procedure :: is_attr => domnode_is_attr
        procedure :: is_text => domnode_is_text
        procedure :: is_cdata_section => domnode_is_cdata_section
        procedure :: is_entity_reference => domnode_is_entity_reference
        procedure :: is_entity => domnode_is_entity
        procedure :: is_processing_instruction => domnode_is_processing_instruction
        procedure :: is_comment => domnode_is_comment
        procedure :: is_document => domnode_is_document
        procedure :: is_document_type => domnode_is_document_type
        procedure :: is_document_fragment => domnode_is_document_fragment
        procedure :: is_notation => domnode_is_notation
        procedure :: to_element => domnode_to_element
        procedure :: to_attr => domnode_to_attr
        procedure :: to_text => domnode_to_text
        procedure :: to_cdata_section => domnode_to_cdata_section
        procedure :: to_processing_instruction => domnode_to_processing_instruction
        procedure :: to_comment => domnode_to_comment
        procedure :: to_document => domnode_to_document
        procedure :: to_document_type => domnode_to_document_type
        procedure :: to_document_fragment => domnode_to_document_fragment
        procedure :: to_notation => domnode_to_notation
        procedure :: clear => domnode_clear
    end type QDomNode

    !> @brief DOM element
    type :: QDomElement
        private
        type(QDomNode) :: node
    contains
        procedure :: tag_name => domelement_tag_name
        procedure :: set_tag_name => domelement_set_tag_name
        procedure :: attribute => domelement_attribute
        procedure :: set_attribute => domelement_set_attribute
        procedure :: remove_attribute => domelement_remove_attribute
        procedure :: has_attribute => domelement_has_attribute
        procedure :: attribute_node => domelement_attribute_node
        procedure :: set_attribute_node => domelement_set_attribute_node
        procedure :: remove_attribute_node => domelement_remove_attribute_node
        procedure :: attributes => domelement_attributes
        procedure :: has_attributes => domelement_has_attributes
        procedure :: namespace_uri => domelement_namespace_uri
        procedure :: prefix => domelement_prefix
        procedure :: local_name => domelement_local_name
        procedure :: set_prefix => domelement_set_prefix
        procedure :: elements_by_tag_name => domelement_elements_by_tag_name
        procedure :: elements_by_tag_name_ns => domelement_elements_by_tag_name_ns
        procedure :: first_child_element => domelement_first_child_element
        procedure :: last_child_element => domelement_last_child_element
        procedure :: next_sibling_element => domelement_next_sibling_element
        procedure :: previous_sibling_element => domelement_previous_sibling_element
        procedure :: text => domelement_text
        procedure :: set_text => domelement_set_text
        procedure :: is_null => domelement_is_null
        procedure :: node_type => domelement_node_type
        procedure :: to_node => domelement_to_node
    end type QDomElement

    !> @brief DOM document
    type :: QDomDocument
        private
        type(QDomNode) :: document_node
        type(forge_error) :: error
    contains
        procedure :: set_content => domdocument_set_content
        procedure :: to_string => domdocument_to_string
        procedure :: document_element => domdocument_document_element
        procedure :: create_element => domdocument_create_element
        procedure :: create_element_ns => domdocument_create_element_ns
        procedure :: create_attribute => domdocument_create_attribute
        procedure :: create_attribute_ns => domdocument_create_attribute_ns
        procedure :: create_text_node => domdocument_create_text_node
        procedure :: create_comment => domdocument_create_comment
        procedure :: create_cdata_section => domdocument_create_cdata_section
        procedure :: create_processing_instruction => domdocument_create_processing_instruction
        procedure :: create_entity_reference => domdocument_create_entity_reference
        procedure :: import_node => domdocument_import_node
        procedure :: elements_by_tag_name => domdocument_elements_by_tag_name
        procedure :: elements_by_tag_name_ns => domdocument_elements_by_tag_name_ns
        procedure :: element_by_id => domdocument_element_by_id
        procedure :: doctype => domdocument_doctype
        procedure :: implementation => domdocument_implementation
        procedure :: has_error => domdocument_has_error
        procedure :: error_string => domdocument_error_string
        procedure :: clear => domdocument_clear
    end type QDomDocument

contains

    ! ========== QDomAttr Implementation ==========

    function domattr_name(this) result(name)
        class(QDomAttr), intent(in) :: this
        character(len=:), allocatable :: name
        name = this%name
    end function domattr_name

    function domattr_value(this) result(value)
        class(QDomAttr), intent(in) :: this
        character(len=:), allocatable :: value
        value = this%value
    end function domattr_value

    subroutine domattr_set_value(this, value)
        class(QDomAttr), intent(inout) :: this
        character(len=*), intent(in) :: value
        this%value = value
    end subroutine domattr_set_value

    function domattr_namespace_uri(this) result(uri)
        class(QDomAttr), intent(in) :: this
        character(len=:), allocatable :: uri
        uri = this%namespace_uri
    end function domattr_namespace_uri

    function domattr_prefix(this) result(prefix)
        class(QDomAttr), intent(in) :: this
        character(len=:), allocatable :: prefix
        prefix = this%prefix
    end function domattr_prefix

    function domattr_local_name(this) result(local_name)
        class(QDomAttr), intent(in) :: this
        character(len=:), allocatable :: local_name
        local_name = this%local_name
    end function domattr_local_name

    function domattr_specified(this) result(specified)
        class(QDomAttr), intent(in) :: this
        logical :: specified
        specified = this%specified
    end function domattr_specified

    ! ========== QDomNode Implementation ==========

    function domnode_node_type(this) result(node_type)
        class(QDomNode), intent(in) :: this
        integer :: node_type
        node_type = this%node_info%node_type
    end function domnode_node_type

    function domnode_node_name(this) result(name)
        class(QDomNode), intent(in) :: this
        character(len=:), allocatable :: name
        name = this%node_info%node_name
    end function domnode_node_name

    function domnode_node_value(this) result(value)
        class(QDomNode), intent(in) :: this
        character(len=:), allocatable :: value
        value = this%node_info%node_value
    end function domnode_node_value

    subroutine domnode_set_node_value(this, value)
        class(QDomNode), intent(inout) :: this
        character(len=*), intent(in) :: value
        this%node_info%node_value = value
    end subroutine domnode_set_node_value

    function domnode_namespace_uri(this) result(uri)
        class(QDomNode), intent(in) :: this
        character(len=:), allocatable :: uri
        uri = this%node_info%namespace_uri
    end function domnode_namespace_uri

    function domnode_prefix(this) result(prefix)
        class(QDomNode), intent(in) :: this
        character(len=:), allocatable :: prefix
        prefix = this%node_info%prefix
    end function domnode_prefix

    function domnode_local_name(this) result(local_name)
        class(QDomNode), intent(in) :: this
        character(len=:), allocatable :: local_name
        local_name = this%node_info%local_name
    end function domnode_local_name

    function domnode_has_child_nodes(this) result(has_children)
        class(QDomNode), intent(in) :: this
        logical :: has_children
        has_children = this%child_count > 0
    end function domnode_has_child_nodes

    function domnode_child_nodes(this) result(children)
        class(QDomNode), intent(in) :: this
        type(QDomNode), dimension(:), allocatable :: children
        if (allocated(this%children)) then
            allocate(children(this%child_count))
            children = this%children(1:this%child_count)
        end if
    end function domnode_child_nodes

    function domnode_first_child(this) result(child)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: child
        if (this%child_count > 0) then
            child = this%children(1)
        end if
    end function domnode_first_child

    function domnode_last_child(this) result(child)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: child
        if (this%child_count > 0) then
            child = this%children(this%child_count)
        end if
    end function domnode_last_child

    function domnode_next_sibling(this) result(sibling)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: sibling
        ! Simplified implementation - would need parent tracking
    end function domnode_next_sibling

    function domnode_previous_sibling(this) result(sibling)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: sibling
        ! Simplified implementation - would need parent tracking
    end function domnode_previous_sibling

    function domnode_parent_node(this) result(parent)
        class(QDomNode), intent(in) :: this
        type(QDomNode), pointer :: parent
        parent => this%parent
    end function domnode_parent_node

    function domnode_owner_document(this) result(doc)
        class(QDomNode), intent(in) :: this
        type(QDomDocument) :: doc
        ! Simplified - would need to traverse up to document root
    end function domnode_owner_document

    function domnode_insert_before(this, new_child, ref_child) result(inserted_node)
        class(QDomNode), intent(inout) :: this
        type(QDomNode), intent(in) :: new_child
        type(QDomNode), intent(in) :: ref_child
        type(QDomNode) :: inserted_node
        ! Simplified implementation
        inserted_node = this%append_child(new_child)
    end function domnode_insert_before

    function domnode_insert_after(this, new_child, ref_child) result(inserted_node)
        class(QDomNode), intent(inout) :: this
        type(QDomNode), intent(in) :: new_child
        type(QDomNode), intent(in) :: ref_child
        type(QDomNode) :: inserted_node
        ! Simplified implementation
        inserted_node = this%append_child(new_child)
    end function domnode_insert_after

    function domnode_replace_child(this, new_child, old_child) result(replaced_node)
        class(QDomNode), intent(inout) :: this
        type(QDomNode), intent(in) :: new_child
        type(QDomNode), intent(in) :: old_child
        type(QDomNode) :: replaced_node
        ! Simplified implementation
        replaced_node = old_child
    end function domnode_replace_child

    function domnode_remove_child(this, old_child) result(removed_node)
        class(QDomNode), intent(inout) :: this
        type(QDomNode), intent(in) :: old_child
        type(QDomNode) :: removed_node
        ! Simplified implementation
        removed_node = old_child
    end function domnode_remove_child

    function domnode_append_child(this, new_child) result(appended_node)
        class(QDomNode), intent(inout) :: this
        type(QDomNode), intent(in) :: new_child
        type(QDomNode) :: appended_node

        type(QDomNode), dimension(:), allocatable :: temp_children

        if (.not. allocated(this%children)) then
            allocate(this%children(10))
        else if (this%child_count >= size(this%children)) then
            allocate(temp_children(size(this%children) * 2))
            temp_children(1:this%child_count) = this%children(1:this%child_count)
            call move_alloc(temp_children, this%children)
        end if

        this%child_count = this%child_count + 1
        this%children(this%child_count) = new_child
        appended_node = new_child
    end function domnode_append_child

    function domnode_clone_node(this, deep) result(clone)
        class(QDomNode), intent(in) :: this
        logical, intent(in) :: deep
        type(QDomNode) :: clone
        clone = this
    end function domnode_clone_node

    function domnode_is_null(this) result(is_null)
        class(QDomNode), intent(in) :: this
        logical :: is_null
        is_null = .not. allocated(this%node_info%node_name)
    end function domnode_is_null

    function domnode_is_element(this) result(is_elem)
        class(QDomNode), intent(in) :: this
        logical :: is_elem
        is_elem = this%node_info%node_type == ElementNode
    end function domnode_is_element

    function domnode_is_attr(this) result(is_attr)
        class(QDomNode), intent(in) :: this
        logical :: is_attr
        is_attr = this%node_info%node_type == AttributeNode
    end function domnode_is_attr

    function domnode_is_text(this) result(is_text)
        class(QDomNode), intent(in) :: this
        logical :: is_text
        is_text = this%node_info%node_type == TextNode
    end function domnode_is_text

    function domnode_is_cdata_section(this) result(is_cdata)
        class(QDomNode), intent(in) :: this
        logical :: is_cdata
        is_cdata = this%node_info%node_type == CDATASectionNode
    end function domnode_is_cdata_section

    function domnode_is_entity_reference(this) result(is_entity_ref)
        class(QDomNode), intent(in) :: this
        logical :: is_entity_ref
        is_entity_ref = this%node_info%node_type == EntityReferenceNode
    end function domnode_is_entity_reference

    function domnode_is_entity(this) result(is_entity)
        class(QDomNode), intent(in) :: this
        logical :: is_entity
        is_entity = this%node_info%node_type == EntityNode
    end function domnode_is_entity

    function domnode_is_processing_instruction(this) result(is_pi)
        class(QDomNode), intent(in) :: this
        logical :: is_pi
        is_pi = this%node_info%node_type == ProcessingInstructionNode
    end function domnode_is_processing_instruction

    function domnode_is_comment(this) result(is_comment)
        class(QDomNode), intent(in) :: this
        logical :: is_comment
        is_comment = this%node_info%node_type == CommentNode
    end function domnode_is_comment

    function domnode_is_document(this) result(is_doc)
        class(QDomNode), intent(in) :: this
        logical :: is_doc
        is_doc = this%node_info%node_type == DocumentNode
    end function domnode_is_document

    function domnode_is_document_type(this) result(is_doctype)
        class(QDomNode), intent(in) :: this
        logical :: is_doctype
        is_doctype = this%node_info%node_type == DocumentTypeNode
    end function domnode_is_document_type

    function domnode_is_document_fragment(this) result(is_doc_frag)
        class(QDomNode), intent(in) :: this
        logical :: is_doc_frag
        is_doc_frag = this%node_info%node_type == DocumentFragmentNode
    end function domnode_is_document_fragment

    function domnode_is_notation(this) result(is_notation)
        class(QDomNode), intent(in) :: this
        logical :: is_notation
        is_notation = this%node_info%node_type == NotationNode
    end function domnode_is_notation

    function domnode_to_element(this) result(element)
        class(QDomNode), intent(in) :: this
        type(QDomElement) :: element
        if (this%is_element()) then
            element%node = this
        end if
    end function domnode_to_element

    function domnode_to_attr(this) result(attr)
        class(QDomNode), intent(in) :: this
        type(QDomAttr) :: attr
        ! Simplified conversion
    end function domnode_to_attr

    function domnode_to_text(this) result(text_node)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: text_node
        if (this%is_text()) then
            text_node = this
        end if
    end function domnode_to_text

    function domnode_to_cdata_section(this) result(cdata_node)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: cdata_node
        if (this%is_cdata_section()) then
            cdata_node = this
        end if
    end function domnode_to_cdata_section

    function domnode_to_processing_instruction(this) result(pi_node)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: pi_node
        if (this%is_processing_instruction()) then
            pi_node = this
        end if
    end function domnode_to_processing_instruction

    function domnode_to_comment(this) result(comment_node)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: comment_node
        if (this%is_comment()) then
            comment_node = this
        end if
    end function domnode_to_comment

    function domnode_to_document(this) result(doc)
        class(QDomNode), intent(in) :: this
        type(QDomDocument) :: doc
        if (this%is_document()) then
            ! Simplified conversion
        end if
    end function domnode_to_document

    function domnode_to_document_type(this) result(doctype_node)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: doctype_node
        if (this%is_document_type()) then
            doctype_node = this
        end if
    end function domnode_to_document_type

    function domnode_to_document_fragment(this) result(doc_frag_node)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: doc_frag_node
        if (this%is_document_fragment()) then
            doc_frag_node = this
        end if
    end function domnode_to_document_fragment

    function domnode_to_notation(this) result(notation_node)
        class(QDomNode), intent(in) :: this
        type(QDomNode) :: notation_node
        if (this%is_notation()) then
            notation_node = this
        end if
    end function domnode_to_notation

    subroutine domnode_clear(this)
        class(QDomNode), intent(inout) :: this
        this%node_info%node_type = ElementNode
        if (allocated(this%node_info%node_name)) deallocate(this%node_info%node_name)
        if (allocated(this%node_info%node_value)) deallocate(this%node_info%node_value)
        if (allocated(this%node_info%namespace_uri)) deallocate(this%node_info%namespace_uri)
        if (allocated(this%node_info%prefix)) deallocate(this%node_info%prefix)
        if (allocated(this%node_info%local_name)) deallocate(this%node_info%local_name)
        if (associated(this%parent)) nullify(this%parent)
        if (allocated(this%children)) deallocate(this%children)
        this%child_count = 0
        call this%attributes%clear()
    end subroutine domnode_clear

    ! ========== QDomElement Implementation ==========

    function domelement_tag_name(this) result(tag_name)
        class(QDomElement), intent(in) :: this
        character(len=:), allocatable :: tag_name
        tag_name = this%node%node_name()
    end function domelement_tag_name

    subroutine domelement_set_tag_name(this, name)
        class(QDomElement), intent(inout) :: this
        character(len=*), intent(in) :: name
        this%node%node_info%node_name = name
    end subroutine domelement_set_tag_name

    function domelement_attribute(this, name, default_value) result(value)
        class(QDomElement), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: default_value
        character(len=:), allocatable :: value

        if (this%node%attributes%contains(name)) then
            value = this%node%attributes%value(name)
        else if (present(default_value)) then
            value = default_value
        else
            value = ""
        end if
    end function domelement_attribute

    subroutine domelement_set_attribute(this, name, value)
        class(QDomElement), intent(inout) :: this
        character(len=*), intent(in) :: name, value
        call this%node%attributes%insert(name, value)
    end subroutine domelement_set_attribute

    subroutine domelement_remove_attribute(this, name)
        class(QDomElement), intent(inout) :: this
        character(len=*), intent(in) :: name
        call this%node%attributes%remove(name)
    end subroutine domelement_remove_attribute

    function domelement_has_attribute(this, name) result(has_attr)
        class(QDomElement), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: has_attr
        has_attr = this%node%attributes%contains(name)
    end function domelement_has_attribute

    function domelement_attribute_node(this, name) result(attr)
        class(QDomElement), intent(in) :: this
        character(len=*), intent(in) :: name
        type(QDomAttr) :: attr
        if (this%has_attribute(name)) then
            attr%name = name
            attr%value = this%attribute(name)
        end if
    end function domelement_attribute_node

    function domelement_set_attribute_node(this, new_attr) result(old_attr)
        class(QDomElement), intent(inout) :: this
        type(QDomAttr), intent(in) :: new_attr
        type(QDomAttr) :: old_attr
        call this%set_attribute(new_attr%name, new_attr%value)
    end function domelement_set_attribute_node

    function domelement_remove_attribute_node(this, old_attr) result(removed_attr)
        class(QDomElement), intent(inout) :: this
        type(QDomAttr), intent(in) :: old_attr
        type(QDomAttr) :: removed_attr
        removed_attr = old_attr
        call this%remove_attribute(old_attr%name)
    end function domelement_remove_attribute_node

    function domelement_attributes(this) result(attrs)
        class(QDomElement), intent(in) :: this
        type(QHashMap) :: attrs
        attrs = this%node%attributes
    end function domelement_attributes

    function domelement_has_attributes(this) result(has_attrs)
        class(QDomElement), intent(in) :: this
        logical :: has_attrs
        has_attrs = this%node%attributes%size() > 0
    end function domelement_has_attributes

    function domelement_namespace_uri(this) result(uri)
        class(QDomElement), intent(in) :: this
        character(len=:), allocatable :: uri
        uri = this%node%namespace_uri()
    end function domelement_namespace_uri

    function domelement_prefix(this) result(prefix)
        class(QDomElement), intent(in) :: this
        character(len=:), allocatable :: prefix
        prefix = this%node%prefix()
    end function domelement_prefix

    function domelement_local_name(this) result(local_name)
        class(QDomElement), intent(in) :: this
        character(len=:), allocatable :: local_name
        local_name = this%node%local_name()
    end function domelement_local_name

    subroutine domelement_set_prefix(this, prefix)
        class(QDomElement), intent(inout) :: this
        character(len=*), intent(in) :: prefix
        this%node%node_info%prefix = prefix
    end subroutine domelement_set_prefix

    function domelement_elements_by_tag_name(this, tag_name) result(elements)
        class(QDomElement), intent(in) :: this
        character(len=*), intent(in) :: tag_name
        type(QDomNode), dimension(:), allocatable :: elements
        ! Simplified implementation
    end function domelement_elements_by_tag_name

    function domelement_elements_by_tag_name_ns(this, ns_uri, local_name) result(elements)
        class(QDomElement), intent(in) :: this
        character(len=*), intent(in) :: ns_uri, local_name
        type(QDomNode), dimension(:), allocatable :: elements
        ! Simplified implementation
    end function domelement_elements_by_tag_name_ns

    function domelement_first_child_element(this, tag_name) result(element)
        class(QDomElement), intent(in) :: this
        character(len=*), intent(in), optional :: tag_name
        type(QDomElement) :: element
        ! Simplified implementation
    end function domelement_first_child_element

    function domelement_last_child_element(this, tag_name) result(element)
        class(QDomElement), intent(in) :: this
        character(len=*), intent(in), optional :: tag_name
        type(QDomElement) :: element
        ! Simplified implementation
    end function domelement_last_child_element

    function domelement_next_sibling_element(this, tag_name) result(element)
        class(QDomElement), intent(in) :: this
        character(len=*), intent(in), optional :: tag_name
        type(QDomElement) :: element
        ! Simplified implementation
    end function domelement_next_sibling_element

    function domelement_previous_sibling_element(this, tag_name) result(element)
        class(QDomElement), intent(in) :: this
        character(len=*), intent(in), optional :: tag_name
        type(QDomElement) :: element
        ! Simplified implementation
    end function domelement_previous_sibling_element

    function domelement_text(this) result(text)
        class(QDomElement), intent(in) :: this
        character(len=:), allocatable :: text
        ! Simplified - concatenate all text nodes
        text = ""
    end function domelement_text

    subroutine domelement_set_text(this, text)
        class(QDomElement), intent(inout) :: this
        character(len=*), intent(in) :: text
        ! Simplified implementation
    end subroutine domelement_set_text

    function domelement_is_null(this) result(is_null)
        class(QDomElement), intent(in) :: this
        logical :: is_null
        is_null = this%node%is_null()
    end function domelement_is_null

    function domelement_node_type(this) result(node_type)
        class(QDomElement), intent(in) :: this
        integer :: node_type
        node_type = this%node%node_type()
    end function domelement_node_type

    function domelement_to_node(this) result(node)
        class(QDomElement), intent(in) :: this
        type(QDomNode) :: node
        node = this%node
    end function domelement_to_node

    ! ========== QDomDocument Implementation ==========

    function domdocument_set_content(this, text, namespace_processing) result(success)
        class(QDomDocument), intent(inout) :: this
        character(len=*), intent(in) :: text
        logical, intent(in), optional :: namespace_processing
        logical :: success

        ! Simplified XML parsing - would use a proper parser
        success = .true.
        this%document_node%node_info%node_type = DocumentNode
        this%document_node%node_info%node_name = "#document"
    end function domdocument_set_content

    function domdocument_to_string(this, indent) result(xml_string)
        class(QDomDocument), intent(in) :: this
        integer, intent(in), optional :: indent
        character(len=:), allocatable :: xml_string

        ! Simplified XML generation
        xml_string = '<?xml version="1.0" encoding="UTF-8"?>' // achar(10)
        ! Would serialize the document tree
    end function domdocument_to_string

    function domdocument_document_element(this) result(element)
        class(QDomDocument), intent(in) :: this
        type(QDomElement) :: element
        ! Return root element
    end function domdocument_document_element

    function domdocument_create_element(this, tag_name) result(element)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: tag_name
        type(QDomElement) :: element

        element%node%node_info%node_type = ElementNode
        element%node%node_info%node_name = tag_name
    end function domdocument_create_element

    function domdocument_create_element_ns(this, ns_uri, q_name) result(element)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: ns_uri, q_name
        type(QDomElement) :: element

        element%node%node_info%node_type = ElementNode
        element%node%node_info%node_name = q_name
        element%node%node_info%namespace_uri = ns_uri
    end function domdocument_create_element_ns

    function domdocument_create_attribute(this, name) result(attr)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: name
        type(QDomAttr) :: attr

        attr%name = name
    end function domdocument_create_attribute

    function domdocument_create_attribute_ns(this, ns_uri, q_name) result(attr)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: ns_uri, q_name
        type(QDomAttr) :: attr

        attr%name = q_name
        attr%namespace_uri = ns_uri
    end function domdocument_create_attribute_ns

    function domdocument_create_text_node(this, value) result(text_node)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: value
        type(QDomNode) :: text_node

        text_node%node_info%node_type = TextNode
        text_node%node_info%node_value = value
    end function domdocument_create_text_node

    function domdocument_create_comment(this, value) result(comment_node)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: value
        type(QDomNode) :: comment_node

        comment_node%node_info%node_type = CommentNode
        comment_node%node_info%node_value = value
    end function domdocument_create_comment

    function domdocument_create_cdata_section(this, value) result(cdata_node)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: value
        type(QDomNode) :: cdata_node

        cdata_node%node_info%node_type = CDATASectionNode
        cdata_node%node_info%node_value = value
    end function domdocument_create_cdata_section

    function domdocument_create_processing_instruction(this, target, data) result(pi_node)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: target, data
        type(QDomNode) :: pi_node

        pi_node%node_info%node_type = ProcessingInstructionNode
        pi_node%node_info%node_name = target
        pi_node%node_info%node_value = data
    end function domdocument_create_processing_instruction

    function domdocument_create_entity_reference(this, name) result(entity_ref_node)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: name
        type(QDomNode) :: entity_ref_node

        entity_ref_node%node_info%node_type = EntityReferenceNode
        entity_ref_node%node_info%node_name = name
    end function domdocument_create_entity_reference

    function domdocument_import_node(this, imported_node, deep) result(imported)
        class(QDomDocument), intent(in) :: this
        type(QDomNode), intent(in) :: imported_node
        logical, intent(in) :: deep
        type(QDomNode) :: imported
        imported = imported_node
    end function domdocument_import_node

    function domdocument_elements_by_tag_name(this, tag_name) result(elements)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: tag_name
        type(QDomNode), dimension(:), allocatable :: elements
        ! Simplified implementation
    end function domdocument_elements_by_tag_name

    function domdocument_elements_by_tag_name_ns(this, ns_uri, local_name) result(elements)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: ns_uri, local_name
        type(QDomNode), dimension(:), allocatable :: elements
        ! Simplified implementation
    end function domdocument_elements_by_tag_name_ns

    function domdocument_element_by_id(this, element_id) result(element)
        class(QDomDocument), intent(in) :: this
        character(len=*), intent(in) :: element_id
        type(QDomElement) :: element
        ! Simplified implementation
    end function domdocument_element_by_id

    function domdocument_doctype(this) result(doctype)
        class(QDomDocument), intent(in) :: this
        type(QDomNode) :: doctype
        ! Return document type node
    end function domdocument_doctype

    function domdocument_implementation(this) result(impl)
        class(QDomDocument), intent(in) :: this
        type(QDomImplementation) :: impl
        ! Return DOM implementation
    end function domdocument_implementation

    function domdocument_has_error(this) result(has_err)
        class(QDomDocument), intent(in) :: this
        logical :: has_err
        has_err = this%error%status%is_error()
    end function domdocument_has_error

    function domdocument_error_string(this) result(err_str)
        class(QDomDocument), intent(in) :: this
        character(len=:), allocatable :: err_str
        err_str = trim(this%error%status%message)
    end function domdocument_error_string

    subroutine domdocument_clear(this)
        class(QDomDocument), intent(inout) :: this
        call this%document_node%clear()
        call this%error%clear()
    end subroutine domdocument_clear

end module forge_xml_dom