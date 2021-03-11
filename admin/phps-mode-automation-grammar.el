;;; phps-mode-automation-grammar --- Grammar -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defconst
  phps-mode-automation-grammar-non-terminals
  '(
    absolute_trait_method_reference
    alt_if_stmt
    alt_if_stmt_without_else
    anonymous_class
    argument
    argument_list
    array_object_dereferencable
    array_pair
    array_pair_list
    attribute
    attribute_decl
    attribute_group
    attributed_class_statement
    attributed_parameter
    attributed_statement
    attributes
    backticks_expr
    backup_doc_comment
    backup_fn_flags
    backup_lex_pos
    callable_expr
    callable_variable
    case_list
    case_separator
    catch_list
    catch_name_list
    class_const_decl
    class_const_list
    class_constant
    class_declaration_statement
    class_modifier
    class_modifiers
    class_name
    class_name_list
    class_name_reference
    class_statement
    class_statement_list
    const_decl
    const_list
    constant
    ctor_arguments
    declare_statement
    dereferencable_scalar
    echo_expr_list
    encaps_list
    encaps_var
    encaps_var_offset
    exit_expr
    expr
    extends_from
    finally_statement
    fn
    for_exprs
    for_statement
    foreach_statement
    foreach_variable
    fully_dereferencable
    function
    function_call
    function_declaration_statement
    global_var
    global_var_list
    group_use_declaration
    identifier
    if_stmt
    if_stmt_without_else
    implements_list
    inline_function
    inline_use_declaration
    inline_use_declarations
    inner_statement
    inner_statement_list
    interface_declaration_statement
    interface_extends_list
    internal_functions_in_yacc
    is_reference
    is_variadic
    isset_variable
    isset_variables
    legacy_namespace_name
    lexical_var
    lexical_var_list
    lexical_vars
    match
    match_arm
    match_arm_cond_list
    match_arm_list
    member_modifier
    member_name
    method_body
    method_modifiers
    mixed_group_use_declaration
    name
    namespace_declaration_name
    namespace_name
    new_expr
    new_variable
    non_empty_argument_list
    non_empty_array_pair_list
    non_empty_for_exprs
    non_empty_match_arm_list
    non_empty_member_modifiers
    non_empty_parameter_list
    optional_expr
    optional_type_without_static
    optional_variable
    optional_visibility_modifier
    parameter
    parameter_list
    possible_array_pair
    possible_comma
    property
    property_list
    property_name
    reserved_non_modifiers
    return_type
    returns_ref
    scalar
    semi_reserved
    simple_variable
    start
    statement
    static_member
    static_var
    static_var_list
    switch_case_list
    top_statement
    top_statement_list
    trait_adaptation
    trait_adaptation_list
    trait_adaptations
    trait_alias
    trait_declaration_statement
    trait_method_reference
    trait_precedence
    type
    type_expr
    type_expr_without_static
    type_without_static
    union_type
    union_type_without_static
    unprefixed_use_declaration
    unprefixed_use_declarations
    unset_variable
    unset_variables
    use_declaration
    use_declarations
    use_type
    variable
    variable_class_name
    variable_modifiers
    while_statement
    )
  "The non-terminals in grammar.")

(defconst
  phps-mode-automation-grammar-terminals
  '(
    "!"
    "%"
    "&"
    "("
    ")"
    "*"
    "+"
    ","
    "-"
    "/"
    ":"
    ";"
    "<"
    "="
    ">"
    "?"
    "@"
    "["
    "]"
    "^"
    "|"
    "~"
    T_ABSTRACT
    T_AND
    T_ARG
    T_ARRAY
    T_ARROW
    T_AS
    T_ASSIGN
    T_ATTRIBUTE
    T_BAD
    T_BOOL
    T_BOOLEAN
    T_BREAK
    T_CALL
    T_CALLABLE
    T_CASE
    T_CATCH
    T_CLASS
    T_CLONE
    T_CLOSE
    T_CLOSURE
    T_COALESCE
    T_COMMENT
    T_COMPILER
    T_CONCAT
    T_CONDITIONAL
    T_CONST
    T_CONSTANT
    T_CONTINUE
    T_CURLY
    T_DEC
    T_DECL
    T_DECLARE
    T_DEFAULT
    T_DIM
    T_DIR
    T_DIV
    T_DNUMBER
    T_DO
    T_DOC
    T_DOLLAR
    T_DOUBLE
    T_ECHO
    T_ELLIPSIS
    T_ELSE
    T_ELSEIF
    T_EMPTY
    T_ENCAPS
    T_ENCAPSED
    T_END
    T_ENDDECLARE
    T_ENDFOR
    T_ENDFOREACH
    T_ENDIF
    T_ENDSWITCH
    T_ENDWHILE
    T_EQUAL
    T_ERROR
    T_EVAL
    T_EXIT
    T_EXPR
    T_EXTENDS
    T_FILE
    T_FINAL
    T_FINALLY
    T_FN
    T_FOR
    T_FOREACH
    T_FQ
    T_FUNC
    T_FUNCTION
    T_GLOBAL
    T_GOTO
    T_GREATER
    T_GROUP
    T_HALT
    T_IDENTICAL
    T_IF
    T_IMPLEMENTS
    T_INC
    T_INCLUDE
    T_INLINE
    T_INSTANCEOF
    T_INSTEADOF
    T_INT
    T_INTERFACE
    T_IS
    T_ISSET
    T_LABEL
    T_LINE
    T_LIST
    T_LNUMBER
    T_LOGICAL
    T_MAGIC
    T_MATCH
    T_METHOD
    T_MINUS
    T_MOD
    T_MUL
    T_NAME
    T_NAMED
    T_NAMESPACE
    T_NEW
    T_NOELSE
    T_NS
    T_NULLSAFE
    T_NUM
    T_OBJECT
    T_OPEN
    T_OPERATOR
    T_OR
    T_PAAMAYIM
    T_PARAM
    T_PLUS
    T_POST
    T_POW
    T_PRE
    T_PRINT
    T_PRIVATE
    T_PROP
    T_PROTECTED
    T_PUBLIC
    T_REF
    T_REQUIRE
    T_RETURN
    T_SHELL
    T_SILENCE
    T_SL
    T_SPACESHIP
    T_SR
    T_START
    T_STATIC
    T_STMT
    T_STRING
    T_SWITCH
    T_THROW
    T_TRAIT
    T_TRY
    T_TYPE
    T_UNARY
    T_UNPACK
    T_UNSET
    T_USE
    T_VAR
    T_VARIABLE
    T_WHILE
    T_WHITESPACE
    T_XOR
    T_YIELD
    )
  "The terminals of grammar.")

(defconst
  phps-mode-automation-grammar-look-ahead-number
  1
  "The look-ahead number of grammar.")

(defconst
  phps-mode-automation-grammar-productions
  '(

    (start
     top_statement_list
     )

    (reserved_non_modifiers
     T_INCLUDE
     T_INCLUDE_ONCE
     T_EVAL
     T_REQUIRE
     T_REQUIRE_ONCE
     T_LOGICAL_OR
     T_LOGICAL_XOR
     T_LOGICAL_AND
     T_INSTANCEOF
     T_NEW
     T_CLONE
     T_EXIT
     T_IF
     T_ELSEIF
     T_ELSE
     T_ENDIF
     T_ECHO
     T_DO
     T_WHILE
     T_ENDWHILE
     T_FOR
     T_ENDFOR
     T_FOREACH
     T_ENDFOREACH
     T_DECLARE
     T_ENDDECLARE
     T_AS
     T_TRY
     T_CATCH
     T_FINALLY
     T_THROW
     T_USE
     T_INSTEADOF
     T_GLOBAL
     T_VAR
     T_UNSET
     T_ISSET
     T_EMPTY
     T_CONTINUE
     T_GOTO
     T_FUNCTION
     T_CONST
     T_RETURN
     T_PRINT
     T_YIELD
     T_LIST
     T_SWITCH
     T_ENDSWITCH
     T_CASE
     T_DEFAULT
     T_BREAK
     T_ARRAY
     T_CALLABLE
     T_EXTENDS
     T_IMPLEMENTS
     T_NAMESPACE
     T_TRAIT
     T_INTERFACE
     T_CLASS
     T_CLASS_C
     T_TRAIT_C
     T_FUNC_C
     T_METHOD_C
     T_LINE
     T_FILE
     T_DIR
     T_NS_C
     T_FN
     T_MATCH
     )

    (semi_reserved
     reserved_non_modifiers
     T_STATIC
     T_ABSTRACT
     T_FINAL
     T_PRIVATE
     T_PROTECTED
     T_PUBLIC
     )

    (identifier
     T_STRING
     semi_reserved
     )

    (top_statement_list
     (top_statement_list top_statement)
     %empty
     )

    (namespace_declaration_name
     identifier
     T_NAME_QUALIFIED
     )

    (namespace_name
     T_STRING
     T_NAME_QUALIFIED
     )

    (legacy_namespace_name
     namespace_name
     T_NAME_FULLY_QUALIFIED
     )

    (name
     T_STRING
     T_NAME_QUALIFIED
     T_NAME_FULLY_QUALIFIED
     T_NAME_RELATIVE
     )

    (attribute_decl
     class_name
     (class_name argument_list)
     )

    (attribute_group
     attribute_decl
     (attribute_group "," attribute_decl)
     )

    (attribute
     (T_ATTRIBUTE
     attribute_group
     possible_comma "]")
     )

    (attributes
     attribute
     (attributes attribute)
     )

    (attributed_statement
     function_declaration_statement
     class_declaration_statement
     trait_declaration_statement
     interface_declaration_statement
     )

    (top_statement
     statement
     attributed_statement
     attributes
     attributed_statement
     (T_HALT_COMPILER "(" ")" ";")
     (T_NAMESPACE namespace_declaration_name ";")
     (T_NAMESPACE namespace_declaration_name)
     ("{" top_statement_list "}")
     T_NAMESPACE
     (T_USE mixed_group_use_declaration ";")
     (T_USE use_type group_use_declaration ";")
     (T_USE use_declarations ";")
     (T_USE use_type use_declarations ";")
     (T_CONST const_list ";")
     )

    (use_type
     T_FUNCTION
     T_CONST
     )

    (group_use_declaration
     (legacy_namespace_name T_NS_SEPARATOR "{" unprefixed_use_declarations possible_comma "}")
     )

    (mixed_group_use_declaration
     (legacy_namespace_name T_NS_SEPARATOR "{" inline_use_declarations possible_comma "}")
     )

    (possible_comma
     %empty
     ","
     )

    (inline_use_declarations
     (inline_use_declarations "," inline_use_declaration)
     inline_use_declaration
     )

    (unprefixed_use_declarations
     (unprefixed_use_declarations "," unprefixed_use_declaration)
     unprefixed_use_declaration
     )

    (use_declarations
     (use_declarations "," use_declaration)
     use_declaration
     )

    (inline_use_declaration
     unprefixed_use_declaration
     (use_type unprefixed_use_declaration)
     )

    (unprefixed_use_declaration
     namespace_name
     (namespace_name T_AS T_STRING)
     )

    (use_declaration
     legacy_namespace_name
     (legacy_namespace_name T_AS T_STRING)
     )

    (const_list
     (const_list "," const_decl)
     const_decl
     )

    (inner_statement_list
     (inner_statement_list inner_statement)
     %empty
     )

    (inner_statement
     statement
     attributed_statement
     (attributes attributed_statement)
     (T_HALT_COMPILER "(" ")" ";")
     )

    (statement
     ("{" inner_statement_list "}")
     if_stmt
     alt_if_stmt
     (T_WHILE "(" expr ")" while_statement)
     (T_DO statement T_WHILE "(" expr ")" ";")
     (T_FOR "(" for_exprs ";" for_exprs ";" for_exprs ")" for_statement)
     (T_SWITCH "(" expr ")" switch_case_list)
     (T_BREAK optional_expr ";")
     (T_CONTINUE optional_expr ";")
     (T_RETURN optional_expr ";")
     (T_GLOBAL global_var_list ";")
     (T_STATIC static_var_list ";")
     (T_ECHO echo_expr_list ";")
     T_INLINE_HTML
     (expr ";")
     (T_UNSET "(" unset_variables possible_comma ")" ";" )
     (T_FOREACH "(" expr T_AS foreach_variable ")" foreach_statement)
     (T_FOREACH "(" expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable ")" foreach_statement)
     (T_DECLARE "(" const_list ")")
     declare_statement
     ";"
     (T_TRY "{" inner_statement_list "}" catch_list finally_statement)
     (T_GOTO T_STRING ";")
     T_STRING
     )

    (catch_list
     %empty
     (catch_list T_CATCH "(" catch_name_list optional_variable ")" "{" inner_statement_list "}")
     )

    (catch_name_list
     class_name
     (catch_name_list "|" class_name)
     )

    (optional_variable
     %empty
     T_VARIABLE)

    (finally_statement
     %empty
     (T_FINALLY "{" inner_statement_list "}")
     )

    (unset_variables
     unset_variable
     (unset_variables "," unset_variable)
     )

    (unset_variable
     variable)

    (function_declaration_statement
     (function returns_ref T_STRING backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags)
     )

    (is_reference
     %empty
     "&"
     )

    (is_variadic
     %empty
     T_ELLIPSIS
     )

    (class_declaration_statement
     (class_modifiers T_CLASS)
     (T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}")
     T_CLASS
     (T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}")
     )

    (class_modifiers
     class_modifier
     (class_modifiers class_modifier)
     )

    (class_modifier
     T_ABSTRACT
     T_FINAL
     )

    (trait_declaration_statement
     T_TRAIT
     (T_STRING backup_doc_comment "{" class_statement_list "}")
     )

    (interface_declaration_statement
     T_INTERFACE
     (T_STRING interface_extends_list backup_doc_comment "{" class_statement_list "}")
     )

    (extends_from
     %empty
     (T_EXTENDS class_name)
     )

    (interface_extends_list
     %empty
     (T_EXTENDS class_name_list)
     )

    (implements_list
     %empty
     (T_IMPLEMENTS class_name_list)
     )

    (foreach_variable
     variable
     ("&" variable)
     (T_LIST "(" array_pair_list ")")
     ("[" array_pair_list "]")
     )

    (for_statement
     statement
     (":" inner_statement_list T_ENDFOR ";")
     )

    (foreach_statement
     statement
     (":" inner_statement_list T_ENDFOREACH ";")
     )

    (declare_statement
     statement
     (":" inner_statement_list T_ENDDECLARE ";")
     )

    (switch_case_list
     ("{" case_list "}")
     ("{" ";" case_list "}")
     (":" case_list T_ENDSWITCH ";")
     (":" ";" case_list T_ENDSWITCH ";")
     )

    (case_list
     %empty
     (case_list T_CASE expr case_separator inner_statement_list)
     (case_list T_DEFAULT case_separator inner_statement_list)
     )

    (case_separator
     ":"
     ";"
     )

    (match
     (T_MATCH "(" expr ")" "{" match_arm_list "}")
     )

    (match_arm_list
     %empty
     (non_empty_match_arm_list possible_comma)
     )

    (non_empty_match_arm_list
     match_arm
     (non_empty_match_arm_list "," match_arm)
     )

    (match_arm
     (match_arm_cond_list possible_comma T_DOUBLE_ARROW expr)
     (T_DEFAULT possible_comma T_DOUBLE_ARROW expr)
     )

    (match_arm_cond_list
     expr
     (match_arm_cond_list "," expr)
     )

    (while_statement
     statement
     (":" inner_statement_list T_ENDWHILE ";")
     )

    (if_stmt_without_else
     (T_IF "(" expr ")" statement)
     (if_stmt_without_else T_ELSEIF "(" expr ")" statement)
     )

    (if_stmt
     (if_stmt_without_else T_NOELSE)
     (if_stmt_without_else T_ELSE statement)
     )

    (alt_if_stmt_without_else
     (T_IF "(" expr ")" ":" inner_statement_list)
     (alt_if_stmt_without_else T_ELSEIF "(" expr ")" ":" inner_statement_list)
     )

    (alt_if_stmt
     (alt_if_stmt_without_else T_ENDIF ";")
     (alt_if_stmt_without_else T_ELSE ":" inner_statement_list T_ENDIF ";")
     )

    (parameter_list
     (non_empty_parameter_list possible_comma)
     %empty
     )

    (non_empty_parameter_list
     attributed_parameter
     (non_empty_parameter_list "," attributed_parameter)
     )

    (attributed_parameter
     (attributes parameter)
     parameter
     )

    (optional_visibility_modifier
     %empty
     T_PUBLIC
     T_PROTECTED
     T_PRIVATE
     )

    (parameter
     (optional_visibility_modifier optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment)
     (is_reference is_variadic T_VARIABLE backup_doc_comment)
     (optional_visibility_modifier optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment "=" expr)
     )

    (optional_type_without_static
     %empty
     type_expr_without_static
     )

    (type_expr
     type
     ("?" type)
     union_type
     )

    (type
     type_without_static
     T_STATIC
     )

    (union_type
     (type "|" type)
     (union_type "|" type)
     )

    (type_expr_without_static
     type_without_static
     ("?" type_without_static)
     union_type_without_static)

    (type_without_static
     T_ARRAY
     T_CALLABLE
     name)

    (union_type_without_static
     (type_without_static "|" type_without_static)
     (union_type_without_static "|" type_without_static))

    (return_type
     %empty
     (":" type_expr))

    (argument_list
     ("(" ")")
     ("(" non_empty_argument_list possible_comma ")"))

    (non_empty_argument_list
     argument
     (non_empty_argument_list "," argument))

    (argument
     expr
     (identifier ":" expr)
     (T_ELLIPSIS expr))

    (global_var_list
     (global_var_list "," global_var)
     global_var)

    (global_var
     simple_variable)

    (static_var_list
     (static_var_list "," static_var)
     static_var)

    (static_var
     T_VARIABLE
     (T_VARIABLE "=" expr))

    (class_statement_list
     (class_statement_list class_statement)
     %empty)

    (attributed_class_statement
     (variable_modifiers optional_type_without_static property_list ",")
     (method_modifiers T_CONST class_const_list ";")
     (method_modifiers function returns_ref identifier backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags method_body backup_fn_flags))

    (class_statement
     attributed_class_statement
     (attributes attributed_class_statement)
     (T_USE class_name_list trait_adaptations))

    (class_name_list
     class_name
     (class_name_list "," class_name))

    (trait_adaptations
     ";"
     ("{" "}")
     ("{" trait_adaptation_list "}"))

    (trait_adaptation_list
     trait_adaptation
     (trait_adaptation_list trait_adaptation))

    (trait_adaptation
     (trait_precedence ";")
     (trait_alias ";"))

    (trait_precedence
     (absolute_trait_method_reference T_INSTEADOF class_name_list))

    

    )
  "The productions of grammar.")

(defconst
  phps-mode-automation-grammar-start
  'start
  "The entry-point of grammar.")

(defconst
  phps-mode-automation-grammar-e-identifier
  '%empty
  "The e-identifier of grammar.")

;; TODO Add lex-analyzer as well


(provide 'phps-mode-automation-grammar)

;;; phps-mode-automation-grammar.el ends here

