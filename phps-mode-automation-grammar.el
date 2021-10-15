;;; phps-mode-automation-grammar --- Grammar -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:


(require 'phps-mode-lexer)

;; Just to stop linter from complaining
(defvar
 phps-mode-parser-position
 nil
 "Position of parser.")

(defvar
 phps-mode-parser-tokens
 nil
 "Reversed list of tokens.")

(defvar
  phps-mode-automation-grammar--context-sensitive-attributes
  '(%prec)
  "List of context-sensitive attributes.")

(defvar
  phps-mode-automation-grammar--lr-context-sensitive-precedence-attribute
  '%prec
  "The LR-parser's context-sensitive precedence attribute.")

(defvar
  phps-mode-automation-grammar--global-attributes
  '(%left %nonassoc %precedence %right)
  "List of valid global attributes.")

(defvar
  phps-mode-automation-grammar--lr-global-precedence-attributes
  '(%left %nonassoc %precedence %right)
  "The LR-parser's list of global precedence attributes.")

(defvar
  phps-mode-automation-grammar--lr--allow-default-conflict-resolution
  t
  "Allow shift resolution to shift/reduce conflicts were precedence is missing.")

(defvar
  phps-mode-automation-grammar--global-declaration
  '(
    (%precedence T_THROW)
    (%precedence PREC_ARROW_FUNCTION)
    (%precedence T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE)
    (%left T_LOGICAL_OR)
    (%left T_LOGICAL_XOR)
    (%left T_LOGICAL_AND)
    (%precedence T_PRINT)
    (%precedence T_YIELD)
    (%precedence T_DOUBLE_ARROW)
    (%precedence T_YIELD_FROM)
    (%precedence "=" T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_POW_EQUAL T_COALESCE_EQUAL)
    (%left "?" ":")
    (%right T_COALESCE)
    (%left T_BOOLEAN_OR)
    (%left T_BOOLEAN_AND)
    (%left "|")
    (%left "^")
    (%left "&")
    (%nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_SPACESHIP)
    (%nonassoc "<" T_IS_SMALLER_OR_EQUAL ">" T_IS_GREATER_OR_EQUAL)
    (%left ".")
    (%left T_SL T_SR)
    (%left "+" "-")
    (%left "*" "/" "%")
    (%precedence "!")
    (%precedence T_INSTANCEOF)
    (%precedence "~" T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST "@" )
    (%right T_POW)
    (%precedence T_CLONE)
    (%precedence T_NOELSE)
    (%precedence T_ELSEIF)
    (%precedence T_ELSE)
    )
  "Declaration for grammar.")

(defvar
  phps-mode-automation-grammar--non-terminals
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
    echo_expr
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

(defvar
  phps-mode-automation-grammar--terminals
  '(
    "!"
    "%"
    "&"
    "("
    ")"
    "*"
    "+"
    "."
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
    "}"
    "{"
    "^"
    "|"
    "~"
    "`"
    "'"
    "\""
    "$"
    PREC_ARROW_FUNCTION
    T_ABSTRACT
    T_AND_EQUAL
    T_ARG
    T_ARRAY
    T_ARRAY_CAST
    T_ARROW
    T_AS
    T_ASSIGN
    T_ATTRIBUTE
    T_BAD
    T_BOOL_CAST
    T_BOOLEAN_AND
    T_BOOLEAN_OR
    T_BREAK
    T_CALL
    T_CALLABLE
    T_CASE
    T_CATCH
    T_CLASS
    T_CLASS_C
    T_CLONE
    T_CLOSE
    T_CLOSURE
    T_COALESCE
    T_COALESCE_EQUAL
    T_COMMENT
    T_COMPILER
    T_CONCAT_EQUAL
    T_CONDITIONAL
    T_CONST
    T_CONSTANT_ENCAPSED_STRING
    T_CONTINUE
    T_CURLY_OPEN
    T_DEC
    T_DECL
    T_DECLARE
    T_DEFAULT
    T_DIM
    T_DIR
    T_DIV_EQUAL
    T_DNUMBER
    T_DO
    T_DOC
    T_DOLLAR_OPEN_CURLY_BRACES
    T_DOUBLE
    T_DOUBLE_ARROW
    T_DOUBLE_CAST
    T_ECHO
    T_ELLIPSIS
    T_ELSE
    T_ELSEIF
    T_EMPTY
    T_ENCAPS
    T_ENCAPSED_AND_WHITESPACE
    T_END_HEREDOC
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
    T_FUNC_C
    T_FUNCTION
    T_GLOBAL
    T_GOTO
    T_GREATER
    T_GROUP
    T_HALT_COMPILER
    T_IDENTICAL
    T_IF
    T_IMPLEMENTS
    T_INC
    T_INCLUDE
    T_INCLUDE_ONCE
    T_INLINE_HTML
    T_INSTANCEOF
    T_INSTEADOF
    T_INT_CAST
    T_INTERFACE
    T_IS_IDENTICAL
    T_IS_NOT_IDENTICAL
    T_IS_EQUAL
    T_IS_NOT_EQUAL
    T_IS_SMALLER_OR_EQUAL
    T_IS_GREATER_OR_EQUAL
    T_ISSET
    T_LABEL
    T_LINE
    T_LIST
    T_LNUMBER
    T_LOGICAL_AND
    T_LOGICAL_OR
    T_LOGICAL_XOR
    T_MAGIC
    T_MATCH
    T_METHOD
    T_METHOD_C
    T_MINUS_EQUAL
    T_MOD_EQUAL
    T_MUL_EQUAL
    T_NAME_FULLY_QUALIFIED
    T_NAME_RELATIVE
    T_NAME_QUALIFIED
    T_NAMESPACE
    T_NEW
    T_NOELSE
    T_NS_C
    T_NULLSAFE_OBJECT_OPERATOR
    T_NUM_STRING
    T_NS_SEPARATOR
    T_OBJECT_CAST
    T_OBJECT_OPERATOR
    T_OPEN
    T_OPERATOR
    T_OR_EQUAL
    T_PAAMAYIM_NEKUDOTAYIM
    T_PARAM
    T_PLUS_EQUAL
    T_POST
    T_POW
    T_POW_EQUAL
    T_PRE
    T_PRINT
    T_PRIVATE
    T_PROP
    T_PROTECTED
    T_PUBLIC
    T_REF
    T_REQUIRE
    T_REQUIRE_ONCE
    T_RETURN
    T_SHELL
    T_SILENCE
    T_SL
    T_SL_EQUAL
    T_SPACESHIP
    T_SR
    T_SR_EQUAL
    T_START_HEREDOC
    T_STATIC
    T_STMT
    T_STRING
    T_STRING_CAST
    T_STRING_VARNAME
    T_SWITCH
    T_THROW
    T_TRAIT
    T_TRAIT_C
    T_TRY
    T_TYPE
    T_UNARY
    T_UNPACK
    T_UNSET
    T_UNSET_CAST
    T_USE
    T_VAR
    T_VARIABLE
    T_WHILE
    T_WHITESPACE
    T_XOR_EQUAL
    T_YIELD
    T_YIELD_FROM
    )
  "The terminals of grammar.")

(defvar
  phps-mode-automation-grammar--look-ahead-number
  1
  "The look-ahead number of grammar.")

(defvar
  phps-mode-automation-grammar--header
  "(require 'phps-mode-lexer)\n(require 'semantic)\n(require 'semantic/lex)\n\n(defvar-local\n phps-mode-parser-position\n nil\n \"Position of parser.\")\n(defvar-local\n phps-mode-parser-tokens\n nil\n \"Reversed list of lexer tokens.\")\n"
  "Header contents for parser.")

(defvar
  phps-mode-automation-grammar--start
  'start
  "The entry-point of grammar.")

(defvar
  phps-mode-automation-grammar--e-identifier
  '%empty
  "The e-identifier of grammar.")

(defvar
  phps-mode-automation-grammar--eof-identifier
  '$
  "The EOF-identifier of grammar.")

(defvar
  phps-mode-automation-grammar--lex-analyzer-function
  (lambda (buffer-index)

    ;; Create lexer buffer if none exists
    (unless (get-buffer "*PHPs Lexer*")
      (generate-new-buffer "*PHPs Lexer*")
      (let ((old-buffer
             (buffer-substring-no-properties
              (point-min)
              (point-max))))
        (with-current-buffer "*PHPs Lexer*"
          (insert old-buffer))))
    
    (with-current-buffer "*PHPs Lexer*"
      (let ((token-list-index))

        ;; Unless we have lexed the buffer
        (unless phps-mode-parser-tokens
          (unless phps-mode-lexer--generated-tokens
            ;; Reset lexer
            (setq-local
             phps-mode-lexer--generated-tokens
             nil)
            (setq-local
             phps-mode-lexer--state
             'ST_INITIAL)
            (setq-local
             phps-mode-lexer--states
             nil)
            (setq-local
             phps-mode-lexer--state-stack
             nil)
            (setq-local
             phps-mode-lexer--heredoc-label
             nil)
            (setq-local
             phps-mode-lexer--heredoc-label-stack
             nil)
            (setq-local
             phps-mode-lexer--nest-location-stack
             nil)
            (goto-char (point-min))

            ;; Run lexer on entire buffer here
            (let ((index (point))
                  (max-index (point-max)))
              (while (< index max-index)
                (phps-mode-lexer--re2c)
                (setq
                 index
                 semantic-lex-end-point)
                (goto-char index))))
          (setq-local
           phps-mode-parser-tokens
           (reverse
            phps-mode-lexer--generated-tokens))

          ;; Reset buffer-index to token-list-index connections
          (setq-local
           phps-mode-parser-position
           nil))

        (if (and
             phps-mode-parser-position
             (= (car (car phps-mode-parser-position)) buffer-index))
            (progn
              (setq
               token-list-index
               (car (cdr (car phps-mode-parser-position)))))

          ;; Search from last requested index and forward until
          ;; we find a token starting at or after buffer-index and
          ;; use this as buffer-index, save buffer-index to
          ;; token-list-index connection
          (let ((previous-token-list-index 0))
            (when (and
                   phps-mode-parser-position
                   (< (car (car phps-mode-parser-position)) buffer-index))
              (setq
               previous-token-list-index
               (car (cdr (car phps-mode-parser-position)))))

            (let ((temp-token-list-index
                   previous-token-list-index)
                  (token-list-size
                   (length
                    phps-mode-parser-tokens))
                  (continue t))
              (while (and
                      continue
                      (<
                       temp-token-list-index
                       token-list-size))
                (let ((token
                       (nth
                        temp-token-list-index
                        phps-mode-parser-tokens)))

                  ;; When token starts at cursor we found correct index
                  ;; Save it
                  (when (= (car (cdr token)) buffer-index)
                    (let ((token-type (car token)))
                      (push
                       (list
                        buffer-index
                        temp-token-list-index)
                       phps-mode-parser-position)
                      (unless (or
                               (equal token-type 'T_OPEN_TAG)
                               (equal token-type 'T_CLOSE_TAG)
                               (equal token-type 'T_DOC_COMMENT)
                               (equal token-type 'T_COMMENT))
                        (setq
                         token-list-index
                         temp-token-list-index)
                        (setq
                         continue
                         nil))))

                  ;; When token starts after cursor, flag move of cursor
                  ;; Save it
                  (when (> (car (cdr token)) buffer-index)
                    (let ((token-type (car token)))
                      (push
                       (list
                        (car (cdr token))
                        temp-token-list-index)
                       phps-mode-parser-position)
                      (unless (or
                               (equal token-type 'T_OPEN_TAG)
                               (equal token-type 'T_CLOSE_TAG)
                               (equal token-type 'T_DOC_COMMENT)
                               (equal token-type 'T_COMMENT))
                        (setq-local
                         phps-mode-parser-lex-analyzer--move-to-index-flag
                         (car (cdr token)))
                        (setq
                         continue
                         nil))))

                  (setq
                   temp-token-list-index
                   (1+ temp-token-list-index))
                  )))))

        (when
            token-list-index
          (let ((token
                 (nth
                  token-list-index
                  phps-mode-parser-tokens)))
            (when (equal (car token) 'T_OPEN_TAG_WITH_ECHO)
              (setf
               (car token)
               'T_ECHO))
            token)))))
  "The custom lex-analyzer.")

(defvar
  phps-mode-automation-grammar--precedence-comparison-function
  (lambda(a-type a-value _b-type b-value)
    (cond

     ((and
       a-value
       b-value)
      (cond
       ((> a-value b-value)
        t)

       ((< a-value b-value)
        nil)

       ((= a-value b-value)

        (cond
         ((equal a-type '%left)
          t)

         ((equal a-type '%right)
          nil)

         ((equal a-type '%precedence)
          t))

        )))

     ((and
       a-value
       (not b-value))
      t)

     ((and
       (not a-value)
       (not b-value))
      nil)

     ))
  "The precedence comparison function of the grammar.")

(defvar
  phps-mode-automation-grammar--lex-analyzer-get-function
  (lambda (token)
    (with-current-buffer "*PHPs Lexer*"
      (let ((start (car (cdr token)))
            (end (cdr (cdr token))))
        (when (<= end (point-max))
          (buffer-substring-no-properties
           start
           end)))))
  "Fetch token meta data.")


(provide 'phps-mode-automation-grammar)

;;; phps-mode-automation-grammar.el ends here

