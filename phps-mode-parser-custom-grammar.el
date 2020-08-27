;;; phps-mode-parser-custom-grammar.el -- Grammar for the parser for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

;; Variables and constants

(defvar phps-mode-parser-custom-grammar nil
  "The current grammar.")

(defconst phps-mode-parser-custom-grammar--start-state 'start
  "The default start state for the grammar.")

(defvar-local phps-mode-parser-custom-grammar--state nil
  "The current state of grammar.")

(defvar-local phps-mode-parser-custom-grammar--state-history nil
  "History of state changes.")

;; Functions

(defmacro phps-mode-paser-custom-grammar--block (name &rest patterns)
  "Add block NAME with PATTERNS."
  `(puthash
    ,name
    (list ,@patterns)
    phps-mode-parser-custom-grammar))


;; Setup grammar
(setq phps-mode-parser-custom-grammar (make-hash-table :test 'equal))

(phps-mode-paser-custom-grammar--block 'empty (list (list nil)))

(phps-mode-paser-custom-grammar--block 'start (list (list 'top_statement_list)))

(phps-mode-paser-custom-grammar--block
 'reserved_non_modifiers
 (list (list 'T_INCLUDE))
 (list (list 'T_INCLUDE_ONCE))
 (list (list 'T_EVAL))
 (list (list 'T_REQUIRE))
 (list (list 'T_REQUIRE_ONCE))
 (list (list 'T_LOGICAL_OR))
 (list (list 'T_LOGICAL_XOR))
 (list (list 'T_LOGICAL_AND))
 (list (list 'T_INSTANCEOF))
 (list (list 'T_NEW))
 (list (list 'T_CLONE))
 (list (list 'T_EXIT))
 (list (list 'T_IF))
 (list (list 'T_ELSEIF))
 (list (list 'T_ELSE))
 (list (list 'T_ENDIF))
 (list (list 'T_ECHO))
 (list (list 'T_DO))
 (list (list 'T_WHILE))
 (list (list 'T_ENDWHILE))
 (list (list 'T_FOR))
 (list (list 'T_ENDFOR))
 (list (list 'T_FOREACH))
 (list (list 'T_ENDFOREACH))
 (list (list 'T_DECLARE))
 (list (list 'T_ENDDECLARE))
 (list (list 'T_AS))
 (list (list 'T_TRY))
 (list (list 'T_CATCH))
 (list (list 'T_FINALLY))
 (list (list 'T_THROW))
 (list (list 'T_USE))
 (list (list 'T_INSTEADOF))
 (list (list 'T_GLOBAL))
 (list (list 'T_VAR))
 (list (list 'T_UNSET))
 (list (list 'T_ISSET))
 (list (list 'T_EMPTY))
 (list (list 'T_CONTINUE))
 (list (list 'T_GOTO))
 (list (list 'T_FUNCTION))
 (list (list 'T_CONST))
 (list (list 'T_RETURN))
 (list (list 'T_PRINT))
 (list (list 'T_YIELD))
 (list (list 'T_LIST))
 (list (list 'T_SWITCH))
 (list (list 'T_ENDSWITCH))
 (list (list 'T_CASE))
 (list (list 'T_DEFAULT))
 (list (list 'T_BREAK))
 (list (list 'T_ARRAY))
 (list (list 'T_CALLABLE))
 (list (list 'T_EXTENDS))
 (list (list 'T_IMPLEMENTS))
 (list (list 'T_NAMESPACE))
 (list (list 'T_TRAIT))
 (list (list 'T_INTERFACE))
 (list (list 'T_CLASS))
 (list (list 'T_CLASS_C))
 (list (list 'T_TRAIT_C))
 (list (list 'T_FUNC_C))
 (list (list 'T_METHOD_C))
 (list (list 'T_LINE))
 (list (list 'T_FILE))
 (list (list 'T_DIR))
 (list (list 'T_NS_C))
 (list (list 'T_FN)))

(phps-mode-paser-custom-grammar--block
 'semi_reserved
 (list (list 'reserved_non_modifiers))
 (list (list 'T_STATIC))
 (list (list 'T_ABSTRACT))
 (list (list 'T_FINAL))
 (list (list 'T_PRIVATE))
 (list (list 'T_PROTECTED))
 (list (list 'T_PUBLIC)))

(phps-mode-paser-custom-grammar--block
 'identifier
 (list (list 'T_STRING))
 (list (list 'semi_reserved)))

(phps-mode-paser-custom-grammar--block
 'top_statement_list
 (list (list 'top_statement_list 'top_statement))
 (list (list 'semi_reserved) (lambda(a) (list 'zval a))))

(phps-mode-paser-custom-grammar--block
 'namespace_name
 (list (list 'T_STRING))
 (list (list 'namespace_name 'T_NS_SEPARATOR 'T_STRING) (lambda(a) (message "NAMESPACE-%s" a))))

(phps-mode-paser-custom-grammar--block
 'name
 (list (list 'namespace_name) (lambda(a) (list 'attr 'phps-mode-parser--ZEND_NAME_NOT_FQ (car (car a)))))
 (list (list 'T_NAMESPACE 'T_NS_SEPARATOR 'namespace_name (lambda(_a _b c) (list 'attr 'phps-mode-parser--ZEND_NAME_RELATIVE c))))
 (list (list 'T_NS_SEPARATOR 'namespace_name) (lambda(_a b) (list 'attr 'phps-mode-parser--ZEND_NAME_FQ b))))

(phps-mode-paser-custom-grammar--block
 'top_statement
 (list (list 'statement))
 (list (list 'function_declaration_statement))
 (list (list 'class_declaration_statement))
 (list (list 'trait_declaration_statement))
 (list (list 'interface_declaration_statement))
 (list
  (list 'T_HALT_COMPILER "(" ")" ";")
  (lambda(_a) (list 'phps-mode-parser--ZEND_AST_HALT_COMPILER (point))))
 (list
  (list 'T_NAMESPACE 'namespace_name ";")
  (lambda(_a b _c) (list 'phps-mode-parser--ZEND_AST_NAMESPACE b)))
 (list
  (list 'T_NAMESPACE 'namespace_name "{" 'top_statement_list "}")
  (lambda(_a b _c d _e)
    (list (list 'phps-mode-parser--ZEND_AST_NAMESPACE b) d)))
 (list
  (list 'T_NAMESPACE "{" 'top_statement_list "}")
  (lambda(_a _b c _d) (list 'phps-mode-parser--ZEND_AST_NAMESPACE c)))
 (list
  (list 'T_USE 'mixed_group_use_declaration ";")
  (lambda(_a b _c) b))
 (list
  (list 'T_USE 'use_type 'group_use_declaration ";")
  (lambda(_a b c _d) (list 'attr b c)))
 (list
  (list 'T_USE 'use_declarations ";")
  (lambda(_a b _c) (list 'attr b 'phps-mode-parser--ZEND_SYMBOL_CLASS)))
 (list
  (list 'T_CONST 'const_list ";")
  (lambda(_a b _c) b)))

(phps-mode-paser-custom-grammar--block
 'use_type
 (list (list 'T_FUNCTION) (lambda(_a) (list 'phps-mode-parser--ZEND_SYMBOL_FUNCTION)))
 (list (list 'T_CONST) (lambda(_a) (list 'phps-mode-parser--ZEND_SYMBOL_CONST))))

(phps-mode-paser-custom-grammar--block
 'group_use_declaration:
 (list (list 'namespace_name 'T_NS_SEPARATOR "{" 'unprefixed_use_declarations 'possible_comma "}") (lambda (_a _b _c _d _e _f)))
 (list (list 'T_NS_SEPARATOR 'namespace_name 'T_NS_SEPARATOR "{" 'unprefixed_use_declarations 'possible_comma "}") (lambda (_a _b _c _d _e _f _g _h))))

(phps-mode-paser-custom-grammar--block
 'mixed_group_use_declaration:
 (list (list 'namespace_name 'T_NS_SEPARATOR "{" 'inline_use_declarations 'possible_comma "}") (lambda(_a _b _c _d _e _f)))
 (list (list 'T_NS_SEPARATOR 'namespace_name 'T_NS_SEPARATOR "{" 'inline_use_declarations 'possible_comma "}") (lambda(_a _b _c _d _e _f _g _h))))

(phps-mode-paser-custom-grammar--block
 'possible_comma
 (list (list 'empty))
 (list (list ",")))

(phps-mode-paser-custom-grammar--block
 'inline_use_declarations
 (list
  (list (list 'inline_use_declarations "," 'inline_use_declaration)(lambda(_a _b _c)))
  (list (list 'inline_use_declaration) (lambda (_a)))))

(phps-mode-paser-custom-grammar--block
 'unprefixed_use_declarations
 (list (list 'unprefixed_use_declarations "," 'unprefixed_use_declaration) (lambda(_a _b _c)))
 (list (list 'unprefixed_use_declaration) (lambda (_a))))

(phps-mode-paser-custom-grammar--block
 'use_declaration
 (list (list 'unprefixed_use_declaration) (lambda(_a)))
 (list (list 'T_NS_SEPARATOR 'unprefixed_use_declaration) (lambda(_a _b))))

(phps-mode-paser-custom-grammar--block
 'const_list
 (list (list 'const_list "," 'const_decl) (lambda(_a _b _c)))
 (list (list 'const_decl) (lambda(_a))))

(phps-mode-paser-custom-grammar--block
 'inner_statement_list
 (list (list 'inner_statement_list 'inner_statement) (lambda(_a _b)))
 (list (list 'empty)(lambda(_a))))

(phps-mode-paser-custom-grammar--block
 'inner_statement
 (list (list 'statement) (lambda(_a)))
 (list (list 'function_declaration_statement) (lambda(_a)))
 (list (list 'class_declaration_statement) (lambda(_a)))
 (list (list 'trait_declaration_statement) (lambda(_a)))
 (list (list 'interface_declaration_statement) (lambda(_a)))
 (list (list 'T_HALT_COMPILER "(" ")" ";") (lambda(_a _b _c _d))))

(phps-mode-paser-custom-grammar--block
 'statement
 (list (list "{" 'inner_statement_list "}") (lambda(_a _b _c)))
 (list (list 'if_stmt) (lambda(_a)))
 (list (list 'alt_if_stmt) (lambda(_a)))
 (list (list 'T_WHILE "(" 'expr ")" 'while_statement) (lambda(_a _b _c _d _e)))
 (list (list 'T_DO 'statement 'T_WHILE "(" 'expr ")" ";") (lambda(_a _b _c _d _e _f _g)))
 (list (list 'T_FOR "(" 'for_exprs ";" 'for_exprs ";" 'for_exprs ")" 'for_statement) (lambda(_a _b _c _d _e _f _g _h _i)))
 (list (list 'T_SWITCH "(" 'expr ")" 'switch_case_list) (lambda(_a)))
 (list (list 'T_BREAK 'optional_expr ";") (lambda(_a)))
 (list (list 'T_CONTINUE 'optional_expr ";") (lambda(_a)))
 (list (list 'T_RETURN 'optional_expr ";") (lambda(_a)))
 (list (list 'T_GLOBAL 'global_var_list ";") (lambda(_a)))
 (list (list 'T_STATIC 'static_var_list ";") (lambda(_a)))
 (list (list 'T_ECHO 'echo_expr_list ";") (lambda(_a)))
 (list (list 'T_INLINE_HTML) (lambda(_a)))
 (list (list 'expr ";") (lambda(_a)))
 (list (list 'T_UNSET "(" 'unset_variables 'possible_comma ")" ";") (lambda(_a)))
 (list (list 'T_FOREACH "(" 'expr 'T_AS 'foreach_variable ")" 'foreach_statement) (lambda(_a)))
 (list (list 'T_FOREACH "(" 'expr 'T_AS 'foreach_variable 'T_DOUBLE_ARROW 'foreach_variable ")" 'foreach_statement) (lambda(_a)))
 (list (list 'T_DECLARE "(" 'const_list ")") (lambda(_a)))
 (list (list ";"))
 (list (list 'T_TRY "{" 'inner_statement_list "}" 'catch_list 'finally_statement) (lambda(_a)))
 (list (list 'T_GOTO 'T_STRING ";") (lambda(_a)))
 (list (list 'T_STRING ",") (lambda(_a))))

(phps-mode-paser-custom-grammar--block
 'catch_list
 (list (list 'empty) (lambda(_a)))
 (list (list 'catch_list 'T_CATCH "(" 'catch_name_list 'optional_variable ")" "{" 'inner_statement_list "}") (lambda(_a))))

(phps-mode-paser-custom-grammar--block
 'catch_name_list
 (list (list 'class_name) (lambda(_a)))
 (list (list 'catch_name_list "|" 'class_name) (lambda(_a))))

(phps-mode-paser-custom-grammar--block
 'optional_variable
 (list (list 'empty) (lambda(_a)))
 (list (list 'T_VARIABLE) (lambda(_a))))

(phps-mode-paser-custom-grammar--block
 'finally_statement
 (list (list 'empty) (lambda(_a)))
 (list (list 'T_FINALLY "{" 'inner_statement_list "}") (lambda(_a))))

(phps-mode-paser-custom-grammar--block
 'unset_variables
 (list (list 'unset_variable) (lambda(_a)))
 (list (list 'unset_variables "," 'unset_variable) (lambda(_a))))

(phps-mode-paser-custom-grammar--block
 'unset_variable
 (list (list 'variable) (lambda(_a))))

(phps-mode-paser-custom-grammar--block
 'function_declaration_statement
 (list (list 'function 'returns_ref 'T_STRING 'backup_doc_comment "(" 'parameter_list ")" 'return_type 'backup_fn_flags "{" 'inner_statement_list "}" 'backup_fn_flags)))

(phps-mode-paser-custom-grammar--block
 'is_reference:
 (list (list 'empty))
 (list (list 'BITWISE_AND)))

(phps-mode-paser-custom-grammar--block
 'is_variadic
 (list (list 'empty))
 (list (list 'T_ELLIPSIS)))

(phps-mode-paser-custom-grammar--block
 'class_declaration_statement
 (list (list 'class_modifiers 'T_CLASS 'T_STRING 'extends_from 'implements_list 'backup_doc_comment "{" 'class_statement_list "}"))
 (list (list 'T_CLASS 'T_STRING 'extends_from 'implements_list 'backup_doc_comment "{" 'class_statement_list "}")))

(phps-mode-paser-custom-grammar--block
 'class_modifiers
 (list (list 'class_modifier))
 (list (list 'class_modifiers 'class_modifier)))

(phps-mode-paser-custom-grammar--block
 'class_modifier
 (list (list 'T_ABSTRACT))
 (list (list 'T_FINAL)))

(phps-mode-paser-custom-grammar--block
 'trait_declaration_statement
 (list (list 'T_TRAIT 'T_STRING 'backup_doc_comment "{" 'class_statement_list "}")))

(phps-mode-paser-custom-grammar--block
 'interface_declaration_statement
 (list (list 'T_INTERFACE 'T_STRING 'interface_extends_list 'backup_doc_comment "{" 'class_statement_list "}")))

(phps-mode-paser-custom-grammar--block
 'extends_from
 (list (list 'empty))
 (list (list 'T_EXTENDS 'class_name)))

(phps-mode-paser-custom-grammar--block
 'interface_extends_list
 (list (list 'empty))
 (list (list 'T_EXTENDS 'class_name_list)))

(phps-mode-paser-custom-grammar--block
 'implements_list
 (list (list 'empty))
 (list (list 'T_IMPLEMENTS 'class_name_list)))

(phps-mode-paser-custom-grammar--block
 'foreach_variable
 (list (list 'variable))
 (list (list "&" 'variable))
 (list (list 'T_LIST "(" 'array_pair_list ")"))
 (list (list "{" 'array_pair_list "}")))

(phps-mode-paser-custom-grammar--block
 'for_statement
 (list (list 'statement))
 (list (list "," 'inner_statement_list 'T_ENDFOR ";")))

(phps-mode-paser-custom-grammar--block
 'foreach_statement
 (list (list 'statement))
 (list (list "," 'inner_statement_list 'T_ENDFOREACH ";")))

(phps-mode-paser-custom-grammar--block
 'declare_statement
 (list (list 'statement))
 (list (list "," 'inner_statement_list 'T_ENDDECLARE ";")))

(phps-mode-paser-custom-grammar--block
 'switch_case_list
 (list (list "{" 'case_list "}"))
 (list (list "{" ";" 'case_list "}"))
 (list (list "," 'case_list 'T_ENDSWITCH ";"))
 (list (list ":" ";" 'case_list 'T_ENDSWITCH ";")))

(phps-mode-paser-custom-grammar--block
 'case_list
 (list (list 'empty))
 (list (list 'case_list 'T_CASE 'expr 'case_separator 'inner_statement_list))
 (list (list 'case_list 'T_DEFAULT 'case_separator 'inner_statement_list)))

(phps-mode-paser-custom-grammar--block
 'case_separator
 (list (list ":"))
 (list (list ";")))

(phps-mode-paser-custom-grammar--block
 'while_statement
 (list (list 'statement))
 (list (list ":" 'inner_statement_list 'T_ENDWHILE ";")))

(phps-mode-paser-custom-grammar--block
 'if_stmt_without_else
 (list (list 'T_IF "(" 'expr ")" 'statement))
 (list (list 'if_stmt_without_else 'T_ELSEIF "(" 'expr ")" 'statement)))

(phps-mode-paser-custom-grammar--block
 'if_stmt
 (list (list 'if_stmt_without_else 'T_NOELSE))
 (list (list 'if_stmt_without_else 'T_ELSE 'statement)))

(phps-mode-paser-custom-grammar--block
 'alt_if_stmt_without_else
 (list (list 'T_IF "(" 'expr ")" ":" 'inner_statement_list))
 (list (list 'alt_if_stmt_without_else 'T_ELSEIF "(" 'expr ")" ":" 'inner_statement_list)))

(phps-mode-paser-custom-grammar--block
 'alt_if_stmt
 (list (list 'alt_if_stmt_without_else 'T_ENDIF ";"))
 (list (list 'alt_if_stmt_without_else 'T_ELSE ":" 'inner_statement_list 'T_ENDIF ";")))

(phps-mode-paser-custom-grammar--block
 'parameter_list
 (list (list 'non_empty_parameter_list 'possible_comma))
 (list (list 'empty)))

(phps-mode-paser-custom-grammar--block
 'non_empty_parameter_list
 (list (list 'parameter))
 (list (list 'non_empty_parameter_list "," 'parameter)))

(phps-mode-paser-custom-grammar--block
 'parameter
 (list (list 'optional_type_without_static 'is_reference 'is_variadic 'T_VARIABLE))
 (list (list 'optional_type_without_static 'is_reference 'is_variadic 'T_VARIABLE "=" 'expr)))

(phps-mode-paser-custom-grammar--block
 'optional_type_without_static
 (list (list 'empty))
 (list (list 'type_expr_without_static)))

(phps-mode-paser-custom-grammar--block
 'type_expr
 (list (list 'type))
 (list (list "?" 'type))
 (list (list 'union_type)))

(phps-mode-paser-custom-grammar--block
 'type
 (list (list 'type_without_static))
 (list (list 'T_STATIC)))

(phps-mode-paser-custom-grammar--block
 'union_type
 (list (list 'type "|" 'type))
 (list (list 'union_type "|" 'type)))

;; Duplicate the type rules without "static"
;; to avoid conflicts with "static" modifier for properties.

(phps-mode-paser-custom-grammar--block
 'type_expr_without_static
 (list (list 'type_without_static))
 (list (list "?" 'type_without_static))
 (list (list 'union_type_without_static)))

(phps-mode-paser-custom-grammar--block
 'type_without_static
 (list (list 'T_ARRAY))
 (list (list 'T_CALLABLE))
 (list (list 'name)))

(phps-mode-paser-custom-grammar--block
 'union_type_without_static
 (list (list 'type_without_static "|" 'type_without_static))
 (list (list 'union_type_without_static "|" 'type_without_static)))

(phps-mode-paser-custom-grammar--block
 'return_type
 (list (list 'empty))
 (list (list ":" 'type_expr)))

(phps-mode-paser-custom-grammar--block
 'argument_list
 (list (list "(" ")"))
 (list (list "(" 'non_empty_argument_list 'possible_comma ")")))

(phps-mode-paser-custom-grammar--block
 'non_empty_argument_list
 (list (list 'argument))
 (list (list 'non_empty_argument_list "," 'argument)))

(phps-mode-paser-custom-grammar--block
 'argument
 (list (list 'expr))
 (list 'T_ELLIPSIS 'expr))

(phps-mode-paser-custom-grammar--block
 'global_var_list
 (list (list 'global_var_list "," 'global_var))
 (list (list 'global_var)))

(phps-mode-paser-custom-grammar--block
 'global_var
 (list (list 'simple_variable)))

(phps-mode-paser-custom-grammar--block
 'static_var_list
 (list (list 'static_var_list "," 'static_var))
 (list (list 'static_var)))

(phps-mode-paser-custom-grammar--block
 'static_var
 (list (list 'T_VARIABLE))
 (list (list 'T_VARIABLE "=" 'expr)))

(phps-mode-paser-custom-grammar--block
 'class_statement_list
 (list (list 'class_statement_list 'class_statement))
 (list (list 'empty)))

(phps-mode-paser-custom-grammar--block
 'class_statement
 (list (list 'variable_modifiers 'optional_type_without_static 'property_list ","))
 (list (list 'method_modifiers 'T_CONST 'class_const_list ";"))
 (list (list 'T_USE 'class_name_list 'trait_adaptations))
 (list (list 'method_modifiers 'function 'returns_ref 'identifier 'backup_doc_comment "(" 'parameter_list ")"
             'return_type 'backup_fn_flags 'method_body 'backup_fn_flags)))

(phps-mode-paser-custom-grammar--block
 'class_name_list
 (list (list 'class_name))
 (list (list 'class_name_list "," 'class_name)))

(phps-mode-paser-custom-grammar--block
 'trait_adaptations
 (list (list ";"))
 (list (list "{" "}"))
 (list (list "{" 'trait_adaptation_list "}")))

(phps-mode-paser-custom-grammar--block
 'trait_adaptation_list
 (list (list 'trait_adaptation))
 (list (list 'trait_adaptation_list 'trait_adaptation)))

(phps-mode-paser-custom-grammar--block
 'trait_adaptation
 (list (list 'trait_precedence))
 (list (list 'trait_alias ";")))

(phps-mode-paser-custom-grammar--block
 'trait_precedence
 (list (list 'absolute_trait_method_reference 'T_INSTEADOF 'class_name_list)))

(phps-mode-paser-custom-grammar--block
 'trait_alias
 (list (list 'trait_method_reference 'T_AS 'T_STRING))
 (list (list 'trait_method_reference 'T_AS 'reserved_non_modifiers))
 (list (list 'trait_method_reference 'T_AS 'member_modifier 'identifier))
 (list (list 'trait_method_reference 'T_AS 'member_modifier)))

(phps-mode-paser-custom-grammar--block
 'trait_method_reference
 (list (list 'identifier))
 (list (list 'absolute_trait_method_reference)))

(phps-mode-paser-custom-grammar--block
 'absolute_trait_method_reference
 (list (list 'class_name 'T_PAAMAYIM_NEKUDOTAYIM 'identifier)))

(phps-mode-paser-custom-grammar--block
 'method_body
 (list (list ";")) ;; abstract method
 (list (list "{" 'inner_statement_list "}")))

(phps-mode-paser-custom-grammar--block
 'variable_modifiers
 (list (list 'non_empty_member_modifiers))
 (list (list 'T_VAR)))

(phps-mode-paser-custom-grammar--block
 'method_modifiers
 (list (list 'empty))
 (list (list 'non_empty_member_modifiers)))

(phps-mode-paser-custom-grammar--block
 'non_empty_member_modifiers
 (list (list 'member_modifier))
 (list (list 'non_empty_member_modifiers 'member_modifier)))

(phps-mode-paser-custom-grammar--block
 'member_modifier
 (list (list 'T_PUBLIC))
 (list (list 'T_PROTECTED))
 (list (list 'T_PRIVATE))
 (list (list 'T_STATIC))
 (list (list 'T_ABSTRACT))
 (list (list 'T_FINAL)))

(phps-mode-paser-custom-grammar--block
 'property_list
 (list (list 'property_list "," 'property))
 (list (list 'property)))

(phps-mode-paser-custom-grammar--block
 'class_const_list
 (list (list 'class_const_list "," 'class_const_decl))
 (list (list 'class_const_decl)))

(phps-mode-paser-custom-grammar--block
 'class_const_decl
 (list (list 'identifier "=" 'expr 'backup_doc_comment)))

(phps-mode-paser-custom-grammar--block
 'const_decl
 (list (list 'T_STRING "=" 'expr 'backup_doc_comment)))

(phps-mode-paser-custom-grammar--block
 'echo_expr_list
 (list (list 'echo_expr_list "," 'echo_expr))
 (list (list 'echo_expr)))

(phps-mode-paser-custom-grammar--block
 'for_exprs
 (list (list 'empty))
 (list (list 'non_empty_for_exprs)))

(phps-mode-paser-custom-grammar--block
 'non_empty_for_exprs
 (list (list 'non_empty_for_exprs "," 'expr))
 (list (list 'expr)))

(phps-mode-paser-custom-grammar--block
 'anonymous_class
 (list (list 'T_CLASS 'ctor_arguments 'extends_from 'implements_list 'backup_doc_comment "{" 'class_statement_list "}")))

(phps-mode-paser-custom-grammar--block
 'new_expr
 (list (list 'T_NEW 'class_name_reference 'ctor_arguments))
 (list (list 'T_NEW 'anonymous_class)))

(phps-mode-paser-custom-grammar--block
 'expr
 (list (list 'variable))
 (list (list 'T_LIST "(" 'array_pair_list ")" "=" 'expr))
 (list (list "[" 'array_pair_list "]" "=" 'expr))
 (list (list 'variable "=" 'expr))
 (list (list 'variable "=" "&" 'variable))
 (list (list 'T_CLONE 'expr))
 (list (list 'variable 'T_PLUS_EQUAL 'expr))
 (list (list 'variable 'T_MINUS_EQUAL 'expr))
 (list (list 'variable 'T_MUL_EQUAL 'expr))
 (list (list 'variable 'T_POW_EQUAL 'expr))
 (list (list 'variable 'T_DIV_EQUAL 'expr))
 (list (list 'variable 'T_CONCAT_EQUAL 'expr))
 (list (list 'variable 'T_MOD_EQUAL 'expr))
 (list (list 'variable 'T_AND_EQUAL 'expr))
 (list (list 'variable 'T_OR_EQUAL 'expr))
 (list (list 'variable 'T_XOR_EQUAL 'expr))
 (list (list 'variable 'T_SL_EQUAL 'expr))
 (list (list 'variable 'T_SR_EQUAL 'expr))
 (list (list 'variable 'T_COALESCE_EQUAL 'expr))
 (list (list 'variable 'T_INC))
 (list (list 'T_INC 'variable))
 (list (list 'variable 'T_DEC))
 (list (list 'T_DEC 'variable))
 (list (list 'expr 'T_BOOLEAN_OR 'expr))
 (list (list 'expr 'T_BOOLEAN_AND 'expr))
 (list (list 'expr 'T_LOGICAL_OR 'expr))
 (list (list 'expr 'T_LOGICAL_AND 'expr))
 (list (list 'expr 'T_LOGICAL_XOR 'expr))
 (list (list 'expr "|" 'expr))
 (list (list 'expr "&" 'expr))
 (list (list 'expr "^" 'expr))
 (list (list 'expr "." 'expr))
 (list (list 'expr "+" 'expr))
 (list (list 'expr "-" 'expr))
 (list (list 'expr "*" 'expr))
 (list (list 'expr 'T_POW 'expr))
 (list (list 'expr "/" 'expr))
 (list (list 'expr "%" 'expr))
 (list (list 'expr 'T_SL 'expr))
 (list (list 'expr 'T_SR 'expr))
 (list (list "+" 'expr "~"))
 (list (list "-" 'expr "~"))
 (list (list "!" 'expr))
 (list (list "~" 'expr))
 (list (list 'expr 'T_IS_IDENTICAL 'expr))
 (list (list 'expr 'T_IS_NOT_IDENTICAL 'expr))
 (list (list 'expr 'T_IS_EQUAL 'expr))
 (list (list 'expr 'T_IS_NOT_EQUAL 'expr))
 (list (list 'expr "<" 'expr))
 (list (list 'expr 'T_IS_SMALLER_OR_EQUAL 'expr))
 (list (list 'expr ">" 'expr))
 (list (list 'expr 'T_IS_GREATER_OR_EQUAL 'expr))
 (list (list 'expr 'T_SPACESHIP 'expr))
 (list (list 'expr 'T_INSTANCEOF 'class_name_reference))
 (list (list "(" 'expr ")"))
 (list (list 'new_expr))
 (list (list 'expr "?" 'expr ":" 'expr))
 (list (list 'expr "?" ":" 'expr))
 (list (list 'expr 'T_COALESCE 'expr))
 (list (list 'internal_functions_in_yacc))
 (list (list 'T_INT_CAST 'expr))
 (list (list 'T_DOUBLE_CAST 'expr))
 (list (list 'T_STRING_CAST 'expr))
 (list (list 'T_ARRAY_CAST 'expr))
 (list (list 'T_OBJECT_CAST 'expr))
 (list (list 'T_BOOL_CAST 'expr))
 (list (list 'T_UNSET_CAST 'expr))
 (list (list 'T_EXIT 'exit_expr))
 (list (list "@" 'expr))
 (list (list 'scalar))
 (list (list "`" 'backticks_expr "`"))
 (list (list 'T_PRINT 'expr))
 (list (list 'T_YIELD))
 (list (list 'T_YIELD 'expr))
 (list (list 'T_YIELD 'expr 'T_DOUBLE_ARROW 'expr))
 (list (list 'T_YIELD_FROM 'expr))
 (list (list 'T_THROW 'expr))
 (list (list 'inline_function))
 (list (list 'T_STATIC 'inline_function)))

(phps-mode-paser-custom-grammar--block
 'inline_function
 (list (list 'function 'returns_ref 'backup_doc_comment "(" 'parameter_list ")" 'lexical_vars 'return_type 'backup_fn_flags "{" 'inner_statement_list "}" 'backup_fn_flags))
 (list (list 'fn 'returns_ref "(" 'parameter_list ")" 'return_type 'backup_doc_comment 'T_DOUBLE_ARROW 'backup_fn_flags 'backup_lex_pos 'expr 'backup_fn_flags)))

(phps-mode-paser-custom-grammar--block
 'fn
 (list (list 'T_FN)))

(phps-mode-paser-custom-grammar--block
 'function
 (list (list 'T_FUNCTION)))

(phps-mode-paser-custom-grammar--block
 'backup_doc_comment
 (list (list 'empty)))

(phps-mode-paser-custom-grammar--block
 'backup_fn_flags
 (list (list 'PREC_ARROW_FUNCTION 'empty)))

(phps-mode-paser-custom-grammar--block
 'backup_lex_pos
 (list (list 'empty)))

(phps-mode-paser-custom-grammar--block
 'returns_ref
 (list (list 'empty))
 (list (list "&")))

(phps-mode-paser-custom-grammar--block
 'lexical_vars
 (list (list 'empty))
 (list (list 'T_USE "(" 'lexical_var_list ")")))

(phps-mode-paser-custom-grammar--block
 'lexical_var_list
 (list (list 'lexical_var_list "," 'lexical_var))
 (list (list 'lexical_var)))

(phps-mode-paser-custom-grammar--block
 'lexical_var
 (list (list 'T_VARIABLE))
 (list (list "&" 'T_VARIABLE)))

(phps-mode-paser-custom-grammar--block
 'function_call
 (list (list 'name 'argument_list))
 (list (list 'class_name 'T_PAAMAYIM_NEKUDOTAYIM 'member_name 'argument_list))
 (list (list 'variable_class_name 'T_PAAMAYIM_NEKUDOTAYIM 'member_name 'argument_list))
 (list (list 'callable_expr 'argument_list)))

(phps-mode-paser-custom-grammar--block
 'class_name
 (list (list 'T_STATIC))
 (list (list 'name)))

(phps-mode-paser-custom-grammar--block
 'class_name_reference
 (list (list 'class_name))
 (list (list 'new_variable))
 (list (list "(" 'expr ")")))

(phps-mode-paser-custom-grammar--block
 'exit_expr
 (list (list 'empty))
 (list (list "(" 'optional_expr ")")))

(phps-mode-paser-custom-grammar--block
 'backticks_expr
 (list (list 'empty))
 (list (list 'T_ENCAPSED_AND_WHITESPACE))
 (list (list 'encaps_list)))

(phps-mode-paser-custom-grammar--block
 'ctor_arguments
 (list (list 'empty))
 (list (list 'argument_list)))

(phps-mode-paser-custom-grammar--block
 'dereferencable_scalar
 (list (list 'T_ARRAY "(" 'array_pair_list ")") (lambda(_a _b _c _d)))
 (list (list "{" 'array_pair_list "}") (lambda (_a _b _c)))
 (list (list 'T_CONSTANT_ENCAPSED_STRING) (lambda (_a)))
 (list (list "\"" 'encaps_list "\"") (lambda (_a _b _c))))

scalar:
		T_LNUMBER 	{ $$ = $1; }
	|	T_DNUMBER 	{ $$ = $1; }
	|	T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC { $$ = $2; }
	|	T_START_HEREDOC T_END_HEREDOC
			{ $$ = zend_ast_create_zval_from_str(ZSTR_EMPTY_ALLOC()); }
	|	T_START_HEREDOC encaps_list T_END_HEREDOC { $$ = $2; }
	|	dereferencable_scalar	{ $$ = $1; }
	|	constant				{ $$ = $1; }
	|	class_constant			{ $$ = $1; }
;

constant:
		name		{ $$ = zend_ast_create(ZEND_AST_CONST, $1); }
	|	T_LINE 		{ $$ = zend_ast_create_ex(ZEND_AST_MAGIC_CONST, T_LINE); }
	|	T_FILE 		{ $$ = zend_ast_create_ex(ZEND_AST_MAGIC_CONST, T_FILE); }
	|	T_DIR   	{ $$ = zend_ast_create_ex(ZEND_AST_MAGIC_CONST, T_DIR); }
	|	T_TRAIT_C	{ $$ = zend_ast_create_ex(ZEND_AST_MAGIC_CONST, T_TRAIT_C); }
	|	T_METHOD_C	{ $$ = zend_ast_create_ex(ZEND_AST_MAGIC_CONST, T_METHOD_C); }
	|	T_FUNC_C	{ $$ = zend_ast_create_ex(ZEND_AST_MAGIC_CONST, T_FUNC_C); }
	|	T_NS_C		{ $$ = zend_ast_create_ex(ZEND_AST_MAGIC_CONST, T_NS_C); }
	|	T_CLASS_C	{ $$ = zend_ast_create_ex(ZEND_AST_MAGIC_CONST, T_CLASS_C); }
;

class_constant:
		class_name T_PAAMAYIM_NEKUDOTAYIM identifier
			{ $$ = zend_ast_create_class_const_or_name($1, $3); }
	|	variable_class_name T_PAAMAYIM_NEKUDOTAYIM identifier
			{ $$ = zend_ast_create_class_const_or_name($1, $3); }
;

optional_expr:
		%empty	{ $$ = NULL; }
	|	expr		{ $$ = $1; }
;

variable_class_name:
		fully_dereferencable { $$ = $1; }
;

fully_dereferencable:
		variable				{ $$ = $1; }
	|	'(' expr ')'			{ $$ = $2; }
	|	dereferencable_scalar	{ $$ = $1; }
	|	class_constant			{ $$ = $1; }
;

array_object_dereferencable:
		fully_dereferencable	{ $$ = $1; }
	|	constant				{ $$ = $1; }
;

callable_expr:
		callable_variable		{ $$ = $1; }
	|	'(' expr ')'			{ $$ = $2; }
	|	dereferencable_scalar	{ $$ = $1; }
;

callable_variable:
		simple_variable
			{ $$ = zend_ast_create(ZEND_AST_VAR, $1); }
	|	array_object_dereferencable '[' optional_expr ']'
			{ $$ = zend_ast_create(ZEND_AST_DIM, $1, $3); }
	|	array_object_dereferencable '{' expr '}'
			{ $$ = zend_ast_create_ex(ZEND_AST_DIM, ZEND_DIM_ALTERNATIVE_SYNTAX, $1, $3); }
	|	array_object_dereferencable T_OBJECT_OPERATOR property_name argument_list
			{ $$ = zend_ast_create(ZEND_AST_METHOD_CALL, $1, $3, $4); }
	|	function_call { $$ = $1; }
;

variable:
		callable_variable
			{ $$ = $1; }
	|	static_member
			{ $$ = $1; }
	|	array_object_dereferencable T_OBJECT_OPERATOR property_name
			{ $$ = zend_ast_create(ZEND_AST_PROP, $1, $3); }
;

simple_variable:
		T_VARIABLE			{ $$ = $1; }
	|	'$' '{' expr '}'	{ $$ = $3; }
	|	'$' simple_variable	{ $$ = zend_ast_create(ZEND_AST_VAR, $2); }
;

static_member:
		class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable
			{ $$ = zend_ast_create(ZEND_AST_STATIC_PROP, $1, $3); }
	|	variable_class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable
			{ $$ = zend_ast_create(ZEND_AST_STATIC_PROP, $1, $3); }
;

new_variable:
		simple_variable
			{ $$ = zend_ast_create(ZEND_AST_VAR, $1); }
	|	new_variable '[' optional_expr ']'
			{ $$ = zend_ast_create(ZEND_AST_DIM, $1, $3); }
	|	new_variable '{' expr '}'
			{ $$ = zend_ast_create_ex(ZEND_AST_DIM, ZEND_DIM_ALTERNATIVE_SYNTAX, $1, $3); }
	|	new_variable T_OBJECT_OPERATOR property_name
			{ $$ = zend_ast_create(ZEND_AST_PROP, $1, $3); }
	|	class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable
			{ $$ = zend_ast_create(ZEND_AST_STATIC_PROP, $1, $3); }
	|	new_variable T_PAAMAYIM_NEKUDOTAYIM simple_variable
			{ $$ = zend_ast_create(ZEND_AST_STATIC_PROP, $1, $3); }
;

member_name:
		identifier { $$ = $1; }
	|	'{' expr '}'	{ $$ = $2; }
	|	simple_variable	{ $$ = zend_ast_create(ZEND_AST_VAR, $1); }
;

property_name:
		T_STRING { $$ = $1; }
	|	'{' expr '}'	{ $$ = $2; }
	|	simple_variable	{ $$ = zend_ast_create(ZEND_AST_VAR, $1); }
;

array_pair_list:
		non_empty_array_pair_list
			{ /* allow single trailing comma */ $$ = zend_ast_list_rtrim($1); }
;

possible_array_pair:
		%empty { $$ = NULL; }
	|	array_pair  { $$ = $1; }
;

non_empty_array_pair_list:
		non_empty_array_pair_list ',' possible_array_pair
			{ $$ = zend_ast_list_add($1, $3); }
	|	possible_array_pair
			{ $$ = zend_ast_create_list(1, ZEND_AST_ARRAY, $1); }
;

array_pair:
		expr T_DOUBLE_ARROW expr
			{ $$ = zend_ast_create(ZEND_AST_ARRAY_ELEM, $3, $1); }
	|	expr
			{ $$ = zend_ast_create(ZEND_AST_ARRAY_ELEM, $1, NULL); }
	|	expr T_DOUBLE_ARROW '&' variable
			{ $$ = zend_ast_create_ex(ZEND_AST_ARRAY_ELEM, 1, $4, $1); }
	|	'&' variable
			{ $$ = zend_ast_create_ex(ZEND_AST_ARRAY_ELEM, 1, $2, NULL); }
	|	T_ELLIPSIS expr
			{ $$ = zend_ast_create(ZEND_AST_UNPACK, $2); }
	|	expr T_DOUBLE_ARROW T_LIST '(' array_pair_list ')'
			{ $5->attr = ZEND_ARRAY_SYNTAX_LIST;
			  $$ = zend_ast_create(ZEND_AST_ARRAY_ELEM, $5, $1); }
	|	T_LIST '(' array_pair_list ')'
			{ $3->attr = ZEND_ARRAY_SYNTAX_LIST;
			  $$ = zend_ast_create(ZEND_AST_ARRAY_ELEM, $3, NULL); }
;

encaps_list:
		encaps_list encaps_var
			{ $$ = zend_ast_list_add($1, $2); }
	|	encaps_list T_ENCAPSED_AND_WHITESPACE
			{ $$ = zend_ast_list_add($1, $2); }
	|	encaps_var
			{ $$ = zend_ast_create_list(1, ZEND_AST_ENCAPS_LIST, $1); }
	|	T_ENCAPSED_AND_WHITESPACE encaps_var
			{ $$ = zend_ast_create_list(2, ZEND_AST_ENCAPS_LIST, $1, $2); }
;

encaps_var:
		T_VARIABLE
			{ $$ = zend_ast_create(ZEND_AST_VAR, $1); }
	|	T_VARIABLE '[' encaps_var_offset ']'
			{ $$ = zend_ast_create(ZEND_AST_DIM,
			      zend_ast_create(ZEND_AST_VAR, $1), $3); }
	|	T_VARIABLE T_OBJECT_OPERATOR T_STRING
			{ $$ = zend_ast_create(ZEND_AST_PROP,
			      zend_ast_create(ZEND_AST_VAR, $1), $3); }
	|	T_DOLLAR_OPEN_CURLY_BRACES expr '}'
			{ $$ = zend_ast_create(ZEND_AST_VAR, $2); }
	|	T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME '}'
			{ $$ = zend_ast_create(ZEND_AST_VAR, $2); }
	|	T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME '[' expr ']' '}'
			{ $$ = zend_ast_create(ZEND_AST_DIM,
			      zend_ast_create(ZEND_AST_VAR, $2), $4); }
	|	T_CURLY_OPEN variable '}' { $$ = $2; }
;

encaps_var_offset:
		T_STRING			{ $$ = $1; }
	|	T_NUM_STRING		{ $$ = $1; }
	|	'-' T_NUM_STRING 	{ $$ = zend_negate_num_string($2); }
	|	T_VARIABLE			{ $$ = zend_ast_create(ZEND_AST_VAR, $1); }
;


internal_functions_in_yacc:
		T_ISSET '(' isset_variables possible_comma ')' { $$ = $3; }
	|	T_EMPTY '(' expr ')' { $$ = zend_ast_create(ZEND_AST_EMPTY, $3); }
	|	T_INCLUDE expr
			{ $$ = zend_ast_create_ex(ZEND_AST_INCLUDE_OR_EVAL, ZEND_INCLUDE, $2); }
	|	T_INCLUDE_ONCE expr
			{ $$ = zend_ast_create_ex(ZEND_AST_INCLUDE_OR_EVAL, ZEND_INCLUDE_ONCE, $2); }
	|	T_EVAL '(' expr ')'
			{ $$ = zend_ast_create_ex(ZEND_AST_INCLUDE_OR_EVAL, ZEND_EVAL, $3); }
	|	T_REQUIRE expr
			{ $$ = zend_ast_create_ex(ZEND_AST_INCLUDE_OR_EVAL, ZEND_REQUIRE, $2); }
	|	T_REQUIRE_ONCE expr
			{ $$ = zend_ast_create_ex(ZEND_AST_INCLUDE_OR_EVAL, ZEND_REQUIRE_ONCE, $2); }
;

isset_variables:
		isset_variable { $$ = $1; }
	|	isset_variables ',' isset_variable
			{ $$ = zend_ast_create(ZEND_AST_AND, $1, $3); }
;

isset_variable:
		expr { $$ = zend_ast_create(ZEND_AST_ISSET, $1); }
;


(provide 'phps-mode-parser-custom-grammar)

;;; phps-mode-parser-custom-grammar.el ends here
