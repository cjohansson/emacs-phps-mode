;;; phps-mode-parser-sdt.el --- Syntax directed translation for grammar -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.


;;; Commentary:


;; Productions PHP 8.1 grammar:

;; Production 0: ((start) (top_statement_list))
;; Production 1: ((reserved_non_modifiers) (T_INCLUDE))
;; Production 2: ((reserved_non_modifiers) (T_INCLUDE_ONCE))
;; Production 3: ((reserved_non_modifiers) (T_EVAL))
;; Production 4: ((reserved_non_modifiers) (T_REQUIRE))
;; Production 5: ((reserved_non_modifiers) (T_REQUIRE_ONCE))
;; Production 6: ((reserved_non_modifiers) (T_LOGICAL_OR))
;; Production 7: ((reserved_non_modifiers) (T_LOGICAL_XOR))
;; Production 8: ((reserved_non_modifiers) (T_LOGICAL_AND))
;; Production 9: ((reserved_non_modifiers) (T_INSTANCEOF))
;; Production 10: ((reserved_non_modifiers) (T_NEW))
;; Production 11: ((reserved_non_modifiers) (T_CLONE))
;; Production 12: ((reserved_non_modifiers) (T_EXIT))
;; Production 13: ((reserved_non_modifiers) (T_IF))
;; Production 14: ((reserved_non_modifiers) (T_ELSEIF))
;; Production 15: ((reserved_non_modifiers) (T_ELSE))
;; Production 16: ((reserved_non_modifiers) (T_ENDIF))
;; Production 17: ((reserved_non_modifiers) (T_ECHO))
;; Production 18: ((reserved_non_modifiers) (T_DO))
;; Production 19: ((reserved_non_modifiers) (T_WHILE))
;; Production 20: ((reserved_non_modifiers) (T_ENDWHILE))
;; Production 21: ((reserved_non_modifiers) (T_FOR))
;; Production 22: ((reserved_non_modifiers) (T_ENDFOR))
;; Production 23: ((reserved_non_modifiers) (T_FOREACH))
;; Production 24: ((reserved_non_modifiers) (T_ENDFOREACH))
;; Production 25: ((reserved_non_modifiers) (T_DECLARE))
;; Production 26: ((reserved_non_modifiers) (T_ENDDECLARE))
;; Production 27: ((reserved_non_modifiers) (T_AS))
;; Production 28: ((reserved_non_modifiers) (T_TRY))
;; Production 29: ((reserved_non_modifiers) (T_CATCH))
;; Production 30: ((reserved_non_modifiers) (T_FINALLY))
;; Production 31: ((reserved_non_modifiers) (T_THROW))
;; Production 32: ((reserved_non_modifiers) (T_USE))
;; Production 33: ((reserved_non_modifiers) (T_INSTEADOF))
;; Production 34: ((reserved_non_modifiers) (T_GLOBAL))
;; Production 35: ((reserved_non_modifiers) (T_VAR))
;; Production 36: ((reserved_non_modifiers) (T_UNSET))
;; Production 37: ((reserved_non_modifiers) (T_ISSET))
;; Production 38: ((reserved_non_modifiers) (T_EMPTY))
;; Production 39: ((reserved_non_modifiers) (T_CONTINUE))
;; Production 40: ((reserved_non_modifiers) (T_GOTO))
;; Production 41: ((reserved_non_modifiers) (T_FUNCTION))
;; Production 42: ((reserved_non_modifiers) (T_CONST))
;; Production 43: ((reserved_non_modifiers) (T_RETURN))
;; Production 44: ((reserved_non_modifiers) (T_PRINT))
;; Production 45: ((reserved_non_modifiers) (T_YIELD))
;; Production 46: ((reserved_non_modifiers) (T_LIST))
;; Production 47: ((reserved_non_modifiers) (T_SWITCH))
;; Production 48: ((reserved_non_modifiers) (T_ENDSWITCH))
;; Production 49: ((reserved_non_modifiers) (T_CASE))
;; Production 50: ((reserved_non_modifiers) (T_DEFAULT))
;; Production 51: ((reserved_non_modifiers) (T_BREAK))
;; Production 52: ((reserved_non_modifiers) (T_ARRAY))
;; Production 53: ((reserved_non_modifiers) (T_CALLABLE))
;; Production 54: ((reserved_non_modifiers) (T_EXTENDS))
;; Production 55: ((reserved_non_modifiers) (T_IMPLEMENTS))
;; Production 56: ((reserved_non_modifiers) (T_NAMESPACE))
;; Production 57: ((reserved_non_modifiers) (T_TRAIT))
;; Production 58: ((reserved_non_modifiers) (T_INTERFACE))
;; Production 59: ((reserved_non_modifiers) (T_CLASS))
;; Production 60: ((reserved_non_modifiers) (T_CLASS_C))
;; Production 61: ((reserved_non_modifiers) (T_TRAIT_C))
;; Production 62: ((reserved_non_modifiers) (T_FUNC_C))
;; Production 63: ((reserved_non_modifiers) (T_METHOD_C))
;; Production 64: ((reserved_non_modifiers) (T_LINE))
;; Production 65: ((reserved_non_modifiers) (T_FILE))
;; Production 66: ((reserved_non_modifiers) (T_DIR))
;; Production 67: ((reserved_non_modifiers) (T_NS_C))
;; Production 68: ((reserved_non_modifiers) (T_FN))
;; Production 69: ((reserved_non_modifiers) (T_MATCH))
;; Production 70: ((reserved_non_modifiers) (T_ENUM))
;; Production 71: ((semi_reserved) (reserved_non_modifiers))
;; Production 72: ((semi_reserved) (T_STATIC))
;; Production 73: ((semi_reserved) (T_ABSTRACT))
;; Production 74: ((semi_reserved) (T_FINAL))
;; Production 75: ((semi_reserved) (T_PRIVATE))
;; Production 76: ((semi_reserved) (T_PROTECTED))
;; Production 77: ((semi_reserved) (T_PUBLIC))
;; Production 78: ((semi_reserved) (T_READONLY))
;; Production 79: ((ampersand) (T_AMPERSAND_FOLLOWED_BY_VAR_OR_VARARG))
;; Production 80: ((ampersand) (T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG))
;; Production 81: ((identifier) (T_STRING))
;; Production 82: ((identifier) (semi_reserved))
;; Production 83: ((top_statement_list) (top_statement_list top_statement))
;; Production 84: ((top_statement_list) (%empty))
;; Production 85: ((namespace_declaration_name) (identifier))
;; Production 86: ((namespace_declaration_name) (T_NAME_QUALIFIED))
;; Production 87: ((namespace_name) (T_STRING))
;; Production 88: ((namespace_name) (T_NAME_QUALIFIED))
;; Production 89: ((legacy_namespace_name) (namespace_name))
;; Production 90: ((legacy_namespace_name) (T_NAME_FULLY_QUALIFIED))
;; Production 91: ((name) (T_STRING))
;; Production 92: ((name) (T_NAME_QUALIFIED))
;; Production 93: ((name) (T_NAME_FULLY_QUALIFIED))
;; Production 94: ((name) (T_NAME_RELATIVE))
;; Production 95: ((attribute_decl) (class_name))
;; Production 96: ((attribute_decl) (class_name argument_list))
;; Production 97: ((attribute_group) (attribute_decl))
;; Production 98: ((attribute_group) (attribute_group "," attribute_decl))
;; Production 99: ((attribute) (T_ATTRIBUTE attribute_group possible_comma "]"))
;; Production 100: ((attributes) (attribute))
;; Production 101: ((attributes) (attributes attribute))
;; Production 102: ((attributed_statement) (function_declaration_statement))
;; Production 103: ((attributed_statement) (class_declaration_statement))
;; Production 104: ((attributed_statement) (trait_declaration_statement))
;; Production 105: ((attributed_statement) (interface_declaration_statement))
;; Production 106: ((attributed_statement) (enum_declaration_statement))
;; Production 107: ((top_statement) (statement))
;; Production 108: ((top_statement) (attributed_statement))
;; Production 109: ((top_statement) (attributes attributed_statement))
;; Production 110: ((top_statement) (T_HALT_COMPILER "(" ")" ";"))
;; Production 111: ((top_statement) (T_NAMESPACE namespace_declaration_name ";"))
;; Production 112: ((top_statement) (T_NAMESPACE namespace_declaration_name "{" top_statement_list "}"))
;; Production 113: ((top_statement) (T_NAMESPACE "{" top_statement_list "}"))
;; Production 114: ((top_statement) (T_USE mixed_group_use_declaration ";"))
;; Production 115: ((top_statement) (T_USE use_type group_use_declaration ";"))
;; Production 116: ((top_statement) (T_USE use_declarations ";"))
;; Production 117: ((top_statement) (T_USE use_type use_declarations ";"))
;; Production 118: ((top_statement) (T_CONST const_list ";"))
;; Production 119: ((use_type) (T_FUNCTION))
;; Production 120: ((use_type) (T_CONST))
;; Production 121: ((group_use_declaration) (legacy_namespace_name T_NS_SEPARATOR "{" unprefixed_use_declarations possible_comma "}"))
;; Production 122: ((mixed_group_use_declaration) (legacy_namespace_name T_NS_SEPARATOR "{" inline_use_declarations possible_comma "}"))
;; Production 123: ((possible_comma) (%empty))
;; Production 124: ((possible_comma) (","))
;; Production 125: ((inline_use_declarations) (inline_use_declarations "," inline_use_declaration))
;; Production 126: ((inline_use_declarations) (inline_use_declaration))
;; Production 127: ((unprefixed_use_declarations) (unprefixed_use_declarations "," unprefixed_use_declaration))
;; Production 128: ((unprefixed_use_declarations) (unprefixed_use_declaration))
;; Production 129: ((use_declarations) (use_declarations "," use_declaration))
;; Production 130: ((use_declarations) (use_declaration))
;; Production 131: ((inline_use_declaration) (unprefixed_use_declaration))
;; Production 132: ((inline_use_declaration) (use_type unprefixed_use_declaration))
;; Production 133: ((unprefixed_use_declaration) (namespace_name))
;; Production 134: ((unprefixed_use_declaration) (namespace_name T_AS T_STRING))
;; Production 135: ((use_declaration) (legacy_namespace_name))
;; Production 136: ((use_declaration) (legacy_namespace_name T_AS T_STRING))
;; Production 137: ((const_list) (const_list "," const_decl))
;; Production 138: ((const_list) (const_decl))
;; Production 139: ((inner_statement_list) (inner_statement_list inner_statement))
;; Production 140: ((inner_statement_list) (%empty))
;; Production 141: ((inner_statement) (statement))
;; Production 142: ((inner_statement) (attributed_statement))
;; Production 143: ((inner_statement) (attributes attributed_statement))
;; Production 144: ((inner_statement) (T_HALT_COMPILER "(" ")" ";"))
;; Production 145: ((statement) ("{" inner_statement_list "}"))
;; Production 146: ((statement) (if_stmt))
;; Production 147: ((statement) (alt_if_stmt))
;; Production 148: ((statement) (T_WHILE "(" expr ")" while_statement))
;; Production 149: ((statement) (T_DO statement T_WHILE "(" expr ")" ";"))
;; Production 150: ((statement) (T_FOR "(" for_exprs ";" for_exprs ";" for_exprs ")" for_statement))
;; Production 151: ((statement) (T_SWITCH "(" expr ")" switch_case_list))
;; Production 152: ((statement) (T_BREAK optional_expr ";"))
;; Production 153: ((statement) (T_CONTINUE optional_expr ";"))
;; Production 154: ((statement) (T_RETURN optional_expr ";"))
;; Production 155: ((statement) (T_GLOBAL global_var_list ";"))
;; Production 156: ((statement) (T_STATIC static_var_list ";"))
;; Production 157: ((statement) (T_ECHO echo_expr_list ";"))
;; Production 158: ((statement) (T_INLINE_HTML))
;; Production 159: ((statement) (expr ";"))
;; Production 160: ((statement) (T_UNSET "(" unset_variables possible_comma ")" ";"))
;; Production 161: ((statement) (T_FOREACH "(" expr T_AS foreach_variable ")" foreach_statement))
;; Production 162: ((statement) (T_FOREACH "(" expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable ")" foreach_statement))
;; Production 163: ((statement) (T_DECLARE "(" const_list ")" declare_statement))
;; Production 164: ((statement) (";"))
;; Production 165: ((statement) (T_TRY "{" inner_statement_list "}" catch_list finally_statement))
;; Production 166: ((statement) (T_GOTO T_STRING ";"))
;; Production 167: ((statement) (T_STRING ":"))
;; Production 168: ((catch_list) (%empty))
;; Production 169: ((catch_list) (catch_list T_CATCH "(" catch_name_list optional_variable ")" "{" inner_statement_list "}"))
;; Production 170: ((catch_name_list) (class_name))
;; Production 171: ((catch_name_list) (catch_name_list "|" class_name))
;; Production 172: ((optional_variable) (%empty))
;; Production 173: ((optional_variable) (T_VARIABLE))
;; Production 174: ((finally_statement) (%empty))
;; Production 175: ((finally_statement) (T_FINALLY "{" inner_statement_list "}"))
;; Production 176: ((unset_variables) (unset_variable))
;; Production 177: ((unset_variables) (unset_variables "," unset_variable))
;; Production 178: ((unset_variable) (variable))
;; Production 179: ((function_declaration_statement) (function returns_ref T_STRING backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags))
;; Production 180: ((is_reference) (%empty))
;; Production 181: ((is_reference) (T_AMPERSAND_FOLLOWED_BY_VAR_OR_VARARG))
;; Production 182: ((is_variadic) (%empty))
;; Production 183: ((is_variadic) (T_ELLIPSIS))
;; Production 184: ((class_declaration_statement) (class_modifiers T_CLASS T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}"))
;; Production 185: ((class_declaration_statement) (T_CLASS T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}"))
;; Production 186: ((class_modifiers) (class_modifier))
;; Production 187: ((class_modifiers) (class_modifiers class_modifier))
;; Production 188: ((class_modifier) (T_ABSTRACT))
;; Production 189: ((class_modifier) (T_FINAL))
;; Production 190: ((trait_declaration_statement) (T_TRAIT T_STRING backup_doc_comment "{" class_statement_list "}"))
;; Production 191: ((interface_declaration_statement) (T_INTERFACE T_STRING interface_extends_list backup_doc_comment "{" class_statement_list "}"))
;; Production 192: ((enum_declaration_statement) (T_ENUM T_STRING enum_backing_type implements_list backup_doc_comment "{" class_statement_list "}"))
;; Production 193: ((enum_backing_type) (%empty))
;; Production 194: ((enum_backing_type) (":" type_expr))
;; Production 195: ((enum_case) (T_CASE backup_doc_comment identifier enum_case_expr ";"))
;; Production 196: ((enum_case_expr) (%empty))
;; Production 197: ((enum_case_expr) ("=" expr))
;; Production 198: ((extends_from) (%empty))
;; Production 199: ((extends_from) (T_EXTENDS class_name))
;; Production 200: ((interface_extends_list) (%empty))
;; Production 201: ((interface_extends_list) (T_EXTENDS class_name_list))
;; Production 202: ((implements_list) (%empty))
;; Production 203: ((implements_list) (T_IMPLEMENTS class_name_list))
;; Production 204: ((foreach_variable) (variable))
;; Production 205: ((foreach_variable) (ampersand variable))
;; Production 206: ((foreach_variable) (T_LIST "(" array_pair_list ")"))
;; Production 207: ((foreach_variable) ("[" array_pair_list "]"))
;; Production 208: ((for_statement) (statement))
;; Production 209: ((for_statement) (":" inner_statement_list T_ENDFOR ";"))
;; Production 210: ((foreach_statement) (statement))
;; Production 211: ((foreach_statement) (":" inner_statement_list T_ENDFOREACH ";"))
;; Production 212: ((declare_statement) (statement))
;; Production 213: ((declare_statement) (":" inner_statement_list T_ENDDECLARE ";"))
;; Production 214: ((switch_case_list) ("{" case_list "}"))
;; Production 215: ((switch_case_list) ("{" ";" case_list "}"))
;; Production 216: ((switch_case_list) (":" case_list T_ENDSWITCH ";"))
;; Production 217: ((switch_case_list) (":" ";" case_list T_ENDSWITCH ";"))
;; Production 218: ((case_list) (%empty))
;; Production 219: ((case_list) (case_list T_CASE expr case_separator inner_statement_list))
;; Production 220: ((case_list) (case_list T_DEFAULT case_separator inner_statement_list))
;; Production 221: ((case_separator) (":"))
;; Production 222: ((case_separator) (";"))
;; Production 223: ((match) (T_MATCH "(" expr ")" "{" match_arm_list "}"))
;; Production 224: ((match_arm_list) (%empty))
;; Production 225: ((match_arm_list) (non_empty_match_arm_list possible_comma))
;; Production 226: ((non_empty_match_arm_list) (match_arm))
;; Production 227: ((non_empty_match_arm_list) (non_empty_match_arm_list "," match_arm))
;; Production 228: ((match_arm) (match_arm_cond_list possible_comma T_DOUBLE_ARROW expr))
;; Production 229: ((match_arm) (T_DEFAULT possible_comma T_DOUBLE_ARROW expr))
;; Production 230: ((match_arm_cond_list) (expr))
;; Production 231: ((match_arm_cond_list) (match_arm_cond_list "," expr))
;; Production 232: ((while_statement) (statement))
;; Production 233: ((while_statement) (":" inner_statement_list T_ENDWHILE ";"))
;; Production 234: ((if_stmt_without_else) (T_IF "(" expr ")" statement))
;; Production 235: ((if_stmt_without_else) (if_stmt_without_else T_ELSEIF "(" expr ")" statement))
;; Production 236: ((if_stmt) (if_stmt_without_else))
;; Production 237: ((if_stmt) (if_stmt_without_else T_ELSE statement))
;; Production 238: ((alt_if_stmt_without_else) (T_IF "(" expr ")" ":" inner_statement_list))
;; Production 239: ((alt_if_stmt_without_else) (alt_if_stmt_without_else T_ELSEIF "(" expr ")" ":" inner_statement_list))
;; Production 240: ((alt_if_stmt) (alt_if_stmt_without_else T_ENDIF ";"))
;; Production 241: ((alt_if_stmt) (alt_if_stmt_without_else T_ELSE ":" inner_statement_list T_ENDIF ";"))
;; Production 242: ((parameter_list) (non_empty_parameter_list possible_comma))
;; Production 243: ((parameter_list) (%empty))
;; Production 244: ((non_empty_parameter_list) (attributed_parameter))
;; Production 245: ((non_empty_parameter_list) (non_empty_parameter_list "," attributed_parameter))
;; Production 246: ((attributed_parameter) (attributes parameter))
;; Production 247: ((attributed_parameter) (parameter))
;; Production 248: ((optional_property_modifiers) (%empty))
;; Production 249: ((optional_property_modifiers) (optional_property_modifiers property_modifier))
;; Production 250: ((property_modifier) (T_PUBLIC))
;; Production 251: ((property_modifier) (T_PROTECTED))
;; Production 252: ((property_modifier) (T_PRIVATE))
;; Production 253: ((property_modifier) (T_READONLY))
;; Production 254: ((parameter) (optional_property_modifiers optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment))
;; Production 255: ((parameter) (optional_property_modifiers optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment "=" expr))
;; Production 256: ((optional_type_without_static) (%empty))
;; Production 257: ((optional_type_without_static) (type_expr_without_static))
;; Production 258: ((type_expr) (type))
;; Production 259: ((type_expr) ("?" type))
;; Production 260: ((type_expr) (union_type))
;; Production 261: ((type_expr) (intersection_type))
;; Production 262: ((type) (type_without_static))
;; Production 263: ((type) (T_STATIC))
;; Production 264: ((union_type) (type "|" type))
;; Production 265: ((union_type) (union_type "|" type))
;; Production 266: ((intersection_type) (type T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG type))
;; Production 267: ((intersection_type) (intersection_type T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG type))
;; Production 268: ((type_expr_without_static) (type_without_static))
;; Production 269: ((type_expr_without_static) ("?" type_without_static))
;; Production 270: ((type_expr_without_static) (union_type_without_static))
;; Production 271: ((type_expr_without_static) (intersection_type_without_static))
;; Production 272: ((type_without_static) (T_ARRAY))
;; Production 273: ((type_without_static) (T_CALLABLE))
;; Production 274: ((type_without_static) (name))
;; Production 275: ((union_type_without_static) (type_without_static "|" type_without_static))
;; Production 276: ((union_type_without_static) (union_type_without_static "|" type_without_static))
;; Production 277: ((intersection_type_without_static) (type_without_static T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG type_without_static))
;; Production 278: ((intersection_type_without_static) (intersection_type_without_static T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG type_without_static))
;; Production 279: ((return_type) (%empty))
;; Production 280: ((return_type) (":" type_expr))
;; Production 281: ((argument_list) ("(" ")"))
;; Production 282: ((argument_list) ("(" non_empty_argument_list possible_comma ")"))
;; Production 283: ((argument_list) ("(" T_ELLIPSIS ")"))
;; Production 284: ((non_empty_argument_list) (argument))
;; Production 285: ((non_empty_argument_list) (non_empty_argument_list "," argument))
;; Production 286: ((argument) (expr))
;; Production 287: ((argument) (identifier ":" expr))
;; Production 288: ((argument) (T_ELLIPSIS expr))
;; Production 289: ((global_var_list) (global_var_list "," global_var))
;; Production 290: ((global_var_list) (global_var))
;; Production 291: ((global_var) (simple_variable))
;; Production 292: ((static_var_list) (static_var_list "," static_var))
;; Production 293: ((static_var_list) (static_var))
;; Production 294: ((static_var) (T_VARIABLE))
;; Production 295: ((static_var) (T_VARIABLE "=" expr))
;; Production 296: ((class_statement_list) (class_statement_list class_statement))
;; Production 297: ((class_statement_list) (%empty))
;; Production 298: ((attributed_class_statement) (variable_modifiers optional_type_without_static property_list ";"))
;; Production 299: ((attributed_class_statement) (method_modifiers T_CONST class_const_list ";"))
;; Production 300: ((attributed_class_statement) (method_modifiers function returns_ref identifier backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags method_body backup_fn_flags))
;; Production 301: ((attributed_class_statement) (enum_case))
;; Production 302: ((class_statement) (attributed_class_statement))
;; Production 303: ((class_statement) (attributes attributed_class_statement))
;; Production 304: ((class_statement) (T_USE class_name_list trait_adaptations))
;; Production 305: ((class_name_list) (class_name))
;; Production 306: ((class_name_list) (class_name_list "," class_name))
;; Production 307: ((trait_adaptations) (";"))
;; Production 308: ((trait_adaptations) ("{" "}"))
;; Production 309: ((trait_adaptations) ("{" trait_adaptation_list "}"))
;; Production 310: ((trait_adaptation_list) (trait_adaptation))
;; Production 311: ((trait_adaptation_list) (trait_adaptation_list trait_adaptation))
;; Production 312: ((trait_adaptation) (trait_precedence ";"))
;; Production 313: ((trait_adaptation) (trait_alias ";"))
;; Production 314: ((trait_precedence) (absolute_trait_method_reference T_INSTEADOF class_name_list))
;; Production 315: ((trait_alias) (trait_method_reference T_AS T_STRING))
;; Production 316: ((trait_alias) (trait_method_reference T_AS reserved_non_modifiers))
;; Production 317: ((trait_alias) (trait_method_reference T_AS member_modifier identifier))
;; Production 318: ((trait_alias) (trait_method_reference T_AS member_modifier))
;; Production 319: ((trait_method_reference) (identifier))
;; Production 320: ((trait_method_reference) (absolute_trait_method_reference))
;; Production 321: ((absolute_trait_method_reference) (class_name T_PAAMAYIM_NEKUDOTAYIM identifier))
;; Production 322: ((method_body) (";"))
;; Production 323: ((method_body) ("{" inner_statement_list "}"))
;; Production 324: ((variable_modifiers) (non_empty_member_modifiers))
;; Production 325: ((variable_modifiers) (T_VAR))
;; Production 326: ((method_modifiers) (%empty))
;; Production 327: ((method_modifiers) (non_empty_member_modifiers))
;; Production 328: ((non_empty_member_modifiers) (member_modifier))
;; Production 329: ((non_empty_member_modifiers) (non_empty_member_modifiers member_modifier))
;; Production 330: ((member_modifier) (T_PUBLIC))
;; Production 331: ((member_modifier) (T_PROTECTED))
;; Production 332: ((member_modifier) (T_PRIVATE))
;; Production 333: ((member_modifier) (T_STATIC))
;; Production 334: ((member_modifier) (T_ABSTRACT))
;; Production 335: ((member_modifier) (T_FINAL))
;; Production 336: ((member_modifier) (T_READONLY))
;; Production 337: ((property_list) (property_list "," property))
;; Production 338: ((property_list) (property))
;; Production 339: ((property) (T_VARIABLE backup_doc_comment))
;; Production 340: ((property) (T_VARIABLE "=" expr backup_doc_comment))
;; Production 341: ((class_const_list) (class_const_list "," class_const_decl))
;; Production 342: ((class_const_list) (class_const_decl))
;; Production 343: ((class_const_decl) (identifier "=" expr backup_doc_comment))
;; Production 344: ((const_decl) (T_STRING "=" expr backup_doc_comment))
;; Production 345: ((echo_expr_list) (echo_expr_list "," echo_expr))
;; Production 346: ((echo_expr_list) (echo_expr))
;; Production 347: ((echo_expr) (expr))
;; Production 348: ((for_exprs) (%empty))
;; Production 349: ((for_exprs) (non_empty_for_exprs))
;; Production 350: ((non_empty_for_exprs) (non_empty_for_exprs "," expr))
;; Production 351: ((non_empty_for_exprs) (expr))
;; Production 352: ((anonymous_class) (T_CLASS ctor_arguments extends_from implements_list backup_doc_comment "{" class_statement_list "}"))
;; Production 353: ((new_expr) (T_NEW class_name_reference ctor_arguments))
;; Production 354: ((new_expr) (T_NEW anonymous_class))
;; Production 355: ((new_expr) (T_NEW attributes anonymous_class))
;; Production 356: ((expr) (variable))
;; Production 357: ((expr) (T_LIST "(" array_pair_list ")" "=" expr))
;; Production 358: ((expr) ("[" array_pair_list "]" "=" expr))
;; Production 359: ((expr) (variable "=" expr))
;; Production 360: ((expr) (variable "=" ampersand variable))
;; Production 361: ((expr) (T_CLONE expr))
;; Production 362: ((expr) (variable T_PLUS_EQUAL expr))
;; Production 363: ((expr) (variable T_MINUS_EQUAL expr))
;; Production 364: ((expr) (variable T_MUL_EQUAL expr))
;; Production 365: ((expr) (variable T_POW_EQUAL expr))
;; Production 366: ((expr) (variable T_DIV_EQUAL expr))
;; Production 367: ((expr) (variable T_CONCAT_EQUAL expr))
;; Production 368: ((expr) (variable T_MOD_EQUAL expr))
;; Production 369: ((expr) (variable T_AND_EQUAL expr))
;; Production 370: ((expr) (variable T_OR_EQUAL expr))
;; Production 371: ((expr) (variable T_XOR_EQUAL expr))
;; Production 372: ((expr) (variable T_SL_EQUAL expr))
;; Production 373: ((expr) (variable T_SR_EQUAL expr))
;; Production 374: ((expr) (variable T_COALESCE_EQUAL expr))
;; Production 375: ((expr) (variable T_INC))
;; Production 376: ((expr) (T_INC variable))
;; Production 377: ((expr) (variable T_DEC))
;; Production 378: ((expr) (T_DEC variable))
;; Production 379: ((expr) (expr T_BOOLEAN_OR expr))
;; Production 380: ((expr) (expr T_BOOLEAN_AND expr))
;; Production 381: ((expr) (expr T_LOGICAL_OR expr))
;; Production 382: ((expr) (expr T_LOGICAL_AND expr))
;; Production 383: ((expr) (expr T_LOGICAL_XOR expr))
;; Production 384: ((expr) (expr "|" expr))
;; Production 385: ((expr) (expr T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG expr))
;; Production 386: ((expr) (expr T_AMPERSAND_FOLLOWED_BY_VAR_OR_VARARG expr))
;; Production 387: ((expr) (expr "^" expr))
;; Production 388: ((expr) (expr "." expr))
;; Production 389: ((expr) (expr "+" expr))
;; Production 390: ((expr) (expr "-" expr))
;; Production 391: ((expr) (expr "*" expr))
;; Production 392: ((expr) (expr T_POW expr))
;; Production 393: ((expr) (expr "/" expr))
;; Production 394: ((expr) (expr "%" expr))
;; Production 395: ((expr) (expr T_SL expr))
;; Production 396: ((expr) (expr T_SR expr))
;; Production 397: ((expr) ("+" expr))
;; Production 398: ((expr) ("-" expr))
;; Production 399: ((expr) ("!" expr))
;; Production 400: ((expr) ("~" expr))
;; Production 401: ((expr) (expr T_IS_IDENTICAL expr))
;; Production 402: ((expr) (expr T_IS_NOT_IDENTICAL expr))
;; Production 403: ((expr) (expr T_IS_EQUAL expr))
;; Production 404: ((expr) (expr T_IS_NOT_EQUAL expr))
;; Production 405: ((expr) (expr "<" expr))
;; Production 406: ((expr) (expr T_IS_SMALLER_OR_EQUAL expr))
;; Production 407: ((expr) (expr ">" expr))
;; Production 408: ((expr) (expr T_IS_GREATER_OR_EQUAL expr))
;; Production 409: ((expr) (expr T_SPACESHIP expr))
;; Production 410: ((expr) (expr T_INSTANCEOF class_name_reference))
;; Production 411: ((expr) ("(" expr ")"))
;; Production 412: ((expr) (new_expr))
;; Production 413: ((expr) (expr "?" expr ":" expr))
;; Production 414: ((expr) (expr "?" ":" expr))
;; Production 415: ((expr) (expr T_COALESCE expr))
;; Production 416: ((expr) (internal_functions_in_yacc))
;; Production 417: ((expr) (T_INT_CAST expr))
;; Production 418: ((expr) (T_DOUBLE_CAST expr))
;; Production 419: ((expr) (T_STRING_CAST expr))
;; Production 420: ((expr) (T_ARRAY_CAST expr))
;; Production 421: ((expr) (T_OBJECT_CAST expr))
;; Production 422: ((expr) (T_BOOL_CAST expr))
;; Production 423: ((expr) (T_UNSET_CAST expr))
;; Production 424: ((expr) (T_EXIT exit_expr))
;; Production 425: ((expr) ("@" expr))
;; Production 426: ((expr) (scalar))
;; Production 427: ((expr) ("`" backticks_expr "`"))
;; Production 428: ((expr) (T_PRINT expr))
;; Production 429: ((expr) (T_YIELD))
;; Production 430: ((expr) (T_YIELD expr))
;; Production 431: ((expr) (T_YIELD expr T_DOUBLE_ARROW expr))
;; Production 432: ((expr) (T_YIELD_FROM expr))
;; Production 433: ((expr) (T_THROW expr))
;; Production 434: ((expr) (inline_function))
;; Production 435: ((expr) (attributes inline_function))
;; Production 436: ((expr) (T_STATIC inline_function))
;; Production 437: ((expr) (attributes T_STATIC inline_function))
;; Production 438: ((expr) (match))
;; Production 439: ((inline_function) (function returns_ref backup_doc_comment "(" parameter_list ")" lexical_vars return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags))
;; Production 440: ((inline_function) (fn returns_ref backup_doc_comment "(" parameter_list ")" return_type T_DOUBLE_ARROW backup_fn_flags backup_lex_pos expr backup_fn_flags))
;; Production 441: ((fn) (T_FN))
;; Production 442: ((function) (T_FUNCTION))
;; Production 443: ((backup_doc_comment) (%empty))
;; Production 444: ((backup_fn_flags) (%empty))
;; Production 445: ((backup_lex_pos) (%empty))
;; Production 446: ((returns_ref) (%empty))
;; Production 447: ((returns_ref) (ampersand))
;; Production 448: ((lexical_vars) (%empty))
;; Production 449: ((lexical_vars) (T_USE "(" lexical_var_list possible_comma ")"))
;; Production 450: ((lexical_var_list) (lexical_var_list "," lexical_var))
;; Production 451: ((lexical_var_list) (lexical_var))
;; Production 452: ((lexical_var) (T_VARIABLE))
;; Production 453: ((lexical_var) (ampersand T_VARIABLE))
;; Production 454: ((function_call) (name argument_list))
;; Production 455: ((function_call) (class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list))
;; Production 456: ((function_call) (variable_class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list))
;; Production 457: ((function_call) (callable_expr argument_list))
;; Production 458: ((class_name) (T_STATIC))
;; Production 459: ((class_name) (name))
;; Production 460: ((class_name_reference) (class_name))
;; Production 461: ((class_name_reference) (new_variable))
;; Production 462: ((class_name_reference) ("(" expr ")"))
;; Production 463: ((exit_expr) (%empty))
;; Production 464: ((exit_expr) ("(" optional_expr ")"))
;; Production 465: ((backticks_expr) (%empty))
;; Production 466: ((backticks_expr) (T_ENCAPSED_AND_WHITESPACE))
;; Production 467: ((backticks_expr) (encaps_list))
;; Production 468: ((ctor_arguments) (%empty))
;; Production 469: ((ctor_arguments) (argument_list))
;; Production 470: ((dereferenceable_scalar) (T_ARRAY "(" array_pair_list ")"))
;; Production 471: ((dereferenceable_scalar) ("[" array_pair_list "]"))
;; Production 472: ((dereferenceable_scalar) (T_CONSTANT_ENCAPSED_STRING))
;; Production 473: ((dereferenceable_scalar) ("\"" encaps_list "\""))
;; Production 474: ((scalar) (T_LNUMBER))
;; Production 475: ((scalar) (T_DNUMBER))
;; Production 476: ((scalar) (T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC))
;; Production 477: ((scalar) (T_START_HEREDOC T_END_HEREDOC))
;; Production 478: ((scalar) (T_START_HEREDOC encaps_list T_END_HEREDOC))
;; Production 479: ((scalar) (dereferenceable_scalar))
;; Production 480: ((scalar) (constant))
;; Production 481: ((scalar) (class_constant))
;; Production 482: ((constant) (name))
;; Production 483: ((constant) (T_LINE))
;; Production 484: ((constant) (T_FILE))
;; Production 485: ((constant) (T_DIR))
;; Production 486: ((constant) (T_TRAIT_C))
;; Production 487: ((constant) (T_METHOD_C))
;; Production 488: ((constant) (T_FUNC_C))
;; Production 489: ((constant) (T_NS_C))
;; Production 490: ((constant) (T_CLASS_C))
;; Production 491: ((class_constant) (class_name T_PAAMAYIM_NEKUDOTAYIM identifier))
;; Production 492: ((class_constant) (variable_class_name T_PAAMAYIM_NEKUDOTAYIM identifier))
;; Production 493: ((optional_expr) (%empty))
;; Production 494: ((optional_expr) (expr))
;; Production 495: ((variable_class_name) (fully_dereferenceable))
;; Production 496: ((fully_dereferenceable) (variable))
;; Production 497: ((fully_dereferenceable) ("(" expr ")"))
;; Production 498: ((fully_dereferenceable) (dereferenceable_scalar))
;; Production 499: ((fully_dereferenceable) (class_constant))
;; Production 500: ((array_object_dereferenceable) (fully_dereferenceable))
;; Production 501: ((array_object_dereferenceable) (constant))
;; Production 502: ((callable_expr) (callable_variable))
;; Production 503: ((callable_expr) ("(" expr ")"))
;; Production 504: ((callable_expr) (dereferenceable_scalar))
;; Production 505: ((callable_variable) (simple_variable))
;; Production 506: ((callable_variable) (array_object_dereferenceable "[" optional_expr "]"))
;; Production 507: ((callable_variable) (array_object_dereferenceable "{" expr "}"))
;; Production 508: ((callable_variable) (array_object_dereferenceable T_OBJECT_OPERATOR property_name argument_list))
;; Production 509: ((callable_variable) (array_object_dereferenceable T_NULLSAFE_OBJECT_OPERATOR property_name argument_list))
;; Production 510: ((callable_variable) (function_call))
;; Production 511: ((variable) (callable_variable))
;; Production 512: ((variable) (static_member))
;; Production 513: ((variable) (array_object_dereferenceable T_OBJECT_OPERATOR property_name))
;; Production 514: ((variable) (array_object_dereferenceable T_NULLSAFE_OBJECT_OPERATOR property_name))
;; Production 515: ((simple_variable) (T_VARIABLE))
;; Production 516: ((simple_variable) ("$" "{" expr "}"))
;; Production 517: ((simple_variable) ("$" simple_variable))
;; Production 518: ((static_member) (class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable))
;; Production 519: ((static_member) (variable_class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable))
;; Production 520: ((new_variable) (simple_variable))
;; Production 521: ((new_variable) (new_variable "[" optional_expr "]"))
;; Production 522: ((new_variable) (new_variable "{" expr "}"))
;; Production 523: ((new_variable) (new_variable T_OBJECT_OPERATOR property_name))
;; Production 524: ((new_variable) (new_variable T_NULLSAFE_OBJECT_OPERATOR property_name))
;; Production 525: ((new_variable) (class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable))
;; Production 526: ((new_variable) (new_variable T_PAAMAYIM_NEKUDOTAYIM simple_variable))
;; Production 527: ((member_name) (identifier))
;; Production 528: ((member_name) ("{" expr "}"))
;; Production 529: ((member_name) (simple_variable))
;; Production 530: ((property_name) (T_STRING))
;; Production 531: ((property_name) ("{" expr "}"))
;; Production 532: ((property_name) (simple_variable))
;; Production 533: ((array_pair_list) (non_empty_array_pair_list))
;; Production 534: ((possible_array_pair) (%empty))
;; Production 535: ((possible_array_pair) (array_pair))
;; Production 536: ((non_empty_array_pair_list) (non_empty_array_pair_list "," possible_array_pair))
;; Production 537: ((non_empty_array_pair_list) (possible_array_pair))
;; Production 538: ((array_pair) (expr T_DOUBLE_ARROW expr))
;; Production 539: ((array_pair) (expr))
;; Production 540: ((array_pair) (expr T_DOUBLE_ARROW ampersand variable))
;; Production 541: ((array_pair) (ampersand variable))
;; Production 542: ((array_pair) (T_ELLIPSIS expr))
;; Production 543: ((array_pair) (expr T_DOUBLE_ARROW T_LIST "(" array_pair_list ")"))
;; Production 544: ((array_pair) (T_LIST "(" array_pair_list ")"))
;; Production 545: ((encaps_list) (encaps_list encaps_var))
;; Production 546: ((encaps_list) (encaps_list T_ENCAPSED_AND_WHITESPACE))
;; Production 547: ((encaps_list) (encaps_var))
;; Production 548: ((encaps_list) (T_ENCAPSED_AND_WHITESPACE encaps_var))
;; Production 549: ((encaps_var) (T_VARIABLE))
;; Production 550: ((encaps_var) (T_VARIABLE "[" encaps_var_offset "]"))
;; Production 551: ((encaps_var) (T_VARIABLE T_OBJECT_OPERATOR T_STRING))
;; Production 552: ((encaps_var) (T_VARIABLE T_NULLSAFE_OBJECT_OPERATOR T_STRING))
;; Production 553: ((encaps_var) (T_DOLLAR_OPEN_CURLY_BRACES expr "}"))
;; Production 554: ((encaps_var) (T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME "}"))
;; Production 555: ((encaps_var) (T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME "[" expr "]" "}"))
;; Production 556: ((encaps_var) (T_CURLY_OPEN variable "}"))
;; Production 557: ((encaps_var_offset) (T_STRING))
;; Production 558: ((encaps_var_offset) (T_NUM_STRING))
;; Production 559: ((encaps_var_offset) ("-" T_NUM_STRING))
;; Production 560: ((encaps_var_offset) (T_VARIABLE))
;; Production 561: ((internal_functions_in_yacc) (T_ISSET "(" isset_variables possible_comma ")"))
;; Production 562: ((internal_functions_in_yacc) (T_EMPTY "(" expr ")"))
;; Production 563: ((internal_functions_in_yacc) (T_INCLUDE expr))
;; Production 564: ((internal_functions_in_yacc) (T_INCLUDE_ONCE expr))
;; Production 565: ((internal_functions_in_yacc) (T_EVAL "(" expr ")"))
;; Production 566: ((internal_functions_in_yacc) (T_REQUIRE expr))
;; Production 567: ((internal_functions_in_yacc) (T_REQUIRE_ONCE expr))
;; Production 568: ((isset_variables) (isset_variable))
;; Production 569: ((isset_variables) (isset_variables "," isset_variable))
;; Production 570: ((isset_variable) (expr))


;;; Code:

;; TODO Add SDT for
;; argument_list
;; array_pair_list
;; catch_list
;; catch_name_list
;; class_statement_list
;; echo_expr_list
;; expr
;; extends_from
;; for_exprs
;; for_statement
;; foreach_statement
;; global_var_list
;; implements_list
;; inner_statement_list
;; inner_statement_list
;; interface_extends_list
;; isset_variables
;; lexical_vars
;; method_body
;; method_modifiers
;; optional_expr
;; statement
;; variable_modifiers

;; TODO Remove function (phps-mode-parser-sdt--get-list-of-object)


(require 'phps-mode-parser)

(defun phps-mode-parser-sdt--get-list-of-object (objects)
  "Get list of OBJECTS."
  (cond

   ((and (listp objects)
         (plist-get objects 'ast-type))
    (list objects))

   ((listp objects)
    objects)

   (t (list objects))))

;; SDT starts here

;; 0 ((start) (top_statement_list))
(puthash 0 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 1 ((reserved_non_modifiers) (T_INCLUDE))
(puthash 1 (lambda(_args _terminals) 'T_INCLUDE) phps-mode-parser--table-translations)

;; 2 ((reserved_non_modifiers) (T_INCLUDE_ONCE))
(puthash 2 (lambda(_args _terminals) 'T_INCLUDE_ONCE) phps-mode-parser--table-translations)

;; 3 ((reserved_non_modifiers) (T_EVAL))
(puthash 3 (lambda(_args _terminals) 'T_EVAL) phps-mode-parser--table-translations)

;; 4 ((reserved_non_modifiers) (T_REQUIRE))
(puthash 4 (lambda(_args _terminals) 'T_REQUIRE) phps-mode-parser--table-translations)

;; 5 ((reserved_non_modifiers) (T_REQUIRE_ONCE))
(puthash 5 (lambda(_args _terminals) 'T_REQUIRE_ONCE) phps-mode-parser--table-translations)

;; 6 ((reserved_non_modifiers) (T_LOGICAL_OR))
(puthash 6 (lambda(_args _terminals) 'T_LOGICAL_OR) phps-mode-parser--table-translations)

;; 7 ((reserved_non_modifiers) (T_LOGICAL_XOR))
(puthash 7 (lambda(_args _terminals) 'T_LOGICAL_XOR) phps-mode-parser--table-translations)

;; 8 ((reserved_non_modifiers) (T_LOGICAL_AND))
(puthash 8 (lambda(_args _terminals) 'T_LOGICAL_AND) phps-mode-parser--table-translations)

;; 9 ((reserved_non_modifiers) (T_INSTANCEOF))
(puthash 9 (lambda(_args _terminals) 'T_INSTANCEOF) phps-mode-parser--table-translations)

;; 10 ((reserved_non_modifiers) (T_NEW))
(puthash 10 (lambda(_args _terminals) 'T_NEW) phps-mode-parser--table-translations)

;; 11 ((reserved_non_modifiers) (T_CLONE))
(puthash 11 (lambda(_args _terminals) 'T_CLONE) phps-mode-parser--table-translations)

;; 12 ((reserved_non_modifiers) (T_EXIT))
(puthash 12 (lambda(_args _terminals) 'T_EXIT) phps-mode-parser--table-translations)

;; 13 ((reserved_non_modifiers) (T_IF))
(puthash 13 (lambda(_args _terminals) 'T_IF) phps-mode-parser--table-translations)

;; 14 ((reserved_non_modifiers) (T_ELSEIF))
(puthash 14 (lambda(_args _terminals) 'T_ELSEIF) phps-mode-parser--table-translations)

;; 15 ((reserved_non_modifiers) (T_ELSE))
(puthash 15 (lambda(_args _terminals) 'T_ELSE) phps-mode-parser--table-translations)

;; 16 ((reserved_non_modifiers) (T_ENDIF))
(puthash 16 (lambda(_args _terminals) 'T_ENDIF) phps-mode-parser--table-translations)

;; 17 ((reserved_non_modifiers) (T_ECHO))
(puthash 17 (lambda(_args _terminals) 'T_ECHO) phps-mode-parser--table-translations)

;; 18 ((reserved_non_modifiers) (T_DO))
(puthash 18 (lambda(_args _terminals) 'T_DO) phps-mode-parser--table-translations)

;; 19 ((reserved_non_modifiers) (T_WHILE))
(puthash 19 (lambda(_args _terminals) 'T_WHILE) phps-mode-parser--table-translations)

;; 20 ((reserved_non_modifiers) (T_ENDWHILE))
(puthash 20 (lambda(_args _terminals) 'T_ENDWHILE) phps-mode-parser--table-translations)

;; 21 ((reserved_non_modifiers) (T_FOR))
(puthash 21 (lambda(_args _terminals) 'T_FOR) phps-mode-parser--table-translations)

;; 22 ((reserved_non_modifiers) (T_ENDFOR))
(puthash 22 (lambda(_args _terminals) 'T_ENDFOR) phps-mode-parser--table-translations)

;; 23 ((reserved_non_modifiers) (T_FOREACH))
(puthash 23 (lambda(_args _terminals) 'T_FOREACH) phps-mode-parser--table-translations)

;; 24 ((reserved_non_modifiers) (T_ENDFOREACH))
(puthash 24 (lambda(_args _terminals) 'T_ENDFOREACH) phps-mode-parser--table-translations)

;; 25 ((reserved_non_modifiers) (T_DECLARE))
(puthash 25 (lambda(_args _terminals) 'T_DECLARE) phps-mode-parser--table-translations)

;; 26 ((reserved_non_modifiers) (T_ENDDECLARE))
(puthash 26 (lambda(_args _terminals) 'T_ENDDECLARE) phps-mode-parser--table-translations)

;; 27 ((reserved_non_modifiers) (T_AS))
(puthash 27 (lambda(_args _terminals) 'T_AS) phps-mode-parser--table-translations)

;; 28 ((reserved_non_modifiers) (T_TRY))
(puthash 28 (lambda(_args _terminals) 'T_TRY) phps-mode-parser--table-translations)

;; 29 ((reserved_non_modifiers) (T_CATCH))
(puthash 29 (lambda(_args _terminals) 'T_CATCH) phps-mode-parser--table-translations)

;; 30 ((reserved_non_modifiers) (T_FINALLY))
(puthash 30 (lambda(_args _terminals) 'T_FINALLY) phps-mode-parser--table-translations)

;; 31 ((reserved_non_modifiers) (T_THROW))
(puthash 31 (lambda(_args _terminals) 'T_THROW) phps-mode-parser--table-translations)

;; 32 ((reserved_non_modifiers) (T_USE))
(puthash 32 (lambda(_args _terminals) 'T_USE) phps-mode-parser--table-translations)

;; 33 ((reserved_non_modifiers) (T_INSTEADOF))
(puthash 33 (lambda(_args _terminals) 'T_INSTEADOF) phps-mode-parser--table-translations)

;; 34 ((reserved_non_modifiers) (T_GLOBAL))
(puthash 34 (lambda(_args _terminals) 'T_GLOBAL) phps-mode-parser--table-translations)

;; 35 ((reserved_non_modifiers) (T_VAR))
(puthash 35 (lambda(_args _terminals) 'T_VAR) phps-mode-parser--table-translations)

;; 36 ((reserved_non_modifiers) (T_UNSET))
(puthash 36 (lambda(_args _terminals) 'T_UNSET) phps-mode-parser--table-translations)

;; 37 ((reserved_non_modifiers) (T_ISSET))
(puthash 37 (lambda(_args _terminals) 'T_ISSET) phps-mode-parser--table-translations)

;; 38 ((reserved_non_modifiers) (T_EMPTY))
(puthash 38 (lambda(_args _terminals) 'T_EMPTY) phps-mode-parser--table-translations)

;; 39 ((reserved_non_modifiers) (T_CONTINUE))
(puthash 39 (lambda(_args _terminals) 'T_CONTINUE) phps-mode-parser--table-translations)

;; 40 ((reserved_non_modifiers) (T_GOTO))
(puthash 40 (lambda(_args _terminals) 'T_GOTO) phps-mode-parser--table-translations)

;; 41 ((reserved_non_modifiers) (T_FUNCTION))
(puthash 41 (lambda(_args _terminals) 'T_FUNCTION) phps-mode-parser--table-translations)

;; 42 ((reserved_non_modifiers) (T_CONST))
(puthash 42 (lambda(_args _terminals) 'T_CONST) phps-mode-parser--table-translations)

;; 43 ((reserved_non_modifiers) (T_RETURN))
(puthash 43 (lambda(_args _terminals) 'T_RETURN) phps-mode-parser--table-translations)

;; 44 ((reserved_non_modifiers) (T_PRINT))
(puthash 44 (lambda(_args _terminals) 'T_PRINT) phps-mode-parser--table-translations)

;; 45 ((reserved_non_modifiers) (T_YIELD))
(puthash 45 (lambda(_args _terminals) 'T_YIELD) phps-mode-parser--table-translations)

;; 46 ((reserved_non_modifiers) (T_LIST))
(puthash 46 (lambda(_args _terminals) 'T_LIST) phps-mode-parser--table-translations)

;; 47 ((reserved_non_modifiers) (T_SWITCH))
(puthash 47 (lambda(_args _terminals) 'T_SWITCH) phps-mode-parser--table-translations)

;; 48 ((reserved_non_modifiers) (T_ENDSWITCH))
(puthash 48 (lambda(_args _terminals) 'T_ENDSWITCH) phps-mode-parser--table-translations)

;; 49 ((reserved_non_modifiers) (T_CASE))
(puthash 49 (lambda(_args _terminals) 'T_CASE) phps-mode-parser--table-translations)

;; 50 ((reserved_non_modifiers) (T_DEFAULT))
(puthash 50 (lambda(_args _terminals) 'T_DEFAULT) phps-mode-parser--table-translations)

;; 51 ((reserved_non_modifiers) (T_BREAK))
(puthash 51 (lambda(_args _terminals) 'T_BREAK) phps-mode-parser--table-translations)

;; 52 ((reserved_non_modifiers) (T_ARRAY))
(puthash 52 (lambda(_args _terminals) 'T_ARRAY) phps-mode-parser--table-translations)

;; 53 ((reserved_non_modifiers) (T_CALLABLE))
(puthash 53 (lambda(_args _terminals) 'T_CALLABLE) phps-mode-parser--table-translations)

;; 54 ((reserved_non_modifiers) (T_EXTENDS))
(puthash 54 (lambda(_args _terminals) 'T_EXTENDS) phps-mode-parser--table-translations)

;; 55 ((reserved_non_modifiers) (T_IMPLEMENTS))
(puthash 55 (lambda(_args _terminals) 'T_IMPLEMENTS) phps-mode-parser--table-translations)

;; 56 ((reserved_non_modifiers) (T_NAMESPACE))
(puthash 56 (lambda(_args _terminals) 'T_NAMESPACE) phps-mode-parser--table-translations)

;; 57 ((reserved_non_modifiers) (T_TRAIT))
(puthash 57 (lambda(_args _terminals) 'T_TRAIT) phps-mode-parser--table-translations)

;; 58 ((reserved_non_modifiers) (T_INTERFACE))
(puthash 58 (lambda(_args _terminals) 'T_INTERFACE) phps-mode-parser--table-translations)

;; 59 ((reserved_non_modifiers) (T_CLASS))
(puthash 59 (lambda(_args _terminals) 'T_CLASS) phps-mode-parser--table-translations)

;; 60 ((reserved_non_modifiers) (T_CLASS_C))
(puthash 60 (lambda(_args _terminals) 'T_CLASS_C) phps-mode-parser--table-translations)

;; 61 ((reserved_non_modifiers) (T_TRAIT_C))
(puthash 61 (lambda(_args _terminals) 'T_TRAIT_C) phps-mode-parser--table-translations)

;; 62 ((reserved_non_modifiers) (T_FUNC_C))
(puthash 62 (lambda(_args _terminals) 'T_FUNC_C) phps-mode-parser--table-translations)

;; 63 ((reserved_non_modifiers) (T_METHOD_C))
(puthash 63 (lambda(_args _terminals) 'T_METHOD_C) phps-mode-parser--table-translations)

;; 64 ((reserved_non_modifiers) (T_LINE))
(puthash 64 (lambda(_args _terminals) 'T_LINE) phps-mode-parser--table-translations)

;; 65 ((reserved_non_modifiers) (T_FILE))
(puthash 65 (lambda(_args _terminals) 'T_FILE) phps-mode-parser--table-translations)

;; 66 ((reserved_non_modifiers) (T_DIR))
(puthash 66 (lambda(_args _terminals) 'T_DIR) phps-mode-parser--table-translations)

;; 67 ((reserved_non_modifiers) (T_NS_C))
(puthash 67 (lambda(_args _terminals) 'T_NS_C) phps-mode-parser--table-translations)

;; 68 ((reserved_non_modifiers) (T_FN))
(puthash 68 (lambda(_args _terminals) 'T_FN) phps-mode-parser--table-translations)

;; 69 ((reserved_non_modifiers) (T_MATCH))
(puthash 69 (lambda(_args _terminals) 'T_MATCH) phps-mode-parser--table-translations)

;; 70 ((reserved_non_modifiers) (T_ENUM))
(puthash 70 (lambda(_args _terminals) 'T_ENUM) phps-mode-parser--table-translations)

;; 71 ((semi_reserved) (reserved_non_modifiers))
(puthash 71 (lambda(_args _terminals) args) phps-mode-parser--table-translations)

;; 72 ((semi_reserved) (T_STATIC))
(puthash 72 (lambda(_args _terminals) 'T_STATIC) phps-mode-parser--table-translations)

;; 73 ((semi_reserved) (T_ABSTRACT))
(puthash 73 (lambda(_args _terminals) 'T_ABSTRACT) phps-mode-parser--table-translations)

;; 74 ((semi_reserved) (T_FINAL))
(puthash 74 (lambda(_args _terminals) 'T_FINAL) phps-mode-parser--table-translations)

;; 75 ((semi_reserved) (T_PRIVATE))
(puthash 75 (lambda(_args _terminals) 'T_PRIVATE) phps-mode-parser--table-translations)

;; 76 ((semi_reserved) (T_PROTECTED))
(puthash 76 (lambda(_args _terminals) 'T_PROTECTED) phps-mode-parser--table-translations)

;; 77 ((semi_reserved) (T_PUBLIC))
(puthash 77 (lambda(_args _terminals) 'T_PUBLIC) phps-mode-parser--table-translations)

;; 78 ((semi_reserved) (T_READONLY))
(puthash 78 (lambda(_args _terminals) 'T_READONLY) phps-mode-parser--table-translations)

;; 79 ((ampersand) (T_AMPERSAND_FOLLOWED_BY_VAR_OR_VARARG))
(puthash 79 (lambda(_args _terminals) 'T_AMPERSAND_FOLLOWED_BY_VAR_OR_VARARG) phps-mode-parser--table-translations)

;; 80 ((ampersand) (T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG))
(puthash 80 (lambda(_args _terminals) 'T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG) phps-mode-parser--table-translations)

;; 81 ((identifier) (T_STRING))
(puthash 81 (lambda(_args terminals) terminals) phps-mode-parser--table-translations)

;; 82 ((identifier) (semi_reserved))
(puthash 82 (lambda(_args terminals) terminals) phps-mode-parser--table-translations)

;; 83 ((top_statement_list) (top_statement_list top_statement))
(puthash
 83
 (lambda(args _terminals) (if (car args) (append (car args) (cdr args)) (cdr args)))
 phps-mode-parser--table-translations)

;; 84 ((top_statement_list) (%empty))
(puthash 84 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 111 top_statement -> (T_NAMESPACE namespace_declaration_name ";")
(puthash
 111
 (lambda(args terminals)
   `(
     ast-type
     namespace
     name
     ,(nth 1 args)
     index
     ,(car (cdr (nth 1 terminals)))
     start
     ,(car (cdr (nth 2 terminals)))
     end
     max
     ))
 phps-mode-parser--table-translations)

;; 112 top_statement -> (T_NAMESPACE namespace_declaration_name "{" top_statement_list "}")
(puthash
 112
 (lambda(args terminals)
   `(
     ast-type
     namespace
     name
     ,(nth 1 args)
     index
     ,(car (cdr (nth 1 terminals)))
     start
     ,(car (cdr (nth 2 terminals)))
     end
     ,(car (cdr (nth 4 terminals)))
     children
     ,(nth 3 args)
     ))
 phps-mode-parser--table-translations)

;; top_statement -> (T_NAMESPACE "{" top_statement_list "}")
(puthash
 113
 (lambda(args _terminals)
   (nth 2 args))
 phps-mode-parser--table-translations)

;; inner_statement_list -> (inner_statement_list inner_statement)
(puthash
 139
 (lambda(args _terminals)
   (if (car args)
       (append (car args) (cdr args))
     (cdr args)))
 phps-mode-parser--table-translations)

;; statement -> ("{" inner_statement_list "}")
(puthash
 145
 (lambda(args _terminals)
   (nth 1 args))
 phps-mode-parser--table-translations)

;; statement -> (T_WHILE "(" expr ")" while_statement)
(puthash
 148
 (lambda(args _terminals)
   `(
     ast-type
     while
     condition
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 4 args)))
   )
 phps-mode-parser--table-translations)

;; statement -> (T_DO statement T_WHILE "(" expr ")" ";")
(puthash
 149
 (lambda(args _terminals)
   `(
     ast-type
     do-while
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 1 args))
     condition
     ,(phps-mode-parser-sdt--get-list-of-object (nth 4 args))
     ))
 phps-mode-parser--table-translations)

;; statement -> (T_FOR "(" for_exprs ";" for_exprs ";" for_exprs ")" for_statement)
(puthash
 150
 (lambda(args _terminals)
   `(
     ast-type
     for
     initial
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     test
     ,(phps-mode-parser-sdt--get-list-of-object (nth 4 args))
     incremental
     ,(phps-mode-parser-sdt--get-list-of-object (nth 6 args))
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 8 args))
     ))
 phps-mode-parser--table-translations)

;; statement -> (T_RETURN optional_expr ";")
(puthash
 154
 (lambda(args _terminals)
   `(
     ast-type
     return-statement
     optional-expr
     ,(phps-mode-parser-sdt--get-list-of-object (nth 1 args))
     ))
 phps-mode-parser--table-translations)

;; statement -> (T_GLOBAL global_var_list ";")
(puthash
 155
 (lambda(args _terminals)
   `(
     ast-type
     global-statement
     global-var-list
     ,(phps-mode-parser-sdt--get-list-of-object (nth 1 args))
     ))
 phps-mode-parser--table-translations)

;; statement -> (T_STATIC static_var_list ";")
(puthash
 156
 (lambda(args _terminals)
   `(
     ast-type
     static-variables-statement
     static-var-list
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; statement -> (T_ECHO echo_expr_list ";")
(puthash
 157
 (lambda(args _terminals)
   `(
     ast-type
     echo-statement
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 1 args))
     ))
 phps-mode-parser--table-translations)

;; statement -> (expr ";")
(puthash
 159
 (lambda(args _terminals)
   (nth 0 args))
 phps-mode-parser--table-translations)

;; statement -> (T_FOREACH "(" expr T_AS foreach_variable ")" foreach_statement)
(puthash
 161
 (lambda(args _terminals)
   `(
     ast-type
     foreach
     expression
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     value
     ,(nth 4 args)
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 6 args))
     ))
 phps-mode-parser--table-translations)

;; statement -> (T_FOREACH "(" expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable ")" foreach_statement)
(puthash
 162
 (lambda(args _terminals)
   `(
     ast-type
     foreach
     expression
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     key
     ,(nth 4 args)
     value
     ,(nth 6 args)
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 8 args))
     ))
 phps-mode-parser--table-translations)

;; statement -> (T_TRY "{" inner_statement_list "}" catch_list finally_statement)
(puthash
 165
 (lambda(args _terminals)
   `(
     ast-type
     try
     inner-statement-list
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     catch-list
     ,(phps-mode-parser-sdt--get-list-of-object (nth 4 args))
     finally-statement
     ,(nth 5 args)
     ))
 phps-mode-parser--table-translations)

;; catch_list -> (catch_list T_CATCH "(" catch_name_list optional_variable ")" "{" inner_statement_list "}")
(puthash
 169
 (lambda(args terminals)
   `(
     ast-type
     catch
     catch-name-list
     ,(phps-mode-parser-sdt--get-list-of-object (nth 3 args))
     optional-variable
     ,(nth 4 args)
     optional-variable-start
     ,(car (cdr (nth 4 terminals)))
     optional-variable-end
     ,(cdr (cdr (nth 4 terminals)))
     children
     ,(nth 7 args)
     ))
 phps-mode-parser--table-translations)

;; function_declaration_statement -> (function returns_ref T_STRING backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags)
(puthash
 179
 (lambda(args terminals)
   `(
     ast-type
     function
     name
     ,(nth 2 args)
     index
     ,(car (cdr (nth 2 terminals)))
     start
     ,(car (cdr (nth 9 terminals)))
     end
     ,(car (cdr (nth 11 terminals)))
     returns-reference-p
     ,(not (equal (nth 1 args) nil))
     parameter-list
     ,(nth 5 args)
     return-type
     ,(nth 7 args)
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 10 args))
     ))
 phps-mode-parser--table-translations)

;; class_declaration_statement -> (class_modifiers T_CLASS T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}")
(puthash
 184
 (lambda(args terminals)
   `(
     ast-type
     class
     modifiers
     ,(nth 0 args)
     name
     ,(nth 2 args)
     extends
     ,(phps-mode-parser-sdt--get-list-of-object (nth 3 args))
     implements
     ,(phps-mode-parser-sdt--get-list-of-object (nth 4 args))
     index
     ,(car (cdr (nth 2 terminals)))
     start
     ,(car (cdr (nth 6 terminals)))
     end
     ,(car (cdr (nth 8 terminals)))
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 7 args))
     ))
 phps-mode-parser--table-translations)

;; class_declaration_statement -> (T_CLASS T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}")
(puthash
 185
 (lambda(args terminals)
   `(
     ast-type
     class
     name
     ,(nth 1 args)
     extends
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     implements
     ,(phps-mode-parser-sdt--get-list-of-object (nth 3 args))
     index
     ,(car (cdr (nth 1 terminals)))
     start
     ,(car (cdr (nth 5 terminals)))
     end
     ,(car (cdr (nth 7 terminals)))
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 6 args))
     ))
 phps-mode-parser--table-translations)

;; interface_declaration_statement -> (T_INTERFACE T_STRING interface_extends_list backup_doc_comment "{" class_statement_list "}")
(puthash
 191
 (lambda(args terminals)
   `(
     ast-type
     interface
     name
     ,(nth 1 args)
     extends
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     index
     ,(car (cdr (nth 1 terminals)))
     start
     ,(car (cdr (nth 4 terminals)))
     end
     ,(car (cdr (nth 6 terminals)))
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 5 args))
     ))
 phps-mode-parser--table-translations)

;; extends_from -> (%empty)
(puthash
 198
 (lambda(_args _terminals)
   nil
   )
 phps-mode-parser--table-translations)

;; extends_from -> (T_EXTENDS class_name)
(puthash
 199
 (lambda(args _terminals)
   (nth 1 args))
 phps-mode-parser--table-translations)

;; foreach_variable -> (ampersand variable)
(puthash
 205
 (lambda(args _terminals)
   `(
     ast-type
     foreach-referenced-variable
     variable
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; if_stmt_without_else -> (T_IF "(" expr ")" statement)
(puthash
 234
 (lambda(args _terminals)
   `(
     ast-type
     if
     condition
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     children
     ,(phps-mode-parser-sdt--get-list-of-object (nth 4 args))
     ))
 phps-mode-parser--table-translations)

;; parameter_list -> (non_empty_parameter_list possible_comma)
(puthash
 242
 (lambda(args _terminals)
   (nth 0 args))
 phps-mode-parser--table-translations)

;; non_empty_parameter_list -> (attributed_parameter)
(puthash
 244
 (lambda(args _terminals)
   (list args))
 phps-mode-parser--table-translations)

;; non_empty_parameter_list -> (non_empty_parameter_list "," attributed_parameter)
(puthash
 245
 (lambda(args _terminals)
   (append (nth 0 args) (list (nth 2 args))))
 phps-mode-parser--table-translations)

;; parameter -> (optional_property_modifiers optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment)
(puthash
 254
 (lambda(args terminals)
   `(
     ast-type
     parameter
     visibility
     ,(nth 0 args)
     type
     ,(nth 1 args)
     is-reference
     ,(nth 2 args)
     is-variadic
     ,(nth 3 args)
     name
     ,(nth 4 args)
     start
     ,(car (cdr (nth 4 terminals)))
     end
     ,(cdr (cdr (nth 4 terminals)))
     ))
 phps-mode-parser--table-translations)

;; parameter -> (optional_property_modifiers optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment "=" expr)
(puthash
 255
 (lambda(args terminals)
   `(
     ast-type
     parameter-with-default-value
     visibility
     ,(nth 0 args)
     type
     ,(nth 1 args)
     is-reference
     ,(nth 2 args)
     is-variadic
     ,(nth 3 args)
     name
     ,(nth 4 args)
     start
     ,(car (cdr (nth 4 terminals)))
     end
     ,(cdr (cdr (nth 4 terminals)))
     backup-doc-comment
     ,(nth 5 args)
     default-value
     ,(phps-mode-parser-sdt--get-list-of-object (nth 7 args))
     ))
 phps-mode-parser--table-translations)

;; argument_list -> ("(" ")")
(puthash
 281
 (lambda(_args _terminals)
   nil)
 phps-mode-parser--table-translations)

;; argument_list -> ("(" non_empty_argument_list possible_comma ")")
(puthash
 282
 (lambda(args _terminals)
   (nth 1 args))
 phps-mode-parser--table-translations)

;; static_var_list -> (static_var_list "," static_var)
(puthash
 292
 (lambda(args _terminals)
   (append (nth 0 args) (list (nth 2 args))))
 phps-mode-parser--table-translations)

;; static_var_list -> (static_var)
(puthash
 293
 (lambda(args _terminals)
   (list args))
 phps-mode-parser--table-translations)

;; static_var -> (T_VARIABLE)
(puthash
 294
 (lambda(args terminals)
   `(
     ast-type
     variable
     name
     ,args
     index
     ,(car (cdr terminals))
     start
     ,(car (cdr terminals))
     end
     ,(cdr (cdr terminals))
     ))
 phps-mode-parser--table-translations)

;; static_var -> (T_VARIABLE "=" expr)
(puthash
 295
 (lambda(args terminals)
   `(
     ast-type
     variable
     name
     ,(nth 0 args)
     expr
     ,(nth 2 args)
     index
     ,(car (cdr (nth 0 terminals)))
     start
     ,(car (cdr (nth 0 terminals)))
     end
     ,(cdr (cdr (nth 0 terminals)))
     ))
 phps-mode-parser--table-translations)

;; class_statement_list -> (class_statement_list class_statement)
(puthash
 296
 (lambda(args _terminals)
   (if (car args)
         (append (car args) (cdr args))
       (cdr args)))
 phps-mode-parser--table-translations)

;; attributed_class_statement -> (variable_modifiers optional_type_without_static property_list ";")
(puthash
 298
 (lambda(args terminals)
   `(
     ast-type
     property
     modifiers
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     type
     ,(nth 1 args)
     subject
     ,(nth 2 args)
     start
     ,(car (cdr (car (nth 2 terminals))))
     end
     ,(cdr (cdr (car (nth 2 terminals))))
     ))
 phps-mode-parser--table-translations)

;; attributed_class_statement -> (method_modifiers function returns_ref identifier backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags method_body backup_fn_flags)
(puthash
 300
 (lambda(args terminals)
   `(
     ast-type
     method
     modifiers
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     returns-reference-p
     ,(not (equal (nth 2 args) nil))
     name
     ,(nth 3 args)
     parameter-list
     ,(nth 6 args)
     return-type
     ,(nth 8 args)
     children
     ,(if (nth 10 args)
          (phps-mode-parser-sdt--get-list-of-object (nth 10 args))
        nil)
     index
     ,(car (cdr (nth 3 terminals)))
     start
     ,(if (nth 10 args)
          (car (cdr (car (nth 10 terminals))))
        nil)
     end
     ,(if (nth 10 args)
          (cdr (cdr (car (cdr (cdr (nth 10 terminals))))))
        nil)
     ))
 phps-mode-parser--table-translations)

;; method_body -> (";")
(puthash
 322
 (lambda(_args _terminals)
   nil)
 phps-mode-parser--table-translations)

;; method_body -> ("{" inner_statement_list "}")
(puthash
 323
 (lambda(args _terminals)
   (nth 1 args))
 phps-mode-parser--table-translations)

;; variable_modifiers -> (T_VAR)
(puthash
 325
 (lambda(_args _terminals)
   'public)
 phps-mode-parser--table-translations)

;; member_modifier -> (T_PUBLIC)
(puthash
 330
 (lambda(_args _terminals)
   'public)
 phps-mode-parser--table-translations)

;; member_modifier -> (T_PROTECTED)
(puthash
 331
 (lambda(_args _terminals)
   'protected)
 phps-mode-parser--table-translations)

;; member_modifier -> (T_PRIVATE)
(puthash
 332
 (lambda(_args _terminals)
   'private)
 phps-mode-parser--table-translations)

;; member_modifier -> (T_STATIC)
(puthash
 333
 (lambda(_args _terminals)
   'static)
 phps-mode-parser--table-translations)

;; member_modifier -> (T_ABSTRACT)
(puthash
 334
 (lambda(_args _terminals)
   'abstract)
 phps-mode-parser--table-translations)

;; member_modifier -> (T_FINAL)
(puthash
 335
 (lambda(_args _terminals)
   'final)
 phps-mode-parser--table-translations)

;; property -> (T_VARIABLE backup_doc_comment)
(puthash
 339
 (lambda(args _terminals)
   (nth 0 args))
 phps-mode-parser--table-translations)

;; property -> (T_VARIABLE "=" expr backup_doc_comment)
(puthash
 340
 (lambda(args terminals)
   `(
     ast-type
     assign-property-variable
     key
     ,(nth 0 args)
     value
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     index
     ,(car (cdr (nth 0 terminals)))
     start
     ,(car (cdr (nth 0 terminals)))
     end
     ,(cdr (cdr (nth 0 terminals)))
     ))
 phps-mode-parser--table-translations)

;; expr -> ("[" array_pair_list "]" "=" expr)
(puthash
 358
 (lambda(args _terminals)
   `(
     ast-type
     assign-variables-from-array
     keys
     ,(nth 1 args)
     values
     ,(phps-mode-parser-sdt--get-list-of-object (nth 4 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (variable "=" expr)
(puthash
 359
 (lambda(args terminals)
   `(
     ast-type
     assign-variable
     key
     ,(nth 0 args)
     value
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     index
     ,(car (cdr (nth 0 terminals)))
     start
     ,(car (cdr (nth 0 terminals)))
     end
     ,(cdr (cdr (nth 0 terminals)))
     ))
 phps-mode-parser--table-translations)

;; expr -> (variable T_INC)
(puthash
 375
 (lambda(args _terminals)
   `(
     ast-type
     increment-variable
     variable
     ,(nth 0 args)
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr T_BOOLEAN_OR expr)
(puthash
 379
 (lambda(args _terminals)
   `(
     ast-type
     boolean-or-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr T_BOOLEAN_AND expr)
(puthash
 380
 (lambda(args _terminals)
   `(
     ast-type
     boolean-and-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr T_LOGICAL_OR expr)
(puthash
 381
 (lambda(args _terminals)
   `(
     ast-type
     logical-or-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr T_LOGICAL_AND expr)
(puthash
 382
 (lambda(args _terminals)
   `(
     ast-type
     logical-and-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr T_LOGICAL_XOR expr)
(puthash
 383
 (lambda(args _terminals)
   `(
     ast-type
     logical-xor-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr "|" expr)
(puthash
 384
 (lambda(args _terminals)
   `(
     ast-type
     bitwise-or-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG expr)
(puthash
 385
 (lambda(args _terminals)
   `(
     ast-type
     bitwise-and-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr T_AMPERSAND_FOLLOWED_BY_VAR_OR_VARARG expr)
(puthash
 386
 (lambda(args _terminals)
   `(
     ast-type
     bitwise-and-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr "^" expr)
(puthash
 387
 (lambda(args _terminals)
   `(
     ast-type
     bitwise-xor-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr "." expr)
(puthash
 388
 (lambda(args _terminals)
   `(
     ast-type
     concat-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr "+" expr)
(puthash
 389
 (lambda(args _terminals)
   `(
     ast-type
     addition-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr "-" expr)
(puthash
 390
 (lambda(args _terminals)
   `(
     ast-type
     subtraction-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr "*" expr)
(puthash
 391
 (lambda(args _terminals)
   `(
     ast-type
     multiplication-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr T_POW expr)
(puthash
 392
 (lambda(args _terminals)
   `(
     ast-type
     exponentiation-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr "/" expr)
(puthash
 393
 (lambda(args _terminals)
   `(
     ast-type
     division-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr "%" expr)
(puthash
 394
 (lambda(args _terminals)
   `(
     ast-type
     modulo-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr T_SL expr)
(puthash
 395
 (lambda(args _terminals)
   `(
     ast-type
     bitwise-shift-left-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> (expr T_SR expr)
(puthash
 396
 (lambda(args _terminals)
   `(
     ast-type
     bitwise-shift-right-expression
     a
     ,(phps-mode-parser-sdt--get-list-of-object (nth 0 args))
     b
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; expr -> ("!" expr)
(puthash
 399
 (lambda(args _terminals)
   `(
     ast-type
     negated-expression
     expression
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; expr -> (T_STATIC inline_function)
(puthash
 436
 (lambda(args _terminals)
   `(
     'ast-type
     'static-inline-function
     'inline-function
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; inline_function -> (function returns_ref backup_doc_comment "(" parameter_list ")" lexical_vars return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags)
(puthash
 439
 (lambda(args terminals)
   `(
     ast-type
     inline-function
     start
     ,(car (cdr (nth 9 terminals)))
     end
     ,(cdr (cdr (nth 11 terminals)))
     returns-ref
     ,(nth 1 args)
     backup-doc-comment
     ,(nth 2 args)
     parameter-list
     ,(nth 4 args)
     lexical-vars
     ,(phps-mode-parser-sdt--get-list-of-object (nth 6 args))
     return-type
     ,(nth 7 args)
     backup-fn-flags-1
     ,(nth 8 args)
     inner-statement-list
     ,(phps-mode-parser-sdt--get-list-of-object (nth 10 args))
     backup-fn-flags-2
     ,(nth 12 args)
     ))
 phps-mode-parser--table-translations)

;; inline_function -> (fn returns_ref backup_doc_comment "(" parameter_list ")" return_type T_DOUBLE_ARROW backup_fn_flags backup_lex_pos expr backup_fn_flags)
(puthash
 440
 (lambda(args terminals)
   `(
     ast-type
     arrow-function
     start
     ,(car (cdr (nth 9 terminals)))
     end
     ,(cdr (cdr (nth 11 terminals)))
     returns-ref
     ,(nth 1 args)
     backup-doc-comment
     ,(nth 2 args)
     parameter-list
     ,(nth 4 args)
     return-type
     ,(nth 6 args)
     backup-fn-flags-1
     ,(nth 8 args)
     backup-lex-pos
     ,(nth 9 args)
     expr
     ,(phps-mode-parser-sdt--get-list-of-object (nth 10 args))
     backup-fn-flags-2
     ,(nth 11 args)
     ))
 phps-mode-parser--table-translations)

;; lexical_vars -> (T_USE "(" lexical_var_list possible_comma ")")
(puthash
 449
 (lambda(args _terminals)
   (nth 2 args))
 phps-mode-parser--table-translations)

;; lexical_var -> (T_VARIABLE)
(puthash
 452
 (lambda(args terminals)
   `(
     ast-type
     lexical-var
     name
     ,args
     start
     ,(car (cdr terminals))
     end
     ,(cdr (cdr terminals))
     ))
 phps-mode-parser--table-translations)

;; function_call -> (name argument_list)
(puthash
 454
 (lambda(args _terminals)
   `(
     ast-type
     function-call
     name
     ,(nth 0 args)
     argument-list
     ,(phps-mode-parser-sdt--get-list-of-object (nth 1 args))
     ))
 phps-mode-parser--table-translations)

;; dereferencable_scalar -> (T_ARRAY "(" array_pair_list ")")
(puthash
 470
 (lambda(args _terminals)
   `(
     ast-type
     dereferencable-scalar
     array-pair-list
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; dereferencable_scalar -> (T_CONSTANT_ENCAPSED_STRING)
(puthash
 472
 (lambda(args _terminals)
   (substring args 1 -1))
 phps-mode-parser--table-translations)

;; scalar -> (T_LNUMBER)
(puthash
 474
 (lambda(args _terminals)
   (string-to-number args))
 phps-mode-parser--table-translations)

;; scalar -> (T_DNUMBER)
(puthash
 475
 (lambda(args _terminals)
   (string-to-number args))
 phps-mode-parser--table-translations)

;; callable_variable -> (array_object_dereferenceable "[" optional_expr "]")
(puthash
 506
 (lambda(args _terminals)
   `(
     ast-type
     callable-variable
     array-object-dereferencable
     array-index
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; callable_variable -> (array_object_dereferenceable "{" expr "}")
(puthash
 507
 (lambda(args _terminals)
   `(
     ast-type
     callable-variable
     array-object-dereferencable
     expr
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; callable_variable -> (array_object_dereferencable T_OBJECT_OPERATOR property_name argument_list)
(puthash
 508
 (lambda(args _terminals)
   `(
     ast-type
     callable-variable
     array-object-dereferencable
     ,(nth 0 args)
     operator
     object-operator
     property-name
     ,(nth 2 args)
     argument-list
     ,(phps-mode-parser-sdt--get-list-of-object (nth 3 args))
     ))
 phps-mode-parser--table-translations)

;; callable_variable -> (array_object_dereferenceable T_NULLSAFE_OBJECT_OPERATOR property_name argument_list)
(puthash
 509
 (lambda(args _terminals)
   `(
     ast-type
     callable-variable
     array-object-dereferencable
     ,(nth 0 args)
     operator
     nullsafe-object-operator
     property-name
     ,(nth 2 args)
     argument-list
     ,(phps-mode-parser-sdt--get-list-of-object (nth 3 args))
     ))
 phps-mode-parser--table-translations)

;; variable -> (array_object_dereferencable T_OBJECT_OPERATOR property_name)
(puthash
 513
 (lambda(args terminals)
   `(
     ast-type
     variable
     array-object-dereferencable
     ,(nth 0 args)
     operator
     object-operator
     property-name
     ,(nth 2 args)
     property-start
     ,(car (cdr (nth 2 terminals)))
     property-end
     ,(cdr (cdr (nth 2 terminals)))
     ))
 phps-mode-parser--table-translations)

;; variable -> (array_object_dereferenceable T_NULLSAFE_OBJECT_OPERATOR property_name)
(puthash
 514
 (lambda(args terminals)
   `(
     ast-type
     variable
     array-object-dereferencable
     ,(nth 0 args)
     operator
     nullsafe-object-operator
     property-name
     ,(nth 2 args)
     property-start
     ,(car (cdr (nth 2 terminals)))
     property-end
     ,(cdr (cdr (nth 2 terminals)))
     ))
 phps-mode-parser--table-translations)

;; simple_variable -> (T_VARIABLE)
(puthash
 515
 (lambda(args terminals)
   `(
     ast-type
     simple-variable
     name
     ,args
     start
     ,(car (cdr terminals))
     end
     ,(cdr (cdr terminals))
     ))
 phps-mode-parser--table-translations)

;; static_member -> (class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
(puthash
 518
 (lambda(args terminals)
   `(
     ast-type
     static-member
     class
     ,(nth 0 args)
     member
     ,(nth 2 args)
     start
     ,(car (cdr (nth 0 terminals)))
     end
     ,(cdr (cdr (nth 0 terminals)))
     ))
 phps-mode-parser--table-translations)

;; static_member -> (variable_class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
(puthash
 519
 (lambda(args terminals)
   `(
     ast-type
     static-member
     variable-class-name
     ,(nth 0 args)
     member
     ,(nth 2 args)
     start
     ,(car (cdr (nth 0 terminals)))
     end
     ,(cdr (cdr (nth 0 terminals)))
     ))
 phps-mode-parser--table-translations)

;; non_empty_array_pair_list -> (non_empty_array_pair_list "," possible_array_pair)
(puthash
 536
 (lambda(args _terminals)
   (if (nth 2 args)
       (append (nth 0 args) (list (nth 2 args)))
     (nth 0 args)))
 phps-mode-parser--table-translations)

;; non_empty_array_pair_list -> (possible_array_pair)
(puthash
 537
 (lambda(args _terminals)
   (list args))
 phps-mode-parser--table-translations)

;; internal_functions_in_yacc -> (T_ISSET "(" isset_variables possible_comma ")")
(puthash
 561
 (lambda(args _terminals)
   `(
     ast-type
     isset-variables
     variables
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; internal_functions_in_yacc -> (T_EMPTY "(" expr ")")
(puthash
 562
 (lambda(args _terminals)
   `(
     ast-type
     empty-expression
     variables
     ,(phps-mode-parser-sdt--get-list-of-object (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; isset_variables -> (isset_variable)
(puthash
 568
 (lambda(args _terminals)
   args)
 phps-mode-parser--table-translations)

;; isset_variables -> (isset_variables "," isset_variable)
(puthash
 569
 (lambda(args _terminals)
   (append (nth 0 args) (nth 2 args)))
 phps-mode-parser--table-translations)

;; isset_variable -> (expr)
(puthash
 570
 (lambda(args _terminals)
   (list args))
 phps-mode-parser--table-translations)

(provide 'phps-mode-parser-sdt)
;;; phps-mode-parser-sdt.el ends here
