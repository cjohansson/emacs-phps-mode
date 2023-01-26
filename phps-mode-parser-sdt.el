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


(require 'phps-mode-parser)

(defvar-local
  phps-mode-parser-sdt-bookkeeping
  (make-hash-table :test 'equal)
  "Bookkeeping of symbol references.")

(defvar-local
  phps-mode-parser-sdt-symbol-table-index
  0
  "Symbol table index.")

(defvar-local
  phps-mode-parser-sdt-symbol-table
  (make-hash-table :test 'equal)
  "Symbol table of parse, symbol ID => (list URI start end)")

(defvar-local
  phps-mode-parser-sdt-symbol-table-by-uri
  (make-hash-table :test 'equal)
  "Symbol table of parse, symbol URI => list of symbol IDs.")

(defvar-local
  phps-mode-parser-sdt-symbol-imenu
  nil
  "Imenu for symbols of parse.")

(defvar-local
  phps-mode-parser-sdt-symbol-imenu--table
  nil
  "Symbols inside namespaces.")

(defvar-local
  phps-mode-parser-sdt-symbol-imenu--stack
  nil
  "Current imenu namespace.")

(defvar-local
  phps-mode-parser-sdt-symbol-imenu--namespace
  nil
  "Current imenu namespace.")

(defvar-local
  phps-mode-parser-sdt--bookkeeping-namespace
  nil
  "Current bookkeeping namespace.")

(defvar-local
  phps-mode-parser-sdt--bookkeeping-symbol-stack
  nil
  "Current bookkeeping symbol stack.")

(defvar-local
  phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack
  nil
  "Current bookkeeping assignment symbol stack.")

(defvar-local
  phps-mode-parser-sdt--bookkeeping-anonymous-function-count
  nil
  "Count of anonymous functions.")

(defvar-local
  phps-mode-parser-sdt--bookkeeping-arrow-function-count
  nil
  "Count of arrow functions.")

(defvar
  phps-mode-parser-sdt--bookkeeping--superglobal-variable-p
  #s(hash-table size 12 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("$_GET" 1 "$_POST" 1 "$_COOKIE" 1 "$_SESSION" 1 "$_REQUEST" 1 "$GLOBALS" 1 "$_SERVER" 1 "$_FILES" 1 "$_ENV" 1 "$argc" 1 "$argv" 1 "$http_​response_​header" 1))
  "Hash-table of super-global variables.")

(defun phps-mode-parser-sdt--get-symbol-uri (name scope)
  "Get URI from symbol NAME in SCOPE."
  (if (gethash
       name
       phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
      name
    (let ((potential-uris (list ""))
          (scope-namespace)
          (scope-class)
          (scope-trait)
          (scope-interface)
          (scope-function))
      (when scope
        (let ((scope-count (length scope))
              (scope-index 0))
          (while (< scope-index scope-count)
            (let ((item (nth scope-index scope))
                  (next-scope)
                  (next-scope-type)
                  (next-scope-is-global)
                  (next-scope-is-this-object-operator)
                  (next-scope-is-self-static-member-operator))

              ;; Should add one scope look-ahead to determine if we should
              ;; ignore function scope before a $this-> or self:: or static:: operator
              (when (< scope-index (1- scope-count))
                (let ((next-scope-index (1+ scope-index)))
                  (setq next-scope (nth next-scope-index scope))
                  (setq next-scope-type (car next-scope))

                  ;; When next scope is arrow function, ignore it
                  (while (and
                          (equal next-scope-type 'arrow-function)
                          (< next-scope-index (1- scope-count)))
                    (setq next-scope-index (1+ next-scope-index))
                    (setq next-scope (nth next-scope-index scope))
                    (setq next-scope-type (car next-scope)))

                  (cond
                   ((equal next-scope-type 'global)
                    (setq next-scope-is-global t))
                   ((equal next-scope-type 'object-operator)
                    (let ((downcased-scope-name (downcase (car (cdr next-scope)))))
                      (when (string= downcased-scope-name "$this")
                        (setq next-scope-is-this-object-operator t))))
                   ((equal next-scope-type 'static-member)
                    (let ((downcased-scope-name (downcase (car (cdr next-scope)))))
                      (when (or
                             (string= downcased-scope-name "self")
                             (string= downcased-scope-name "static"))
                        (when (equal next-scope-index (1+ scope-index))
                          (let ((potential-uri-count (length potential-uris))
                                (potential-uri-index 0))
                            (while (< potential-uri-index potential-uri-count)
                              (setf
                               (nth potential-uri-index potential-uris)
                               (format "static %s" (nth potential-uri-index potential-uris)))
                              (setq potential-uri-index (1+ potential-uri-index)))))
                        (setq next-scope-is-self-static-member-operator t)))))))

              (let ((space-type (nth 0 item))
                    (space-name (nth 1 item)))
                (cond

                 ((equal space-type 'namespace)
                  (unless next-scope-is-global
                    (setq scope-namespace (list space-name (nth 2 item)))
                    (let ((potential-uri-count (length potential-uris))
                          (potential-uri-index 0))
                      (while (< potential-uri-index potential-uri-count)
                        (setf
                         (nth potential-uri-index potential-uris)
                         (format
                          "namespace %s %s"
                          space-name
                          (nth potential-uri-index potential-uris)))
                        (setq potential-uri-index (1+ potential-uri-index))))))

                 ((equal space-type 'class)
                  (unless next-scope-is-global
                    (setq scope-class (list space-name (nth 2 item)))
                    (let ((potential-uri-count (length potential-uris))
                          (potential-uri-index 0))
                      (while (< potential-uri-index potential-uri-count)
                        (setf
                         (nth potential-uri-index potential-uris)
                         (format
                          "class %s %s"
                          space-name
                          (nth potential-uri-index potential-uris)))
                        (setq potential-uri-index (1+ potential-uri-index))))))

                 ((equal space-type 'interface)
                  (unless next-scope-is-global
                    (setq scope-interface (list space-name (nth 2 item)))
                    (let ((potential-uri-count (length potential-uris))
                          (potential-uri-index 0))
                      (while (< potential-uri-index potential-uri-count)
                        (setf
                         (nth potential-uri-index potential-uris)
                         (format
                          "interface %s %s"
                          space-name
                          (nth potential-uri-index potential-uris)))
                        (setq potential-uri-index (1+ potential-uri-index))))))

                 ((equal space-type 'trait)
                  (unless next-scope-is-global
                    (setq scope-trait (list space-name (nth 2 item)))
                    (let ((potential-uri-count (length potential-uris))
                          (potential-uri-index 0))
                      (while (< potential-uri-index potential-uri-count)
                        (setf
                         (nth potential-uri-index potential-uris)
                         (format
                          "trait %s %s"
                          space-name
                          (nth potential-uri-index potential-uris)))
                        (setq potential-uri-index (1+ potential-uri-index))))))

                 ((equal space-type 'function)
                  (unless (or
                           next-scope-is-global
                           next-scope-is-this-object-operator
                           next-scope-is-self-static-member-operator)
                    (setq scope-function (list space-name (nth 2 item)))
                    (let ((potential-uri-count (length potential-uris))
                          (potential-uri-index 0))
                      (while (< potential-uri-index potential-uri-count)
                        (setf
                         (nth potential-uri-index potential-uris)
                         (format
                          "function %s %s"
                          space-name
                          (nth potential-uri-index potential-uris)))
                        (setq potential-uri-index (1+ potential-uri-index))))))

                 ((equal space-type 'anonymous-function)
                  (let ((potential-uri-count (length potential-uris))
                        (potential-uri-index 0))
                    (while (< potential-uri-index potential-uri-count)
                      (setf
                       (nth potential-uri-index potential-uris)
                       (format
                        "anonymous %s %s"
                        space-name
                        (nth potential-uri-index potential-uris)))
                      (setq potential-uri-index (1+ potential-uri-index)))))

                 ((equal space-type 'static)
                  (let ((potential-uri-count (length potential-uris))
                        (potential-uri-index 0))
                    (while (< potential-uri-index potential-uri-count)
                      (setf
                       (nth potential-uri-index potential-uris)
                       (format
                        "static %s"
                        (nth potential-uri-index potential-uris)))
                      (setq potential-uri-index (1+ potential-uri-index)))))

                 ((equal space-type 'arrow-function)
                  ;; branch of two alternative namespaces here
                  ;; one with and one without the arrow function scope
                  (let ((potential-uri-count (length potential-uris))
                        (potential-uri-index 0)
                        (new-potential-uris))
                    (while (< potential-uri-index potential-uri-count)
                      (push
                       (format
                        "arrow %s %s"
                        space-name
                        (nth potential-uri-index potential-uris))
                       new-potential-uris)
                      (setq potential-uri-index (1+ potential-uri-index)))
                    (setq potential-uris (append new-potential-uris potential-uris))
                    ;; (message "new-potential-uris: %S" new-potential-uris)
                    ;; (message "potential-uris: %S" potential-uris)
                    ))

                 )))
            (setq scope-index (1+ scope-index)))))

      (let ((potential-uri-count (length potential-uris))
            (potential-uri-index 0)
            (matching-uri))

        ;; Iterate potential-uris, select first match or if no match just return the first
        (while (and
                (< potential-uri-index potential-uri-count)
                (not matching-uri))
          (let ((potential-uri (nth potential-uri-index potential-uris)))
            (setq
             potential-uri
             (format
              "%sid %s"
              potential-uri
              name))
            (setf
             (nth potential-uri-index potential-uris)
             potential-uri)
            (when (gethash potential-uri phps-mode-parser-sdt-symbol-table-by-uri)
              (setq matching-uri potential-uri))
            (setq potential-uri-index (1+ potential-uri-index))))
        (if matching-uri
            (list
             matching-uri
             (list 'namespace scope-namespace 'class scope-class 'trait scope-trait 'interface scope-interface 'function scope-function))
          (list
           (nth 0 potential-uris)
           (list 'namespace scope-namespace 'class scope-class 'trait scope-trait 'interface scope-interface 'function scope-function)))))))

(defun phps-mode-parser-sdt--parse-top-statement ()
  "Parse latest top statement."
   ;; (message "phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack: %S" phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
  ;; (message "phps-mode-parser-sdt--bookkeeping-symbol-stack: %S" phps-mode-parser-sdt--bookkeeping-symbol-stack)

  ;; Add imenu class, trait, interface and functions here
  (when
      phps-mode-parser-sdt-symbol-imenu--namespace
    (message "phps-mode-parser-sdt-symbol-imenu--namespace: %S" phps-mode-parser-sdt-symbol-imenu--namespace)
    (let ((imenu-nail (format "namespace %s" phps-mode-parser-sdt-symbol-imenu--namespace)))
      (unless
          (gethash
           imenu-nail
           phps-mode-parser-sdt-symbol-imenu--table)
        (puthash
         imenu-nail
         (make-hash-table :test 'equal)
         phps-mode-parser-sdt-symbol-imenu--table))))

  (when phps-mode-parser-sdt-symbol-imenu--stack
    ;; Go through imenu stack and add new items to imenu index
    (message "phps-mode-parser-sdt-symbol-imenu--stack: %S" phps-mode-parser-sdt-symbol-imenu--stack)
    (let ((imenu-namespace)
          (imenu-class)
          (imenu-trait)
          (imenu-interface)
          (imenu-function))
      (dolist (imenu-scope phps-mode-parser-sdt-symbol-imenu--stack)
        (let ((imenu-item-type (nth 0 imenu-scope))
              (imenu-item-name (nth 1 imenu-scope))
              (imenu-item-start (nth 2 imenu-scope)))
          (cond
           ((equal imenu-item-type 'namespace)
            (setq imenu-namespace (list imenu-item-name imenu-item-start)))
           ((equal imenu-item-type 'class)
            (setq imenu-class (list imenu-item-name imenu-item-start)))
           ((equal imenu-item-type 'interface)
            (setq imenu-interface (list imenu-item-name imenu-item-start)))
           ((equal imenu-item-type 'trait)
            (setq imenu-trait (list imenu-item-name imenu-item-start)))
           ((equal imenu-item-type 'function)
            (setq imenu-function (list imenu-item-name imenu-item-start))))))
      (cond

       (imenu-namespace
        (let ((imenu-nail (format "namespace %s" (nth 0 imenu-namespace))))
          (unless (gethash imenu-nail phps-mode-parser-sdt-symbol-imenu--table)
            (let ((imenu-object (make-hash-table :test 'equal)))
              (puthash 'declaration (nth 1 imenu-namespace) imenu-object)
              (puthash imenu-nail imenu-object phps-mode-parser-sdt-symbol-imenu--table)))
          (cond
           (imenu-class
            (let ((imenu-nail2 (format "class %s" (nth 0 imenu-class))))
              (unless
                  (gethash
                   imenu-nail2
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))
                (let ((imenu-object (make-hash-table :test 'equal)))
                  (puthash 'declaration (nth 1 imenu-class) imenu-object)
                  (puthash
                   imenu-nail2
                   imenu-object
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))))
              (when imenu-function
                (let ((imenu-nail3 (format "function %s" (nth 0 imenu-function))))
                  (unless
                      (gethash
                       imenu-nail3
                       (gethash
                        imenu-nail2
                        (gethash
                         imenu-nail
                         phps-mode-parser-sdt-symbol-imenu--table))))
                  (let ((imenu-object (make-hash-table :test 'equal)))
                    (puthash 'declaration (nth imenu-function))
                    (puthash
                     imenu-nail3
                     imenu-object
                     (gethash
                      imenu-nail2
                      (gethash
                       imenu-nail
                       phps-mode-parser-sdt-symbol-imenu--table))))))))

           (imenu-trait
            (let ((imenu-nail2 (format "trait %s" (nth 0 imenu-trait))))
              (unless
                  (gethash
                   imenu-nail2
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))
                (let ((imenu-object (make-hash-table :test 'equal)))
                  (puthash 'declaration (nth 1 imenu-class) imenu-object)
                  (puthash
                   imenu-nail2
                   imenu-object
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))))
              (when imenu-function
                (let ((imenu-nail3 (format "function %s" (nth 0 imenu-function))))
                  (unless
                      (gethash
                       imenu-nail3
                       (gethash
                        imenu-nail2
                        (gethash
                         imenu-nail
                         phps-mode-parser-sdt-symbol-imenu--table))))
                  (let ((imenu-object (make-hash-table :test 'equal)))
                    (puthash 'declaration (nth imenu-function))
                    (puthash
                     imenu-nail3
                     imenu-object
                     (gethash
                      imenu-nail2
                      (gethash
                       imenu-nail
                       phps-mode-parser-sdt-symbol-imenu--table))))))))

           (imenu-interface
            (let ((imenu-nail2 (format "interface %s" (nth 0 imenu-interface))))
              (unless
                  (gethash
                   imenu-nail2
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))
                (let ((imenu-object (make-hash-table :test 'equal)))
                  (puthash 'declaration (nth 1 imenu-class) imenu-object)
                  (puthash
                   imenu-nail2
                   imenu-object
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))))
              (when imenu-function
                (let ((imenu-nail3 (format "function %s" (nth 0 imenu-function))))
                  (unless
                      (gethash
                       imenu-nail3
                       (gethash
                        imenu-nail2
                        (gethash
                         imenu-nail
                         phps-mode-parser-sdt-symbol-imenu--table))))
                  (let ((imenu-object (make-hash-table :test 'equal)))
                    (puthash 'declaration (nth imenu-function))
                    (puthash
                     imenu-nail3
                     imenu-object
                     (gethash
                      imenu-nail2
                      (gethash
                       imenu-nail
                       phps-mode-parser-sdt-symbol-imenu--table)))))))))))

       (imenu-class
        (let ((imenu-nail (format "class %s" (nth 0 imenu-class))))
          (unless
              (gethash
               imenu-nail
               phps-mode-parser-sdt-symbol-imenu--table)
            (let ((imenu-object (make-hash-table :test 'equal)))
              (puthash 'declaration (nth 1 imenu-class) imenu-object)
              (puthash
               imenu-nail
               imenu-object
               phps-mode-parser-sdt-symbol-imenu--table)
              (when imenu-function
                (let ((imenu-nail2 (format "function %s" (nth 0 imenu-function))))
                  (unless
                      (gethash
                       imenu-nail2
                       (gethash
                        imenu-nail
                        phps-mode-parser-sdt-symbol-imenu--table))))
                (let ((imenu-object (make-hash-table :test 'equal)))
                  (puthash 'declaration (nth imenu-function))
                  (puthash
                   imenu-nail2
                   imenu-object
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))))))))

       (imenu-trait
        (let ((imenu-nail (format "trait %s" (nth 0 imenu-trait))))
          (unless
              (gethash
               imenu-nail
               phps-mode-parser-sdt-symbol-imenu--table)
            (let ((imenu-object (make-hash-table :test 'equal)))
              (puthash 'declaration (nth 1 imenu-class) imenu-object)
              (puthash
               imenu-nail
               imenu-object
               phps-mode-parser-sdt-symbol-imenu--table)
              (when imenu-function
                (let ((imenu-nail2 (format "function %s" (nth 0 imenu-function))))
                  (unless
                      (gethash
                       imenu-nail2
                       (gethash
                        imenu-nail
                        phps-mode-parser-sdt-symbol-imenu--table))))
                (let ((imenu-object (make-hash-table :test 'equal)))
                  (puthash 'declaration (nth imenu-function))
                  (puthash
                   imenu-nail2
                   imenu-object
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))))))))

       (imenu-interface
        (let ((imenu-nail (format "interface %s" (nth 0 imenu-interface))))
          (unless
              (gethash
               imenu-nail
               phps-mode-parser-sdt-symbol-imenu--table)
            (let ((imenu-object (make-hash-table :test 'equal)))
              (puthash 'declaration (nth 1 imenu-class) imenu-object)
              (puthash
               imenu-nail
               imenu-object
               phps-mode-parser-sdt-symbol-imenu--table)
              (when imenu-function
                (let ((imenu-nail2 (format "function %s" (nth 0 imenu-function))))
                  (unless
                      (gethash
                       imenu-nail2
                       (gethash
                        imenu-nail
                        phps-mode-parser-sdt-symbol-imenu--table))))
                (let ((imenu-object (make-hash-table :test 'equal)))
                  (puthash 'declaration (nth imenu-function))
                  (puthash
                   imenu-nail2
                   imenu-object
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))))))))

       (imenu-function
        (let ((imenu-nail (format "function %s" (nth 0 imenu-function))))
          (unless
              (gethash
               imenu-nail
               phps-mode-parser-sdt-symbol-imenu--table)
            (let ((imenu-object (make-hash-table :test 'equal)))
              (puthash 'declaration (nth 1 imenu-function) imenu-object)
              (puthash
               imenu-nail
               imenu-object
               phps-mode-parser-sdt-symbol-imenu--table)))))))
    (setq phps-mode-parser-sdt-symbol-imenu--stack nil))

  ;; Parse bookkeeping writes and reads at every statement terminus
  (when phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack
    ;; Declare variables
    (dolist (
             symbol-list
             (reverse phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack))
      (let* ((symbol-name (car symbol-list))
             (symbol-scope (car (cdr symbol-list)))
             (symbol-start (car (cdr (cdr symbol-list))))
             (symbol-end (car (cdr (cdr (cdr symbol-list)))))
             (symbol-uri-object
              (phps-mode-parser-sdt--get-symbol-uri
               symbol-name
               symbol-scope))
             (symbol-uri (car symbol-uri-object))
             (symbol-namespace)
             (symbol-class)
             (symbol-interface)
             (symbol-trait)
             (symbol-function))

        ;; Collect namespace, class, interface, trait and function here
        (when (nth 1 (nth 1 symbol-uri-object))
          (setq symbol-namespace (car (nth 1 (nth 1 symbol-uri-object)))))
        (when (nth 3 (nth 1 symbol-uri-object))
          (setq symbol-class (car (nth 3 (nth 1 symbol-uri-object)))))
        (when (nth 5 (nth 1 symbol-uri-object))
          (setq symbol-trait (car (nth 5 (nth 1 symbol-uri-object)))))
        (when (nth 7 (nth 1 symbol-uri-object))
          (setq symbol-interface (car (nth 7 (nth 1 symbol-uri-object)))))
        (when (nth 9 (nth 1 symbol-uri-object))
          (setq symbol-function (car (nth 9 (nth 1 symbol-uri-object)))))

        ;; Place symbol in imenu if not there already
        (cond

         ;; Symbol is inside namespace
         (symbol-namespace
          (let ((imenu-nail (format "namespace %s" symbol-namespace)))
            (cond

             ;; Symbol is inside class inside a namespace
             (symbol-class
              (let ((imenu-nail2 (format "class %s" symbol-class)))
                (cond

                 ;; Symbol is inside function inside class inside namespace
                 (symbol-function
                  (let ((imenu-nail3 (format "function %s" symbol-function)))
                    (unless
                        (gethash
                         symbol-name
                         (gethash
                          imenu-nail3
                          (gethash
                           imenu-nail2
                           (gethash
                            imenu-nail
                            phps-mode-parser-sdt-symbol-imenu--table))))
                      (puthash
                       symbol-name
                       symbol-start
                       (gethash
                        imenu-nail3
                        (gethash
                         imenu-nail2
                         (gethash
                          imenu-nail
                          phps-mode-parser-sdt-symbol-imenu--table)))))))

                 ;; Symbol is inside class inside namespace
                 (t
                  (unless
                      (gethash
                       symbol-name
                       (gethash
                        imenu-nail2
                        (gethash
                         imenu-nail
                         phps-mode-parser-sdt-symbol-imenu--table)))
                    (puthash
                     symbol-name
                     symbol-start
                     (gethash
                      imenu-nail2
                      (gethash
                       imenu-nail
                       phps-mode-parser-sdt-symbol-imenu--table))))))))

             ;; Symbol is inside interface inside namespace
             (symbol-interface
              (let ((imenu-nail2 (format "interface %s" symbol-interface)))
                (cond

                 ;; Symbol is inside function inside interface inside namespace
                 (symbol-function
                  (let ((imenu-nail3 (format "function %s" symbol-function)))
                    (unless
                        (gethash
                         symbol-name
                         (gethash
                          imenu-nail3
                          (gethash
                           imenu-nail2
                           (gethash
                            imenu-nail
                            phps-mode-parser-sdt-symbol-imenu--table))))
                      (puthash
                       symbol-name
                       symbol-start
                       (gethash
                        imenu-nail3
                        (gethash
                         imenu-nail2
                         (gethash
                          imenu-nail
                          phps-mode-parser-sdt-symbol-imenu--table)))))))

                 ;; Symbol is inside interface inside namespace
                 (t
                  (unless
                      (gethash
                       symbol-name
                       (gethash
                        imenu-nail2
                        (gethash
                         imenu-nail
                         phps-mode-parser-sdt-symbol-imenu--table)))
                    (puthash
                     symbol-name
                     symbol-start
                     (gethash
                      imenu-nail2
                      (gethash
                       imenu-nail
                       phps-mode-parser-sdt-symbol-imenu--table))))))))

             ;; Symbol is inside trait inside namespace
             (symbol-trait
              (let ((imenu-nail2 (format "trait %s" symbol-trait)))
                (cond

                 ;; Symbol is inside function inside trait inside a namespace
                 (symbol-function
                  (let ((imenu-nail3 (format "function %s" symbol-function)))
                    (unless
                        (gethash
                         symbol-name
                         (gethash
                          imenu-nail3
                          (gethash
                           imenu-nail2
                           (gethash
                            imenu-nail
                            phps-mode-parser-sdt-symbol-imenu--table))))
                      (puthash
                       symbol-name
                       symbol-start
                       (gethash
                        imenu-nail3
                        (gethash
                         imenu-nail2
                         (gethash
                          imenu-nail
                          phps-mode-parser-sdt-symbol-imenu--table)))))))

                 ;; Symbol is inside trait inside namespace
                 (t
                  (unless
                      (gethash
                       symbol-name
                       (gethash
                        imenu-nail2
                        (gethash
                         imenu-nail
                         phps-mode-parser-sdt-symbol-imenu--table)))
                    (puthash
                     symbol-name
                     symbol-start
                     (gethash
                      imenu-nail2
                      (gethash
                       imenu-nail
                       phps-mode-parser-sdt-symbol-imenu--table))))))))

             ;; Symbol is inside a namespace
             (t
              (cond

               ;; Symbol is inside function inside namespace
               (symbol-function
                (let ((imenu-nail2 (format "function %s" symbol-function)))
                  (unless
                      (gethash
                       symbol-name
                       (gethash
                        imenu-nail2
                        (gethash
                         imenu-nail
                         phps-mode-parser-sdt-symbol-imenu--table)))
                    (puthash
                     symbol-name
                     symbol-start
                     (gethash
                      imenu-nail2
                      (gethash
                       imenu-nail
                       phps-mode-parser-sdt-symbol-imenu--table))))))

               ;; Symbol is inside a namespace
               (t
                (unless
                    (gethash
                     symbol-name
                     (gethash
                      imenu-nail
                      phps-mode-parser-sdt-symbol-imenu--table))
                  (puthash
                   symbol-name
                   symbol-start
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table)))))))))

         ;; Symbol is inside class
         (symbol-class
          (let ((imenu-nail (format "class %s" symbol-class)))
            (cond

             ;; Symbol is inside function inside class
             (symbol-function
              (let ((imenu-nail2 (format "function %s" symbol-function)))
                (unless
                    (gethash
                     symbol-name
                     (gethash
                      imenu-nail2
                      (gethash
                       imenu-nail
                       phps-mode-parser-sdt-symbol-imenu--table)))
                  (puthash
                   symbol-name
                   symbol-start
                   (gethash
                    imenu-nail2
                    (gethash
                     imenu-nail
                     phps-mode-parser-sdt-symbol-imenu--table))))))

             ;; Symbol is inside class
             (t
              (unless
                  (gethash
                   symbol-name
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))
                (puthash
                 symbol-name
                 symbol-start
                 (gethash
                  imenu-nail
                  phps-mode-parser-sdt-symbol-imenu--table)))))))

         ;; Symbol is inside trait
         (symbol-trait
          (let ((imenu-nail (format "trait %s" symbol-class)))
            (cond

             ;; Symbol is inside function inside trait
             (symbol-function
              (let ((imenu-nail2 (format "function %s" symbol-function)))
                (unless
                    (gethash
                     symbol-name
                     (gethash
                      imenu-nail2
                      (gethash
                       imenu-nail
                       phps-mode-parser-sdt-symbol-imenu--table)))
                  (puthash
                   symbol-name
                   symbol-start
                   (gethash
                    imenu-nail2
                    (gethash
                     imenu-nail
                     phps-mode-parser-sdt-symbol-imenu--table))))))

             ;; Symbol is inside trait
             (t
              (unless
                  (gethash
                   symbol-name
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))
                (puthash
                 symbol-name
                 symbol-start
                 (gethash
                  imenu-nail
                  phps-mode-parser-sdt-symbol-imenu--table)))))))

         ;; Symbol is inside interface
         (symbol-interface
          (let ((imenu-nail (format "interface %s" symbol-class)))
            (cond

             ;; Symbol is inside function inside interface
             (symbol-function
              (let ((imenu-nail2 (format "function %s" symbol-function)))
                (unless
                    (gethash
                     symbol-name
                     (gethash
                      imenu-nail2
                      (gethash
                       imenu-nail
                       phps-mode-parser-sdt-symbol-imenu--table)))
                  (puthash
                   symbol-name
                   symbol-start
                   (gethash
                    imenu-nail2
                    (gethash
                     imenu-nail
                     phps-mode-parser-sdt-symbol-imenu--table))))))

             ;; Symbol is inside interface
             (t
              (unless
                  (gethash
                   symbol-name
                   (gethash
                    imenu-nail
                    phps-mode-parser-sdt-symbol-imenu--table))
                (puthash
                 symbol-name
                 symbol-start
                 (gethash
                  imenu-nail
                  phps-mode-parser-sdt-symbol-imenu--table)))))))

         (symbol-function
          (let ((imenu-nail (format "function %s" symbol-function)))
            (unless
                (gethash
                 symbol-name
                 (gethash
                  imenu-nail
                  phps-mode-parser-sdt-symbol-imenu--table))
              (puthash
               symbol-name
               symbol-start
               (gethash
                imenu-nail
                phps-mode-parser-sdt-symbol-imenu--table)))))

         ;; Symbol is in no scope
         (t
          (unless (gethash symbol-name phps-mode-parser-sdt-symbol-imenu--table)
            (puthash
             symbol-name
             symbol-start
             phps-mode-parser-sdt-symbol-imenu--table))))


        (message
         "assign symbol uri: %S from %S + %S, start: %S, end: %S, object: %S"
         symbol-uri
         symbol-name
         symbol-scope
         symbol-start
         symbol-end
         symbol-uri-object)

        (setq
         phps-mode-parser-sdt-symbol-table-index
         (1+ phps-mode-parser-sdt-symbol-table-index))

        ;; Register new symbol in symbol-table
        ;; and place a reference to it in the symbol URI hash-map
        (if (gethash symbol-uri phps-mode-parser-sdt-symbol-table-by-uri)
            (progn
              (let ((symbol-uri-duplicate
                     (format
                      "%s (%d)"
                      symbol-uri
                      (1+ (length (gethash symbol-uri phps-mode-parser-sdt-symbol-table-by-uri))))))
                ;; TODO Place symbol in the correct place of the hierarchy here
                ;; (push
                ;;  `(,symbol-uri-duplicate . ,symbol-start)
                ;;  phps-mode-parser-sdt-symbol-imenu)
                )

              (puthash
               phps-mode-parser-sdt-symbol-table-index
               (list
                symbol-uri
                symbol-start
                symbol-end)
               phps-mode-parser-sdt-symbol-table)
              (puthash
               symbol-uri
               (append
                (gethash symbol-uri phps-mode-parser-sdt-symbol-table-by-uri)
                (list phps-mode-parser-sdt-symbol-table-index))
               phps-mode-parser-sdt-symbol-table-by-uri))

          ;; TODO Place symbol at the correct place of the hierarchy here
          ;; (push
          ;;  `(,symbol-uri . ,symbol-start)
          ;;  phps-mode-parser-sdt-symbol-imenu)

          (puthash
           phps-mode-parser-sdt-symbol-table-index
           (list
            symbol-uri
            symbol-start
            symbol-end)
           phps-mode-parser-sdt-symbol-table)
          (puthash
           symbol-uri
           (list phps-mode-parser-sdt-symbol-table-index)
           phps-mode-parser-sdt-symbol-table-by-uri))))
    (setq
     phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack
     nil))

  (when phps-mode-parser-sdt--bookkeeping-symbol-stack
    ;; Bookkeeping hit / misses of symbol references here
    (dolist (
             symbol-list
             phps-mode-parser-sdt--bookkeeping-symbol-stack)
      (let* ((symbol-name (car symbol-list))
             (symbol-scope (car (cdr symbol-list)))
             (symbol-start (car (cdr (cdr symbol-list))))
             (symbol-end (car (cdr (cdr (cdr symbol-list)))))
             (symbol-uri-object
              (phps-mode-parser-sdt--get-symbol-uri
               symbol-name
               symbol-scope))
             (symbol-uri (car symbol-uri-object))
             (symbol-hit 0))
        (cond

         ;; Super-global variable
         ((gethash
           symbol-name
           phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
          (setq symbol-hit -1))

         ;; Declared variable
         ((gethash
           symbol-uri
           phps-mode-parser-sdt-symbol-table-by-uri)
          ;; (message "matches: %S" (gethash
          ;;  symbol-uri
          ;;  phps-mode-parser-sdt-symbol-table-by-uri))
          (let* ((matching-symbol-ids
                  (gethash
                   symbol-uri
                   phps-mode-parser-sdt-symbol-table-by-uri))
                 (matching-symbol-index 0)
                 (matching-symbol-count (length matching-symbol-ids))
                 (matching-hit))
            (while (and
                 (not matching-hit)
                 (< matching-symbol-index matching-symbol-count))
              (let* ((matching-symbol-id
                      (nth matching-symbol-index matching-symbol-ids))
                     (matching-symbol
                      (gethash
                       matching-symbol-id
                       phps-mode-parser-sdt-symbol-table))
                     (matching-symbol-start
                      (nth 1 matching-symbol)))
                ;; (message "matching-symbol: %S" matching-symbol)
                ;; (message "matching-symbol-start: %S" matching-symbol-start)
                (when (<= matching-symbol-start symbol-start)
                  (setq matching-hit t)
                  (setq symbol-hit matching-symbol-id)))
              (setq matching-symbol-index (1+ matching-symbol-index))))))

        (puthash
         (list
          symbol-start
          symbol-end)
         symbol-hit
         phps-mode-parser-sdt-bookkeeping)

        (message
         "reference symbol uri: %S from %S + %S, start: %S, end: %S, object: %S, hit?: %S"
         symbol-uri
         symbol-name
         symbol-scope
         symbol-start
         symbol-end
         symbol-uri-object
         symbol-hit)

        ))
    (setq
     phps-mode-parser-sdt--bookkeeping-symbol-stack
     nil)))


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
(puthash 71 (lambda(args _terminals) args) phps-mode-parser--table-translations)

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
(puthash 81 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 82 ((identifier) (semi_reserved))
(puthash 82 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 83 ((top_statement_list) (top_statement_list top_statement))
(puthash
 83
 (lambda(args _terminals) (if (car args) (append (car args) (cdr args)) (cdr args)))
 phps-mode-parser--table-translations)

;; 84 ((top_statement_list) (%empty))
(puthash 84 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 85 ((namespace_declaration_name) (identifier))
(puthash
 85
 (lambda(args terminals)
   args)
 phps-mode-parser--table-translations)

;; 86 ((namespace_declaration_name) (T_NAME_QUALIFIED))
(puthash 86 (lambda(_args terminals) terminals) phps-mode-parser--table-translations)

;; 87 ((namespace_name) (T_STRING))
(puthash 87 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 88 ((namespace_name) (T_NAME_QUALIFIED))
(puthash 88 (lambda(_args terminals) terminals) phps-mode-parser--table-translations)

;; 89 ((legacy_namespace_name) (namespace_name))
(puthash 89 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 90 ((legacy_namespace_name) (T_NAME_FULLY_QUALIFIED))
(puthash 90 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 91 ((name) (T_STRING))
(puthash 91 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 92 ((name) (T_NAME_QUALIFIED))
(puthash 92 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 93 ((name) (T_NAME_FULLY_QUALIFIED))
(puthash 93 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 94 ((name) (T_NAME_RELATIVE))
(puthash 94 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 95 ((attribute_decl) (class_name))
(puthash
 95
 (lambda(args _terminals)
   `(
     ast-type
     attribute-decl
     class-name
     ,args
     ))
 phps-mode-parser--table-translations)

;; 96 ((attribute_decl) (class_name argument_list))
(puthash
 96
 (lambda(args _terminals)
   `(
     ast-type
     attribute-decl
     class-name
     ,(nth 0 args)
     argument-list
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 97 ((attribute_group) (attribute_decl))
(puthash
 97
 (lambda(args _terminals)
   `(
     ast-type
     attribute-group
     attribute-decl
     (,args)
     ))
 phps-mode-parser--table-translations)

;; 98 ((attribute_group) (attribute_group "," attribute_decl))
(puthash
 98
 (lambda(args _terminals)
   `(
     ast-type
     attribute-group
     attribute-group
     ,(append (plist-get (nth 0 args) 'ast-type) (list (nth 2 args)))
     ))
 phps-mode-parser--table-translations)

;; 99 ((attribute) (T_ATTRIBUTE attribute_group possible_comma "]"))
(puthash
 99
 (lambda(args _terminals)
   `(
     ast-type
     attribute
     attribute-group
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 100 ((attributes) (attribute))
(puthash
 100
 (lambda(args _terminals)
   `(
     ast-type
     attributes
     attributes
     (,args)
     ))
 phps-mode-parser--table-translations)

;; 101 ((attributes) (attributes attribute))
(puthash 101 (lambda(args _terminals) (append (nth 0 args) (list (nth 1 args)))) phps-mode-parser--table-translations)

;; 102 ((attributed_statement) (function_declaration_statement))
(puthash 102 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 103 ((attributed_statement) (class_declaration_statement))
(puthash 103 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 104 ((attributed_statement) (trait_declaration_statement))
(puthash 104 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 105 ((attributed_statement) (interface_declaration_statement))
(puthash 105 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 106 ((attributed_statement) (enum_declaration_statement))
(puthash 106 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 107 ((top_statement) (statement))
(puthash
 107
 (lambda(args _terminals)
   (phps-mode-parser-sdt--parse-top-statement)
   args)
 phps-mode-parser--table-translations)

;; 108 ((top_statement) (attributed_statement))
(puthash
 108
 (lambda(args _terminals)
   (phps-mode-parser-sdt--parse-top-statement)
   args)
 phps-mode-parser--table-translations)

;; 109 ((top_statement) (attributed_statement))
(puthash
 109
 (lambda(args _terminals)
   (phps-mode-parser-sdt--parse-top-statement)
   args)
 phps-mode-parser--table-translations)

;; 110 ((top_statement) (T_HALT_COMPILER "(" ")" ";"))
(puthash
 109
 (lambda(args _terminals)
   (phps-mode-parser-sdt--parse-top-statement)
   (nth 0 args))
 phps-mode-parser--table-translations)

;; 111 top_statement -> (T_NAMESPACE namespace_declaration_name ";")
(puthash
 111
 (lambda(args terminals)
   (let ((name (nth 1 args))
         (index (car (cdr (nth 1 terminals))))
         (start (car (cdr (nth 2 terminals))))
         (end (point-max)))

     ;; Add to imenu if not there already
     (let ((imenu-nail (format "namespace %s" name)))
       (unless (gethash
                imenu-nail
                phps-mode-parser-sdt-symbol-imenu--table)
         (let ((namespace-table (make-hash-table :test 'equal)))
           (puthash
            'declaration
            index
            namespace-table)
           (puthash
            imenu-nail
            namespace-table
            phps-mode-parser-sdt-symbol-imenu--table))))

     (phps-mode-parser-sdt--parse-top-statement)
     (push
      (list 'namespace namespace-name start end)
      phps-mode-parser-sdt--bookkeeping-namespace)
     (setq
      phps-mode-parser-sdt-symbol-imenu--namespace
      name)
     `(
       ast-type
       namespace
       ast-name
       ,name
       ast-index
       ,index
       ast-start
       ,start
       ast-end
       ,end)))
 phps-mode-parser--table-translations)

;; 112 top_statement -> (T_NAMESPACE namespace_declaration_name "{" top_statement_list "}")
(puthash
 112
 (lambda(args terminals)
   (let ((name (nth 1 args))
         (index (car (cdr (nth 1 terminals))))
         (start (car (cdr (nth 2 terminals))))
         (end (car (cdr (nth 4 terminals)))))

     ;; Add to imenu if not there already
     (let ((imenu-nail (format "namespace %s" name)))
       (unless (gethash
                imenu-nail
                phps-mode-parser-sdt-symbol-imenu--table)
         (let ((namespace-table (make-hash-table :test 'equal)))
           (puthash
            'declaration
            index
            namespace-table)
           (puthash
            imenu-nail
            namespace-table
            phps-mode-parser-sdt-symbol-imenu--table))))

     ;; Add namespace to all imenu stack items
     (when phps-mode-parser-sdt-symbol-imenu--stack
       (dolist (item phps-mode-parser-sdt-symbol-imenu--stack)
         (let ((item-start (nth 2 item))
               (item-end (nth 3 item)))
           (when (and
                  (>= item-start start)
                  (<= item-end end))
             (setcar
              (list 'namespace name start end)
              item)))))

     ;; Add namespace to all symbols in scope (start, end) here
     (when phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (car (cdr symbol-list))))
               (push
                (list 'namespace name start end)
                symbol-scope)
               (setcar
                (cdr symbol-list)
                symbol-scope))))))

     (when phps-mode-parser-sdt--bookkeeping-symbol-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-stack)
         (let ((symbol-name (car symbol-list))
               (symbol-scope (car (cdr symbol-list))))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (car (cdr symbol-list))))
               (push
                (list 'namespace name start end)
                symbol-scope)
               (setcar
                (cdr symbol-list)
                symbol-scope))))))

     (phps-mode-parser-sdt--parse-top-statement)
     (setq phps-mode-parser-sdt--bookkeeping-namespace nil)
     `(
       ast-type
       namespace
       ast-name
       ,name
       ast-index
       ,index
       ast-start
       ,start
       ast-end
       ,end
       top-statement-list
       ,(nth 3 args))))
 phps-mode-parser--table-translations)

;; 113 top_statement -> (T_NAMESPACE "{" top_statement_list "}")
(puthash
 113
 (lambda(args terminals)
   (phps-mode-parser-sdt--parse-top-statement)
   `(
     ast-type
     namespace
     ast-index
     ,(car (cdr (nth 0 terminals)))
     ast-start
     ,(car (cdr (nth 1 terminals)))
     ast-end
     ,(car (cdr (nth 3 terminals)))
     top-statement-list
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 114 ((top_statement) (T_USE mixed_group_use_declaration ";"))
(puthash
 114
 (lambda(args _terminals)
   (phps-mode-parser-sdt--parse-top-statement)
   `(
     ast-type
     mixed-group-use-declaration-top-statement
     ast-value
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 115 ((top_statement) (T_USE use_type group_use_declaration ";"))
(puthash
 115
 (lambda(args _terminals)
   (phps-mode-parser-sdt--parse-top-statement)
   `(
     ast-type
     type-group-use-declaration-top-statement
     use-type
     ,(nth 1 args)
     group-use-declaration
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 116 ((top_statement) (T_USE use_declarations ";"))
(puthash
 116
 (lambda(args _terminals)
   (phps-mode-parser-sdt--parse-top-statement)
   `(
     ast-type
     use-declarations-top-statement
     ast-value
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 117 ((top_statement) (T_USE use_type use_declarations ";"))
(puthash
 117
 (lambda(args _terminals)
   (phps-mode-parser-sdt--parse-top-statement)
   `(
     ast-type
     type-use-declarations-top-statement
     ast-value
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 118 ((top_statement) (T_CONST const_list ";"))
(puthash
 118
 (lambda(args _terminals)
   (phps-mode-parser-sdt--parse-top-statement)
   `(
     ast-type
     const-list-top-statement
     ast-value
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 119 ((use_type) (T_FUNCTION))
(puthash 119 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 120 ((use_type) (T_CONST))
(puthash 120 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 121 ((group_use_declaration) (legacy_namespace_name T_NS_SEPARATOR "{" unprefixed_use_declarations possible_comma "}"))
(puthash
 121
 (lambda(args _terminals)
   `(
     ast-type
     legacy-group-use-declaration
     unprefixed-use-declarations
     ,(nth 3 args)
     ))
 phps-mode-parser--table-translations)

;; 122 ((mixed_group_use_declaration) (legacy_namespace_name T_NS_SEPARATOR "{" inline_use_declarations possible_comma "}"))
(puthash
 122
 (lambda(args _terminals)
   `(
     ast-type
     mixed-group-use-declaration
     inline-use-declarations
     ,(nth 3 args)
     ))
 phps-mode-parser--table-translations)

;; 123 ((possible_comma) (%empty))
(puthash 123 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 124 ((possible_comma) (","))
(puthash 124 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 125 ((inline_use_declarations) (inline_use_declarations "," inline_use_declaration))
(puthash
 125
 (lambda(args _terminals)
   `(
     ast-type
     inline-use-declarations
     ,(append (nth 0 args) (list (nth 2 args)))
     ))
 phps-mode-parser--table-translations)

;; 126 ((inline_use_declarations) (inline_use_declaration))
(puthash
 126
 (lambda(args _terminals)
   `(
     ast-type
     inline-use-declarations
     ,(list (nth 1 args))
     ))
 phps-mode-parser--table-translations)

;; 127 ((unprefixed_use_declarations) (unprefixed_use_declarations "," unprefixed_use_declaration))
(puthash
 127
 (lambda(args _terminals)
   `(
     ast-type
     unprefixed-use-declarations
     ,(append (nth 0 args) (list (nth 2 args)))
     ))
 phps-mode-parser--table-translations)

;; 128 ((unprefixed_use_declarations) (unprefixed_use_declaration))
(puthash
 128
 (lambda(args _terminals)
   `(
     ast-type
     unprefixed-use-declarations
     ,(list args))
   )
 phps-mode-parser--table-translations)

;; 129 ((use_declarations) (use_declarations "," use_declaration))
(puthash
 129
 (lambda(args _terminals)
   `(
     ast-type
     use-declarations
     ,(append (nth 0 args) (list (nth 2 args)))
     ))
 phps-mode-parser--table-translations)

;; 130 ((use_declarations) (use_declaration))
(puthash
 130
 (lambda(args _terminals)
   `(
     ast-type
     use-declarations
     ,(list args)
     ))
 phps-mode-parser--table-translations)

;; 131 ((inline_use_declaration) (unprefixed_use_declaration))
(puthash
 131
 (lambda(args _terminals)
   `(
     ast-type
     inline-use-declaration
     declation
     ,(list args)
     ))
 phps-mode-parser--table-translations)

;; 132 ((inline_use_declaration) (use_type unprefixed_use_declaration))
(puthash
 132
 (lambda(args _terminals)
   `(
     ast-type
     inline-use-declaration
     use-type
     ,(nth 0 args)
     declaration
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 133 ((unprefixed_use_declaration) (namespace_name))
(puthash
 133
 (lambda(args _terminals)
   `(
     namespace-name
     ,args
     ))
 phps-mode-parser--table-translations)

;; 134 ((unprefixed_use_declaration) (namespace_name T_AS T_STRING))
(puthash
 134
 (lambda(args _terminals)
   `(
     namespace-name
     ,(nth 0 args)
     as
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 135 ((use_declaration) (legacy_namespace_name))
(puthash
 135
 (lambda(args _terminals)
   `(
     ast-type
     use-declaration
     legacy-namespace-name
     ,args
     ))
 phps-mode-parser--table-translations)

;; 136 ((use_declaration) (legacy_namespace_name T_AS T_STRING))
(puthash
 136
 (lambda(args _terminals)
   `(
     ast-type
     use-declaration
     legacy-namespace-name
     ,(nth 0 args)
     as
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 137 ((const_list) (const_list "," const_decl))
(puthash 137 (lambda(args _terminals) (append (nth 0 args) (list (nth 2 args)))) phps-mode-parser--table-translations)

;; 138 ((const_list) (const_decl))
(puthash 138 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 139 inner_statement_list -> (inner_statement_list inner_statement)
(puthash
 139
 (lambda(args _terminals)
   (if (car args)
       (append (car args) (cdr args))
     (cdr args)))
 phps-mode-parser--table-translations)

;; 140 ((inner_statement_list) (%empty))
(puthash 140 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 141 ((inner_statement) (statement))
(puthash 141 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 142 ((inner_statement) (attributed_statement))
(puthash 142 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 143 ((inner_statement) (attributes attributed_statement))
(puthash 143 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 144 ((inner_statement) (T_HALT_COMPILER "(" ")" ";"))
(puthash 144 (lambda(_args _terminals) '(ast-type halt-compiler-inner-statement)) phps-mode-parser--table-translations)

;; 145 ((statement) ("{" inner_statement_list "}"))
(puthash
 145
 (lambda(args _terminals)
   (nth 1 args))
 phps-mode-parser--table-translations)

;; 146 ((statement) (if_stmt))
(puthash
 146
 (lambda(args _terminals)
   args)
 phps-mode-parser--table-translations)

;; 147 ((statement) (alt_if_stmt))
(puthash
 147
 (lambda(args _terminals)
   args)
 phps-mode-parser--table-translations)

;; 148 ((statement) (T_WHILE "(" expr ")" while_statement))
(puthash
 148
 (lambda(args _terminals)
   `(
     ast-type
     while-statement
     expr
     ,(nth 2 args)
     while-statement
     ,(nth 4 args))
   )
 phps-mode-parser--table-translations)

;; 149 ((statement) (T_DO statement T_WHILE "(" expr ")" ";"))
(puthash
 149
 (lambda(args _terminals)
   `(
     ast-type
     do-statement
     statement
     ,(nth 1 args)
     expr
     ,(nth 4 args)
     ))
 phps-mode-parser--table-translations)

;; 150 ((statement) (T_FOR "(" for_exprs ";" for_exprs ";" for_exprs ")" for_statement))
(puthash
 150
 (lambda(args _terminals)
   `(
     ast-type
     for-statement
     initial
     ,(nth 2 args)
     test
     ,(nth 4 args)
     incremental
     ,(nth 6 args)
     for-statement
     ,(nth 8 args)
     ))
 phps-mode-parser--table-translations)

;; 151 ((statement) (T_SWITCH "(" expr ")" switch_case_list))
(puthash
 151
 (lambda(args _terminals)
   `(
     ast-type
     switch-statement
     expr
     ,(nth 2 args)
     switch-case-list
     ,(nth 4 args)
     ))
 phps-mode-parser--table-translations)

;; 152 ((statement) (T_BREAK optional_expr ";"))
(puthash
 152
 (lambda(args _terminals)
   `(
     ast-type
     break-statement
     optional-expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 153 ((statement) (T_CONTINUE optional_expr ";"))
(puthash
 153
 (lambda(args _terminals)
   `(
     ast-type
     continue-statement
     optional-expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 154 ((statement) (T_RETURN optional_expr ";"))
(puthash
 154
 (lambda(args _terminals)
   `(
     ast-type
     return-statement
     optional-expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 155 ((statement) (T_GLOBAL global_var_list ";"))
(puthash
 155
 (lambda(args _terminals)
   (dolist (stack-item phps-mode-parser-sdt--bookkeeping-symbol-stack)
     (push '(global) (nth 1 stack-item)))
   (let ((global-var-list (nth 1 args)))
     (dolist (global-var global-var-list)
       (let ((global-var-type (plist-get global-var 'ast-type)))
         (cond
          ((equal global-var-type 'simple-variable-variable)
           (let ((variable-name (plist-get global-var 'variable))
                 (variable-start (plist-get global-var 'ast-start))
                 (variable-end (plist-get global-var 'ast-end)))
             (push
              (list
               variable-name
               phps-mode-parser-sdt--bookkeeping-namespace
               variable-start
               variable-end)
              phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)))))))
   `(
     ast-type
     global-statement
     global-var-list
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 156 ((statement) (T_STATIC static_var_list ";"))
(puthash
 156
 (lambda(args _terminals)
   (let ((static-var-list (nth 1 args)))
     (dolist (static-var static-var-list)
       (let ((static-var-type (plist-get static-var 'ast-type)))
         (cond
          ((equal static-var-type 'variable)
           (let* ((variable-name (plist-get static-var 'ast-name))
                  (variable-start (plist-get static-var 'ast-start))
                  (variable-end (plist-get static-var 'ast-end)))
             (push
              (list
               variable-name
               phps-mode-parser-sdt--bookkeeping-namespace
               variable-start
               variable-end)
              phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
             (push
              (list
               variable-name
               phps-mode-parser-sdt--bookkeeping-namespace
               variable-start
               variable-end)
              phps-mode-parser-sdt--bookkeeping-symbol-stack)))))))
   `(
     ast-type
     static-statement
     static-var-list
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 157 ((statement) (T_ECHO echo_expr_list ";"))
(puthash
 157
 (lambda(args _terminals)
   `(
     ast-type
     echo-statement
     echo-expr-list
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 158 ((statement) (T_INLINE_HTML))
(puthash
 158
 (lambda(args _terminals)
   args)
 phps-mode-parser--table-translations)

;; 159 ((statement) (expr ";"))
(puthash
 159
 (lambda(args _terminals)
   `(
     ast-type
     expr-statement
     expr
     ,(nth 0 args)
     ))
 phps-mode-parser--table-translations)

;; 160 ((statement) (T_UNSET "(" unset_variables possible_comma ")" ";"))
(puthash
 160
 (lambda(args _terminals)
   (nth 0 args))
 phps-mode-parser--table-translations)

;; 161 ((statement) (T_FOREACH "(" expr T_AS foreach_variable ")" foreach_statement))
(puthash
 161
 (lambda(args _terminals)
   `(
     ast-type
     foreach-statement
     expr
     ,(nth 2 args)
     as
     ,(nth 4 args)
     foreach-statement
     ,(nth 6 args)))
 phps-mode-parser--table-translations)

;; 162 ((statement) (T_FOREACH "(" expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable ")" foreach_statement))
(puthash
 162
 (lambda(args _terminals)
   `(
     ast-type
     foreach-statement
     expr
     ,(nth 2 args)
     as
     ,(nth 4 args)
     value
     ,(nth 6 args)
     foreach-statement
     ,(nth 8 args)))
 phps-mode-parser--table-translations)

;; 163 ((statement) (T_DECLARE "(" const_list ")" declare_statement))
(puthash
 163
 (lambda(args _terminals)
   `(
     ast-type
     declare-statement
     const-list
     ,(nth 2 args)
     declare-statement
     ,(nth 4 args)
     value
     ,(nth 6 args)
     ))
 phps-mode-parser--table-translations)

;; 164 ((statement) (";"))
(puthash
 164
 (lambda(_args _terminals)
   nil)
 phps-mode-parser--table-translations)

;; 165 ((statement) (T_TRY "{" inner_statement_list "}" catch_list finally_statement))
(puthash
 165
 (lambda(args _terminals)
   `(
     ast-type
     try-statement
     inner-statement-list
     ,(nth 2 args)
     catch-list
     ,(nth 4 args)
     finally-statement
     ,(nth 5 args)))
 phps-mode-parser--table-translations)

;; 166 ((statement) (T_GOTO T_STRING ";"))
(puthash
 166
 (lambda(args _terminals)
   `(
     ast-type
     goto-statement
     label
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 167 ((statement) (T_STRING ":"))
(puthash
 167
 (lambda(args _terminals)
   `(
     ast-type
     label-statement
     label
     ,(nth 0 args)))
 phps-mode-parser--table-translations)

;; 168 ((catch_list) (%empty))
(puthash 168 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 169 ((catch_list) (catch_list T_CATCH "(" catch_name_list optional_variable ")" "{" inner_statement_list "}"))
(puthash
 169
 (lambda(args terminals)
   (let ((optional-variable (nth 4 args))
         (optional-variable-start)
         (optional-variable-end))
     (when optional-variable
       (setq optional-variable-start (car (cdr (nth 4 terminals))))
       (setq optional-variable-end (cdr (cdr (nth 4 terminals))))
       (push
        (list
         optional-variable
         phps-mode-parser-sdt--bookkeeping-namespace
         optional-variable-start
         optional-variable-end)
        phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
       (push
        (list
         optional-variable
         phps-mode-parser-sdt--bookkeeping-namespace
         optional-variable-start
         optional-variable-end)
        phps-mode-parser-sdt--bookkeeping-symbol-stack))
   `(
     ast-type
     catch-list
     catch-name-list
     ,(nth 3 args)
     optional-variable
     ,optional-variable
     optional-variable-start
     ,optional-variable-start
     optional-variable-end
     ,optional-variable-end
     inner-statement-list
     ,(nth 7 args))))
 phps-mode-parser--table-translations)

;; 170 ((catch_name_list) (class_name))
(puthash
 170
 (lambda(args _terminals)
   `(
     ast-type
     catch-name-list
     class-names
     (,args)
     ))
 phps-mode-parser--table-translations)

;; 171 ((catch_name_list) (catch_name_list "|" class_name))
(puthash
 171
 (lambda(args _terminals)
   `(
     ast-type
     catch-name-list
     class-names
     ,(append (nth 0 args) (list (nth 2 args)))
     ))
 phps-mode-parser--table-translations)

;; 172 ((optional_variable) (%empty))
(puthash 172 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 173 ((optional_variable) (T_VARIABLE))
(puthash 173 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 174 ((finally_statement) (%empty))
(puthash 174 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 175 ((finally_statement) (T_FINALLY "{" inner_statement_list "}"))
(puthash
 175
 (lambda(args _terminals)
   `(
     ast-type
     finally-statement
     inner-statement-list
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 176 ((unset_variables) (unset_variable))
(puthash
 176
 (lambda(args _terminals)
   `(
     ast-type
     unset-variables
     variables
     ,(list args)
     ))
 phps-mode-parser--table-translations)

;; 177 ((unset_variables) (unset_variables "," unset_variable))
(puthash
 177
 (lambda(args _terminals)
   `(
     ast-type
     unset-variables
     variables
     ,(append (nth 0 args) (nth 2 args))
     ))
 phps-mode-parser--table-translations)

;; 178 ((unset_variable) (variable))
(puthash 178 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 179 ((function_declaration_statement) (function returns_ref T_STRING backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags))
(puthash
 179
 (lambda(args terminals)
   ;; (message "179-parameter_list-args: %S" (nth 5 args))
   ;; (message "parameter_list-terminals: %S" (nth 5 terminals))

   ;; Iterate optional parameters are declare them as new variable declarations
   (when-let ((parameter-list (nth 5 args)))
     (dolist (parameter parameter-list)
       (let ((parameter-ast-type (plist-get parameter 'ast-type)))
         (cond
          ((equal parameter-ast-type 'attributed-parameter)
           (let* ((attributed-parameter
                  (plist-get
                   parameter
                   'parameter))
                  (attributed-parameter-name
                   (plist-get attributed-parameter 'ast-name))
                  (symbol-name
                   attributed-parameter-name)
                  (symbol-scope
                   phps-mode-parser-sdt--bookkeeping-namespace)
                  (symbol-start
                   (plist-get attributed-parameter 'ast-start))
                  (symbol-end
                   (plist-get attributed-parameter 'ast-end)))
             (push
              (list
               symbol-name
               symbol-scope
               symbol-start
               symbol-end)
              phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
             (push
              (list
               symbol-name
               symbol-scope
               symbol-start
               symbol-end)
              phps-mode-parser-sdt--bookkeeping-symbol-stack)))))))

   ;; (message "before:")
   ;; (message
   ;;  "phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack: %S"
   ;;  phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
   ;; (message
   ;;  "phps-mode-parser-sdt--bookkeeping-symbol-stack: %S"
   ;;  phps-mode-parser-sdt--bookkeeping-symbol-stack)

   ;; Go through stacks and modify symbol namespaces
   ;; - add function scope but only for non-super-global variables
   (let ((function-name (nth 2 args))
         (function-start (car (cdr (nth 2 terminals))))
         (function-end (car (cdr (nth 11 terminals)))))

     ;; Add function to imenu stack
     (push
      (list 'function function-name function-start function-end)
      phps-mode-parser-sdt-symbol-imenu--stack)

     (when phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (car (cdr symbol-list))))
               (push
                (list 'function function-name function-start function-end)
                symbol-scope)
               (setcar
                (cdr symbol-list)
                symbol-scope))))))
     (when phps-mode-parser-sdt--bookkeeping-symbol-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (car (cdr symbol-list))))
               (push
                (list 'function function-name function-start function-end)
                symbol-scope)
               (setcar
                (cdr symbol-list)
                symbol-scope)))))))


   ;; (message "after:")
   ;; (message
   ;;  "phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack: %S"
   ;;  phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
   ;; (message
   ;;  "phps-mode-parser-sdt--bookkeeping-symbol-stack: %S"
   ;;  phps-mode-parser-sdt--bookkeeping-symbol-stack)


   `(
     ast-type
     function
     ast-name
     ,(nth 2 args)
     ast-index
     ,(car (cdr (nth 2 terminals)))
     ast-start
     ,(car (cdr (nth 9 terminals)))
     ast-end
     ,(car (cdr (nth 11 terminals)))
     returns-reference-p
     ,(not (equal (nth 1 args) nil))
     parameter-list
     ,(nth 5 args)
     return-type
     ,(nth 7 args)
     inner-statement-list
     ,(nth 10 args)
     ))
 phps-mode-parser--table-translations)

;; 180 ((is_reference) (%empty))
(puthash 180 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 181 ((is_reference) (T_AMPERSAND_FOLLOWED_BY_VAR_OR_VARARG))
(puthash 181 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 182 ((is_variadic) (%empty))
(puthash 182 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 183 ((is_variadic) (T_ELLIPSIS))
(puthash 183 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 184 ((class_declaration_statement) (class_modifiers T_CLASS T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}"))
(puthash
 184
 (lambda(args terminals)
   ;; Go through stacks and modify symbol name-spaces
   ;; but only for non-super-global variables.
   ;; 
   ;; Should place class scope first in scope
   ;; unless a namespace exists, in that case it should be placed second in scope
   (let ((class-name (nth 2 args))
         (class-start (car (cdr (nth 2 terminals))))
         (class-end (cdr (cdr (nth 8 terminals)))))

     ;; Add class scope to all functions in class
     (when phps-mode-parser-sdt-symbol-imenu--stack
       (dolist (item phps-mode-parser-sdt-symbol-imenu--stack)
         (let ((item-start (nth 2 item))
               (item-end (nth 3 item)))
           (when (and
                  (>= item-start class-start)
                  (<= item-end class-end))
             (setcar
              item
              (list 'class class-name class-start class-end))))))

     ;; Add class to imenu stack
     (push
      (list 'class class-name class-start class-end)
      phps-mode-parser-sdt-symbol-imenu--stack)

     (when phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (reverse (car (cdr symbol-list)))))
               (if (equal (car (car symbol-scope)) 'namespace)
                   (let ((namespace-name (nth 1 (car symbol-scope)))
                         (namespace-start (nth 2 (car symbol-scope)))
                         (namespace-end (nth 3 (car symbol-scope))))
                     (setcar
                      symbol-scope
                      (list 'class class-name class-start class-end))
                     (push
                      (list 'namespace namespace-name namespace-start namespace-end)
                      symbol-scope))
                 (push
                  (list 'class class-name class-start class-end)
                  symbol-scope))
               (setcar
                (cdr symbol-list)
                (reverse symbol-scope)))))))

     (when phps-mode-parser-sdt--bookkeeping-symbol-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (reverse (car (cdr symbol-list)))))
               (if (equal (car (car symbol-scope)) 'namespace)
                   (let ((namespace-name (nth 1 (car symbol-scope)))
                         (namespace-start (nth 2 (car symbol-scope)))
                         (namespace-end (nth 3 (car symbol-scope))))
                     (setcar
                      symbol-scope
                      (list 'class class-name class-start class-end))
                     (push
                      (list 'namespace namespace-name namespace-start namespace-end)
                      symbol-scope))
                 (push
                  (list 'class class-name class-start class-end)
                  symbol-scope))
               (setcar
                (cdr symbol-list)
                (reverse symbol-scope))))))))

   `(
     ast-type
     class
     modifiers
     ,(nth 0 args)
     ast-name
     ,(nth 2 args)
     extends-from
     ,(nth 3 args)
     implements-list
     ,(nth 4 args)
     ast-index
     ,(car (cdr (nth 2 terminals)))
     ast-start
     ,(car (cdr (nth 6 terminals)))
     ast-end
     ,(car (cdr (nth 8 terminals)))
     class-statement-list
     ,(nth 7 args)
     ))
 phps-mode-parser--table-translations)

;; 185 ((class_declaration_statement) (T_CLASS T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}"))
(puthash
 185
 (lambda(args terminals)
   ;; Go through stacks and modify symbol name-spaces
   ;; but only for non-super-global variables.
   ;; 
   ;; Should place class scope first in scope
   ;; unless a namespace exists, in that case it should be placed second in scope
   (let ((class-name (nth 1 args))
         (class-start (car (cdr (nth 1 terminals))))
         (class-end (cdr (cdr (nth 7 terminals)))))
     (when phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (reverse (car (cdr symbol-list)))))
               (if (equal (car (car symbol-scope)) 'namespace)
                   (let ((namespace-name (nth 1 (car symbol-scope)))
                         (namespace-start (nth 2 (car symbol-scope)))
                         (namespace-end (nth 3 (car symbol-scope))))
                     (setcar
                      symbol-scope
                      (list 'class class-name class-start class-end))
                     (push
                      (list 'namespace namespace-name namespace-start namespace-end)
                      symbol-scope))
                 (push
                  (list 'class class-name class-start class-end)
                  symbol-scope))
               (setcar
                (cdr symbol-list)
                (reverse symbol-scope)))))))

     (when phps-mode-parser-sdt--bookkeeping-symbol-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (reverse (car (cdr symbol-list)))))
               (if (equal (car (car symbol-scope)) 'namespace)
                   (let ((namespace-name (nth 1 (car symbol-scope)))
                         (namespace-start (nth 2 (car symbol-scope)))
                         (namespace-end (nth 3 (car symbol-scope))))
                     (setcar
                      symbol-scope
                      (list 'class class-name class-start class-end))
                     (push
                      (list 'namespace namespace-name namespace-start namespace-end)
                      symbol-scope))
                 (push
                  (list 'class class-name class-start class-end)
                  symbol-scope))
               (setcar
                (cdr symbol-list)
                (reverse symbol-scope))))))))

   `(
     ast-type
     class
     ast-name
     ,(nth 1 args)
     extends-from
     ,(nth 2 args)
     implements-list
     ,(nth 3 args)
     ast-index
     ,(car (cdr (nth 1 terminals)))
     ast-start
     ,(car (cdr (nth 5 terminals)))
     ast-end
     ,(car (cdr (nth 7 terminals)))
     class-statement-list
     ,(nth 6 args)))
 phps-mode-parser--table-translations)

;; 186 ((class_modifiers) (class_modifier))
(puthash 186 (lambda(args _terminals) `(,args)) phps-mode-parser--table-translations)

;; 187 ((class_modifiers) (class_modifiers class_modifier))
(puthash 187 (lambda(args _terminals) `(append ,(nth 0 args) (,(nth 1 args)))) phps-mode-parser--table-translations)

;; 188 ((class_modifier) (T_ABSTRACT))
(puthash 188 (lambda(_args _terminals) 'T_ABSTRACT) phps-mode-parser--table-translations)

;; 189 ((class_modifier) (T_FINAL))
(puthash 189 (lambda(_args _terminals) 'T_FINAL) phps-mode-parser--table-translations)

;; 190 ((trait_declaration_statement) (T_TRAIT T_STRING backup_doc_comment "{" class_statement_list "}"))
(puthash
 190
 (lambda(args terminals)
   ;; Go through stacks and modify symbol name-spaces
   ;; but only for non-super-global variables.
   ;; 
   ;; Should place class scope first in scope
   ;; unless a namespace exists, in that case it should be placed second in scope
   (let ((class-name (nth 1 args))
         (class-start (car (cdr (nth 1 terminals))))
         (class-end (cdr (cdr (nth 5 terminals)))))

     ;; Add class scope to all functions in class
     (when phps-mode-parser-sdt-symbol-imenu--stack
       (dolist (item phps-mode-parser-sdt-symbol-imenu--stack)
         (let ((item-start (nth 2 item))
               (item-end (nth 3 item)))
           (when (and
                  (>= item-start class-start)
                  (<= item-end class-end))
             (setcar
              item
              (list 'class class-name class-start class-end))))))

     ;; Add class to imenu stack
     (push
      (list 'class class-name class-start class-end)
      phps-mode-parser-sdt-symbol-imenu--stack)

     (when phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (reverse (car (cdr symbol-list)))))
               (if (equal (car (car symbol-scope)) 'namespace)
                   (let ((namespace-name (nth 1 (car symbol-scope)))
                         (namespace-start (nth 2 (car symbol-scope)))
                         (namespace-end (nth 3 (car symbol-scope))))
                     (setcar
                      symbol-scope
                      (list 'trait class-name class-start class-end))
                     (push
                      (list 'namespace namespace-name namespace-start namespace-end)
                      symbol-scope))
                 (push
                  (list 'trait class-name class-start class-end)
                  symbol-scope))
               (setq symbol-scope (reverse symbol-scope))
               (setcar
                (cdr symbol-list)
                symbol-scope))))))

     (when phps-mode-parser-sdt--bookkeeping-symbol-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (reverse (car (cdr symbol-list)))))
               (if (equal (car (car symbol-scope)) 'namespace)
                   (let ((namespace-name (nth 1 (car symbol-scope)))
                         (namespace-start (nth 2 (car symbol-scope)))
                         (namespace-end (nth 3 (car symbol-scope))))
                     (setcar
                      symbol-scope
                      (list 'trait class-name class-start class-end))
                     (push
                      (list 'namespace namespace-name namespace-start namespace-end)
                      symbol-scope))
                 (push
                  (list 'trait class-name class-start class-end)
                  symbol-scope))
               (setq symbol-scope (reverse symbol-scope))
               (setcar
                (cdr symbol-list)
                symbol-scope)))))))

   `(
     ast-type
     trait-declaration-statement
     ast-name
     ,(nth 1 args)
     backup-doc-comment
     ,(nth 2 args)
     class-statement-list
     ,(nth 4 args)))
 phps-mode-parser--table-translations)

;; 191 ((interface_declaration_statement) (T_INTERFACE T_STRING interface_extends_list backup_doc_comment "{" class_statement_list "}"))
(puthash
 191
 (lambda(args terminals)
   ;; Go through stacks and modify symbol name-spaces
   ;; but only for non-super-global variables.
   ;; 
   ;; Should place class scope first in scope
   ;; unless a namespace exists, in that case it should be placed second in scope
   (let ((class-name (nth 1 args))
         (class-start (car (cdr (nth 1 terminals))))
         (class-end (cdr (cdr (nth 1 terminals)))))

     ;; Add class scope to all functions in class
     (when phps-mode-parser-sdt-symbol-imenu--stack
       (dolist (item phps-mode-parser-sdt-symbol-imenu--stack)
         (let ((item-start (nth 2 item))
               (item-end (nth 3 item)))
           (when (and
                  (>= item-start class-start)
                  (<= item-end class-end))
             (setcar
              item
              (list 'class class-name class-start class-end))))))

     ;; Add class to imenu stack
     (push
      (list 'class class-name class-start class-end)
      phps-mode-parser-sdt-symbol-imenu--stack)

     (when phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (reverse (car (cdr symbol-list)))))
               (if (equal (car (car symbol-scope)) 'namespace)
                   (let ((namespace-name (nth 1 (car symbol-scope)))
                         (namespace-start (nth 2 (car symbol-scope)))
                         (namespace-end (nth 3 (car symbol-scope))))
                     (setcar
                      symbol-scope
                      (list 'interface class-name class-start class-end))
                     (push
                      (list 'namespace namespace-name namespace-start namespace-end)
                      symbol-scope))
                 (push
                  (list 'interface class-name class-start class-end)
                  symbol-scope))
               (setq symbol-scope (reverse symbol-scope))
               (setcar
                (cdr symbol-list)
                symbol-scope))))))

     (when phps-mode-parser-sdt--bookkeeping-symbol-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-stack)
         (let ((symbol-name (car symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (reverse (car (cdr symbol-list)))))
               (if (equal (car (car symbol-scope)) 'namespace)
                   (let ((namespace-name (nth 1 (car symbol-scope)))
                         (namespace-start (nth 2 (car symbol-scope)))
                         (namespace-end (nth 3 (car symbol-scope))))
                     (setcar
                      symbol-scope
                      (list 'interface class-name class-start class-end))
                     (push
                      (list 'namespace namespace-name namespace-start namespace-end)
                      symbol-scope))
                 (push
                  (list 'interface class-name class-start class-end)
                  symbol-scope))
               (setq symbol-scope (reverse symbol-scope))
               (setcar
                (cdr symbol-list)
                symbol-scope)))))))

   `(
     ast-type
     interface-declaration-statement
     ast-name
     ,(nth 1 args)
     interface-extends-list
     ,(nth 2 args)
     ast-index
     ,(car (cdr (nth 1 terminals)))
     ast-start
     ,(car (cdr (nth 4 terminals)))
     ast-end
     ,(car (cdr (nth 6 terminals)))
     class-statement-list
     ,(nth 5 args)
     ))
 phps-mode-parser--table-translations)

;; 192 ((enum_declaration_statement) (T_ENUM T_STRING enum_backing_type implements_list backup_doc_comment "{" class_statement_list "}"))
(puthash
 192
 (lambda(args terminals)
   `(
     ast-type
     enum-declaration-statement
     ast-name
     ,(nth 1 args)
     enum-backing-type
     ,(nth 2 args)
     implements-list
     ,(nth 3 args)
     ast-index
     ,(car (cdr (nth 1 terminals)))
     ast-start
     ,(car (cdr (nth 5 terminals)))
     ast-end
     ,(car (cdr (nth 7 terminals)))
     class-statement-list
     ,(nth 6 args)
     ))
 phps-mode-parser--table-translations)

;; 193 ((enum_backing_type) (%empty))
(puthash 193 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 194 ((enum_backing_type) (":" type_expr))
(puthash 194 (lambda(args _terminals) (nth 1 args)) phps-mode-parser--table-translations)

;; 195 ((enum_case) (T_CASE backup_doc_comment identifier enum_case_expr ";"))
(puthash
 195
 (lambda(args _terminals)
   `(
     ast-type
     enum-case
     backup-doc-comment
     ,(nth 1 args)
     identifier
     ,(nth 2 args)
     enum-case-expr
     ,(nth 3 args)
     ))
 phps-mode-parser--table-translations)

;; 196 ((enum_case_expr) (%empty))
(puthash 196 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 197 ((enum_case_expr) ("=" expr))
(puthash
 197
 (lambda(args _terminals)
   `(
     ast-type
     enum-case-expr
     expr
     ,(nth 1 args)
     )
   )
 phps-mode-parser--table-translations)

;; 198 ((extends_from) (%empty))
(puthash 198 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 199 ((extends_from) (T_EXTENDS class_name))
(puthash
 199
 (lambda(args _terminals)
   `(
     ast-type
     extends-from
     class-name
     ,(nth 1 args)
     )
   )
 phps-mode-parser--table-translations)

;; 200 ((interface_extends_list) (%empty))
(puthash 200 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 201 ((interface_extends_list) (T_EXTENDS class_name_list))
(puthash
 201
 (lambda(args _terminals)
   `(
     ast-type
     interface-extends-list
     class-name-list
     ,(nth 1 args)
     )
   )
 phps-mode-parser--table-translations)

;; 202 ((implements_list) (%empty))
(puthash 202 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 203 ((implements_list) (T_IMPLEMENTS class_name_list))
(puthash
 203
 (lambda(args _terminals)
   `(
     ast-type
     implements-list
     class-name-list
     ,(nth 1 args)
     )
   )
 phps-mode-parser--table-translations)

;; 204 ((foreach_variable) (variable))
(puthash
 204
 (lambda(args terminals)
   ;; Save variable declaration in bookkeeping buffer
   (let ((variable-type (plist-get args 'ast-type)))
     (cond
      ((equal variable-type 'variable-callable-variable)
       (let* ((callable-variable (plist-get args 'callable-variable))
              (callable-variable-type (plist-get callable-variable 'ast-type)))
         (cond
          ((equal callable-variable-type 'callable-variable-simple-variable)
           (let ((callable-variable-simple-variable
                  (plist-get callable-variable 'simple-variable)))
             (let ((callable-variable-simple-variable-type
                    (plist-get
                     callable-variable-simple-variable
                     'ast-type)))
               (cond
                ((equal
                  callable-variable-simple-variable-type
                  'simple-variable-variable)
                 (let* ((variable-name
                         (plist-get
                          callable-variable-simple-variable
                          'variable))
                        (symbol-name
                         variable-name)
                        (symbol-start
                         (car (cdr terminals)))
                        (symbol-end
                         (cdr (cdr terminals)))
                        (symbol-scope
                         phps-mode-parser-sdt--bookkeeping-namespace))
                   ;; (message "declared foreach variable from terminals: %S" terminals)
                   (push
                    (list
                     symbol-name
                     symbol-scope
                     symbol-start
                     symbol-end)
                    phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack))))))))))))

   `(
     ast-type
     foreach-variable
     variable
     ,args))
 phps-mode-parser--table-translations)

;; 205 ((foreach_variable) (ampersand variable))
(puthash
 205
 (lambda(args terminals)
   ;; Save variable declaration in bookkeeping buffer
   (let* ((variable (nth 1 args))
          (variable-type (plist-get variable 'ast-type)))
     (cond
      ((equal variable-type 'variable-callable-variable)
       (let* ((callable-variable (plist-get variable 'callable-variable))
              (callable-variable-type (plist-get callable-variable 'ast-type)))
         (cond
          ((equal callable-variable-type 'callable-variable-simple-variable)
           (let ((callable-variable-simple-variable
                  (plist-get callable-variable 'simple-variable)))
             (let ((callable-variable-simple-variable-type
                    (plist-get
                     callable-variable-simple-variable
                     'ast-type)))
               (cond
                ((equal
                  callable-variable-simple-variable-type
                  'simple-variable-variable)
                 (let* ((variable-name
                         (plist-get
                          callable-variable-simple-variable
                          'variable))
                        (symbol-name
                         variable-name)
                        (symbol-start
                         (car (cdr (nth 1 terminals))))
                        (symbol-end
                         (cdr (cdr (nth 1 terminals))))
                        (symbol-scope
                         phps-mode-parser-sdt--bookkeeping-namespace))
                   ;; (message "declared foreach variable from terminals: %S" (nth 1 terminals))
                   (push
                    (list
                     symbol-name
                     symbol-scope
                     symbol-start
                     symbol-end)
                    phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack))))))))))))

   `(
     ast-type
     foreach-referenced-variable
     variable
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 206 ((foreach_variable) (T_LIST "(" array_pair_list ")"))
(puthash
 206
 (lambda(args _terminals)
   ;; TODO Declare variable here

   `(
     ast-type
     foreach-list-variable
     array-pair-list
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 207 ((foreach_variable) ("[" array_pair_list "]"))
(puthash
 207
 (lambda(args _terminals)
   ;; TODO Declare variable here

   `(
     ast-type
     foreach-variable
     array-pair-list
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 208 ((for_statement) (statement))
(puthash
 208
 (lambda(args _terminals)
   `(
     ast-type
     for-statement
     statement
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 209 ((for_statement) (":" inner_statement_list T_ENDFOR ";"))
(puthash
 209
 (lambda(args _terminals)
   `(
     ast-type
     for-statement
     inner-statement-list
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 210 ((foreach_statement) (statement))
(puthash
 210
 (lambda(args _terminals)
   `(
     ast-type
     foreach-statement
     statement
     ,args))
 phps-mode-parser--table-translations)

;; 211 ((foreach_statement) (":" inner_statement_list T_ENDFOREACH ";"))
(puthash
 211
 (lambda(args _terminals)
   `(
     ast-type
     foreach-statement
     inner-statement-list
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 212 ((declare_statement) (statement))
(puthash
 212
 (lambda(args _terminals)
   `(
     ast-type
     declare-statement
     statement
     ,args
     ))
 phps-mode-parser--table-translations)

;; 213 ((declare_statement) (":" inner_statement_list T_ENDDECLARE ";"))
(puthash
 213
 (lambda(args _terminals)
   `(
     ast-type
     declare-statement
     inner-statement-list
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 214 ((switch_case_list) ("{" case_list "}"))
(puthash 214 (lambda(args _terminals) (nth 1 args)) phps-mode-parser--table-translations)

;; 215 ((switch_case_list) ("{" ";" case_list "}"))
(puthash 214 (lambda(args _terminals) (nth 2 args)) phps-mode-parser--table-translations)

;; 216 ((switch_case_list) (":" case_list T_ENDSWITCH ";"))
(puthash 214 (lambda(args _terminals) (nth 1 args)) phps-mode-parser--table-translations)

;; 217 ((switch_case_list) (":" ";" case_list T_ENDSWITCH ";"))
(puthash 217 (lambda(args _terminals) (nth 2 args)) phps-mode-parser--table-translations)

;; 218 ((case_list) (%empty))
(puthash 218 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 219 ((case_list) (case_list T_CASE expr case_separator inner_statement_list))
(puthash
 219
 (lambda(args _terminals)
   `(
     ast-type
     expr-case-list
     expr
     ,(nth 2 args)
     inner-statement-list
     ,(nth 4 args)))
 phps-mode-parser--table-translations)

;; 220 ((case_list) (case_list T_DEFAULT case_separator inner_statement_list))
(puthash
 220
 (lambda(args _terminals)
   `(
     ast-type
     default-case-list
     inner-statement-list
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 221 ((case_separator) (":"))
(puthash 221 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 222 ((case_separator) (":"))
(puthash 222 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 223 ((match) (T_MATCH "(" expr ")" "{" match_arm_list "}"))
(puthash
 223
 (lambda(args _terminals)
   `(
     ast-type
     match
     expr
     ,(nth 2 args)
     match-arm-list
     ,(nth 5 args)
     ))
 phps-mode-parser--table-translations)

;; 224 ((match_arm_list) (%empty))
(puthash 224 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 225 ((match_arm_list) (non_empty_match_arm_list possible_comma))
(puthash 225 (lambda(args _terminals) (nth 0 args)) phps-mode-parser--table-translations)

;; 226 ((non_empty_match_arm_list) (match_arm))
(puthash 226 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 227 ((non_empty_match_arm_list) (non_empty_match_arm_list "," match_arm))
(puthash
 227
 (lambda(args _terminals)
   (append (nth 0 args) (list (nth 2 args))))
 phps-mode-parser--table-translations)

;; 228 ((match_arm) (match_arm_cond_list possible_comma T_DOUBLE_ARROW expr))
(puthash
 228
 (lambda(args _terminals)
   `(
     ast-type
     cond-match-arm
     match-arm-cond-list
     ,(nth 0 args)
     expr
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 229 ((match_arm) (T_DEFAULT possible_comma T_DOUBLE_ARROW expr))
(puthash
 229
 (lambda(args _terminals)
   `(
     ast-type
     default-match-arm
     expr
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 230 ((match_arm_cond_list) (expr))
(puthash 230 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 231 ((match_arm_cond_list) (match_arm_cond_list "," expr))
(puthash 231 (lambda(args _terminals) (append (nth 0 args) (list (nth 2 args)))) phps-mode-parser--table-translations)

;; 232 ((while_statement) (statement))
(puthash 232 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 233 ((while_statement) (":" inner_statement_list T_ENDWHILE ";"))
(puthash 233 (lambda(args _terminals) (nth 1 args)) phps-mode-parser--table-translations)

;; 234 ((if_stmt_without_else) (T_IF "(" expr ")" statement))
(puthash
 234
 (lambda(args _terminals)
   `(
     ast-type
     if-stmt-without-else
     if-condition
     ,(nth 2 args)
     statement
     ,(nth 4 args)
     ))
 phps-mode-parser--table-translations)

;; 235 ((if_stmt_without_else) (if_stmt_without_else T_ELSEIF "(" expr ")" statement))
(puthash
 235
 (lambda(args _terminals)
   `(
     ast-type
     if-elseif-stmt-without-else
     if-stmt-without-else
     ,(nth 0 args)
     elseif-condition
     ,(nth 3 args)
     statement
     ,(nth 5 args)
     ))
 phps-mode-parser--table-translations)

;; 236 ((if_stmt) (if_stmt_without_else))
(puthash 236 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 237 ((if_stmt) (if_stmt_without_else T_ELSE statement))
(puthash
 237
 (lambda(args _terminals)
   `(
     ast-type
     if-else-stmt
     if-stmt-without-else
     ,(nth 0 args)
     else-statement
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 238 ((alt_if_stmt_without_else) (T_IF "(" expr ")" ":" inner_statement_list))
(puthash
 238
 (lambda(args _terminals)
   `(
     ast-type
     alt-if-stmt-without-else
     if-condition
     ,(nth 2 args)
     inner-statement-list
     ,(nth 5 args)
     ))
 phps-mode-parser--table-translations)

;; 239 ((alt_if_stmt_without_else) (alt_if_stmt_without_else T_ELSEIF "(" expr ")" ":" inner_statement_list))
(puthash
 239
 (lambda(args _terminals)
   `(
     ast-type
     alt-if-stmt-without-else-elseif
     alt-if-stmt-without-else
     ,(nth 2 args)
     elseif-condition
     ,(nth 3 args)
     inner-statement-list
     ,(nth 6 args)
     ))
 phps-mode-parser--table-translations)

;; 240 ((alt_if_stmt) (alt_if_stmt_without_else T_ENDIF ";"))
(puthash 240 (lambda(args _terminals) (nth 0 args)) phps-mode-parser--table-translations)

;; 241 ((alt_if_stmt) (alt_if_stmt_without_else T_ELSE ":" inner_statement_list T_ENDIF ";"))
(puthash
 241
 (lambda(args _terminals)
   `(
     ast-type
     alt-if-stmt-else
     alt-if-stmt-without-else
     ,(nth 0 args)
     inner-statement-list
     ,(nth 3 args)
     ))
 phps-mode-parser--table-translations)

;; 242 ((parameter_list) (non_empty_parameter_list possible_comma))
(puthash 242 (lambda(args _terminals) (nth 0 args)) phps-mode-parser--table-translations)

;; 243 ((parameter_list) (%empty))
(puthash 243 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 244 ((non_empty_parameter_list) (attributed_parameter))
(puthash 244 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 245 ((non_empty_parameter_list) (non_empty_parameter_list "," attributed_parameter))
(puthash 245 (lambda(args _terminals) (append (nth 0 args) (list (nth 2 args)))) phps-mode-parser--table-translations)

;; 246 ((attributed_parameter) (attributes parameter))
(puthash
 246
 (lambda(args _terminals)
   `(
     ast-type
     attributed-parameter
     attributes
     ,(nth 0 args)
     parameter
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 247 ((attributed_parameter) (parameter))
(puthash
 247
 (lambda(args _terminals)
   `(
     ast-type
     attributed-parameter
     parameter
     ,args
     ))
 phps-mode-parser--table-translations)

;; 248 ((optional_property_modifiers) (%empty))
(puthash 248 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 249 ((optional_property_modifiers) (optional_property_modifiers property_modifier))
(puthash 249 (lambda(args _terminals) (append (nth 0 args) (list (nth 1 args)))) phps-mode-parser--table-translations)

;; 250 ((property_modifier) (T_PUBLIC))
(puthash 250 (lambda(_args _terminals) 'T_PUBLIC) phps-mode-parser--table-translations)

;; 251 ((property_modifier) (T_PROTECTED))
(puthash 251 (lambda(_args _terminals) 'T_PROTECTED) phps-mode-parser--table-translations)

;; 252 ((property_modifier) (T_PROTECTED))
(puthash 252 (lambda(_args _terminals) 'T_PRIVATE) phps-mode-parser--table-translations)

;; 253 ((property_modifier) (T_PROTECTED))
(puthash 253 (lambda(_args _terminals) 'T_PROTECTED) phps-mode-parser--table-translations)

;; 254 ((parameter) (optional_property_modifiers optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment))
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
     ,(if (nth 2 args) t nil)
     is-variadic
     ,(if (nth 3 args) t nil)
     ast-name
     ,(nth 4 args)
     ast-start
     ,(car (cdr (nth 4 terminals)))
     ast-end
     ,(cdr (cdr (nth 4 terminals)))
     ))
 phps-mode-parser--table-translations)

;; 255 ((parameter) (optional_property_modifiers optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment "=" expr))
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
     ,(if (nth 2 args) t nil)
     is-variadic
     ,(if (nth 3 args) t nil)
     ast-name
     ,(nth 4 args)
     ast-start
     ,(car (cdr (nth 4 terminals)))
     ast-end
     ,(cdr (cdr (nth 4 terminals)))
     backup-doc-comment
     ,(nth 5 args)
     default-value
     ,(nth 7 args)
     ))
 phps-mode-parser--table-translations)

;; 256 ((optional_type_without_static) (%empty))
(puthash 256 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 257 ((optional_type_without_static) (type_expr_without_static))
(puthash 257 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 258 ((type_expr) (type))
(puthash
 258
 (lambda(args _terminals)
   `(
     ast-type
     plain-type
     type
     ,args
     ))
 phps-mode-parser--table-translations)

;; 259 ((type_expr) ("?" type))
(puthash
 259
 (lambda(args _terminals)
   `(
     ast-type
     nullable-type
     type
     ,args
     ))
 phps-mode-parser--table-translations)

;; 260 ((type_expr) (union_type))
(puthash
 260
 (lambda(args _terminals)
   `(
     ast-type
     union-type
     type
     ,args
     ))
 phps-mode-parser--table-translations)

;; 261 ((type_expr) (intersection_type))
(puthash
 261
 (lambda(args _terminals)
   `(
     ast-type
     intersection-type
     type
     ,args
     ))
 phps-mode-parser--table-translations)

;; 262 ((type) (type_without_static))
(puthash 262 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 263 ((type) (T_STATIC))
(puthash 263 (lambda(_args _terminals) 'T_STATIC) phps-mode-parser--table-translations)

;; 264 ((union_type) (type "|" type))
(puthash 264 (lambda(args _terminals) (list (nth 0 args) (nth 2 args))) phps-mode-parser--table-translations)

;; 265 ((union_type) (union_type "|" type))
(puthash 265 (lambda(args _terminals) (append (nth 0 args) (list (nth 2 args)))) phps-mode-parser--table-translations)

;; 266 ((intersection_type) (type T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG type))
(puthash 266 (lambda(args _terminals) (list (nth 0 args) (nth 2 args))) phps-mode-parser--table-translations)

;; 267 ((intersection_type) (intersection_type T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG type))
(puthash 267 (lambda(args _terminals) (append (nth 0 args) (list (nth 1 args)))) phps-mode-parser--table-translations)

;; 268 ((type_expr_without_static) (type_without_static))
(puthash 268 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 269 ((type_expr_without_static) ("?" type_without_static))
(puthash 269 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 270 ((type_expr_without_static) (union_type_without_static))
(puthash 270 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 271 ((type_expr_without_static) (intersection_type_without_static))
(puthash 271 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 272 ((type_without_static) (T_ARRAY))
(puthash 272 (lambda(_args _terminals) 'T_ARRAY) phps-mode-parser--table-translations)

;; 273 ((type_without_static) (T_CALLABLE))
(puthash 273 (lambda(_args _terminals) 'T_CALLABLE) phps-mode-parser--table-translations)

;; 274 ((type_without_static) (name))
(puthash 273 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 275 ((union_type_without_static) (type_without_static "|" type_without_static))
(puthash 275 (lambda(args _terminals) (list (nth 0 args) (nth 2 args))) phps-mode-parser--table-translations)

;; 276 ((union_type_without_static) (union_type_without_static "|" type_without_static))
(puthash 276 (lambda(args _terminals) (append (nth 0 args) (list (nth 2 args)))) phps-mode-parser--table-translations)

;; 277 ((intersection_type_without_static) (type_without_static T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG type_without_static))
(puthash 277 (lambda(args _terminals) (list (nth 0 args) (nth 2 args))) phps-mode-parser--table-translations)

;; 278 ((intersection_type_without_static) (intersection_type_without_static T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG type_without_static))
(puthash 278 (lambda(args _terminals) (append (nth 0 args) (list (nth 2 args)))) phps-mode-parser--table-translations)

;; 279 ((return_type) (%empty))
(puthash 279 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 280 ((return_type) (":" type_expr))
(puthash 280 (lambda(args _terminals) (nth 1 args)) phps-mode-parser--table-translations)

;; 281 ((argument_list) ("(" ")"))
(puthash 281 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 282 ((argument_list) ("(" non_empty_argument_list possible_comma ")"))
(puthash 282 (lambda(args _terminals) (nth 1 args)) phps-mode-parser--table-translations)

;; 283 ((argument_list) ("(" T_ELLIPSIS ")"))
(puthash 283 (lambda(_args _terminals) 'T_ELLIPSIS) phps-mode-parser--table-translations)

;; 284 ((non_empty_argument_list) (argument))
(puthash 284 (lambda(args _terminals) (list (nth 0 args))) phps-mode-parser--table-translations)

;; 285 ((non_empty_argument_list) (non_empty_argument_list "," argument))
(puthash 285 (lambda(args _terminals) (append (nth 0 args) (list (nth 2 args)))) phps-mode-parser--table-translations)

;; 286 ((argument) (expr))
(puthash
 286
 (lambda(args _terminals)
   `(
     ast-type
     argument
     type
     nil
     value
     ,args
     )
   )
 phps-mode-parser--table-translations)

;; 287 ((argument) (identifier ":" expr))
(puthash
 287
 (lambda(args _terminals)
   `(
     ast-type
     argument
     type
     ,(nth 0 args)
     value
     ,(nth 2 args)
     )
   )
 phps-mode-parser--table-translations)

;; 288 ((argument) (T_ELLIPSIS expr))
(puthash
 288
 (lambda(args _terminals)
   `(
     ast-type
     ellipsis-argument
     value
     ,(nth 2 args)
     )
   )
 phps-mode-parser--table-translations)

;; 289 ((global_var_list) (global_var_list "," global_var))
(puthash 289 (lambda(args _terminals) (append (nth 0 args) (list (nth 2 args)))) phps-mode-parser--table-translations)

;; 290 ((global_var_list) (global_var))
(puthash 290 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 291 ((global_var) (simple_variable))
(puthash 291 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 292 ((static_var_list) (static_var_list "," static_var))
(puthash 292 (lambda(args _terminals) (append (nth 0 args) (list (nth 2 args)))) phps-mode-parser--table-translations)

;; 293 ((static_var_list) (static_var))
(puthash 293 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 294 ((static_var) (T_VARIABLE))
(puthash
 294
 (lambda(args terminals)
   `(
     ast-type
     variable
     ast-name
     ,args
     ast-index
     ,(car (cdr terminals))
     ast-start
     ,(car (cdr terminals))
     ast-end
     ,(cdr (cdr terminals))))
 phps-mode-parser--table-translations)

;; 295 ((static_var) (T_VARIABLE "=" expr))
(puthash
 295
 (lambda(args terminals)
   ;; TODO Should bookkeep here
   `(
     ast-type
     variable
     ast-name
     ,(nth 0 args)
     expr
     ,(nth 2 args)
     ast-index
     ,(car (cdr (nth 0 terminals)))
     ast-start
     ,(car (cdr (nth 0 terminals)))
     ast-end
     ,(cdr (cdr (nth 0 terminals)))))
 phps-mode-parser--table-translations)

;; 296 ((class_statement_list) (class_statement_list class_statement))
(puthash
 296
 (lambda(args _terminals)
   (if (car args)
       (append (car args) (cdr args))
     (cdr args)))
 phps-mode-parser--table-translations)

;; 297 ((class_statement_list) (%empty))
(puthash 297 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 298 ((attributed_class_statement) (variable_modifiers optional_type_without_static property_list ";"))
(puthash
 298
 (lambda(args terminals)
   `(
     ast-type
     property
     modifiers
     ,(nth 0 args)
     type
     ,(nth 1 args)
     subject
     ,(nth 2 args)
     ast-start
     ,(car (cdr (car (nth 2 terminals))))
     ast-end
     ,(cdr (cdr (car (nth 2 terminals))))
     ))
 phps-mode-parser--table-translations)

;; 299 ((attributed_class_statement) (method_modifiers T_CONST class_const_list ";"))
(puthash
 299
 (lambda(args terminals)
   ;; TODO Bookkeep class constants here
   `(
     ast-type
     constant
     modifiers
     ,(nth 0 args)
     subject
     ,(nth 2 args)
     ast-start
     ,(car (cdr (car (nth 2 terminals))))
     ast-end
     ,(cdr (cdr (car (nth 2 terminals))))))
 phps-mode-parser--table-translations)

;; 300 ((attributed_class_statement) (method_modifiers function returns_ref identifier backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags method_body backup_fn_flags))
(puthash
 300
 (lambda(args terminals)
   `(
     ast-type
     method
     modifiers
     ,(nth 0 args)
     returns-reference-p
     ,(not (equal (nth 2 args) nil))
     ast-name
     ,(nth 3 args)
     parameter-list
     ,(nth 6 args)
     return-type
     ,(nth 8 args)
     method-body
     ,(if (nth 10 args)
          (nth 10 args)
        nil)
     ast-index
     ,(car (cdr (nth 3 terminals)))
     ast-start
     ,(if (nth 10 args)
          (car (cdr (car (nth 10 terminals))))
        nil)
     ast-end
     ,(if (nth 10 args)
          (cdr (cdr (car (cdr (cdr (nth 10 terminals))))))
        nil)))
 phps-mode-parser--table-translations)

;; 301 ((attributed_class_statement) (enum_case))
(puthash
 301
 (lambda(args _terminals)
   `(
     ast-type
     class-enum
     enum-case
     ,args
     )
   )
 phps-mode-parser--table-translations)

;; 302 ((class_statement) (attributed_class_statement))
(puthash
 302
 (lambda(args _terminals)
   (let* ((attributed-class-statement
           args)
          (attributed-class-statement-type
           (plist-get attributed-class-statement 'ast-type)))
     (cond
      ;; Property
      ((equal attributed-class-statement-type 'property)
       (let ((property-list
              (plist-get attributed-class-statement 'subject))
             (is-static))
         (when-let (property-modifiers
                    (plist-get attributed-class-statement 'modifiers))
           (dolist (modifier property-modifiers)
             (when (equal modifier 'static)
               (setq is-static t))))
         (dolist (property property-list)
           (let ((property-type
                  (plist-get property 'ast-type)))
             (cond
              ((or
                (equal property-type 'property-variable)
                (equal property-type 'property-assigned-variable))
               (let ((symbol-name
                      (plist-get property 'variable))
                     (symbol-start
                      (plist-get property 'ast-start))
                     (symbol-end
                      (plist-get property 'ast-end))
                     (symbol-scope
                      phps-mode-parser-sdt--bookkeeping-namespace))
                 (when is-static
                   (push (list 'static) symbol-scope))
                 (push
                  (list
                   symbol-name
                   symbol-scope
                   symbol-start
                   symbol-end)
                  phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
                 (push
                  (list
                   symbol-name
                   symbol-scope
                   symbol-start
                   symbol-end)
                  phps-mode-parser-sdt--bookkeeping-symbol-stack))))))))

      ;; Method Declaration
      ((equal attributed-class-statement-type 'method)
       ;; Go through stacks and modify symbol namespaces
       ;; - add function scope but only for:
       ;;     - non-super-global variables
       ;;     - symbols defined inside function limits
       (let* ((function-name
              (plist-get
               attributed-class-statement
               'ast-name))
              (function-name-downcased
               (downcase function-name))
             (function-start
              (plist-get
               attributed-class-statement
               'ast-start))
             (function-end
              (plist-get
               attributed-class-statement
               'ast-end))
             (function-parameter-list
              (plist-get
               attributed-class-statement
               'parameter-list))
             (is-static-p)
             (is-contructor-p
              (string=
               function-name-downcased
               "__construct")))

         ;; Is static method?
         (when-let (method-modifiers
                    (plist-get
                     attributed-class-statement
                     'modifiers))
           (dolist (method-modifier method-modifiers)
             (when (equal method-modifier 'static)
               (setq is-static-p t))))

         (when (and function-start function-end)

           ;; Add $this symbol in scope unless method is static
           (unless is-static-p
             (push
              (list
               "$this"
               phps-mode-parser-sdt--bookkeeping-namespace
               (plist-get attributed-class-statement 'ast-start)
               (plist-get attributed-class-statement 'ast-end))
              phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack))

           ;; Add function scope to symbol assignments
           (when phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack
             (dolist (
                      symbol-list
                      phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
               (let ((symbol-name (car symbol-list))
                     (symbol-start (nth 2 symbol-list)))
                 (unless (or
                          (gethash
                           symbol-name
                           phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
                          (< symbol-start function-start)
                          (> symbol-start function-end))
                   (let ((symbol-scope (car (cdr symbol-list))))
                     (push
                      (list 'function function-name function-start function-end)
                      symbol-scope)
                     (setcar
                      (cdr symbol-list)
                      symbol-scope))))))

           ;; Add function scope to symbol reads
           (when phps-mode-parser-sdt--bookkeeping-symbol-stack
             (dolist (
                      symbol-list
                      phps-mode-parser-sdt--bookkeeping-symbol-stack)
               (let ((symbol-name (nth 0 symbol-list))
                     (symbol-start (nth 2 symbol-list)))
                 (unless (or
                          (gethash
                           symbol-name
                           phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
                          (< symbol-start function-start)
                          (> symbol-start function-end))
                   (let ((symbol-scope (car (cdr symbol-list))))
                     (push
                      (list 'function function-name function-start function-end)
                      symbol-scope)
                     (setcar
                      (cdr symbol-list)
                      symbol-scope)))))))

         ;; Create symbol assignments out of method parameters
         (let ((symbol-scope
                phps-mode-parser-sdt--bookkeeping-namespace))
           (push
            (list 'function function-name function-start function-end)
            symbol-scope)
           (dolist (parameter function-parameter-list)
             (let ((parameter-ast-type
                    (plist-get parameter 'ast-type)))
               (cond
                ((equal parameter-ast-type 'attributed-parameter)
                 (let* ((attributed-parameter
                         (plist-get
                          parameter
                          'parameter))
                        (attributed-parameter-name
                         (plist-get attributed-parameter 'ast-name))
                        (attributed-parameter-visibility
                         (plist-get attributed-parameter 'visibility))
                        (symbol-name
                         attributed-parameter-name)
                        (symbol-start
                         (plist-get attributed-parameter 'ast-start))
                        (symbol-end
                         (plist-get attributed-parameter 'ast-end)))

                   (when (and
                          is-contructor-p
                          attributed-parameter-visibility)
                     ;; Declare class properties here
                     (push
                      (list
                       symbol-name
                       phps-mode-parser-sdt--bookkeeping-namespace
                       symbol-start
                       symbol-end)
                      phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack))

                   (push
                    (list
                     symbol-name
                     symbol-scope
                     symbol-start
                     symbol-end)
                    phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
                   (push
                    (list
                     symbol-name
                     symbol-scope
                     symbol-start
                     symbol-end)
                    phps-mode-parser-sdt--bookkeeping-symbol-stack)))))))))))

   `(
     ast-type
     class-statement
     attributed-class-statement
     ,args))
 phps-mode-parser--table-translations)

;; 303 ((class_statement) (attributes attributed_class_statement))
(puthash
 303
 (lambda(args _terminals)
   (let* ((attributed-class-statement
           (nth 1 args))
          (attributed-class-statement-type
           (plist-get attributed-class-statement 'ast-type)))
     (cond
      ((equal attributed-class-statement-type 'property)
       (let ((property-list
              (plist-get attributed-class-statement 'subject)))
         (dolist (property property-list)
           (let ((property-type
                  (plist-get property 'ast-type)))
             (cond
              ((equal property-type 'property-variable)
               (let ((symbol-name
                      (plist-get property 'variable))
                     (symbol-start
                      (plist-get property 'ast-start))
                     (symbol-end
                      (plist-get property 'ast-end))
                     (symbol-scope
                      phps-mode-parser-sdt--bookkeeping-namespace))
                 (push
                  (list
                   symbol-name
                   symbol-scope
                   symbol-start
                   symbol-end)
                  phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack))))))))))

   `(
     ast-type
     class-statement
     attributes
     ,(nth 0 args)
     attributed-class-statement
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 304 ((class_statement) (T_USE class_name_list trait_adaptations))
(puthash
 304
 (lambda(args _terminals)
   `(
     ast-type
     use-class-statement
     class-name-list
     ,(nth 1 args)
     trait-adaptations
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 305 ((class_name_list) (class_name))
(puthash 305 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 306 ((class_name_list) (class_name_list "," class_name))
(puthash 306 (lambda(args _terminals) (append (nth 0 args) (nth 2 args))) phps-mode-parser--table-translations)

;; 307 ((trait_adaptations) (";"))
(puthash 307 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 308 ("{" "}"))
(puthash 308 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 309 ((trait_adaptations) ("{" trait_adaptation_list "}"))
(puthash 309 (lambda(args _terminals) (nth 1 args)) phps-mode-parser--table-translations)

;; 310 ((trait_adaptation_list) (trait_adaptation))
(puthash 310 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 311 ((trait_adaptation_list) (trait_adaptation_list trait_adaptation))
(puthash 311 (lambda(args _terminals) (append (nth 0 args) (nth 1 args))) phps-mode-parser--table-translations)

;; 312 ((trait_adaptation) (trait_precedence ";"))
(puthash 312 (lambda(args _terminals) (nth 0 args)) phps-mode-parser--table-translations)

;; 313 ((trait_adaptation) (trait_alias ";"))
(puthash
 313
 (lambda(args _terminals) (nth 0 args))
 phps-mode-parser--table-translations)

;; 314 ((trait_precedence) (absolute_trait_method_reference T_INSTEADOF class_name_list))
(puthash
 314
 (lambda(args _terminals)
   `(
     ast-type
     trait-precendence
     absolute-trait-method-reference
     ,(nth 0 args)
     instead-of
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 315 ((trait_alias) (trait_method_reference T_AS T_STRING))
(puthash
 315
 (lambda(args _terminals)
   `(
     ast-type
     trait-alias
     trait-method-reference
     ,(nth 0 args)
     as
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 316 ((trait_alias) (trait_method_reference T_AS reserved_non_modifiers))
(puthash
 316
 (lambda(args _terminals)
   `(
     ast-type
     trait-non-modifier
     trait-method-reference
     ,(nth 0 args)
     as
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 317 ((trait_alias) (trait_method_reference T_AS member_modifier identifier))
(puthash
 317
 (lambda(args _terminals)
   `(
     ast-type
     trait-alias-modifier
     trait-method-reference
     ,(nth 0 args)
     as
     ,(nth 2 args)
     identifier
     ,(nth 3 args)
     ))
 phps-mode-parser--table-translations)

;; 318 ((trait_alias) (trait_method_reference T_AS member_modifier))
(puthash
 318
 (lambda(args _terminals)
   `(
     ast-type
     trait-modifier
     trait-method-reference
     ,(nth 0 args)
     as
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 319 ((trait_method_reference) (identifier))
(puthash
 319
 (lambda(args _terminals)
   `(
     ast-type
     trait-method-reference-identifier
     identifier
     ,args))
 phps-mode-parser--table-translations)

;; 320 ((trait_method_reference) (absolute_trait_method_reference))
(puthash
 320
 (lambda(args _terminals)
   `(
     ast-type
     trait-method-reference-absolute
     absolute-trait-method-reference
     ,args)
   )
 phps-mode-parser--table-translations)

;; 321 ((absolute_trait_method_reference) (class_name T_PAAMAYIM_NEKUDOTAYIM identifier))
(puthash
 321
 (lambda(args _terminals)
   `(
     ast-type
     absolute-trait-method-reference
     class-name
     ,(nth 0 args)
     member
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 322 ((method_body) (";"))
(puthash 322 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 323 ((method_body) ("{" inner_statement_list "}"))
(puthash 323 (lambda(args _terminals) (nth 1 args)) phps-mode-parser--table-translations)

;; 324 ((variable_modifiers) (non_empty_member_modifiers))
(puthash 324 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 325 ((variable_modifiers) (T_VAR))
(puthash 325 (lambda(_args _terminals) '(public)) phps-mode-parser--table-translations)

;; 326 ((method_modifiers) (%empty))
(puthash 326 (lambda(_args _terminals) nil) phps-mode-parser--table-translations)

;; 327 ((method_modifiers) (non_empty_member_modifiers))
(puthash 327 (lambda(args _terminals) args) phps-mode-parser--table-translations)

;; 328 ((non_empty_member_modifiers) (member_modifier))
(puthash 328 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 329 ((non_empty_member_modifiers) (non_empty_member_modifiers member_modifier))
(puthash 329 (lambda(args _terminals) (append (nth 0 args) (list (nth 1 args)))) phps-mode-parser--table-translations)

;; 330 ((member_modifier) (T_PUBLIC))
(puthash 330 (lambda(_args _terminals) 'public) phps-mode-parser--table-translations)

;; 331 ((member_modifier) (T_PROTECTED))
(puthash 331 (lambda(_args _terminals) 'protected) phps-mode-parser--table-translations)

;; 332 ((member_modifier) (T_PRIVATE))
(puthash 332 (lambda(_args _terminals) 'private) phps-mode-parser--table-translations)

;; 333 ((member_modifier) (T_STATIC))
(puthash 333 (lambda(_args _terminals) 'static) phps-mode-parser--table-translations)

;; 334 ((member_modifier) (T_ABSTRACT))
(puthash 334 (lambda(_args _terminals) 'abstract) phps-mode-parser--table-translations)

;; 335 ((member_modifier) (T_FINAL))
(puthash 335 (lambda(_args _terminals) 'final) phps-mode-parser--table-translations)

;; 336 ((member_modifier) (T_READONLY))
(puthash 336 (lambda(_args _terminals) 'readonly) phps-mode-parser--table-translations)

;; 337 ((property_list) (property_list "," property))
(puthash 337 (lambda(args _terminals) (append (nth 0 args) (nth 2 args))) phps-mode-parser--table-translations)

;; 338 ((property_list) (property))
(puthash 338 (lambda(args _terminals) (list args)) phps-mode-parser--table-translations)

;; 339 ((property) (T_VARIABLE backup_doc_comment))
(puthash
 339
 (lambda(args terminals)
   `(
     ast-type
     property-variable
     ast-start
     ,(car (cdr (car terminals)))
     ast-index
     ,(car (cdr (car terminals)))
     ast-end
     ,(cdr (cdr (car terminals)))
     variable
     ,(nth 0 args)
     backup-doc-comment
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 340 ((property) (T_VARIABLE "=" expr backup_doc_comment))
(puthash
 340
 (lambda(args terminals)
   `(
     ast-type
     property-assigned-variable
     variable
     ,(nth 0 args)
     expr
     ,(nth 2 args)
     backup-doc-comment
     ,(nth 3 args)
     ast-index
     ,(car (cdr (nth 0 terminals)))
     ast-start
     ,(car (cdr (nth 0 terminals)))
     ast-end
     ,(cdr (cdr (nth 0 terminals)))))
 phps-mode-parser--table-translations)

;; 341 ((class_const_list) (class_const_list "," class_const_decl))
(puthash
 341
 (lambda(args _terminals)
   `(append ,(nth 1 args) ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 342 ((class_const_list) (class_const_decl))
(puthash
 342
 (lambda(args _terminals)
   (list args))
 phps-mode-parser--table-translations)

;; 343 ((class_const_decl) (identifier "=" expr backup_doc_comment))
(puthash
 343
 (lambda(args _terminals)
   `(
     ast-type
     constant-assignment
     ast-identifier
     ,(nth 0 args)
     ast-expr
     ,(nth 2 args)
     ast-backup-expression
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 344 ((const_decl) (T_STRING "=" expr backup_doc_comment))
(puthash
 344
 (lambda(args _terminals)
   `(
     ast-type
     constant-string-assignment
     ast-string
     ,(nth 0 args)
     ast-expr
     ,(nth 2 args)
     ast-backup-expression
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 345 ((echo_expr_list) (echo_expr_list "," echo_expr))
(puthash
 345
 (lambda(args _terminals)
   `(
     ast-type
     echo-expr-list
     ast-echo-expr-list
     ,(nth 0 args)
     ast-echo-expr
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 346 ((echo_expr_list) (echo_expr))
(puthash
 346
 (lambda(args _terminals)
   `(
     ast-type
     echo-expr-list
     ast-echo-expr
     ,args))
 phps-mode-parser--table-translations)

;; 347 ((echo_expr) (expr))
(puthash
 347
 (lambda(args _terminals)
   `(
     ast-type
     echo-expr
     ast-expr
     ,args))
 phps-mode-parser--table-translations)

;; 348 ((for_exprs) (%empty))
(puthash
 348
 (lambda(_args _terminals)
   `(
     ast-type
     empty-for-exprs))
 phps-mode-parser--table-translations)

;; 349 ((for_exprs) (non_empty_for_exprs))
(puthash
 349
 (lambda(args _terminals)
   `(
     ast-type
     non-empty-for-exprs
     ast-for-expr
     ,args
     ))
 phps-mode-parser--table-translations)

;; 350 ((non_empty_for_exprs) (non_empty_for_exprs "," expr))
(puthash
 350
 (lambda(args _terminals)
   `(
     ast-type
     non-empty-for-exprs
     ast-for-expr
     ,(nth 0 args)
     ast-expr
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 351 ((non_empty_for_exprs) (expr))
(puthash
 351
 (lambda(args _terminals)
   `(
     ast-type
     non-empty-for-exprs
     ast-expr
     ,args
     ))
 phps-mode-parser--table-translations)

;; 352 ((anonymous_class) (T_CLASS ctor_arguments extends_from implements_list backup_doc_comment "{" class_statement_list "}"))
(puthash
 352
 (lambda(args _terminals)
   `(
     ast-type
     anonymous-class
     ctor-arguments
     ,(nth 1 args)
     extends-from
     ,(nth 2 args)
     implements-list
     ,(nth 3 args)
     backup-doc-comment
     ,(nth 4 args)
     class-statement-list
     ,(nth 6 args)
     ))
 phps-mode-parser--table-translations)

;; 353 ((new_expr) (T_NEW class_name_reference ctor_arguments))
(puthash
 353
 (lambda(args _terminals)
   `(
     ast-type
     new-expr-class
     class-name
     ,(nth 1 args)
     ctor-arguments
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 354 ((new_expr) (T_NEW anonymous_class))
(puthash
 354
 (lambda(args _terminals)
   `(
     ast-type
     new-expr-anonymous-class
     anonymous-class
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 355 ((new_expr) (T_NEW attributes anonymous_class))
(puthash
 355
 (lambda(args _terminals)
   `(
     ast-type
     new-expr-attributed-anonymous-class
     attributes
     ,(nth 1 args)
     anonymous-class
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 356 ((expr) (variable))
(puthash
 356
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable
     variable
     ,args))
 phps-mode-parser--table-translations)

;; 357 ((expr) (T_LIST "(" array_pair_list ")" "=" expr))
(puthash
 357
 (lambda(args _terminals)
   `(
     ast-type
     expr-list
     ,(nth 2 args)
     expr
     ,(nth 4 args)
     ))
 phps-mode-parser--table-translations)

;; 358 ((expr) ("[" array_pair_list "]" "=" expr))
(puthash
 358
 (lambda(args _terminals)
   (let ((array-pair-list (nth 1 args)))
     (dolist (array-item array-pair-list)
       (let ((array-item-type (plist-get array-item 'ast-type)))
         (cond
          ((equal array-item-type 'array-pair-expr)
           (let* ((array-item-expr (plist-get array-item 'expr))
                  (array-item-expr-type (plist-get array-item-expr 'ast-type)))
             (cond
              ((equal array-item-expr-type 'expr-variable)
               (let* ((expr-variable (plist-get array-item-expr 'variable))
                      (expr-variable-type (plist-get expr-variable 'ast-type)))
                 (cond
                  ((equal expr-variable-type 'variable-callable-variable)
                   (let* ((callable-variable (plist-get expr-variable 'callable-variable))
                          (callable-variable-type (plist-get callable-variable 'ast-type)))
                     (cond
                      ((equal callable-variable-type 'callable-variable-simple-variable)
                       (let* ((callable-simple-variable
                               (plist-get callable-variable 'simple-variable))
                              (variable-name
                               (plist-get callable-simple-variable 'variable))
                              (variable-start
                               (plist-get callable-simple-variable 'ast-start))
                              (variable-end
                               (plist-get callable-simple-variable 'ast-end)))
                         (push
                          (list
                           variable-name
                           phps-mode-parser-sdt--bookkeeping-namespace
                           variable-start
                           variable-end)
                          phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack))))))))))))))))
   `(
     ast-type
     expr-list
     array-pair-list
     ,(nth 1 args)
     expr
     ,(nth 4 args)
     ))
 phps-mode-parser--table-translations)

;; 359 ((expr) (variable "=" expr))
(puthash
 359
 (lambda(args terminals)
   ;; Save variable declaration in bookkeeping buffer
   (let ((variable-type (plist-get (nth 0 args) 'ast-type)))
     (cond
      ((equal variable-type 'variable-callable-variable)
       (let* ((callable-variable (plist-get (nth 0 args) 'callable-variable))
              (callable-variable-type (plist-get callable-variable 'ast-type)))
         (cond
          ((equal callable-variable-type 'callable-variable-simple-variable)
           (let ((callable-variable-simple-variable (plist-get callable-variable 'simple-variable)))
             (let ((callable-variable-simple-variable-type
                    (plist-get
                     callable-variable-simple-variable
                     'ast-type)))
               (cond
                ((equal
                  callable-variable-simple-variable-type
                  'simple-variable-variable)
                 (let* ((variable-name
                         (plist-get
                          callable-variable-simple-variable
                          'variable))
                        (symbol-name
                          variable-name)
                        (symbol-start
                         (car (cdr (car terminals))))
                        (symbol-end
                         (cdr (cdr (car terminals))))
                        (symbol-scope
                         phps-mode-parser-sdt--bookkeeping-namespace))

                   ;; (message "declared variable from terminals: %S" terminals)
                   (push
                    (list
                     symbol-name
                     symbol-scope
                     symbol-start
                     symbol-end)
                    phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack))))))))))))

   `(
     ast-type
     expr-assign-variable-by-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 2 args)
     ast-index
     ,(car (cdr (nth 0 terminals)))
     ast-start
     ,(car (cdr (nth 0 terminals)))
     ast-end
     ,(cdr (cdr (nth 0 terminals)))))
 phps-mode-parser--table-translations)

;; 360 ((expr) (variable "=" ampersand variable))
(puthash
 360
 (lambda(args terminals)
   `(
     ast-type
     expr-assign-variable-by-reference
     variable
     ,(nth 0 args)
     reference
     ,(nth 2 args)
     ast-index
     ,(car (cdr (nth 0 terminals)))
     ast-start
     ,(car (cdr (nth 0 terminals)))
     ast-end
     ,(cdr (cdr (nth 0 terminals)))
     ))
 phps-mode-parser--table-translations)

;; 361 ((expr) (T_CLONE expr))
(puthash
 361
 (lambda(args _terminals)
   `(
     ast-type
     expr-clone
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 362 ((expr) (variable T_PLUS_EQUAL expr))
(puthash
 362
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-plus-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 363 ((expr) (variable T_MINUS_EQUAL expr))
(puthash
 363
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-minus-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 364 ((expr) (variable T_MUL_EQUAL expr))
(puthash
 364
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-mul-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 365 ((expr) (variable T_POW_EQUAL expr))
(puthash
 365
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-pow-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 366 ((expr) (variable T_DIV_EQUAL expr))
(puthash
 366
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-div-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 367 ((expr) (variable T_CONCAT_EQUAL expr))
(puthash
 367
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-concat-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 368 ((expr) (variable T_MOD_EQUAL expr))
(puthash
 368
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-mod-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 369 ((expr) (variable T_AND_EQUAL expr))
(puthash
 369
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-and-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 370 ((expr) (variable T_OR_EQUAL expr))
(puthash
 370
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-or-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 371 ((expr) (variable T_XOR_EQUAL expr))
(puthash
 371
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-xor-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 372 ((expr) (variable T_SL_EQUAL expr))
(puthash
 372
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-sl-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 373 ((expr) (variable T_SR_EQUAL expr))
(puthash
 373
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-sr-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 374 ((expr) (variable T_COALESCE_EQUAL expr))
(puthash
 374
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-coalesce-equal-expr
     variable
     ,(nth 0 args)
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 375 ((expr) (variable T_INC))
(puthash
 375
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-inc
     variable
     ,(nth 0 args)
     ))
 phps-mode-parser--table-translations)

;; 376 ((expr) (T_INC variable))
(puthash
 376
 (lambda(args _terminals)
   `(
     ast-type
     expr-inc-variable
     variable
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 377 ((expr) (variable T_DEC))
(puthash
 377
 (lambda(args _terminals)
   `(
     ast-type
     expr-variable-dec
     variable
     ,(nth 0 args)
     ))
 phps-mode-parser--table-translations)

;; 378 ((expr) (T_DEC variable))
(puthash
 378
 (lambda(args _terminals)
   `(
     ast-type
     expr-dev-variable
     variable
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 379 ((expr) (expr T_BOOLEAN_OR expr))
(puthash
 379
 (lambda(args _terminals)
   `(
     ast-type
     expr-boolean-or-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 380 ((expr) (expr T_BOOLEAN_AND expr))
(puthash
 380
 (lambda(args _terminals)
   `(
     ast-type
     expr-boolean-and-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 381 ((expr) (expr T_LOGICAL_OR expr))
(puthash
 381
 (lambda(args _terminals)
   `(
     ast-type
     expr-logical-or-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 382 ((expr) (expr T_LOGICAL_AND expr))
(puthash
 382
 (lambda(args _terminals)
   `(
     ast-type
     expr-logical-and-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 383 ((expr) (expr T_LOGICAL_XOR expr))
(puthash
 383
 (lambda(args _terminals)
   `(
     ast-type
     expr-logical-xor-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 384 ((expr) (expr "|" expr))
(puthash
 384
 (lambda(args _terminals)
   `(
     ast-type
     expr-bitwise-or-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 385 ((expr) (expr T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG expr))
(puthash
 385
 (lambda(args _terminals)
   `(
     ast-type
     expr-bitwise-and-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 386 ((expr) (expr T_AMPERSAND_FOLLOWED_BY_VAR_OR_VARARG expr))
(puthash
 386
 (lambda(args _terminals)
   `(
     ast-type
     expr-bitwise-and-variable
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 387 ((expr) (expr "^" expr))
(puthash
 387
 (lambda(args _terminals)
   `(
     ast-type
     expr-bitwise-xor-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 388 ((expr) (expr "." expr))
(puthash
 388
 (lambda(args _terminals)
   `(
     ast-type
     expr-string-concatenation-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 389 ((expr) (expr "+" expr))
(puthash
 389
 (lambda(args _terminals)
   `(
     ast-type
     expr-arithmetic-addition-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 390 ((expr) (expr "-" expr))
(puthash
 390
 (lambda(args _terminals)
   `(
     ast-type
     expr-arithmetic-substraction-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 391 ((expr) (expr "*" expr))
(puthash
 391
 (lambda(args _terminals)
   `(
     ast-type
     expr-arithmetic-multiplication-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 392 ((expr) (expr T_POW expr))
(puthash
 392
 (lambda(args _terminals)
   `(
     ast-type
     expr-arithmetic-exponentiation-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 393 ((expr) (expr "/" expr))
(puthash
 393
 (lambda(args _terminals)
   `(
     ast-type
     expr-arithmetic-division-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 394 ((expr) (expr "%" expr))
(puthash
 394
 (lambda(args _terminals)
   `(
     ast-type
     expr-arithmetic-modulo-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 395 ((expr) (expr T_SL expr))
(puthash
 395
 (lambda(args _terminals)
   `(
     ast-type
     expr-bitwise-shift-left-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 396 ((expr) (expr T_SR expr))
(puthash
 396
 (lambda(args _terminals)
   `(
     ast-type
     expr-bitwise-shift-right-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 397 ((expr) ("+" expr))
(puthash
 397
 (lambda(args _terminals)
   `(
     ast-type
     expr-arithmetic-addition
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 398 ((expr) ("-" expr))
(puthash
 398
 (lambda(args _terminals)
   `(
     ast-type
     expr-arithmetic-subtraction
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 399 ((expr) ("!" expr))
(puthash
 399
 (lambda(args _terminals)
   `(
     ast-type
     expr-logical-not
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 400 ((expr) ("~" expr))
(puthash
 400
 (lambda(args _terminals)
   `(
     ast-type
     expr-bitwise-not
     expr
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 401 ((expr) (expr T_IS_IDENTICAL expr))
(puthash
 401
 (lambda(args _terminals)
   `(
     ast-type
     expr-is-identical-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 402 ((expr) (expr T_IS_NOT_IDENTICAL expr))
(puthash
 402
 (lambda(args _terminals)
   `(
     ast-type
     expr-is-not-identical-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 403 ((expr) (expr T_IS_EQUAL expr))
(puthash
 403
 (lambda(args _terminals)
   `(
     ast-type
     expr-comparison-is-equal-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 404 ((expr) (expr T_IS_NOT_EQUAL expr))
(puthash
 404
 (lambda(args _terminals)
   `(
     ast-type
     expr-comparison-is-not-equal-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 405 ((expr) (expr "<" expr))
(puthash
 405
 (lambda(args _terminals)
   `(
     ast-type
     expr-comparison-is-less-than-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 406 ((expr) (expr T_IS_SMALLER_OR_EQUAL expr))
(puthash
 406
 (lambda(args _terminals)
   `(
     ast-type
     expr-comparison-is-less-or-equal-than-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 407 ((expr) (expr ">" expr))
(puthash
 407
 (lambda(args _terminals)
   `(
     ast-type
     expr-comparison-is-greater-than-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 408 ((expr) (expr T_IS_GREATER_OR_EQUAL expr))
(puthash
 408
 (lambda(args _terminals)
   `(
     ast-type
     expr-comparison-is-greater-or-equal-than-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 409 ((expr) (expr T_SPACESHIP expr))
(puthash
 409
 (lambda(args _terminals)
   `(
     ast-type
     expr-comparison-spaceship-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 410 ((expr) (expr T_INSTANCEOF class_name_reference))
(puthash
 410
 (lambda(args _terminals)
   `(
     ast-type
     expr-type-instance-of
     expr-1
     ,(nth 0 args)
     class-name-reference
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 411 ((expr) ("(" expr ")"))
(puthash
 411
 (lambda(args _terminals)
   (nth 1 args))
 phps-mode-parser--table-translations)

;; 412 ((expr) (new_expr))
(puthash
 412
 (lambda(args _terminals)
   `(
     ast-type
     expr-new-expr
     expr
     ,args))
 phps-mode-parser--table-translations)

;; 413 ((expr) (expr "?" expr ":" expr))
(puthash
 413
 (lambda(args _terminals)
   `(
     ast-type
     expr-ternary-expr
     expr-test
     ,(nth 0 args)
     expr-true
     ,(nth 2 args)
     expr-false
     ,(nth 4 args)))
 phps-mode-parser--table-translations)

;; 414 ((expr) (expr "?" ":" expr))
(puthash
 414
 (lambda(args _terminals)
   `(
     ast-type
     expr-ternary-expr
     expr-test
     ,(nth 0 args)
     expr-true
     ,(nth 0 args)
     expr-false
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 415 ((expr) (expr T_COALESCE expr))
(puthash
 415
 (lambda(args _terminals)
   `(
     ast-type
     expr-coalesce-expr
     expr-1
     ,(nth 0 args)
     expr-2
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 416 ((expr) (internal_functions_in_yacc))
(puthash
 416
 (lambda(args _terminals)
   `(
     ast-type
     expr-internal-functions-in-yacc
     expr-internal-functions-in-yacc
     ,args))
 phps-mode-parser--table-translations)

;; 417 ((expr) (T_INT_CAST expr))
(puthash
 417
 (lambda(args _terminals)
   `(
     ast-type
     expr-cast-int
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 418 ((expr) (T_DOUBLE_CAST expr))
(puthash
 418
 (lambda(args _terminals)
   `(
     ast-type
     expr-cast-double
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 419 ((expr) (T_STRING_CAST expr))
(puthash
 419
 (lambda(args _terminals)
   `(
     ast-type
     expr-cast-string
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 420 ((expr) (T_ARRAY_CAST expr))
(puthash
 420
 (lambda(args _terminals)
   `(
     ast-type
     expr-cast-array
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 421 ((expr) (T_OBJECT_CAST expr))
(puthash
 421
 (lambda(args _terminals)
   `(
     ast-type
     expr-cast-object
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 422 ((expr) (T_BOOL_CAST expr))
(puthash
 422
 (lambda(args _terminals)
   `(
     ast-type
     expr-cast-bool
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 423 ((expr) (T_UNSET_CAST expr))
(puthash
 423
 (lambda(args _terminals)
   `(
     ast-type
     expr-cast-unset
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 424 ((expr) (T_EXIT exit_expr))
(puthash
 424
 (lambda(args _terminals)
   `(
     ast-type
     expr-exit
     exit-expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 425 ((expr) ("@" expr))
(puthash
 425
 (lambda(args _terminals)
   `(
     ast-type
     expr-error-suppress
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 426 ((expr) (scalar))
(puthash
 426
 (lambda(args _terminals)
   `(
     ast-type
     expr-scalar
     scalar
     ,args))
 phps-mode-parser--table-translations)

;; 427 ((expr) ("`" backticks_expr "`"))
(puthash
 427
 (lambda(args _terminals)
   `(
     ast-type
     expr-backticks-expr
     backticks-expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 428 ((expr) (T_PRINT expr))
(puthash
 428
 (lambda(args _terminals)
   `(
     ast-type
     expr-print-expr
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 429 ((expr) (T_YIELD))
(puthash
 429
 (lambda(_args _terminals)
   `(
     ast-type
     expr-yield))
 phps-mode-parser--table-translations)

;; 430 ((expr) (T_YIELD expr))
(puthash
 430
 (lambda(args _terminals)
   `(
     ast-type
     expr-yield-expr
     expr
     ,args))
 phps-mode-parser--table-translations)

;; 431 ((expr) (T_YIELD expr T_DOUBLE_ARROW expr))
(puthash
 431
 (lambda(args _terminals)
   `(
     ast-type
     expr-yield-expr-key-value
     expr-key
     ,(nth 1 args)
     expr-value
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 432 ((expr) (T_YIELD_FROM expr))
(puthash
 432
 (lambda(args _terminals)
   `(
     ast-type
     expr-yield-from-expr
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 433 ((expr) (T_THROW expr))
(puthash
 433
 (lambda(args _terminals)
   `(
     ast-type
     expr-throw-expr
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 434 ((expr) (inline_function))
(puthash
 434
 (lambda(args _terminals)
   `(
     ast-type
     expr-inline-function
     inline-function
     ,args))
 phps-mode-parser--table-translations)

;; 435 ((expr) (attributes inline_function))
(puthash
 435
 (lambda(args _terminals)
   `(
     ast-type
     expr-attributed-inline-function
     attributes
     ,(nth 0 args)
     inline-function
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 436 ((expr) (T_STATIC inline_function))
(puthash
 436
 (lambda(args _terminals)
   `(
     ast-type
     expr-static-inline-function
     inline-function
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 437 ((expr) (attributes T_STATIC inline_function))
(puthash
 437
 (lambda(args _terminals)
   `(
     ast-type
     expr-attributed-static-inline-function
     attributes
     ,(nth 0 args)
     inline-function
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 438 ((expr) (match))
(puthash
 438
 (lambda(args _terminals)
   `(
     ast-type
     expr-match
     match
     ,args))
 phps-mode-parser--table-translations)

;; 439 ((inline_function) (function returns_ref backup_doc_comment "(" parameter_list ")" lexical_vars return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags))
(puthash
 439
 (lambda(args terminals)
   (let ((function-start
          (cdr (cdr (nth 9 terminals))))
         (function-end
          (car (cdr (nth 11 terminals))))
         (namespace
          phps-mode-parser-sdt--bookkeeping-namespace)
         (parameter-list
          (nth 4 args))
         (lexical-vars
          (nth 6 args)))
     (setq
      phps-mode-parser-sdt--bookkeeping-anonymous-function-count
      (1+ phps-mode-parser-sdt--bookkeeping-anonymous-function-count))
     (push
      (list
       'anonymous-function
       phps-mode-parser-sdt--bookkeeping-anonymous-function-count)
      namespace)

     ;; Go through parameters and assign variables inside function
     (when parameter-list
       (dolist (parameter parameter-list)
         (let ((parameter-type
                (plist-get
                 parameter
                 'ast-type)))
           (cond
            ((equal parameter-type 'attributed-parameter)
             (let* ((attributed-parameter
                     (plist-get parameter 'parameter))
                    (parameter-name
                     (plist-get attributed-parameter 'ast-name))
                    (parameter-start
                     (plist-get attributed-parameter 'ast-start))
                    (parameter-end
                     (plist-get attributed-parameter 'ast-end)))
               (push
                (list
                 parameter-name
                 namespace
                 parameter-start
                 parameter-end)
                phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
               (push
                (list
                 parameter-name
                 namespace
                 parameter-start
                 parameter-end)
                phps-mode-parser-sdt--bookkeeping-symbol-stack)))))))

     ;; Go through lexical_vars and assign inside function
     (when lexical-vars
       (let ((lexical-vars-type
              (plist-get lexical-vars 'ast-type)))
         (cond
          ((equal lexical-vars-type 'lexical-vars)
           (let ((lexical-var-list
                  (plist-get lexical-vars 'lexical-var-list)))
             (dolist (lexical-var lexical-var-list)
               (let ((lexical-var-type
                      (plist-get lexical-var 'ast-type)))
                 (cond
                  ((equal lexical-var-type 'lexical-var-variable)
                   (let ((lexical-var-name
                          (plist-get lexical-var 'ast-name))
                         (lexical-var-start
                          (plist-get lexical-var 'ast-start))
                         (lexical-var-end
                          (plist-get lexical-var 'ast-end)))
                     (push
                      (list
                       lexical-var-name
                       namespace
                       lexical-var-start
                       lexical-var-end)
                      phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
                     (push
                      (list
                       lexical-var-name
                       phps-mode-parser-sdt--bookkeeping-namespace
                       lexical-var-start
                       lexical-var-end)
                      phps-mode-parser-sdt--bookkeeping-symbol-stack)))))))))))

     ;; Go through phps-mode-parser-sdt--bookkeeping-symbol-stack in scope and add namespace
     (when (and
            phps-mode-parser-sdt--bookkeeping-symbol-stack
            function-start
            function-end)
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-stack)
         (let ((symbol-name (nth 0 symbol-list))
               (symbol-start (nth 2 symbol-list)))
           (unless (or
                    (gethash
                     symbol-name
                     phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
                    (< symbol-start function-start)
                    (> symbol-start function-end))
             (let ((symbol-scope (car (cdr symbol-list))))
               (push
                (list
                 'anonymous-function
                 phps-mode-parser-sdt--bookkeeping-anonymous-function-count)
                symbol-scope)
               (setcar
                (cdr symbol-list)
                symbol-scope)))))))

   `(
     ast-type
     inline-function
     function
     ,(nth 0 args)
     returns-ref
     ,(nth 1 args)
     backup-doc-comment
     ,(nth 2 args)
     parameter-list
     ,(nth 4 args)
     lexical-vars
     ,(nth 6 args)
     return-type
     ,(nth 7 args)
     backup-fn-flags-pre
     ,(nth 8 args)
     inner-statement-list
     ,(nth 10 args)
     backup-fn-flags-post
     ,(nth 12 args)))
 phps-mode-parser--table-translations)

;; 440 ((inline_function) (fn returns_ref backup_doc_comment "(" parameter_list ")" return_type T_DOUBLE_ARROW backup_fn_flags backup_lex_pos expr backup_fn_flags))
(puthash
 440
 (lambda(args _terminals)
   (let ((namespace
          phps-mode-parser-sdt--bookkeeping-namespace)
         (parameter-list
          (nth 4 args)))
     (setq
      phps-mode-parser-sdt--bookkeeping-arrow-function-count
      (1+ phps-mode-parser-sdt--bookkeeping-arrow-function-count))
     (push
      (list
       'arrow-function
       phps-mode-parser-sdt--bookkeeping-arrow-function-count)
      namespace)

     ;; Go through symbol stack in scope and add namespace
     (when phps-mode-parser-sdt--bookkeeping-symbol-stack
       (dolist (
                symbol-list
                phps-mode-parser-sdt--bookkeeping-symbol-stack)
         (let ((symbol-name (nth 0 symbol-list)))
           (unless (gethash
                    symbol-name
                    phps-mode-parser-sdt--bookkeeping--superglobal-variable-p)
             (let ((symbol-scope (car (cdr symbol-list))))
               (push
                (list
                 'arrow-function
                 phps-mode-parser-sdt--bookkeeping-arrow-function-count)
                symbol-scope)
               (setcar
                (cdr symbol-list)
                symbol-scope))))))

     ;; Go through parameters and assign variables inside function
     (when parameter-list
       (dolist (parameter parameter-list)
         (let ((parameter-type
                (plist-get
                 parameter
                 'ast-type)))
           (cond
            ((equal parameter-type 'attributed-parameter)
             (let* ((attributed-parameter
                     (plist-get parameter 'parameter))
                    (parameter-name
                     (plist-get attributed-parameter 'ast-name))
                    (parameter-start
                     (plist-get attributed-parameter 'ast-start))
                    (parameter-end
                     (plist-get attributed-parameter 'ast-end)))
               (push
                (list
                 parameter-name
                 namespace
                 parameter-start
                 parameter-end)
                phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack)
               (push
                (list
                 parameter-name
                 namespace
                 parameter-start
                 parameter-end)
                phps-mode-parser-sdt--bookkeeping-symbol-stack))))))))
   `(
     ast-type
     inline-fn
     returns-ref
     ,(nth 1 args)
     backup-doc-comment
     ,(nth 2 args)
     parameter-list
     ,(nth 4 args)
     return-type
     ,(nth 6 args)
     backup-fn-flags-pre
     ,(nth 8 args)
     backup-lex-pos
     ,(nth 9 args)
     expr
     ,(nth 10 args)
     backup-fn-flags-post
     ,(nth 11 args)))
 phps-mode-parser--table-translations)

;; 441 ((fn) (T_FN))
(puthash
 441
 (lambda(_args _terminals)
   `(
     ast-type
     fn))
 phps-mode-parser--table-translations)

;; 442 ((function) (T_FUNCTION))
(puthash
 442
 (lambda(_args _terminals)
   `(
     ast-type
     function))
 phps-mode-parser--table-translations)

;; 443 ((backup_doc_comment) (%empty))
(puthash
 443
 (lambda(_args _terminals) nil)
 phps-mode-parser--table-translations)

;; 444 ((backup_fn_flags) (%empty))
(puthash
 444
 (lambda(_args _terminals) nil)
 phps-mode-parser--table-translations)

;; 445 ((backup_lex_pos) (%empty))
(puthash
 445
 (lambda(_args _terminals) nil)
 phps-mode-parser--table-translations)

;; 446 ((returns_ref) (%empty))
(puthash
 446
 (lambda(_args _terminals) nil)
 phps-mode-parser--table-translations)

;; 447 ((returns_ref) (ampersand))
(puthash
 447
 (lambda(_args _terminals) t)
 phps-mode-parser--table-translations)

;; 448 ((lexical_vars) (%empty))
(puthash
 448
 (lambda(_args _terminals) nil)
 phps-mode-parser--table-translations)

;; 449 ((lexical_vars) (T_USE "(" lexical_var_list possible_comma ")"))
(puthash
 449
 (lambda(args _terminals)
   `(
     ast-type
     lexical-vars
     lexical-var-list
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 450 ((lexical_var_list) (lexical_var_list "," lexical_var))
(puthash
 450
 (lambda(args _terminals)
   (append (nth 0 args) (nth 2 args)))
 phps-mode-parser--table-translations)

;; 451 ((lexical_var_list) (lexical_var))
(puthash
 451
 (lambda(args _terminals)
   (list args))
 phps-mode-parser--table-translations)

;; 452 ((lexical_var) (T_VARIABLE))
(puthash
 452
 (lambda(args terminals)
   `(
     ast-type
     lexical-var-variable
     ast-name
     ,args
     ast-start
     ,(car (cdr terminals))
     ast-end
     ,(cdr (cdr terminals))))
 phps-mode-parser--table-translations)

;; 453 ((lexical_var) (ampersand T_VARIABLE))
(puthash
 453
 (lambda(args terminals)
   `(
     ast-type
     lexical-var-reference
     ast-name
     ,(nth 1 args)
     ast-start
     ,(car (cdr (nth 1 terminals)))
     ,(cdr (cdr (nth 1 terminals)))))
 phps-mode-parser--table-translations)

;; 454 ((function_call) (name argument_list))
(puthash
 454
 (lambda(args _terminals)
   `(
     ast-type
     function-call
     name
     ,(nth 0 args)
     argument-list
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 455 ((function_call) (class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list))
(puthash
 455
 (lambda(args _terminals)
   `(
     ast-type
     function-call-class
     class-name
     ,(nth 0 args)
     member-name
     ,(nth 2 args)
     argument-list
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 456 ((function_call) (variable_class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list))
(puthash
 456
 (lambda(args _terminals)
   `(
     ast-type
     function-call-variable-class
     variable-class-name
     ,(nth 0 args)
     member-name
     ,(nth 2 args)
     argument-list
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 457 ((function_call) (callable_expr argument_list))
(puthash
 457
 (lambda(args _terminals)
   `(
     ast-type
     function-call-callable-expr
     callable-expr
     ,(nth 0 args)
     argument-list
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 458 ((class_name) (T_STATIC))
(puthash
 458
 (lambda(_args _terminals)
   `(
     ast-type
     class-name-static))
 phps-mode-parser--table-translations)

;; 459 ((class_name) (name))
(puthash
 459
 (lambda(args _terminals)
   `(
     ast-type
     class-name-name
     name
     ,args))
 phps-mode-parser--table-translations)

;; 460 ((class_name_reference) (class_name))
(puthash
 460
 (lambda(args _terminals)
   `(
     ast-type
     class-name-reference-class-name
     class-name
     ,args))
 phps-mode-parser--table-translations)

;; 461 ((class_name_reference) (new_variable))
(puthash
 461
 (lambda(args _terminals)
   `(
     ast-type
     class-name-reference-new-variable
     new-variable
     ,args))
 phps-mode-parser--table-translations)

;; 462 ((class_name_reference) ("(" expr ")"))
(puthash
 462
 (lambda(args _terminals)
   `(
     ast-type
     class-name-reference-expr
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 463 ((exit_expr) (%empty))
(puthash
 463
 (lambda(_args _terminals)
   `(
     ast-type
     exit-expr))
 phps-mode-parser--table-translations)

;; 464 ((exit_expr) ("(" optional_expr ")"))
(puthash
 464
 (lambda(args _terminals)
   `(
     ast-type
     exit-expr-optional-expr
     optional-expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 465 ((backticks_expr) (%empty))
(puthash
 465
 (lambda(_args _terminals)
   `(
     ast-type
     backticks-expr-empty))
 phps-mode-parser--table-translations)

;; 466 ((backticks_expr) (T_ENCAPSED_AND_WHITESPACE))
(puthash
 466
 (lambda(args _terminals)
   `(
     ast-type
     backticks-expr-encapsed-and-whitespace
     encapsed-and-whitespace
     ,args))
 phps-mode-parser--table-translations)

;; 467 ((backticks_expr) (encaps_list))
(puthash
 467
 (lambda(args _terminals)
   `(
     ast-type
     backticks-expr-encaps-list
     encaps-list
     ,args))
 phps-mode-parser--table-translations)

;; 468 ((ctor_arguments) (%empty))
(puthash
 468
 (lambda(_args _terminals)
   `(
     ast-type
     ctor-arguments-empty))
 phps-mode-parser--table-translations)

;; 469 ((ctor_arguments) (argument_list))
(puthash
 469
 (lambda(args _terminals)
   `(
     ast-type
     ctor-arguments-argument-list
     argument-list
     ,args))
 phps-mode-parser--table-translations)

;; 470 ((dereferenceable_scalar) (T_ARRAY "(" array_pair_list ")"))
(puthash
 470
 (lambda(args _terminals)
   `(
     ast-type
     dereferencable-scalar-array-pair
     array-pair-list
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 471 ((dereferenceable_scalar) ("[" array_pair_list "]"))
(puthash
 471
 (lambda(args _terminals)
   `(
     ast-type
     dereferencable-scalar-array-pair
     array-pair-list
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 472 ((dereferenceable_scalar) (T_CONSTANT_ENCAPSED_STRING))
(puthash
 472
 (lambda(args _terminals)
   `(
     ast-type
     dereferencable-scalar-constant-encapsed-string
     constant-encapsed-string
     ,args
     ))
 phps-mode-parser--table-translations)

;; 473 ((dereferenceable_scalar) ("\"" encaps_list "\""))
(puthash
 473
 (lambda(args _terminals)
   `(
     ast-type
     dereferencable-scalar-encaps-list
     encaps-list
     ,args
     ))
 phps-mode-parser--table-translations)

;; 474 ((scalar) (T_LNUMBER))
(puthash
 474
 (lambda(args _terminals)
   `(
     ast-type
     dereferencable-scalar-scalar
     scalar
     ,args
     ))
 phps-mode-parser--table-translations)

;; 475 ((scalar) (T_DNUMBER))
(puthash
 475
 (lambda(args _terminals)
   `(
     ast-type
     dereferencable-scalar-scalar
     scalar
     ,args
     ))
 phps-mode-parser--table-translations)

;; 476 ((scalar) (T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC))
(puthash
 476
 (lambda(args _terminals)
   `(
     ast-type
     dereferencable-scalar-scalar
     scalar
     ,(nth 2 args)
     ))
 phps-mode-parser--table-translations)

;; 477 ((scalar) (T_START_HEREDOC T_END_HEREDOC))
(puthash
 477
 (lambda(_args _terminals)
   `(
     ast-type
     dereferencable-scalar-scalar
     scalar
     ""
     ))
 phps-mode-parser--table-translations)

;; 478 ((scalar) (T_START_HEREDOC encaps_list T_END_HEREDOC))
(puthash
 478
 (lambda(args _terminals)
   `(
     ast-type
     dereferencable-scalar-scalar
     scalar
     ,(nth 1 args)
     ))
 phps-mode-parser--table-translations)

;; 479 ((scalar) (dereferenceable_scalar))
(puthash
 479
 (lambda(args _terminals)
   `(
     ast-type
     scalar-dereferencable-scalar
     dereferenceable-scalar
     ,args
     ))
 phps-mode-parser--table-translations)

;; 480 ((scalar) (constant))
(puthash
 480
 (lambda(args _terminals)
   `(
     ast-type
     scalar-constant
     constant
     ,args
     ))
 phps-mode-parser--table-translations)

;; 481 ((scalar) (class_constant))
(puthash
 481
 (lambda(args _terminals)
   `(
     ast-type
     scalar-class-constant
     class-constant
     ,args
     ))
 phps-mode-parser--table-translations)

;; 482 ((constant) (name))
(puthash
 482
 (lambda(args _terminals)
   `(
     ast-type
     constant-name
     name
     ,args
     ))
 phps-mode-parser--table-translations)

;; 483 ((constant) (T_LINE))
(puthash
 483
 (lambda(_args _terminals)
   `(
     ast-type
     constant-line
     ))
 phps-mode-parser--table-translations)

;; 484 ((constant) (T_FILE))
(puthash
 484
 (lambda(_args _terminals)
   `(
     ast-type
     constant-file
     ))
 phps-mode-parser--table-translations)

;; 485 ((constant) (T_DIR))
(puthash
 485
 (lambda(_args _terminals)
   `(
     ast-type
     constant-dir
     ))
 phps-mode-parser--table-translations)

;; 486 ((constant) (T_TRAIT_C))
(puthash
 486
 (lambda(_args _terminals)
   `(
     ast-type
     constant-trait
     ))
 phps-mode-parser--table-translations)

;; 487 ((constant) (T_METHOD_C))
(puthash
 487
 (lambda(_args _terminals)
   `(
     ast-type
     constant-method
     ))
 phps-mode-parser--table-translations)

;; 488 ((constant) (T_FUNC_C))
(puthash
 488
 (lambda(_args _terminals)
   `(
     ast-type
     constant-function
     ))
 phps-mode-parser--table-translations)

;; 489 ((constant) (T_NS_C))
(puthash
 489
 (lambda(_args _terminals)
   `(
     ast-type
     constant-namespace
     ))
 phps-mode-parser--table-translations)

;; 490 ((constant) (T_CLASS_C))
(puthash
 490
 (lambda(_args _terminals)
   `(
     ast-type
     constant-class
     ))
 phps-mode-parser--table-translations)

;; 491 ((class_constant) (class_name T_PAAMAYIM_NEKUDOTAYIM identifier))
(puthash
 491
 (lambda(args _terminals)
   `(
     ast-type
     class-constant-class-name
     class-name
     ,(nth 0 args)
     identifier
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 492 ((class_constant) (variable_class_name T_PAAMAYIM_NEKUDOTAYIM identifier))
(puthash
 492
 (lambda(args _terminals)
   `(
     ast-type
     class-constant-variable-class-name
     variable-class-name
     ,(nth 0 args)
     identifier
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 493 ((optional_expr) (%empty))
(puthash
 493
 (lambda(_args _terminals)
   `(
     ast-type
     optional-expr-empty))
 phps-mode-parser--table-translations)

;; 494 ((optional_expr) (expr))
(puthash
 494
 (lambda(args _terminals)
   `(
     ast-type
     optional-expr-not-empty
     expr
     ,args))
 phps-mode-parser--table-translations)

;; 495 ((variable_class_name) (fully_dereferenceable))
(puthash
 495
 (lambda(args _terminals)
   `(
     ast-type
     variable-class-name
     fully-dereferenceable
     ,args))
 phps-mode-parser--table-translations)

;; 496 ((fully_dereferenceable) (variable))
(puthash
 496
 (lambda(args _terminals)
   `(
     ast-type
     fully-dereferenceable-variable
     variable
     ,args))
 phps-mode-parser--table-translations)

;; 497 ((fully_dereferenceable) ("(" expr ")"))
(puthash
 497
 (lambda(args _terminals)
   `(
     ast-type
     fully-dereferenceable-expr
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 498 ((fully_dereferenceable) (dereferenceable_scalar))
(puthash
 498
 (lambda(args _terminals)
   `(
     ast-type
     fully-dereferenceable-scalar
     dereferenceable-scalar
     ,args))
 phps-mode-parser--table-translations)

;; 499 ((fully_dereferenceable) (class_constant))
(puthash
 499
 (lambda(args _terminals)
   `(
     ast-type
     fully-dereferenceable-class-constant
     class-constant
     ,args))
 phps-mode-parser--table-translations)

;; 500 ((array_object_dereferenceable) (fully_dereferenceable))
(puthash
 500
 (lambda(args _terminals)
   `(
     ast-type
     array-object-dereferenceable-fully-dereferenceable
     fully-dereferenceable
     ,args))
 phps-mode-parser--table-translations)

;; 501 ((array_object_dereferenceable) (constant))
(puthash
 501
 (lambda(args _terminals)
   `(
     ast-type
     array-object-dereferenceable-constant
     constant
     ,args))
 phps-mode-parser--table-translations)

;; 502 ((callable_expr) (callable_variable))
(puthash
 502
 (lambda(args _terminals)
   `(
     ast-type
     callable-expr-callable-variable
     callable-variable
     ,args))
 phps-mode-parser--table-translations)

;; 503 ((callable_expr) ("(" expr ")"))
(puthash
 503
 (lambda(args _terminals)
   `(
     ast-type
     callable-expr-expr
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 504 ((callable_expr) (dereferenceable_scalar))
(puthash
 504
 (lambda(args _terminals)
   `(
     ast-type
     callable-expr-dereferenceable-scalar
     ,args))
 phps-mode-parser--table-translations)

;; 505 ((callable_variable) (simple_variable))
(puthash
 505
 (lambda(args _terminals)
   `(
     ast-type
     callable-variable-simple-variable
     simple-variable
     ,args))
 phps-mode-parser--table-translations)

;; 506 ((callable_variable) (array_object_dereferenceable "[" optional_expr "]"))
(puthash
 506
 (lambda(args _terminals)
   `(
     ast-type
     callable-variable-array-object-dereferenceable-indexed-expr
     array-object-dereferenceable
     ,(nth 0 args)
     optional-expr
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 507 ((callable_variable) (array_object_dereferenceable "{" expr "}"))
(puthash
 507
 (lambda(args _terminals)
   `(
     ast-type
     callable-variable-array-object-dereferenceable-indexed-expr
     array-object-dereferenceable
     ,(nth 0 args)
     optional-expr
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 508 ((callable_variable) (array_object_dereferenceable T_OBJECT_OPERATOR property_name argument_list))
(puthash
 508
 (lambda(args _terminals)
   `(
     ast-type
     callable-variable-array-object-dereferenceable-property-name
     array-object-dereferenceable
     ,(nth 0 args)
     property-name
     ,(nth 2 args)
     argument-list
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 509 ((callable_variable) (array_object_dereferenceable T_NULLSAFE_OBJECT_OPERATOR property_name argument_list))
(puthash
 509
 (lambda(args _terminals)
   `(
     ast-type
     callable-variable-array-object-dereferenceable-nullsafe-property-name
     array-object-dereferenceable
     ,(nth 0 args)
     property-name
     ,(nth 2 args)
     argument-list
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 510 ((callable_variable) (function_call))
(puthash
 510
 (lambda(args _terminals)
   `(
     ast-type
     callable-variable-function-call
     function-call
     ,args))
 phps-mode-parser--table-translations)

;; 511 ((variable) (callable_variable))
(puthash
 511
 (lambda(args _terminals)
   `(
     ast-type
     variable-callable-variable
     callable-variable
     ,args))
 phps-mode-parser--table-translations)

;; 512 ((variable) (static_member))
(puthash
 512
 (lambda(args _terminals)
   `(
     ast-type
     variable-static-member
     static-member
     ,args))
 phps-mode-parser--table-translations)

;; 513 ((variable) (array_object_dereferenceable T_OBJECT_OPERATOR property_name))
(puthash
 513
 (lambda(args terminals)
   (let* ((array-object-dereferenceable (nth 0 args))
          (array-object-dereferenceable-type
           (plist-get
            array-object-dereferenceable
            'ast-type)))
     (when (equal
            array-object-dereferenceable-type
            'array-object-dereferenceable-fully-dereferenceable)
       (let* ((dereferenceable
               (plist-get
                array-object-dereferenceable
                'fully-dereferenceable))
              (dereferencable-type
               (plist-get
                dereferenceable
                'ast-type)))
         (when (equal
                dereferencable-type
                'fully-dereferenceable-variable)
           (let* ((fully-dereferenceable-variable
                   (plist-get
                    dereferenceable
                    'variable))
                  (fully-dereferenceable-variable-type
                   (plist-get
                    fully-dereferenceable-variable
                    'ast-type)))
             (when (equal
                    fully-dereferenceable-variable-type
                    'variable-callable-variable)
               (let* ((callable-variable
                       (plist-get
                        fully-dereferenceable-variable
                        'callable-variable))
                      (callable-variable-type
                       (plist-get
                        callable-variable
                        'ast-type)))
                 (when (equal
                        callable-variable-type
                        'callable-variable-simple-variable)
                   (let* ((simple-variable
                           (plist-get
                            (plist-get
                             callable-variable
                             'simple-variable)
                            'variable)))
                     (push
                      (list
                       simple-variable
                       phps-mode-parser-sdt--bookkeeping-namespace
                       (car (cdr (nth 0 terminals)))
                       (cdr (cdr (nth 0 terminals))))
                      phps-mode-parser-sdt--bookkeeping-symbol-stack)
                     (let* ((property (nth 2 args))
                            (property-type (plist-get property 'ast-type)))
                       (when (equal
                              property-type
                              'property-name-string)
                         (let ((property-string
                                (plist-get
                                 property
                                 'string)))
                           (let ((namespace phps-mode-parser-sdt--bookkeeping-namespace))
                             (push (list 'object-operator simple-variable) namespace)
                             (push
                              (list
                               (format "$%s" property-string)
                               namespace
                               (car (cdr (nth 2 terminals)))
                               (cdr (cdr (nth 2 terminals))))
                              phps-mode-parser-sdt--bookkeeping-symbol-stack))))))))))))))
   `(
     ast-type
     variable-array-object-dereferenceable-property-name
     array-object-dereferenceable
     ,(nth 0 args)
     property-name
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 514 ((variable) (array_object_dereferenceable T_NULLSAFE_OBJECT_OPERATOR property_name))
(puthash
 514
 (lambda(args _terminals)
   `(
     ast-type
     variable-array-object-dereferenceable-nullsafe-property-name
     array-object-dereferenceable
     ,(nth 0 args)
     property-name
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 515 ((simple_variable) (T_VARIABLE))
(puthash
 515
 (lambda(args terminals)
   (let ((symbol-name
           args)
         (symbol-start
          (car (cdr terminals)))
         (symbol-end
          (cdr (cdr terminals)))
         (namespace phps-mode-parser-sdt--bookkeeping-namespace))
     (push
      (list
       symbol-name
       namespace
       symbol-start
       symbol-end)
      phps-mode-parser-sdt--bookkeeping-symbol-stack))

   `(
     ast-type
     simple-variable-variable
     ast-start
     ,(car (cdr terminals))
     ast-end
     ,(cdr (cdr terminals))
     variable
     ,args))
 phps-mode-parser--table-translations)

;; 516 ((simple_variable) ("$" "{" expr "}"))
(puthash
 516
 (lambda(args _terminals)
   `(
     ast-type
     simple-variable-expr
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 517 ((simple_variable) ("$" simple_variable))
(puthash
 517
 (lambda(args terminals)
   `(
     ast-type
     simple-variable-variable
     ,(nth 1 args)
     ast-start
     ,(car (cdr (nth 1 terminals)))
     ast-end
     ,(cdr (cdr (nth 1 terminals)))
     ))
 phps-mode-parser--table-translations)

;; 518 ((static_member) (class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable))
(puthash
 518
 (lambda(args _terminals)
   (let* ((class-name (nth 0 args))
          (class-name-type (plist-get class-name 'ast-type)))
     (cond
      ((equal class-name-type 'class-name-name)
       (let ((class-name-string (plist-get class-name 'name))
             (namespace phps-mode-parser-sdt--bookkeeping-namespace))
         (push (list 'static-member class-name-string) namespace)
         (setf
          (nth 1 (car phps-mode-parser-sdt--bookkeeping-symbol-stack))
          namespace)))
      ((equal class-name-type 'class-name-static)
       (let ((namespace phps-mode-parser-sdt--bookkeeping-namespace))
         (push (list 'static-member "static") namespace)
         (setf
          (nth 1 (car phps-mode-parser-sdt--bookkeeping-symbol-stack))
          namespace)))))
   `(
     ast-type
     static-member-class-name
     class-name
     ,(nth 0 args)
     simple-variable
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 519 ((static_member) (variable_class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable))
(puthash
 519
 (lambda(args _terminals)
   ;; TODO Add bookkeeping here
   `(
     ast-type
     static-member-variable-class-name
     variable-class-name
     ,(nth 0 args)
     simple-variable
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 520 ((new_variable) (simple_variable))
(puthash
 520
 (lambda(args _terminals)
   `(
     ast-type
     new-variable-simple-variable
     simple-variable
     ,args))
 phps-mode-parser--table-translations)

;; 521 ((new_variable) (new_variable "[" optional_expr "]"))
(puthash
 521
 (lambda(args _terminals)
   `(
     ast-type
     new-variable-optional-expr
     new-variable
     ,(nth 0 args)
     optional-expr
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 522 ((new_variable) (new_variable "{" expr "}"))
(puthash
 522
 (lambda(args _terminals)
   `(
     ast-type
     new-variable-expr
     new-variable
     ,(nth 0 args)
     optional-expr
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 523 ((new_variable) (new_variable T_OBJECT_OPERATOR property_name))
(puthash
 523
 (lambda(args _terminals)
   `(
     ast-type
     new-variable-property-name
     new-variable
     ,(nth 0 args)
     property-name
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 524 ((new_variable) (new_variable T_NULLSAFE_OBJECT_OPERATOR property_name))
(puthash
 524
 (lambda(args _terminals)
   `(
     ast-type
     new-variable-nullsafe-property-name
     new-variable
     ,(nth 0 args)
     property-name
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 525 ((new_variable) (class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable))
(puthash
 525
 (lambda(args _terminals)
   `(
     ast-type
     new-variable-class-name
     class-name
     ,(nth 0 args)
     simple-variable
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 526 ((new_variable) (new_variable T_PAAMAYIM_NEKUDOTAYIM simple_variable))
(puthash
 526
 (lambda(args _terminals)
   `(
     ast-type
     new-variable-simple-variable
     new-variable
     ,(nth 0 args)
     simple-variable
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 527 ((member_name) (identifier))
(puthash
 527
 (lambda(args _terminals)
   `(
     ast-type
     member-name-identifier
     identifier
     ,args))
 phps-mode-parser--table-translations)

;; 528 ((member_name) ("{" expr "}"))
(puthash
 528
 (lambda(args _terminals)
   `(
     ast-type
     member-name-expr
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 529 ((member_name) (simple_variable))
(puthash
 529
 (lambda(args _terminals)
   `(
     ast-type
     member-name-simple-variable
     simple-variable
     ,args))
 phps-mode-parser--table-translations)

;; 530 ((property_name) (T_STRING))
(puthash
 530
 (lambda(args _terminals)
   `(
     ast-type
     property-name-string
     string
     ,args))
 phps-mode-parser--table-translations)

;; 531 ((property_name) ("{" expr "}"))
(puthash
 531
 (lambda(args _terminals)
   `(
     ast-type
     properyt-name-expr
     expr
     ,args))
 phps-mode-parser--table-translations)

;; 532 ((property_name) (simple_variable))
(puthash
 532
 (lambda(args _terminals)
   `(
     ast-type
     property-name-simple-variable
     simple-variable
     ,args))
 phps-mode-parser--table-translations)

;; 533 ((array_pair_list) (non_empty_array_pair_list))
(puthash
 533
 (lambda(args _terminals)
   args)
 phps-mode-parser--table-translations)

;; 534 ((possible_array_pair) (%empty))
(puthash
 534
 (lambda(_args _terminals)
   nil)
 phps-mode-parser--table-translations)

;; 535 ((possible_array_pair) (array_pair))
(puthash
 535
 (lambda(args _terminals)
   args)
 phps-mode-parser--table-translations)

;; 536 ((non_empty_array_pair_list) (non_empty_array_pair_list "," possible_array_pair))
(puthash
 536
 (lambda(args _terminals)
   (if (nth 2 args)
       (append (nth 0 args) (list (nth 2 args)))
     (nth 0 args)))
 phps-mode-parser--table-translations)

;; 537 ((non_empty_array_pair_list) (possible_array_pair))
(puthash
 537
 (lambda(args _terminals)
   (list args))
 phps-mode-parser--table-translations)

;; 538 ((array_pair) (expr T_DOUBLE_ARROW expr))
(puthash
 538
 (lambda(args _terminals)
   `(
     ast-type
     array-pair-expr1-double-arrow-expr2
     expr1
     ,(nth 0 args)
     expr2
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 539 ((array_pair) (expr))
(puthash
 539
 (lambda(args _terminals)
   `(
     ast-type
     array-pair-expr
     expr
     ,args))
 phps-mode-parser--table-translations)

;; 540 ((array_pair) (expr T_DOUBLE_ARROW ampersand variable))
(puthash
 540
 (lambda(args _terminals)
   `(
     ast-type
     array-pair-expr-double-arrow-referenced-variable
     expr
     ,(nth 0 args)
     referenced-variable
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 541 ((array_pair) (ampersand variable))
(puthash
 541
 (lambda(args _terminals)
   `(
     ast-type
     array-pair-referenced-variable
     referenced-variable
     ,args))
 phps-mode-parser--table-translations)

;; 542 ((array_pair) (T_ELLIPSIS expr))
(puthash
 542
 (lambda(args _terminals)
   `(
     ast-type
     array-pair-ellipsis-expr
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 543 ((array_pair) (expr T_DOUBLE_ARROW T_LIST "(" array_pair_list ")"))
(puthash
 543
 (lambda(args _terminals)
   `(
     ast-type
     array-pair-double-arrow-list
     expr
     ,(nth 0 args)
     array-pair-list
     ,(nth 4 args)))
 phps-mode-parser--table-translations)

;; 544 ((array_pair) (T_LIST "(" array_pair_list ")"))
(puthash
 544
 (lambda(args _terminals)
   `(
     ast-type
     array-pair-list
     array-pair-list
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 545 ((encaps_list) (encaps_list encaps_var))
(puthash
 545
 (lambda(args _terminals)
   `(
     ast-type
     escaps-list-encaps-list
     ,(append (nth 0 args) (list (nth 1 args)))))
 phps-mode-parser--table-translations)

;; 546 ((encaps_list) (encaps_list T_ENCAPSED_AND_WHITESPACE))
(puthash
 546
 (lambda(args _terminals)
   `(
     ast-type
     escaps-list-encapsed-and-whitespace
     encaps-list
     ,(nth 0 args)
     encapsed-and-whitespace
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 547 ((encaps_list) (encaps_var))
(puthash
 547
 (lambda(args _terminals)
   `(
     ast-type
     escaps-list-encaps-var
     encaps-var
     ,args))
 phps-mode-parser--table-translations)

;; 548 ((encaps_list) (T_ENCAPSED_AND_WHITESPACE encaps_var))
(puthash
 548
 (lambda(args _terminals)
   `(
     ast-type
     escaps-list-enscapsed-and-whitespace-encaps-var
     encapsed-and-whitespace
     ,(nth 0 args)
     encaps-var
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 549 ((encaps_var) (T_VARIABLE))
(puthash
 549
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-variable
     variable
     ,args))
 phps-mode-parser--table-translations)

;; 550 ((encaps_var) (T_VARIABLE "[" encaps_var_offset "]"))
(puthash
 550
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-variable-with-offset
     variable
     ,(nth 0 args)
     encaps-var-offset
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 551 ((encaps_var) (T_VARIABLE T_OBJECT_OPERATOR T_STRING))
(puthash
 551
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-variable-property
     variable
     ,(nth 0 args)
     property
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 552 ((encaps_var) (T_VARIABLE T_NULLSAFE_OBJECT_OPERATOR T_STRING))
(puthash
 552
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-variable-nullsafe-property
     variable
     ,(nth 0 args)
     property
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 553 ((encaps_var) (T_DOLLAR_OPEN_CURLY_BRACES expr "}"))
(puthash
 553
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-braces-expr
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 554 ((encaps_var) (T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME "}"))
(puthash
 554
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-braces-string-varname
     string-varname
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 555 ((encaps_var) (T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME "[" expr "]" "}"))
(puthash
 555
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-braces-string-varname-indexed-expr
     string-varname
     ,(nth 1 args)
     indexed-expr
     ,(nth 3 args)))
 phps-mode-parser--table-translations)

;; 556 ((encaps_var) (T_CURLY_OPEN variable "}"))
(puthash
 556
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-curly-open-variable
     variable
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 557 ((encaps_var_offset) (T_STRING))
(puthash
 557
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-offset-string
     string
     ,args))
 phps-mode-parser--table-translations)

;; 558 ((encaps_var_offset) (T_NUM_STRING))
(puthash
 558
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-offset-num-string
     num-string
     ,args))
 phps-mode-parser--table-translations)

;; 559 ((encaps_var_offset) ("-" T_NUM_STRING))
(puthash
 559
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-offset-negative-num-string
     num-string
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 560 ((encaps_var_offset) (T_VARIABLE))
(puthash
 560
 (lambda(args _terminals)
   `(
     ast-type
     encaps-var-offset-variable
     variable
     ,args))
 phps-mode-parser--table-translations)

;; 561 ((internal_functions_in_yacc) (T_ISSET "(" isset_variables possible_comma ")"))
(puthash
 561
 (lambda(args terminals)
   (when phps-mode-parser-sdt--bookkeeping-symbol-stack
     (let ((isset-start (car (cdr (nth 1 terminals))))
           (isset-end (cdr (cdr (nth 4 terminals)))))
       (dolist (item phps-mode-parser-sdt--bookkeeping-symbol-stack)
         (let ((item-start (nth 2 item))
               (item-end (nth 3 item)))
           (when (and
                  (> item-start isset-start)
                  (< item-end isset-end))
             (push
              (list
               (nth 0 item)
               phps-mode-parser-sdt--bookkeeping-namespace
               item-start
               item-end)
             phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack))))))
   `(
     ast-type
     internal-isset
     isset-variables
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 562 ((internal_functions_in_yacc) (T_EMPTY "(" expr ")"))
(puthash
 562
 (lambda(args terminals)
   (when phps-mode-parser-sdt--bookkeeping-symbol-stack
     (let ((empty-start (car (cdr (nth 1 terminals))))
           (empty-end (cdr (cdr (nth 3 terminals)))))
       (dolist (item phps-mode-parser-sdt--bookkeeping-symbol-stack)
         (let ((item-start (nth 2 item))
               (item-end (nth 3 item)))
           (when (and
                  (> item-start empty-start)
                  (< item-end empty-end))
             (push
              (list
               (nth 0 item)
               phps-mode-parser-sdt--bookkeeping-namespace
               item-start
               item-end)
              phps-mode-parser-sdt--bookkeeping-symbol-assignment-stack))))))
   `(
     ast-type
     internal-empty-expr
     expr
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 563 ((internal_functions_in_yacc) (T_INCLUDE expr))
(puthash
 563
 (lambda(args _terminals)
   `(
     ast-type
     internal-include-expr
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 564 ((internal_functions_in_yacc) (T_INCLUDE_ONCE expr))
(puthash
 564
 (lambda(args _terminals)
   `(
     ast-type
     internal-include-once
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 565 ((internal_functions_in_yacc) (T_EVAL "(" expr ")"))
(puthash
 565
 (lambda(args _terminals)
   `(
     ast-type
     internal-eval
     expr
     ,(nth 2 args)))
 phps-mode-parser--table-translations)

;; 566 ((internal_functions_in_yacc) (T_REQUIRE expr))
(puthash
 566
 (lambda(args _terminals)
   `(
     ast-type
     internal-require
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 567 ((internal_functions_in_yacc) (T_REQUIRE_ONCE expr))
(puthash
 567
 (lambda(args _terminals)
   `(
     ast-type
     internal-require-once
     expr
     ,(nth 1 args)))
 phps-mode-parser--table-translations)

;; 568 ((isset_variables) (isset_variable))
(puthash
 568
 (lambda(args _terminals)
   (list args))
 phps-mode-parser--table-translations)

;; 569 ((isset_variables) (isset_variables "," isset_variable))
(puthash
 569
 (lambda(args _terminals)
   (append (nth 0 args) (nth 2 args)))
 phps-mode-parser--table-translations)

;; 570 ((isset_variable) (expr))
(puthash
 570
 (lambda(args _terminals)
   args)
 phps-mode-parser--table-translations)

(provide 'phps-mode-parser-sdt)
;;; phps-mode-parser-sdt.el ends here
