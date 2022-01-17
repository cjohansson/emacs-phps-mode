# Improvements

* Catch signaling from AST-generation, bookkeeping and imenu generation
* Fix HTML/XML indentation support

# Bugs

* Lexer/parser fix for multiple cases like

    switch($here) {
            case Type::T_MIXIN:
            case Type::T_FUNCTION:
                list(, $block) = $child;
                // the block need to be able to go up to it's parent env to resolve var
    }

* Imenu-generation of conditionally defined functions and classes
* Bookkeeping of chained object operators like WC()->cart->subtotal
* Condition handling of errors caused by imenu or bookkeeping generation
