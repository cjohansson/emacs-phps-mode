# TODO

## Indentation

## Code intelligence

* Bookkeeping of chained object operators like WC()->cart->subtotal
* Bookkeeping of variables inside classes with multiple methods seems to not work
* Move bookkeeping and imenu generation to main thread to be able to populate Sematic Subsystem in the future
* Catch signaling from AST-generation, bookkeeping and imenu generation
* Lexer/parser fix for multiple cases like

    switch($here) {
            case Type::T_MIXIN:
            case Type::T_FUNCTION:
                list(, $block) = $child;
                // the block need to be able to go up to it's parent env to resolve var
    }

* Imenu-generation of conditionally defined functions and classes
* Bookkeeping of class properties inside condition lists
* Fix race-condition in incremental parser when running asynchronous using threads

Perform an edit while an incremental parse is going to reproduce

Fix by reloading file or running C-r to rescan and clear cache
