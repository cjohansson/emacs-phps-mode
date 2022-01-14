# To-do items for emacs-phps-mode

* Catch signaling from AST-generation, bookkeeping and imenu generation
* Fix HTML/XML indentation support
* Optimize lexer by having predefined lists of lambdas inside a hash-map that indexes per state of lexer
* Lexer/parser fix for multiple cases like

            case Type::T_MIXIN:
            case Type::T_FUNCTION:
                list(, $block) = $child;
                // the block need to be able to go up to it's parent env to resolve var
