# TODO

## Indentation

<?php

if (true) {
    $valid = true;
    $variable = myFunction();
    switch ($variable) {
        case Object::CASE1:
            throw new Exception(
                'MyException'
            );
        case Object::Case2:
            throw new \Exception(
                'MyException2',
            );
        case Object::Case3:
            $valid = false;
            break;
        case Object::Case4:
            if (!Object2::validate($variable)) {
                $valid = false;
            }
            break;
        case Object::Case5:
            $valid = false;
            break;
        case Object::Case6:
            $valid = true;
        break;
    }
}

## Code intelligence

* Fix race-condition in incremental parser when running asynchronous using threads

Perform an edit while an incremental parse is going to reproduce

Fix by reloading file or running C-r to rescan and clear cache

* Bookkeeping of chained object operators like WC()->cart->subtotal
* Bookkeeping of variables inside classes with multiple methods seems to not work
* Move bookkeeping and imenu generation to main thread to be able to populate Sematic Subsystem in the future
* Catch signaling from AST-generation, bookkeeping and imenu generation
* Imenu-generation of conditionally defined functions and classes
* Bookkeeping of class properties inside condition lists

## Other

Support for mmm-mode or similar
