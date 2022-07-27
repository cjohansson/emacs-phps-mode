# TODO

## Indentation

### Alternative switch case

switch($case) {
    case 1:
    case 2;
    echo 'here';
}

### Multi-line function call with named arguments

function myFunction(
    $arg1,
    $arg2
) {
}
myFunction(
    arg1:
    $var1,
    arg2:
    $var2,
);


## Code intelligence

* Bookkeeping of chained object operators like WC()->cart->subtotal
* Bookkeeping of variables inside classes with multiple methods seems to not work
* Bookkeeping of class properties inside condition lists
* Imenu-generation of conditionally defined functions and classes
* Integrate imenu-generation into bookkeeping generation

## Other

* Support for mmm-mode or similar?
* Build in eldoc support like php-eldoc?
