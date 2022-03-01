# TODO

## Indentation

* Case 1

```php
<?php
if (true) {
    $table = $installer->getConnection()
        ->newTable($installer->getTable('my_table'))
        ->addColumn();
}
```

* Case 2

```php
<?php
if (true) {
    $criteria = $this->searchCriteriaBuilder
        ->addFilter('status', $status)
        ->addFilter(method', 'my_method_' . $object->getId())
        ->create();
}
```

* Case 3

```php
<?php
if (true) {
    /*
    was here
    */
    echo 'there';
}
```

## Code intelligence

* Bookkeeping of chained object operators like WC()->cart->subtotal
* Bookkeeping of variables inside classes with multiple methods seems to not work
* Move bookkeeping and imenu generation to main thread to be able to populate Sematic Subsystem in the future
* Catch signaling from AST-generation, bookkeeping and imenu generation
* Imenu-generation of conditionally defined functions and classes
* Bookkeeping of class properties inside condition lists
* Fix race-condition in incremental parser when running asynchronous using threads

Perform an edit while an incremental parse is going to reproduce

Fix by reloading file or running C-r to rescan and clear cache
