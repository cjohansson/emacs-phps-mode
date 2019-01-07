# Indentation algorithm for PHP

Document describing indentation algorithm.

## Terminology

* `nesting`: sum of all kinds of open brackets
* `nesting-stack`: contains nesting-levels before indentation was increased
* `nesting-start`: nesting-level at the beginning of the last line
* `nesting-end`: nesting-level at the end of the last line
* `nesting-stack-start`: nesting-start at top of stack
* `nesting-stack-end`: nesting-end at top of stack

## Algorithm

Here follows pseudo-code for a algorithm that calculates indentation for each line in buffer.

```php
foreach token in buffer:

    calculate nesting-end;

    if nesting-stack AND nesting-end <= nesting-stack-start: // #decrease
        pop stack;
        indent--;
    endif;

    if we reached end of a line:
    
        indent-start = indent;
    
        if temp-pre-indent: // #temp-pre-indent
            indent-start = temp-pre-indent;
        endif;

        save line indent-start; // #save

        if nesting-end > 0 AND (!nesting-stack OR nesting-end > nesting-stack-end): // #increase
            if !nesting-stack:
                nesting-stack-end = 0;
            endif;
            
            push (nesting-stack-end nesting-end) to stack;
            indent++;
        endif;
    endif;

endforeach;
```

## Examples

PHP examples using algorithms defined above, explained each line.

```php				// #save indent: 0
if (function(		// #save indent: 0, #increase push (0 2) indent: 1
    false)			// #save indent: 1
) {				// #decrease pop (0 2) indent: 0, #save indent: 0, #increase push (0 1) indent: 1
    echo true;		// #save indent: 1
}					// #decrease pop (0 1) indent: 0, #save indent: 0
```

## Inline control structure for if-else

```php				// #save indent: 0
if (true)			// #save indent: 0
    echo true;		// #temp-pre-indent: 1, #save indent: 1
else				// #save indent: 0
    echo false;	// #temp-pre-indent: 1, #save indent: 1
```

## Alternative control structure for if-else 2

```php					// #save indent: 0
if (true &&			// #save indent: 0, #increase push (0 1) indent: 1
    true				// #save indent: 1
):						// #decrease pop (0 1) indent: 0, #save indent: 0, #increase push (0 1) indent: 1
    echo true;			// #save indent_ 1
elseif (true			// #decrease pop (0 1) indent: 0, #save indent: 0, #increase push (0 1) indent: 1
    || false):			// #decrease pop (0 1) indent: 0, #save indent: 0, #increase push (0 1) indent: 1
    echo 'another';	// #save indent: 1
else:					// #decrease pop (0 1) indent: 0, #save indent: 0, #increase push (0 1) indent: 1
    echo false;		// #save indent: 1
endif;					// #decrease pop (0 1) indent: 0, #save indent: 0
```

## Multi-line assignments 1

```php
<?php				// #save indent: 0
$var = array(		// #save indent: 0, #increase push (0 2) indent: 1
    'def'			// #save indent: 1
);					// #decrease pop (0 2) indent: 0, #save indent: 0
```

## Multi-line assignments 2

```php
<?php				// #save indent: 0
$var = 'abc'		// #save indent: 0, #increase push (0 1) indent: 1
    . 'def'		// #save indent: 1
    . 'ghj';		// #decrease pop (0 1) indent: 0, #save indent: 0 /* ERROR */
```
