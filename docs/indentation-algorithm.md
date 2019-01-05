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

    if nesting-stack AND nesting-end <= nesting-stack-end:
        pop stack;
        indentation--;
    endif;

    if we reached end of a line:
        if nesting-stack AND nesting-end <= nesting-stack-start:
            pop stack;
            indent--;
        endif;

        save line indent;

        if nesting-end > 0 AND (!nesting-stack OR nesting-end > nesting-stack-end):
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

```php
If (function( <- (1)
    false)
) { <- (3, 1)
    echo true;
} <- (3)
```
