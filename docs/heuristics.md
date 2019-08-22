## Heuristics

These should solve the problem of freezing editor for too long when making white-space changes to code in large files. Otherwise a full incremental re-parse would be triggered more often than necessary.

### Return
When pressing return when the rest of the current line after cursor is only white-space, move indexes of tokens, lexer states, indentation and imenu forward one point


### Backspace
When pressing backspace when the rest of the current line before cursor is only white-space, move indexes of tokens, lexer states, indentation and imenu backward one pint

### Indenting
When indenting a line, calculate difference in white-space and change indexes of buffer after point correspondingly

### Other changes
When user has done none-whitespace changes, determine unchanged previous position R, determine changed maximum position X, determine new buffer length L. Do incremental lex from R to X, if new states at X equals old states at X just move indexes with delta X, otherwise do incremental lex of rest of buffer.


[Back to start](../../../)
