## Heuristics

These should solve the problem of freezing editor when making white-space changes to code. Otherwise a full incremental re-parse would be triggered more often than necessary.

* When pressing return when the rest of the current line after cursor is only white-space
* When pressing backspace when the rest of the current line before cursor is only white-space

In both of these cases, move indexes of tokens, states, indentations and imenu

[Back to start](../../../)
