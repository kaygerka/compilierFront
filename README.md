# compilierFront
This is the source code for a compiler front end for a simple C-like language. It consists of two main components:
- A Lexical Analyzer (Lexer) — simple.l
- A Parser/Code Generator — simple.y
Together, these tools translate a subset of C-like source code into x86-64 assembly language.

# simple.l: 
Reads the input source code and breaks it into tokens (keywords, identifiers, operators, constants, etc.) for the parser.

How it works:
- Recognizes C-like keywords (if, else, while, for, return, etc.), operators (+, -, *, /, ==, !=, &&, ||, etc.), data types (long, void, char*, etc.), constants, and identifiers.
- Ignores whitespace and tracks line numbers.
- Returns tokens to the parser, sometimes with associated values (like strings or numbers).

# simple.y: 
Parses the token stream from the lexer, checks for syntactic correctness, and generates x86-64 assembly code for the input program.
  
Functions: 
- Supports function definitions, including argument handling and stack frame setup/teardown.
- Handles global and local variables, including arrays and pointers.
- Supports assignments, function calls, compound statements, control flow (if, else, while, do-while, for, break, continue, return).
- Handles arithmetic, relational, logical, and assignment expressions, generating corresponding assembly instructions.


