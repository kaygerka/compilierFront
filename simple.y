%token	<string_val> WORD
%token 	NOTOKEN LPARENT RPARENT LBRACE RBRACE LCURLY RCURLY COMA SEMICOLON EQUAL STRING_CONST LONG LONGSTAR VOID CHARSTAR CHARSTARSTAR INTEGER_CONST AMPERSAND OROR ANDAND EQUALEQUAL NOTEQUAL LESS GREAT LESSEQUAL GREATEQUAL PLUS MINUS TIMES DIVIDE PERCENT IF ELSE WHILE DO FOR CONTINUE BREAK RETURN
%union	{
    char   *string_val;
    int nargs;
    int my_nlabel;
    int type;
}

%{
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int yylex();
int yyerror(const char * s);

extern int line_number;
const char * input_file;
char * asm_file;
FILE * fasm;

// MAX VARS ------
#define MAX_ARGS 5
int nargs;
char * args_table[MAX_ARGS];

// GLOBAL -----
#define MAX_GLOBALS 100
int nglobals = 0;
char * global_vars_table[MAX_GLOBALS];

// LOCAL -----
#define MAX_LOCALS 32
int nlocals = 0;
char * local_vars_table[MAX_LOCALS];

/* === testing array types*/
#define TYPE_LONG 0
#define TYPE_CHAR 1
int global_vars_type[MAX_GLOBALS];
int local_vars_type[MAX_LOCALS];
/*============*/

#define MAX_STRINGS 100
int nstrings = 0;
char * string_table[MAX_STRINGS];

// ASSEMBLY 
char *regStk[]={ "rbx", "r10", "r13", "r14", "r15"};
char nregStk = sizeof(regStk)/sizeof(char*);
char *regArgs[]={ "rdi", "rsi", "rdx", "rcx", "r8", "r9"};
char nregArgs = sizeof(regArgs)/sizeof(char*);

int top = 0;
int nargs =0;
int nlabel = 0;

#define MAX_LOOP_DEPTH 100
int loop_stack[MAX_LOOP_DEPTH];
int loop_stack_top = 0;

// push loop on stack
void push_loop(int label) {
  if (loop_stack_top < MAX_LOOP_DEPTH) {
    loop_stack[loop_stack_top++] = label;
  } else {
    yyerror("Loop nesting too deep");
  }
}
// pop loop from stack
int pop_loop() {
  if (loop_stack_top > 0) {
    return loop_stack[--loop_stack_top];
   }
  yyerror("Break outside of loop");
  return -1;
}

%}

%%

goal:	program
	;

program :
        function_or_var_list;
/*======================================= FUNCTION ===================================*/
function_or_var_list:
        function_or_var_list function
        | function_or_var_list global_var
        | /*empty */
	;

function: var_type WORD {
    // zero out counters
    nlocals = 0;
    nargs = 0;

    // print .text
    fprintf(fasm, "\t.text\n");
    // declare funt name as .globl
    fprintf(fasm, ".globl %s\n", $2);
    // function label
    fprintf(fasm, "%s:\n", $2);

    // saves old frame ptr and set up new one
    fprintf(fasm, "\t# Save Frame pointer\n");
    fprintf(fasm, "\tpushq %%rbp\n");
    fprintf(fasm, "\tmovq %%rsp,%%rbp\n");

    /* KEEP BC WE ARE MAKING SPACE FOR LOCAL VAR */

    // allocating space for local vars on the stack
    fprintf(fasm, "\tsubq $%d, %%rsp\n", MAX_LOCALS * 8);

    // save callee registers onto the stack
    fprintf(fasm, "# Save registers. \n");
    fprintf(fasm, "# Push one extra to align stack to 16 bytes\n");
    fprintf(fasm, "\tpushq %%rbx\n");
    fprintf(fasm, "\tpushq %%rbx\n");
    fprintf(fasm, "\tpushq %%r10\n");
    fprintf(fasm, "\tpushq %%r13\n");
    fprintf(fasm, "\tpushq %%r14\n");
    fprintf(fasm, "\tpushq %%r15\n");
}
// BODY
LPARENT arguments RPARENT compound_statement {
    // RESTORE THE REGISTERS
    fprintf(fasm, "# Restore registers\n");
    fprintf(fasm, "\tpopq %%r15\n");
    fprintf(fasm, "\tpopq %%r14\n");
    fprintf(fasm, "\tpopq %%r13\n");
    fprintf(fasm, "\tpopq %%r10\n");
    fprintf(fasm, "\tpopq %%rbx\n");
    fprintf(fasm, "\tpopq %%rbx\n");

    /* KEEP THIS TO RESTORE STACK TO HOW IT WAS BEFORE */
    // deallocate local var space on stack
    fprintf(fasm, "\taddq $%d, %%rsp\n", MAX_LOCALS * 8);
    //restore old frame ptr and stakc ptr
    fprintf(fasm, "\tleave\n");
    // return from the function
    fprintf(fasm, "\tret\n");
};

/* ================================================ ARGUMENTS ======================================================= */
arg_list:
         arg
         | arg_list COMA arg
         ;

arguments:
          arg_list | {
          }

arg: var_type WORD {
        // checks if correct
        assert(nargs < MAX_ARGS);
        // adds argument name to table
        args_table[nargs] = strdup($2);
        // ARGUMENTS ARE TREATED AS LOCAL VARIABLES SO WE ADD IT TO THE LOCAL VAR TABLE
        local_vars_table[nlocals] = strdup($2);
        // MOVE ARG VALUE INTO CORRECT STACK LOCATION
        // regArgs[nargs] = %rdi or %rsi
        // argument offset (nargs+1)*8
        fprintf(fasm, "\tmovq %%%s, -%d(%%rbp)\n", regArgs[nargs], (nargs + 1) * 8);
        //incrm num of args and local vars
        nargs++;
        nlocals++;
}
/* ====== ==================================== GLOBAL VARIABLES ====================================================== */
global_var: 
        var_type global_var_list SEMICOLON;

global_var_list: WORD {
    // assigns WORD to id char ptr
    char * id = $1;
    // globl var is defined
    fprintf(fasm, "# Defining global var %s\n", id);
    // prints .data
    fprintf(fasm, "\t.data\n");
    // prints .comm and the size and alignement of the uninitialized globl var
    fprintf(fasm, "\t.comm %s,8,8\n", $1);
    // adds teh name of new globl var to name arr
    global_vars_table[nglobals] = strdup($1);
    // adds teh type of new glbl var to type arr
    global_vars_type[nglobals] = $<type>0;
    // incrm num of globl vars
    nglobals++;
}
| global_var_list COMA WORD {
    assert(nglobals < MAX_GLOBALS);
    // add name to nam arr and type to type arr
    global_vars_table[nglobals] = strdup($3);
    global_vars_type[nglobals] = $<type>0;
    // print .data
    fprintf(fasm, "\t.data\n");
    // prints .comm and size and alignment of 8 bytes of declared unitizialized globl var
    fprintf(fasm, "\t.comm %s,8,8\n", $3);
    //incrm num of g vars
    nglobals++;
};

/* =================================== ASSIGNMENT ====================================*/
// defines types
var_type: CHARSTAR { $<type>$ = TYPE_CHAR; }
        | CHARSTARSTAR { $<type>$ = TYPE_LONG; }
        | LONG { $<type>$ = TYPE_LONG; }
        | LONGSTAR { $<type>$ = TYPE_LONG; }
        | VOID { $<type>$ = TYPE_LONG; };

// checks if local var exists 
// SIMPLE ASSIGNMENT
assignment: WORD EQUAL expression {
    // get var name WORD
    char * id = $<string_val>1;
    // set index = -1
    int localvar = -1;
    // loop thru the local var table and if the id is in it...
    for (int i = 0; i < nlocals; i++) {
      if (strcmp(id, local_vars_table[i]) == 0) {
        // set localvar as the index
        localvar = i;
        break;
      }
    }

    // does the id exist?
    if (localvar != -1) {
      // yes
      // move the local var to the correct position on the stack
      fprintf(fasm, "\tmovq %%%s, -%d(%%rbp)\n", regStk[top-1], 8*(localvar+1));
    } else {
      // no
      // move to global variable
      fprintf(fasm, "\tmovq %%%s, %s(%%rip)\n", regStk[top-1], id);
    }
    //decrmt top reg as we used the value (cmp)
    top--;
}

// ARRAY ASSIGNMENT
| WORD LBRACE expression RBRACE EQUAL expression {
    // arr name
    char * id = $<string_val>1;
    // local var index
    int localvar = -1;
    // var type
    int var_type = TYPE_LONG;

    // checks if var exists
    for (int i = 0; i < nlocals; i++) {
        if (strcmp(id, local_vars_table[i]) == 0) {
            // sest index as found
            localvar = i;
            // sets the var type
            var_type = local_vars_type[i];
            break;
        }
    }

    // if not local, it searches thru global
    if (localvar == -1) {
        for (int i = 0; i < nglobals; i++) {
            if (strcmp(id, global_vars_table[i]) == 0) {
                // found
                // set the var type
                var_type = global_vars_type[i];
                break;
            }
        }
    }

    // prints noting array operation with id
    fprintf(fasm, "\n\t# Array assignment %s[]=\n", id);
    //moves index from 2nd from top of stack to rax
    fprintf(fasm, "\tmovq %%%s, %%rax\n", regStk[top-2]);
    
    // if the arr type is a LONG -> multiply the index by 8 bytes
    if (var_type == TYPE_LONG) {
        fprintf(fasm, "\timulq $8, %%rax\n");
    }

    // Adds the base addy of the arr to calc offset
    // if local arr -> use stack offset
    if (localvar != -1) {
        fprintf(fasm, "\taddq -%d(%%rbp), %%rax\n", 8*(localvar+1));
    // if globl -> add base addy
    } else {
        fprintf(fasm, "\taddq %s(%%rip), %%rax\n", id);
    }

    // if arr type is a CHAR ->
    if (var_type == TYPE_CHAR) {
        // store the value from top reg into calculated memory addy
        //-> movb for char (1 byte)
        fprintf(fasm, "\tmovb %%%sb, (%%rax)\n", regStk[top-1]);
    } else {
        // movq for long (8 bytes)
        fprintf(fasm, "\tmovq %%%s, (%%rax)\n", regStk[top-1]);
    }
    // remove top two reg (stored index and value to be stored)
    top -= 2;
}
/* ==================================== CALL ==================================== */
// function name WORD
call : WORD LPARENT  call_arguments RPARENT {

    // gets the function name from WORD
    char * funcName = $<string_val>1;
    // gets num of arguments from call_arg
    int nargs = $<nargs>3;

    //adds a comment on function name and number of arg
    fprintf(fasm," # func=%s nargs=%d\n", funcName, nargs);
    // another comment indicating the start
    fprintf(fasm," # Move values from reg stack to reg args\n");

    // moves arguments from the reg stack to the correct arg reg (reverse)
    for (int i = nargs - 1; i >= 0; i--) {
        top--;
        fprintf(fasm, "\tmovq %%%s, %%%s\n", regStk[top], regArgs[i]);
    }
    // PRINTTF
    if (!strcmp(funcName, "printf")) {
       // printf has a variable number of arguments
       // and it need the following eax = 0
       fprintf(fasm, "\tmovl    $0, %%eax\n");
    }
    // creates the actual function call instruction
     fprintf(fasm, "\tcall %s\n", funcName);
     // move return value to top reg
     fprintf(fasm, "\tmovq %%rax, %%%s\n", regStk[top]);
     //incremnt register stakc
     top++;
};

// single expression
call_arg_list: expression {
          $<nargs>$=1;
  }

  // list of arguments
  | call_arg_list COMA expression {
    $<nargs>$++;
  };

call_arguments: call_arg_list { $<nargs>$=$<nargs>1; }
  | /*empty*/ { $<nargs>$=0;
  };
/* ==================================== EXPRESSIONS =======================*/
expression :
         logical_or_expr
	 ;
/* --------------------- OR ---------------------*/
logical_or_expr:
         logical_and_expr
  | /*empty*/ { $<nargs>$=0;
};

expression :
         logical_or_expr
;

logical_or_expr:
         logical_and_expr
   | logical_or_expr OROR logical_and_expr {
   // prints indicating start of OR 
   fprintf(fasm, "\n\t# Logical OR\n");
   // checks if there is at least 2 values on the register stack
     if (top >= 2) {
      // create a unqiue label
       int label = nlabel++;
       // compares 2nd to top with 0 (if false)
       fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-2]);
       // jump to true if it not equal to 0
       fprintf(fasm, "\tjne .Ltrue_%d\n", label);
       // compares the top with 0 
       fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-1]);
       // jump to true if not equal to 0
       fprintf(fasm, "\tjne .Ltrue_%d\n", label);
       // if both comparison is false (both = 0), it sets the results to false
       fprintf(fasm, "\tmovq $0, %%%s\n", regStk[top-2]);
       // and jumps to end
       fprintf(fasm, "\tjmp .Lend_%d\n", label);
       // THE TRUE JUMP 
       fprintf(fasm, ".Ltrue_%d:\n", label);
       // sets the results to 1
       fprintf(fasm, "\tmovq $1, %%%s\n", regStk[top-2]);
       // END JUMP
       fprintf(fasm, ".Lend_%d:\n", label);
       // dcrmt top register bc two values and now one
       top--;
     }
}

/* --------------------- AND  -------------------- */;

logical_and_expr:
         equality_expr
| logical_and_expr ANDAND equality_expr { 
    // prints indicating start
   fprintf(fasm, "\n\t# Logical AND\n");
      // check if at least two values
      if (top >= 2) {
        // unqiuw label
        int label = nlabel++;
        // check if 2nd from top is = 0
        fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-2]);
        // if = 0 jump to fALSE
        fprintf(fasm, "\tje .Lfalse_%d\n", label);
        // if 2nd !=0
        // checks if top is = 0
        fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-1]);
        // if =0 jump to false
        fprintf(fasm, "\tje .Lfalse_%d\n", label);
        // if both != 0, set 2nd from top  = 1
        fprintf(fasm, "\tmovq $1, %%%s\n", regStk[top-2]);
        // jump to end
        fprintf(fasm, "\tjmp .Lend_%d\n", label);
        // at FALSE SET 2nd from top to = 0
        fprintf(fasm, ".Lfalse_%d:\n", label);
        fprintf(fasm, "\tmovq $0, %%%s\n", regStk[top-2]);
        // at END
        fprintf(fasm, ".Lend_%d:\n", label);
        // dcrmt top bc result is in 2nd to top
        top--;
     }
 };
/*------------------ EQUAL ------------------ */
equality_expr:
         relational_expr
    | equality_expr EQUALEQUAL relational_expr { 
    // print start
    fprintf(fasm, "\n\t# ==\n");
    if (top >= 2) {
      // create unque label 
      int label = nlabel++;
      // compares top 2 registers by subtracting sec - first
      fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);
      // sets al = 1 if equal, 0 if not
      fprintf(fasm, "\tsete %%al\n");
      // moves al result to 2nd from top register
      fprintf(fasm, "\tmovzbq %%al, %%%s\n", regStk[top-2]);
      // decrm top register 
      top--;
    }
}

/* ------------------ NOT EQUAL -------------- */
   | equality_expr NOTEQUAL relational_expr {
   // print start of not equal
   fprintf(fasm, "\n\t# !=\n");
   // check if at least 2 values
    if (top >= 2) {
      // create a unique label for this operation
      int label = nlabel++;
      // compares the first 2 registers from teh top 2 - 1
      fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);
      // sets al: inequal = 1, equal = 0
      fprintf(fasm, "\tsetne %%al\n");
      //sets al to 2nd from the top register
      fprintf(fasm, "\tmovzbq %%al, %%%s\n", regStk[top-2]);
      //decrement top register
      top--;
    }
   };

/* ----------------- LESS THAN --------------- */
relational_expr:
         additive_expr
   | relational_expr LESS additive_expr { 
        // prints to start
        fprintf(fasm, "\n\t# <\n");
        // enough values checked
        if (top >= 2) {
            // compares the top two registers (2nd - 1st)
            fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);
            // set if less to al less than = 1, greater = 0
            fprintf(fasm, "\tsetl %%al\n");
            // set al to 2nd reg from the top
            fprintf(fasm, "\tmovzbq %%al, %%%s\n", regStk[top-2]);
            //decrmt top register
            top--;
        }
}
/* ------------------ GREATER THAN ------------- */
   | relational_expr GREAT additive_expr { 
   // print start of greaterh than
   fprintf(fasm, "\n\t# >\n");
    // check if enough values
    if (top >= 2) {
        // compares the 2 registers from the top of the stack (2nd - 1st)
        fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);
        // sets al = 1  if top reg is > than second, 0 unless otherwise
        fprintf(fasm, "\tsetg %%al\n");
        // set al to 2nd reg from the top
        fprintf(fasm, "\tmovzbq %%al, %%%s\n", regStk[top-2]);
        // decrmt top register
        top--;
    }
}

/* ------------------ LESS THAN OR EQUAL ------------- */
   | relational_expr LESSEQUAL additive_expr {
   // print to indicate start
   fprintf(fasm, "\n\t# <=\n");
   // check if enough vlaues
    if (top >= 2) {
        // comapres the two registeres from the top
        fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);
        // if the top reg is <= 2nd, al = 1, al = 0 otherwise
        fprintf(fasm, "\tsetle %%al\n");
        // set 2nd register from the top to equal al
        fprintf(fasm, "\tmovzbq %%al, %%%s\n", regStk[top-2]);
        // decrmt top register
        top--;
    }
}

/* ---------------- GREATER THAN OR EQUAL ---------------*/
  | relational_expr GREATEQUAL additive_expr { 
  // indicate start
   fprintf(fasm, "\n\t# >=\n");
    // check if enough values
    if (top >= 2) {
        // compares the top 2 reg in the stack
        fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);
        // if the top reg is >= 2nd reg al = 1, otherwise = 0
        fprintf(fasm, "\tsetge %%al\n");
        // 2nd from the top reg = al
        fprintf(fasm, "\tmovzbq %%al, %%%s\n", regStk[top-2]);
        //decrmt top register
        top--;
    }
};

/* -------------------- ADDING -------------------- */
additive_expr:
          multiplicative_expr
    | additive_expr PLUS multiplicative_expr {
    // print to start
    fprintf(fasm,"\n\t# +\n");
    // checks if there are available registers in the stack
    if (top<nregStk) {
      // adds the top two reg in the stack -> resutl in 2nd reg
      fprintf(fasm, "\taddq %%%s,%%%s\n", regStk[top-1], regStk[top-2]);
      //dcrmt top register
      top--;
    }
 }
 /*SUBTRACTING -----------------------------------------------*/
    | additive_expr MINUS multiplicative_expr { 
    // print indicate start
    fprintf(fasm,"\n\t# -\n");
    // checks if enough available registesr
    if (top<nregStk) {
      // subtracts the top towo reg in stack -> result in 2nd reg
      fprintf(fasm, "\tsubq %%%s,%%%s\n",regStk[top-1], regStk[top-2]);
      // decrmt top register
      top--;
    }
} ;

/* ------------------ MULTIPLYING ---------------- */
multiplicative_expr:
          primary_expr
    | multiplicative_expr TIMES primary_expr {
      // print indicate start
      fprintf(fasm,"\n\t# *\n");
      // check if enough registers
      if (top<nregStk) {
        //multiplies the top 2 registers from the top -> results in 2nd reg
        fprintf(fasm, "\timulq %%%s,%%%s\n",regStk[top-1], regStk[top-2]);
        // decrmt top register
        top--;
      }
} /* DIVISION ---------------------------------------------------*/
    | multiplicative_expr DIVIDE primary_expr { 
    // prints 
     fprintf(fasm,"\n\t# /\n");
     // checks if there are available registers in the stack
      if (top<nregStk) {
        // moves teh value from the 2nd to top register -> rax (dividend aka big number)
        fprintf(fasm, "\tmovq %%%s, %%rax\n", regStk[top-2]);
        // extends rax to 128 bits (needed for signed division)
        fprintf(fasm, "\tcqto\n");
        // actually does the divison (rax / value at the top of the stack)
        // quotient stored in rax
        fprintf(fasm, "\tidivq %%%s\n", regStk[top-1]);
        // moves the quotient/answer from rax to the second to top register in the stack
        fprintf(fasm, "\tmovq %%rax, %%%s\n", regStk[top-2]);
        // decrmt the register at top of stack
        top--;
    }
}
/* MODULO ------------------------------------------------------*/
    | multiplicative_expr PERCENT primary_expr { 
    // print indicate start
    fprintf(fasm,"\n\t# %%\n");
    // check if enough regiester
    if (top<nregStk) {
        // move 2nd to top to rax
        fprintf(fasm, "\tmovq %%%s, %%rax\n", regStk[top-2]);
        // change to 128 bits bc signed (division?)
        fprintf(fasm, "\tcqto\n");
        // divides 128 rax with top reg and stores in 128 rax and remainder in rdx
        fprintf(fasm, "\tidivq %%%s\n", regStk[top-1]);
        // move remainder in rdx to reg 2nd from the top
        fprintf(fasm, "\tmovq %%rdx, %%%s\n", regStk[top-2]);
        // dcrmt top register
        top--;
    }
} ;

/* ================================== PRIMARY =====================================*/
primary_expr:  STRING_CONST {
      // add STRING_CONST to string table
      string_table[nstrings]=$<string_val>1;
      // indicate current top of reg stack
      fprintf(fasm, "\t#top=%d\n", top);
      // shows string being pushed to top
      fprintf(fasm, "\n\t# push string %s top=%d\n",$<string_val>1, top);

      // check sif enought space available on the reg stack
      if (top<nregStk) {
        // move address of string to the next avail reg
        fprintf(fasm, "\tmovq $string%d, %%%s\n", nstrings, regStk[top]);
        // incrm top reg stack bc using reg
        top++;
      }
      // incrm strings counter
      nstrings++;
      }
   | call
   /* VARIABLES FROM GUSTAVO VIDEO DO NOT CHANGE =========== */
   | WORD {
      // assign id = WORD
      char * id = $<string_val>1;
      // id index and arg  = -1 (not found)
      int localvar = -1;
      int arg = -1;
      /* FOR LOCAL VARIABLES */
      // checks if id is a local var
      for (int i = 0; i < nlocals; i++) {
        if (strcmp(id, local_vars_table[i]) == 0) {
          // set id index and local var
          localvar = i;
          break;
        }
      }
      /* FOR ARGUMENTS */
      // checks if id is an arg
      for (int i = 0; i < nargs; i++) {
        if (strcmp(id, args_table[i]) == 0) {
            // set arg as index of id
            arg = i;
            break;
        }
    }
      // if argument exist
      if (arg != -1) {
        // move arg value to next avail reg (calcs arg pos on stack)
        fprintf(fasm, "\tmovq -%d(%%rbp), %%%s\n", 8*(arg+1), regStk[top]);
      // if local var exists
      } else if (localvar != -1) {
          // move local var val to next avail reg (pos offset calculated)
         fprintf(fasm, "\tmovq -%d(%%rbp), %%%s\n", 8*(localvar+1), regStk[top]);
      } else {
        // if global var exists
        // move from global mem to next avail reg
        fprintf(fasm, "\tmovq %s(%%rip), %%%s\n", id, regStk[top]);
      }
      // incrm bc new reg is used
      top++;
}

    // ARRAY ACCESS W/ SQUARE BRACKETS
    | WORD LBRACE expression RBRACE {
    // sets arr name = id
    char * id = $<string_val>1;
    // mark index of arr as not found
    int localvar = -1;
    // set arr type as long
    int var_type = TYPE_LONG;

    // check if id is a local var
    for (int i = 0; i < nlocals; i++) {
        if (strcmp(id, local_vars_table[i]) == 0) {
            // set arr index
            localvar = i;
            // set arr type
            var_type = local_vars_type[i];
            break;
        }
    }

    // if arr not found with local
    if (localvar == -1) {
        // check if global 
        for (int i = 0; i < nglobals; i++) {
            if (strcmp(id, global_vars_table[i]) == 0) {
                // set arr type
                var_type = global_vars_type[i];
                break;
            }
        }
    }

    // ARRAY INDEXING OPERATION comment
    fprintf(fasm, "\n\t# Array indexing %s[]\n", id);
    // move index from top reg to rax
    fprintf(fasm, "\tmovq %%%s, %%rax\n", regStk[top-1]); 

    // if arr is long
    if (var_type == TYPE_LONG) {
        // multiply array index by 8 bytes for byte offset
        fprintf(fasm, "\timulq $8, %%rax\n");
    }

    // is arr local var
    if (localvar != -1) {
        //add base address to rax
        fprintf(fasm, "\taddq -%d(%%rbp), %%rax\n", 8*(localvar+1));
    // is global 
    } else {
        // add base address to rax
        fprintf(fasm, "\taddq %s(%%rip), %%rax\n", id);
    }

    // if arr is char
    if (var_type == TYPE_CHAR) {
        // loads a single byte from calculated addy into lowbyte top reg
        fprintf(fasm, "\tmovb (%%rax), %%%sb\n", regStk[top-1]);
    // if arr is long
    } else {
        // loads 8 bytes from calulated addy into top reg
        fprintf(fasm, "\tmovq (%%rax), %%%s\n", regStk[top-1]); 
    }
}

/*AMPERSAND ----------------------------------------------------------*/
    | AMPERSAND WORD {
        // sets id = word
        char * id = $2;
        // index of local var not found
        int localvar = -1;
        // loops thru local var to see if id is one
        for (int i = 0; i < nlocals; i++) {
            if (strcmp(id, local_vars_table[i]) == 0) {
                // set index of id
                localvar = i;
                break;
            }
        }

        // if local var
        if (localvar != -1) {
            // load addy from rbp to next avil reg
            fprintf(fasm, "\tleaq -%d(%%rbp), %%%s\n", 8*(localvar+1), regStk[top]);
        } else {
        // if global
            //load addy into next avail reg
            fprintf(fasm, "\tleaq %s(%%rip), %%%s\n", id, regStk[top]);
        }
        // new reg incrm top
        top++;
}
    | INTEGER_CONST {
      // indicate pushing constant
      fprintf(fasm, "\n\t# push %s\n", $<string_val>1);
      // check if enough registers
      if (top<nregStk) {
        // move int constant to next avail reg
        fprintf(fasm, "\tmovq $%s,%%%s\n",  $<string_val>1, regStk[top]);
        // incrm top bc another reg is used
        top++;
      }
    }
    | LPARENT expression RPARENT
    ;

/* =============================== STATEMENT ====================== */
compound_statement:
   LCURLY statement_list RCURLY
   ;

statement_list:
         statement_list statement
	 | /*empty*/
	 ;

/* ============================================ LOCAL VARIABLES ===================================================== */

/*      VARIABLE TYPE (INT, STR, ETC)// LIST OF LOCAL VAR BEING DECLARED// ;*/
local_var:
        var_type local_var_list SEMICOLON;

/* DEFINING LOCAL VARIABLES BASED ON GUSTAVOS VIDEO ===================*/
// identifer from the variable list
local_var_list: WORD {
    // assert there is a correct amount of local variables
    assert(nlocals < MAX_LOCALS);
    // adding the var to the var name table (string val is in union saying it is a char*
    local_vars_table[nlocals] = $<string_val>1;
    // adds the var type in the array with the same index as prev
    local_vars_type[nlocals] = $<type>0;
    // increment number of local variable
    nlocals++;
}
/* pulls up all variables list that follow ", WORD"*/
| local_var_list COMA WORD {
    // check if under 32
    assert(nlocals < MAX_LOCALS);
    // adding the var to the table from WORD
    local_vars_table[nlocals] = strdup($<string_val>3);
    // adds the var type to the arr
    local_vars_type[nlocals] = $<type>0;
    // incrm the local var counter
    nlocals++;
};
statement:
         assignment SEMICOLON
	 | call SEMICOLON { top= 0; /* Reset register stack */ }
	 | local_var
   // handles {}
	 | compound_statement
/* IF ----------------------------------------------------------------*/
	 | IF LPARENT expression RPARENT{
        // creates a new label
        int label = nlabel++;
        // checks if statement is false (compares the top of the stack value to 0)
        fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-1]);
        // if false == 0, jump to else label
        fprintf(fasm, "\tje .Lelse_%d\n", label);
        //decrement the register stack ptr
        top--;
        // stores the unique label to be used for the rest of the if else 
        $<my_nlabel>$ = label;
   }
   // end of the if block
   statement{
        // jumps over the "else" to finish the entire if statement
        fprintf(fasm, "\tjmp .Lendif_%d\n", $<my_nlabel>5);
        // start of end whcih is jumped to if the if was false
        fprintf(fasm, ".Lelse_%d:\n", $<my_nlabel>5);
   } 
  // end of the else block
   else_optional{
        // continues
        fprintf(fasm, ".Lendif_%d:\n", $<my_nlabel>5);
   }


/*=============== WHILE ===================*/
	 | WHILE LPARENT {
		// unqiuw label
    $<my_nlabel>$ = nlabel++;
    // prints start
    fprintf(fasm, ".L%d_start:\n", $<my_nlabel>$);
    // pushes the current loop label  onto the stack (i created push_loop())
    push_loop($<my_nlabel>$);
    }

    // EXPRESSION [while(EXPRESSION)]
    expression RPARENT {
    // compares the result of the expression (top reg) to 0
		fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-1]);
    // jump to END of the loop if = 0
    fprintf(fasm, "\tje .L%d_end\n", $<my_nlabel>3);
    //decrm top reg (used the expressoin)
    top--;
    }
    // LOOP BODY
    statement {
    // prints continue lable
    fprintf(fasm, ".L%d_continue:\n", $<my_nlabel>3);
    // jumps back to the start of the loop
		fprintf(fasm, "\tjmp .L%d_start\n", $<my_nlabel>3);
    // prints end label
    fprintf(fasm, ".L%d_end:\n", $<my_nlabel>3);
    // pop the loop from the stack
    pop_loop();
    }

/* ================ DO-WHILE =============*/
	 | DO {
    // create unqiwu label
    $<my_nlabel>$ = nlabel++;
    // label start
    fprintf(fasm, ".L%d_start:\n", $<my_nlabel>$);
    // push loop onto stack
    push_loop($<my_nlabel>$);
    }

    statement WHILE LPARENT expression RPARENT SEMICOLON {
    // print continue lable
    fprintf(fasm, ".L%d_continue:\n", $<my_nlabel>2);
    // compare the result of the expression to the top reg w/0
    fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-1]);
    // jump to start if not equal to 0 (true)
    fprintf(fasm, "\tjne .L%d_start\n", $<my_nlabel>2);
    // print end label
    fprintf(fasm, ".L%d_end:\n", $<my_nlabel>2);
    // decremtn top reg bc exp result
    top--;
    // pop loop off stack
    pop_loop();
    }

/* ======= FOR ============*/
| FOR LPARENT {
    // create unqiwu label
    $<my_nlabel>$ = nlabel++;
    //prints initialization of loop
    fprintf(fasm, ".L%d_init:\n", $<my_nlabel>$);
    // pushes loop onto stack
    push_loop($<my_nlabel>$);


} assignment SEMICOLON {
    // prints the condition label
    fprintf(fasm, ".L%d_cond:\n", $<my_nlabel>3);

} expression SEMICOLON {
    // compare the top register to 0
    fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-1]);
    // jump to END if equal to 0 (false)
    fprintf(fasm, "\tje .L%d_end\n", $<my_nlabel>3);
    // decrmt top reg (exp result)
    top--;
    // print label for body
    fprintf(fasm, "\tjmp .L%d_body\n", $<my_nlabel>3);
    // print label for continue
    fprintf(fasm, ".L%d_continue:\n", $<my_nlabel>3);
    // print label for increment (assignment)
    fprintf(fasm, ".L%d_incr:\n", $<my_nlabel>3);


} assignment RPARENT {
    // jumpts to condition after incrment
    fprintf(fasm, "\tjmp .L%d_cond\n", $<my_nlabel>3);
    // prints label for body
    fprintf(fasm, ".L%d_body:\n", $<my_nlabel>3);

} statement {
    // jump to increment after executing the loop's body
    fprintf(fasm, "\tjmp .L%d_incr\n", $<my_nlabel>3);
    //pritns label for end 
    fprintf(fasm, ".L%d_end:\n", $<my_nlabel>3);
    // pop loop off of stack
    pop_loop();
}
//closes the for loop rule
| jump_statement
	 ;

else_optional:
         ELSE  statement
	 | /*empty*/ {
   //fprintf(fasm, ".Lelse_%d:\n", $<my_nlabel>0);
   };     

/* ================================ JUMP ===================== */
jump_statement:
    /*CONTINUE -----------------------------------------------*/
     CONTINUE SEMICOLON {
     // retrieves latest loop from loop_stack and = labe
      int label = loop_stack[loop_stack_top - 1];
        // not currently in a loop = -1,
        if (label != -1) {
            // jump to continue
            fprintf(fasm, "\tjmp .L%d_continue\n", label);
        } else {
            // print if not in loop
            yyerror("Continue outside of loop");
        }
     }
   /*BREAK ---------------------------------------------------*/
	 | BREAK SEMICOLON {
        // retrieve latest loop from loop stack = label
        int label = loop_stack[loop_stack_top - 1];
        // chekc if currrently in loop, no = -1
        if (label != -1) {
            // jump to end
            fprintf(fasm, "\tjmp .L%d_end\n", label);
        } else {
            // print error bc not in loop
            yyerror("Break outside of loop");
        }
        }
/*RETURN -----------------------------------------------------*/
	 | RETURN expression SEMICOLON {
        // move top reg value to rax
        fprintf(fasm, "\tmovq %%%s, %%rax\n", regStk[top-1]);
        // decrement top reg
        top--;
         // Add these lines to properly return from the function
         fprintf(fasm, "# Restore registers\n");
         //restore the values of registers that were saved at the beginning of the function
         fprintf(fasm, "\tpopq %%r15\n");
         fprintf(fasm, "\tpopq %%r14\n");
         fprintf(fasm, "\tpopq %%r13\n");
         fprintf(fasm, "\tpopq %%r10\n");
         fprintf(fasm, "\tpopq %%rbx\n");
         fprintf(fasm, "\tpopq %%rbx\n");

        /* GUSTAVOS CODE!! KEEP THIS TO RESTORE STACK TO HOW IT WAS BEFORE */
         // add back the memory from the deallocation from the local variabel
         fprintf(fasm, "\taddq $%d, %%rsp\n", MAX_LOCALS * 8);
         // leave instr: movq %rbp, %rsp followed by popq %rbp.
         fprintf(fasm, "\tleave\n");
         // popping the return address from the stack and jumping to it
         fprintf(fasm, "\tret\n");
};

%%

void yyset_in (FILE *  in_str );

int
yyerror(const char * s)
{
	fprintf(stderr,"%s:%d: %s\n", input_file, line_number, s);
}

/* ============================ MAIN ======================= */
int
main(int argc, char **argv)
{
	printf("-------------WARNING: You need to implement global and local vars ------\n");
	printf("------------- or you may get problems with top------\n");
	input_file = strdup(argv[1]);

	int len = strlen(input_file);
	if (len < 2 || input_file[len-2]!='.' || input_file[len-1]!='c') {
		fprintf(stderr, "Error: file extension is not .c\n");
		exit(1);
	}

	// Get assembly file name
	asm_file = strdup(input_file);
	asm_file[len-1]='s';

	// Open file to compile
	FILE * f = fopen(input_file, "r");
	if (f==NULL) {
		fprintf(stderr, "Cannot open file %s\n", input_file);
		perror("fopen");
		exit(1);
	}

	// Create assembly file
	fasm = fopen(asm_file, "w");
	if (fasm==NULL) {
		fprintf(stderr, "Cannot open file %s\n", asm_file);
		perror("fopen");
		exit(1);
	}

	// Uncomment for debugging
	//fasm = stderr;

	// Create compilation file
	// 
	yyset_in(f);
	yyparse();

	// Generate string table
	int i;
	for (i = 0; i<nstrings; i++) {
		fprintf(fasm, "string%d:\n", i);
		fprintf(fasm, "\t.string %s\n\n", string_table[i]);
	}

	fclose(f);
	fclose(fasm);

	return 0;
}
