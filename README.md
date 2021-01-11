# Micro16 Minimal C Compiler

This repository contains a compiler for a tiny subset of C that compiles to Micro16 Assembly. The Micro16 is a very
minimal CPU created for teaching purposes at the Technical University of Vienna. A very broad overview of it's
instructions and capability can be viewed
here https://vowi.fsinf.at/wiki/TU_Wien:Technische_Grundlagen_der_Informatik_VU_(Kastner)/Kapitel_Micro16 (German)

Due to these limitations a fully C compliant compiler is impossible. The main reason is the lack of indirect addressing
therefore making recursion impossible.

For that reason there is also no concept of functions; Everything is written in "global scope". The smallest addressable
storage unit is 16 bit. Due to the lack of a carry over flag, the only integer type is `int` which is 16 bit. Besides
integer types, pointer types exist and can be created recursively.

An example Mini C program implementing Euclids algorithm may look like:

```c
int r0 = R0;
int r1 = R1;
int mod;
do
{
    mod = r1 % r0;
    r1 = r0;
    r0 = mod;
}
while(mod != 0);
R2 = mod;
```

Due to the common need to accept input in specific registers and outputting them to others, the Keywords R0 to R10, AC
and PC exist which can be used to read and write to these registers. One thing to keep in mind however that this usage
is **NOT** visiblie to the Register Allocator, besides attempting to optimize redundant moves. Therefore they shall only
occur at start or end of the C program if possible.

The above currently compiles to following Assembly:

```
R2 <- R0
:doWhileBody
(R1); if N goto .modNeg
R0 <- R1
goto .cont
:modNeg
R3 <- ~R1
R3 <- 1 + R3
R0 <- R3
:cont
R3 <- ~R2
R3 <- 1 + R3
R4 <- R0
:modBody
R5 <- R3 + R4
(R5); if N goto .modEnd
R4 <- R5
goto .modBody
:modEnd
(R4); if Z goto .doWhileContinue
R3 <- R2
R1 <- R3
R2 <- R4
goto .doWhileBody
:doWhileContinue
R2 <- R4
```

Memory access can be simply done through pointers:

```c
int* p = 10;
int r = *p;
r++;
*p = r;
```

## Usage

Have a file that you write your C code into and then write `Micro16C filename.c`. The compiler outputs to stdout. If you
want it written to a file writing `Micro16C filename.c > output.asm` ought to work on most shells.

## Implementation

The whole compiler is written form scratch in F#. It starts at the frontend that first runs a lexer over the string
input, parse the token stream, checks for semantic correctness and then generates IR code. The implemented IR is very
similar to LLVM and in SSA form. Almost all optimizations and code transformations are done in IR and at the very end
compiled to assembly. To compile to assembly register allocation is done using a linear scan register allocator

The IR of the Euclid program above serialized as text is:

```
; succ = ["%doWhileBody"] pred = []
%entry:
        %0 = load R0
        %1 = load R1
        goto %doWhileBody

; succ = ["%modNeg"; "%cont"] pred = ["%boolCont.copy"; "%entry"]
%doWhileBody:
        %3 = phi (%2,%boolCont.copy) (%1,%entry)
        %2 = phi (%4,%boolCont.copy) (%0,%entry)
        br %3 < 0 %modNeg %cont

; succ = ["%cont"] pred = ["%doWhileBody"]
%modNeg:
        %5 = not %3
        %6 = add 1 %5
        goto %cont

; succ = ["%modBody"] pred = ["%modNeg"; "%doWhileBody"]
%cont:
        %7 = phi (%6,%modNeg) (%3,%doWhileBody)
        %8 = not %2
        %9 = add 1 %8
        goto %modBody

; succ = ["%modEnd"; "%modCont"] pred = ["%modCont"; "%cont"]
%modBody:
        %4 = phi (%10,%modCont) (%7,%cont)
        %10 = add %9 %4
        br %10 < 0 %modEnd %modCont

; succ = ["%modBody"] pred = ["%modBody"]
%modCont:
        goto %modBody

; succ = ["%doWhileContinue"; "%boolCont.copy"] pred = ["%modBody"]
%modEnd:
        br %4 == 0 %doWhileContinue %boolCont.copy

; succ = ["%doWhileBody"] pred = ["%modEnd"]
%boolCont.copy:
        goto %doWhileBody

; succ = [] pred = ["%modEnd"]
%doWhileContinue:
        store %4 -> R2
```

## Things left to do

* Spilling to memory
* Division and Multiply
* Unsigned integer types
* Few more things...