\documentclass{article}
%include polycode.fmt

%subst blankline = "\\[5mm]"

% See https://github.com/kosmikus/lhs2tex/issues/58
%format <$> = "\mathbin{\langle\$\rangle}"
%format <&> = "\mathbin{\langle\&\rangle}"
%format <|> = "\mathbin{\langle\:\vline\:\rangle}"
%format <?> = "\mathbin{\langle?\rangle}"
%format <*> = "\mathbin{\langle*\rangle}"
%format <*  = "\mathbin{\langle*}"
%format *>  = "\mathbin{*\rangle}"

\long\def\ignore#1{}

\usepackage{hyperref}
\hypersetup{
	colorlinks = true,
}

\begin{document}

\title{QBE Intermediate Language}
\date{}
\maketitle
\frenchspacing

\ignore{
\begin{code}
module Language.QBE.Parser where

import Data.List (singleton)
import Data.Functor ((<&>))
import qualified Language.QBE as Q
import Language.QBE.Util (bind, decNumber, float)
import Text.ParserCombinators.Parsec
  ( Parser,
    anyChar,
    char,
    manyTill,
    newline,
    oneOf,
    skipMany1,
    (<?>),
    (<|>),
    choice, string, letter, many, alphaNum, optionMaybe, between, noneOf, try, many1, sepBy1, skipMany
  )
\end{code}}

This is the description of the
\href{https://c9x.me/compile/doc/il-v1.2.html}{QBE intermediate language},
specified through ParSec parser combinators and generated from a literate
Haskell file. The natural language description is derived from the offical QBE
IL documentation, licensed under MIT. Presently, this implementation targets
version 1.2 of the QBE intermediate language.

\section{Basic Concepts}

The intermediate language (IL) is a higher-level language than the
machine's assembly language. It smoothes most of the
irregularities of the underlying hardware and allows an infinite number
of temporaries to be used. This higher abstraction level lets frontend
programmers focus on language design issues.

\subsection{Input Files}

The intermediate language is provided to QBE as text. Usually, one file
is generated per each compilation unit from the frontend input language.
An IL file is a sequence of \nameref{sec:definitions} for
data, functions, and types. Once processed by QBE, the resulting file
can be assembled and linked using a standard toolchain (e.g., GNU
binutils).

\begin{code}
comment :: Parser ()
comment = skipMany blankNL >> comment' >> skipMany blankNL
  where
    comment' = char '#' >> manyTill anyChar newline
\end{code}

\ignore{
\begin{code}
skipNoCode :: Parser () -> Parser ()
skipNoCode blankP = try (skipMany1 comment <?> "comments") <|> blankP
\end{code}}

Here is a complete "Hello World" IL file which defines a function that
prints to the screen. Since the string is not a first class object (only
the pointer is) it is defined outside the function\textquotesingle s
body. Comments start with a \# character and finish with the end of the
line.

\begin{verbatim}
data $str = { b "hello world", b 0 }

export function w $main() {
@start
        # Call the puts function with $str as argument.
        %r =w call $puts(l $str)
        ret 0
}
\end{verbatim}

If you have read the LLVM language reference, you might recognize the
example above. In comparison, QBE makes a much lighter use of types and
the syntax is terser.

\subsection{Sigils}

\begin{code}
ident :: Parser String
ident = do
  start <- letter <|> oneOf "_."
  rest <- many (alphaNum <|> oneOf "$._")
  return $ start : rest

userDef :: Parser String
userDef = char ':' >> ident

global :: Parser String
global = char '$' >> ident

local :: Parser String
local = char '%' >> ident

label :: Parser String
label = char '@' >> ident
\end{code}

The intermediate language makes heavy use of sigils, all user-defined
names are prefixed with a sigil. This is to avoid keyword conflicts, and
also to quickly spot the scope and nature of identifiers.

\begin{itemize}
  \item \texttt{:} is for user-defined \nameref{sec:aggregate-types}
  \item \texttt{\$} is for globals (represented by a pointer)
  \item \texttt{\%} is for function-scope temporaries
  \item \texttt{@@} is for block labels
\end{itemize}

\subsection{Spacing}

\begin{code}
blank :: Parser Char
blank = oneOf "\t " <?> "blank"

blankNL :: Parser Char
blankNL = oneOf "\n\t " <?> "blank or newline"
\end{code}

Individual tokens in IL files must be separated by one or more spacing
characters. Both spaces and tabs are recognized as spacing characters.
In data and type definitions, newlines may also be used as spaces to
prevent overly long lines. When exactly one of two consecutive tokens is
a symbol (for example \texttt{,} or \texttt{=} or \texttt{\{}), spacing may be omitted.

\begin{code}
ws :: Parser a -> Parser a
ws p = p <* skipMany blank

ws1 :: Parser a -> Parser a
ws1 p = p <* skipMany1 blank

wsNL :: Parser a -> Parser a
wsNL p = p <* skipNoCode (skipMany blankNL)

wsNL1 :: Parser a -> Parser a
wsNL1 p = p <* skipNoCode (skipMany1 blankNL)
\end{code}

\subsection{String Literals}

\begin{code}
stringLit :: Parser String
stringLit = concat <$> stringLit'
  where
    stringLit' :: Parser [[Char]]
    stringLit' = quoted $ many ((singleton <$> noneOf "\"\\") <|> escSeq)

    quoted :: Parser a -> Parser a
    quoted = let q = char '"' in between q q

    escSeq :: Parser [Char]
    escSeq = try $ do
      esc <- char '\\'
      chr <- anyChar
      return $ [esc, chr]
\end{code}

Strings are enclosed by double quotes and are, for example, used to specify a
section name as part of the \nameref{sec:linkage} information. Within a string,
a double quote can be escaped using a \texttt{\textbackslash} character. All
escape sequences, including double quote escaping, are passed through as-is to
the generated assembly file.

\section{Types}

\subsection{Simple Types}

The IL makes minimal use of types. By design, the types used are
restricted to what is necessary for unambiguous compilation to machine
code and C interfacing. Unlike LLVM, QBE is not using types as a means
to safety; they are only here for semantic purposes.

\begin{code}
baseType :: Parser Q.BaseType
baseType = choice
  [ bind "w" Q.Word
  , bind "l" Q.Long
  , bind "s" Q.Single
  , bind "d" Q.Double ]
\end{code}

The four base types are \texttt{w} (word), \texttt{l} (long), \texttt{s} (single), and \texttt{d}
(double), they stand respectively for 32-bit and 64-bit integers, and
32-bit and 64-bit floating-point numbers. There are no pointer types
available; pointers are typed by an integer type sufficiently wide to
represent all memory addresses (e.g., \texttt{l} on 64-bit architectures).
Temporaries in the IL can only have a base type.

\begin{code}
extType :: Parser Q.ExtType
extType = (Q.Base <$> baseType)
       <|> bind "b" Q.Byte
       <|> bind "h" Q.HalfWord
\end{code}

Extended types contain base types plus \texttt{b} (byte) and \texttt{h} (half word),
respectively for 8-bit and 16-bit integers. They are used in \nameref{sec:aggregate-types}
and \nameref{sec:data} definitions.

For C interfacing, the IL also provides user-defined aggregate types as
well as signed and unsigned variants of the sub-word extended types.
Read more about these types in the \nameref{sec:aggregate-types}
and \nameref{sec:functions} sections.

\subsection{Subtyping}

The IL has a minimal subtyping feature, for integer types only. Any
value of type \texttt{l} can be used in a \texttt{w} context. In that case, only the
32 least significant bits of the word value are used.

Make note that it is the opposite of the usual subtyping on integers (in
C, we can safely use an \texttt{int} where a \texttt{long} is expected). A long value
cannot be used in word context. The rationale is that a word can be
signed or unsigned, so extending it to a long could be done in two ways,
either by zero-extension, or by sign-extension.

\subsection{Constants and Vals}
\label{sec:constants-and-vals}

% Ident is not documented in the original QBE specification.
% See https://c9x.me/git/qbe.git/tree/parse.c?h=v1.2#n304

\begin{code}
constant :: Parser Q.Const
constant =
  (Q.Number <$> decNumber)
    <|> (Q.SFP <$> sfp)
    <|> (Q.DFP <$> dfp)
    <|> (Q.Global <$> ident)
    <?> "const"
  where
    sfp = string "s_" >> float
    dfp = string "d_" >> float

dynConst :: Parser Q.DynConst
dynConst =
  (Q.Const <$> constant)
    <|> (Q.Thread <$> ident)
    <?> "dynconst"
\end{code}

Constants come in two kinds: compile-time constants and dynamic
constants. Dynamic constants include compile-time constants and other
symbol variants that are only known at program-load time or execution
time. Consequently, dynamic constants can only occur in function bodies.

The representation of integers is two's complement.
Floating-point numbers are represented using the single-precision and
double-precision formats of the IEEE 754 standard.

Constants specify a sequence of bits and are untyped. They are always
parsed as 64-bit blobs. Depending on the context surrounding a constant,
only some of its bits are used. For example, in the program below, the
two variables defined have the same value since the first operand of the
subtraction is a word (32-bit) context.

\begin{verbatim}
%x =w sub -1, 0 %y =w sub 4294967295, 0
\end{verbatim}

Because specifying floating-point constants by their bits makes the code
less readable, syntactic sugar is provided to express them. Standard
scientific notation is prefixed with \texttt{s\_} and \texttt{d\_} for single and
double precision numbers respectively. Once again, the following example
defines twice the same double-precision constant.

\begin{verbatim}
%x =d add d_0, d_-1
%y =d add d_0, -4616189618054758400
\end{verbatim}

Global symbols can also be used directly as constants; they will be
resolved and turned into actual numeric constants by the linker.

When the \texttt{thread} keyword prefixes a symbol name, the
symbol\textquotesingle s numeric value is resolved at runtime in the
thread-local storage.

\begin{code}
val :: Parser Q.Value
val =
  (Q.VConst <$> dynConst)
    <|> (Q.VLocal <$> local)
    <?> "val"
\end{code}

Vals are used as arguments in regular, phi, and jump instructions within
function definitions. They are either constants or function-scope
temporaries.

\subsection{Linkage}
\label{sec:linkage}

% The string literal is not documented in the original QBE specification.
% See https://c9x.me/git/qbe.git/tree/parse.c?h=v1.2#n287

\begin{code}
linkage :: Parser Q.Linkage
linkage =
  wsNL (bind "export" Q.LExport)
    <|> wsNL (bind "thread" Q.LThread)
    <|> do
      _ <- ws1 $ string "section"
      -- TODO: make this less awful
      choice
        [ try $ ws1 stringLit >>= (\x -> wsNL stringLit <&> Q.LSection x . pure),
          wsNL stringLit <&> (`Q.LSection` Nothing)
        ]
\end{code}

Function and data definitions (see below) can specify linkage
information to be passed to the assembler and eventually to the linker.

The \texttt{export} linkage flag marks the defined item as visible outside the
current file\textquotesingle s scope. If absent, the symbol can only be
referred to locally. Functions compiled by QBE and called from C need to
be exported.

The \texttt{thread} linkage flag can only qualify data definitions. It mandates
that the object defined is stored in thread-local storage. Each time a
runtime thread starts, the supporting platform runtime is in charge of
making a new copy of the object for the fresh thread. Objects in
thread-local storage must be accessed using the \texttt{thread \$IDENT} syntax,
as specified in the \nameref{sec:constants-and-vals} section.

A \texttt{section} flag can be specified to tell the linker to put the defined
item in a certain section. The use of the section flag is platform
dependent and we refer the user to the documentation of their assembler
and linker for relevant information.

\begin{verbatim}
section ".init_array" data $.init.f = { l $f }
\end{verbatim}

The section flag can be used to add function pointers to a global
initialization list, as depicted above. Note that some platforms provide
a BSS section that can be used to minimize the footprint of uniformly
zeroed data. When this section is available, QBE will automatically make
use of it and no section flag is required.

The section and export linkage flags should each appear at most once in
a definition. If multiple occurrences are present, QBE is free to use
any.

\subsection{Definitions}
\label{sec:definitions}

Definitions are the essential components of an IL file. They can define
three types of objects: aggregate types, data, and functions. Aggregate
types are never exported and do not compile to any code. Data and
function definitions have file scope and are mutually recursive (even
across IL files). Their visibility can be controlled using linkage
flags.

\subsubsection{Aggregate Types}
\label{sec:aggregate-types}

\begin{code}
subType :: Parser Q.SubType
subType =
  (Q.SExtType <$> extType)
    <|> (Q.SUserDef <$> userDef)

-- TODO: "For ease of IL generation, a trailing comma is tolerated by the parser".
aggRegular :: Parser Q.AggType
aggRegular = Q.ARegular <$> sepBy1 aggRegular' (wsNL $ char ',')
  where
    aggRegular' = do
      -- TODO: newline is required if there is a number argument
      f <- wsNL subType
      s <- optionMaybe decNumber
      pure (f, s)

aggOpaque :: Parser Q.AggType
aggOpaque = Q.AOpaque <$> wsNL decNumber

-- TODO: support union type
typeDef :: Parser Q.TypeDef
typeDef = do
  _ <- wsNL1 (string "type")
  i <- wsNL1 userDef
  _ <- wsNL1 (char '=')
  -- TODO: Provide common alignment parser combinator.
  a <- optionMaybe (ws1 (string "align") >> wsNL align)
  braces (aggRegular <|> aggOpaque) <&> Q.TypeDef i a
 where
  -- TODO: Move this to a common combinator
  braces = between (wsNL $ char '{') (wsNL $ char '}')
\end{code}

Aggregate type definitions start with the \texttt{type} keyword. They have file
scope, but types must be defined before being referenced. The inner
structure of a type is expressed by a comma-separated list of types
enclosed in curly braces.

\begin{verbatim}
type :fourfloats = { s, s, d, d }
\end{verbatim}

For ease of IL generation, a trailing comma is tolerated by the parser.
In case many items of the same type are sequenced (like in a C array),
the shorter array syntax can be used.

\begin{verbatim}
type :abyteandmanywords = { b, w 100 }
\end{verbatim}

By default, the alignment of an aggregate type is the maximum alignment
of its members. The alignment can be explicitly specified by the
programmer.

\begin{verbatim}
type :cryptovector = align 16 { w 4 }
\end{verbatim}

Union types allow the same chunk of memory to be used with different layouts. They are defined by enclosing multiple regular aggregate type bodies in a pair of curly braces. Size and alignment of union types are set to the maximum size and alignment of each variation or, in the case of alignment, can be explicitly specified.

\begin{verbatim}
type :un9 = { { b } { s } }
\end{verbatim}

Opaque types are used when the inner structure of an aggregate cannot be specified; the alignment for opaque types is mandatory. They are defined simply by enclosing their size between curly braces.

\begin{verbatim}
type :opaque = align 16 { 32 }
\end{verbatim}

\subsubsection{Data}
\label{sec:data}

\begin{code}
dataDef :: Parser Q.DataDef
dataDef = do
  link <- many linkage
  name <- wsNL1 (string "data") >> wsNL global
  _ <- wsNL (char '=')
  alignment <- optionMaybe (ws1 (string "align") >> wsNL align)
  braces dataObjs <&> Q.DataDef link name alignment
 where
    braces = between (wsNL $ char '{') (wsNL $ char '}')
    dataObjs = sepBy1 dataObj (wsNL $ char ',')
\end{code}

Data definitions express objects that will be emitted in the compiled
file. Their visibility and location in the compiled artifact are
controlled with linkage flags described in the \nameref{sec:linkage}
section.

They define a global identifier (starting with the sigil \texttt{\$}), that
will contain a pointer to the object specified by the definition.

\begin{code}
dataObj :: Parser Q.DataObj
dataObj =
  (Q.OZeroFill <$> (wsNL1 (char 'z') >> wsNL decNumber))
    <|> do
      t <- wsNL1 extType
      i <- many1 (wsNL dataItem)
      return $ Q.OItem t i

dataItem :: Parser Q.DataItem
dataItem =
  (Q.DString <$> stringLit)
    <|> (Q.DConst <$> constant)
    <|> do
      i <- global
      off <- optionMaybe (char '+' >> decNumber)
      return $ Q.DSymbol i off
\end{code}

Objects are described by a sequence of fields that start with a type
letter. This letter can either be an extended type, or the \texttt{z} letter.
If the letter used is an extended type, the data item following
specifies the bits to be stored in the field. When several data items
follow a letter, they initialize multiple fields of the same size.

\begin{code}
align :: Parser Q.AllocAlign
align =
  choice
    [ bind "4" Q.AlignWord,
      bind "8" Q.AlignLong,
      bind "16" Q.AlignLongLong
    ]
\end{code}

The members of a struct will be packed. This means that padding has to
be emitted by the frontend when necessary. Alignment of the whole data
objects can be manually specified, and when no alignment is provided,
the maximum alignment from the platform is used.

When the \texttt{z} letter is used the number following indicates the size of
the field; the contents of the field are zero initialized. It can be
used to add padding between fields or zero-initialize big arrays.

\subsubsection{Functions}
\label{sec:functions}

\begin{code}
funcDef :: Parser Q.FuncDef
funcDef = do
  link <- many linkage
  _ <- ws1 (string "function")
  retTy <- optionMaybe (ws1 abity)
  name <- ws global
  args <- wsNL params
  body <- between (wsNL $ char '{') (char '}') $ many1 block
  return $ Q.FuncDef link name retTy args body
\end{code}

Function definitions contain the actual code to emit in the compiled
file. They define a global symbol that contains a pointer to the
function code. This pointer can be used in \texttt{call} instructions or stored
in memory.

\begin{code}
abity :: Parser Q.Abity
abity = (Q.ABase <$> baseType)
    <|> (Q.AUserDef <$> userDef)
\end{code}

The type given right before the function name is the return type of the
function. All return values of this function must have this return type.
If the return type is missing, the function must not return any value.

\begin{code}
param :: Parser Q.FuncParam
param = (Q.Env <$> (ws1 (string "env") >> local))
    <|> (string "..." >> pure Q.Variadic)
    <|> do
          ty <- ws1 abity
          Q.Regular ty <$> local

params :: Parser [Q.FuncParam]
params = between (ws $ char '(') (char ')') params'
  where
    params' = sepBy1 param (ws $ char ',')
\end{code}

The parameter list is a comma separated list of temporary names prefixed
by types. The types are used to correctly implement C compatibility.
When an argument has an aggregate type, a pointer to the aggregate is
passed by thea caller. In the example below, we have to use a load
instruction to get the value of the first (and only) member of the
struct.

\begin{verbatim}
type :one = { w }

function w $getone(:one %p) {
@start
        %val =w loadw %p
        ret %val
}
\end{verbatim}

If a function accepts or returns values that are smaller than a word,
such as \texttt{signed char} or \texttt{unsigned short} in C, one of the sub-word type
must be used. The sub-word types \texttt{sb}, \texttt{ub}, \texttt{sh}, and \texttt{uh} stand,
respectively, for signed and unsigned 8-bit values, and signed and
unsigned 16-bit values. Parameters associated with a sub-word type of
bit width N only have their N least significant bits set and have base
type \texttt{w}. For example, the function

\begin{verbatim}
function w $addbyte(w %a, sb %b) {
@start
        %bw =w extsb %b
        %val =w add %a, %bw
        ret %val
}
\end{verbatim}

needs to sign-extend its second argument before the addition. Dually,
return values with sub-word types do not need to be sign or zero
extended.

If the parameter list ends with \texttt{...}, the function is a variadic
function: it can accept a variable number of arguments. To access the
extra arguments provided by the caller, use the \texttt{vastart} and \texttt{vaarg}
instructions described in the \nameref{sec:variadic} section.

Optionally, the parameter list can start with an environment parameter
\texttt{env \%e}. This special parameter is a 64-bit integer temporary (i.e.,
of type \texttt{l}). If the function does not use its environment parameter,
callers can safely omit it. This parameter is invisible to a C caller:
for example, the function

\begin{verbatim}
export function w $add(env %e, w %a, w %b) {
@start
        %c =w add %a, %b
        ret %c
}
\end{verbatim}

must be given the C prototype \texttt{int add(int, int)}. The intended use of
this feature is to pass the environment pointer of closures while
retaining a very good compatibility with C. The \nameref{sec:call}
section explains how to pass an environment parameter.

Since global symbols are defined mutually recursive, there is no need
for function declarations: a function can be referenced before its
definition. Similarly, functions from other modules can be used without
previous declaration. All the type information necessary to compile a
call is in the instruction itself.

The syntax and semantics for the body of functions are described in the
\nameref{sec:control} section.

\section{Control}
\label{sec:control}

The IL represents programs as textual transcriptions of control flow
graphs. The control flow is serialized as a sequence of blocks of
straight-line code which are connected using jump instructions.

\subsection{Blocks}

\begin{code}
block :: Parser Q.Block
block = do
  l <- wsNL1 label
  s <- many (wsNL1 statement)
  Q.Block l s <$> (wsNL1 jumpInstr)
\end{code}

All blocks have a name that is specified by a label at their beginning.
Then follows a sequence of instructions that have "fall-through" flow.
Finally one jump terminates the block. The jump can either transfer
control to another block of the same function or return; jumps are
described further below.

The first block in a function must not be the target of any jump in the
program. If a jump to the function start is needed, the frontend must
insert an empty prelude block at the beginning of the function.

When one block jumps to the next block in the IL file, it is not
necessary to write the jump instruction, it will be automatically added
by the parser. For example the start block in the example below jumps
directly to the loop block.

\subsection{Statement}

% TODO: Not documented in the QBE BNF.

\begin{code}
instr :: Parser Q.Instr
instr =
  choice
    [ binaryInstr Q.Add "add",
      binaryInstr Q.Sub "sub"
    ]
  where
    binaryInstr conc keyword = do
      _ <- ws (string keyword)
      vfst <- ws val <* ws (char ',')
      conc vfst <$> ws val

assign :: Parser Q.Statement
assign = do
  n <- ws local
  t <- ws (char '=') >> ws1 baseType
  Q.Assign n t <$> instr

statement :: Parser Q.Statement
statement = assign
\end{code}

\subsection{Jumps}

\begin{code}
jumpInstr :: Parser Q.JumpInstr
jumpInstr = (string "hlt" >> pure Q.Halt)
        -- TODO: Return requires a space if there is an optionMaybe
        <|> Q.Return <$> ((ws $ string "ret") >> optionMaybe val)
        <|> try (Q.Jump <$> ((ws1 $ string "jmp") >> label))
        <|> do
          _ <- ws1 $ string "jnz"
          v <- ws val <* ws (char ',')
          l1 <- ws label <* ws (char ',')
          l2 <- ws1 label
          return $ Q.Jnz v l1 l2
\end{code}

A jump instruction ends every block and transfers the control to another
program location. The target of a jump must never be the first block in
a function. The three kinds of jumps available are described in the
following list.

\begin{enumerate}
  \item \textbf{Unconditional jump.} Simply jumps to another block of the same function.
  \item \textbf{Conditional jump.} When its word argument is non-zero, it jumps to its first label argument; otherwise it jumps to the other label. The argument must be of word type; because of subtyping a long argument can be passed, but only its least significant 32 bits will be compared to 0.
  \item \textbf{Function return.} Terminates the execution of the current function, optionally returning a value to the caller. The value returned must be of the type given in the function prototype. If the function prototype does not specify a return type, no return value can be used.
  \item \textbf{Program termination.} Terminates the execution of the program with a target-dependent error. This instruction can be used when it is expected that the execution never reaches the end of the block it closes; for example, after having called a function such as \texttt{exit()}.
\end{enumerate}

\section{Instructions}
\label{sec:instructions}

To-Do.

\subsection{Call}
\label{sec:call}

To-Do.

\subsection{Variadic}
\label{sec:variadic}

To-Do.

\end{document}
