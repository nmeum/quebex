% SPDX-FileCopyrightText: 2015-2024 Quentin Carbonneaux <quentin@c9x.me>
% SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
%
% SPDX-License-Identifier: MIT AND GPL-3.0-only

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

\title{QBE Intermediate Language\vspace{-2em}}
\date{}
\maketitle
\frenchspacing

\ignore{
\begin{code}
module Language.QBE.Parser
  ( skipInitComments,
    dataDef,
    typeDef,
    funcDef,
  )
where

import Data.Char (chr)
import Data.Word (Word64)
import Data.Functor ((<&>))
import Data.List (singleton)
import Data.Map qualified as Map
import qualified Language.QBE.Types as Q
import Language.QBE.Util (bind, decNumber, octNumber, float)
import Text.ParserCombinators.Parsec
  ( Parser,
    alphaNum,
    anyChar,
    between,
    char,
    choice,
    letter,
    many,
    many1,
    manyTill,
    newline,
    noneOf,
    oneOf,
    optional,
    optionMaybe,
    sepBy,
    sepBy1,
    skipMany,
    skipMany1,
    string,
    try,
    (<?>),
    (<|>),
  )
\end{code}
}

This an executable description of the
\href{https://c9x.me/compile/doc/il-v1.2.html}{QBE intermediate language},
specified through \href{https://hackage.haskell.org/package/parsec}{Parsec}
parser combinators and generated from a literate Haskell file. The description
is derived from the original QBE IL documentation, licensed under MIT.
Presently, this implementation targets version 1.2 of the QBE intermediate
language and aims to be equivalent with the original specification.

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
\end{code}
}

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

\subsection{Parser Combinators}

\ignore{
\begin{code}
bracesNL :: Parser a -> Parser a
bracesNL = between (wsNL $ char '{') (wsNL $ char '}')

quoted :: Parser a -> Parser a
quoted = let q = char '"' in between q q

sepByTrail1 :: Parser a -> Parser sep -> Parser [a]
sepByTrail1 p sep = do
  x <- p
  xs <- many (try $ sep >> p)
  _ <- optional sep
  return (x:xs)

sepByTrail :: Parser a -> Parser sep -> Parser [a]
sepByTrail p sep = sepByTrail1 p sep <|> return []

parenLst :: Parser a -> Parser [a]
parenLst p = between (ws $ char '(') (char ')') inner
  where
    inner = sepBy (ws p) (ws $ char ',')

unaryInstr :: (Q.Value -> Q.Instr) -> String -> Parser Q.Instr
unaryInstr conc keyword = do
  _ <- ws (string keyword)
  conc <$> ws val

binaryInstr :: (Q.Value -> Q.Value -> Q.Instr) -> String -> Parser Q.Instr
binaryInstr conc keyword = do
  _ <- ws (string keyword)
  vfst <- ws val <* ws (char ',')
  conc vfst <$> ws val

-- Can only appear in data and type definitions and hence allows newlines.
alignAny :: Parser Word64
alignAny = (ws1 (string "align")) >> wsNL decNumber
\end{code}
}

The original QBE specification defines the syntax using a BNF grammar. In
contrast, this document defines it using Parsec parser combinators. As such,
this specification is less formal but more accurate as the parsing code is
actually executable. Consequently, this specification also captures constructs
omitted in the original specification (e.g., \nameref{sec:identifiers}, or
\nameref{sec:strlit}). Nonetheless, the formal language recognized by these
combinators aims to be equivalent to the one of the BNF grammar.

\subsection{Identifiers}
\label{sec:identifiers}

% Ident is not documented in the original QBE specification.
% See https://c9x.me/git/qbe.git/tree/parse.c?h=v1.2#n304

\begin{code}
ident :: Parser String
ident = do
  start <- letter <|> oneOf "._"
  rest <- many (alphaNum <|> oneOf "$._")
  return $ start : rest
\end{code}

Identifiers for data, types, and functions can start with any ASCII letter or
the special characters \texttt{.} and \texttt{\_}. This initial character can
be followed by a sequence of zero or more alphanumeric characters and the
special characters \texttt{\$}, \texttt{.}, and \texttt{\_}.

\subsection{Sigils}

\begin{code}
userDef :: Parser Q.UserIdent
userDef = Q.UserIdent <$> (char ':' >> ident)

global :: Parser Q.GlobalIdent
global = Q.GlobalIdent <$> (char '$' >> ident)

local :: Parser Q.LocalIdent
local = Q.LocalIdent <$> (char '%' >> ident)

label :: Parser Q.BlockIdent
label = Q.BlockIdent <$> (char '@' >> ident)
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

\ignore{
\begin{code}
ws :: Parser a -> Parser a
ws p = p <* skipMany blank

ws1 :: Parser a -> Parser a
ws1 p = p <* skipMany1 blank

wsNL :: Parser a -> Parser a
wsNL p = p <* skipNoCode (skipMany blankNL)

wsNL1 :: Parser a -> Parser a
wsNL1 p = p <* skipNoCode (skipMany1 blankNL)

-- Only intended to be used to skip comments at the start of a file.
skipInitComments :: Parser ()
skipInitComments = skipNoCode (skipMany blankNL)
\end{code}
}

\subsection{String Literals}
\label{sec:strlit}

% The string literal is not documented in the original QBE specification.
% See https://c9x.me/git/qbe.git/tree/parse.c?h=v1.2#n287

\begin{code}
strLit :: Parser String
strLit = concat <$> quoted (many strChr)
  where
    strChr :: Parser [Char]
    strChr = (singleton <$> noneOf "\"\\") <|> escSeq

    -- TODO: not documnted in the QBE BNF.
    octEsc :: Parser Char
    octEsc = do
      n <- octNumber
      pure $ chr (fromIntegral n)

    escSeq :: Parser [Char]
    escSeq = try $ do
      esc <- char '\\'
      (singleton <$> octEsc) <|> (anyChar <&> (\c -> [esc, c]))
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
\label{sec:subtyping}

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

\begin{code}
dynConst :: Parser Q.DynConst
dynConst =
  (Q.Const <$> constant)
    <|> (Q.Thread <$> global)
    <?> "dynconst"
\end{code}

Constants come in two kinds: compile-time constants and dynamic
constants. Dynamic constants include compile-time constants and other
symbol variants that are only known at program-load time or execution
time. Consequently, dynamic constants can only occur in function bodies.

The representation of integers is two's complement.
Floating-point numbers are represented using the single-precision and
double-precision formats of the IEEE 754 standard.

\begin{code}
constant :: Parser Q.Const
constant =
  (Q.Number <$> decNumber)
    <|> (Q.SFP <$> sfp)
    <|> (Q.DFP <$> dfp)
    <|> (Q.Global <$> global)
    <?> "const"
  where
    sfp = string "s_" >> float
    dfp = string "d_" >> float
\end{code}

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

\begin{code}
linkage :: Parser Q.Linkage
linkage =
  wsNL (bind "export" Q.LExport)
    <|> wsNL (bind "thread" Q.LThread)
    <|> do
      _ <- ws1 $ string "section"
      (try secWithFlags) <|> sec
  where
    sec :: Parser Q.Linkage
    sec = wsNL strLit <&> (`Q.LSection` Nothing)

    secWithFlags :: Parser Q.Linkage
    secWithFlags = do
      n <- ws1 strLit
      wsNL strLit <&> Q.LSection n . Just
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
typeDef :: Parser Q.TypeDef
typeDef = do
  _ <- wsNL1 (string "type")
  i <- wsNL1 userDef
  _ <- wsNL1 (char '=')
  a <- optionMaybe alignAny
  bracesNL (opaqueType <|> unionType <|> regularType) <&> Q.TypeDef i a
\end{code}

Aggregate type definitions start with the \texttt{type} keyword. They have file
scope, but types must be defined before being referenced. The inner
structure of a type is expressed by a comma-separated list of fields.

\begin{code}
subType :: Parser Q.SubType
subType =
  (Q.SExtType <$> extType)
    <|> (Q.SUserDef <$> userDef)

field :: Parser Q.Field
field = do
  -- TODO: newline is required if there is a number argument
  f <- wsNL subType
  s <- ws $ optionMaybe decNumber
  pure (f, s)

fields :: Bool -> Parser [Q.Field]
fields allowEmpty =
  (if allowEmpty then sepByTrail else sepByTrail1) field (wsNL $ char ',')
\end{code}

A field consists of a subtype, either an extended type or a user-defined type,
and an optional number expressing the value of this field. In case many items
of the same type are sequenced (like in a C array), the shorter array syntax
can be used.

\begin{code}
regularType :: Parser Q.AggType
regularType = Q.ARegular <$> fields True
\end{code}

Three different kinds of aggregate types are presentl ysupported: regular
types, union types and opaque types. The fields of regular types will be
packed. By default, the alignment of an aggregate type is the maximum alignment
of its members. The alignment can be explicitly specified by the programmer.

\begin{code}
unionType :: Parser Q.AggType
unionType = Q.AUnion <$> many1 (wsNL unionType')
  where
    unionType' :: Parser [Q.Field]
    unionType' = bracesNL $ fields False
\end{code}

Union types allow the same chunk of memory to be used with different layouts. They are defined by enclosing multiple regular aggregate type bodies in a pair of curly braces. Size and alignment of union types are set to the maximum size and alignment of each variation or, in the case of alignment, can be explicitly specified.

\begin{code}
opaqueType :: Parser Q.AggType
opaqueType = Q.AOpaque <$> wsNL decNumber
\end{code}

Opaque types are used when the inner structure of an aggregate cannot be specified; the alignment for opaque types is mandatory. They are defined simply by enclosing their size between curly braces.

\subsubsection{Data}
\label{sec:data}

\begin{code}
dataDef :: Parser Q.DataDef
dataDef = do
  link <- many linkage
  name <- wsNL1 (string "data") >> wsNL global
  _ <- wsNL (char '=')
  alignment <- optionMaybe alignAny
  bracesNL dataObjs <&> Q.DataDef link name alignment
 where
    -- TODO: sepByTrail is not documented in the QBE BNF.
    dataObjs = sepByTrail dataObj (wsNL $ char ',')
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
\end{code}

Objects are described by a sequence of fields that start with a type
letter. This letter can either be an extended type, or the \texttt{z} letter.
If the letter used is an extended type, the data item following
specifies the bits to be stored in the field.

\begin{code}
dataItem :: Parser Q.DataItem
dataItem =
  (Q.DString <$> strLit)
    <|> try
      ( do
          i <- ws global
          off <- (ws $ char '+') >> ws decNumber
          return $ Q.DSymOff i off
      )
    <|> (Q.DConst <$> constant)
\end{code}

Within each object, several items can be defined. When several data items
follow a letter, they initialize multiple fields of the same size.

\begin{code}
allocSize :: Parser Q.AllocSize
allocSize =
  choice
    [ bind "4" Q.AllocWord,
      bind "8" Q.AllocLong,
      bind "16" Q.AllocLongLong
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
  body <- between (wsNL1 $ char '{') (wsNL $ char '}') $ many1 block

  case (Q.insertJumps body) of
    Nothing -> fail $ "invalid fallthrough in " ++ show name
    Just bl -> return $ Q.FuncDef link name retTy args bl
\end{code}

Function definitions contain the actual code to emit in the compiled
file. They define a global symbol that contains a pointer to the
function code. This pointer can be used in \texttt{call} instructions or stored
in memory.

\begin{code}
subWordType :: Parser Q.SubWordType
subWordType = choice
  [ try $ bind "sb" Q.SignedByte
  , try $ bind "ub" Q.UnsignedByte
  , bind "sh" Q.SignedHalf
  , bind "uh" Q.UnsignedHalf ]

abity :: Parser Q.Abity
abity = try (Q.ASubWordType <$> subWordType)
    <|> (Q.ABase <$> baseType)
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
params = parenLst param
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
\label{sec:blocks}

\begin{code}
block :: Parser Q.Block'
block = do
  l <- wsNL1 label
  p <- many (wsNL1 $ try phiInstr)
  s <- many (wsNL1 statement)
  Q.Block' l p s <$> (optionMaybe $ wsNL1 jumpInstr)
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

\subsection{Jumps}
\label{sec:jumps}

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
          l2 <- ws label
          return $ Q.Jnz v l1 l2
\end{code}

A jump instruction ends every block and transfers the control to another
program location. The target of a jump must never be the first block in
a function. The three kinds of jumps available are described in the
following list.

\begin{enumerate}
  \item \textbf{Unconditional jump.} Jumps to another block of the same function.
  \item \textbf{Conditional jump.} When its word argument is non-zero, it jumps to its first label argument; otherwise it jumps to the other label. The argument must be of word type; because of subtyping a long argument can be passed, but only its least significant 32 bits will be compared to 0.
  \item \textbf{Function return.} Terminates the execution of the current function, optionally returning a value to the caller. The value returned must be of the type given in the function prototype. If the function prototype does not specify a return type, no return value can be used.
  \item \textbf{Program termination.} Terminates the execution of the program with a target-dependent error. This instruction can be used when it is expected that the execution never reaches the end of the block it closes; for example, after having called a function such as \texttt{exit()}.
\end{enumerate}

\section{Instructions}
\label{sec:instructions}

\begin{code}
instr :: Parser Q.Instr
instr =
  choice
    [ try $ binaryInstr Q.Add "add",
      try $ binaryInstr Q.Sub "sub",
      try $ binaryInstr Q.Mul "mul",
      try $ binaryInstr Q.Div "div",
      try $ binaryInstr Q.URem "urem",
      try $ binaryInstr Q.Rem "rem",
      try $ binaryInstr Q.UDiv "udiv",
      try $ binaryInstr Q.Or "or",
      try $ binaryInstr Q.Xor "xor",
      try $ binaryInstr Q.And "and",
      try $ binaryInstr Q.Sar "sar",
      try $ binaryInstr Q.Shr "shr",
      try $ binaryInstr Q.Shl "shl",
      try $ unaryInstr Q.Neg "neg",
      try $ unaryInstr Q.Cast "cast",
      try $ unaryInstr Q.Copy "copy",
      try $ loadInstr,
      try $ allocInstr,
      try $ compareInstr,
      try $ extInstr,
      try $ truncInstr
    ]
\end{code}

Instructions are the smallest piece of code in the IL, they form the body of
\nameref{sec:blocks}. This specification distinguishes instructions and
volatile instructions, the latter do not return a value. For the former, the IL
uses a three-address code, which means that one instruction computes an
operation between two operands and assigns the result to a third one.

\begin{code}
assign :: Parser Q.Statement
assign = do
  n <- ws local
  t <- ws (char '=') >> ws1 baseType
  Q.Assign n t <$> instr

volatileInstr :: Parser Q.Statement
volatileInstr = Q.Volatile <$> (storeInstr <|> blitInstr)

-- TODO: Not documented in the QBE BNF.
statement :: Parser Q.Statement
statement = (try callInstr) <|> assign <|> volatileInstr
\end{code}

An instruction has both a name and a return type, this return type is a base
type that defines the size of the instruction's result. The type of the
arguments can be unambiguously inferred using the instruction name and the
return type. For example, for all arithmetic instructions, the type of the
arguments is the same as the return type. The two additions below are valid if
\texttt{\%y} is a word or a long (because of \nameref{sec:subtyping}).

\begin{verbatim}
%x =w add 0, %y
%z =w add %x, %x
\end{verbatim}

Some instructions, like comparisons and memory loads have operand types
that differ from their return types. For instance, two floating points
can be compared to give a word result (0 if the comparison succeeds, 1
if it fails).

\begin{verbatim}
%c =w cgts %a, %b
\end{verbatim}

In the example above, both operands have to have single type. This is
made explicit by the instruction suffix.

\subsection{Arithmetic and Bits}

\begin{quote}
\begin{itemize}
\item \texttt{add}, \texttt{sub}, \texttt{div}, \texttt{mul}
\item \texttt{neg}
\item \texttt{udiv}, \texttt{rem}, \texttt{urem}
\item \texttt{or}, \texttt{xor}, \texttt{and}
\item \texttt{sar}, \texttt{shr}, \texttt{shl}
\end{itemize}
\end{quote}

The base arithmetic instructions in the first bullet are available for
all types, integers and floating points.

When \texttt{div} is used with word or long return type, the arguments are
treated as signed. The unsigned integral division is available as \texttt{udiv}
instruction. When the result of a division is not an integer, it is truncated
towards zero.

The signed and unsigned remainder operations are available as \texttt{rem} and
\texttt{urem}. The sign of the remainder is the same as the one of the
dividend. Its magnitude is smaller than the divisor one. These two instructions
and \texttt{udiv} are only available with integer arguments and result.

Bitwise OR, AND, and XOR operations are available for both integer
types. Logical operations of typical programming languages can be
implemented using \nameref{sec:comparisions} and \nameref{sec:jumps}.

Shift instructions \texttt{sar}, \texttt{shr}, and \texttt{shl}, shift right or
left their first operand by the amount from the second operand. The shifting
amount is taken modulo the size of the result type. Shifting right can either
preserve the sign of the value (using \texttt{sar}), or fill the newly freed
bits with zeroes (using \texttt{shr}). Shifting left always fills the freed
bits with zeroes.

Remark that an arithmetic shift right (\texttt{sar}) is only equivalent to a
division by a power of two for non-negative numbers. This is because the shift
right "truncates" towards minus infinity, while the division truncates towards
zero.

\subsection{Memory}
\label{sec:memory}

The following sections discuss instructions for interacting with values stored in memory.

\subsubsection{Store instructions}

\begin{code}
storeInstr :: Parser Q.VolatileInstr
storeInstr = do
  t <- string "store" >> ws1 extType
  v <- ws val
  _ <- ws $ char ','
  ws val <&> Q.Store t v
\end{code}

Store instructions exist to store a value of any base type and any extended
type. Since halfwords and bytes are not first class in the IL, \texttt{storeh}
and \texttt{storeb} take a word as argument. Only the first 16 or 8 bits of
this word will be stored in memory at the address specified in the second
argument.

\subsubsection{Load instructions}

\begin{code}
loadInstr :: Parser Q.Instr
loadInstr = do
  _ <- string "load"
  t <- ws1 $ choice
    [ try $ bind "sw" (Q.LBase Q.Word),
      try $ bind "uw" (Q.LBase Q.Word),
      try $ Q.LSubWord <$> subWordType,
      Q.LBase <$> baseType
    ]
  ws val <&> Q.Load t
\end{code}

For types smaller than long, two variants of the load instruction are
available: one will sign extend the loaded value, while the other will zero
extend it. Note that all loads smaller than long can load to either a long or a
word.

The two instructions \texttt{loadsw} and \texttt{loaduw} have the same effect
when they are used to define a word temporary. A \texttt{loadw} instruction is
provided as syntactic sugar for \texttt{loadsw} to make explicit that the
extension mechanism used is irrelevant.

\subsubsection{Blits}

\begin{code}
blitInstr :: Parser Q.VolatileInstr
blitInstr = do
  v1 <- (ws1 $ string "blit") >> ws val <* (ws $ char ',')
  v2 <- ws val <* (ws $ char ',')
  nb <- decNumber
  return $ Q.Blit v1 v2 nb
\end{code}

The blit instruction copies in-memory data from its first address argument to
its second address argument. The third argument is the number of bytes to copy.
The source and destination spans are required to be either non-overlapping, or
fully overlapping (source address identical to the destination address). The
byte count argument must be a nonnegative numeric constant; it cannot be a
temporary.

One blit instruction may generate a number of instructions proportional to its
byte count argument, consequently, it is recommended to keep this argument
relatively small. If large copies are necessary, it is preferable that
frontends generate calls to a supporting \texttt{memcpy} function.

\subsubsection{Stack Allocation}

\begin{code}
allocInstr :: Parser Q.Instr
allocInstr = do
  siz <- (ws $ string "alloc") >> (ws1 allocSize)
  val <&> Q.Alloc siz
\end{code}

These instructions allocate a chunk of memory on the stack. The number ending
the instruction name is the alignment required for the allocated slot. QBE will
make sure that the returned address is a multiple of that alignment value.

Stack allocation instructions are used, for example, when compiling the C local
variables, because their address can be taken. When compiling Fortran,
temporaries can be used directly instead, because it is illegal to take the
address of a variable.

\subsection{Comparisons}
\label{sec:comparisions}

Comparison instructions return an integer value (either a word or a long), and
compare values of arbitrary types. The returned value is 1 if the two operands
satisfy the comparison relation, or 0 otherwise. The names of comparisons
respect a standard naming scheme in three parts.

\begin{code}
compareInstr :: Parser Q.Instr
compareInstr = do
  _ <- char 'c'
  op <- compareOp
  ty <- ws1 baseType
  lhs <- ws val <* ws (char ',')
  rhs <- ws val
  pure $ Q.Compare ty op lhs rhs
\end{code}

\begin{code}
compareOp :: Parser Q.CmpOp
compareOp = choice
  [ bind "eq" Q.CEq
  , bind "ne" Q.CNe
  , try $ bind "sle" Q.CSle
  , try $ bind "slt" Q.CSlt
  , try $ bind "sge" Q.CSge
  , try $ bind "sgt" Q.CSgt
  , try $ bind "ule" Q.CUle
  , try $ bind "ult" Q.CUlt
  , try $ bind "uge" Q.CUge
  , try $ bind "ugt" Q.CUgt ]
\end{code}

For example, \texttt{cod} compares two double-precision floating point numbers
and returns 1 if the two floating points are not NaNs, or 0 otherwise. The
\texttt{csltw} instruction compares two words representing signed numbers and
returns 1 when the first argument is smaller than the second one.

\subsection{Conversions}

\begin{code}
extInstr :: Parser Q.Instr
extInstr = do
  _ <- string "ext"
  (try extInt) <|> extFloat
 where
  subLongType :: Parser Q.SubLongType
  subLongType = try (Q.SLSubWord <$> subWordType)
    <|> bind "sw" Q.SLSignedWord
    <|> bind "uw" Q.SLUnsignedWord

  extInt :: Parser Q.Instr
  extInt = do
    ty <- ws1 subLongType
    ws val <&> Q.Ext ty

  extFloat :: Parser Q.Instr
  extFloat = do
    _ <- ws1 $ char 's'
    ws val <&> Q.ExtSingle

truncInstr :: Parser Q.Instr
truncInstr = do
  _ <- ws1 $ string "truncd"
  ws val <&> Q.TruncDouble
\end{code}

Conversion operations change the representation of a value, possibly modifying
it if the target type cannot hold the value of the source type. Conversions can
extend the precision of a temporary (e.g., from signed 8-bit to 32-bit), or
convert a floating point into an integer and vice versa.

\subsection{Cast and Copy}

The \texttt{cast} and \texttt{copy} instructions return the bits of their
argument verbatim. However a cast will change an integer into a floating point
of the same width and vice versa.

Casts can be used to make bitwise operations on the representation of floating
point numbers. For example the following program will compute the opposite of
the single-precision floating point number \texttt{\%f} into \texttt{\%rs}.

\begin{verbatim}
%b0 =w cast %f
%b1 =w xor 2147483648, %b0  # flip the msb
%rs =s cast %b1
\end{verbatim}

\subsection{Call}
\label{sec:call}

\begin{code}
-- TODO: Code duplication with 'param'.
callArg :: Parser Q.FuncArg
callArg = (Q.ArgEnv <$> (ws1 (string "env") >> val))
    <|> (string "..." >> pure Q.ArgVar)
    <|> do
          ty <- ws1 abity
          Q.ArgReg ty <$> val

callArgs :: Parser [Q.FuncArg]
callArgs = parenLst callArg

callInstr :: Parser Q.Statement
callInstr = do
  retValue <- optionMaybe $ do
    i <- ws local <* ws (char '=')
    a <- ws1 abity
    return (i, a)
  toCall <- ws1 (string "call") >> ws val
  fnArgs <- callArgs
  return $ Q.Call retValue toCall fnArgs
\end{code}

The call instruction is special in several ways. It is not a three-address
instruction and requires the type of all its arguments to be given. Also, the
return type can be either a base type or an aggregate type. These specifics are
required to compile calls with C compatibility (i.e., to respect the ABI).

When an aggregate type is used as argument type or return type, the value
respectively passed or returned needs to be a pointer to a memory location
holding the value. This is because aggregate types are not first-class
citizens of the IL.

Sub-word types are used for arguments and return values of width less than a
word. Details on these types are presented in the \nameref{sec:functions} section.
Arguments with sub-word types need not be sign or zero extended according to
their type. Calls with a sub-word return type define a temporary of base type
\texttt{w} with its most significant bits unspecified.

Unless the called function does not return a value, a return temporary must be
specified, even if it is never used afterwards.

An environment parameter can be passed as first argument using the \texttt{env}
keyword. The passed value must be a 64-bit integer. If the called function does
not expect an environment parameter, it will be safely discarded. See the
\nameref{sec:functions} section for more information about environment
parameters.

When the called function is variadic, there must be a \texttt{...} marker
separating the named and variadic arguments.

\subsection{Variadic}
\label{sec:variadic}

To-Do.

\subsection{Phi}

\begin{code}
phiBranch :: Parser (Q.BlockIdent, Q.Value)
phiBranch = do
  n <- ws1 label
  v <- val
  pure (n, v)

phiInstr :: Parser Q.Phi
phiInstr = do
  -- TODO: code duplication with 'assign'
  n <- ws local
  t <- ws (char '=') >> ws1 baseType

  _ <- ws1 (string "phi")
  -- TODO: combinator for sepBy
  p <- Map.fromList <$> sepBy1 (ws phiBranch) (ws $ char ',')
  return $ Q.Phi n t p
\end{code}

First and foremost, phi instructions are NOT necessary when writing a frontend
to QBE. One solution to avoid having to deal with SSA form is to use stack
allocated variables for all source program variables and perform assignments
and lookups using \nameref{sec:memory} operations. This is what LLVM users
typically do.

Another solution is to simply emit code that is not in SSA form! Contrary to
LLVM, QBE is able to fixup programs not in SSA form without requiring the
boilerplate of loading and storing in memory. For example, the following
program will be correctly compiled by QBE.

\begin{verbatim}
@start
    %x =w copy 100
    %s =w copy 0
@loop
    %s =w add %s, %x
    %x =w sub %x, 1
    jnz %x, @loop, @end
@end
    ret %s
\end{verbatim}

Now, if you want to know what phi instructions are and how to use them in QBE,
you can read the following.

Phi instructions are specific to SSA form. In SSA form values can only be
assigned once, without phi instructions, this requirement is too strong to
represent many programs. For example consider the following C program.

\begin{verbatim}
int f(int x) {
    int y;
    if (x)
        y = 1;
    else
        y = 2;
    return y;
}
\end{verbatim}

The variable \texttt{y} is assigned twice, the solution to translate it in SSA
form is to insert a phi instruction.

\begin{verbatim}
@ifstmt
    jnz %x, @ift, @iff
@ift
    jmp @retstmt
@iff
    jmp @retstmt
@retstmt
    %y =w phi @ift 1, @iff 2
    ret %y
\end{verbatim}

Phi instructions return one of their arguments depending on where the control
came from. In the example, \texttt{\%y} is set to 1 if the
\texttt{\textbackslash{}ift} branch is taken, or it is set to 2 otherwise.

An important remark about phi instructions is that QBE assumes that if a
variable is defined by a phi it respects all the SSA invariants. So it is
critical to not use phi instructions unless you know exactly what you are
doing.
\end{document}
