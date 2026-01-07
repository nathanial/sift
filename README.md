# Sift

A Parsec-style parser combinator library for Lean 4.

## Installation

Add to your `lakefile.lean`:

```lean
require sift from git "https://github.com/nathanial/sift" @ "v0.0.1"
```

## Usage

```lean
import Sift

open Sift

-- Parse a number
def number : Parser Nat := natural

-- Parse an identifier
def ident : Parser String := identifier

-- Parse a simple expression: number or (expr)
partial def expr : Parser Nat :=
  natural <|> parens expr

-- Run a parser
#eval Parser.run number "42"        -- .ok 42
#eval Parser.run ident "foo123"     -- .ok "foo123"
#eval Parser.parse number "42"      -- .ok 42 (requires all input consumed)
```

## Core Types

```lean
-- Parser state tracks position in input
structure ParseState where
  input : String
  pos : Nat
  line : Nat
  column : Nat

-- Errors include position and expected items
structure ParseError where
  pos : SourcePos
  message : String
  expected : List String

-- The parser monad
def Parser (α : Type) := ParseState → Except ParseError (α × ParseState)
```

## Primitives

| Function | Description |
|----------|-------------|
| `satisfy pred` | Match char satisfying predicate |
| `char c` | Match specific character |
| `anyChar` | Match any character |
| `string s` | Match exact string |
| `eof` | Match end of input |
| `peek` | Look at next char without consuming |
| `take n` | Take exactly n characters |
| `takeWhile pred` | Take chars while predicate holds |
| `skipWhile pred` | Skip chars while predicate holds |

## Combinators

| Function | Description |
|----------|-------------|
| `attempt p` / `try p` | Backtrack on failure |
| `p <\|> q` | Try p, then q if p fails without consuming |
| `choice [p, q, ...]` | Try parsers in order |
| `optional p` | Parse optionally, return `Option` |
| `many p` | Zero or more |
| `many1 p` | One or more |
| `count n p` | Exactly n occurrences |
| `sepBy p sep` | Zero or more separated by sep |
| `sepBy1 p sep` | One or more separated by sep |
| `between open close p` | Parse p between delimiters |
| `chainl1 p op` | Left-associative chain |
| `chainr1 p op` | Right-associative chain |
| `notFollowedBy p` | Succeed if p fails |
| `lookAhead p` | Parse without consuming |
| `manyTill p end` | Parse until end matches |
| `p <?> "name"` | Label parser for error messages |

## Character Classes

| Function | Description |
|----------|-------------|
| `digit` | [0-9] |
| `letter` | [a-zA-Z] |
| `alphaNum` | [a-zA-Z0-9] |
| `lower` / `upper` | [a-z] / [A-Z] |
| `space` / `whitespace` | Space / any whitespace |
| `spaces` / `spaces1` | Skip whitespace |
| `newline` / `tab` | Specific whitespace |
| `oneOf "abc"` | Any char in string |
| `noneOf "abc"` | Any char not in string |
| `hexDigit` / `octDigit` | Hex/octal digits |

## Text Utilities

| Function | Description |
|----------|-------------|
| `natural` | Parse unsigned integer |
| `integer` | Parse signed integer |
| `hexNatural` | Parse hex number (no prefix) |
| `identifier` | Parse identifier (letter/_ followed by alphanum/_) |
| `lexeme p` | Parse p, skip trailing whitespace |
| `symbol s` | Parse string, skip trailing whitespace |
| `parens p` | Parse between `(` and `)` |
| `braces p` | Parse between `{` and `}` |
| `brackets p` | Parse between `[` and `]` |
| `commaSep p` | Comma-separated list |
| `stringLiteral` | Double-quoted string with escapes |
| `charLiteral` | Single-quoted character |

## Example: Expression Parser

```lean
import Sift

open Sift

-- Simple arithmetic expressions
partial def expr : Parser Int := term >>= rest
where
  term : Parser Int := factor >>= termRest
  factor : Parser Int :=
    (integer <* spaces) <|> parens expr
  termRest (acc : Int) : Parser Int := do
    let op ← optional (oneOf "+-" <* spaces)
    match op with
    | none => pure acc
    | some '+' => do let b ← factor; termRest (acc + b)
    | some '-' => do let b ← factor; termRest (acc - b)
    | _ => pure acc
  rest (acc : Int) : Parser Int := do
    let op ← optional (oneOf "*/" <* spaces)
    match op with
    | none => pure acc
    | some '*' => do let b ← term; rest (acc * b)
    | some '/' => do let b ← term; rest (acc / b)
    | _ => pure acc

#eval Parser.run expr "1 + 2 * 3"  -- .ok 7
```

## Backtracking

By default, `<|>` only tries the alternative if the first parser fails **without consuming input**. Use `attempt` (or `try`) to enable backtracking:

```lean
-- This fails on "ab" because "abc" consumes "ab" before failing
let p := string "abc" <|> string "ab"
Parser.run p "ab"  -- error

-- This succeeds because attempt resets position on failure
let p := attempt (string "abc") <|> string "ab"
Parser.run p "ab"  -- .ok "ab"
```

## License

MIT
