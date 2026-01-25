# CLAUDE.md - Sift Parser Combinator Library

Parsec-style parser combinator library for Lean 4 with full UTF-8 support.

## Build and Test

```bash
lake build          # Build library
lake test           # Run test suite
```

Uses `crucible` test framework. Tests are in `SiftTests/`.

## Architecture Overview

```
Sift/
  Core.lean         # Parser monad, ParseState, ParseError, run functions
  Primitives.lean   # satisfy, char, string, anyChar, eof, peek, take, takeWhile
  Combinators.lean  # attempt, orElse, many, sepBy, chainl/r, between, manyTill
  Char.lean         # Character classes: digit, letter, space, oneOf, hexDigit
  Text.lean         # Text utilities: natural, integer, identifier, lexeme, stringLiteral
  Prec.lean         # Precedence climbing for expression parsing (buildExprParser equivalent)
  Indent.lean       # Indentation-sensitive parsing for Python-style blocks
Sift.lean           # Main import, re-exports all modules
```

## Core Types

### ParseState

```lean
structure ParseState (sigma : Type) where
  input : String
  pos : Nat              -- Byte position (UTF-8)
  line : Nat := 1        -- 1-indexed
  column : Nat := 1      -- 1-indexed, counts characters not bytes
  userState : sigma      -- Custom user state
```

### Parser Monad

```lean
def Parser (sigma : Type) (alpha : Type) := ParseState sigma -> Except ParseError (alpha * ParseState sigma)
```

**Type parameter sigma**: User-defined state. Use `Unit` for stateless parsing:
```lean
def myParser : Parser Unit Nat := natural
Parser.run myParser "123"  -- Uses Unit state
```

For stateful parsing (e.g., indentation):
```lean
def myParser : Parser IndentState Expr := ...
Parser.runWith myParser input IndentState.init
```

### Running Parsers

| Function | Description |
|----------|-------------|
| `Parser.run p input` | Run with `Unit` state, may leave trailing input |
| `Parser.runWith p input state` | Run with custom initial state |
| `Parser.parse p input` | Run with `Unit` state, **requires all input consumed** |
| `Parser.parseWith p input state` | Run with custom state, requires all input consumed |
| `Parser.runWithState p input` | Returns `(result, finalState)` pair |

## Key APIs

### Primitives (Sift/Primitives.lean)

```lean
satisfy (pred : Char -> Bool) : Parser sigma Char
char (c : Char) : Parser sigma Char
anyChar : Parser sigma Char
string (s : String) : Parser sigma String
stringCI (s : String) : Parser sigma String     -- Case-insensitive
charCI (c : Char) : Parser sigma Char           -- Case-insensitive
eof : Parser sigma Unit
peek : Parser sigma (Option Char)               -- Non-consuming
peek! : Parser sigma Char                       -- Fails on EOF
skip : Parser sigma Unit
take (n : Nat) : Parser sigma String
takeWhile (pred : Char -> Bool) : Parser sigma String
takeWhile1 (pred : Char -> Bool) : Parser sigma String
skipWhile (pred : Char -> Bool) : Parser sigma Unit
skipWhile1 (pred : Char -> Bool) : Parser sigma Unit
atEnd : Parser sigma Bool
peekString (n : Nat) : Parser sigma (Option String)
```

### Combinators (Sift/Combinators.lean)

```lean
attempt p / try p              -- Backtrack on failure
p <|> q                        -- Alternative (only if p fails without consuming)
choice [p, q, r]               -- Try list in order
optional p : Parser sigma (Option alpha)
many p : Parser sigma (Array alpha)
many1 p : Parser sigma (Array alpha)
skipMany p / skipMany1 p       -- many/many1 discarding results
count n p                      -- Exactly n occurrences
manyTill p endp                -- Until endp matches (doesn't consume end)
sepBy p sep / sepBy1 p sep     -- Separated lists
endBy p sep / endBy1 p sep     -- Ended by separator
sepEndBy p sep / sepEndBy1 p sep
between open close p           -- Parse between delimiters
notFollowedBy p                -- Negative lookahead
lookAhead p                    -- Positive lookahead (non-consuming)
chainl1 p op / chainl p op default   -- Left-associative chain
chainr1 p op / chainr p op default   -- Right-associative chain
p <?> "label"                  -- Label for error messages
withFilter p pred msg          -- Filter result with predicate
```

### Character Classes (Sift/Char.lean)

```lean
digit, letter, alphaNum, lower, upper
space, whitespace, hspace      -- space only / any whitespace / horizontal only
spaces, spaces1                -- Skip whitespace
hspaces, hspaces1              -- Skip horizontal whitespace only (not newlines)
newline, cr, eol, tab
oneOf "chars" / noneOf "chars"
hexDigit, octDigit, binDigit
ascii, printable, control
```

### Text Utilities (Sift/Text.lean)

```lean
manyChars p / many1Chars p     -- Collect chars into String
natural : Parser sigma Nat      -- Unsigned integer
integer : Parser sigma Int      -- Signed integer
hexNatural / binNatural / octNatural
float : Parser sigma Float      -- Full float with optional exponent
decimal : Parser sigma Float    -- Requires decimal point, no exponent
scientific : Parser sigma Float -- Requires exponent
hexDigitsN n                   -- Exactly n hex digits as Nat
unicodeEscape4 / unicodeEscape8
digitsWithUnderscores pred     -- "1_000_000" style (Rust/Python/TOML)
decimalWithUnderscores / hexWithUnderscores
identifier                     -- letter/_ followed by alphanum/_
lexeme p                       -- p followed by spaces
symbol s                       -- string s followed by spaces
parens p / braces p / brackets p / angles p
commaSep p / commaSep1 p
semiSep p / semiSep1 p
escapeChar quote               -- Common escape sequences
stringLiteral                  -- Double-quoted with escapes
charLiteral                    -- Single-quoted with escapes
```

## Expression Parsing with Precedence Climbing (Sift/Prec.lean)

**IMPORTANT**: The `prec` function already implements full expression parsing with operator precedence. This is equivalent to Parsec's `buildExprParser`. Do not reimplement this functionality.

### Operator Types

```lean
inductive Assoc where
  | left   -- a + b + c = (a + b) + c
  | right  -- a ^ b ^ c = a ^ (b ^ c)
  | none   -- Non-associative

structure BinOpSpec (sigma alpha : Type) where
  parser : Parser sigma Unit     -- Matches operator (include trailing whitespace)
  prec : Nat                     -- Higher = binds tighter
  assoc : Assoc := .left
  combine : alpha -> alpha -> alpha

structure PrefixOpSpec (sigma alpha : Type) where
  parser : Parser sigma Unit
  prec : Nat
  apply : alpha -> alpha

structure PostfixOpSpec (sigma alpha : Type) where
  parser : Parser sigma Unit
  prec : Nat
  apply : alpha -> alpha

structure OpTable (sigma alpha : Type) where
  binOps : List (BinOpSpec sigma alpha)
  prefixOps : List (PrefixOpSpec sigma alpha) := []
  postfixOps : List (PostfixOpSpec sigma alpha) := []
```

### Usage Pattern

```lean
inductive Expr where
  | num : Int -> Expr
  | add : Expr -> Expr -> Expr
  | mul : Expr -> Expr -> Expr
  | neg : Expr -> Expr

def sym (s : String) : Parser Unit Unit := string s *> spaces

def opTable : OpTable Unit Expr := {
  binOps := [
    { parser := sym "+", prec := 1, combine := Expr.add },
    { parser := sym "*", prec := 2, combine := Expr.mul }
  ]
  prefixOps := [
    { parser := sym "-", prec := 3, apply := Expr.neg }
  ]
}

def atom : Parser Unit Expr := Expr.num <$> integer

-- This handles all precedence, associativity, prefix/postfix automatically
def parseExpr : Parser Unit Expr := prec opTable atom
```

For binary-only operators, use the simpler `precBinary`:
```lean
def parseExpr := precBinary opTable.binOps atom
```

## Indentation-Sensitive Parsing (Sift/Indent.lean)

For Python-style block structure using indentation.

### Basic Position Primitives (work with any state type)

```lean
getColumn : Parser sigma Nat           -- Current column (1-indexed)
getLine : Parser sigma Nat             -- Current line (1-indexed)
atColumn col : Parser sigma Unit       -- Assert at specific column
indented refCol : Parser sigma Unit    -- Assert column > reference
onLine refLine : Parser sigma Unit     -- Assert on specific line
measureIndent : Parser sigma Nat       -- Count leading spaces from col 1
skipToEol : Parser sigma Unit          -- Skip to end of line
```

### Full Indent Stack (requires IndentState)

```lean
structure IndentState where
  indentStack : List Nat := [0]

IndentState.init : IndentState
IndentState.currentIndent : Nat
IndentState.pushIndent (n : Nat) : IndentState
IndentState.popTo (n : Nat) : IndentState * Nat

-- Parser combinators
getCurrentIndent : Parser IndentState Nat
pushIndentLevel (n : Nat) : Parser IndentState Unit
popToIndentLevel (n : Nat) : Parser IndentState Nat
processIndent : Parser IndentState IndentChange
block (p : Parser IndentState alpha) : Parser IndentState (Array alpha)
sameLevel (p : Parser IndentState alpha) : Parser IndentState (Array alpha)
blockLines (parseLine : Parser IndentState alpha) : Parser IndentState (Array alpha)
```

### Usage Pattern

```lean
def parseIf : Parser IndentState Stmt := do
  let _ <- string "if"
  hspaces1
  let cond <- identifier
  let _ <- char ':'
  eol
  let body <- block parseStmt
  pure (.ifStmt cond body)

-- Run with indent state
Parser.runWith parseIf "if x:\n    y\n    z" IndentState.init
```

## Important Implementation Notes

1. **Backtracking behavior**: `<|>` only tries alternative if first parser fails **without consuming input**. Use `attempt`/`try` for full backtracking.

2. **UTF-8 handling**:
   - `pos` tracks **byte offset**, not character count
   - `column` tracks **character count** (for error messages)
   - All character parsers handle multi-byte UTF-8 correctly

3. **User state type parameter**: All parsers are parameterized by `sigma`. For simple parsing use `Unit`. Indent parsing requires `IndentState`.

4. **Error merging**: Errors at the same position have their `expected` lists merged for better messages.

5. **Fuel-based recursion**: `many`, `manyTill`, etc. use remaining input size as fuel to ensure termination without `partial`.

6. **Operator parser ordering**: Put longer/more specific operators first in `binOps` list (e.g., `++` before `+`).

7. **hspaces vs spaces**: Use `hspaces` when you want to skip only horizontal whitespace (spaces/tabs) but not newlines.

## Regex Integration with Rune

Sift cannot directly depend on rune (circular dependency: rune uses sift for regex parsing). However, projects that use both libraries can create a regex combinator:

```lean
import Sift
import Rune

namespace Sift

/-- Match a compiled regex at the current position -/
def regex {σ : Type} (re : Rune.Regex) : Parser σ String := fun s =>
  match re.matchAt s.input s.pos with
  | some m =>
    -- Update position to end of match
    -- Note: Need to track line/column changes through matched text
    let matched := m.text
    let newState := matched.foldl (fun st c =>
      if c == '\n' then { st with line := st.line + 1, column := 1 }
      else { st with column := st.column + 1 }
    ) { s with pos := m.stop }
    .ok (matched, newState)
  | none =>
    .error (ParseError.fromState s s!"regex '{re.getPattern}' did not match")

/-- Match regex and return capture groups -/
def regexCaptures {σ : Type} (re : Rune.Regex) : Parser σ (String × Array (Option String)) := fun s =>
  match re.matchAt s.input s.pos with
  | some m =>
    let matched := m.text
    let newState := matched.foldl (fun st c =>
      if c == '\n' then { st with line := st.line + 1, column := 1 }
      else { st with column := st.column + 1 }
    ) { s with pos := m.stop }
    let captures := (List.range m.numCaptures).map (fun i => m.group (i + 1)) |>.toArray
    .ok ((matched, captures), newState)
  | none =>
    .error (ParseError.fromState s s!"regex '{re.getPattern}' did not match")

end Sift
```

Usage:
```lean
let emailRe := Rune.Regex.compile! "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
let parseEmail : Parser Unit String := regex emailRe
```

**Key points:**
- Use `Rune.Regex.matchAt` (not `find`) to match at current position only
- Update line/column tracking when advancing through matched text
- Rune v0.0.3+ provides `matchAt` and `matchPrefix` for position-specific matching

## Testing Patterns

Tests use the `crucible` framework:

```lean
import Crucible
import Sift

open Crucible
open Sift

testSuite "My Tests"

test "parser succeeds" := do
  match Parser.run myParser "input" with
  | .ok result => result === expectedValue
  | .error _ => panic! "expected success"

test "parser fails" := do
  match Parser.run myParser "bad input" with
  | .error e => shouldContainSubstr (toString e) "expected message"
  | .ok _ => panic! "expected failure"
```

Key test files:
- `SiftTests/Core.lean` - Parser monad, run functions, position tracking
- `SiftTests/Primitives.lean` - satisfy, char, string, take, etc.
- `SiftTests/Combinators.lean` - attempt, many, sepBy, chainl/r, etc.
- `SiftTests/Char.lean` - Character class parsers
- `SiftTests/Text.lean` - Numbers, identifiers, literals
- `SiftTests/Prec.lean` - Expression parsing with precedence
- `SiftTests/Indent.lean` - Indentation-sensitive parsing
- `SiftTests/Utf8.lean` - Multi-byte character handling

## Dependencies

- `crucible` (test framework)

## Version

Current: v0.0.10
