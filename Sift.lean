import Sift.Core
import Sift.Primitives
import Sift.Combinators
import Sift.Char
import Sift.Text
import Sift.Prec
import Sift.Indent
import Sift.Debug

/-!
# Sift - Parser Combinator Library

A Parsec-style parser combinator library for Lean 4.

## Usage

```lean
import Sift

open Sift

-- Parse a simple expression
def digit : Parser Char := satisfy Char.isDigit
def number : Parser Nat := do
  let digits ← many1 digit
  pure (digits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0)

#eval Parser.run number "123"  -- .ok 123
```
-/
