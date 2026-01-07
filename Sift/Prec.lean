import Sift.Core
import Sift.Combinators

/-!
# Sift.Prec

Precedence climbing parser combinator for expression parsing.

This module provides a table-driven precedence climbing algorithm that handles:
- Binary operators with different precedence levels
- Left, right, and non-associative operators
- Prefix unary operators
- Postfix unary operators

## Usage

```lean
import Sift
import Sift.Prec

open Sift

-- Define an expression type
inductive Expr where
  | num : Int → Expr
  | add : Expr → Expr → Expr
  | mul : Expr → Expr → Expr
  | neg : Expr → Expr

-- Define operators
def opTable : OpTable Unit Expr := {
  binOps := [
    { parser := symbol "+" *> pure (), prec := 1, combine := Expr.add },
    { parser := symbol "*" *> pure (), prec := 2, combine := Expr.mul }
  ]
  prefixOps := [
    { parser := symbol "-" *> pure (), prec := 3, apply := Expr.neg }
  ]
}

-- Parse expressions
def parseExpr : Parser Unit Expr := prec opTable parseAtom
```
-/

namespace Sift

/-- Operator associativity -/
inductive Assoc where
  | left   -- a + b + c = (a + b) + c
  | right  -- a ^ b ^ c = a ^ (b ^ c)
  | none   -- a < b < c is invalid (requires explicit parens)
  deriving Repr, BEq, Inhabited

/-- Binary operator specification -/
structure BinOpSpec (σ α : Type) where
  /-- Parser that matches the operator (should consume the operator and any trailing whitespace) -/
  parser : Parser σ Unit
  /-- Precedence level (higher binds tighter) -/
  prec : Nat
  /-- Associativity (default: left) -/
  assoc : Assoc := .left
  /-- Function to combine left and right operands into result -/
  combine : α → α → α

/-- Prefix unary operator specification -/
structure PrefixOpSpec (σ α : Type) where
  /-- Parser that matches the operator -/
  parser : Parser σ Unit
  /-- Precedence level (determines binding in nested prefix: --x) -/
  prec : Nat
  /-- Function to apply to the operand -/
  apply : α → α

/-- Postfix unary operator specification -/
structure PostfixOpSpec (σ α : Type) where
  /-- Parser that matches the operator -/
  parser : Parser σ Unit
  /-- Precedence level -/
  prec : Nat
  /-- Function to apply to the operand -/
  apply : α → α

/-- Operator table for expression parsing -/
structure OpTable (σ α : Type) where
  /-- Binary operators (tried in order, so put longer/more specific operators first) -/
  binOps : List (BinOpSpec σ α)
  /-- Prefix unary operators -/
  prefixOps : List (PrefixOpSpec σ α) := []
  /-- Postfix unary operators (applied after atom, before binary ops) -/
  postfixOps : List (PostfixOpSpec σ α) := []

namespace OpTable

/-- Get the minimum precedence level from all binary operators -/
def minPrec {σ α : Type} (table : OpTable σ α) : Nat :=
  table.binOps.foldl (fun acc op => min acc op.prec) (table.binOps.head?.map (·.prec) |>.getD 0)

end OpTable

/-- Try to parse any operator from a list, returning the first match -/
private def tryParseOp {σ α : Type} (ops : List (BinOpSpec σ α)) (minPrec : Nat) : Parser σ (Option (BinOpSpec σ α)) := fun s =>
  let rec go : List (BinOpSpec σ α) → Except ParseError (Option (BinOpSpec σ α) × ParseState σ)
    | [] => .ok (none, s)
    | op :: rest =>
      if op.prec >= minPrec then
        match attempt op.parser s with
        | .ok ((), s') => .ok (some op, s')
        | .error _ => go rest
      else
        go rest
  go ops

/-- Try to parse any prefix operator -/
private def tryPrefixOp {σ α : Type} (ops : List (PrefixOpSpec σ α)) : Parser σ (Option (PrefixOpSpec σ α)) := fun s =>
  let rec go : List (PrefixOpSpec σ α) → Except ParseError (Option (PrefixOpSpec σ α) × ParseState σ)
    | [] => .ok (none, s)
    | op :: rest =>
      match attempt op.parser s with
      | .ok ((), s') => .ok (some op, s')
      | .error _ => go rest
  go ops

/-- Try to parse any postfix operator at or above minimum precedence -/
private def tryPostfixOp {σ α : Type} (ops : List (PostfixOpSpec σ α)) (minPrec : Nat) : Parser σ (Option (PostfixOpSpec σ α)) := fun s =>
  let rec go : List (PostfixOpSpec σ α) → Except ParseError (Option (PostfixOpSpec σ α) × ParseState σ)
    | [] => .ok (none, s)
    | op :: rest =>
      if op.prec >= minPrec then
        match attempt op.parser s with
        | .ok ((), s') => .ok (some op, s')
        | .error _ => go rest
      else
        go rest
  go ops

/-- Parse expression with precedence climbing algorithm.

    This is the main entry point for expression parsing with operator precedence.

    Parameters:
    - table: Operator specifications (binary, prefix, and postfix)
    - atom: Parser for primary expressions (literals, identifiers, parenthesized expressions)

    The algorithm:
    1. Parse any prefix operators
    2. Parse an atom
    3. Apply any postfix operators
    4. While there's a binary operator with precedence >= minPrec:
       - Parse the operator
       - Recursively parse RHS with appropriate minPrec based on associativity
       - Combine LHS and RHS
-/
partial def prec {σ α : Type} (table : OpTable σ α) (atom : Parser σ α) : Parser σ α :=
  parseExpr table.minPrec
where
  /-- Parse atom with prefix operators applied -/
  parseAtomWithPrefix : Parser σ α := do
    match ← tryPrefixOp table.prefixOps with
    | some op =>
      -- Parse operand at this prefix's precedence level
      let operand ← parseExpr op.prec
      pure (op.apply operand)
    | none =>
      atom

  /-- Apply postfix operators -/
  applyPostfix (e : α) (minPrec : Nat) : Parser σ α := do
    match ← tryPostfixOp table.postfixOps minPrec with
    | some op =>
      -- Keep applying postfix operators
      applyPostfix (op.apply e) minPrec
    | none =>
      pure e

  /-- Parse expression with minimum precedence level -/
  parseExpr (minPrec : Nat) : Parser σ α := do
    -- Parse prefix operators and atom
    let atomResult ← parseAtomWithPrefix
    -- Apply any postfix operators
    let mut left ← applyPostfix atomResult minPrec

    -- Keep consuming binary operators at or above minPrec
    let mut continue_ := true
    while continue_ do
      let saved ← Parser.get
      match ← tryParseOp table.binOps minPrec with
      | none =>
        Parser.set saved
        continue_ := false
      | some op =>
        -- Determine next minimum precedence based on associativity
        let nextMinPrec := match op.assoc with
          | .left => op.prec + 1   -- For left-assoc, RHS must bind tighter
          | .right => op.prec      -- For right-assoc, RHS can be same precedence
          | .none => op.prec + 1   -- Non-assoc treated like left for single application
        let right ← parseExpr nextMinPrec
        left := op.combine left right

    pure left

/-- Simplified precedence climbing for binary operators only (no prefix/postfix).

    Use this when you only have binary operators. For full support including
    prefix and postfix operators, use `prec` instead.
-/
partial def precBinary {σ α : Type} (ops : List (BinOpSpec σ α)) (atom : Parser σ α) : Parser σ α :=
  prec { binOps := ops } atom

end Sift
