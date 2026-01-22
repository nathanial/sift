import Crucible
import Sift
import Sift.Prec

namespace SiftTests.Prec

open Crucible
open Sift

-- Simple expression type for testing
inductive Expr where
  | num : Int → Expr
  | add : Expr → Expr → Expr
  | sub : Expr → Expr → Expr
  | mul : Expr → Expr → Expr
  | div : Expr → Expr → Expr
  | pow : Expr → Expr → Expr
  | neg : Expr → Expr
  | not_ : Expr → Expr
  | postIncr : Expr → Expr
  deriving Repr, BEq

-- Evaluate expression
def Expr.eval : Expr → Int
  | .num n => n
  | .add a b => a.eval + b.eval
  | .sub a b => a.eval - b.eval
  | .mul a b => a.eval * b.eval
  | .div a b => if b.eval == 0 then 0 else a.eval / b.eval
  | .pow a b => Int.ofNat (a.eval.toNat ^ b.eval.toNat)
  | .neg a => -a.eval
  | .not_ a => if a.eval == 0 then 1 else 0
  | .postIncr a => a.eval + 1

-- Skip whitespace
def ws : Parser Unit Unit := skipMany (satisfy fun c => c == ' ')

-- Parse a symbol (with trailing whitespace)
def sym (s : String) : Parser Unit Unit := do
  let _ ← string s
  ws

-- Parse a number
def num : Parser Unit Expr := do
  ws
  let n ← integer
  ws
  pure (.num n)

-- Basic arithmetic operator table (+ - * /)
def arithOps : OpTable Unit Expr := {
  binOps := [
    { parser := sym "+", prec := 1, assoc := .left, combine := Expr.add },
    { parser := sym "-", prec := 1, assoc := .left, combine := Expr.sub },
    { parser := sym "*", prec := 2, assoc := .left, combine := Expr.mul },
    { parser := sym "/", prec := 2, assoc := .left, combine := Expr.div }
  ]
}

-- Arithmetic with prefix negation
def arithWithPrefix : OpTable Unit Expr := {
  binOps := arithOps.binOps
  prefixOps := [
    { parser := sym "-", prec := 3, apply := Expr.neg }
  ]
}

-- Arithmetic with right-associative power operator
def arithWithPow : OpTable Unit Expr := {
  binOps := arithOps.binOps ++ [
    { parser := sym "^", prec := 3, assoc := .right, combine := Expr.pow }
  ]
}

-- With postfix operator
def arithWithPostfix : OpTable Unit Expr := {
  binOps := arithOps.binOps
  postfixOps := [
    { parser := sym "++", prec := 4, apply := Expr.postIncr }
  ]
}

-- Full table with prefix and postfix
def fullOps : OpTable Unit Expr := {
  binOps := arithOps.binOps ++ [
    { parser := sym "^", prec := 3, assoc := .right, combine := Expr.pow }
  ]
  prefixOps := [
    { parser := sym "-", prec := 4, apply := Expr.neg },
    { parser := sym "!", prec := 4, apply := Expr.not_ }
  ]
  postfixOps := [
    { parser := sym "++", prec := 5, apply := Expr.postIncr }
  ]
}

-- Parse atom (just numbers for basic tests)
def atom : Parser Unit Expr := num

-- Parse atom with parentheses
partial def atomWithParens : Parser Unit Expr := do
  ws
  (do
    let _ ← char '('
    ws
    let e ← prec fullOps atomWithParens
    let _ ← char ')'
    ws
    pure e)
  <|> num

testSuite "Prec"

-- Basic precedence tests

test "single number" := do
  match Parser.run (prec arithOps atom) "42" with
  | .ok e => e.eval ≡ 42
  | .error _ => panic! "expected success"

test "simple addition" := do
  match Parser.run (prec arithOps atom) "1 + 2" with
  | .ok e => e.eval ≡ 3
  | .error _ => panic! "expected success"

test "addition is left associative" := do
  -- 10 - 3 - 2 = (10 - 3) - 2 = 5
  match Parser.run (prec arithOps atom) "10 - 3 - 2" with
  | .ok e => e.eval ≡ 5
  | .error _ => panic! "expected success"

test "multiplication binds tighter than addition" := do
  -- 1 + 2 * 3 = 1 + (2 * 3) = 7
  match Parser.run (prec arithOps atom) "1 + 2 * 3" with
  | .ok e => e.eval ≡ 7
  | .error _ => panic! "expected success"

test "complex precedence" := do
  -- 2 + 3 * 4 - 5 = 2 + 12 - 5 = 9
  match Parser.run (prec arithOps atom) "2 + 3 * 4 - 5" with
  | .ok e => e.eval ≡ 9
  | .error _ => panic! "expected success"

test "division precedence" := do
  -- 10 - 6 / 2 = 10 - 3 = 7
  match Parser.run (prec arithOps atom) "10 - 6 / 2" with
  | .ok e => e.eval ≡ 7
  | .error _ => panic! "expected success"

-- Prefix operator tests

test "prefix negation" := do
  match Parser.run (prec arithWithPrefix atom) "-5" with
  | .ok e => e.eval ≡ -5
  | .error _ => panic! "expected success"

test "prefix negation with addition" := do
  -- -5 + 3 = (-5) + 3 = -2
  match Parser.run (prec arithWithPrefix atom) "-5 + 3" with
  | .ok e => e.eval ≡ -2
  | .error _ => panic! "expected success"

test "double prefix negation" := do
  -- --5 = -(-5) = 5
  match Parser.run (prec arithWithPrefix atom) "- -5" with
  | .ok e => e.eval ≡ 5
  | .error _ => panic! "expected success"

test "prefix binds tighter than binary" := do
  -- 3 * -2 = 3 * (-2) = -6
  match Parser.run (prec arithWithPrefix atom) "3 * -2" with
  | .ok e => e.eval ≡ -6
  | .error _ => panic! "expected success"

-- Right associativity tests

test "power is right associative" := do
  -- 2 ^ 3 ^ 2 = 2 ^ (3 ^ 2) = 2 ^ 9 = 512
  match Parser.run (prec arithWithPow atom) "2 ^ 3 ^ 2" with
  | .ok e => e.eval ≡ 512
  | .error _ => panic! "expected success"

test "power binds tighter than multiplication" := do
  -- 2 * 3 ^ 2 = 2 * (3 ^ 2) = 2 * 9 = 18
  match Parser.run (prec arithWithPow atom) "2 * 3 ^ 2" with
  | .ok e => e.eval ≡ 18
  | .error _ => panic! "expected success"

-- Postfix operator tests

test "postfix increment" := do
  match Parser.run (prec arithWithPostfix atom) "5 ++" with
  | .ok e => e.eval ≡ 6
  | .error _ => panic! "expected success"

test "postfix with binary operator" := do
  -- 5 ++ + 3 = (5++) + 3 = 6 + 3 = 9
  match Parser.run (prec arithWithPostfix atom) "5 ++ + 3" with
  | .ok e => e.eval ≡ 9
  | .error _ => panic! "expected success"

-- Combined prefix, binary, postfix tests

test "prefix and postfix together" := do
  -- -5 ++ = -(5++) = -(6) = -6? No, postfix binds tighter: (-5)++ = -4
  -- Actually: prefix is applied first to atom, then postfix. Let me trace:
  -- parseAtomWithPrefix sees -, parses rest at prec 4
  --   that recursively sees 5 as atom
  -- Then applyPostfix sees ++ and applies it
  -- So: -(5++) = -6
  match Parser.run (prec fullOps atom) "- 5 ++" with
  | .ok e => e.eval ≡ -6
  | .error _ => panic! "expected success"

test "complex expression with all operators" := do
  -- 2 + -3 * 4 = 2 + ((-3) * 4) = 2 + (-12) = -10
  match Parser.run (prec fullOps atom) "2 + -3 * 4" with
  | .ok e => e.eval ≡ -10
  | .error _ => panic! "expected success"

-- Parentheses tests (requires recursive atom)

test "parentheses override precedence" := do
  -- (1 + 2) * 3 = 3 * 3 = 9
  match Parser.run (prec fullOps atomWithParens) "(1 + 2) * 3" with
  | .ok e => e.eval ≡ 9
  | .error _ => panic! "expected success"

test "nested parentheses" := do
  -- ((2 + 3)) * 2 = 5 * 2 = 10
  match Parser.run (prec fullOps atomWithParens) "((2 + 3)) * 2" with
  | .ok e => e.eval ≡ 10
  | .error _ => panic! "expected success"

-- precBinary helper tests

test "precBinary works for simple case" := do
  match Parser.run (precBinary arithOps.binOps atom) "1 + 2 * 3" with
  | .ok e => e.eval ≡ 7
  | .error _ => panic! "expected success"



end SiftTests.Prec
