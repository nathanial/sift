import Sift.Core
import Sift.Primitives
import Sift.Combinators
import Sift.Char

/-!
# Sift.Indent

Indentation-sensitive parsing combinators for Python-style block structure.

## Overview

Python-style indentation uses an **indent stack** to track nesting levels:
- INDENT token when indentation increases
- DEDENT token(s) when indentation decreases
- Statements at same indentation level are siblings

## Usage

For basic column/line tracking, use `getColumn`, `getLine`, etc. with any state type.

For full Python-style indentation, use `IndentState` as your user state:

```lean
open Sift

def parseBlock : Parser IndentState (Array Stmt) :=
  block parseStmt

#eval Parser.runWith parseBlock "if x:\n    y\n    z" IndentState.init
```
-/

namespace Sift

/-! ## Basic Position Primitives -/

/-- Get current column (1-indexed) -/
def getColumn {σ : Type} : Parser σ Nat := fun s => .ok (s.column, s)

/-- Get current line (1-indexed) -/
def getLine {σ : Type} : Parser σ Nat := fun s => .ok (s.line, s)

/-- Parse only if current column equals expected -/
def atColumn {σ : Type} (col : Nat) : Parser σ Unit := do
  let c ← getColumn
  if c == col then pure ()
  else Parser.fail s!"expected column {col}, got {c}"

/-- Parse only if current column > reference -/
def indented {σ : Type} (refCol : Nat) : Parser σ Unit := do
  let c ← getColumn
  if c > refCol then pure ()
  else Parser.fail s!"expected indentation > {refCol}, got {c}"

/-- Parse only if on same line as reference -/
def onLine {σ : Type} (refLine : Nat) : Parser σ Unit := do
  let l ← getLine
  if l == refLine then pure ()
  else Parser.fail s!"expected line {refLine}, got {l}"

/-- Measure indentation at current position (count leading spaces from column 1).
    If not at column 1, returns 0 (already past indentation). -/
def measureIndent {σ : Type} : Parser σ Nat := do
  let col ← getColumn
  if col != 1 then pure 0
  else do
    let spaces ← takeWhile (· == ' ')
    pure spaces.length

/-- Skip to end of current line (consume everything except newline) -/
def skipToEol {σ : Type} : Parser σ Unit :=
  skipWhile (fun c => c != '\n')

/-- Parse content that must be more indented than a reference column -/
def indentedContent {σ α : Type} (refCol : Nat) (p : Parser σ α) : Parser σ α := do
  indented refCol
  p

/-! ## Indent Stack State -/

/-- State for indent-sensitive parsing -/
structure IndentState where
  /-- Stack of indent levels, starts with 0 (column 1 = indent 0) -/
  indentStack : List Nat := [0]
  deriving Repr, Inhabited

namespace IndentState

/-- Create initial indent state -/
def init : IndentState := { indentStack := [0] }

/-- Current indent level -/
def currentIndent (s : IndentState) : Nat :=
  s.indentStack.head?.getD 0

/-- Push new indent level -/
def pushIndent (s : IndentState) (n : Nat) : IndentState :=
  { s with indentStack := n :: s.indentStack }

/-- Pop indent level, returns new state and number of levels popped -/
def popTo (s : IndentState) (n : Nat) : IndentState × Nat :=
  let rec go (stack : List Nat) (count : Nat) : List Nat × Nat :=
    match stack with
    | [] => ([0], count)
    | x :: xs => if x <= n then (x :: xs, count) else go xs (count + 1)
  let (newStack, count) := go s.indentStack 0
  ({ indentStack := newStack }, count)

/-- Check if an indent level is valid (exists in stack) -/
def isValidIndent (s : IndentState) (n : Nat) : Bool :=
  s.indentStack.any (· == n)

end IndentState

/-! ## Block Parsing Combinators -/

/-- Get current indent level from user state -/
def getCurrentIndent : Parser IndentState Nat := do
  let state ← Parser.getUserState
  pure state.currentIndent

/-- Push a new indent level -/
def pushIndentLevel (n : Nat) : Parser IndentState Unit := do
  Parser.modifyUserState (·.pushIndent n)

/-- Pop to a given indent level -/
def popToIndentLevel (n : Nat) : Parser IndentState Nat := do
  let state ← Parser.getUserState
  let (newState, count) := state.popTo n
  Parser.setUserState newState
  pure count

/-- Parse the indentation at the start of a line and update state accordingly.
    Returns the indentation level and whether it was an indent/dedent/same. -/
inductive IndentChange
  | indent    -- Increased indentation
  | dedent (levels : Nat)  -- Decreased indentation by N levels
  | same      -- Same indentation
  | invalid   -- Invalid indentation (not aligned to any level)
  deriving Repr, BEq

/-- Process line indentation and determine the change -/
def processIndent : Parser IndentState IndentChange := do
  let indent ← measureIndent
  let state ← Parser.getUserState
  let current := state.currentIndent
  if indent > current then
    Parser.setUserState (state.pushIndent indent)
    pure .indent
  else if indent < current then
    let (newState, levels) := state.popTo indent
    if newState.currentIndent != indent then
      Parser.fail s!"inconsistent indentation: {indent} does not match any indent level"
    Parser.setUserState newState
    pure (.dedent levels)
  else
    pure .same

/-- Parse a single line with its content, handling the indentation.
    The parser p receives the content after indentation is consumed. -/
def indentedLine {α : Type} (p : Parser IndentState α) : Parser IndentState (IndentChange × α) := do
  let change ← processIndent
  let result ← p
  pure (change, result)

/-- Parse an indented block (one or more lines at deeper indentation than current).
    The block starts on the next line after a colon or similar delimiter. -/
def block {α : Type} (p : Parser IndentState α) : Parser IndentState (Array α) := do
  let state ← Parser.getUserState
  let baseIndent := state.currentIndent
  -- First line of block - must be more indented
  let firstIndent ← measureIndent
  if firstIndent <= baseIndent then
    Parser.fail s!"expected indented block (current indent: {baseIndent}, got: {firstIndent})"
  Parser.setUserState (state.pushIndent firstIndent)
  let first ← p
  -- Collect more lines at same or greater indentation
  let rest ← many do
    -- Skip to next line
    skipToEol
    eol
    -- Check indentation
    let indent ← measureIndent
    if indent < firstIndent then
      Parser.fail "end of block"
    else if indent > firstIndent then
      -- Nested indentation - the inner parser should handle this
      Parser.fail "nested block should be handled by inner parser"
    p
  -- Pop back to base indent
  let _ ← popToIndentLevel baseIndent
  pure (#[first] ++ rest)

/-- Parse lines at the same indentation level (siblings) -/
def sameLevel {α : Type} (p : Parser IndentState α) : Parser IndentState (Array α) := do
  let state ← Parser.getUserState
  let level := state.currentIndent
  let first ← p
  let rest ← many do
    skipToEol
    eol
    let indent ← measureIndent
    if indent != level then
      Parser.fail "different indent level"
    p
  pure (#[first] ++ rest)

/-- Parse a block where each line is parsed by p, handling nested blocks.
    This is the main entry point for Python-style block parsing. -/
partial def blockLines {α : Type}
    (parseLine : Parser IndentState α)
    : Parser IndentState (Array α) := do
  let state ← Parser.getUserState
  let baseIndent := state.currentIndent
  -- Measure first line indentation
  let firstIndent ← measureIndent
  if firstIndent <= baseIndent then
    Parser.fail s!"expected indented block"
  Parser.setUserState (state.pushIndent firstIndent)
  -- Parse lines
  let mut results := #[]
  let first ← parseLine
  results := results.push first
  -- Continue while we have more lines at this level or deeper
  let rec loop (acc : Array α) : Parser IndentState (Array α) := do
    -- Try to continue
    let atEof ← atEnd
    if atEof then pure acc
    else do
      -- Skip current line ending
      let _ ← optional do
        skipToEol
        eol
      let atEof2 ← atEnd
      if atEof2 then pure acc
      else do
        -- Check next line's indentation
        let state ← Parser.getUserState
        let peek ← optional do
          let indent ← measureIndent
          pure indent
        match peek with
        | none => pure acc
        | some indent =>
          if indent < firstIndent then
            -- Dedent - end of block
            let (newState, _) := state.popTo indent
            Parser.setUserState newState
            pure acc
          else if indent == firstIndent then
            -- Same level - continue block
            let item ← parseLine
            loop (acc.push item)
          else
            -- Deeper indent - this should be handled by the line parser
            let item ← parseLine
            loop (acc.push item)
  loop results

/-! ## Utility Combinators -/

/-- Parse with indentation context: runs parser p with the current indentation
    as the reference for any nested blocks -/
def withIndentContext {α : Type} (p : Parser IndentState α) : Parser IndentState α := do
  let col ← getColumn
  let state ← Parser.getUserState
  -- Set current column as indent level
  Parser.setUserState (state.pushIndent (col - 1))
  let result ← p
  -- Restore previous indent
  let _ ← popToIndentLevel state.currentIndent
  pure result

/-- Parse content that must start at a specific column -/
def atIndent {α : Type} (indent : Nat) (p : Parser IndentState α) : Parser IndentState α := do
  let col ← getColumn
  if col - 1 != indent then
    Parser.fail s!"expected indent {indent}, got {col - 1}"
  p

/-- Check that we're at the expected indentation level without consuming input -/
def checkIndent (expected : Nat) : Parser IndentState Unit := do
  let state ← Parser.getUserState
  let current := state.currentIndent
  if current != expected then
    Parser.fail s!"expected indent level {expected}, got {current}"

/-- Parse a "soft" block where indentation is implicit based on first line -/
def softBlock {α : Type} (p : Parser IndentState α) : Parser IndentState (Array α) := do
  let col ← getColumn
  let baseIndent := col - 1
  let state ← Parser.getUserState
  Parser.setUserState (state.pushIndent baseIndent)
  let results ← many do
    let indent ← measureIndent
    if indent < baseIndent then
      Parser.fail "end of soft block"
    p
  let _ ← popToIndentLevel state.currentIndent
  pure results

end Sift
