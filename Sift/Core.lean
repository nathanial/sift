namespace Sift

/-!
# Sift.Core

Core types for the parser combinator library:
- `ParseState`: tracks position in input
- `ParseError`: error with position and message
- `Parser`: the main parser monad
-/

/-- Source position for error reporting -/
structure SourcePos where
  offset : Nat
  line : Nat
  column : Nat
  deriving Repr, BEq, Inhabited

instance : ToString SourcePos where
  toString p := s!"line {p.line}, column {p.column}"

/-- Parser state: tracks position in the input string -/
structure ParseState where
  /-- The original input string -/
  input : String
  /-- Current byte position -/
  pos : Nat
  /-- Current line number (1-indexed) -/
  line : Nat := 1
  /-- Current column number (1-indexed) -/
  column : Nat := 1
  deriving Repr, BEq

namespace ParseState

/-- Create initial state from input string -/
def init (input : String) : ParseState :=
  { input, pos := 0, line := 1, column := 1 }

/-- Check if at end of input -/
def atEnd (s : ParseState) : Bool :=
  s.pos >= s.input.length

/-- Get current character (if not at end) -/
def current? (s : ParseState) : Option Char :=
  if s.atEnd then none else some (String.Pos.Raw.get s.input ⟨s.pos⟩)

/-- Advance to next character, updating line/column -/
def advance (s : ParseState) : ParseState :=
  if s.atEnd then s
  else
    let c := String.Pos.Raw.get s.input ⟨s.pos⟩
    let (newLine, newCol) :=
      if c == '\n' then (s.line + 1, 1)
      else (s.line, s.column + 1)
    { s with pos := s.pos + 1, line := newLine, column := newCol }

/-- Get current source position -/
def sourcePos (s : ParseState) : SourcePos :=
  { offset := s.pos, line := s.line, column := s.column }

end ParseState

/-- Parser error with position and message -/
structure ParseError where
  pos : SourcePos
  message : String
  expected : List String := []
  deriving Repr, BEq

instance : ToString ParseError where
  toString e :=
    let expStr := if e.expected.isEmpty then ""
                  else s!", expected: {", ".intercalate e.expected}"
    s!"parse error at {e.pos}: {e.message}{expStr}"

namespace ParseError

/-- Create error from parse state -/
def fromState (s : ParseState) (msg : String) : ParseError :=
  { pos := s.sourcePos, message := msg }

/-- Add expected item to error -/
def expecting (e : ParseError) (item : String) : ParseError :=
  { e with expected := [item] }

/-- Merge expected items from two errors at the same position -/
def merge (e1 e2 : ParseError) : ParseError :=
  if e1.pos == e2.pos then
    { e1 with expected := e1.expected ++ e2.expected }
  else if e1.pos.offset > e2.pos.offset then e1 else e2

end ParseError

/-- The Parser monad: a function from state to result -/
def Parser (α : Type) := ParseState → Except ParseError (α × ParseState)

namespace Parser

instance {α : Type} : Inhabited (Parser α) where
  default := fun s => .error (ParseError.fromState s "no parse")

/-- Get current state -/
def get : Parser ParseState := fun s => .ok (s, s)

/-- Set current state -/
def set (s : ParseState) : Parser Unit := fun _ => .ok ((), s)

/-- Modify current state -/
def modify (f : ParseState → ParseState) : Parser Unit := fun s => .ok ((), f s)

/-- Pure value (always succeeds) -/
protected def pure {α : Type} (a : α) : Parser α := fun s => .ok (a, s)

/-- Bind (sequence parsers) -/
protected def bind {α β : Type} (p : Parser α) (f : α → Parser β) : Parser β := fun s =>
  match p s with
  | .ok (a, s') => f a s'
  | .error e => .error e

instance : Monad Parser where
  pure := Parser.pure
  bind := Parser.bind

instance : MonadExceptOf ParseError Parser where
  throw e := fun _ => .error e
  tryCatch p handler := fun s =>
    match p s with
    | .ok result => .ok result
    | .error e => handler e s

/-- Fail with a message -/
def fail {α : Type} (msg : String) : Parser α := fun s =>
  .error (ParseError.fromState s msg)

/-- Run a parser on an input string -/
def run {α : Type} (p : Parser α) (input : String) : Except ParseError α :=
  match p (ParseState.init input) with
  | .ok (a, _) => .ok a
  | .error e => .error e

/-- Run a parser and return both result and final state -/
def runWithState {α : Type} (p : Parser α) (input : String) : Except ParseError (α × ParseState) :=
  p (ParseState.init input)

/-- Run a parser, requiring it to consume all input -/
def parse {α : Type} (p : Parser α) (input : String) : Except ParseError α :=
  match p (ParseState.init input) with
  | .ok (a, finalState) =>
    if finalState.atEnd then .ok a
    else .error (ParseError.fromState finalState "unexpected trailing input")
  | .error e => .error e

/-- Get current source position -/
def position : Parser SourcePos := fun s => .ok (s.sourcePos, s)

end Parser

end Sift
