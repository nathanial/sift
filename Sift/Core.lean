namespace Sift

/-!
# Sift.Core

Core types for the parser combinator library:
- `ParseState`: tracks position in input plus user state
- `ParseError`: error with position and message
- `Parser`: the main parser monad (parameterized by user state type)
-/

/-- Source position for error reporting -/
structure SourcePos where
  offset : Nat
  line : Nat
  column : Nat
  deriving Repr, BEq, Inhabited

instance : ToString SourcePos where
  toString p := s!"line {p.line}, column {p.column}"

/-- Parser state: tracks position in the input string plus user-defined state -/
structure ParseState (σ : Type) where
  /-- The original input string -/
  input : String
  /-- Current byte position -/
  pos : Nat
  /-- Current line number (1-indexed) -/
  line : Nat := 1
  /-- Current column number (1-indexed) -/
  column : Nat := 1
  /-- User-defined state -/
  userState : σ
  deriving Repr

namespace ParseState

/-- Create initial state from input string and user state -/
def init {σ : Type} (input : String) (userState : σ) : ParseState σ :=
  { input, pos := 0, line := 1, column := 1, userState }

/-- Check if at end of input -/
def atEnd {σ : Type} (s : ParseState σ) : Bool :=
  s.pos >= s.input.utf8ByteSize

/-- Get current character (if not at end) -/
def current? {σ : Type} (s : ParseState σ) : Option Char :=
  if s.atEnd then none else some (String.Pos.Raw.get s.input ⟨s.pos⟩)

/-- Advance to next character, updating line/column -/
def advance {σ : Type} (s : ParseState σ) : ParseState σ :=
  if s.atEnd then s
  else
    let c := String.Pos.Raw.get s.input ⟨s.pos⟩
    let (newLine, newCol) :=
      if c == '\n' then (s.line + 1, 1)
      else (s.line, s.column + 1)
    -- Advance by UTF-8 byte size of the character, not by 1
    { s with pos := s.pos + c.utf8Size, line := newLine, column := newCol }

/-- Get current source position -/
def sourcePos {σ : Type} (s : ParseState σ) : SourcePos :=
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
def fromState {σ : Type} (s : ParseState σ) (msg : String) : ParseError :=
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

/-- The Parser monad: a function from state to result, parameterized by user state type σ -/
def Parser (σ : Type) (α : Type) := ParseState σ → Except ParseError (α × ParseState σ)

namespace Parser

instance {σ α : Type} : Inhabited (Parser σ α) where
  default := fun s => .error (ParseError.fromState s "no parse")

/-- Get current state -/
def get {σ : Type} : Parser σ (ParseState σ) := fun s => .ok (s, s)

/-- Set current state -/
def set {σ : Type} (s : ParseState σ) : Parser σ Unit := fun _ => .ok ((), s)

/-- Modify current state -/
def modify {σ : Type} (f : ParseState σ → ParseState σ) : Parser σ Unit := fun s => .ok ((), f s)

/-- Get user state -/
def getUserState {σ : Type} : Parser σ σ := fun s => .ok (s.userState, s)

/-- Set user state -/
def setUserState {σ : Type} (st : σ) : Parser σ Unit := fun s =>
  .ok ((), { s with userState := st })

/-- Modify user state -/
def modifyUserState {σ : Type} (f : σ → σ) : Parser σ Unit := fun s =>
  .ok ((), { s with userState := f s.userState })

/-- Pure value (always succeeds) -/
protected def pure {σ α : Type} (a : α) : Parser σ α := fun s => .ok (a, s)

/-- Bind (sequence parsers) -/
protected def bind {σ α β : Type} (p : Parser σ α) (f : α → Parser σ β) : Parser σ β := fun s =>
  match p s with
  | .ok (a, s') => f a s'
  | .error e => .error e

instance {σ : Type} : Monad (Parser σ) where
  pure := Parser.pure
  bind := Parser.bind

instance {σ : Type} : MonadExceptOf ParseError (Parser σ) where
  throw e := fun _ => .error e
  tryCatch p handler := fun s =>
    match p s with
    | .ok result => .ok result
    | .error e => handler e s

/-- Fail with a message -/
def fail {σ α : Type} (msg : String) : Parser σ α := fun s =>
  .error (ParseError.fromState s msg)

/-- Run a parser on an input string (for parsers with no user state) -/
def run {α : Type} (p : Parser Unit α) (input : String) : Except ParseError α :=
  match p (ParseState.init input ()) with
  | .ok (a, _) => .ok a
  | .error e => .error e

/-- Run a parser on an input string with initial user state -/
def runWith {σ α : Type} (p : Parser σ α) (input : String) (initState : σ) : Except ParseError α :=
  match p (ParseState.init input initState) with
  | .ok (a, _) => .ok a
  | .error e => .error e

/-- Run a parser and return both result and final state -/
def runWithState {α : Type} (p : Parser Unit α) (input : String) : Except ParseError (α × ParseState Unit) :=
  p (ParseState.init input ())

/-- Run a parser with user state
 and return both result and final state -/
def runWithStateFull {σ α : Type} (p : Parser σ α) (input : String) (initState : σ) : Except ParseError (α × ParseState σ) :=
  p (ParseState.init input initState)

/-- Run a parser, requiring it to consume all input -/
def parse {α : Type} (p : Parser Unit α) (input : String) : Except ParseError α :=
  match p (ParseState.init input ()) with
  | .ok (a, finalState) =>
    if finalState.atEnd then .ok a
    else .error (ParseError.fromState finalState "unexpected trailing input")
  | .error e => .error e

/-- Run a parser with user state, requiring it to consume all input -/
def parseWith {σ α : Type} (p : Parser σ α) (input : String) (initState : σ) : Except ParseError α :=
  match p (ParseState.init input initState) with
  | .ok (a, finalState) =>
    if finalState.atEnd then .ok a
    else .error (ParseError.fromState finalState "unexpected trailing input")
  | .error e => .error e

/-- Get current source position -/
def position {σ : Type} : Parser σ SourcePos := fun s => .ok (s.sourcePos, s)

end Parser

end Sift
