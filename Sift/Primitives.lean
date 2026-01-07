import Sift.Core

/-!
# Sift.Primitives

Primitive parsers: satisfy, char, string, anyChar, eof, peek
-/

namespace Sift

/-- Match a character satisfying a predicate -/
def satisfy (pred : Char → Bool) : Parser Char := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some c =>
    if pred c then .ok (c, s.advance)
    else .error (ParseError.fromState s s!"unexpected character '{c}'")

/-- Match a specific character -/
def char (c : Char) : Parser Char := fun s =>
  match s.current? with
  | none => .error ((ParseError.fromState s "unexpected end of input").expecting s!"'{c}'")
  | some c' =>
    if c == c' then .ok (c, s.advance)
    else .error ((ParseError.fromState s s!"unexpected '{c'}'").expecting s!"'{c}'")

/-- Match any character -/
def anyChar : Parser Char := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some c => .ok (c, s.advance)

/-- Match a specific string -/
partial def string (expected : String) : Parser String := fun s =>
  let rec go (idx : Nat) (state : ParseState) : Except ParseError (String × ParseState) :=
    if idx >= expected.length then
      .ok (expected, state)
    else
      match state.current? with
      | none =>
        -- Report at current position (may have consumed input)
        .error ((ParseError.fromState state "unexpected end of input").expecting s!"\"{expected}\"")
      | some c =>
        let ec := String.Pos.Raw.get expected ⟨idx⟩
        if c == ec then
          go (idx + 1) state.advance
        else
          -- Report at current position so consumed input prevents backtracking
          .error ((ParseError.fromState state s!"unexpected '{c}'").expecting s!"\"{expected}\"")
  go 0 s

/-- Match end of input -/
def eof : Parser Unit := fun s =>
  if s.atEnd then .ok ((), s)
  else .error (ParseError.fromState s s!"expected end of input, got '{String.Pos.Raw.get s.input ⟨s.pos⟩}'")

/-- Peek at current character without consuming -/
def peek : Parser (Option Char) := fun s =>
  .ok (s.current?, s)

/-- Peek at current character, fail if at end -/
def peek! : Parser Char := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some c => .ok (c, s)

/-- Skip a single character -/
def skip : Parser Unit := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some _ => .ok ((), s.advance)

/-- Take exactly n characters -/
partial def take (n : Nat) : Parser String := fun s =>
  let rec go (remaining : Nat) (acc : String) (state : ParseState) : Except ParseError (String × ParseState) :=
    if remaining == 0 then .ok (acc, state)
    else match state.current? with
    | none => .error (ParseError.fromState state s!"expected {remaining} more characters")
    | some c => go (remaining - 1) (acc.push c) state.advance
  go n "" s

/-- Take characters while predicate holds -/
partial def takeWhile (pred : Char → Bool) : Parser String := fun s =>
  let rec go (acc : String) (state : ParseState) : String × ParseState :=
    match state.current? with
    | none => (acc, state)
    | some c =>
      if pred c then go (acc.push c) state.advance
      else (acc, state)
  .ok (go "" s)

/-- Take at least one character while predicate holds -/
partial def takeWhile1 (pred : Char → Bool) : Parser String := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some c =>
    if pred c then
      let rec go (acc : String) (state : ParseState) : String × ParseState :=
        match state.current? with
        | none => (acc, state)
        | some c' =>
          if pred c' then go (acc.push c') state.advance
          else (acc, state)
      .ok (go (String.singleton c) s.advance)
    else .error (ParseError.fromState s s!"unexpected '{c}'")

/-- Skip characters while predicate holds -/
partial def skipWhile (pred : Char → Bool) : Parser Unit := fun s =>
  let rec go (state : ParseState) : ParseState :=
    match state.current? with
    | none => state
    | some c =>
      if pred c then go state.advance
      else state
  .ok ((), go s)

/-- Skip at least one character while predicate holds -/
partial def skipWhile1 (pred : Char → Bool) : Parser Unit := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some c =>
    if pred c then
      let rec go (state : ParseState) : ParseState :=
        match state.current? with
        | none => state
        | some c' =>
          if pred c' then go state.advance
          else state
      .ok ((), go s.advance)
    else .error (ParseError.fromState s s!"unexpected '{c}'")

/-- Check if at end of input -/
def atEnd : Parser Bool := fun s =>
  .ok (s.atEnd, s)

/-- Peek at next N characters without consuming -/
partial def peekString (n : Nat) : Parser (Option String) := fun s =>
  if s.atEnd then .ok (none, s)
  else
    let available := s.input.length - s.pos
    if available < n then .ok (none, s)
    else
      let result := String.Pos.Raw.extract s.input ⟨s.pos⟩ ⟨s.pos + n⟩
      .ok (some result, s)

end Sift
