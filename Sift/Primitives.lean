import Sift.Core

/-!
# Sift.Primitives

Primitive parsers: satisfy, char, string, anyChar, eof, peek
-/

namespace Sift

/-- Match a character satisfying a predicate -/
def satisfy {σ : Type} (pred : Char → Bool) : Parser σ Char := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some c =>
    if pred c then .ok (c, s.advance)
    else .error (ParseError.fromState s s!"unexpected character '{c}'")

/-- Match a specific character -/
def char {σ : Type} (c : Char) : Parser σ Char := fun s =>
  match s.current? with
  | none => .error ((ParseError.fromState s "unexpected end of input").expecting s!"'{c}'")
  | some c' =>
    if c == c' then .ok (c, s.advance)
    else .error ((ParseError.fromState s s!"unexpected '{c'}'").expecting s!"'{c}'")

/-- Match any character -/
def anyChar {σ : Type} : Parser σ Char := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some c => .ok (c, s.advance)

/-- Match a specific string -/
partial def string {σ : Type} (expected : String) : Parser σ String := fun s =>
  let rec go (idx : Nat) (state : ParseState σ) : Except ParseError (String × ParseState σ) :=
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
def eof {σ : Type} : Parser σ Unit := fun s =>
  if s.atEnd then .ok ((), s)
  else .error (ParseError.fromState s s!"expected end of input, got '{String.Pos.Raw.get s.input ⟨s.pos⟩}'")

/-- Peek at current character without consuming -/
def peek {σ : Type} : Parser σ (Option Char) := fun s =>
  .ok (s.current?, s)

/-- Peek at current character, fail if at end -/
def peek! {σ : Type} : Parser σ Char := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some c => .ok (c, s)

/-- Skip a single character -/
def skip {σ : Type} : Parser σ Unit := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some _ => .ok ((), s.advance)

/-- Take exactly n characters -/
partial def take {σ : Type} (n : Nat) : Parser σ String := fun s =>
  let rec go (remaining : Nat) (acc : String) (state : ParseState σ) : Except ParseError (String × ParseState σ) :=
    if remaining == 0 then .ok (acc, state)
    else match state.current? with
    | none => .error (ParseError.fromState state s!"expected {remaining} more characters")
    | some c => go (remaining - 1) (acc.push c) state.advance
  go n "" s

/-- Take characters while predicate holds -/
partial def takeWhile {σ : Type} (pred : Char → Bool) : Parser σ String := fun s =>
  let rec go (acc : String) (state : ParseState σ) : String × ParseState σ :=
    match state.current? with
    | none => (acc, state)
    | some c =>
      if pred c then go (acc.push c) state.advance
      else (acc, state)
  .ok (go "" s)

/-- Take at least one character while predicate holds -/
partial def takeWhile1 {σ : Type} (pred : Char → Bool) : Parser σ String := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some c =>
    if pred c then
      let rec go (acc : String) (state : ParseState σ) : String × ParseState σ :=
        match state.current? with
        | none => (acc, state)
        | some c' =>
          if pred c' then go (acc.push c') state.advance
          else (acc, state)
      .ok (go (String.singleton c) s.advance)
    else .error (ParseError.fromState s s!"unexpected '{c}'")

/-- Skip characters while predicate holds -/
partial def skipWhile {σ : Type} (pred : Char → Bool) : Parser σ Unit := fun s =>
  let rec go (state : ParseState σ) : ParseState σ :=
    match state.current? with
    | none => state
    | some c =>
      if pred c then go state.advance
      else state
  .ok ((), go s)

/-- Skip at least one character while predicate holds -/
partial def skipWhile1 {σ : Type} (pred : Char → Bool) : Parser σ Unit := fun s =>
  match s.current? with
  | none => .error (ParseError.fromState s "unexpected end of input")
  | some c =>
    if pred c then
      let rec go (state : ParseState σ) : ParseState σ :=
        match state.current? with
        | none => state
        | some c' =>
          if pred c' then go state.advance
          else state
      .ok ((), go s.advance)
    else .error (ParseError.fromState s s!"unexpected '{c}'")

/-- Check if at end of input -/
def atEnd {σ : Type} : Parser σ Bool := fun s =>
  .ok (s.atEnd, s)

/-- Peek at next N characters without consuming -/
partial def peekString {σ : Type} (n : Nat) : Parser σ (Option String) := fun s =>
  if s.atEnd then .ok (none, s)
  else
    let available := s.input.length - s.pos
    if available < n then .ok (none, s)
    else
      let result := String.Pos.Raw.extract s.input ⟨s.pos⟩ ⟨s.pos + n⟩
      .ok (some result, s)

end Sift
