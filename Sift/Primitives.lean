import Sift.Core

/-!
# Sift.Primitives

Primitive parsers: satisfy, char, string, anyChar, eof, peek
-/

namespace Sift

/-- Match a character satisfying a predicate -/
def satisfy {σ : Type} (pred : Char → Bool) : Parser σ Char := fun s =>
  match s.current? with
  | none =>
    .error ((ParseError.unexpectedEof s).expecting "character matching predicate")
  | some c =>
    if pred c then .ok (c, s.advance)
    else .error ((ParseError.unexpectedChar s c).expecting "character matching predicate")

/-- Match a specific character -/
def char {σ : Type} (c : Char) : Parser σ Char := fun s =>
  match s.current? with
  | none => .error ((ParseError.unexpectedEof s).expecting s!"'{c}'")
  | some c' =>
    if c == c' then .ok (c, s.advance)
    else .error ((ParseError.unexpectedChar s c').expecting s!"'{c}'")

/-- Match any character -/
def anyChar {σ : Type} : Parser σ Char := fun s =>
  match s.current? with
  | none => .error ((ParseError.unexpectedEof s).expecting "any character")
  | some c => .ok (c, s.advance)

/-- Match a specific string -/
def string {σ : Type} (expected : String) : Parser σ String := fun s =>
  let rec go (chars : List Char) (state : ParseState σ) : Except ParseError (String × ParseState σ) :=
    match chars with
    | [] => .ok (expected, state)
    | ec :: rest =>
      match state.current? with
      | none =>
        -- Report at current position (may have consumed input)
        .error ((ParseError.unexpectedEof state).expecting s!"\"{expected}\"")
      | some c =>
        if c == ec then
          go rest state.advance
        else
          -- Report at current position so consumed input prevents backtracking
          .error ((ParseError.unexpectedChar state c).expecting s!"\"{expected}\"")
  go expected.toList s

/-- Match a string case-insensitively (returns the matched string as it appeared in input) -/
partial def stringCI {σ : Type} (expected : String) : Parser σ String := fun s =>
  let rec go (idx : Nat) (state : ParseState σ) (acc : String) : Except ParseError (String × ParseState σ) :=
    if idx >= expected.utf8ByteSize then
      .ok (acc, state)
    else
      match state.current? with
      | none =>
        .error ((ParseError.unexpectedEof state).expecting s!"\"{expected}\"")
      | some c =>
        let ec := String.Pos.Raw.get expected ⟨idx⟩
        if c.toLower == ec.toLower then
          go (idx + ec.utf8Size) state.advance (acc.push c)
        else
          .error ((ParseError.unexpectedChar state c).expecting s!"\"{expected}\"")
  go 0 s ""

/-- Match a character case-insensitively (returns the character as it appeared in input) -/
def charCI {σ : Type} (c : Char) : Parser σ Char := fun s =>
  match s.current? with
  | none => .error ((ParseError.unexpectedEof s).expecting s!"'{c}' (case-insensitive)")
  | some c' =>
    if c.toLower == c'.toLower then .ok (c', s.advance)
    else .error ((ParseError.unexpectedChar s c').expecting s!"'{c}' (case-insensitive)")

/-- Match end of input -/
def eof {σ : Type} : Parser σ Unit := fun s =>
  if s.atEnd then .ok ((), s)
  else
    let got := String.Pos.Raw.get s.input ⟨s.pos⟩
    .error ((ParseError.expectedEof s got).expecting "end of input")

/-- Peek at current character without consuming -/
def peek {σ : Type} : Parser σ (Option Char) := fun s =>
  .ok (s.current?, s)

/-- Peek at current character, fail if at end -/
def peek! {σ : Type} : Parser σ Char := fun s =>
  match s.current? with
  | none => .error ((ParseError.unexpectedEof s).expecting "any character")
  | some c => .ok (c, s)

/-- Skip a single character -/
def skip {σ : Type} : Parser σ Unit := fun s =>
  match s.current? with
  | none => .error ((ParseError.unexpectedEof s).expecting "any character")
  | some _ => .ok ((), s.advance)

/-- Take exactly n characters -/
def take {σ : Type} (n : Nat) : Parser σ String := fun s =>
  let rec go : Nat → String → ParseState σ → Except ParseError (String × ParseState σ)
    | 0, acc, state => .ok (acc, state)
    | Nat.succ remaining, acc, state =>
      match state.current? with
      | none =>
        let e := ParseError.fromState state s!"expected {remaining.succ} more characters"
        .error ({ e with kind := .unexpectedEof }.expecting s!"{remaining.succ} more characters")
      | some c => go remaining (acc.push c) state.advance
  go n "" s

/-- Take characters while predicate holds -/
def takeWhile {σ : Type} (pred : Char → Bool) : Parser σ String := fun s =>
  let fuel := s.input.utf8ByteSize - s.pos
  let rec go : Nat → String → ParseState σ → String × ParseState σ
    | 0, acc, state => (acc, state)
    | Nat.succ remaining, acc, state =>
      match state.current? with
      | none => (acc, state)
      | some c =>
        if pred c then go remaining (acc.push c) state.advance
        else (acc, state)
  .ok (go fuel "" s)

/-- Take at least one character while predicate holds -/
def takeWhile1 {σ : Type} (pred : Char → Bool) : Parser σ String := fun s =>
  match s.current? with
  | none =>
    .error ((ParseError.unexpectedEof s).expecting "character matching predicate")
  | some c =>
    if pred c then
      match takeWhile pred s.advance with
      | .ok (rest, state) => .ok (String.singleton c ++ rest, state)
      | .error e => .error e
    else .error ((ParseError.unexpectedChar s c).expecting "character matching predicate")

/-- Skip characters while predicate holds -/
def skipWhile {σ : Type} (pred : Char → Bool) : Parser σ Unit := fun s =>
  let fuel := s.input.utf8ByteSize - s.pos
  let rec go : Nat → ParseState σ → ParseState σ
    | 0, state => state
    | Nat.succ remaining, state =>
      match state.current? with
      | none => state
      | some c =>
        if pred c then go remaining state.advance
        else state
  .ok ((), go fuel s)

/-- Skip at least one character while predicate holds -/
def skipWhile1 {σ : Type} (pred : Char → Bool) : Parser σ Unit := fun s =>
  match s.current? with
  | none =>
    .error ((ParseError.unexpectedEof s).expecting "character matching predicate")
  | some c =>
    if pred c then
      match skipWhile pred s.advance with
      | .ok ((), state) => .ok ((), state)
      | .error e => .error e
    else .error ((ParseError.unexpectedChar s c).expecting "character matching predicate")

/-- Check if at end of input -/
def atEnd {σ : Type} : Parser σ Bool := fun s =>
  .ok (s.atEnd, s)

/-- Peek at next N characters without consuming -/
partial def peekString {σ : Type} (n : Nat) : Parser σ (Option String) := fun s =>
  if s.atEnd then .ok (none, s)
  else
    -- Find the byte position after n characters
    let rec findEndPos (pos : Nat) (remaining : Nat) : Option Nat :=
      if remaining == 0 then some pos
      else if pos >= s.input.utf8ByteSize then none
      else
        let c := String.Pos.Raw.get s.input ⟨pos⟩
        findEndPos (pos + c.utf8Size) (remaining - 1)
    match findEndPos s.pos n with
    | none => .ok (none, s)
    | some endPos =>
      let result := String.Pos.Raw.extract s.input ⟨s.pos⟩ ⟨endPos⟩
      .ok (some result, s)

end Sift
