import Sift.Core
import Sift.Primitives

/-!
# Sift.Combinators

Parser combinators: try, orElse, many, sepBy, chainl, choice, etc.
-/

namespace Sift

/-- Try a parser, resetting position on failure (enables backtracking) -/
def attempt {σ α : Type} (p : Parser σ α) : Parser σ α := fun s =>
  match p s with
  | .ok result => .ok result
  | .error e =>
    -- Reset error position to original position
    .error { e with pos := s.sourcePos }

/-- Alias for attempt -/
abbrev «try» {σ : Type} := @attempt σ

/-- Alternative: try second parser only if first fails without consuming input -/
def orElse {σ α : Type} (p : Parser σ α) (q : Unit → Parser σ α) : Parser σ α := fun s =>
  match p s with
  | .ok result => .ok result
  | .error e =>
    -- Only try alternative if no input was consumed
    if e.pos.offset == s.pos then
      match q () s with
      | .ok result => .ok result
      | .error e2 => .error (e.merge e2)
    else
      .error e

instance {σ α : Type} : OrElse (Parser σ α) where
  orElse := orElse

instance {σ : Type} : Alternative (Parser σ) where
  failure := Parser.fail "no alternative"
  orElse := orElse

/-- Choice from a list of parsers -/
def choice {σ α : Type} (ps : List (Parser σ α)) : Parser σ α :=
  ps.foldl (fun acc p => acc <|> p) (Parser.fail "no choice")

/-- Optional parser: returns none on failure -/
def optional {σ α : Type} (p : Parser σ α) : Parser σ (Option α) :=
  (some <$> p) <|> pure none

/-- Zero or more occurrences -/
def many {σ α : Type} (p : Parser σ α) : Parser σ (Array α) := fun s =>
  let fuel := s.input.utf8ByteSize - s.pos
  let rec go : Nat → Array α → ParseState σ → Except ParseError (Array α × ParseState σ)
    | 0, acc, state => .ok (acc, state)
    | Nat.succ remaining, acc, state =>
      match p state with
      | .ok (a, state') =>
        if state'.pos <= state.pos then
          .error (ParseError.fromState state "parser did not consume input")
        else
          go remaining (acc.push a) state'
      | .error _ => .ok (acc, state)
  go fuel #[] s

/-- One or more occurrences -/
def many1 {σ α : Type} (p : Parser σ α) : Parser σ (Array α) := fun s =>
  -- First parse must succeed
  match p s with
  | .error e => .error e
  | .ok (first, s') =>
    -- Then collect zero or more additional matches
    let fuel := s'.input.utf8ByteSize - s'.pos
    let rec go : Nat → Array α → ParseState σ → Except ParseError (Array α × ParseState σ)
      | 0, acc, state => .ok (acc, state)
      | Nat.succ remaining, acc, state =>
        match p state with
        | .ok (a, state') =>
          if state'.pos <= state.pos then
            .error (ParseError.fromState state "parser did not consume input")
          else
            go remaining (acc.push a) state'
        | .error _ => .ok (acc, state)
    go fuel #[first] s'

/-- Skip zero or more occurrences -/
def skipMany {σ α : Type} (p : Parser σ α) : Parser σ Unit := fun s =>
  let fuel := s.input.utf8ByteSize - s.pos
  let rec go : Nat → ParseState σ → Except ParseError (ParseState σ)
    | 0, state => .ok state
    | Nat.succ remaining, state =>
      match p state with
      | .ok (_, state') =>
        if state'.pos <= state.pos then
          .error (ParseError.fromState state "parser did not consume input")
        else
          go remaining state'
      | .error _ => .ok state
  match go fuel s with
  | .ok state' => .ok ((), state')
  | .error e => .error e

/-- Skip one or more occurrences -/
def skipMany1 {σ α : Type} (p : Parser σ α) : Parser σ Unit := do
  let _ ← p
  skipMany p

/-- Exactly n occurrences -/
def count {σ α : Type} (n : Nat) (p : Parser σ α) : Parser σ (Array α) := do
  let mut result := #[]
  for _ in [:n] do
    result := result.push (← p)
  pure result

/-- Parse until end condition (does not consume end marker) -/
def manyTill {σ α β : Type} (p : Parser σ α) (endp : Parser σ β) : Parser σ (Array α) := fun s =>
  let fuel := s.input.utf8ByteSize - s.pos
  let rec go : Nat → Array α → ParseState σ → Except ParseError (Array α × ParseState σ)
    | 0, acc, state =>
      match endp state with
      | .ok _ => .ok (acc, state)  -- Don't consume end marker
      | .error e1 =>
        if e1.pos.offset == state.pos then
          match p state with
          | .ok (_, state') =>
            if state'.pos <= state.pos then
              .error (ParseError.fromState state "parser did not consume input")
            else
              .error e1
          | .error e2 => .error e2
        else
          .error e1
    | Nat.succ remaining, acc, state =>
      -- First try the end parser
      match endp state with
      | .ok _ => .ok (acc, state)  -- Don't consume end marker
      | .error e1 =>
        if e1.pos.offset == state.pos then
          -- End parser failed without consuming, try main parser
          match p state with
          | .ok (a, state') =>
            if state'.pos <= state.pos then
              .error (ParseError.fromState state "parser did not consume input")
            else
              go remaining (acc.push a) state'
          | .error e2 => .error e2
        else
          .error e1
  go fuel #[] s

/-- One or more separated by sep -/
def sepBy1 {σ α β : Type} (p : Parser σ α) (sep : Parser σ β) : Parser σ (Array α) := do
  let first ← p
  let rest ← many (sep *> p)
  pure (#[first] ++ rest)

/-- Zero or more separated by sep -/
def sepBy {σ α β : Type} (p : Parser σ α) (sep : Parser σ β) : Parser σ (Array α) :=
  (sepBy1 p sep) <|> pure #[]

/-- Zero or more ended by sep -/
def endBy {σ α β : Type} (p : Parser σ α) (sep : Parser σ β) : Parser σ (Array α) :=
  many (p <* sep)

/-- One or more ended by sep -/
def endBy1 {σ α β : Type} (p : Parser σ α) (sep : Parser σ β) : Parser σ (Array α) :=
  many1 (p <* sep)

/-- One or more separated and optionally ended by sep -/
def sepEndBy1 {σ α β : Type} (p : Parser σ α) (sep : Parser σ β) : Parser σ (Array α) := do
  let items ← sepBy1 p sep
  let _ ← optional sep
  pure items

/-- Zero or more separated and optionally ended by sep -/
def sepEndBy {σ α β : Type} (p : Parser σ α) (sep : Parser σ β) : Parser σ (Array α) :=
  (sepEndBy1 p sep) <|> pure #[]

/-- Parse between open and close -/
def between {σ α β γ : Type} (open_ : Parser σ α) (close : Parser σ β) (p : Parser σ γ) : Parser σ γ :=
  open_ *> p <* close

/-- Succeed if parser fails without consuming input -/
def notFollowedBy {σ α : Type} (p : Parser σ α) : Parser σ Unit := fun s =>
  match p s with
  | .ok _ => .error (ParseError.fromState s "unexpected success")
  | .error e =>
    if e.pos.offset == s.pos then .ok ((), s)
    else .error e

/-- Lookahead: try parser, succeed with result but don't consume -/
def lookAhead {σ α : Type} (p : Parser σ α) : Parser σ α := fun s =>
  match p s with
  | .ok (a, _) => .ok (a, s)  -- Reset state but keep result
  | .error e => .error e

/-- Left-associative chain, at least one -/
def chainl1 {σ α : Type} (p : Parser σ α) (op : Parser σ (α → α → α)) : Parser σ α := do
  let first ← p
  let rest ← many (do
    let f ← op
    let next ← p
    pure (f, next))
  pure (rest.foldl (fun acc (f, next) => f acc next) first)

/-- Left-associative chain with default -/
def chainl {σ α : Type} (p : Parser σ α) (op : Parser σ (α → α → α)) (default : α) : Parser σ α :=
  (chainl1 p op) <|> pure default

/-- Right-associative chain, at least one -/
def chainr1 {σ α : Type} (p : Parser σ α) (op : Parser σ (α → α → α)) : Parser σ α := do
  let first ← p
  let rest ← many (do
    let f ← op
    let next ← p
    pure (f, next))
  let rec go : α → List ((α → α → α) × α) → α
    | left, [] => left
    | left, (f, right) :: more => f left (go right more)
  pure (go first rest.toList)

/-- Right-associative chain with default -/
def chainr {σ α : Type} (p : Parser σ α) (op : Parser σ (α → α → α)) (default : α) : Parser σ α :=
  (chainr1 p op) <|> pure default

/-! ## Error Recovery Combinators -/

/-- Recover from parser failure by calling handler with the error.
    Handler receives the error and can attempt recovery or fail again. -/
def recover {σ α : Type} (p : Parser σ α) (handler : ParseError → Parser σ α) : Parser σ α := fun s =>
  match p s with
  | .ok result => .ok result
  | .error e => handler e s

/-- Return default value on parser failure (simple recovery). -/
def recoverWith {σ α : Type} (p : Parser σ α) (default : α) : Parser σ α := fun s =>
  match p s with
  | .ok result => .ok result
  | .error _ => .ok (default, s)

/-- Skip input until parser succeeds (for resynchronization).
    Returns the result of the successful parse. -/
def skipUntil {σ α : Type} (p : Parser σ α) : Parser σ α := fun s =>
  let fuel := s.input.utf8ByteSize - s.pos
  let rec go : Nat → ParseState σ → Except ParseError (α × ParseState σ)
    | 0, state => .error (ParseError.unexpectedEof state)
    | Nat.succ remaining, state =>
      match p state with
      | .ok result => .ok result
      | .error _ =>
        if state.atEnd then .error (ParseError.unexpectedEof state)
        else go remaining state.advance
  go fuel s

/-- Skip input until parser succeeds, return Unit (just sync position). -/
def skipUntil_ {σ α : Type} (p : Parser σ α) : Parser σ Unit := fun s =>
  match skipUntil p s with
  | .ok (_, s') => .ok ((), s')
  | .error e => .error e

/-- Recover with default value and skip to synchronization point.
    On error: returns default and advances to where syncParser succeeds. -/
def withRecovery {σ α β : Type} (p : Parser σ α) (default : α) (syncParser : Parser σ β) : Parser σ α := fun s =>
  match p s with
  | .ok result => .ok result
  | .error _ =>
    match skipUntil syncParser s with
    | .ok (_, s') => .ok (default, s')
    | .error e => .error e

/-- Parse many items, recovering from individual failures.
    Collects successful parses; on failure, skips to sync point and continues. -/
def manyRecover {σ α β : Type} (p : Parser σ α) (syncParser : Parser σ β) : Parser σ (Array α) := fun s =>
  let fuel := s.input.utf8ByteSize - s.pos
  let rec go : Nat → Array α → ParseState σ → Except ParseError (Array α × ParseState σ)
    | 0, acc, state => .ok (acc, state)
    | Nat.succ remaining, acc, state =>
      if state.atEnd then .ok (acc, state)
      else
        match p state with
        | .ok (a, state') =>
          if state'.pos <= state.pos then .ok (acc, state)  -- No progress, stop
          else go remaining (acc.push a) state'
        | .error _ =>
          -- Try to skip to sync point
          match skipUntil syncParser state with
          | .ok (_, state') =>
            if state'.pos <= state.pos then .ok (acc, state)  -- No progress, stop
            else go remaining acc state'
          | .error _ => .ok (acc, state)  -- Can't sync, return what we have
  go fuel #[] s

/-! ## Permutation Combinators -/

/-- Type class for permutation parsing. Allows `permute` to work with
    tuples of 2, 3, or 4 parsers. -/
class Permutable (σ : Type) (Parsers : Type) (Result : outParam Type) where
  /-- Parse elements in any order, each exactly once.
      Returns results in the order of the parser tuple, not parse order. -/
  permute : Parsers → Parser σ Result

/-- Permute two parsers. -/
instance {σ α β : Type} : Permutable σ (Parser σ α × Parser σ β) (α × β) where
  permute := fun (p1, p2) =>
    -- Try p1 first, then p2
    (do let a ← attempt p1; let b ← p2; pure (a, b))
    <|>
    -- Or p2 first, then p1
    (do let b ← attempt p2; let a ← p1; pure (a, b))

/-- Permute three parsers. -/
instance {σ α β γ : Type} : Permutable σ (Parser σ α × Parser σ β × Parser σ γ) (α × β × γ) where
  permute := fun (p1, p2, p3) =>
    -- p1 first
    (do let a ← attempt p1
        let (b, c) ← Permutable.permute (p2, p3)
        pure (a, b, c))
    <|>
    -- p2 first
    (do let b ← attempt p2
        let (a, c) ← Permutable.permute (p1, p3)
        pure (a, b, c))
    <|>
    -- p3 first
    (do let c ← attempt p3
        let (a, b) ← Permutable.permute (p1, p2)
        pure (a, b, c))

/-- Permute four parsers. -/
instance {σ α β γ δ : Type} : Permutable σ (Parser σ α × Parser σ β × Parser σ γ × Parser σ δ) (α × β × γ × δ) where
  permute := fun (p1, p2, p3, p4) =>
    -- p1 first
    (do let a ← attempt p1
        let (b, c, d) ← Permutable.permute (p2, p3, p4)
        pure (a, b, c, d))
    <|>
    -- p2 first
    (do let b ← attempt p2
        let (a, c, d) ← Permutable.permute (p1, p3, p4)
        pure (a, b, c, d))
    <|>
    -- p3 first
    (do let c ← attempt p3
        let (a, b, d) ← Permutable.permute (p1, p2, p4)
        pure (a, b, c, d))
    <|>
    -- p4 first
    (do let d ← attempt p4
        let (a, b, c) ← Permutable.permute (p1, p2, p3)
        pure (a, b, c, d))

/-- Convenience function for permutation parsing.
    Usage: `permute (p1, p2)` or `permute (p1, p2, p3)` etc. -/
def permute {σ Parsers Result : Type} [Permutable σ Parsers Result] (parsers : Parsers) : Parser σ Result :=
  Permutable.permute parsers

/-- Label a parser for better error messages -/
def label {σ α : Type} (p : Parser σ α) (name : String) : Parser σ α := fun s =>
  match p s with
  | .ok result => .ok result
  | .error e =>
    if e.pos.offset == s.pos then
      .error (e.expecting name)
    else
      .error e

/-- Operator for label: parser <?> "name" -/
infixl:0 " <?> " => label

/-- Parse with a condition that must be satisfied -/
def withFilter {σ α : Type} (p : Parser σ α) (pred : α → Bool) (msg : String) : Parser σ α := do
  let a ← p
  if pred a then pure a
  else Parser.fail msg

/-- Map over parser result -/
instance {σ : Type} : Functor (Parser σ) where
  map f p := fun s =>
    match p s with
    | .ok (a, s') => .ok (f a, s')
    | .error e => .error e

/-- Applicative instance -/
instance {σ : Type} : Applicative (Parser σ) where
  pure := Parser.pure
  seq pf pa := fun s =>
    match pf s with
    | .ok (f, s') =>
      match pa () s' with
      | .ok (a, s'') => .ok (f a, s'')
      | .error e => .error e
    | .error e => .error e

/-- Sequence, keeping left result -/
def seqLeft {σ α β : Type} (p : Parser σ α) (q : Parser σ β) : Parser σ α := do
  let a ← p
  let _ ← q
  pure a

/-- Sequence, keeping right result -/
def seqRight {σ α β : Type} (p : Parser σ α) (q : Parser σ β) : Parser σ β := do
  let _ ← p
  q

instance {σ : Type} : SeqLeft (Parser σ) where
  seqLeft p q := seqLeft p (q ())

instance {σ : Type} : SeqRight (Parser σ) where
  seqRight p q := seqRight p (q ())

end Sift
