import Sift.Core
import Sift.Primitives

/-!
# Sift.Combinators

Parser combinators: try, orElse, many, sepBy, chainl, choice, etc.
-/

namespace Sift

/-- Try a parser, resetting position on failure (enables backtracking) -/
def attempt {α : Type} (p : Parser α) : Parser α := fun s =>
  match p s with
  | .ok result => .ok result
  | .error e =>
    -- Reset error position to original position
    .error { e with pos := s.sourcePos }

/-- Alias for attempt -/
abbrev «try» := @attempt

/-- Alternative: try second parser only if first fails without consuming input -/
def orElse {α : Type} (p : Parser α) (q : Unit → Parser α) : Parser α := fun s =>
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

instance {α : Type} : OrElse (Parser α) where
  orElse := orElse

instance : Alternative Parser where
  failure := Parser.fail "no alternative"
  orElse := orElse

/-- Choice from a list of parsers -/
def choice {α : Type} (ps : List (Parser α)) : Parser α :=
  ps.foldl (fun acc p => acc <|> p) (Parser.fail "no choice")

/-- Optional parser: returns none on failure -/
def optional {α : Type} (p : Parser α) : Parser (Option α) :=
  (some <$> p) <|> pure none

/-- Zero or more occurrences -/
partial def many {α : Type} (p : Parser α) : Parser (Array α) := fun s =>
  let rec go (acc : Array α) (state : ParseState) : Array α × ParseState :=
    match p state with
    | .ok (a, state') => go (acc.push a) state'
    | .error _ => (acc, state)
  .ok (go #[] s)

/-- One or more occurrences -/
def many1 {α : Type} (p : Parser α) : Parser (Array α) := do
  let first ← p
  let rest ← many p
  pure (#[first] ++ rest)

/-- Skip zero or more occurrences -/
partial def skipMany {α : Type} (p : Parser α) : Parser Unit := fun s =>
  let rec go (state : ParseState) : ParseState :=
    match p state with
    | .ok (_, state') => go state'
    | .error _ => state
  .ok ((), go s)

/-- Skip one or more occurrences -/
def skipMany1 {α : Type} (p : Parser α) : Parser Unit := do
  let _ ← p
  skipMany p

/-- Exactly n occurrences -/
def count {α : Type} (n : Nat) (p : Parser α) : Parser (Array α) := do
  let mut result := #[]
  for _ in [:n] do
    result := result.push (← p)
  pure result

/-- Parse until end condition (does not consume end marker) -/
partial def manyTill {α β : Type} (p : Parser α) (endp : Parser β) : Parser (Array α) := fun s =>
  let rec go (acc : Array α) (state : ParseState) : Except ParseError (Array α × ParseState) :=
    -- First try the end parser
    match endp state with
    | .ok _ => .ok (acc, state)  -- Don't consume end marker
    | .error e1 =>
      if e1.pos.offset == state.pos then
        -- End parser failed without consuming, try main parser
        match p state with
        | .ok (a, state') => go (acc.push a) state'
        | .error e2 => .error e2
      else
        .error e1
  go #[] s

/-- One or more separated by sep -/
partial def sepBy1 {α β : Type} (p : Parser α) (sep : Parser β) : Parser (Array α) := do
  let first ← p
  let rest ← many (sep *> p)
  pure (#[first] ++ rest)

/-- Zero or more separated by sep -/
partial def sepBy {α β : Type} (p : Parser α) (sep : Parser β) : Parser (Array α) :=
  (sepBy1 p sep) <|> pure #[]

/-- Zero or more ended by sep -/
partial def endBy {α β : Type} (p : Parser α) (sep : Parser β) : Parser (Array α) :=
  many (p <* sep)

/-- One or more ended by sep -/
def endBy1 {α β : Type} (p : Parser α) (sep : Parser β) : Parser (Array α) :=
  many1 (p <* sep)

/-- One or more separated and optionally ended by sep -/
partial def sepEndBy1 {α β : Type} (p : Parser α) (sep : Parser β) : Parser (Array α) := do
  let first ← p
  let more ← optional sep
  match more with
  | none => pure #[first]
  | some _ =>
    let rest ← sepEndBy1 p sep <|> pure #[]
    pure (#[first] ++ rest)

/-- Zero or more separated and optionally ended by sep -/
partial def sepEndBy {α β : Type} (p : Parser α) (sep : Parser β) : Parser (Array α) :=
  (sepEndBy1 p sep) <|> pure #[]

/-- Parse between open and close -/
def between {α β γ : Type} (open_ : Parser α) (close : Parser β) (p : Parser γ) : Parser γ :=
  open_ *> p <* close

/-- Succeed if parser fails without consuming input -/
def notFollowedBy {α : Type} (p : Parser α) : Parser Unit := fun s =>
  match p s with
  | .ok _ => .error (ParseError.fromState s "unexpected success")
  | .error e =>
    if e.pos.offset == s.pos then .ok ((), s)
    else .error e

/-- Lookahead: try parser, succeed with result but don't consume -/
def lookAhead {α : Type} (p : Parser α) : Parser α := fun s =>
  match p s with
  | .ok (a, _) => .ok (a, s)  -- Reset state but keep result
  | .error e => .error e

/-- Left-associative chain, at least one -/
partial def chainl1 {α : Type} (p : Parser α) (op : Parser (α → α → α)) : Parser α := do
  let first ← p
  let rec go (acc : α) : Parser α := do
    let more ← optional op
    match more with
    | none => pure acc
    | some f =>
      let next ← p
      go (f acc next)
  go first

/-- Left-associative chain with default -/
partial def chainl {α : Type} (p : Parser α) (op : Parser (α → α → α)) (default : α) : Parser α :=
  (chainl1 p op) <|> pure default

/-- Right-associative chain, at least one -/
partial def chainr1 {α : Type} (p : Parser α) (op : Parser (α → α → α)) : Parser α := do
  let first ← p
  let more ← optional op
  match more with
  | none => pure first
  | some f =>
    let rest ← chainr1 p op
    pure (f first rest)

/-- Right-associative chain with default -/
partial def chainr {α : Type} (p : Parser α) (op : Parser (α → α → α)) (default : α) : Parser α :=
  (chainr1 p op) <|> pure default

/-- Label a parser for better error messages -/
def label {α : Type} (p : Parser α) (name : String) : Parser α := fun s =>
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
def withFilter {α : Type} (p : Parser α) (pred : α → Bool) (msg : String) : Parser α := do
  let a ← p
  if pred a then pure a
  else Parser.fail msg

/-- Map over parser result -/
instance : Functor Parser where
  map f p := fun s =>
    match p s with
    | .ok (a, s') => .ok (f a, s')
    | .error e => .error e

/-- Applicative instance -/
instance : Applicative Parser where
  pure := Parser.pure
  seq pf pa := fun s =>
    match pf s with
    | .ok (f, s') =>
      match pa () s' with
      | .ok (a, s'') => .ok (f a, s'')
      | .error e => .error e
    | .error e => .error e

/-- Sequence, keeping left result -/
def seqLeft {α β : Type} (p : Parser α) (q : Parser β) : Parser α := do
  let a ← p
  let _ ← q
  pure a

/-- Sequence, keeping right result -/
def seqRight {α β : Type} (p : Parser α) (q : Parser β) : Parser β := do
  let _ ← p
  q

instance : SeqLeft Parser where
  seqLeft p q := seqLeft p (q ())

instance : SeqRight Parser where
  seqRight p q := seqRight p (q ())

end Sift
