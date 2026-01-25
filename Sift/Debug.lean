import Sift.Core
import Sift.Combinators

namespace Sift

/-!
# Debug Combinators

Combinators for debugging parsers during development by tracing input consumption
and parser decisions. Uses `dbgTrace` which only outputs in debug builds.
-/

/-- Trace a parser: logs entry, exit, and result -/
def trace {σ α : Type} [Repr α] (name : String) (p : Parser σ α) : Parser σ α := fun s =>
  dbgTrace s!"[{name}] entering at line {s.line}, col {s.column}" fun _ =>
  match p s with
  | .ok (a, s') =>
    dbgTrace s!"[{name}] succeeded: {repr a}" fun _ =>
    .ok (a, s')
  | .error e =>
    dbgTrace s!"[{name}] failed: {e.message}" fun _ =>
    .error e

/-- Trace parser without requiring Repr (just logs success/fail) -/
def trace' {σ α : Type} (name : String) (p : Parser σ α) : Parser σ α := fun s =>
  dbgTrace s!"[{name}] entering at line {s.line}, col {s.column}" fun _ =>
  match p s with
  | .ok (a, s') =>
    dbgTrace s!"[{name}] succeeded at line {s'.line}, col {s'.column}" fun _ =>
    .ok (a, s')
  | .error e =>
    dbgTrace s!"[{name}] failed: {e.message}" fun _ =>
    .error e

/-- Log current parser state -/
def traceState {σ : Type} (msg : String) : Parser σ Unit := fun s =>
  dbgTrace s!"[state] {msg}: pos={s.pos}, line={s.line}, col={s.column}" fun _ =>
  .ok ((), s)

/-- Trace only when parser fails -/
def traceOnFail {σ α : Type} (name : String) (p : Parser σ α) : Parser σ α := fun s =>
  match p s with
  | .ok result => .ok result
  | .error e =>
    dbgTrace s!"[{name}] failed at line {e.pos.line}, col {e.pos.column}: {e.message}" fun _ =>
    .error e

/-- Trace only when parser succeeds -/
def traceOnSuccess {σ α : Type} [Repr α] (name : String) (p : Parser σ α) : Parser σ α := fun s =>
  match p s with
  | .ok (a, s') =>
    dbgTrace s!"[{name}] succeeded: {repr a}" fun _ =>
    .ok (a, s')
  | .error e => .error e

/-- Show the next N characters of input (for debugging) -/
def traceAhead {σ : Type} (n : Nat := 20) : Parser σ Unit := fun s =>
  let remaining := s.input.drop s.pos
  let preview := remaining.take n
  let ellipsis := if remaining.length > n then "..." else ""
  dbgTrace s!"[ahead] \"{preview}{ellipsis}\"" fun _ =>
  .ok ((), s)

end Sift
