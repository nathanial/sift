import Crucible
import Sift

namespace SiftTests.Debug

open Crucible
open Sift

testSuite "Debug Combinators"

test "trace doesn't affect success" := do
  let p := trace "digit" digit
  match Parser.run p "5" with
  | .ok c => c ≡ '5'
  | .error _ => panic! "expected success"

test "trace doesn't affect failure" := do
  let p := trace "digit" digit
  match Parser.run p "x" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "trace' (no Repr) doesn't affect success" := do
  let p := trace' "string" (string "hello")
  match Parser.run p "hello" with
  | .ok s => s ≡ "hello"
  | .error _ => panic! "expected success"

test "trace' (no Repr) doesn't affect failure" := do
  let p := trace' "string" (string "hello")
  match Parser.run p "goodbye" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "traceState doesn't consume input" := do
  let p := traceState "check" *> string "hello"
  match Parser.run p "hello" with
  | .ok s => s ≡ "hello"
  | .error _ => panic! "expected success"

test "traceOnFail doesn't affect success" := do
  let p := traceOnFail "digit" digit
  match Parser.run p "5" with
  | .ok c => c ≡ '5'
  | .error _ => panic! "expected success"

test "traceOnFail doesn't affect failure" := do
  let p := traceOnFail "digit" digit
  match Parser.run p "x" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "traceOnSuccess doesn't affect success" := do
  let p := traceOnSuccess "digit" digit
  match Parser.run p "5" with
  | .ok c => c ≡ '5'
  | .error _ => panic! "expected success"

test "traceOnSuccess doesn't affect failure" := do
  let p := traceOnSuccess "digit" digit
  match Parser.run p "x" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "traceAhead doesn't consume input" := do
  let p := traceAhead 10 *> string "hello"
  match Parser.run p "hello world" with
  | .ok s => s ≡ "hello"
  | .error _ => panic! "expected success"

test "trace works with parser composition" := do
  let p := do
    let a ← trace "first" (char 'a')
    let b ← trace "second" (char 'b')
    pure (a, b)
  match Parser.run p "ab" with
  | .ok (a, b) =>
    a ≡ 'a'
    b ≡ 'b'
  | .error _ => panic! "expected success"

end SiftTests.Debug
