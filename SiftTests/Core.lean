import Crucible
import Sift

namespace SiftTests.Core

open Crucible
open Sift

testSuite "Core Parser"

test "run succeeds on valid input" := do
  match Parser.run (pure 42) "input" with
  | .ok n => n ≡ 42
  | .error _ => panic! "expected success"

test "run fails with error message" := do
  let result : Except ParseError Nat := Parser.run (Parser.fail "oops") "input"
  match result with
  | .error e => shouldContainSubstr (toString e) "oops"
  | .ok _ => panic! "expected failure"

test "parse requires all input consumed" := do
  match Parser.parse (string "hello") "hello world" with
  | .error e => shouldContainSubstr (toString e) "trailing"
  | .ok _ => panic! "expected failure"

test "parse succeeds when all input consumed" := do
  match Parser.parse (string "hello") "hello" with
  | .ok s => s ≡ "hello"
  | .error _ => panic! "expected success"

test "position tracks offset" := do
  let p := do
    let _ ← string "abc"
    Parser.position
  match Parser.run p "abcde" with
  | .ok pos => pos.offset ≡ 3
  | .error _ => panic! "unexpected failure"

test "position tracks line and column" := do
  let p := do
    let _ ← string "ab\ncd"
    Parser.position
  match Parser.run p "ab\ncde" with
  | .ok pos =>
    pos.line ≡ 2
    pos.column ≡ 3
  | .error _ => panic! "unexpected failure"

test "position tracks multiple newlines" := do
  let p := do
    let _ ← string "a\nb\nc"
    Parser.position
  match Parser.run p "a\nb\nc" with
  | .ok pos =>
    pos.line ≡ 3
    pos.column ≡ 2
  | .error _ => panic! "unexpected failure"

#generate_tests

end SiftTests.Core
