import Crucible
import Sift

namespace SiftTests.Primitives

open Crucible
open Sift

testSuite "Primitives"

test "satisfy matches predicate" := do
  match Parser.run (satisfy Char.isDigit) "5abc" with
  | .ok c => c ≡ '5'
  | .error _ => panic! "expected success"

test "satisfy fails on non-match" := do
  match Parser.run (satisfy Char.isDigit) "abc" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "satisfy fails on empty input" := do
  match Parser.run (satisfy Char.isDigit) "" with
  | .error e => shouldContainSubstr (toString e) "end of input"
  | .ok _ => panic! "expected failure"

test "char matches exact character" := do
  match Parser.run (char 'a') "abc" with
  | .ok c => c ≡ 'a'
  | .error _ => panic! "expected success"

test "char fails on wrong character" := do
  match Parser.run (char 'a') "bcd" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "anyChar matches any character" := do
  match Parser.run anyChar "xyz" with
  | .ok c => c ≡ 'x'
  | .error _ => panic! "expected success"

test "anyChar fails on empty input" := do
  match Parser.run anyChar "" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "string matches exact string" := do
  match Parser.run (string "hello") "hello world" with
  | .ok s => s ≡ "hello"
  | .error _ => panic! "expected success"

test "string fails on partial match" := do
  match Parser.run (string "hello") "hell" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "string fails on wrong string" := do
  match Parser.run (string "hello") "world" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "eof succeeds at end" := do
  match Parser.run eof "" with
  | .ok () => pure ()
  | .error _ => panic! "expected success"

test "eof fails with remaining input" := do
  match Parser.run eof "x" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "peek does not consume" := do
  let p := do
    let c ← peek
    let d ← anyChar
    pure (c, d)
  match Parser.run p "ab" with
  | .ok (c, d) =>
    c ≡ some 'a'
    d ≡ 'a'
  | .error _ => panic! "expected success"

test "peek returns none at end" := do
  match Parser.run peek "" with
  | .ok c => c ≡ none
  | .error _ => panic! "expected success"

test "take gets exact count" := do
  match Parser.run (take 3) "abcde" with
  | .ok s => s ≡ "abc"
  | .error _ => panic! "expected success"

test "take fails if not enough" := do
  match Parser.run (take 5) "abc" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "takeWhile collects matching" := do
  match Parser.run (takeWhile Char.isDigit) "123abc" with
  | .ok s => s ≡ "123"
  | .error _ => panic! "expected success"

test "takeWhile returns empty on no match" := do
  match Parser.run (takeWhile Char.isDigit) "abc" with
  | .ok s => s ≡ ""
  | .error _ => panic! "expected success"

test "takeWhile1 requires at least one" := do
  match Parser.run (takeWhile1 Char.isDigit) "abc" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"



end SiftTests.Primitives
