import Crucible
import Sift

namespace SiftTests.Combinators

open Crucible
open Sift

testSuite "Combinators"

test "try backtracks on failure" := do
  let p := attempt (string "abc") <|> string "ab"
  match Parser.run p "ab" with
  | .ok s => s ≡ "ab"
  | .error _ => panic! "expected success"

test "orElse tries alternative on empty failure" := do
  let p := char 'a' <|> char 'b'
  match Parser.run p "b" with
  | .ok c => c ≡ 'b'
  | .error _ => panic! "expected success"

test "orElse does not try alternative after consuming" := do
  let p := string "abc" <|> string "ab"
  match Parser.run p "abd" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "choice selects first matching" := do
  let p := choice [string "foo", string "bar", string "baz"]
  match Parser.run p "bar" with
  | .ok s => s ≡ "bar"
  | .error _ => panic! "expected success"

test "optional returns some on match" := do
  match Parser.run (Sift.optional (char 'a')) "abc" with
  | .ok c => c ≡ some 'a'
  | .error _ => panic! "expected success"

test "optional returns none on no match" := do
  match Parser.run (Sift.optional (char 'a')) "bcd" with
  | .ok c => c ≡ none
  | .error _ => panic! "expected success"

test "many collects zero" := do
  match Parser.run (many digit) "abc" with
  | .ok arr => arr.size ≡ 0
  | .error _ => panic! "expected success"

test "many collects multiple" := do
  match Parser.run (many digit) "123abc" with
  | .ok arr =>
    arr.size ≡ 3
    arr[0]! ≡ '1'
    arr[1]! ≡ '2'
    arr[2]! ≡ '3'
  | .error _ => panic! "expected success"

test "many1 requires at least one" := do
  match Parser.run (many1 digit) "abc" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "many1 collects multiple" := do
  match Parser.run (many1 digit) "456xyz" with
  | .ok arr =>
    arr.size ≡ 3
    arr[0]! ≡ '4'
  | .error _ => panic! "expected success"

test "count parses exactly n" := do
  match Parser.run (count 3 digit) "12345" with
  | .ok arr =>
    arr.size ≡ 3
    arr[0]! ≡ '1'
    arr[2]! ≡ '3'
  | .error _ => panic! "expected success"

test "count fails if not enough" := do
  match Parser.run (count 5 digit) "123" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "sepBy parses separated values" := do
  match Parser.run (sepBy digit (char ',')) "1,2,3" with
  | .ok arr =>
    arr.size ≡ 3
    arr[0]! ≡ '1'
    arr[1]! ≡ '2'
    arr[2]! ≡ '3'
  | .error _ => panic! "expected success"

test "sepBy returns empty on no match" := do
  match Parser.run (sepBy digit (char ',')) "abc" with
  | .ok arr => arr.size ≡ 0
  | .error _ => panic! "expected success"

test "sepBy1 requires at least one" := do
  match Parser.run (sepBy1 digit (char ',')) "abc" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "between extracts middle" := do
  match Parser.run (between (char '(') (char ')') (many letter)) "(abc)" with
  | .ok arr => arr.size ≡ 3
  | .error _ => panic! "expected success"

test "chainl1 left associates" := do
  let num := natural
  let sub := char '-' *> pure (fun a b => a - b)
  match Parser.run (chainl1 num sub) "10-3-2" with
  | .ok n => n ≡ 5  -- (10 - 3) - 2 = 5
  | .error _ => panic! "expected success"

test "chainr1 right associates" := do
  let num := natural
  let pow := char '^' *> pure (fun a b => a ^ b)
  match Parser.run (chainr1 num pow) "2^3^2" with
  | .ok n => n ≡ 512  -- 2 ^ (3 ^ 2) = 2 ^ 9 = 512
  | .error _ => panic! "expected success"

test "notFollowedBy succeeds when parser fails" := do
  let p := string "if" <* notFollowedBy alphaNum
  match Parser.run p "if " with
  | .ok s => s ≡ "if"
  | .error _ => panic! "expected success"

test "notFollowedBy fails when parser succeeds" := do
  let p := string "if" <* notFollowedBy alphaNum
  match Parser.run p "iffy" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "lookAhead does not consume" := do
  let p := do
    let _ ← lookAhead (string "abc")
    string "ab"
  match Parser.run p "abc" with
  | .ok s => s ≡ "ab"
  | .error _ => panic! "expected success"

test "manyTill collects until end" := do
  let p := manyTill anyChar (string "END")
  match Parser.run p "abcEND" with
  | .ok arr =>
    arr.size ≡ 3
    arr[0]! ≡ 'a'
  | .error _ => panic! "expected success"

test "label improves error message" := do
  let p := digit <?> "a digit"
  match Parser.run p "abc" with
  | .error e => shouldContainSubstr (toString e) "a digit"
  | .ok _ => panic! "expected failure"

#generate_tests

end SiftTests.Combinators
