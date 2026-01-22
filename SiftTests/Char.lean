import Crucible
import Sift

namespace SiftTests.Char

open Crucible
open Sift

testSuite "Character Classes"

test "digit matches 0-9" := do
  match Parser.run digit "0" with
  | .ok c => c ≡ '0'
  | .error _ => panic! "expected success"
  match Parser.run digit "5" with
  | .ok c => c ≡ '5'
  | .error _ => panic! "expected success"
  match Parser.run digit "9" with
  | .ok c => c ≡ '9'
  | .error _ => panic! "expected success"

test "digit fails on non-digit" := do
  match Parser.run digit "a" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "letter matches a-zA-Z" := do
  match Parser.run letter "a" with
  | .ok c => c ≡ 'a'
  | .error _ => panic! "expected success"
  match Parser.run letter "Z" with
  | .ok c => c ≡ 'Z'
  | .error _ => panic! "expected success"

test "letter fails on non-letter" := do
  match Parser.run letter "5" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "alphaNum matches letters and digits" := do
  match Parser.run alphaNum "a" with
  | .ok c => c ≡ 'a'
  | .error _ => panic! "expected success"
  match Parser.run alphaNum "5" with
  | .ok c => c ≡ '5'
  | .error _ => panic! "expected success"

test "lower matches a-z" := do
  match Parser.run lower "a" with
  | .ok c => c ≡ 'a'
  | .error _ => panic! "expected success"
  match Parser.run lower "z" with
  | .ok c => c ≡ 'z'
  | .error _ => panic! "expected success"

test "lower fails on uppercase" := do
  match Parser.run lower "A" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "upper matches A-Z" := do
  match Parser.run upper "A" with
  | .ok c => c ≡ 'A'
  | .error _ => panic! "expected success"
  match Parser.run upper "Z" with
  | .ok c => c ≡ 'Z'
  | .error _ => panic! "expected success"

test "space matches space" := do
  match Parser.run space " " with
  | .ok c => c ≡ ' '
  | .error _ => panic! "expected success"

test "whitespace matches various" := do
  match Parser.run whitespace " " with
  | .ok c => c ≡ ' '
  | .error _ => panic! "expected success"
  match Parser.run whitespace "\t" with
  | .ok c => c ≡ '\t'
  | .error _ => panic! "expected success"
  match Parser.run whitespace "\n" with
  | .ok c => c ≡ '\n'
  | .error _ => panic! "expected success"

test "spaces skips multiple" := do
  let p := spaces *> string "hello"
  match Parser.run p "   hello" with
  | .ok s => s ≡ "hello"
  | .error _ => panic! "expected success"

test "spaces skips none" := do
  let p := spaces *> string "hello"
  match Parser.run p "hello" with
  | .ok s => s ≡ "hello"
  | .error _ => panic! "expected success"

test "newline matches newline" := do
  match Parser.run newline "\n" with
  | .ok c => c ≡ '\n'
  | .error _ => panic! "expected success"

test "tab matches tab" := do
  match Parser.run tab "\t" with
  | .ok c => c ≡ '\t'
  | .error _ => panic! "expected success"

test "oneOf matches characters in set" := do
  let p := oneOf "aeiou"
  match Parser.run p "e" with
  | .ok c => c ≡ 'e'
  | .error _ => panic! "expected success"

test "oneOf fails on character not in set" := do
  let p := oneOf "aeiou"
  match Parser.run p "x" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "noneOf matches characters not in set" := do
  let p := noneOf "aeiou"
  match Parser.run p "x" with
  | .ok c => c ≡ 'x'
  | .error _ => panic! "expected success"

test "noneOf fails on character in set" := do
  let p := noneOf "aeiou"
  match Parser.run p "a" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "hexDigit matches hex characters" := do
  match Parser.run hexDigit "0" with
  | .ok c => c ≡ '0'
  | .error _ => panic! "expected success"
  match Parser.run hexDigit "a" with
  | .ok c => c ≡ 'a'
  | .error _ => panic! "expected success"
  match Parser.run hexDigit "F" with
  | .ok c => c ≡ 'F'
  | .error _ => panic! "expected success"

test "octDigit matches octal characters" := do
  match Parser.run octDigit "0" with
  | .ok c => c ≡ '0'
  | .error _ => panic! "expected success"
  match Parser.run octDigit "7" with
  | .ok c => c ≡ '7'
  | .error _ => panic! "expected success"

test "octDigit fails on 8 or 9" := do
  match Parser.run octDigit "8" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "hspace matches space and tab" := do
  match Parser.run hspace " " with
  | .ok c => c ≡ ' '
  | .error _ => panic! "expected success"
  match Parser.run hspace "\t" with
  | .ok c => c ≡ '\t'
  | .error _ => panic! "expected success"

test "hspace fails on newline" := do
  match Parser.run hspace "\n" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "hspaces skips horizontal whitespace only" := do
  let p := hspaces *> string "hello"
  match Parser.run p "  \t hello" with
  | .ok s => s ≡ "hello"
  | .error _ => panic! "expected success"

test "hspaces does not skip newlines" := do
  let p := hspaces *> newline
  match Parser.run p "  \n" with
  | .ok c => c ≡ '\n'
  | .error _ => panic! "expected success"

test "hspaces1 requires at least one" := do
  match Parser.run hspaces1 "hello" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"



end SiftTests.Char
