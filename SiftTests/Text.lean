import Crucible
import Sift

namespace SiftTests.Text

open Crucible
open Sift

testSuite "Text Utilities"

test "manyChars collects characters" := do
  match Parser.run (manyChars digit) "123abc" with
  | .ok s => s ≡ "123"
  | .error _ => panic! "expected success"

test "many1Chars requires at least one" := do
  match Parser.run (many1Chars digit) "abc" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "natural parses unsigned integer" := do
  match Parser.run natural "0" with
  | .ok n => n ≡ 0
  | .error _ => panic! "expected success"
  match Parser.run natural "42" with
  | .ok n => n ≡ 42
  | .error _ => panic! "expected success"
  match Parser.run natural "12345" with
  | .ok n => n ≡ 12345
  | .error _ => panic! "expected success"

test "natural fails on non-digit start" := do
  match Parser.run natural "abc" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "integer parses positive" := do
  match Parser.run integer "42" with
  | .ok n => n ≡ 42
  | .error _ => panic! "expected success"
  match Parser.run integer "+42" with
  | .ok n => n ≡ 42
  | .error _ => panic! "expected success"

test "integer parses negative" := do
  match Parser.run integer "-42" with
  | .ok n => n ≡ (-42)
  | .error _ => panic! "expected success"

test "hexNatural parses hex" := do
  match Parser.run hexNatural "ff" with
  | .ok n => n ≡ 255
  | .error _ => panic! "expected success"
  match Parser.run hexNatural "FF" with
  | .ok n => n ≡ 255
  | .error _ => panic! "expected success"
  match Parser.run hexNatural "10" with
  | .ok n => n ≡ 16
  | .error _ => panic! "expected success"
  match Parser.run hexNatural "a5" with
  | .ok n => n ≡ 165
  | .error _ => panic! "expected success"

test "binNatural parses binary" := do
  match Parser.run binNatural "1010" with
  | .ok n => n ≡ 10
  | .error _ => panic! "expected success"
  match Parser.run binNatural "11111111" with
  | .ok n => n ≡ 255
  | .error _ => panic! "expected success"

test "octNatural parses octal" := do
  match Parser.run octNatural "77" with
  | .ok n => n ≡ 63
  | .error _ => panic! "expected success"
  match Parser.run octNatural "10" with
  | .ok n => n ≡ 8
  | .error _ => panic! "expected success"

test "identifier parses valid identifiers" := do
  match Parser.run identifier "foo" with
  | .ok s => s ≡ "foo"
  | .error _ => panic! "expected success"
  match Parser.run identifier "_bar" with
  | .ok s => s ≡ "_bar"
  | .error _ => panic! "expected success"
  match Parser.run identifier "x123" with
  | .ok s => s ≡ "x123"
  | .error _ => panic! "expected success"
  match Parser.run identifier "camelCase" with
  | .ok s => s ≡ "camelCase"
  | .error _ => panic! "expected success"

test "identifier fails on digit start" := do
  match Parser.run identifier "123abc" with
  | .error _ => pure ()
  | .ok _ => panic! "expected failure"

test "lexeme skips trailing whitespace" := do
  let p := do
    let x ← lexeme natural
    let y ← natural
    pure (x, y)
  match Parser.run p "42   37" with
  | .ok (x, y) =>
    x ≡ 42
    y ≡ 37
  | .error _ => panic! "expected success"

test "symbol parses string and skips whitespace" := do
  let p := do
    let _ ← symbol "let"
    identifier
  match Parser.run p "let   foo" with
  | .ok s => s ≡ "foo"
  | .error _ => panic! "expected success"

test "parens parses between parentheses" := do
  let p := parens (lexeme natural)
  match Parser.run p "( 42 )" with
  | .ok n => n ≡ 42
  | .error _ => panic! "expected success"

test "braces parses between braces" := do
  let p := braces (lexeme natural)
  match Parser.run p "{ 42 }" with
  | .ok n => n ≡ 42
  | .error _ => panic! "expected success"

test "brackets parses between brackets" := do
  let p := brackets (lexeme natural)
  match Parser.run p "[ 42 ]" with
  | .ok n => n ≡ 42
  | .error _ => panic! "expected success"

test "commaSep parses comma-separated values" := do
  let p := commaSep natural
  match Parser.run p "1, 2, 3" with
  | .ok arr =>
    arr.size ≡ 3
    arr[0]! ≡ 1
    arr[1]! ≡ 2
    arr[2]! ≡ 3
  | .error _ => panic! "expected success"

test "commaSep returns empty on no match" := do
  let p := commaSep natural
  match Parser.run p "" with
  | .ok arr => arr.size ≡ 0
  | .error _ => panic! "expected success"

test "semiSep parses semicolon-separated values" := do
  let p := semiSep natural
  match Parser.run p "1; 2; 3" with
  | .ok arr =>
    arr.size ≡ 3
    arr[0]! ≡ 1
  | .error _ => panic! "expected success"

test "stringLiteral parses simple string" := do
  match Parser.run stringLiteral "\"hello\"" with
  | .ok s => s ≡ "hello"
  | .error _ => panic! "expected success"

test "stringLiteral parses escapes" := do
  match Parser.run stringLiteral "\"hello\\nworld\"" with
  | .ok s => s ≡ "hello\nworld"
  | .error _ => panic! "expected success"
  match Parser.run stringLiteral "\"tab\\there\"" with
  | .ok s => s ≡ "tab\there"
  | .error _ => panic! "expected success"

test "charLiteral parses character" := do
  match Parser.run charLiteral "'a'" with
  | .ok c => c ≡ 'a'
  | .error _ => panic! "expected success"

test "charLiteral parses escape" := do
  match Parser.run charLiteral "'\\n'" with
  | .ok c => c ≡ '\n'
  | .error _ => panic! "expected success"

#generate_tests

end SiftTests.Text
