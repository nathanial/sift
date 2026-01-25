import Crucible
import Sift

namespace SiftTests.Indent

open Crucible
open Sift

testSuite "Indentation Parsing"

/-! ## Basic Position Primitives -/

test "getColumn returns current column" := do
  let p := string "ab" *> getColumn
  match Parser.run p "abc" with
  | .ok col => col ≡ 3
  | .error e => panic! s!"unexpected error: {e}"

test "getColumn at start is 1" := do
  let p := getColumn
  match Parser.run p "abc" with
  | .ok col => col ≡ 1
  | .error e => panic! s!"unexpected error: {e}"

test "getLine returns current line" := do
  let p := string "a\nb" *> getLine
  match Parser.run p "a\nb" with
  | .ok line => line ≡ 2
  | .error e => panic! s!"unexpected error: {e}"

test "getLine at start is 1" := do
  let p := getLine
  match Parser.run p "abc" with
  | .ok line => line ≡ 1
  | .error e => panic! s!"unexpected error: {e}"

test "measureIndent counts leading spaces" := do
  let p := measureIndent
  match Parser.run p "    hello" with
  | .ok n => n ≡ 4
  | .error e => panic! s!"unexpected error: {e}"

test "measureIndent returns 0 for no spaces" := do
  let p := measureIndent
  match Parser.run p "hello" with
  | .ok n => n ≡ 0
  | .error e => panic! s!"unexpected error: {e}"

test "measureIndent returns 0 if not at column 1" := do
  let p := string "ab" *> measureIndent
  match Parser.run p "ab  cd" with
  | .ok n => n ≡ 0
  | .error e => panic! s!"unexpected error: {e}"

test "atColumn succeeds at correct column" := do
  let p := string "ab" *> atColumn 3
  match Parser.run p "abc" with
  | .ok () => pure ()
  | .error e => panic! s!"unexpected error: {e}"

test "atColumn fails at wrong column" := do
  let p := string "ab" *> atColumn 5
  match Parser.run p "abc" with
  | .ok () => panic! "expected failure"
  | .error e => shouldContainSubstr (toString e) "expected column 5"

test "indented succeeds when at column greater than reference" := do
  -- After consuming "abc", we're at column 4, which is > 2
  let p := string "abc" *> indented 2
  match Parser.run p "abcdef" with
  | .ok () => pure ()
  | .error e => panic! s!"unexpected error: {e}"

test "indented fails when not more indented" := do
  -- At column 1, which is not > 2
  let p := indented 2
  match Parser.run p "test" with
  | .ok () => panic! "expected failure"
  | .error e => shouldContainSubstr (toString e) "expected indentation"

test "onLine succeeds on same line" := do
  let p := string "ab" *> onLine 1
  match Parser.run p "abc" with
  | .ok () => pure ()
  | .error e => panic! s!"unexpected error: {e}"

test "onLine fails on different line" := do
  let p := string "a\nb" *> onLine 1
  match Parser.run p "a\nb" with
  | .ok () => panic! "expected failure"
  | .error e => shouldContainSubstr (toString e) "expected line 1"

/-! ## Indent State -/

test "IndentState starts with level 0" := do
  let state := IndentState.init
  state.currentIndent ≡ 0

test "IndentState.pushIndent adds level" := do
  let state := IndentState.init.pushIndent 4
  state.currentIndent ≡ 4
  state.indentStack.length ≡ 2

test "IndentState.popTo removes levels" := do
  let state := IndentState.init.pushIndent 4 |>.pushIndent 8
  let (newState, count) := state.popTo 4
  newState.currentIndent ≡ 4
  count ≡ 1

test "IndentState.popTo handles multiple levels" := do
  let state := IndentState.init.pushIndent 2 |>.pushIndent 4 |>.pushIndent 8
  let (newState, count) := state.popTo 2
  newState.currentIndent ≡ 2
  count ≡ 2

test "IndentState.popTo to base level" := do
  let state := IndentState.init.pushIndent 4 |>.pushIndent 8
  let (newState, count) := state.popTo 0
  newState.currentIndent ≡ 0
  count ≡ 2

/-! ## Process Indent -/

test "processIndent detects indent increase" := do
  -- Start at column 1 with 4 spaces
  let p := processIndent
  match Parser.runWith p "    x" IndentState.init with
  | .ok change => change ≡ IndentChange.indent
  | .error e => panic! s!"unexpected error: {e}"

test "processIndent detects same level" := do
  let state := IndentState.init.pushIndent 4
  let p := processIndent
  match Parser.runWith p "    x" state with
  | .ok change => change ≡ IndentChange.same
  | .error e => panic! s!"unexpected error: {e}"

test "processIndent detects dedent" := do
  let state := IndentState.init.pushIndent 4
  let p := processIndent
  match Parser.runWith p "x" state with
  | .ok change =>
    match change with
    | .dedent n => n ≡ 1
    | _ => panic! "expected dedent"
  | .error e => panic! s!"unexpected error: {e}"

test "processIndent fails on invalid indent" := do
  let state := IndentState.init.pushIndent 4  -- valid: [4, 0]
  let p := processIndent
  -- Dedent to 2, which is not in the stack
  match Parser.runWith p "  x" state with
  | .ok _ => panic! "expected failure for invalid indent"
  | .error e => shouldContainSubstr (toString e) "inconsistent indentation"

/-! ## Block Parsing -/

test "block parses simple indented content" := do
  let p := do
    let _ ← string "if:"
    eol
    block (takeWhile1 Char.isAlpha)
  match Parser.runWith p "if:\n    x" IndentState.init with
  | .ok arr =>
    arr.size ≡ 1
    arr[0]! ≡ "x"
  | .error e => panic! s!"unexpected error: {e}"

test "block parses multiple lines at same indent" := do
  let p := do
    let _ ← string "block:"
    eol
    block (takeWhile1 Char.isAlpha)
  match Parser.runWith p "block:\n    a\n    b\n    c" IndentState.init with
  | .ok arr =>
    arr.size ≡ 3
    arr[0]! ≡ "a"
    arr[1]! ≡ "b"
    arr[2]! ≡ "c"
  | .error e => panic! s!"unexpected error: {e}"

test "block fails without increased indent" := do
  let p := do
    let _ ← string "if:"
    eol
    block (takeWhile1 Char.isAlpha)
  match Parser.runWith p "if:\nx" IndentState.init with
  | .ok _ => panic! "expected failure"
  | .error e => shouldContainSubstr (toString e) "expected indented block"

/-! ## Python-Like Language Example -/

-- A simple statement type for testing
inductive TestStmt where
  | assign : String → String → TestStmt  -- name = value
  | ifStmt : String → Array TestStmt → TestStmt  -- if cond: block
  | pass : TestStmt
  deriving Repr, BEq

-- Simple identifier parser
def identifier : Parser IndentState String :=
  takeWhile1 Char.isAlpha

-- Parse "x = value"
def parseAssign : Parser IndentState TestStmt := do
  let name ← identifier
  hspaces
  let _ ← char '='
  hspaces
  let value ← takeWhile (fun c => c != '\n')
  pure (.assign name value)

-- Parse "pass"
def parsePass : Parser IndentState TestStmt :=
  string "pass" *> pure .pass

-- Mutual recursion for if statements
mutual
  partial def parseTestStmt : Parser IndentState TestStmt :=
    -- Try parsePass before parseAssign since "pass" would otherwise be parsed as an identifier
    attempt parseIf <|> attempt parsePass <|> parseAssign

  partial def parseIf : Parser IndentState TestStmt := do
    let _ ← string "if"
    hspaces1
    let cond ← identifier
    let _ ← char ':'
    eol
    let body ← block parseTestStmt
    pure (.ifStmt cond body)
end

test "parse simple assignment" := do
  let p := parseAssign
  match Parser.runWith p "x = 42" IndentState.init with
  | .ok stmt =>
    match stmt with
    | .assign name value =>
      name ≡ "x"
      value ≡ "42"
    | _ => panic! "expected assign"
  | .error e => panic! s!"unexpected error: {e}"

test "parse if with single statement" := do
  let p := parseIf
  match Parser.runWith p "if x:\n    pass" IndentState.init with
  | .ok stmt =>
    match stmt with
    | .ifStmt cond body =>
      cond ≡ "x"
      body.size ≡ 1
    | _ => panic! "expected if statement"
  | .error e => panic! s!"unexpected error: {e}"

test "parse if with multiple statements" := do
  let p := parseIf
  match Parser.runWith p "if test:\n    a = 1\n    b = 2" IndentState.init with
  | .ok stmt =>
    match stmt with
    | .ifStmt cond body =>
      cond ≡ "test"
      body.size ≡ 2
    | _ => panic! "expected if statement"
  | .error e => panic! s!"unexpected error: {e}"

/-! ## Edge Cases -/

test "empty input at start" := do
  let p := measureIndent
  match Parser.run p "" with
  | .ok n => n ≡ 0
  | .error _ => pure ()  -- EOF is also acceptable

test "tabs are not counted as indent spaces" := do
  let p := measureIndent
  match Parser.run p "\thello" with
  | .ok n => n ≡ 0  -- tabs are not spaces
  | .error e => panic! s!"unexpected error: {e}"

test "mixed spaces and content" := do
  let p := do
    let indent ← measureIndent
    let word ← takeWhile1 Char.isAlpha
    pure (indent, word)
  match Parser.run p "  hello" with
  | .ok (indent, word) =>
    indent ≡ 2
    word ≡ "hello"
  | .error e => panic! s!"unexpected error: {e}"

end SiftTests.Indent
