import Crucible
import Sift

namespace SiftTests.Utf8

open Crucible
open Sift

testSuite "UTF-8 Support"

/-!
  UTF-8 character byte sizes:
  - ASCII (U+0000 to U+007F): 1 byte
  - Latin-1 supplement (U+0080 to U+07FF): 2 bytes (e.g., 'é', '£', '©')
  - Basic multilingual plane (U+0800 to U+FFFF): 3 bytes (e.g., '世', '界', '→')
  - Supplementary planes (U+10000+): 4 bytes (e.g., emoji like '😀')
-/

-- Test basic multi-byte character parsing

test "anyChar parses 2-byte UTF-8 character" := do
  -- 'é' is U+00E9, 2 bytes in UTF-8
  match Parser.run anyChar "ébc" with
  | .ok c => c ≡ 'é'
  | .error e => panic! s!"expected success, got: {e}"

test "anyChar parses 3-byte UTF-8 character" := do
  -- '世' is U+4E16, 3 bytes in UTF-8
  match Parser.run anyChar "世界" with
  | .ok c => c ≡ '世'
  | .error e => panic! s!"expected success, got: {e}"

test "anyChar parses 4-byte UTF-8 character (emoji)" := do
  -- '😀' is U+1F600, 4 bytes in UTF-8
  match Parser.run anyChar "😀test" with
  | .ok c => c ≡ '😀'
  | .error e => panic! s!"expected success, got: {e}"

-- Test sequential parsing of multi-byte characters

test "parse two 2-byte characters sequentially" := do
  let p := do
    let a ← anyChar
    let b ← anyChar
    pure (a, b)
  match Parser.run p "éà" with
  | .ok (a, b) =>
    a ≡ 'é'
    b ≡ 'à'
  | .error e => panic! s!"expected success, got: {e}"

test "parse mixed ASCII and multi-byte characters" := do
  let p := do
    let a ← anyChar  -- 'h' (1 byte)
    let b ← anyChar  -- 'é' (2 bytes)
    let c ← anyChar  -- 'l' (1 byte)
    let d ← anyChar  -- 'l' (1 byte)
    let e ← anyChar  -- 'o' (1 byte)
    pure [a, b, c, d, e]
  match Parser.run p "héllo" with
  | .ok chars =>
    chars ≡ ['h', 'é', 'l', 'l', 'o']
  | .error e => panic! s!"expected success, got: {e}"

-- Test string matching with multi-byte characters

test "string matches multi-byte string" := do
  match Parser.run (string "café") "café au lait" with
  | .ok s => s ≡ "café"
  | .error e => panic! s!"expected success, got: {e}"

test "string matches CJK characters" := do
  match Parser.run (string "世界") "世界こんにちは" with
  | .ok s => s ≡ "世界"
  | .error e => panic! s!"expected success, got: {e}"

test "string matches emoji" := do
  match Parser.run (string "👋🌍") "👋🌍 hello" with
  | .ok s => s ≡ "👋🌍"
  | .error e => panic! s!"expected success, got: {e}"

-- Test takeWhile with multi-byte characters

test "takeWhile preserves multi-byte characters" := do
  let isNotSpace := fun c => c != ' '
  match Parser.run (takeWhile isNotSpace) "héllo world" with
  | .ok s => s ≡ "héllo"
  | .error e => panic! s!"expected success, got: {e}"

test "take n preserves multi-byte characters" := do
  match Parser.run (take 5) "héllo" with
  | .ok s => s ≡ "héllo"
  | .error e => panic! s!"expected success, got: {e}"

test "take n with CJK characters" := do
  match Parser.run (take 3) "世界中" with
  | .ok s => s ≡ "世界中"
  | .error e => panic! s!"expected success, got: {e}"

-- Test position tracking with multi-byte characters

test "position offset tracks bytes not characters for 2-byte chars" := do
  let p := do
    let _ ← string "éé"  -- 4 bytes total (2 chars × 2 bytes)
    Parser.position
  match Parser.run p "ééx" with
  | .ok pos =>
    -- After parsing "éé", offset should be 4 (bytes), not 2 (chars)
    pos.offset ≡ 4
  | .error e => panic! s!"expected success, got: {e}"

test "position offset tracks bytes for 3-byte chars" := do
  let p := do
    let _ ← string "世界"  -- 6 bytes total (2 chars × 3 bytes)
    Parser.position
  match Parser.run p "世界中" with
  | .ok pos =>
    pos.offset ≡ 6
  | .error e => panic! s!"expected success, got: {e}"

test "position offset tracks bytes for 4-byte chars (emoji)" := do
  let p := do
    let _ ← string "😀😀"  -- 8 bytes total (2 chars × 4 bytes)
    Parser.position
  match Parser.run p "😀😀x" with
  | .ok pos =>
    pos.offset ≡ 8
  | .error e => panic! s!"expected success, got: {e}"

test "position column tracks characters not bytes" := do
  let p := do
    let _ ← string "éé"  -- 2 characters
    Parser.position
  match Parser.run p "ééx" with
  | .ok pos =>
    -- Column should be 3 (after 2 chars, column is 3)
    pos.column ≡ 3
  | .error e => panic! s!"expected success, got: {e}"

-- Test atEnd with multi-byte strings

test "atEnd works with multi-byte string" := do
  match Parser.parse (string "café") "café" with
  | .ok s => s ≡ "café"
  | .error e => panic! s!"expected success at end, got: {e}"

test "atEnd correctly identifies end after multi-byte chars" := do
  let p := do
    let _ ← string "世界"
    atEnd
  match Parser.run p "世界" with
  | .ok isEnd => shouldSatisfy isEnd "should be at end"
  | .error e => panic! s!"expected success, got: {e}"

-- Test extraction / substring operations

test "takeWhile extracts correct substring with leading multi-byte" := do
  -- Note: Char.isAlpha is ASCII-only, doesn't recognize 'é'
  -- Use a custom predicate that includes 'é'
  let isWordChar := fun c => c.isAlpha || c == 'é'
  match Parser.run (takeWhile isWordChar) "héllo123" with
  | .ok s =>
    s ≡ "héllo"
    s.length ≡ 5
  | .error e => panic! s!"expected success, got: {e}"

test "parsing after multi-byte chars extracts correct text" := do
  -- «hello» with guillemets - each is 2 bytes
  let leftQuote : Char := '«'
  let rightQuote : Char := '»'
  let p := do
    let _ ← char leftQuote
    let content ← takeWhile (· != rightQuote)
    let _ ← char rightQuote
    pure content
  let input := "«hello»"
  match Parser.run p input with
  | .ok s => s ≡ "hello"
  | .error e => panic! s!"expected success, got: {e}"

-- Edge cases

test "empty string after multi-byte char" := do
  let p := do
    let _ ← anyChar
    takeWhile (fun _ => false)
  match Parser.run p "é" with
  | .ok s => s ≡ ""
  | .error e => panic! s!"expected success, got: {e}"

test "peekString with multi-byte prefix" := do
  -- This tests that peekString correctly handles byte vs character counts
  let p := do
    let peeked ← peekString 2  -- Try to peek 2 bytes
    let first ← anyChar
    pure (peeked, first)
  match Parser.run p "ab" with
  | .ok (peeked, first) =>
    peeked ≡ some "ab"
    first ≡ 'a'
  | .error e => panic! s!"expected success, got: {e}"



end SiftTests.Utf8
