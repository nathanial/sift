# Sift Roadmap

Parser combinator library for Lean 4 - Improvement opportunities and feature proposals.

## Code Cleanup

### [Priority: High] Fix Deprecated API Usage

**Issue:** The codebase uses deprecated Lean 4 APIs that generate compiler warnings:
- `String.get` is deprecated in favor of `String.Pos.Raw.get`
- `String.mk` is deprecated in favor of `String.ofList`

**Location:**
- `Sift/Core.lean`: lines 46, 52
- `Sift/Primitives.lean`: lines 44, 55
- `Sift/Text.lean`: lines 17, 22, 113

**Action Required:**
1. Replace `s.input.get <pos>` with proper String position API
2. Replace `String.mk chars.toList` with `String.ofList chars.toList`

**Estimated Effort:** Small

### [Priority: Medium] Eliminate Unnecessary `partial` Annotations

**Issue:** Several functions are marked `partial` but may not require it with proper termination proofs. While recursion in `many`, `sepBy`, etc. is inherently unbounded based on input, some inner `go` functions could potentially use structural recursion.

**Location:**
- `Sift/Primitives.lean`: `string`, `take`, `takeWhile`, `takeWhile1`, `skipWhile`, `skipWhile1`
- `Sift/Combinators.lean`: `many`, `skipMany`, `manyTill`, `sepBy1`, `sepBy`, `endBy`, `sepEndBy1`, `sepEndBy`, `chainl1`, `chainl`, `chainr1`, `chainr`

**Action Required:** Evaluate whether termination proofs can be provided for inner recursive helpers, reducing reliance on `partial`.

**Estimated Effort:** Medium

### [Priority: Low] Consolidate Duplicate Escape Sequence Handling

**Issue:** The `stringLiteral` and `charLiteral` parsers in `Sift/Text.lean` both define identical `escapeChar` helper functions.

**Location:** `Sift/Text.lean`: lines 117-123 and 132-138

**Action Required:** Extract a shared `escapeChar` parser at the module level to avoid duplication.

**Estimated Effort:** Small

---

## Code Improvements

### [Priority: High] Unicode-Aware String Position Handling

**Current State:** The parser uses byte offsets (`Nat`) for position tracking, with direct indexing via deprecated `String.get`. Lean strings are UTF-8 encoded, meaning byte offsets may not correctly handle multi-byte characters.

**Proposed Change:** Use Lean's `String.Pos` type throughout for position tracking. Update `ParseState` to use proper `String.Pos` instead of `Nat` for the position field.

**Benefits:**
- Correct handling of UTF-8 multi-byte characters
- Future-proof against Lean standard library changes
- Eliminates deprecation warnings

**Affected Files:** `Sift/Core.lean`, `Sift/Primitives.lean`

**Estimated Effort:** Medium

### [Priority: High] Improve Error Message Quality

**Current State:** Error messages include position and a message, but `expected` items are sometimes incomplete. The `merge` function for combining errors at the same position could lose important context.

**Proposed Change:**
1. Ensure all primitive parsers consistently set `expected` field
2. Improve error merging to preserve the most specific error information
3. Add structured error variants (unexpected char, unexpected eof, etc.) instead of string-only messages

**Benefits:**
- Better debugging experience
- Clearer error messages for end users
- More machine-parseable error information

**Affected Files:** `Sift/Core.lean`, `Sift/Primitives.lean`

**Estimated Effort:** Medium

### [Priority: Medium] Add `MonadState` Instance

**Current State:** Parser state access is via custom `get`, `set`, `modify` functions in the Parser namespace.

**Proposed Change:** Add `MonadState ParseState Parser` instance for compatibility with generic state-manipulating code.

**Benefits:**
- Better integration with standard library patterns
- Enables use of generic state combinators

**Affected Files:** `Sift/Core.lean`

**Estimated Effort:** Small

### [Priority: Medium] Performance: Avoid Intermediate Arrays in `many`

**Current State:** The `many` combinator accumulates results into an `Array` by pushing elements one at a time in a loop.

**Proposed Change:** Consider alternative collection strategies or bulk allocation hints for very long sequences.

**Benefits:**
- Reduced memory allocation overhead for large inputs
- Better cache locality

**Affected Files:** `Sift/Combinators.lean`

**Estimated Effort:** Medium

### [Priority: Low] Add `Inhabited` Instance for `Parser`

**Current State:** Parser has an `Inhabited` instance that returns a failing parser.

**Proposed Change:** This is already done. However, consider if `Inhabited` should instead be `Default` for semantic clarity (the default parser fails, which may be unexpected).

**Affected Files:** `Sift/Core.lean`

**Estimated Effort:** Small

---

## Feature Proposals

### [Priority: High] Floating-Point Number Parser

**Description:** Add parsers for floating-point numbers (scientific notation, decimals).

**Rationale:** Floating-point numbers are extremely common in real-world parsing tasks (JSON, configuration files, data formats). The library currently only supports integers.

**Proposed API:**
```lean
def float : Parser Float        -- Parse decimal or scientific notation
def decimal : Parser Float      -- Parse decimal number only
def scientific : Parser Float   -- Parse scientific notation (1.5e10)
```

**Affected Files:** `Sift/Text.lean`

**Estimated Effort:** Medium

**Dependencies:** None

### [Priority: High] Streaming/Lazy Parsing Support

**Description:** Add support for parsing input incrementally or lazily, rather than requiring the entire input string upfront.

**Rationale:** Parsing large files or network streams requires the ability to process input incrementally. Current design requires loading entire input into a `String`.

**Proposed API:**
```lean
structure StreamState where
  buffer : String
  bufferPos : Nat
  isComplete : Bool

def Parser.runStreaming : Parser α → (Unit → IO (Option String)) → IO (Except ParseError α)
```

**Affected Files:** `Sift/Core.lean`, new `Sift/Streaming.lean`

**Estimated Effort:** Large

**Dependencies:** None

### [Priority: High] Context-Sensitive Parsing (Reader/State)

**Description:** Add support for user-defined state that persists across parser invocations, enabling context-sensitive parsing.

**Rationale:** Many real parsers need to track context (indentation level, symbol tables, configuration). Currently users must thread state manually.

**Proposed API:**
```lean
def ParserT (σ : Type) (α : Type) := σ → ParseState → Except ParseError (α × σ × ParseState)
-- Or transformer-style:
def ParserT (m : Type → Type) (α : Type) := ParseState → m (Except ParseError (α × ParseState))
```

**Affected Files:** New `Sift/ParserT.lean`

**Estimated Effort:** Large

**Dependencies:** None

### [Priority: Medium] Indentation-Sensitive Parsing

**Description:** Add combinators for indentation-sensitive languages (Python, YAML, Haskell-style layout).

**Rationale:** Indentation-sensitive parsing is a common requirement but requires careful handling of whitespace and column tracking.

**Proposed API:**
```lean
def indented (p : Parser α) : Parser α          -- Parse if indented relative to reference
def block (p : Parser α) : Parser (Array α)     -- Parse indentation-delimited block
def sameLine (p : Parser α) : Parser α          -- Parse on same line
def checkIndent : Parser Unit                   -- Verify current indentation
def withIndent (n : Nat) (p : Parser α) : Parser α
```

**Affected Files:** New `Sift/Indentation.lean`

**Estimated Effort:** Medium

**Dependencies:** Context-sensitive parsing support

### [Priority: Medium] Expression Parser Builder

**Description:** Add a configurable expression parser that handles operator precedence and associativity automatically.

**Rationale:** The existing `chainl1`/`chainr1` combinators work but require manual precedence handling. A table-driven approach is more ergonomic for complex expression grammars.

**Proposed API:**
```lean
inductive Assoc | left | right | none

structure Operator (α : Type) where
  parser : Parser (α → α → α)
  assoc : Assoc
  precedence : Nat

def buildExprParser (operators : Array (Array (Operator α))) (term : Parser α) : Parser α
```

**Affected Files:** New `Sift/Expression.lean`

**Estimated Effort:** Medium

**Dependencies:** None

### [Priority: Medium] Regex Integration

**Description:** Add a combinator that matches against a compiled regex from the workspace's `rune` library.

**Rationale:** Some parsing tasks are more naturally expressed as regex patterns. Integration with the workspace's existing regex library would provide a powerful hybrid approach.

**Proposed API:**
```lean
def regex (pattern : Rune.Regex) : Parser String
def regexCaptures (pattern : Rune.Regex) : Parser (Array String)
```

**Affected Files:** New `Sift/Regex.lean`, `lakefile.lean` (add dependency)

**Estimated Effort:** Medium

**Dependencies:** `rune` library

### [Priority: Medium] Permutation Parser

**Description:** Parse elements in any order (each exactly once).

**Rationale:** Common in configuration parsing where keys can appear in any order. Currently requires complex manual implementation.

**Proposed API:**
```lean
def permute2 (p1 : Parser α) (p2 : Parser β) : Parser (α × β)
def permute3 (p1 : Parser α) (p2 : Parser β) (p3 : Parser γ) : Parser (α × β × γ)
-- Or with heterogeneous list
def permutation (parsers : List (Parser ())) : Parser Unit  -- simplified
```

**Affected Files:** New `Sift/Permutation.lean` or `Sift/Combinators.lean`

**Estimated Effort:** Medium

**Dependencies:** None

### [Priority: Medium] ByteArray/Binary Parsing

**Description:** Add support for parsing binary data from `ByteArray` instead of `String`.

**Rationale:** Many formats are binary (images, protocols, archives). The current string-based parser cannot handle null bytes or binary data efficiently.

**Proposed API:**
```lean
def BinaryParser (α : Type) := ByteArray → Nat → Except ParseError (α × Nat)

namespace BinaryParser
  def byte : BinaryParser UInt8
  def bytes (n : Nat) : BinaryParser ByteArray
  def uint16LE : BinaryParser UInt16
  def uint32BE : BinaryParser UInt32
  -- etc.
end BinaryParser
```

**Affected Files:** New `Sift/Binary.lean`

**Estimated Effort:** Large

**Dependencies:** None

### [Priority: Medium] Debug/Trace Combinators

**Description:** Add combinators for debugging parsers during development.

**Rationale:** Parser debugging is notoriously difficult. Tracing input consumption and parser decisions would significantly improve development experience.

**Proposed API:**
```lean
def trace (name : String) (p : Parser α) : Parser α
def traceState (msg : String) : Parser Unit
def traceOnFail (p : Parser α) : Parser α
```

**Affected Files:** `Sift/Combinators.lean` or new `Sift/Debug.lean`

**Estimated Effort:** Small

**Dependencies:** None

### [Priority: Low] Case-Insensitive String Matching

**Description:** Add case-insensitive variants of string matching primitives.

**Rationale:** Many text formats (HTML tags, SQL keywords) are case-insensitive.

**Proposed API:**
```lean
def stringCI (expected : String) : Parser String
def charCI (c : Char) : Parser Char
```

**Affected Files:** `Sift/Primitives.lean`

**Estimated Effort:** Small

**Dependencies:** None

### [Priority: Low] Recovery/Error Correction

**Description:** Add combinators for error recovery to enable parsing to continue after errors.

**Rationale:** For IDE integration and better user experience, parsers should be able to recover from errors and continue parsing to report multiple issues.

**Proposed API:**
```lean
def recover (p : Parser α) (handler : ParseError → Parser α) : Parser α
def withRecovery (p : Parser α) (default : α) (skipUntil : Parser Unit) : Parser α
```

**Affected Files:** `Sift/Combinators.lean`

**Estimated Effort:** Medium

**Dependencies:** None

### [Priority: Low] Substring Parsing

**Description:** Add ability to parse a `Substring` directly without copying to a new `String`.

**Rationale:** Parsing portions of larger strings without allocation improves performance.

**Proposed API:**
```lean
def Parser.runSubstring (p : Parser α) (input : Substring) : Except ParseError α
```

**Affected Files:** `Sift/Core.lean`

**Estimated Effort:** Small

**Dependencies:** None

### [Priority: Low] JSON Number Parser

**Description:** Add a parser specifically for JSON-compliant numbers.

**Rationale:** JSON has specific rules for numbers (no leading zeros except 0.x, no +prefix, etc.). A dedicated parser ensures compliance.

**Affected Files:** `Sift/Text.lean`

**Estimated Effort:** Small

**Dependencies:** Floating-point parser

---

## Test Coverage Improvements

### [Priority: Medium] Add Edge Case Tests

**Issue:** Current tests cover happy paths well but could benefit from more edge cases.

**Missing Test Cases:**
- Empty input for all primitives
- Very long inputs (stress testing)
- Unicode characters in input
- Deeply nested structures with `many`
- Error message content verification
- Position tracking with multi-byte UTF-8 characters
- `endBy`, `endBy1` combinators (no dedicated tests)
- `sepEndBy`, `sepEndBy1` combinators (no dedicated tests)
- `skipMany`, `skipMany1` combinators (no dedicated tests)
- `withFilter` combinator (no dedicated tests)

**Affected Files:** `SiftTests/*.lean`

**Estimated Effort:** Medium

### [Priority: Low] Add Performance Benchmarks

**Issue:** No performance benchmarks exist to catch regressions or measure improvements.

**Action Required:**
1. Create benchmark suite for common operations
2. Benchmark `many` with different input sizes
3. Compare with manual parsing for reference
4. Add CI integration for performance tracking

**Affected Files:** New `SiftBench/` directory

**Estimated Effort:** Medium

---

## Documentation Improvements

### [Priority: Medium] Add Module-Level Documentation

**Issue:** While the README is comprehensive, individual modules lack detailed doc comments explaining design decisions and usage patterns.

**Action Required:**
1. Add module documentation to each `.lean` file
2. Document the backtracking behavior in detail
3. Add examples for each combinator in doc comments

**Affected Files:** All `Sift/*.lean` files

**Estimated Effort:** Medium

### [Priority: Low] Create Tutorial/Guide

**Issue:** Users new to parser combinators need a tutorial explaining concepts, not just API reference.

**Action Required:**
1. Write a progressive tutorial starting with simple parsers
2. Include a complete worked example (e.g., JSON parser)
3. Explain common patterns and pitfalls

**Affected Files:** New `docs/` directory or expanded README

**Estimated Effort:** Medium

---

## Integration Opportunities

### [Priority: Medium] Potential Adoption by Workspace Projects

Several workspace projects implement custom parsers that could potentially use Sift:

1. **totem** (TOML parser) - Has own parser primitives (`Totem/Parser/Primitives.lean`)
2. **markup** (HTML parser) - Has own error types and position tracking
3. **herald** (HTTP parser) - Could use Sift for header parsing
4. **rune** (regex) - Regex pattern parser could use Sift

**Benefits of Adoption:**
- Consistent error handling across workspace
- Reduced code duplication
- Shared bug fixes and improvements

**Action Required:**
1. Evaluate API compatibility with existing parsers
2. Document migration path
3. Consider backwards-compatible adapter layer

**Estimated Effort:** Large (per project)

---

## Summary by Priority

### High Priority
- Fix deprecated API usage (cleanup)
- Unicode-aware string position handling (improvement)
- Improve error message quality (improvement)
- Floating-point number parser (feature)
- Streaming/lazy parsing support (feature)
- Context-sensitive parsing (feature)

### Medium Priority
- Eliminate unnecessary `partial` annotations (cleanup)
- Add `MonadState` instance (improvement)
- Performance improvements for `many` (improvement)
- Indentation-sensitive parsing (feature)
- Expression parser builder (feature)
- Regex integration (feature)
- Permutation parser (feature)
- ByteArray/binary parsing (feature)
- Debug/trace combinators (feature)
- Add edge case tests (testing)
- Add module-level documentation (docs)
- Workspace project adoption (integration)

### Low Priority
- Consolidate duplicate escape handling (cleanup)
- Case-insensitive string matching (feature)
- Recovery/error correction (feature)
- Substring parsing (feature)
- JSON number parser (feature)
- Add performance benchmarks (testing)
- Create tutorial/guide (docs)
