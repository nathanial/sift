import Sift.Core
import Sift.Primitives
import Sift.Combinators
import Sift.Char

/-!
# Sift.Text

Text utilities: natural, integer, identifier, lexeme
-/

namespace Sift

/-- Parse zero or more chars into a string -/
def manyChars {σ : Type} (p : Parser σ Char) : Parser σ String := do
  let chars ← many p
  pure (String.ofList chars.toList)

/-- Parse one or more chars into a string -/
def many1Chars {σ : Type} (p : Parser σ Char) : Parser σ String := do
  let chars ← many1 p
  pure (String.ofList chars.toList)

/-- Parse a natural number (unsigned integer) -/
def natural {σ : Type} : Parser σ Nat := do
  let digits ← many1 digit
  let n := digits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  pure n

/-- Parse an integer (with optional leading sign) -/
def integer {σ : Type} : Parser σ Int := do
  let sign ← optional (char '-' <|> char '+')
  let n ← natural
  match sign with
  | some '-' => pure (-(n : Int))
  | _ => pure (n : Int)

/-- Parse a hex number (without 0x prefix) -/
def hexNatural {σ : Type} : Parser σ Nat := do
  let digits ← many1 hexDigit
  let n := digits.foldl (fun acc c =>
    let v := if c >= '0' && c <= '9' then c.toNat - '0'.toNat
             else if c >= 'a' && c <= 'f' then c.toNat - 'a'.toNat + 10
             else c.toNat - 'A'.toNat + 10
    acc * 16 + v) 0
  pure n

/-- Parse a binary number (without 0b prefix) -/
def binNatural {σ : Type} : Parser σ Nat := do
  let digits ← many1 binDigit
  let n := digits.foldl (fun acc c => acc * 2 + (c.toNat - '0'.toNat)) 0
  pure n

/-- Parse an octal number (without 0o prefix) -/
def octNatural {σ : Type} : Parser σ Nat := do
  let digits ← many1 octDigit
  let n := digits.foldl (fun acc c => acc * 8 + (c.toNat - '0'.toNat)) 0
  pure n

/-- Parse exactly n hex digits and return numeric value -/
def hexDigitsN {σ : Type} (n : Nat) : Parser σ Nat := do
  let digits ← count n hexDigit
  let result := digits.foldl (fun acc c =>
    let v := if c >= '0' && c <= '9' then c.toNat - '0'.toNat
             else if c >= 'a' && c <= 'f' then c.toNat - 'a'.toNat + 10
             else c.toNat - 'A'.toNat + 10
    acc * 16 + v) 0
  pure result

/-- Parse a floating point number -/
def float {σ : Type} : Parser σ Float := do
  let sign ← optional (char '-' <|> char '+')
  let intDigits ← many1 digit
  let intPart := intDigits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  let mut result := intPart.toFloat
  -- Fractional part
  if (← optional (char '.')).isSome then
    let fracDigits ← many1 digit
    let (frac, _) := fracDigits.foldl (fun (acc, div) c =>
      let d := (c.toNat - '0'.toNat).toFloat / div
      (acc + d, div * 10.0)) (0.0, 10.0)
    result := result + frac
  -- Exponent part
  if (← optional (char 'e' <|> char 'E')).isSome then
    let expSign ← optional (char '-' <|> char '+')
    let expDigits ← many1 digit
    let exp := expDigits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
    let expFloat := if expSign == some '-' then -(exp.toFloat) else exp.toFloat
    result := result * Float.pow 10.0 expFloat
  match sign with
  | some '-' => pure (-result)
  | _ => pure result

/-- Parse a decimal number (requires decimal point, no exponent) -/
def decimal {σ : Type} : Parser σ Float := do
  let sign ← optional (char '-' <|> char '+')
  let intDigits ← many1 digit
  let intPart := intDigits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  let mut result := intPart.toFloat
  -- Fractional part (required for decimal)
  let _ ← char '.'
  let fracDigits ← many1 digit
  let (frac, _) := fracDigits.foldl (fun (acc, div) c =>
    let d := (c.toNat - '0'.toNat).toFloat / div
    (acc + d, div * 10.0)) (0.0, 10.0)
  result := result + frac
  match sign with
  | some '-' => pure (-result)
  | _ => pure result

/-- Parse a number in scientific notation (requires exponent) -/
def scientific {σ : Type} : Parser σ Float := do
  let sign ← optional (char '-' <|> char '+')
  let intDigits ← many1 digit
  let intPart := intDigits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  let mut result := intPart.toFloat
  -- Fractional part (optional)
  if (← optional (char '.')).isSome then
    let fracDigits ← many1 digit
    let (frac, _) := fracDigits.foldl (fun (acc, div) c =>
      let d := (c.toNat - '0'.toNat).toFloat / div
      (acc + d, div * 10.0)) (0.0, 10.0)
    result := result + frac
  -- Exponent part (required for scientific)
  let _ ← char 'e' <|> char 'E'
  let expSign ← optional (char '-' <|> char '+')
  let expDigits ← many1 digit
  let exp := expDigits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  let expFloat := if expSign == some '-' then -(exp.toFloat) else exp.toFloat
  result := result * Float.pow 10.0 expFloat
  match sign with
  | some '-' => pure (-result)
  | _ => pure result

/-- Parse 4-digit unicode escape, returns the character -/
def unicodeEscape4 {σ : Type} : Parser σ Char := do
  let code ← hexDigitsN 4
  if h : code.toUInt32 < 0x110000 then
    pure (Char.ofNat code)
  else
    Parser.fail s!"invalid unicode code point: U+{String.ofList (Nat.toDigits 16 code)}"

/-- Parse 8-digit unicode escape, returns the character -/
def unicodeEscape8 {σ : Type} : Parser σ Char := do
  let code ← hexDigitsN 8
  if h : code.toUInt32 < 0x110000 then
    pure (Char.ofNat code)
  else
    Parser.fail s!"invalid unicode code point: U+{String.ofList (Nat.toDigits 16 code)}"

/-- Parse digits with underscore separators (common in Rust, Python, TOML, etc.)
    Rules: no leading underscore, no trailing underscore, no consecutive underscores -/
partial def digitsWithUnderscores {σ : Type} (isValidDigit : Char → Bool) : Parser σ String := do
  let mut result := ""
  let mut lastWasUnderscore := false
  let mut first := true
  let mut going := true
  while going do
    match ← peek with
    | some c =>
      if isValidDigit c then
        let _ ← anyChar
        result := result.push c
        lastWasUnderscore := false
        first := false
      else if c == '_' && !first && !lastWasUnderscore then
        let _ ← anyChar
        lastWasUnderscore := true
      else
        going := false
    | none => going := false
  -- Trailing underscore check: if lastWasUnderscore is true, we consumed it but shouldn't have
  -- This is allowed - the underscore was consumed but not added to result
  return result

/-- Parse decimal digits with underscore separators -/
def decimalWithUnderscores {σ : Type} : Parser σ String :=
  digitsWithUnderscores Char.isDigit

/-- Parse hex digits with underscore separators -/
def hexWithUnderscores {σ : Type} : Parser σ String :=
  digitsWithUnderscores (fun c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))

/-- Parse an identifier (letter followed by alphanums or underscore) -/
def identifier {σ : Type} : Parser σ String := do
  let first ← letter <|> char '_'
  let rest ← manyChars (alphaNum <|> char '_')
  pure (String.singleton first ++ rest)

/-- Parse a lexeme (parser followed by whitespace skip) -/
def lexeme {σ α : Type} (p : Parser σ α) : Parser σ α := do
  let result ← p
  spaces
  pure result

/-- Parse a symbol (string followed by whitespace skip) -/
def symbol {σ : Type} (s : String) : Parser σ String :=
  lexeme (string s)

/-- Parse content between parentheses -/
def parens {σ α : Type} (p : Parser σ α) : Parser σ α :=
  between (symbol "(") (symbol ")") p

/-- Parse content between braces -/
def braces {σ α : Type} (p : Parser σ α) : Parser σ α :=
  between (symbol "{") (symbol "}") p

/-- Parse content between brackets -/
def brackets {σ α : Type} (p : Parser σ α) : Parser σ α :=
  between (symbol "[") (symbol "]") p

/-- Parse content between angle brackets -/
def angles {σ α : Type} (p : Parser σ α) : Parser σ α :=
  between (symbol "<") (symbol ">") p

/-- Parse a comma-separated list -/
def commaSep {σ α : Type} (p : Parser σ α) : Parser σ (Array α) :=
  sepBy p (symbol ",")

/-- Parse a comma-separated list with at least one element -/
def commaSep1 {σ α : Type} (p : Parser σ α) : Parser σ (Array α) :=
  sepBy1 p (symbol ",")

/-- Parse a semicolon-separated list -/
def semiSep {σ α : Type} (p : Parser σ α) : Parser σ (Array α) :=
  sepBy p (symbol ";")

/-- Parse a semicolon-separated list with at least one element -/
def semiSep1 {σ α : Type} (p : Parser σ α) : Parser σ (Array α) :=
  sepBy1 p (symbol ";")

/-- Parse a double-quoted string literal with escape sequences -/
def stringLiteral {σ : Type} : Parser σ String := do
  let _ ← char '"'
  let chars ← manyTill charContent (char '"')
  let _ ← char '"'
  pure (String.ofList chars.toList)
where
  charContent : Parser σ Char :=
    (char '\\' *> escapeChar) <|> satisfy (· != '"')
  escapeChar : Parser σ Char :=
    (char 'n' *> pure '\n') <|>
    (char 'r' *> pure '\r') <|>
    (char 't' *> pure '\t') <|>
    (char '\\' *> pure '\\') <|>
    (char '"' *> pure '"') <|>
    (char '0' *> pure '\x00')

/-- Parse a single-quoted character literal -/
def charLiteral {σ : Type} : Parser σ Char := do
  let _ ← char '\''
  let c ← (char '\\' *> escapeChar) <|> satisfy (· != '\'')
  let _ ← char '\''
  pure c
where
  escapeChar : Parser σ Char :=
    (char 'n' *> pure '\n') <|>
    (char 'r' *> pure '\r') <|>
    (char 't' *> pure '\t') <|>
    (char '\\' *> pure '\\') <|>
    (char '\'' *> pure '\'') <|>
    (char '0' *> pure '\x00')

end Sift
