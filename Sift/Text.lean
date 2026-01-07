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
def manyChars (p : Parser Char) : Parser String := do
  let chars ← many p
  pure (String.mk chars.toList)

/-- Parse one or more chars into a string -/
def many1Chars (p : Parser Char) : Parser String := do
  let chars ← many1 p
  pure (String.mk chars.toList)

/-- Parse a natural number (unsigned integer) -/
def natural : Parser Nat := do
  let digits ← many1 digit
  let n := digits.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  pure n

/-- Parse an integer (with optional leading sign) -/
def integer : Parser Int := do
  let sign ← optional (char '-' <|> char '+')
  let n ← natural
  match sign with
  | some '-' => pure (-(n : Int))
  | _ => pure (n : Int)

/-- Parse a hex number (without 0x prefix) -/
def hexNatural : Parser Nat := do
  let digits ← many1 hexDigit
  let n := digits.foldl (fun acc c =>
    let v := if c >= '0' && c <= '9' then c.toNat - '0'.toNat
             else if c >= 'a' && c <= 'f' then c.toNat - 'a'.toNat + 10
             else c.toNat - 'A'.toNat + 10
    acc * 16 + v) 0
  pure n

/-- Parse a binary number (without 0b prefix) -/
def binNatural : Parser Nat := do
  let digits ← many1 binDigit
  let n := digits.foldl (fun acc c => acc * 2 + (c.toNat - '0'.toNat)) 0
  pure n

/-- Parse an octal number (without 0o prefix) -/
def octNatural : Parser Nat := do
  let digits ← many1 octDigit
  let n := digits.foldl (fun acc c => acc * 8 + (c.toNat - '0'.toNat)) 0
  pure n

/-- Parse an identifier (letter followed by alphanums or underscore) -/
def identifier : Parser String := do
  let first ← letter <|> char '_'
  let rest ← manyChars (alphaNum <|> char '_')
  pure (String.singleton first ++ rest)

/-- Parse a lexeme (parser followed by whitespace skip) -/
def lexeme {α : Type} (p : Parser α) : Parser α := do
  let result ← p
  spaces
  pure result

/-- Parse a symbol (string followed by whitespace skip) -/
def symbol (s : String) : Parser String :=
  lexeme (string s)

/-- Parse content between parentheses -/
def parens {α : Type} (p : Parser α) : Parser α :=
  between (symbol "(") (symbol ")") p

/-- Parse content between braces -/
def braces {α : Type} (p : Parser α) : Parser α :=
  between (symbol "{") (symbol "}") p

/-- Parse content between brackets -/
def brackets {α : Type} (p : Parser α) : Parser α :=
  between (symbol "[") (symbol "]") p

/-- Parse content between angle brackets -/
def angles {α : Type} (p : Parser α) : Parser α :=
  between (symbol "<") (symbol ">") p

/-- Parse a comma-separated list -/
def commaSep {α : Type} (p : Parser α) : Parser (Array α) :=
  sepBy p (symbol ",")

/-- Parse a comma-separated list with at least one element -/
def commaSep1 {α : Type} (p : Parser α) : Parser (Array α) :=
  sepBy1 p (symbol ",")

/-- Parse a semicolon-separated list -/
def semiSep {α : Type} (p : Parser α) : Parser (Array α) :=
  sepBy p (symbol ";")

/-- Parse a semicolon-separated list with at least one element -/
def semiSep1 {α : Type} (p : Parser α) : Parser (Array α) :=
  sepBy1 p (symbol ";")

/-- Parse a double-quoted string literal with escape sequences -/
def stringLiteral : Parser String := do
  let _ ← char '"'
  let chars ← manyTill charContent (char '"')
  let _ ← char '"'
  pure (String.mk chars.toList)
where
  charContent : Parser Char :=
    (char '\\' *> escapeChar) <|> satisfy (· != '"')
  escapeChar : Parser Char :=
    (char 'n' *> pure '\n') <|>
    (char 'r' *> pure '\r') <|>
    (char 't' *> pure '\t') <|>
    (char '\\' *> pure '\\') <|>
    (char '"' *> pure '"') <|>
    (char '0' *> pure '\x00')

/-- Parse a single-quoted character literal -/
def charLiteral : Parser Char := do
  let _ ← char '\''
  let c ← (char '\\' *> escapeChar) <|> satisfy (· != '\'')
  let _ ← char '\''
  pure c
where
  escapeChar : Parser Char :=
    (char 'n' *> pure '\n') <|>
    (char 'r' *> pure '\r') <|>
    (char 't' *> pure '\t') <|>
    (char '\\' *> pure '\\') <|>
    (char '\'' *> pure '\'') <|>
    (char '0' *> pure '\x00')

end Sift
