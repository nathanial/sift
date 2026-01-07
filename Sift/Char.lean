import Sift.Core
import Sift.Primitives
import Sift.Combinators

/-!
# Sift.Char

Character class parsers: digit, letter, space, oneOf, etc.
-/

namespace Sift

/-- Match a digit [0-9] -/
def digit : Parser Char :=
  satisfy Char.isDigit <?> "digit"

/-- Match a letter [a-zA-Z] -/
def letter : Parser Char :=
  satisfy Char.isAlpha <?> "letter"

/-- Match alphanumeric [a-zA-Z0-9] -/
def alphaNum : Parser Char :=
  satisfy Char.isAlphanum <?> "alphanumeric"

/-- Match lowercase [a-z] -/
def lower : Parser Char :=
  satisfy (fun c => c >= 'a' && c <= 'z') <?> "lowercase letter"

/-- Match uppercase [A-Z] -/
def upper : Parser Char :=
  satisfy (fun c => c >= 'A' && c <= 'Z') <?> "uppercase letter"

/-- Match a single space ' ' -/
def space : Parser Char :=
  char ' ' <?> "space"

/-- Match any whitespace character -/
def whitespace : Parser Char :=
  satisfy Char.isWhitespace <?> "whitespace"

/-- Skip zero or more whitespace characters -/
def spaces : Parser Unit :=
  skipWhile Char.isWhitespace

/-- Skip one or more whitespace characters -/
def spaces1 : Parser Unit :=
  skipWhile1 Char.isWhitespace

/-- Match horizontal whitespace (space or tab) -/
def hspace : Parser Char :=
  satisfy (fun c => c == ' ' || c == '\t') <?> "horizontal whitespace"

/-- Skip zero or more horizontal whitespace characters (space/tab only, not newlines) -/
def hspaces : Parser Unit :=
  skipWhile (fun c => c == ' ' || c == '\t')

/-- Skip one or more horizontal whitespace characters -/
def hspaces1 : Parser Unit :=
  skipWhile1 (fun c => c == ' ' || c == '\t')

/-- Match newline -/
def newline : Parser Char :=
  char '\n' <?> "newline"

/-- Match carriage return -/
def cr : Parser Char :=
  char '\r' <?> "carriage return"

/-- Match CRLF or LF -/
def eol : Parser Unit :=
  (char '\r' *> char '\n' *> pure ()) <|> (char '\n' *> pure ()) <?> "end of line"

/-- Match tab -/
def tab : Parser Char :=
  char '\t' <?> "tab"

/-- Match any character in the string -/
def oneOf (chars : String) : Parser Char :=
  satisfy (fun c => chars.any (· == c)) <?> s!"one of \"{chars}\""

/-- Match any character not in the string -/
def noneOf (chars : String) : Parser Char :=
  satisfy (fun c => !chars.any (· == c)) <?> s!"none of \"{chars}\""

/-- Match a hex digit [0-9a-fA-F] -/
def hexDigit : Parser Char :=
  satisfy (fun c =>
    (c >= '0' && c <= '9') ||
    (c >= 'a' && c <= 'f') ||
    (c >= 'A' && c <= 'F')) <?> "hex digit"

/-- Match an octal digit [0-7] -/
def octDigit : Parser Char :=
  satisfy (fun c => c >= '0' && c <= '7') <?> "octal digit"

/-- Match a binary digit [0-1] -/
def binDigit : Parser Char :=
  satisfy (fun c => c == '0' || c == '1') <?> "binary digit"

/-- Match an ASCII character -/
def ascii : Parser Char :=
  satisfy (fun c => c.toNat < 128) <?> "ASCII character"

/-- Match a printable ASCII character -/
def printable : Parser Char :=
  satisfy (fun c => c.toNat >= 32 && c.toNat < 127) <?> "printable character"

/-- Match a control character -/
def control : Parser Char :=
  satisfy (fun c => c.toNat < 32 || c.toNat == 127) <?> "control character"

end Sift
