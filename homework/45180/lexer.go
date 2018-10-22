package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
	"unicode"
)

const (
	// Single-character tokens.
	TokenLeftParen = iota
	TokenRightParen
	TokenLeftBrace
	TokenRightBrace
	TokenSemicolon
	TokenDot
	TokenComma
	TokenColon
	TokenQuestionmark
	TokenLeftSquareBracket
	TokenRightSquareBracket
	TokenMinus
	TokenPlus
	TokenStar
	TokenDivision
	TokenModulo
	TokenBitwiseNot
	TokenBitwiseAnd
	TokenBitwiseXor
	TokenBitwiseOr

	// One or two character tokens
	TokenBang
	TokenNotEqual
	TokenEqual
	TokenEqualEqual
	TokenStrictEqual
	TokenStrictNotEqual
	TokenGreater
	TokenGreaterEqual
	TokenLess
	TokenLessEqual
	TokenMinusMinus
	TokenPlusplus
	TokenLeftShift
	TokenRightShift
	TokenLogicalAnd
	TokenLogicalOr
	TokenStarEqual
	TokenDivideEqual
	TokenModuloEqual
	TokenPlusEqual
	TokenMinusEqual
	TokenLeftShiftEqual
	TokenRightShiftEqual
	TokenBitwiseAndEqual
	TokenBitwiseXorEqual
	TokenBitwiseOrEqual

	TokenString
	TokenNumber
	TokenKeyword
	TokenWhitespace
	TokenIdentifier

	TokenEOF
	TokenError
)

var keywords = map[string]struct{}{
	"break":      {},
	"case":       {},
	"catch":      {},
	"class":      {},
	"const":      {},
	"continue":   {},
	"debugger":   {},
	"default":    {},
	"delete":     {},
	"do":         {},
	"else":       {},
	"export":     {},
	"extends":    {},
	"finally":    {},
	"for":        {},
	"function":   {},
	"if":         {},
	"import":     {},
	"in":         {},
	"instanceof": {},
	"new":        {},
	"return":     {},
	"super":      {},
	"switch":     {},
	"this":       {},
	"throw":      {},
	"try":        {},
	"typeof":     {},
	"var":        {},
	"void":       {},
	"while":      {},
	"with":       {},
	"yield":      {},
	"null":       {},
	"undefined":  {},
	"true":       {},
	"false":      {},
}

var singleCharTokens = map[rune]struct{}{
	'(': {},
	')': {},
	'[': {},
	']': {},
	'{': {},
	'}': {},
	',': {},
	'.': {},
	';': {},
	'-': {},
	'+': {},
	'*': {},
	'/': {},
	'%': {},
	'~': {},
	'=': {},
	'!': {},
	'>': {},
	'<': {},
	'&': {},
	'|': {},
	'^': {},
	'?': {},
	':': {},
}

type Token struct {
	TokenType  int
	TokenValue string
	Err        error
}

type Tokenizer interface {
	Tokenize(reader io.Reader, tokens chan Token)
}

type tokenizerImpl struct {
	emitWhitespace bool
}

func NewTokenizer(emitWhitespace bool) Tokenizer {
	return tokenizerImpl{emitWhitespace}
}

func (tokenizer tokenizerImpl) Tokenize(in io.Reader, tokens chan Token) {
	reader := bufio.NewReader(in)
	defer close(tokens)
	for {
		var symbol rune
		var err error

		if token := newToken(reader, readWhitespace); tokenizer.emitWhitespace {
			tokens <- token
		}
		if symbol, err = peek(reader); err != nil {
			var tokenType int
			if err == io.EOF {
				tokenType = TokenEOF
			} else {
				tokenType = TokenError
			}
			tokens <- Token{tokenType, "", err}
			return
		}

		if isSingleCharToken(symbol) {
			tokens <- newToken(reader, readSingleCharToken)
			continue
		} else {
			reader.UnreadRune()
		}

		if unicode.IsDigit(symbol) {
			tokens <- newToken(reader, readNumber)
			continue
		}

		if symbol == '"' {
			tokens <- newToken(reader, readString)
			continue
		}

		tokens <- newToken(reader, readKeywordOrIdentifier)
	}
}

func newToken(reader *bufio.Reader, tokenGenerator func(reader *bufio.Reader) (int, string, error)) Token {
	if tokenType, tokenValue, err := tokenGenerator(reader); err != nil {
		if err == io.EOF {
			tokenType = TokenEOF
		} else {
			tokenType = TokenError
		}
		return Token{tokenType, "", err}
	} else {
		return Token{tokenType, tokenValue, nil}
	}
}

func peek(reader *bufio.Reader) (rune, error) {
	symbol, _, err := reader.ReadRune()
	reader.UnreadRune()
	return symbol, err
}

func isWhitespace(r rune) bool {
	return r == ' ' || r == '\t' || r == '\n'
}

func readWhitespace(reader *bufio.Reader) (int, string, error) {
	var symbol rune
	var err error
	var sb strings.Builder
	for {
		if symbol, _, err = reader.ReadRune(); err == nil && isWhitespace(symbol) {
			sb.WriteRune(symbol)
		} else {
			break
		}
	}
	reader.UnreadRune()
	return TokenWhitespace, sb.String(), err
}

func isSingleCharToken(symbol rune) bool {
	_, isPresent := singleCharTokens[symbol]
	return isPresent
}

func readSingleCharToken(reader *bufio.Reader) (int, string, error) {
	var token int
	symbol, _, err := reader.ReadRune()
	tokenValue := string(symbol)
	switch symbol {
	case '(':
		token = TokenLeftParen
		break
	case ')':
		token = TokenRightParen
		break
	case '[':
		token = TokenLeftSquareBracket
		break
	case ']':
		token = TokenRightSquareBracket
		break
	case '{':
		token = TokenLeftBrace
		break
	case '}':
		token = TokenRightBrace
		break
	case ',':
		token = TokenComma
		break
	case '.':
		token = TokenDot
		break
	case ';':
		token = TokenSemicolon
		break
	case '-':
		token = TokenMinus
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			token = TokenMinusEqual
			tokenValue = "-="
		} else if s == '-' {
			token = TokenMinusMinus
			tokenValue = "--"
		} else {
			reader.UnreadRune()
		}
		break
	case '+':
		token = TokenPlus
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			token = TokenPlusEqual
			tokenValue = "+="
		} else if s == '+' {
			token = TokenPlusplus
			tokenValue = "++"
		} else {
			reader.UnreadRune()
		}
		break
	case '*':
		token = TokenStar
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			token = TokenStarEqual
			tokenValue = "*="
		} else {
			reader.UnreadRune()
		}
		break
	case '/':
		token = TokenDivision
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			token = TokenDivideEqual
			tokenValue = "/="
		} else {
			reader.UnreadRune()
		}
		break
	case '%':
		token = TokenModulo
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			token = TokenModuloEqual
			tokenValue = "%="
		} else {
			reader.UnreadRune()
		}
		break
	case '~':
		token = TokenBitwiseNot
		break
	case '=':
		token = TokenEqual
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			if s, _, readErr := reader.ReadRune(); err != nil {
				err = readErr
			} else if s == '=' {
				token = TokenStrictEqual
				tokenValue = "==="
			} else {
				token = TokenEqualEqual
				reader.UnreadRune()
				tokenValue = "=="
			}
		} else {
			reader.UnreadRune()
		}
		break
	case '!':
		token = TokenBang
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			if s, _, readErr := reader.ReadRune(); err != nil {
				err = readErr
			} else if s == '=' {
				token = TokenStrictNotEqual
				tokenValue = "!==="
			} else {
				token = TokenNotEqual
				tokenValue = "!=="
				reader.UnreadRune()
			}
		} else {
			reader.UnreadRune()
		}
		break
	case '>':
		token = TokenGreater
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			token = TokenGreaterEqual
			tokenValue = ">="
		} else if s == '>' {
			if s, _, readErr := reader.ReadRune(); err != nil {
				err = readErr
			} else if s == '=' {
				token = TokenRightShiftEqual
				tokenValue = ">>="
			} else {
				reader.UnreadRune()
				token = TokenRightShift
				tokenValue = ">>"
			}
		} else {
			reader.UnreadRune()
		}
		break
	case '<':
		token = TokenLess
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			token = TokenLessEqual
		} else if s == '>' {
			if s, _, readErr := reader.ReadRune(); err != nil {
				err = readErr
			} else if s == '=' {
				token = TokenLeftShiftEqual
				tokenValue = "<<="
			} else {
				reader.UnreadRune()
				token = TokenLeftShift
				tokenValue = "<<"
			}
		} else {
			reader.UnreadRune()
		}
		break
	case '&':
		token = TokenBitwiseAnd
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			token = TokenBitwiseAndEqual
			tokenValue = "&="
		} else if s == '&' {
			token = TokenLogicalAnd
			tokenValue = "&&"
		} else {
			reader.UnreadRune()
		}
		break
	case '|':
		token = TokenBitwiseOr
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			token = TokenBitwiseOrEqual
			tokenValue = "|="
		} else if s == '|' {
			token = TokenLogicalOr
			tokenValue = "||"
		} else {
			reader.UnreadRune()
		}
		break
	case '^':
		token = TokenBitwiseXor
		if s, _, readErr := reader.ReadRune(); err != nil {
			err = readErr
		} else if s == '=' {
			token = TokenBitwiseXorEqual
			tokenValue = "^="
		} else {
			reader.UnreadRune()
		}
		break
	case '?':
		token = TokenQuestionmark
		break
	case ':':
		token = TokenColon
		break
	default:
		err = errors.New(fmt.Sprintf("Invalid token %s", symbol))
		token = TokenError
	}
	return token, tokenValue, err
}

func isKeyword(rawToken string) bool {
	_, isPresent := keywords[strings.ToLower(rawToken)]
	return isPresent
}

func isIdentifierOrKeywordSymbol(symbol rune) bool {
	return unicode.IsLetter(symbol) || unicode.IsDigit(symbol) || symbol == '_'
}

func readKeywordOrIdentifier(reader *bufio.Reader) (int, string, error) {
	var sb strings.Builder
	for {
		symbol, _, err := reader.ReadRune()
		if err != nil || isWhitespace(symbol) || !isIdentifierOrKeywordSymbol(symbol) {
			if err == nil {
				reader.UnreadRune()
				break
			}
			if err != io.EOF {
				return TokenError, "", err
			}
			if err == io.EOF {
				return TokenEOF, "", err
			}
		} else {
			sb.WriteRune(symbol)
		}
	}
	rawToken := sb.String()
	if isKeyword(rawToken) {
		return TokenKeyword, rawToken, nil
	} else {
		return TokenIdentifier, rawToken, nil
	}
}

func isNumberSymbol(symbol rune) bool {
	return unicode.IsDigit(symbol) || symbol == '.' || symbol == 'e' || symbol == '-'
}

// Obviously extremely naive and will most certainly not work correctly in anything but the
// straightforward case. For example this fails epicly if we have an expression like
// 2+3 which will be parsed as a valid single number.
// TODO Address this nonsense.
func readNumber(reader *bufio.Reader) (int, string, error) {
	var sb strings.Builder
	for {
		if symbol, _, err := reader.ReadRune(); err != nil || !isNumberSymbol(symbol) {
			reader.UnreadRune()
			return TokenNumber, sb.String(), err
		} else {
			sb.WriteRune(symbol)
		}
	}
}

func readString(reader *bufio.Reader) (int, string, error) {
	var sb strings.Builder
	openingQuote, _, err := reader.ReadRune()
	if err != nil {
		return TokenError, "", err
	}
	for {
		if symbol, _, err := reader.ReadRune(); err != nil {
			return TokenError, "", err
		} else if symbol == openingQuote {
			return TokenString, sb.String(), nil
		} else {
			sb.WriteRune(symbol)
		}
	}
}

func main() {
	file, _ := os.Open(os.Args[1])
	reader := bufio.NewReader(file)

	tokenChan := make(chan Token)
	go NewTokenizer(true).Tokenize(reader, tokenChan)
	fmt.Println(`
<!doctype>
<html>
	<head>
		<style>
        	.keyword {
            	color: red;
            }
            .number {
                color: blue;
            }
            .string {
                color: green;
            }
            .operator {
                font-style: bold;
            }
            .identifier {
                color: orange;
            }
        </style>
    </head>
    <body>
    	<pre class="code">`)

	for token := range tokenChan {
		if token.TokenType >= TokenMinus && token.TokenType <= TokenBitwiseOrEqual {
			outputToken("operator", token.TokenValue)
		} else if token.TokenType == TokenString {
			outputToken("string", token.TokenValue)
		} else if token.TokenType == TokenKeyword {
			outputToken("keyword", token.TokenValue)
		} else if token.TokenType == TokenNumber {
			outputToken("number", token.TokenValue)
		} else if token.TokenType == TokenIdentifier {
			outputToken("identifier", token.TokenValue)
		} else {
			fmt.Print(token.TokenValue)
		}
	}

	fmt.Print("</pre></body></html>")
	file.Close()
}

func outputToken(tokenType, tokenValue string) {
	fmt.Print(fmt.Sprintf("<span class=\"%s\">%s</span>", tokenType, tokenValue))
}
