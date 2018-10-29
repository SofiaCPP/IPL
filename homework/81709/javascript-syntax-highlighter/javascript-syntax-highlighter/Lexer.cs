using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    public class Lexer
    {
        private Dictionary<string, TokenType> idsMap;

        private InputStream stream;

        public Lexer(InputStream stream)
        {
            this.stream = stream;
            this.idsMap = IdTokenTypes.AsMap();
        }

        public List<Token> Tokenize()
        {
            List<Token> tokens = new List<Token>();
            do
            {
                tokens.Add(NextToken());
            } while (!stream.IsEndOfStream());

            return tokens;
        }

        public Token NextToken()
        {
            //TODO Parse Comments

            char cur = stream.Next();

            if (Utils.IsCarriageReturn(cur))
                cur = stream.Next();

            if (Utils.IsWhiteSpace(cur) || Utils.IsTab(cur) || Utils.IsNewLine(cur))
                return new WordToken(cur.ToString(), TokenType.Whitespace);

            switch (cur)
            {
                case '=':
                    return Match('=') ? Match('=') ? new WordToken("===", TokenType.StrictEqual)
                           : new WordToken("==", TokenType.EqualEqual)
                           : new Token(TokenType.Equal);
                case '!':
                    return Match('=') ? Match('=') ? new WordToken("!==", TokenType.StrictNotEqual)
                           : new WordToken("!=", TokenType.NotEqual)
                           : new Token(TokenType.Bang);
                case '>':
                    return Match('=') ? new WordToken(">=", TokenType.GreaterEqual)
                           : Match('>') ? Match('=') ? new WordToken(">>=", TokenType.RightShiftEqual)
                           : new WordToken(">>", TokenType.RightShift)
                           : new Token(TokenType.Greater);
                case '<':
                    return Match('=') ? new WordToken("<=", TokenType.LessEqual)
                           : Match('<') ? Match('=') ? new WordToken("<<=", TokenType.LeftShiftEqual)
                           : new WordToken("<<", TokenType.LeftShift)
                           : new Token(TokenType.Less);
                case '+':
                    return Match('+') ? new WordToken("++", TokenType.PlusPlus)
                           : Match('=') ? new WordToken("+=", TokenType.PlusEqual)
                           : new Token(TokenType.Plus);
                case '-':
                    return Match('-') ? new WordToken("--", TokenType.MinusMinus)
                           : Match('=') ? new WordToken("-=", TokenType.MinusEqual)
                           : new Token(TokenType.Minus);
                case '/':
                    return Match('=') ? new WordToken("/=", TokenType.DivideEqual)
                           : new Token(TokenType.Division);
                case '%':
                    return Match('=') ? new WordToken("%=", TokenType.ModuloEqual)
                           : new Token(TokenType.Modulo);
                case '^':
                    return Match('=') ? new WordToken("^=", TokenType.BitwiseXorEqual)
                            : new Token(TokenType.BitwiseXor);
                case '|':
                    return Match('=') ? new WordToken("|=", TokenType.BitwiseOrEqual)
                            : new Token(TokenType.BitwiseOr);
                case '&':
                    return Match('=') ? new WordToken("&=", TokenType.BitwiseAndEqual)
                            : new Token(TokenType.BitwiseAnd);
                default:
                    break;
            }

            if (cur < 127 && Enum.IsDefined(typeof(TokenType), (int)cur))
                return new Token((TokenType)cur);

            if(cur == '"')
            {
                return ParseString(cur);
            }

            if(char.IsDigit(cur))
            {
                return ParseNumber(cur);
            }

            if (char.IsLetter(cur))
            {
                return ParseKeywordOrIdentifier(cur);
            }

            return new Token(TokenType.Invalid);
        }

        private bool Match(char ch)
        {
            if(stream.Peek() == ch)
            {
                stream.Next();
                return true;
            }
            return false;
        }

        private Token ParseString(char ch)
        {
            string lexeme = "" + ch;
            while (stream.Peek() != '"')
            {
                lexeme += stream.Next();
                if (stream.IsEndOfStream())
                {
                    return new WordToken(lexeme, TokenType.Invalid);
                }
            }
            lexeme += stream.Next();
            return new WordToken(lexeme, TokenType.String);
        }

        private Token ParseNumber(char cur)
        {
            long num = cur - '0';
            char peek = stream.Peek();
            while (char.IsDigit(peek))
            {
                num = num * 10 + (stream.Next() - '0');

                if (stream.IsEndOfStream())
                {
                    return new WordToken(num.ToString(), TokenType.Invalid);
                }
                peek = stream.Peek();
            }
            return new NumToken(num);
        }

        private Token ParseKeywordOrIdentifier(char ch)
        {
            string lexeme = "" + ch;
            bool chanceToBeIdentifier = char.IsLower(ch);

            char peek = stream.Peek();
            while (char.IsLetterOrDigit(peek))
            {
                if (chanceToBeIdentifier && (char.IsUpper(peek) || char.IsDigit(peek)))
                {
                    chanceToBeIdentifier = false;
                }

                lexeme += stream.Next();
                if (stream.IsEndOfStream())
                {
                    return new WordToken(lexeme, TokenType.Invalid);
                }
                peek = stream.Peek();
            }

            if (chanceToBeIdentifier)
            {
                TokenType keywordType;
                idsMap.TryGetValue(lexeme, out keywordType);
                if(keywordType != 0)
                    return new WordToken(lexeme, keywordType);
            }

            return new WordToken(lexeme, TokenType.Identifier);
        }

        private Token ParseKeyword(char cur)
        {
            string lexeme = "" + cur;
            char peek = stream.Peek();
            while (char.IsLetter(peek) && char.IsLower(peek))
            {
                lexeme += stream.Next();
                if (stream.IsEndOfStream())
                {
                    return new WordToken(lexeme, TokenType.Invalid);
                }
                peek = stream.Peek();
            }

            TokenType keywordType;
            idsMap.TryGetValue(lexeme, out keywordType);

            return keywordType == 0 ? null : new WordToken(lexeme, keywordType);
        }

        private Token ParseIdentifier(char cur)
        {
            string lexeme = "" + cur;
            char peek = stream.Peek();
            while (char.IsLetterOrDigit(peek))
            {
                lexeme += stream.Next();
                if (stream.IsEndOfStream())
                {
                    return new WordToken(lexeme, TokenType.Invalid);
                }
                peek = stream.Peek();
            }
            return new WordToken(lexeme, TokenType.Identifier);
        }
    }
}
