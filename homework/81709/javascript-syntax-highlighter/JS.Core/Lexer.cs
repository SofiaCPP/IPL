using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JSCompiler.Core
{
    public class Lexer
    {
        private Dictionary<string, TokenType> keywords;

        private Dictionary<string, TokenType> otherReserved;

        private InputStream stream;

        public Lexer(InputStream stream)
        {
            this.stream = stream;
            this.keywords = ReservedStrings.KeywordsAsMap();
            this.otherReserved = ReservedStrings.OthersAsMap();
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
            char cur = stream.Next();

            if (Utils.IsCarriageReturn(cur))
                cur = stream.Next();

            if (Utils.IsWhiteSpace(cur))
                return new Token(TokenType.Whitespace);

            if (Utils.IsTab(cur))
                return new Token(TokenType.Tab);

            if (Utils.IsNewLine(cur))
                return new Token(TokenType.NewLine);

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
                    return Match('/') ? ParseOneLineComment()
                        : Match('*') ? ParseMultilineToken()
                        : Match('=') ? new WordToken("/=", TokenType.DivideEqual)
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

            if(Utils.IsStringBound(cur))
            {
                return ParseString(cur);
            }

            if(char.IsDigit(cur))
            {
                return ParseNumber(cur);
            }

            if (char.IsLetter(cur))
            {
                return ParseReservedOrIdentifier(cur);
            }

            return new WordToken(cur.ToString(), TokenType.Error);
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

        private Token ParseOneLineComment()
        {
            string lexeme = "//";
            char peek = stream.Peek();
            while(!Utils.IsNewLine(peek) && !Utils.IsCarriageReturn(peek) && !stream.IsEndOfStream())
            {
                lexeme += stream.Next();
                peek = stream.Peek();
            }

            return new WordToken(lexeme, TokenType.Comment);
        }

        private Token ParseMultilineToken()
        {
            string lexeme = "/*";
            int lineBeg = stream.Line + 1;
            while (!stream.IsEndOfStream())
            {
                while (stream.Peek() != '*' && !stream.IsEndOfStream())
                {
                    lexeme += stream.Next();
                };
                lexeme += stream.Next();
                if (stream.Peek() == '/')
                {
                    lexeme += stream.Next();
                    return new WordToken(lexeme, TokenType.Comment);
                }
            }
            return new WordToken(lexeme, TokenType.Comment);
        }

        //TODO Fix parsing of string combinations of ' and ".
        //Example: Fix "My name is 'Josh'. He is cool."
        private Token ParseString(char c)
        {
            string lexeme = c.ToString();
            int startLine = stream.Line + 1;
            char peek = stream.Peek();
            while (!Utils.IsStringBound(peek))
            {
                if (Utils.IsNewLine(peek) || Utils.IsCarriageReturn(peek))
                {
                    return new WordToken(lexeme, TokenType.Error);
                }

                lexeme += stream.Next();
                if (stream.IsEndOfStream())
                {
                    return new WordToken(lexeme, TokenType.Error);
                }
                peek = stream.Peek();
            }
            lexeme += stream.Next();
            return new WordToken(lexeme, TokenType.String);
        }

        private Token ParseNumber(char cur)
        {
            long num = cur - '0';
            char peek = stream.Peek();
            while (char.IsDigit(peek) && !stream.IsEndOfStream())
            {
                num = num * 10 + (stream.Next() - '0');
                peek = stream.Peek();
            }
            return new NumToken(num);
        }

        private Token ParseReservedOrIdentifier(char ch)
        {
            string lexeme = "" + ch;

            while (char.IsLetterOrDigit(stream.Peek()) && !stream.IsEndOfStream())
            {
                lexeme += stream.Next();
            }

            TokenType reservedType;

            keywords.TryGetValue(lexeme, out reservedType);
            if(reservedType != 0)
                return new WordToken(lexeme, reservedType);

            otherReserved.TryGetValue(lexeme, out reservedType);
            if(reservedType != 0)
                return new WordToken(lexeme, reservedType);

            return new WordToken(lexeme, TokenType.Identifier);
        }
    }
}
