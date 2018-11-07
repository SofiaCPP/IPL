using JSCompiler.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JSCompiler.Core
{
    public class Lexer
    {
        public static readonly string UNCLOSED_MULTILINE_COMMENT = "Unclosed mulitline comment";

        public static readonly string UNCLOSED_STRING = "Unclosed string";

        public static readonly string UNCLOSED_STRING_NEWLINE = "Unclosed string newline";

        public static readonly string UNIDENTIFIED_SYMBOL = "Unidentified symbol";


        private Dictionary<string, TokenType> keywords;

        private Dictionary<string, TokenType> otherReserved;

        private IInputStream stream;

        public Lexer(IInputStream stream)
        {
            this.stream = stream;
            this.keywords = ReservedStrings.KeywordsAsMap();
            this.otherReserved = ReservedStrings.OthersAsMap();
        }

        public List<Token> Tokenize(Action<int, string> onError)
        {
            List<Token> tokens = new List<Token>();
            do
            {
                Token next = NextToken(onError);
                if(next.Type != TokenType.Skip)
                {
                    tokens.Add(next);
                }
            } while (!stream.IsEndOfStream());
            tokens.Add(new Token(TokenType.EOF));
            return tokens;
        }

        public Token NextToken(Action<int, string> onError)
        {
            char cur = stream.Next();

            if (Utils.IsCarriageReturn(cur))
                return new Token(TokenType.Skip);

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
                case '*':
                    return Match('=') ? new WordToken("*=", TokenType.StarEqual)
                        : new Token(TokenType.Star);
                case '/':
                    return Match('/') ? ParseOneLineComment()
                        : Match('*') ? ParseMultilineToken(onError)
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
                        : Match('|') ? new WordToken("||", TokenType.LogicalOr)
                        : new Token(TokenType.BitwiseOr);
                case '&':
                    return Match('=') ? new WordToken("&=", TokenType.BitwiseAndEqual)
                        : Match('&') ? new WordToken("&&", TokenType.LogicalAnd)
                        : new Token(TokenType.BitwiseAnd);
                default:
                    break;
            }

            if (cur < 127 && Enum.IsDefined(typeof(TokenType), (int)cur))
                return new Token((TokenType)cur);

            if(Utils.IsStringSingleQuote(cur) || Utils.IsStringDoubleQuote(cur))
            {
                return ParseSingleQuoteString(cur, onError);
            }

            if (char.IsDigit(cur))
            {
                return ParseNumber(cur);
            }

            if (char.IsLetter(cur))
            {
                return ParseReservedOrIdentifier(cur);
            }

            onError(stream.Line, UNIDENTIFIED_SYMBOL);
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

        private Token ParseMultilineToken(Action<int, string> onError)
        {
            string lexeme = "/*";
            int lineBeg = stream.Line + 1;
            while (!stream.IsEndOfStream())
            {
                while (stream.Peek() != '*')
                {
                    if (stream.IsEndOfStream())
                    {
                        onError(lineBeg, UNCLOSED_MULTILINE_COMMENT);
                        return new WordToken(lexeme, TokenType.Comment);
                    }

                    lexeme += stream.Next();
                };
                lexeme += stream.Next();
                if (stream.Peek() == '/')
                {
                    lexeme += stream.Next();
                    return new WordToken(lexeme, TokenType.Comment);
                }
            }
            onError(lineBeg, UNCLOSED_MULTILINE_COMMENT);
            return new WordToken(lexeme, TokenType.Comment);
        }
       
        private Token ParseSingleQuoteString(char quote, Action<int, string> onError)
        {
            string lexeme = "" + quote;
            int startLine = stream.Line + 1;
            char peek = stream.Peek();
            while(peek != quote)
            {
                if (Utils.IsNewLine(peek) || Utils.IsCarriageReturn(peek))
                {
                    onError(startLine, UNCLOSED_STRING_NEWLINE);
                    return new WordToken(lexeme, TokenType.Error);
                }

                if (stream.IsEndOfStream())
                {
                    onError(startLine, UNCLOSED_STRING);
                    return new WordToken(lexeme, TokenType.Error);
                }

                lexeme += peek;
                stream.Next();

                if(peek == '\\')
                {
                    char next = stream.Peek();

                    if (next == quote || next == '\\')
                    {
                        lexeme += next;
                        stream.Next();
                    }
                    else
                    {
                        if (Utils.IsCarriageReturn(next)) //Skip Carriage return
                        {
                            lexeme += stream.Next();
                            next = stream.Peek();
                        }
                        if (Utils.IsNewLine(next))
                        {
                            lexeme += stream.Next();
                        }
                    }
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
