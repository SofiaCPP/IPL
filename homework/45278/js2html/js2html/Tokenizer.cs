using js2html.Models;
using System;
using System.Collections.Generic;
using System.Text;

namespace js2html
{
    public class Tokenizer
    {
        public Dictionary<string, TokenType> KeywordsTable { get; set; }
        public int Line { get; set; }
        public int Current { get; set; }
        public string Code { get; set; }
        public IEnumerable<Token> Tokens { get; set; }
        public Tokenizer(string code)
        {
            KeywordsTable = new Dictionary<string, TokenType>();
            KeywordsTable.Add("break", TokenType.Break);
            KeywordsTable.Add("case", TokenType.Case);
            KeywordsTable.Add("catch", TokenType.Catch);
            KeywordsTable.Add("class", TokenType.Class);
            KeywordsTable.Add("const", TokenType.Const);
            KeywordsTable.Add("continue", TokenType.Continue);
            KeywordsTable.Add("debugger", TokenType.Debugger);
            KeywordsTable.Add("default", TokenType.Default);
            KeywordsTable.Add("delete", TokenType.Delete);
            KeywordsTable.Add("do", TokenType.Do);
            KeywordsTable.Add("else", TokenType.Else);
            KeywordsTable.Add("export", TokenType.Export);
            KeywordsTable.Add("extends", TokenType.Extends);
            KeywordsTable.Add("finally", TokenType.Finally);
            KeywordsTable.Add("for", TokenType.For);
            KeywordsTable.Add("function", TokenType.Function);
            KeywordsTable.Add("if", TokenType.If);
            KeywordsTable.Add("import", TokenType.Import);
            KeywordsTable.Add("in", TokenType.In);
            KeywordsTable.Add("instanceof", TokenType.Instanceof);
            KeywordsTable.Add("new", TokenType.New);
            KeywordsTable.Add("return", TokenType.Return);
            KeywordsTable.Add("super", TokenType.Super);
            KeywordsTable.Add("switch", TokenType.Switch);
            KeywordsTable.Add("this", TokenType.This);
            KeywordsTable.Add("throw", TokenType.Throw);
            KeywordsTable.Add("try", TokenType.Try);
            KeywordsTable.Add("typeof", TokenType.Typeof);
            KeywordsTable.Add("var", TokenType.Var);
            KeywordsTable.Add("void", TokenType.Void);
            KeywordsTable.Add("while", TokenType.While);
            KeywordsTable.Add("with", TokenType.With);
            KeywordsTable.Add("yield", TokenType.Yield);
            KeywordsTable.Add("null", TokenType.Null);
            KeywordsTable.Add("undefined", TokenType.Undefined);
            KeywordsTable.Add("true", TokenType.True);
            KeywordsTable.Add("false", TokenType.False);

            Current = 0;
            Line = 1;
            Code = code;
        }

        public IEnumerable<Token> Tokenize()
        {
            List<Token> tokens = new List<Token>();
            do
            {
                tokens.Add(GetNextToken());
            } while (tokens[tokens.Count - 1].TokenType != TokenType.Eof);
            return tokens;
        }

        private Token ProduceWhitespaceToken()
        {
            var lexeme = new StringBuilder();
            while(string.IsNullOrWhiteSpace(Code[Current].ToString()))
            {
                lexeme.Append(Code[Current]);
                if (Code[Current] == '\n')
                {
                    Current++;
                    return new Token(TokenType.Whitespace, Line++, lexeme.ToString());
                }
                Current++;
            }
            return new Token(TokenType.Whitespace, Line, lexeme.ToString());
        }

        private string ReadString()
        {
            var opening = Code[Current++];
            var lexeme = new StringBuilder(opening);
            while (!Match(opening))
            {
                lexeme.Append(Code[Current]);
                Current++;
            }
            lexeme.Append(opening);
            return lexeme.ToString();
        }

        private string ReadNumber()
        {
            var lexeme = new StringBuilder();
            while(Code[Current] >= '0' && Code[Current] <= '9')
            {
                lexeme.Append(Code[Current]);
                Current++;
            }
            return lexeme.ToString();
        }

        private string ReadKeyword()
        {
            var lexeme = new StringBuilder();
            while (Code[Current] >= 'a' && Code[Current] <= 'z')
            {
                lexeme.Append(Code[Current]);
                Current++;
            }
            return lexeme.ToString();
        }

        private string ReadIdentifier()
        {
            var lexeme = new StringBuilder();
            while (IsValidIdentifierChar())
            {
                lexeme.Append(Code[Current]);
                Current++;
            }
            return lexeme.ToString();
        }

        public Token GetNextToken()
        {
            if (Current == Code.Length)
            {
                return new Token(TokenType.Eof, Line);
            }
            if (Code[Current] == '\0')
            {
                return new Token(TokenType.Eof, Line);
            }
            

            if (string.IsNullOrWhiteSpace(Code[Current].ToString())) return ProduceWhitespaceToken();

            switch (Code[Current])
            {
                case '(': Current++; return new Token(TokenType.LeftParen, Line, "(");
                case ')': Current++; return new Token(TokenType.RightParen, Line, ")");
                case '{': Current++; return new Token(TokenType.LeftBrace, Line, "{");
                case '}': Current++; return new Token(TokenType.RightBrace, Line, "}");
                case ',': Current++; return new Token(TokenType.Comma, Line, ",");
                case '.': Current++; return new Token(TokenType.Dot, Line, ".");
                case '-':
                    Current++;
                    if (Match('-'))
                        return new Token(TokenType.MinusMinus, Line, "--");
                    else if (Match('='))
                        return new Token(TokenType.MinusEqual, Line, "-=" );
                    else
                        return new Token(TokenType.Minus, Line, "-");
                case '+':
                    Current++;
                    if (Match('+'))
                        return new Token(TokenType.PlusPlus, Line, "++");
                    else if (Match('='))
                        return new Token(TokenType.PlusEqual, Line, "+=");
                    else
                        return new Token(TokenType.Plus, Line, "+");
                case ';': Current++; return new Token(TokenType.Semicolon, Line, ";");
                case '*': Current++; return new Token(TokenType.Star, Line, "*");
                case '/': Current++; return new Token(TokenType.Division, Line, "/");
                case '%': Current++; return new Token(TokenType.Modulo, Line, "%");
                case '~': Current++; return new Token(TokenType.BitwiseNot, Line, "~");
                case '=':
                    Current++;
                    if (Match('='))
                    {
                        if (Match('='))
                            return new Token(TokenType.StrictEqual, Line, "===");
                        else
                            return new Token(TokenType.EqualEqual, Line, "==");
                    }
                    else
                        return new Token(TokenType.Equal, Line, "=");
                case '!':
                    Current++;
                    if (Match('='))
                    {
                        if (Match('='))
                            return new Token(TokenType.StrictNotEqual, Line, "!==");
                        else
                            return new Token(TokenType.BangEqual, Line, "!!");
                    }
                    else
                        return new Token(TokenType.Bang, Line, "!");
                case '>':
                    Current++;
                    if (Match('='))
                        return new Token(TokenType.GreaterEqual, Line, ">=");
                    else if (Match('>'))
                        return new Token(TokenType.RightShift, Line, ">>");
                    else
                        return new Token(TokenType.Greater, Line, ">");
                case '<':
                    Current++;
                    if (Match('='))
                        return new Token(TokenType.LessEqual, Line, "<=");
                    else if (Match('<'))
                        return new Token(TokenType.LeftShift, Line, "<<");
                    else
                        return new Token(TokenType.Less, Line, "<");
                case '&':
                    Current++;
                    if (Match('&')) return new Token(TokenType.LogicalAnd, Line, "&&");
                    else return new Token(TokenType.BitwiseAnd, Line, "&");
                case '|':
                    Current++;
                    if (Match('|')) return new Token(TokenType.LogicalOr, Line,"||");
                    else return new Token(TokenType.BitwiseOr, Line, "|");
                case '^': Current++; return new Token(TokenType.BitwiseXor, Line, "^");
                case '?': Current++; return new Token(TokenType.QuestionMark, Line, "?");
                case ':': Current++; return new Token(TokenType.Colon, Line, ":");
                case '[': Current++; return new Token(TokenType.LeftSquareBracket, Line, "[");
                case ']': Current++; return new Token(TokenType.RightSquareBracket, Line, "]");
                default:
                    break;
            }
            
            if(Code[Current] == '\'' || Code[Current] == '"')
            {
                var lexeme = ReadString();
                return new Token(TokenType.String, Line, lexeme);
            }

            if(Code[Current] >= '0' && Code[Current] <= '9')
            {
                var lexeme = ReadNumber();
                return new Token(TokenType.Number, Line, lexeme, double.Parse(lexeme));
            }

            if(Code[Current] >= 'a' && Code[Current] <= 'z')
            {
                var start = Current;
                var lexeme = ReadKeyword();
                TokenType type = TokenType.Eof;
                if(KeywordsTable.TryGetValue(lexeme, out type))
                {
                    return new Token(type, Line, lexeme);
                }
                else
                {
                    Current = start;
                }
            }

            if(IsValidIdentifierStarting())
            {
                var lexeme = ReadIdentifier();
                return new Token(TokenType.Identifier, Line, lexeme);
            }


            return new Token(TokenType.Eof, Line);
        }

        private bool IsValidIdentifierStarting()
        {
            return (Code[Current] >= 'A' && Code[Current] <= 'z') || Code[Current] == '_';
        }

        private bool IsValidIdentifierChar()
        {
            return IsValidIdentifierStarting() || (Code[Current] >= '0' && Code[Current] <= '9');
        }

        private bool Match(char c)
        {
            if (c == Code[Current])
            {
                ++Current;
                return true;
            }
            return false;
        }
    }
}
