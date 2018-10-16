using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// TODO: IsValidSting() -> check if identifier is valid, some invalid ones { c!, c@, c# , ... } 

namespace Wizard
{
    public class Tokenizer
    {
        public Tokenizer(string code, string location)
        {
            Code = code;
            Location = location;
            CurrentLine = 0;
            CurrentIndex = 0;
            Keywords = new Dictionary<string, TokenType>()
            {
                { "break", TokenType.Break },
                { "case",TokenType.Case},
                {"catch", TokenType.Catch },
                {"class", TokenType.Class },
                {"const", TokenType.Const },
                {"continue", TokenType.Continue },
                {"debugger", TokenType.Debugger },
                {"default", TokenType.Default },
                {"delete", TokenType.Delete },
                {"do", TokenType.Do },
                {"else", TokenType.Else },
                {"export", TokenType.Export },
                {"extends", TokenType.Extends },
                {"finally", TokenType.Finally },
                {"for", TokenType.For },
                {"function", TokenType.Function },
                {"if", TokenType.If },
                {"import", TokenType.Import },
                {"in", TokenType.In },
                {"instanceof", TokenType.Instanceof },
                {"let", TokenType.Let },
                {"new", TokenType.New },
                {"return", TokenType.Return },
                {"super", TokenType.Super },
                {"switch", TokenType.Switch },
                {"this", TokenType.This },
                {"throw", TokenType.Throw },
                {"try", TokenType.Try },
                {"typeof", TokenType.Typeof },
                {"var", TokenType.Var },
                {"void", TokenType.Void },
                {"while", TokenType.While },
                {"with", TokenType.With },
                {"yield", TokenType.Yield }
            };
        }

        public string Code { get; }
        public string Location { get; }
        public uint CurrentLine { get; private set; }
        public int CurrentIndex { get; private set; }
        public Dictionary<string, TokenType> Keywords { get; }


        public Token GenerateToken(TokenType type)
        {
            return new Token(type, CurrentLine, "", Location, null);
        }

        public Token GenerateToken(TokenType type, string lexeme)
        {
            return new Token(type, CurrentLine, lexeme, Location, null);
        }
        public void PrintTokens()
        {
            var tokens = GenerateTokens();
            foreach (var token in tokens)
            {
                Console.WriteLine(token.ToString());
            }
        }
        public List<Token> GenerateTokens()
        {
            var tokens = new List<Token>();

            do
            {
                tokens.Add(NextToken());
            }
            while (CurrentIndex != Code.Length);

            return tokens;
        }

        public void PrettyPrint(List<Token> tokens, string fileDestination)
        {
            List<TokenType> operators = LoadOperators();//keywords identifier let number string var
            using (var writer = new StreamWriter(fileDestination))
            {
                foreach (var token in tokens)
                {
                    if (token.Type == TokenType.Whitespace) writer.Write("&nbsp;");
                    else if (token.Type == TokenType.Newline) writer.Write("<br>");
                    else if (token.Type == TokenType.Number) writer.Write("<span><font color=\"#00FFFF  \">" + token.Lexeme + "</font></span>");
                    else if (token.Type == TokenType.Identifier) writer.Write("<span><font color=\"#FFC300\">" + token.Lexeme + "</font></span>");
                    else if (token.Type == TokenType.String) writer.Write("<span><font color=\"#C70039\">\"" + token.Lexeme + "\"</font></span>");
                    else if (Keywords.ContainsValue(token.Type))
                    {
                        writer.Write("<span><font color=\"#900C3F\">" + token.Lexeme + "</font></span>");
                    }
                    else if (operators.Contains(token.Type)) writer.Write("<span><b>" + token.Lexeme + "</b></span>");
                    else
                    {
                        writer.Write("<span><font color=\"#581845\">" + token.Lexeme + "</font></span>");
                    }
                }
            }
        }
        
        private List<TokenType> LoadOperators()
        {
            var operators = new List<TokenType>();
            operators.Add(TokenType.Bang);
            operators.Add(TokenType.BangEqual);
            operators.Add(TokenType.BitwiseAnd);
            operators.Add(TokenType.BitwiseAndEqual);
            operators.Add(TokenType.BitwiseNot);
            operators.Add(TokenType.BitwiseOr);
            operators.Add(TokenType.BitwiseOrEqual);
            operators.Add(TokenType.BitwiseXor);
            operators.Add(TokenType.BitwiseXorEqual);
            operators.Add(TokenType.DivideEqual);
            operators.Add(TokenType.Division);
            operators.Add(TokenType.DoubleBang);
            operators.Add(TokenType.DoubleStar);
            operators.Add(TokenType.DoubleStarEqual);
            operators.Add(TokenType.Equal);
            operators.Add(TokenType.EqualEqual);
            operators.Add(TokenType.Greater);
            operators.Add(TokenType.GreaterEqual);
            operators.Add(TokenType.LeftShift);
            operators.Add(TokenType.LeftShiftEqual);
            operators.Add(TokenType.Less);
            operators.Add(TokenType.LessEqual);
            operators.Add(TokenType.LogicalAnd);
            operators.Add(TokenType.LogicalOr);
            operators.Add(TokenType.Minus);
            operators.Add(TokenType.MinusEqual);
            operators.Add(TokenType.MinusMinus);
            operators.Add(TokenType.Modulo);
            operators.Add(TokenType.ModuloEqual);
            operators.Add(TokenType.Plus);
            operators.Add(TokenType.PlusEqual);
            operators.Add(TokenType.PlusPlus);
            operators.Add(TokenType.QuestionMark);
            operators.Add(TokenType.RightShift);
            operators.Add(TokenType.RightShiftEqual);
            operators.Add(TokenType.Star);
            operators.Add(TokenType.StarEqual);
            operators.Add(TokenType.StrictEqual);
            operators.Add(TokenType.StrictNotEqual);

            return operators;
        }


        private Token NextToken()
        {
            char c = Code[CurrentIndex];

            switch (c)
            {
                case '\0': CurrentIndex++; return GenerateToken(TokenType.Eof);
                case '(': CurrentIndex++; return GenerateToken(TokenType.LeftParenthesis, "(");
                case '[': CurrentIndex++; return GenerateToken(TokenType.LeftSquareBracket, "[");
                case '{': CurrentIndex++; return GenerateToken(TokenType.LeftBrace, "{");
                case '}': CurrentIndex++; return GenerateToken(TokenType.RightBrace, "}");
                case ')': CurrentIndex++; return GenerateToken(TokenType.RightParenthesis, ")");
                case ']': CurrentIndex++; return GenerateToken(TokenType.RightSquareBracket, "]");
                case '?': CurrentIndex++; return GenerateToken(TokenType.QuestionMark, "?");
                case '.': CurrentIndex++; return GenerateToken(TokenType.Dot, ".");
                case ',': CurrentIndex++; return GenerateToken(TokenType.Comma, ",");
                case ':': CurrentIndex++; return GenerateToken(TokenType.Colon, ":");
                case ';': CurrentIndex++; return GenerateToken(TokenType.Semicolon, ";");
                case '~': CurrentIndex++; return GenerateToken(TokenType.BitwiseNot, "~");
                case '\n': CurrentIndex++; return GenerateToken(TokenType.Newline, "\n");
                case '\t': CurrentIndex++; return GenerateToken(TokenType.Tab, "\t");
                case ' ': CurrentIndex++; return GenerateToken(TokenType.Whitespace, " ");

                case '/':
                    CurrentIndex++;
                    return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.DivideEqual, "/=") : GenerateToken(TokenType.Division, "/");
                case '%':
                    CurrentIndex++;
                    return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.ModuloEqual, "%=") : GenerateToken(TokenType.Modulo, "%"); // %=
                case '^':
                    CurrentIndex++;
                    return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.BitwiseXorEqual, "^=") : GenerateToken(TokenType.BitwiseXor, "^");
                case '+':
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.PlusEqual, "+=");
                    if (Code[CurrentIndex] == '+') return GenerateToken(TokenType.PlusPlus, "++");
                    return GenerateToken(TokenType.Plus, "+");// + ++ +=
                case '-':
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.MinusEqual, "-=");
                    if (Code[CurrentIndex] == '-') return GenerateToken(TokenType.MinusMinus, "--");
                    return GenerateToken(TokenType.Minus, "-");// -- -=
                case '&':
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.BitwiseAndEqual, "&=");
                    if (Code[CurrentIndex] == '&') return GenerateToken(TokenType.LogicalAnd, "&&");
                    return GenerateToken(TokenType.BitwiseAnd, "&");
                case '|': // 
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.BitwiseOrEqual, "|=");
                    if (Code[CurrentIndex] == '|') return GenerateToken(TokenType.LogicalOr, "||");
                    return GenerateToken(TokenType.BitwiseOr, "|");

                case '=': // = == ===
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=')
                    {
                        CurrentIndex++;
                        return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.StrictEqual, "===") : GenerateToken(TokenType.EqualEqual, "==");
                    }
                    return GenerateToken(TokenType.Equal, "=");

                case '!': // ! !! != !==
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '!') return GenerateToken(TokenType.DoubleBang, "!!");
                    if (Code[CurrentIndex] == '=')
                    {
                        CurrentIndex++;
                        return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.StrictNotEqual, "!==") : GenerateToken(TokenType.BangEqual, "!=");
                    }
                    return GenerateToken(TokenType.Bang, "!");

                case '*': // * *= ** **=
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.StarEqual, "*=");
                    if (Code[CurrentIndex] == '*')
                    {
                        CurrentIndex++;
                        return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.DoubleStarEqual, "**=") : GenerateToken(TokenType.DoubleStar, "**");
                    }
                    return GenerateToken(TokenType.Star, "*");

                case '>': // > >= >> >>=
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.GreaterEqual, ">=");
                    if (Code[CurrentIndex] == '>')
                    {
                        CurrentIndex++;
                        return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.RightShiftEqual, ">>=") : GenerateToken(TokenType.RightShift, ">>");
                    }
                    return GenerateToken(TokenType.Greater, ">");

                case '<': // < <= << <<=
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.LessEqual, "<=");
                    if (Code[CurrentIndex] == '<')
                    {
                        CurrentIndex++;
                        return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.LeftShiftEqual, "<<=") : GenerateToken(TokenType.LeftShift, "<<");
                    }
                    return GenerateToken(TokenType.Less, "<");
                default:
                    break;
            }

            if (IsLetterOrUnderscore(c))
            {
                int nextSeparatorIndex = -1;
                string word = GetCurrentWord(out nextSeparatorIndex);

                if (nextSeparatorIndex == -1)
                {
                    throw new Exception("GetCurrentWord blew up boi.\n");
                }

                CurrentIndex = nextSeparatorIndex;

                if (Keywords.ContainsKey(word))
                {
                    return new Token(Keywords[word], CurrentLine, word, Location, null);
                }

                return new Token(TokenType.Identifier, CurrentLine, word, Location, null);
            }

            if (c == '"')// requires additional logic for errors and corner cases
            {
                CurrentIndex++;

                int indexOfNextDoubleQuotes = Code.IndexOf('"', CurrentIndex);
                string str = Code.Substring(CurrentIndex, indexOfNextDoubleQuotes - CurrentIndex);

                CurrentIndex = indexOfNextDoubleQuotes + 1;
                return GenerateToken(TokenType.String, str);
            }
            if (IsNumber(c))
            {
                int nextSeparatorIndex = -1;
                double number = GetCurrentNumber(out nextSeparatorIndex);

                if (nextSeparatorIndex != -1)
                {
                    CurrentIndex = nextSeparatorIndex;
                    return new Token(TokenType.Number, CurrentLine, number.ToString(), Location, number);
                }

                throw new Exception("GetCurrentNumber failed you my boi.\n");
            }
            return new Token(TokenType.Eof, CurrentLine, "", Location, null);
        }

        private double GetCurrentNumber(out int nextSeparatorIndex)
        {

            string word = GetCurrentWord(out nextSeparatorIndex);
            double number;

            if (double.TryParse(word, out number))
            {
                return number;
            }

            nextSeparatorIndex = -1;
            return -1;
        }

        private bool IsNumber(char c)
        {
            return c >= '0' && c <= '9';
        }

        private string GetCurrentWord(out int nextSeparatorIndex)
        {
            nextSeparatorIndex = GetNextSeparatorIndex();
            int length = nextSeparatorIndex - CurrentIndex;
            string currentWord = Code.Substring(CurrentIndex, length);
            //if (IsValidString(currentWord)) 
            return currentWord;

        }


        private int GetNextSeparatorIndex()// wrong implementation, should check if separator is any operator
        {
            var separators = new List<int>();
            separators.Add(Code.IndexOf('\n', CurrentIndex));
            separators.Add(Code.IndexOf('\t', CurrentIndex));
            separators.Add(Code.IndexOf(' ', CurrentIndex));
            separators.Add(Code.IndexOf(',', CurrentIndex));
            separators.Add(Code.IndexOf('(', CurrentIndex));
            separators.Add(Code.IndexOf(')', CurrentIndex));
            separators.Add(Code.IndexOf(';', CurrentIndex));
            separators.Add(Code.IndexOf('+', CurrentIndex));
            separators.Add(Code.IndexOf('-', CurrentIndex));
            separators.Sort();

            foreach (var separator in separators)
            {
                if (separator >= 0) return separator;
            }

            return -1;
        }

        private bool IsLetterOrUnderscore(char c)
        {
            return c >= 'A' && c <= 'z' || c == '_';
        }
    }
}
