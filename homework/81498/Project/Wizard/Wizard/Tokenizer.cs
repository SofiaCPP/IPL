﻿using System;
using System.Collections.Generic;
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
        List<Token> GenerateTokens()
        {
            var tokens = new List<Token>();

            do
            {
                tokens.Add(NextToken());
            }
            while (CurrentIndex != Code.Length);

            return tokens;
        }

        private Token NextToken()
        {
            char c = Code[CurrentIndex];

            switch (c)
            {
                case '\0': CurrentIndex++; return GenerateToken(TokenType.Eof);
                case '(': CurrentIndex++; return GenerateToken(TokenType.LeftParenthesis);
                case '[': CurrentIndex++; return GenerateToken(TokenType.LeftSquareBracket);
                case '{': CurrentIndex++; return GenerateToken(TokenType.LeftBrace);
                case '}': CurrentIndex++; return GenerateToken(TokenType.RightBrace);
                case ')': CurrentIndex++; return GenerateToken(TokenType.RightParenthesis);
                case ']': CurrentIndex++; return GenerateToken(TokenType.RightSquareBracket);
                case '?': CurrentIndex++; return GenerateToken(TokenType.QuestionMark);
                case '.': CurrentIndex++; return GenerateToken(TokenType.Dot);
                case ',': CurrentIndex++; return GenerateToken(TokenType.Comma);
                case ':': CurrentIndex++; return GenerateToken(TokenType.Colon);
                case ';': CurrentIndex++; return GenerateToken(TokenType.Semicolon);
                case '~': CurrentIndex++; return GenerateToken(TokenType.BitwiseNot);
                case '\n': CurrentIndex++; return GenerateToken(TokenType.Newline);
                case '\t': CurrentIndex++; return GenerateToken(TokenType.Tab);
                case ' ': CurrentIndex++; return GenerateToken(TokenType.Whitespace);

                case '/':
                    CurrentIndex++;
                    return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.DivideEqual) : GenerateToken(TokenType.Division);
                case '%':
                    CurrentIndex++;
                    return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.ModuloEqual) : GenerateToken(TokenType.Modulo); // %=
                case '^':
                    CurrentIndex++;
                    return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.BitwiseXorEqual) : GenerateToken(TokenType.BitwiseXor);
                case '+':
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.PlusEqual);
                    if (Code[CurrentIndex] == '+') return GenerateToken(TokenType.PlusPlus);
                    return GenerateToken(TokenType.Plus);// ++ +=
                case '-':
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.MinusEqual);
                    if (Code[CurrentIndex] == '-') return GenerateToken(TokenType.MinusMinus);
                    return GenerateToken(TokenType.Minus);// -- -=
                case '&':
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.BitwiseAndEqual);
                    if (Code[CurrentIndex] == '&') return GenerateToken(TokenType.LogicalAnd);
                    return GenerateToken(TokenType.BitwiseAnd);
                case '|': // 
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.BitwiseOrEqual);
                    if (Code[CurrentIndex] == '|') return GenerateToken(TokenType.LogicalOr);
                    return GenerateToken(TokenType.BitwiseOr);

                case '=': // = == ===
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=')
                    {
                        CurrentIndex++;
                        return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.StrictEqual) : GenerateToken(TokenType.EqualEqual);
                    }
                    return GenerateToken(TokenType.Equal);

                case '!': // ! !! != !==
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '!') return GenerateToken(TokenType.DoubleBang);
                    if (Code[CurrentIndex] == '=')
                    {
                        CurrentIndex++;
                        return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.StrictNotEqual) : GenerateToken(TokenType.BangEqual);
                    }
                    return GenerateToken(TokenType.Bang);

                case '*': // * *= ** **=
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.StarEqual);
                    if (Code[CurrentIndex] == '*')
                    {
                        CurrentIndex++;
                        return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.DoubleStarEqual) : GenerateToken(TokenType.DoubleStar);
                    }
                    return GenerateToken(TokenType.Star);

                case '>': // > >= >> >>=
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.GreaterEqual);
                    if (Code[CurrentIndex] == '>')
                    {
                        CurrentIndex++;
                        return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.RightShiftEqual) : GenerateToken(TokenType.RightShift);
                    }
                    return GenerateToken(TokenType.Greater);

                case '<': // < <= << <<=
                    CurrentIndex++;
                    if (Code[CurrentIndex] == '=') return GenerateToken(TokenType.LessEqual);
                    if (Code[CurrentIndex] == '<')
                    {
                        CurrentIndex++;
                        return Code[CurrentIndex] == '=' ? GenerateToken(TokenType.LeftShiftEqual) : GenerateToken(TokenType.LeftShift);
                    }
                    return GenerateToken(TokenType.Less);
                default:
                    break;
            }

            if (IsLetterOrUnderscore(c))
            {
                int nextEmptySpaceIndex = -1;
                string word = GetCurrentWord(out nextEmptySpaceIndex);

                if (nextEmptySpaceIndex == -1)
                {
                    throw new Exception("GetCurrentWord blew up boi.\n");
                }

                CurrentIndex = nextEmptySpaceIndex;

                if (Keywords.ContainsKey(word))
                {
                    return new Token(Keywords[word], CurrentLine, "", Location, null);
                }

                return new Token(TokenType.Identifier, CurrentLine, word, Location, null);
            }

            if (IsNumber(c))
            {
                int nextEmptySpaceIndex = -1;
                double number = GetCurrentNumber(out nextEmptySpaceIndex);

                if (nextEmptySpaceIndex != -1)
                {
                    CurrentIndex = nextEmptySpaceIndex;
                    return new Token(TokenType.Number, CurrentLine, number.ToString(), Location, number);
                }

                throw new Exception("GetCurrentNumber failed you my boi.\n");
            }
            return new Token(TokenType.Eof, CurrentLine, "", Location, null);
        }

        private double GetCurrentNumber(out int nextEmptySpaceIndex)
        {

            string word = GetCurrentWord(out nextEmptySpaceIndex);
            double number;

            if (double.TryParse(word, out number))
            {
                return number;
            }

            nextEmptySpaceIndex = -1;
            return -1;
        }

        private bool IsNumber(char c)
        {
            return c >= '0' && c <= '9';
        }

        private string GetCurrentWord(out int emptySpaceIndex)
        {
            emptySpaceIndex = GetNextEmptySpaceIndex();
            int length = emptySpaceIndex - CurrentIndex;
            string currentWord = Code.Substring(CurrentIndex, length);
            //if (IsValidString(currentWord)) 
            return currentWord;

        }


        private int GetNextEmptySpaceIndex()
        {
            var emptySpaces = new List<int>();
            emptySpaces.Add(Code.IndexOf('\n', CurrentIndex));
            emptySpaces.Add(Code.IndexOf('\t', CurrentIndex));
            emptySpaces.Add(Code.IndexOf(' ', CurrentIndex));
            emptySpaces.Add(Code.IndexOf(',', CurrentIndex));
            emptySpaces.Add(Code.IndexOf('(', CurrentIndex));
            emptySpaces.Add(Code.IndexOf(')', CurrentIndex));
            emptySpaces.Add(Code.IndexOf(';', CurrentIndex));

            emptySpaces.Sort();

            foreach (var emptySpace in emptySpaces)
            {
                if (emptySpace >= 0) return emptySpace;
            }

            return -1;
        }

        private bool IsLetterOrUnderscore(char c)
        {
            return c >= 'A' && c <= 'z' || c == '_';
        }
    }
}