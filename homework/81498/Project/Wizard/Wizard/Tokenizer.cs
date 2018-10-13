using System;
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
            CurrentLine = 0;
            CurrentIndex = 0;
            Keywords = new Dictionary<string, TokenType>()
            {
                { "function", TokenType.Function },
                { "return",TokenType.Return }
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
                case '\0': CurrentIndex++; return GenerateToken(TokenType.EOF);
                case '(': CurrentIndex++; return GenerateToken(TokenType.LeftParenthesis);
                case ')': CurrentIndex++; return GenerateToken(TokenType.RightParenthesis);
                case ',': CurrentIndex++; return GenerateToken(TokenType.Comma);
                case '{': CurrentIndex++; return GenerateToken(TokenType.LeftBrace);
                case '}': CurrentIndex++; return GenerateToken(TokenType.RightBrace);
                case ';': CurrentIndex++; return GenerateToken(TokenType.Semicolon);
                case '+': CurrentIndex++; return GenerateToken(TokenType.Plus);
                case ' ': CurrentIndex++; return GenerateToken(TokenType.Whitespace);
                case '\t': CurrentIndex++; return GenerateToken(TokenType.Tab);
                case '\n': CurrentIndex++; ++CurrentLine; return GenerateToken(TokenType.Newline);
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
            return new Token(TokenType.EOF, CurrentLine, "", Location, null);
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
