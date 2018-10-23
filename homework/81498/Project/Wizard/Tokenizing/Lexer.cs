using System;
using System.Collections.Generic;
using System.Linq;
using Wizard.Misc;

namespace Wizard
{
    public class Lexer
    {
        public Lexer(string code, string location)
        {
            _code = code;
            _location = location;

            _currentLine = 1;
            _indexInCode = 0;
            _indexInCurrentLine = 0;

            _keywordsToTokensMap = KeywordsToTokens.AsMap();
            _complexOperatorsToTokensMap = OperatorsToTokens.AsMap(); // you didn't use this shithead
            _exactMatchSymbolsToTokensMap = ExactMatchSymbolsToTokens.AsMap();
        }

        private const string _letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        private const string _digits = "0123456789";
        private const string _identifiers = _letters + _digits + "_";
        private const string _exactMatchSymbols = " \t:,({[]});'?.";
        private const string _singleLetterOperators = "!|&/=><-%+*";

        private string _code;
        private string _location;
        private int _currentLine;
        private int _indexInCode;
        private int _indexInCurrentLine;

        private Dictionary<string, TokenType> _keywordsToTokensMap;
        private Dictionary<string, TokenType> _complexOperatorsToTokensMap;
        private Dictionary<char, TokenType> _exactMatchSymbolsToTokensMap;

        public List<Token> GenerateTokens()
        {
            var tokens = new List<Token>();
            do
            {
                tokens.Add(NextToken());
            }
            while (_indexInCode != _code.Length);

            return tokens;
        }

        private Token NextToken()
        {
            char c = _code[_indexInCode];

            if (_exactMatchSymbols.Contains(c))
            {
                AdvanceIndex();
                return GenerateToken(_exactMatchSymbolsToTokensMap[c], c.ToString());
            }

            if (c is '\"')
            {
                return GenerateToken(TokenType.String, GetNextString());
            }

            if (c is '\n')
            {
                AdvanceIndexOnNewLine();
                return GenerateToken(TokenType.Newline, "\n");
            }
            
            // empty string parsed by Decimal.TryParse returns 0 but that's none of my business
            if (_digits.Contains(c))
            {
                string nextNumber = GetNextNumber();
                return GenerateToken(TokenType.Number, nextNumber);
            }

            if (_identifiers.Contains(c))
            {
                string nextWord = GetNextWord();

                if (_keywordsToTokensMap.ContainsKey(nextWord))
                {
                    return GenerateToken(_keywordsToTokensMap[nextWord], nextWord);
                }

                return GenerateToken(TokenType.Identifier, nextWord);
            }

            if (_singleLetterOperators.Contains(c))
            {
                return GetOperatorToken();
            }
            return new Token(TokenType.Eof, _currentLine, "", _location, null);
        }

        private Token GenerateToken(TokenType type)
        {
            return new Token(type, _currentLine, "", _location, null);
        }

        private Token GenerateToken(TokenType type, string lexeme)
        {
            return new Token(type, _currentLine, lexeme, _location, null);
        }

        private void AdvanceIndex()
        {
            ++_indexInCode;
            ++_indexInCurrentLine;
        }

        private void AdvanceIndexOnNewLine()
        {
            _indexInCode++;
            _currentLine++;
            _indexInCurrentLine = 0;
        }

        private char AdvanceAndGetNext()
        {
            AdvanceIndex();
            return _code[_indexInCode];
        }

        private string GetNextString()
        {   // advance once to ignore double quotes
            AdvanceIndex();
            try
            {
                char c = _code[_indexInCode];
                string quoteTerminatedString = "";

                while (c != '\"')
                {
                    quoteTerminatedString += c;

                    c = AdvanceAndGetNext();
                }

                // advance again to ignore ending quotes
                AdvanceIndex();

                return quoteTerminatedString;
            }
            catch (ArgumentOutOfRangeException)
            {
                return "";
            }
        }

        private string GetNextWord()
        {
            char c = _code[_indexInCode];
            string identifiersOnlyString = "";

            try
            {
                while (_identifiers.Contains(c))
                {
                    identifiersOnlyString += c;

                    c = AdvanceAndGetNext();
                }

                return identifiersOnlyString;
            }
            catch (ArgumentOutOfRangeException)
            {
                return "";
            }
        }

        private string GetNextNumber()
        {
            string nonDigitTerminatedString = "";

            try
            {
                char c = _code[_indexInCode];

                while (_digits.Contains(c))
                {
                    nonDigitTerminatedString += c;

                    c = AdvanceAndGetNext();
                }

                return nonDigitTerminatedString;
            }
            catch (ArgumentOutOfRangeException)
            {
                return "";
            }
        }

        // isn't there better way to do this? I mean structural way, like interface, another class or something? 
        // Should I move handlers to another class?
        // helper functions in an interface?
        // what should be return in the catch of exceptions?
        private Token GetOperatorToken()
        {
            try
            {
                char c = _code[_indexInCode];
                switch (c)
                {
                    case '>': return HandleOperatorGreater();    // > >= 
                    case '<': return HandleOperatorLess();       // < <= 
                    case '/': return HandleOperatorDivision();   // / /=
                    case '%': return HandleOperatorModulo();     // % %=
                    case '+': return HandleOperatorPlus();       // + += ++
                    case '-': return HandleOperatorMinus();      // - -= --
                    case '&': return HandleOperatorBitwiseAnd(); // & &= &&
                    case '|': return HandleOperatorBitwiseOr();  // | |= ||
                    case '=': return HandleOperatorEquals();     // = == ===
                    case '!': return HandleOperatorBang();       // ! != !! !==
                    case '*': return HandleOperatorStar();       // * *= ** **=
                    default:
                        break;
                }

                return GenerateToken(TokenType.None); // throw exception?
            }
            catch (ArgumentOutOfRangeException)
            {
                return GenerateToken(TokenType.None);
            }
        }

        private Token HandleOperatorStar()
        {
            char next = AdvanceAndGetNext();
            if (next is '=')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.StarEqual, "*=");
            }

            if (next is '*')
            {
                next = AdvanceAndGetNext();
                if (next is '=')
                {
                    AdvanceIndex();
                    return GenerateToken(TokenType.DoubleStarEqual, "**=");
                }

                return GenerateToken(TokenType.DoubleStar, "**");
            }

            return GenerateToken(TokenType.Star, "*");
        }

        private Token HandleOperatorBang()
        {
            char next = AdvanceAndGetNext();

            if (next is '!')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.DoubleBang, "!!");
            }

            if (next is '=')
            {
                next = AdvanceAndGetNext();
                if (next is '=')
                {
                    AdvanceIndex();
                    return GenerateToken(TokenType.StrictNotEqual, "!==");
                }

                return GenerateToken(TokenType.BangEqual, "!=");
            }

            return GenerateToken(TokenType.Bang, "!");
        }

        private Token HandleOperatorEquals()
        {
            char next = AdvanceAndGetNext();
            if (next is '=')
            {
                next = AdvanceAndGetNext();
                if (next is '=')
                {
                    AdvanceIndex();
                    GenerateToken(TokenType.StrictEqual, "===");
                }

                return GenerateToken(TokenType.EqualEqual, "==");
            }

            return GenerateToken(TokenType.Equal, "=");
        }

        private Token HandleOperatorBitwiseOr()
        {
            char next = AdvanceAndGetNext();

            if (next is '=')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.BitwiseOrEqual, "|=");
            }

            if (next is '|')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.LogicalOr, "||");
            }

            return GenerateToken(TokenType.BitwiseOr, "|");
        }

        private Token HandleOperatorBitwiseAnd()
        {
            char next = AdvanceAndGetNext();

            if (next is '=')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.BitwiseAndEqual, "&=");
            }

            if (next is '&')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.LogicalAnd, "&&");
            }

            return GenerateToken(TokenType.BitwiseAnd, "&");
        }

        private Token HandleOperatorMinus()
        {
            char next = AdvanceAndGetNext();

            if (next is '=')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.MinusEqual, "-=");
            }

            if (next is '-')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.MinusMinus, "--");
            }

            return GenerateToken(TokenType.Minus, "-");
        }

        private Token HandleOperatorPlus()
        {
            char next = AdvanceAndGetNext();

            if (next is '=')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.PlusEqual, "+=");
            }

            if (next is '+')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.PlusPlus, "++");
            }

            return GenerateToken(TokenType.Plus, "+");
        }

        private Token HandleOperatorModulo()
        {
            char next = AdvanceAndGetNext();
            if (next is '=')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.ModuloEqual, "%=");
            }

            return GenerateToken(TokenType.Modulo, "%");
        }

        private Token HandleOperatorDivision()
        {
            char next = AdvanceAndGetNext();

            if (next is '=')
            {
                AdvanceIndex();
                GenerateToken(TokenType.DivideEqual, "/=");
            }

            return GenerateToken(TokenType.Division, "/");

        }

        private Token HandleOperatorLess()
        {
            char next = AdvanceAndGetNext();

            if (next == '=')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.LessEqual, "<=");
            }

            return GenerateToken(TokenType.Less, "<");
        }

        private Token HandleOperatorGreater()
        {
            char next = AdvanceAndGetNext();

            if (next is '=')
            {
                AdvanceIndex();
                return GenerateToken(TokenType.GreaterEqual, ">=");
            }

            return GenerateToken(TokenType.Greater, ">");
        }
    }
}