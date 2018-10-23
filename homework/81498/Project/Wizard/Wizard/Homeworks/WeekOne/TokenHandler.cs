using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Wizard.Homeworks.WeekOne;
using Wizard.Misc;

namespace Wizard.Homeworks
{   // private or property?
    public class TokenHandler
    {
        private List<Token> _tokens;
        private List<TokenType> _operatorsAsTokenList;
        private List<TokenType> _keywordsAsTokenTypes;

        public string FileDestination { get; set; }

        public TokenHandler(List<Token> tokens, string fileDestionation)
        {
            _tokens = tokens;
            FileDestination = fileDestionation;

            _operatorsAsTokenList = OperatorsToTokens.AsTokenList();
            _keywordsAsTokenTypes = KeywordsToTokens.AsTokenList();
        }

        public void PrintTokens()
        {
            foreach (var token in _tokens)
            {
                Console.WriteLine(token.ToString());
            }
        }

        public void PrettyPrint(string fileDestination)
        {
            using (var writer = new StreamWriter(fileDestination))
            {
                foreach (var token in _tokens)
                {
                    string coloredLexeme = "";
                    if (token.Type is TokenType.True ||
                       token.Type is TokenType.False)
                    {
                        coloredLexeme = "<span><font color=\"" + Color.ForBoolean + "\">" + token.Lexeme + "</font></span>";
                        writer.Write(coloredLexeme);
                        continue;
                    }

                    if (_operatorsAsTokenList.Contains(token.Type))
                    {
                        coloredLexeme = "<span><font color=\"" + Color.ForBooleanOperator + "\"><b>" + token.Lexeme + "</b></font></span>";
                        writer.Write(coloredLexeme); // more
                        continue;
                    }

                    if (_keywordsAsTokenTypes.Contains(token.Type))
                    {
                        coloredLexeme = "<span><font color=\"" + Color.ForKeyword + "\">" + token.Lexeme + "</font></span>";
                        writer.Write(coloredLexeme); // more
                        continue;
                    }

                    switch (token.Type)
                    {
                        case TokenType.Whitespace:
                            writer.Write("&nbsp;");
                            break;
                        case TokenType.Newline: writer.Write("<br>"); break;
                        case TokenType.String: // more
                            coloredLexeme = "<span><font color=\"" + Color.ForString + "\">\"" + token.Lexeme + "\"</font></span>";
                            writer.Write(coloredLexeme);
                            break;
                        case TokenType.Number:
                            coloredLexeme = "<span><font color=\"" + Color.ForNumber + "\">" + token.Lexeme + "</font></span>";
                            writer.Write(coloredLexeme);
                            break;
                        case TokenType.Identifier:
                            coloredLexeme = "<span><font color=\"" + Color.ForIdentifier + "\">" + token.Lexeme + "</font></span>";
                            writer.Write(coloredLexeme);
                            break;
                        default:
                            coloredLexeme = "<span><font color=\"" + Color.ForDefault + "\">" + token.Lexeme + "</font></span>";
                            writer.Write(coloredLexeme);
                            break;
                    }
                }
            }
        }
    }
}
