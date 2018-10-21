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
        private List<TokenType> _booleanOperatorAsTokenTypes;
        private List<TokenType> _keywordsAsTokenTypes;

        public string FileDestination { get; set; }

        public TokenHandler(List<Token> tokens, string fileDestionation)
        {
            _tokens = tokens;
            FileDestination = fileDestionation;

            _booleanOperatorAsTokenTypes = BooleanOperatorsTokenTypes.AsList();
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
                    if (token.Type is TokenType.True ||
                       token.Type is TokenType.False)
                    {
                        writer.Write("<span><font color=\"#FF0000\">" + token.Lexeme + "</font></span>"); // red
                        continue;
                    }

                    if (_booleanOperatorAsTokenTypes.Contains(token.Type))
                    {
                        writer.Write("<span><font color=\"#444444\"><b>" + token.Lexeme + "</b></font></span>"); // more
                        continue;
                    }

                    if (_keywordsAsTokenTypes.Contains(token.Type))
                    {
                        writer.Write("<span><font color=\"#6062AC\">" + token.Lexeme + "</font></span>"); // more
                        continue;
                    }

                    switch (token.Type)
                    {
                        case TokenType.Whitespace:
                            writer.Write("&nbsp;");
                            break;
                        case TokenType.Newline: writer.Write("<br>"); break;
                        case TokenType.String: // more
                            writer.Write("<span><font color=\"#4E409A\">\"" + token.Lexeme + "\"</font></span>");
                            break;
                        case TokenType.Number:
                            writer.Write("<span><font color=\"#FEC051\">" + token.Lexeme + "</font></span>");
                            break;
                        case TokenType.Identifier:
                            writer.Write("<span><font color=\"#8BBCDA\">" + token.Lexeme + "</font></span>");
                            break;
                        default:
                            writer.Write("<span><font color=\"#000000\">" + token.Lexeme + "</font></span>");
                            break;
                    }
                }
            }
        }
    }
}
