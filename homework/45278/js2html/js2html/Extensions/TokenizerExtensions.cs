using System;
using System.Collections.Generic;
using System.Text;

namespace js2html.Extensions
{
    public static class TokenizerExtensions
    {
        public static IEnumerable<string> ParseHtml(this Tokenizer tokenizer)
        {
            var whitespace = string.Empty;
            foreach(var token in tokenizer.Tokens)
            {
                if (token.TokenType == Models.TokenType.Number)
                {
                    var result = string.Format("<span class=\"number\">{0}{1}</span>", whitespace, token.Lexeme);
                    whitespace = string.Empty;
                    yield return result;
                }
                else if(token.TokenType == Models.TokenType.String)
                {
                    var result = string.Format("<span class=\"string\" style=\"color:indianred\">{0}{1}</span>", whitespace, token.Lexeme);
                    whitespace = string.Empty;
                    yield return result;
                }
                else if (tokenizer.KeywordsTable.ContainsKey(token.Lexeme))
                {
                    var result = string.Format("<span class=\"keyword\" style=\"color:blue\">{0}{1}</span>", whitespace, token.Lexeme);
                    whitespace = string.Empty;
                    yield return result;
                }
                else if (token.TokenType == Models.TokenType.Identifier)
                {
                    var result = string.Format("<span class=\"identifier\">{0}{1}</span>", whitespace, token.Lexeme);
                    whitespace = string.Empty;
                    yield return result;
                }
                else if (token.TokenType == Models.TokenType.Whitespace)
                {
                    if(whitespace != string.Empty)
                    {
                        var tempWhitespace = token.Lexeme.Replace(" ", "&nbsp;").Replace("\r\n", "<br>").Replace("\n","<br>").Replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;");
                        whitespace = whitespace + tempWhitespace;
                    }
                    else
                        whitespace = token.Lexeme.Replace(" ", "&nbsp;").Replace("\r\n", "<br>").Replace("\n", "<br>").Replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;");
                }
                else if (token.TokenType == Models.TokenType.Eof)
                    yield return whitespace;
                else
                {
                    //string result = "";
                    //if (whitespace.Contains("<br>")) 
                    //{
                    //    result = string.Format("{0}<span class=\"operator\">{1}</span>", whitespace, token.Lexeme);
                    //}
                    //else
                    var result = string.Format("<span class=\"operator\">{0}{1}</span>", whitespace, token.Lexeme);
                    whitespace = string.Empty;
                    yield return result;
                }
            }
        }
        
        //static private string BuildWhitespace(string lexeme)
        //{ 
        //    StringBuilder result = new StringBuilder();
        //    foreach(var c in lexeme)
        //    {
        //        if (c != '\n')
        //            result.Append("&nbsp;");
        //        else
        //            result.Append("<br>");
        //    }
            
        //    return result.ToString();
        //}
    }
}
