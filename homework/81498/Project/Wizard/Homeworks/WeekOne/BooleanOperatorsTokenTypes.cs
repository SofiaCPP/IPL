using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Wizard.Homeworks.WeekOne
{
    public static class BooleanOperatorsTokenTypes
    {
        public static List<TokenType> AsList()
        {
            return new List<TokenType>()
            {
                TokenType.Bang,
                TokenType.BangEqual,
                TokenType.LogicalOr,
                TokenType.LogicalAnd,
                TokenType.EqualEqual,
                TokenType.StrictEqual,
                TokenType.StrictNotEqual,
                TokenType.Greater,
                TokenType.GreaterEqual,
                TokenType.Less,
                TokenType.LessEqual
            };
        }
    }
}
