using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Wizard.Misc
{
    public static class OperatorsToTokens
    {
        public static Dictionary<string, TokenType> AsMap()
        {
            return new Dictionary<string, TokenType>()
            {
                {"!" ,TokenType.Bang},
                {"=" ,TokenType.Equal},
                {"|" ,TokenType.BitwiseOr},
                {"&" ,TokenType.BitwiseAnd},
                {">" ,TokenType.Greater},
                {"<" ,TokenType.Less},
                {"+" ,TokenType.Plus},
                {"-" ,TokenType.Minus},
                {"*" ,TokenType.Star},
                {"/" ,TokenType.Division},
                {"%" ,TokenType.Modulo},

                {"!=" ,TokenType.BangEqual},
                {"/=" ,TokenType.DivideEqual},
                {"!!" ,TokenType.DoubleBang},
                {"**" ,TokenType.DoubleStar},
                {"==" ,TokenType.EqualEqual},
                {">=" ,TokenType.GreaterEqual},
                {"+=" ,TokenType.PlusEqual},
                {"++" ,TokenType.PlusPlus},
                {"&&" ,TokenType.LogicalAnd},
                {"||" ,TokenType.LogicalOr},
                {"<=" ,TokenType.LessEqual},
                {"*=" ,TokenType.StarEqual},
                {"-=" ,TokenType.MinusEqual},
                {"--" ,TokenType.MinusMinus},
                {"%=" ,TokenType.ModuloEqual},

                {"===" ,TokenType.StrictEqual},
                {"!==" ,TokenType.StrictNotEqual},
                {"**=" ,TokenType.DoubleStarEqual},
            };
        }

        public static List<TokenType> AsTokenList()
        {
            return AsMap().Values.ToList();
        }
    }
}
