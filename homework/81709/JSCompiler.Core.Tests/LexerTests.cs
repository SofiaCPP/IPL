using Microsoft.VisualStudio.TestTools.UnitTesting;
using JSCompiler.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Threading.Tasks;

namespace JSCompiler.Core.Tests
{
    [TestClass()]
    public class LexerTests
    {

        [TestMethod]
        public void Tokenize_Tab_ReturnsTabToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes("\t ");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());

            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Tab),
                new Token(TokenType.Whitespace),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_SkipCarriageReturn_ReturnsNextToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes("\r\r");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());

            List<Token> expected = new List<Token>
            {
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_WhiteSpace_ReturnsWhitespaceToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes("  ");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());

            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new Token(TokenType.Whitespace),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_MultilineComment_ReturnsCommentToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes("/* \n\n**\n this is comment */**");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new WordToken("/* \n\n**\n this is comment */", TokenType.Comment),
                new Token(TokenType.Star),
                new Token(TokenType.Star),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_MultilineCommentUnclosed_ReturnsCommentTokenAndCallsOnError()
        {
            byte[] buff = Encoding.ASCII.GetBytes("/* \n\n**\n this is comment ** ////gvj");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.AreEqual(Lexer.UNCLOSED_MULTILINE_COMMENT, cause));
            List<Token> expected = new List<Token>
            {
                new WordToken("/* \n\n**\n this is comment ** ////gvj", TokenType.Comment),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_SingleLineCommentStopOnCarriageReturn_ReturnsCommentToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes("// some comment here \r\n*");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new WordToken("// some comment here ", TokenType.Comment),
                new Token(TokenType.NewLine),
                new Token(TokenType.Star),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_SingleLineCommentStopOnNewLine_ReturnsCommentToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes("// some comment here \n\n*");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new WordToken("// some comment here ", TokenType.Comment),
                new Token(TokenType.NewLine),
                new Token(TokenType.NewLine),
                new Token(TokenType.Star),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_SingleLineCommentStopOnEndStream_ReturnsCommentToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes("// some comment here  ");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new WordToken("// some comment here  ", TokenType.Comment),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }
        
        [TestMethod]
        public void Tokenize_Operators_ReturnsOperatorTokens()
        {
            byte[] buff = Encoding.ASCII.GetBytes("(){},.-+;*/%~&^|?:[]=!><\\=====!==!=>=<=--++>><<&&||*=/=%=+=-=<<=>>=&=^=|=");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.LeftParen),
                new Token(TokenType.RightParen),
                new Token(TokenType.LeftBrace),
                new Token(TokenType.RightBrace),
                new Token(TokenType.Comma),
                new Token(TokenType.Dot),
                new Token(TokenType.Minus),
                new Token(TokenType.Plus),
                new Token(TokenType.Semicolon),
                new Token(TokenType.Star),
                new Token(TokenType.Division),
                new Token(TokenType.Modulo),
                new Token(TokenType.BitwiseNot),
                new Token(TokenType.BitwiseAnd),
                new Token(TokenType.BitwiseXor),
                new Token(TokenType.BitwiseOr),
                new Token(TokenType.QuestionMark),
                new Token(TokenType.Colon),
                new Token(TokenType.LeftSquareBracket),
                new Token(TokenType.RightSquareBracket),
                new Token(TokenType.Equal),
                new Token(TokenType.Bang),
                new Token(TokenType.Greater),
                new Token(TokenType.Less),
                new Token(TokenType.Backslash),

                new WordToken("===", TokenType.StrictEqual),
                new WordToken("==", TokenType.EqualEqual),
                new WordToken("!==", TokenType.StrictNotEqual),
                new WordToken("!=", TokenType.NotEqual),
                new WordToken(">=", TokenType.GreaterEqual),
                new WordToken("<=", TokenType.LessEqual),
                new WordToken("--", TokenType.MinusMinus),
                new WordToken("++", TokenType.PlusPlus),
                new WordToken(">>", TokenType.RightShift),
                new WordToken("<<", TokenType.LeftShift),
                new WordToken("&&", TokenType.LogicalAnd),
                new WordToken("||", TokenType.LogicalOr),
                new WordToken("*=", TokenType.StarEqual),
                new WordToken("/=", TokenType.DivideEqual),
                new WordToken("%=", TokenType.ModuloEqual),
                new WordToken("+=", TokenType.PlusEqual),
                new WordToken("-=", TokenType.MinusEqual),
                new WordToken("<<=", TokenType.LeftShiftEqual),
                new WordToken(">>=", TokenType.RightShiftEqual),
                new WordToken("&=", TokenType.BitwiseAndEqual),
                new WordToken("^=", TokenType.BitwiseXorEqual),
                new WordToken("|=", TokenType.BitwiseOrEqual),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringDoubleQuotes_ReturnsStringToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" \"This is string\" ");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("\"This is string\"", TokenType.String),
                new Token(TokenType.Whitespace),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringDoubleQuotesUnclosed_ReturnsStringTokenAndCallsOnError()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" \"This is string ");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.AreEqual(cause, Lexer.UNCLOSED_STRING));
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("\"This is string ", TokenType.Error),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringDoubleQuotesUnclosedNewLine_ReturnsStringTokenAndCallsOnError()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" \"This is string \n \" \r\n");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.AreEqual(cause, Lexer.UNCLOSED_STRING_NEWLINE));
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("\"This is string ", TokenType.Error),
                new Token(TokenType.NewLine),
                new Token(TokenType.Whitespace),
                new WordToken("\" ", TokenType.Error),
                new Token(TokenType.NewLine),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringDoubleQuotesEscapeNewLine_ReturnsStringToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" \"This is string \\\r\n \\\n +\"");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("\"This is string \\\r\n \\\n +\"", TokenType.String),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringDoubleQuotesEscapeDoubleQuote_ReturnsStringToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" \"This is string \\\"inner\\\" \"");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("\"This is string \\\"inner\\\" \"", TokenType.String),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringDoubleQuotesEscapeBackslash_ReturnsStringToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" \"This is string \\\\ or this \\ \"");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("\"This is string \\\\ or this \\ \"", TokenType.String),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringDoubleQuotesSingleQuotesCombination_ReturnsStringToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" \"This is string 'inner'\"");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("\"This is string 'inner'\"", TokenType.String),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringSingleQuotes_ReturnsStringToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" 'This is string' ");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("'This is string'", TokenType.String),
                new Token(TokenType.Whitespace),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringSingleQuotesUnclosed_ReturnsStringTokenAndCallsOnError()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" 'This is string ");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.AreEqual(cause, Lexer.UNCLOSED_STRING));
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("'This is string ", TokenType.Error),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringSingleQuotesUnclosedNewLine_ReturnsStringTokenAndCallsOnError()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" 'This is string \n ' \r\n");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.AreEqual(cause, Lexer.UNCLOSED_STRING_NEWLINE));
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("'This is string ", TokenType.Error),
                new Token(TokenType.NewLine),
                new Token(TokenType.Whitespace),
                new WordToken("' ", TokenType.Error),
                new Token(TokenType.NewLine),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringSingleQuotesEscapeNewLine_ReturnsStringToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" 'This is string \\\r\n \\\n +'");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("'This is string \\\r\n \\\n +'", TokenType.String),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringSingleQuotesEscapeDoubleQuote_ReturnsStringToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" 'This is string \\'inner\\' '");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("'This is string \\'inner\\' '", TokenType.String),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringSingleQuotesEscapeBackslash_ReturnsStringToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" 'This is string \\\\ or this \\ '");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("'This is string \\\\ or this \\ '", TokenType.String),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_StringSingleQuotesSingleQuotesCombination_ReturnsStringToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" 'This is string \"inner\"'");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("'This is string \"inner\"'", TokenType.String),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_Number_ReturnsNumberToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" 1234");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new NumToken(1234),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_ReservedOrIdentifier_ReturnsReservedToken()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" Object var someIdentifier132 ");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.Fail());
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("Object", TokenType.Object),
                new Token(TokenType.Whitespace),
                new WordToken("var", TokenType.Var),
                new Token(TokenType.Whitespace),
                new WordToken("someIdentifier132", TokenType.Identifier),
                new Token(TokenType.Whitespace),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }

        [TestMethod]
        public void Tokenize_UnresolvedSymbol_ReturnsUnidentifiedTokenAndCallsOnError()
        {
            byte[] buff = Encoding.ASCII.GetBytes(" Object `");
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream inputStream = new BaseInputStream(memStream);
            Lexer lexer = new Lexer(inputStream);

            List<Token> tokens = lexer.Tokenize((line, cause) => Assert.AreEqual(cause, Lexer.UNIDENTIFIED_SYMBOL));
            List<Token> expected = new List<Token>
            {
                new Token(TokenType.Whitespace),
                new WordToken("Object", TokenType.Object),
                new Token(TokenType.Whitespace),
                new WordToken("`", TokenType.Error),
                new Token(TokenType.EOF)
            };

            CollectionAssert.AreEqual(tokens, expected);
        }
    }
}