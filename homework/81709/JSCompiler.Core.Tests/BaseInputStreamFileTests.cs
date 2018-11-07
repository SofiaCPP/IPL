using System;
using System.Text;
using System.Collections.Generic;
using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace JSCompiler.Core.Tests
{
    [TestClass]
    public class BaseInputStreamTests
    {
        [TestMethod]
        public void IsEndOfStream_WhenNotEnd_ReturnsFalse()
        {
            byte[] buff = new byte[3] { 32, 14, 25 };
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream testInputStream = new BaseInputStream(memStream);

            bool isEnd;
            for(int i = 0; i < 2; i++)
            {
                isEnd = testInputStream.IsEndOfStream();
                testInputStream.Next();
                Assert.IsFalse(isEnd);
            }
        }

        [TestMethod]
        public void IsEndOfStream_AtBeginningWithoutData_ReturnsTrue()
        {
            MemoryStream memStream = new MemoryStream();
            BaseInputStream testInputStream = new BaseInputStream(memStream);

            bool isEnd = testInputStream.IsEndOfStream();

            Assert.IsTrue(isEnd);
        }

        [TestMethod]
        public void IsEndOfStream_WhenEnd_ReturnsTrue()
        {
            byte[] buff = new byte[3] { 32, 14, 25 };
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream testInputStream = new BaseInputStream(memStream);

            for (int i = 0; i < 3; i++, testInputStream.Next());
            bool isEnd = testInputStream.IsEndOfStream();

            Assert.IsTrue(isEnd);
        }

        [TestMethod]
        public void Peek_AtPosition_ReturnsCorrectItemFromStream()
        {
            byte[] buff = new byte[8] { 32, 14, 25, 14, 12, 51, 25, 33 };
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream testInputStream = new BaseInputStream(memStream);
            char item;

            for (int i = 0; i < 5; i++, testInputStream.Next())
            {
                item = testInputStream.Peek();
                Assert.AreEqual((char)buff[i], item);
            }
        }

        [TestMethod]
        public void Peek_AtEnd_ReturnsEndItem()
        {
            byte[] buff = new byte[3] { 32, 14, 25 };
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream testInputStream = new BaseInputStream(memStream);

            for (int i = 0; i < 3; i++, testInputStream.Next());
            char last = testInputStream.Peek();

            Assert.AreEqual(0, last);
        }

        [TestMethod]
        [ExpectedException(typeof(EndOfStreamException))]
        public void Next_WhenEndStream_ThrowsEndOfStreamException()
        {
            byte[] buff = new byte[3] { 32, 14, 25 };
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream testInputStream = new BaseInputStream(memStream);

            for (int i = 0; i < 4; i++, testInputStream.Next());
        }

        [TestMethod]
        public void Next_WhenBuffersAreSwapped_ReturnsCorrectItem()
        {
            byte[] buff = new byte[10] { 32, 14, 25, 14, 23, 42, 16, 21, 15, 76 };
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream testInputStream = new BaseInputStream(memStream, 3);

            for (int i = 0; i < 10; i++)
            {
                char curr = testInputStream.Next();
                Assert.AreEqual((char)buff[i], curr);
            }
        }

        [TestMethod]
        public void Line_NewLinesTracking_ReturnsCorrectNumberOfLines()
        {
            byte[] buff = new byte[10] { (byte) '\n', 32, 14, 25, 14, (byte) '\n', 42, 16, (byte) '\n', (byte)'\n' };
            MemoryStream memStream = new MemoryStream(buff);
            BaseInputStream testInputStream = new BaseInputStream(memStream);

            for (int i = 0; i < 10; i++, testInputStream.Next());

            Assert.AreEqual(4, testInputStream.Line);
        }
    }
}
