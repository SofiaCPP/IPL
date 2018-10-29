using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace javascript_syntax_highlighter
{
    public class InputStream
    {
        private char[][] buffs;

        private int index;

        private int activeBuffer;

        private StreamReader reader;

        public InputStream(Stream stream, int buffLength = 4096)
        {
            this.Line = 0;
            this.index = 0;
            this.activeBuffer = 0;
            this.buffs = new char[2][];
            this.buffs[0] = new char[buffLength];
            this.buffs[1] = new char[buffLength];
            this.reader = new StreamReader(stream);

            ReadStream(buffs[0], buffLength);
        }

        public InputStream(string path, int buffLength = 4096)
            : this(new FileStream(path, FileMode.Open), buffLength) { }

        public int Line { get; private set; }

        public void Close()
        {
            reader.Close();
        }

        public bool IsEndOfStream()
        {
            return Peek() == 0;
        }

        public char Next()
        {
            char cur = Peek();

            if (cur == '\n')
            {
                Line++;
            }

            index++;

            if (index >= buffs[activeBuffer].Length)
            {
                SwitchBuffers();
            }
                      
            return cur;
        }

        public char Peek()
        {
            return buffs[activeBuffer][index];
        }

        private void SwitchBuffers()
        {
            index = 0;
            activeBuffer = activeBuffer == 0 ? 1 : 0;
            ReadStream(buffs[activeBuffer], buffs[activeBuffer].Length);
        }

        private int ReadStream(char[] buffer, int length)
        {
            int bytesRead = reader.Read(buffer, 0, length);

            if (bytesRead < length)
            {
                buffer[bytesRead] = (char) 0;
            }

            return bytesRead;
        }
    }
}
