using System;
using System.Collections;
using System.Collections.Generic;

namespace javascript_syntax_highlighter
{
    abstract class InputStream
    {
        private char[][] buffs;

        private int index;

        private int activeBuffer;

        public InputStream(int buffLength = 4096)
        {
            this.Line = 0;
            this.index = 0;
            this.activeBuffer = 0;
            this.buffs = new char[2][];

            this.buffs[0] = new char[buffLength];
            this.buffs[1] = new char[buffLength];
        }

        public int Line { get; private set; }

        public abstract bool IsEndOfStream();

        public abstract void Close();

        public char Next()
        {
            index++;

            if (index >= buffs[activeBuffer].Length)
            {
                SwitchBuffers();
            }

            char cur = Current();

            if(cur == '\n')
            {
                Line++;
            }

            return cur;
        }

        public char Current()
        {
            return buffs[activeBuffer][index];
        }

        public void SkipWhiteSpaces()
        {
            char cur = Current();
            while (cur == ' ' || cur == '\t' || cur == '\n')
            {
                cur = Next();
            }
        }

        protected abstract int ReadStream(char[] buffer, int length);

        private void SwitchBuffers()
        {
            index = 0;
            activeBuffer = activeBuffer == 0 ? activeBuffer = 1 : activeBuffer = 0;
            ReadStream(buffs[activeBuffer], buffs[activeBuffer].Length);
        }

    }
}
