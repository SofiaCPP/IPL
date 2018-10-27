using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    class FileInputStream : InputStream
    {
        private StreamReader reader;
        private int index;

        public FileInputStream(string path)
        {
            reader = new StreamReader(path, Encoding.ASCII);
            index = 0;
        }

        public override void Close()
        {
            reader.Close();
        }

        public override bool IsEndOfStream()
        {
            return reader.EndOfStream;
        }

        protected override int ReadStream(char[] buffer, int length)
        {
            int bytesRead = reader.Read(buffer, index, length);

            if (bytesRead == 0)
            {
                this.Close();
            }

            index += length;

            return bytesRead;
        }
    }
}
