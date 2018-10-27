using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    class FileInputStream : IInputStream
    {
        private StreamReader fileStr;
        private int line;

        public FileInputStream(string path)
        {
            fileStr = new StreamReader(path, Encoding.ASCII);

        }

        public bool IsEOF()
        {
            throw new NotImplementedException();
        }

        public int Line()
        {
            return this.line;
        }

        public char Next()
        {
            throw new NotImplementedException();
        }

        public char Peek()
        {
            throw new NotImplementedException();
        }
    }
}
