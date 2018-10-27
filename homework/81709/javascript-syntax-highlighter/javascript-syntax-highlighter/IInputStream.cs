using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace javascript_syntax_highlighter
{
    interface IInputStream
    {
        char Peek();

        char Next();

        int Line();

        bool IsEOF();
    }
}
