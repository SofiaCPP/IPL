using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JSCompiler.Core
{
    public interface IInputStream
    {
        void Close();
        bool IsEndOfStream();
        char Next();
        char Peek();
        int Line { get; }
    }
}
