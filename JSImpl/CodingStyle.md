C++ coding style guide


Spacing
Do not place spaces around unary operators.

Right:
i++;
Wrong:
i ++;
Do place spaces around binary and ternary operators.

Right:
y = m * x + b;
f(a, b);
c = a | b;
return condition ? 1 : 0;
Wrong:
y=m*x+b;
f(a,b);
c = a|b;
return condition ? 1:0;
Place spaces around the colon in a range-based for loop.

Right:
Vector<PluginModuleInfo> plugins;
for (auto& plugin : plugins)
    registerPlugin(plugin);
Wrong:
Vector<PluginModuleInfo> plugins;
for (auto& plugin: plugins)
    registerPlugin(plugin);
Do not place spaces before comma and semicolon.

Right:
for (int i = 0; i < 10; ++i)
    doSomething();

f(a, b);
Wrong:
for (int i = 0 ; i < 10 ; ++i)
    doSomething();

f(a , b) ;
Place spaces between control statements and their parentheses.

Right:
if (condition)
    doIt();
Wrong:
if(condition)
    doIt();
Do not place spaces between a function and its parentheses, or between a parenthesis and its content.

Right:
f(a, b);
Wrong:
f (a, b);
f( a, b );
When initializing an object, place a space before the leading brace as well as between the braces and their content.

Right:
Foo foo { bar };
Wrong:
Foo foo{ bar };
Foo foo {bar};


Line breaking
Each statement should get its own line.

Right:
x++;
y++;
if (condition)
    doIt();
Wrong:
x++; y++;
if (condition) doIt();
An else statement should go on the next line as a preceding close brace if one is present.

Right:
if (condition)
{
    ...
}
else
{
    ...
}

if (condition)
    doSomething();
else
    doSomethingElse();

if (condition)
    doSomething();
else
{
    ...
}
Wrong:
if (condition) {
    ...
}
else {
    ...
}


Braces

Place braces on the line after if/for/else/ function definition etc.

Right:
int main()
{
    ...
}

class MyClass
{
    ...
};

for (int i = 0; i < 10; ++i)
{
    ...
}

namespace WebCore 
{
    ...
}
Wrong:

int main() {
    ...
}

class MyClass {
    ...
};

for (int i = 0; i < 10; ++i) {
    ...
}

namespace WebCore {
    ...
}

One-line control clauses should not use braces unless comments are included or a single statement spans multiple lines.

Names

Use CamelCase. Capitalize the first letter, including all letters in an acronym, in a class, function, struct, protocol, or namespace name. 
Lower-case the first letter, including all letters in a local variable.
Use prefix 'm_' for class members and CamelCase without prefix for struct members.
Use CamelCase for static class and struct members.

Right:
struct Data;
size_t bufferSize;
class HTMLDocument;
String mimeType();

class A
{
    ...
    int m_Count;
}

struct A
{
    ...
    int m_Count;
}
Wrong:
struct data;
size_t buffer_size;
class HtmlDocument;
String MIMEType();

class A
{
    ...
    int count;
}

struct A
{
    ...
    int Count;
}


Data members in C++ classes should be private.

Precede boolean values with words like “is” and “did”.

Right:
bool isValid;
bool didSendData;
Wrong:
bool valid;
bool sentData;

Constructors for C++ classes should initialize all of their members using C++ initializer syntax. Each member (and superclass) should be indented on a separate line, with the colon or comma preceding the member on that line.

Right:
MyClass::MyClass(Document* document)
    : MySuperClass()
    , m_myMember(0)
    , m_document(document)
{
}

MyOtherClass::MyOtherClass()
    : MySuperClass()
{
}
Wrong:
MyClass::MyClass(Document* document) : MySuperClass()
{
    m_myMember = 0;
    m_document = document;
}

MyOtherClass::MyOtherClass() : MySuperClass() {}

Using namespace - do not use “using” never!

Pointers and memory managment

Use smart pointers always for example IPLSharedPtr.
Never use raw new. You can use IPLMakeSharePtr instead.

Containers

You can use everything from CommonTypes.h for example IPLVector,
IPLString, IPLUnorderedMap if you need something more think twice
and ask Marto or Mitko.
