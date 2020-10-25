flex lexer.lex
g++ -c lex.yy.c    -o lexer.o   -std=c99
g++ -c handler.cpp -o handler.o -std=c++11
g++ lexer.o handler.o -o lexer.exe
echo "Compile complete"
pause
lexer.exe < example_code.cpp > example.html
echo "Lex Complete"
pause
example.html