#include "control_dropdown.hpp"
#include "tags.hpp"
#include "../lexer/lexer.hpp"
#include "../parser/parser.hpp"
#include "../utils/file_processor.hpp"

void ControlDropdown::ProcessFile(String file_name)
{
  String expression = FileProcessor::ReadFile(file_name);
  CString cstring_expression = expression.c_str();

  LexerOptions lexer_options = { .tokenize_spaces = false, .tokenize_new_lines = true, .store_all_data = true };
  Lexer lexer(lexer_options);
  Vector<Token> tokens = lexer.TokenizeExpression(cstring_expression);

  Parser parser(tokens);
  Program program = parser.ParseProgram();

  DisplayVisitor visitor;
  String view = "";

  view += VIEW_WRAPPER_BEGIN;
  view += program.Accept(visitor);
  view += VIEW_WRAPPER_END;

  String view_file_name = file_name + ".html";
  FileProcessor::WriteFile(view_file_name, view);
}
