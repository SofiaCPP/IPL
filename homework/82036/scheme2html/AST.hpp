#pragma once
#include <vector>
#include <string>
#include <cstring>

class AST;

class ASTNode
{
public:
  ASTNode(int, const char *, ASTNode *);
  ASTNode(const ASTNode&) = delete;
  ASTNode& operator=(const ASTNode&) = delete;
  void print(FILE *out);

private:
  friend class AST;

  static int unique_ids_count;
  int unique_id;

  std::vector<ASTNode *> m_children{};
  ASTNode *m_parent{};
  std::string m_token{};
  int m_id{};

  std::string get_html_tag();
  bool is_space();
  bool is_newline();
};

class AST
{
public:
  AST();
  AST(const AST&) = delete;
  AST& operator=(const AST&) = delete;
  ~AST();
  void print(FILE *);
  void put_token(int, const char *);

private:
  ASTNode *m_root;
  ASTNode *current_parrent;
  ASTNode *last_put_node;

  void erase(ASTNode*);
};