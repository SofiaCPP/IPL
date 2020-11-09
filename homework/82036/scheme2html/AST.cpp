#include <map>
#include "AST.hpp"
std::map<int, std::string> colors = {
    {1000, "#cccccc"},
    {2000, "#6A9955"},
    {3000, "#569CD6"},
    {4000, "#b5cea8"},
    {5000, "#cccccc"},
    {6000, "#cccccc"},
    {7000, "#4e94ce"},
    {8000, "#4e94ce"},
    {'(', "#cccccc"},
    {')', "#cccccc"},
    {}};

int ASTNode::unique_ids_count = 0;

bool ASTNode::is_space()
{
  return m_token == " ";
}

bool ASTNode::is_newline()
{
  return m_token == "\n";
}

std::string ASTNode::get_html_tag()
{
  if (is_newline())
  {
    return "<br>";
  }
  std::string span;
  span.append("<span style=\"color:");
  span.append(colors[m_id]);
  span.append(";\">");
  if (is_space())
  {
    span.append("&nbsp;");
  }
  else
  {
    span.append(m_token);
  }
  span.append("</span>");
  return span;
}

ASTNode::ASTNode(int id, const char *token, ASTNode *parent) : unique_id(unique_ids_count++),
                                                               m_id{id},
                                                               m_token{token},
                                                               m_parent(parent) {}

void AST::erase(ASTNode* current)
{
  for (auto child : current->m_children)
  {
    erase(child);
  }
  delete current;
}

void ASTNode::print(FILE *out)
{
  if (m_token == "(")
  {
    std::string tag;
    tag.append("<button style=\"background-color:#0e639c; color:#ffffff; border: 0; border-radius: 20px; font-size: 15px\" id=");
    tag.append(std::to_string(unique_id));
    tag.append(">-</button>");
    tag.append("<span id=");
    tag.append("span" + std::to_string(unique_id));
    tag.append(" ");
    tag.append(">");
    fputs(tag.c_str(), out);
  }
  if (m_parent != nullptr)
  {
    fputs(get_html_tag().c_str(), out);
  }
  if (m_token == ")")
  {
    fputs("</span>", out);
  }
  for (auto child : m_children)
  {
    child->print(out);
  }
}

AST::AST() : m_root{new ASTNode(0, "", nullptr)},
             current_parrent{m_root},
             last_put_node{m_root} {}

void AST::print(FILE *out)
{
  m_root->print(out);
}

void AST::put_token(int id, const char *token)
{
  if (strcmp(token, "(") == 0)
  {
    current_parrent = last_put_node;
  }
  else if (strcmp(token, ")") == 0)
  {
    current_parrent = last_put_node->m_parent;
  }
  auto new_node = new ASTNode(id, token, current_parrent);
  current_parrent->m_children.push_back(new_node);
  last_put_node = new_node;
}

AST::~AST()
{
  erase(m_root);
}