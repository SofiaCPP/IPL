//
// Created by Marty Kostov on 22.10.22.
//

#include "HTMLBuilder.hpp"

HTMLBuilder::HTMLBuilder(const std::vector<Token> *tokens) : line(0), column(0), tokens(tokens) {}

std::string HTMLBuilder::getText() {
    std::string text = "<!DOCTYPE html>\n"
                       "<html lang=\"en\">\n"
                       "<head>\n"
                       "    <meta charset=\"UTF-8\">\n"
                       "    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">\n"
                       "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
                       "    <title>go2html</title>\n"
                       "    <style>\n"
                       "        .invalid    {text-decoration: underline; text-decoration-style: wavy; text-decoration-color: red;}\n"
                       "        .operator   {font-weight: bold;}\n"
                       "        .keyword    {color: #fa7;}\n"
                       "        .type       {color: #d35;}\n"
                       "        .number     {color: #09b;}\n"
                       "        .string     {color: #0a4;}\n"
                       "        .identifier {color: #bbe;}\n"
                       "        .comment    {color: #99a;}\n"
                       "        .container  {color: #fff; background-color: #222; padding: 10px; width: 800px; margin: auto;}\n"
                       "        .container pre {margin: 0;}\n"
                       "    </style>\n"
                       "</head>\n"
                       "<body>\n"
                       "    <div class=\"container\">\n"
                       "<pre>";


    for (const auto& token: *tokens) {
        text += getWord(token);
    }

    text += "</pre>\n"
            "    </div>\n"
            "</body>\n"
            "</html>";

    return text;
}

std::string HTMLBuilder::getWord(const Token &token) {
    std::string cssClass;

    switch (token.type) {
        case TokenType::Invalid:
            cssClass = "invalid";
            break;
        case TokenType::Operator:
            cssClass = "operator";
            break;
        case TokenType::Keyword:
            cssClass = "keyword";
            break;
        case TokenType::Type:
            cssClass = "type";
            break;
        case TokenType::Number:
            cssClass = "number";
            break;
        case TokenType::String:
            cssClass = "string";
            break;
        case TokenType::Identifier:
            cssClass = "identifier";
            break;
        case TokenType::Comment:
            cssClass = "comment";
            break;
        case TokenType::End:
            cssClass = "end";
            break;
    }

    auto word = "<span class=\""+cssClass+"\">"+token.value+"</span>";
    column += token.value.size();

    return word;
}
