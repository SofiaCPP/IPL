#include <cstdio>
#include "lexcer.h"

int main(int argc, char *argv[]) {

  if (argc != 2) {
    printf("Usage: %s <*.{c,h} file>\n", argv[0]);
    return 1; /* wrong usage */
  }
  
  auto fpIn = fopen(argv[1], "r");
  if (!fpIn) {
    return 2; /* unable to open file or inavlid file */
  }

  auto outputName = LexCer::getOutputFileName(argv[1]);
  auto fpOut = fopen(outputName.c_str(), "w");
  if (!fpOut) {
    return 2;
  }
  
  
  fputs(
        "<!doctype>\n"
        "<html>\n"
        "<head>\n"
        "    <title>LexCed</title>\n"
        "    <style>\n"
        "        .keyword {\n"
        "            color: purple;\n"
        "        }\n"
        "        .number {\n"
        "            color: darkblue;\n"
        "        }\n"
        "        .string {\n"
        "            color: green;\n"
        "        }\n"
        "        .operator {\n"
        "            font-style: bold;\n"
        "        }\n"
        "        .invalid {\n"
        "            text-decoration:       underline;\n"
        "            text-decoration-color: red;\n"
        "            text-decoration-style: wavy;\n"
        "        }\n"
        "        .typename {\n"
        "            color: blue;\n"
        "        }\n"
        "        .preproc {\n"
        "            color: purple;\n"
        "        }\n"
        "        .identifier {\n"
        "            color: black;\n"
        "        }\n"
        "        .string {\n"
        "            color: brown;\n"
        "        }\n"
        "        .comment {\n"
        "            color: green;\n"
        "        }\n"
        "    </style>\n"
        "</head>\n"
        "<body>\n"
        "<pre class=\"code\">\n",
        fpOut
        );
  LexCer::lexCit(fpIn, fpOut);
  fputs("</pre></body></html>", fpOut);

  printf("Highlighted text written in %s!\n", outputName.c_str());
  
  return 0;
}
