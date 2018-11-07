#include <cstdio>
#include <cstring>

#include "cyclomatic.h"
#include "html_utils.h"
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

  auto outputName = HTMLUtils::getOutputFileName(argv[1]);
  auto fpOut = fopen(outputName.c_str(), "w");
  if (!fpOut) {
    return 2;
  }
  
  
  fputs(
        "<!DOCTYPE html>\n"
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
        "        .complexity-level-one {\n"
        "            background-color: PaleGreen;\n"
        "        }\n"
        "        .complexity-level-two {\n"
        "            background-color: DarkGreen;\n"
        "        }\n"
        "        .complexity-level-three {\n"
        "            background-color: SaddleBrown;\n"
        "        }\n"
        "        .complexity-level-four {\n"
        "            background-color: Brown;\n"
        "        }\n"
        "        .complexity-level-five {\n"
        "            background-color: Maroon;\n"
        "        }\n"
        "    </style>\n"
        "</head>\n"
        "<body onload=\"toggleComplexity()\">\n"
        "<pre class=\"code\">\n",
        fpOut
        );
  ParCer::outputWithCyclomaticComplexity(fpIn, fpOut);
  fputs("</pre>\n"
        "<script>\n"
        "function toggleComplexity() {\n"
        "  var funcs = document.getElementsByClassName(\"func\");"
        "  if (funcs.length == 0) return;\n"
        "  var len = funcs.length;\n"
        "  for (var i = 0; i < len; ++i) {\n"
        "    var f = document.getElementsByClassName(\"func\")[0];\n"
        "    var complexity = f.innerHTML.match(\"(<!-- )([0-9]+)( -->)\")[2];\n"
        "    if (complexity < 3)\n"
        "        f.className = \"complexity-level-one\";\n"
        "    else if (complexity < 7)\n"
        "        f.className = \"complexity-level-two\";\n"
        "    else if (complexity < 15)\n"
        "        f.className = \"complexity-level-three\";\n"
        "    else if (complexity < 20)\n"
        "        f.className = \"complexity-level-four\";\n"
        "    else\n"
        "        f.className = \"complexity-level-five\";\n"
        "  }\n"
        "}\n",
        fpOut);
  fputs("</script></body></html>\n", fpOut);

  printf("Highlighted text written in %s!\n", outputName.c_str());
  
  return 0;
}
