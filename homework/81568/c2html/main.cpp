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
        "<body>\n"
        "<button style=\"font-size:100px;\" type=\"button\" onclick=\"toggleComplexity()\">"
        "Toggle Complexity</button>\n"
        "<pre class=\"code\">\n",
        fpOut
        );
  unsigned numOfFunctions = ParCer::outputWithCyclomaticComplexity(fpIn, fpOut);
  fputs("</pre>\n", fpOut);
  // the script to toggle complexity of functions needs numOfFunctions to know how long to go
  // switching it on/off could be done with a little bit more hacking but that's enough I think
  fputs("<script>"
        "function toggleComplexity() {\n"
        "alert(\"yey\");\n"
        "if (document.getElementsByClassName(\"a1\").length == 0) return;\n"
        "for (var i = 1; i <= ",
        fpOut);
  char num[100];
  sprintf(num, "%u", numOfFunctions);
  fputs(num, fpOut);
  fputs("; ++i) {\n"
        "var class_name = \"a\" + i;\n"
        "var d = document.getElementsByClassName(class_name)[0];\n"
        "var complexity = d.innerHTML.match(\"(<!-- )([0-9]+)( -->)\")[2];\n"
        "if (complexity < 3)\n"
        "    d.className = \"complexity-level-one\";\n"
        "else if (complexity < 7)\n"
        "    d.className = \"complexity-level-two\";\n"
        "else if (complexity < 15)\n"
        "    d.className = \"complexity-level-three\";\n"
        "else if (complexity < 20)\n"
        "    d.className = \"complexity-level-four\";\n"
        "else\n"
        "    d.className = \"complexity-level-five\";\n"
        "}\n" // for
        "}\n", // toogleComplexity
        fpOut);
  fputs("</script></body></html>\n", fpOut);

  printf("Highlighted text written in %s!\n", outputName.c_str());
  
  return 0;
}
