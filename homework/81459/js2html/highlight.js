(() => {
   const inputFile = process.argv[2];
   const outputFile = process.argv[3];

   const fs = require('fs');

   fs.readFile(inputFile, 'utf8', (err, contents) => {
      if (err) {
         throw err;
      }

      const html = generateHtml(contents);

      if (outputFile) {
         fs.writeFile(outputFile, html, 'utf8', (err) => {
            if (err) {
               throw err;
            }
         });
      } else {
         console.log(html);
      }
   });
})();

const tokens = [
   {type: 'KEYWORD', className: 'orange', regex: /^(break|case|catch|class|const|continue|debugger|default|delete|do|else|export|extends|finally|for|function|if|import|in|instanceof|new|of|return|super|switch|this|throw|try|typeof|var|void|while|with|yield)\b/},
   {type: 'WHITESPACE', className: '', regex: /^\s+/},
   {type: 'BRACKET', className: 'white', regex: /^[\[\]{}()]/},
   {type: 'PUNCTUATION', className: 'white', regex: /^[:;,.]/},
   {type: 'COMMENT', className: 'grey', regex: /^\/\/.*/},
   {type: 'OPERATOR', className: 'red', regex: /^(\+|-|\/|={1,3}|!={0,2}|&{1,2}|\|{1,2}|\*|~|\^)/},
   {type: 'BOOLEAN_LITERAL', className: 'purple', regex: /^(true|false)\b/},
   {type: 'NUMBER_LITERAL', className: 'blue', regex: /^-?\d+(?:\.\d+)?(?:e[+\-]?\d+)?/i},
   {type: 'STRING_LITERAL', className: 'green', regex: /^("(?:\\.|[^"])*"|'(?:\\.|[^'])*')/},
   {type: 'NULL', className: 'orange', regex: /^(null|undefined)\b/},
   {type: 'IDENTIFIER', className: 'white', regex: /^\w+/},
   {type: "UNKNOWN", className: 'white', regex: /^./}
];

function generateHtml(code) {
   let highlitedTokens = [];

   while (code && code.length) {
      for (const token of tokens) {
         if (token.regex.test(code)) {
            const match = token.regex.exec(code);
            highlitedTokens.push(highlightToken(match[0], token));
            code = code.substring(match[0].length);
            break;
         }
      }
   }

   return `${getHtmlHeader()}${highlitedTokens.join('')}${getHtmlFooter()}`
}

function highlightToken(token, tokenType) {
   return `<span class="${tokenType.className}">${token}</span>`
}

const getHtmlHeader = () => `<!doctype html>
<html>
    <head>
        <style>
            .orange {
                color: orange;
            }
            .white {
                color: white;
            }
            .grey {
                color: grey;
            }
            .green {
                color: green;
            }
            .purple {
                color: purple;
            }
            .red {
                color: red;
            }
            .blue {
                color: cyan;
            }
            .gold {
                color: gold;
            }
            .background {
                background: #2d2d2d;
            }
        </style>
    </head>
    <body class="background">
        <pre>
`;

const getHtmlFooter = () => `
        </pre>
    </body>
</html>
`;
