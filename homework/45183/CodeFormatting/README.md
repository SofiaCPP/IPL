# Overview
The code here takes `.js` files as input and produces color formatted JavaScript code in an `.html` file.

# Prerequisites
* Windows OS
* VisualStudio 2015 or 2017

# How to use
* Download GENie from https://github.com/bkaradzic/GENie
* Execute `./genie.exe vs2017` to generate a Visual Studio 2017 project.
* Open the solution to Build and Run.
* The output files should be created in the same folder as the `.js` files and they should open in your browser.

# Known issues
* Lexer can't parse RegExp.
* Lexer can't parse Template literals.