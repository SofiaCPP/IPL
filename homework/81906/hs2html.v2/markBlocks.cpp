#include<iostream>
#include<fstream>
using namespace std;
int main(){
  ifstream cfile("coordinatesOfBlocks");
  ifstream hsfile("spaghetti.hs");
  ofstream result("spaghetti.hs-foldable");

  int lineN = 0;
  while(!cfile.eof()) {
    int startLine, startColumn, endLine, endColumn;
    cfile >> startLine >> startColumn >> endLine >> endColumn;

    string line;
    while(lineN < endLine && !hsfile.eof()){
      getline(hsfile, line);
      if(lineN == startLine){	
	line.insert(startColumn, "/*");
      }
      result << line << endl;;
      lineN++;
    }
    getline(hsfile, line);
    if(lineN == endLine){
      line.append("*/");
    }
    lineN++;
    result << line << endl;
  }
  return 0;
}
