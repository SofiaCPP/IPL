#include <stdio.h>
#include <vector>
char cells[1024] = {0};
char* ptr = cells;
FILE* program;


void read_while_loop(){
  int position = ftell(program);
  //printf("%d\n", position);
  char c;

  while((c = fgetc(program)) != ']'){
    if(c == '['){
      read_while_loop();
    }

    switch (c) {
      case '+':
        (*ptr)++;
        break;
      case '-':
        (*ptr)--;
        break;
      case '>':
        ptr++;
        break;
      case '<':
        ptr--;
        break;
      case '.':
        putchar(*ptr);
        break;
      case ',':
        *ptr = getchar();
        break;
    }
  }

  if((int)*ptr != 0){
    fseek(program, position, SEEK_SET);
    read_while_loop();
  }
}

int main(){
  program = fopen("input.file", "r");
  char c;

  while((c = fgetc(program)) != EOF ){

    switch (c) {
      case '+':
        (*ptr)++;
        break;
      case '-':
        (*ptr)--;
        break;
      case '>':
        ptr++;
        break;
      case '<':
        ptr--;
        break;
      case '.':
        putchar(*ptr);
        break;
      case ',':
        *ptr = getchar();
        break;
      case '[':
        read_while_loop();
        break;
    }
  }
  // printf("%c\n", cells[0]);
}
