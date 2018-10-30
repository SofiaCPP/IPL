#ifndef SAMPLE_CODE_C
#define SAMPLE_CODE_C

#define ONE_LINE_MACRO 5

#include <stdio.h>

int bitcount(long long x) {
  int cnt = 0;
  while (x) {
    ++cnt;
    x &= (x-1);
  }
  return cnt;
}

void shift_to_pos(int *num, int p, int n) {
  *num = p > n ? *num << (p - n + 1) : *num >> (n - p + 1);
}

int setbits(x, p, n, y) {
  int mask = (1 << n) - 1;
  int to_set = y & mask;
  shift_to_pos(&mask, p, n);
  shift_to_pos(&to_set, p, n);
  return (x & ~mask) | to_set;
}

unsigned invert(unsigned x, unsigned p, unsigned n) {
  ++p; // in case p < n
  while(n-- && p--) {
    if (!(x & (1 << n))) {
      x |= (1 << n);
    } else {
      x ^= (1 << n);
    }
  }
  return x;
}

int rightrot(long long x, int n) {
  int bitcnt = bitcount(x);
  x <<= n;
  return (x & ( (1 << (bitcnt)) - 1) ) | (x & ~( (1 << bitcnt) - 1 )) >> bitcnt;     
}

char num[32];

void to_binary(unsigned x) {
  int i = 31;
  num[32] = '\0';
  while (x) {
    if (x & 1) num[i] = '1';
    else num[i] = '0';
    --i;
    x >>= 1;
  }
  for (; i >= 0; --i) num[i] = '0';
  for (; i < 32; ++i) {
    printf("%c%s", num[i], !((i+1) % 4) ? " " : "");
  }
}

void switch_why_not(void) {
  char c = 'X';
  switch(c) {
  case 'x':
    printf("hex\n");
    break;
  case 'X':
    printf("why break stuff\n");
  case 1:
  case 2:
  case 3:
  case 4:
  case 5:
  case 6:    
  default:
    break;
  }

  return;
}

main() {
  long long x = 0b110110;
  printf("rightrot:\n");
  to_binary(x);
  printf("\n");
  to_binary(rightrot(x, 3));
  printf("\n");
  
  unsigned i = 0b101010;
  printf("invert:\n");
  to_binary(i);
  printf("\n");
  to_binary(invert(i, 4, 4));
  printf("\n");
  
  long long a = 0b111001010011;
  long long b = 0b11111010;
  printf("bits of: "); to_binary(a); printf("\n");
  printf("bits to: "); to_binary(b); printf("\n");
  to_binary(setbits(a, 6, 4, b));


  int testInt = 12345678;
  float testFloat = 1234.5678f;
  double testDouble = 1234.5678;
  
  return 0;
}

#endif /* SAMPLE_CODE_C */
