#include <stdio.h>

char mem[10000], src[1000];

// if src[i] == '[' or ']', match[src[i]] is the index to the matching bracket
int match[1000 * sizeof(char*)]; 

int ptr = 50000;

int main(int argc, const char** argv) {
	FILE* f;
	long src_size;

	if (argc < 2 || !(f = fopen(argv[1], "r")) 
	             || (src_size = fread(src, 1, 100000, f)) < 0) 
	{
		fprintf(stderr, "failed to open file");
		return 1;
	}

	int stack[100], sp = 0;
	for (int i = 0; i < src_size; i++) {
		if (src[i] == '[') {
			stack[sp++] = i;
		}
		else if (src[i] == ']') {
			match[i] = stack[--sp];
			match[match[i]] = i;
		}
	}

	for (int ip = 0; ip < src_size; ip++) {
		switch (src[ip]) {
			case '>': ptr++;                break;
			case '<': ptr--;                break;
			case '+': mem[ptr]++;           break;
			case '-': mem[ptr]--;           break;
			case '.': putchar(mem[ptr]);    break;
			case ',': mem[ptr] = getchar(); break;
			case '[':
				if (!mem[ptr])
					ip = match[ip];
				break;
			case ']': 
				if (mem[ptr])
					ip = match[ip];
				break;
		}
	}

	putchar('\n');
	return 0;
}
