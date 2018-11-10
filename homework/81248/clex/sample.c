float xs = 0.5e123213f;
float xss = .5f;

typedef struct word_struct {
	const char* begin;
	int length;
} word;

const word get_word(const char* begin){
	const char* current = begin;
	short len = 0;
	while( (*current != ' ') && (*current != 0) && (*current != '\n') ) {
				current++;
		len++;
	}
	word res = {
		.begin = begin,
		.length = len
	};
	return res;
}

void print_op(const char symb, FILE* file_write) {
	fprintf(file_write, "%c", symb);
}

const int single_char(char *line, int *count, FILE* file_write) {
	char letter = *(line + *count);
	switch(letter) {
			case ';': print_op(';', file_write); return 1;
			case '{': print_op('{', file_write); return 1;
			case '}': print_op('}', file_write); return 1;
			case ',': print_op(',', file_write); return 1;
			case ':': print_op(':', file_write); return 1;
			case '=': print_op('=', file_write); return 1;
			case '(': print_op('(', file_write); return 1;
			case ')': print_op(')', file_write); return 1;
			case '[': print_op('[', file_write); return 1;
			case ']': print_op(']', file_write); return 1;
			case '.': print_op('.', file_write); return 1;
			case '&': print_op('&', file_write); return 1;
			case '!': print_op('!', file_write); return 1;
			case '~': print_op('~', file_write); return 1;
			case '-': print_op('-', file_write); return 1;
			case '+': print_op('+', file_write); return 1;
			case '*': print_op('*', file_write); return 1;
			case '/': print_op('/', file_write); return 1;
			case '%': print_op('%', file_write); return 1;
			case '<': print_op('<', file_write); return 1;
			case '>': print_op('>', file_write); return 1;
			case '^': print_op('^', file_write); return 1;
			case '|': print_op('|', file_write); return 1;
			case '?': print_op('?', file_write); return 1;
			default: print_op('n', file_write); return 1;

				}
}

const int number(char *line, int *count, FILE* file_write){

}

int main (void) {

	FILE *file_read;
	FILE *file_write;

	file_read = fopen("example_code.c", "r");
	file_write = fopen("output", "w");

	char *line = NULL;
	size_t len = 0;
	ssize_t nread;
	printf("------------------------------\n\tWRITING\n------------------------------\n");
	while((nread = getline(&line, &len, file_read)) != -1) {
		int counter = 0;
		while(*(line + counter) != 0 ) {
			if(*(line+counter) == '\n') {
				print_op('\n', file_write);
			}
						if(single_char(line, &counter, file_write)) {
				counter++;
				continue;
			}
						if(number(line, &counter, file_write)) {
				continue;
			}



					}
	}


}
