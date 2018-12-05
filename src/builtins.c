#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAXFLOATSIZE 50

char* str_of_int(int x) {
  int length = snprintf( NULL, 0, "%d", x );
  char* str = malloc( length + 1 );
  snprintf( str, length + 1, "%d", x );
  return str;
}

char* str_of_bool(int x) {
  int length;  
  char* str;
  if (x) {
    str = malloc(sizeof("true") + 1);
    snprintf(str, sizeof("true") + 1, "%s", "true");
  } else {
    str = malloc(sizeof("false") + 1);
    snprintf(str, sizeof("false") + 1, "%s", "false");
  }
  return str;
}

//BUT TECHNICALLY IT"S A C DOUBLE TYPE NOT C FLOAT TYPE
char* str_of_float(double x) {
  char* str;
  str = (char *) malloc( sizeof(char) * MAXFLOATSIZE);
  snprintf(str, MAXFLOATSIZE, "%g", x);
  return str;
}

// rounds float to nearest int
int int_of_float(double x){
  return (int) round(x);
}

double float_of_int(int x){
  return x * 1.0;
}

char* string_concat(char * str1, char* str2) {
	int totalLength = strlen(str1) + strlen(str2) + 1;
	char* result = calloc( totalLength, 1 );
	strcat(result, str1);
	strcat(result, str2);
	return result;
}

int string_equals(char * str1, char* str2) {
	int res = strcmp(str1, str2);
	if (res == 0) {
		return 1;
	}
	return 0;
}

// max_size doesn't have to include the terminating \0
// takes a size to malloc and which the string must be shorter than
// this max size capped at 4096
char* scan_line(int max_size){
  if (max_size > 4096){
    return NULL;
  }
    char *str = (char*)malloc(sizeof(char)*(max_size+1));
    fgets(str,max_size, stdin);
    return str;
}

// regular successful exit
int exit_success(int code){
  exit(0);
  return 0;
}

// exit with error message
int die(char * error_msg, int code){
  printf("%s\n", error_msg);
  exit(code);
  return 0;
}
