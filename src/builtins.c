#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
