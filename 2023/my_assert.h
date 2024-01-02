#ifndef MY_ASSERT_H_INCLUDED
#define MY_ASSERT_H_INCLUDED

#include <stdlib.h>

#ifndef MY_ASSERT_EXIT_ON_FAILED
#define MY_ASSERT_EXIT_ON_FAILED 1
#endif

typedef enum __cmp {lt=0, lte, gt, gte, eq, neq} cmp;

const char* cmp2string(cmp c) {
    static const char tr[6][3] = {"<", "<=", ">", ">=", "==", "!="};
    return tr[c];
}

#define my_assert(X, CMP, Y) do {                                               \
    int res;                                                                    \
    typeof(X) x = (X);                                                          \
    typeof(Y) y = (Y);                                                          \
    switch(CMP) {                                                               \
        case lt:                                                                \
            res = x < y;                                                        \
            break;                                                              \
        case lte:                                                               \
            res = x <= y;                                                       \
            break;                                                              \
        case gt:                                                                \
            res = x > y;                                                        \
            break;                                                              \
        case gte:                                                               \
            res = x >= y;                                                       \
            break;                                                              \
        case eq:                                                                \
            res = x == y;                                                       \
            break;                                                              \
        case neq:                                                               \
            res = x != y;                                                       \
            break;                                                              \
        default:                                                                \
            printf("Bad comparsion operator");                                  \
    }                                                                           \
    if (0 == res) {                                                             \
        printf( "ASSERT FAILED on line %d in function:"                         \
                "%s: %s %s %s (", __LINE__, __func__, #X, cmp2string(CMP), #Y); \
        pprint(x);                                                              \
        printf(", ");                                                           \
        pprint(y);                                                              \
        printf(")\n");                                                          \
        if (MY_ASSERT_EXIT_ON_FAILED)                                           \
            exit(EXIT_FAILURE);                                                 \
    }                                                                           \
} while(0);

#define pprint(x) \
do { \
  char* format; \
  if (__builtin_types_compatible_p(typeof(x), int) || \
      __builtin_types_compatible_p(typeof(x), unsigned int) || \
      __builtin_types_compatible_p(typeof(x), short int) || \
      __builtin_types_compatible_p(typeof(x), unsigned short int) ) \
    format = "%d"; \
  else if (__builtin_types_compatible_p(typeof(x), long int) || \
	   __builtin_types_compatible_p(typeof(x), unsigned long int) ) \
    format = "%ld"; \
  else if (__builtin_types_compatible_p(typeof(x), double) || \
	   __builtin_types_compatible_p(typeof(x), double) || \
	   __builtin_types_compatible_p(typeof(x), float) || \
	   __builtin_types_compatible_p(typeof(x), float) ) \
    format = "%f"; \
  else if (__builtin_types_compatible_p(typeof(x), char) || \
	   __builtin_types_compatible_p(typeof(x), unsigned char) ) \
    format = "%c"; \
  else if (__builtin_types_compatible_p(typeof(x), char*) || \
	   __builtin_types_compatible_p(typeof(x), const char*) || \
	   __builtin_types_compatible_p(typeof(x), unsigned char*) || \
	   __builtin_types_compatible_p(typeof(x), const unsigned char*) ) \
    format = "%s"; \
  else {\
    printf("Unsupported type in file: %s:%d function: %s variable: %s = 0x%x\n", __FILE__, __LINE__, __func__, #x, (unsigned int) x); \
    break; \
  } \
  printf("%s = ", #x); \
  printf(format, x); \
} while (0);

#endif //MY_ASSERT_H_INCLUDED
