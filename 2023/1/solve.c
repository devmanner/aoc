#include <stdio.h>
#include <string.h>
#include <assert.h>

#define MY_ASSERT_EXIT_ON_FAILED 0
#include "../my_assert.h"

#define BUFFSIZE 256

int first_num(char *s, int inc) {
    for (; *s < '0' || *s > '9'; s += inc)
        ;
    return *s - '0';
}

char *numbers[] = {"zero", "0", "one", "1", "two", "2", "three", "3", "four", "4", "five", "5", "six", "6", "seven", "7", "eight", "8", "nine", "9"};

void char_nums(char *s, int *fst, int *lst) {
    int i;
    char *found;
    char *fstp = s+strlen(s);
    char *lstp = s;
    for (i = 0; i < 20; ++i) {
        for (int si = 0; NULL != (found = strnstr(s+si, numbers[i], BUFFSIZE)); si += strlen(numbers[i])) {
            if (found < fstp) {
                fstp = found;
                *fst = i>>1;
            }
            if (found > lstp) {
                lstp = found;
                *lst = i>>1;
            }
        }
    }
    printf("%s %d %d\n", s, *fst, *lst);
}

int stage1(char* fname) {
    char buff[BUFFSIZE];
    int res = 0;
    FILE *fp = fopen(fname, "r");
    assert(fp);
    while (fgets(buff, BUFFSIZE, fp))
        res += 10*first_num(buff, 1) + first_num(buff + strlen(buff)-1, -1);

    fclose(fp);
    return res;
}

int stage2(char* fname) {
    char buff[BUFFSIZE];
    int fst, lst;
    int res = 0;
    FILE *fp = fopen(fname, "r");
    assert(fp);
    while (fgets(buff, BUFFSIZE, fp)) {
        char_nums(buff, &fst, &lst);
        printf("%d\n", fst*10 + lst);
        res += fst*10 + lst;
    }

    fclose(fp);

    printf("Result: %d\n", res);

    return res;
}

void test() {
    my_assert(142, eq, stage1("example1.txt"));
    my_assert(281, eq, stage2("example2.txt"));
    my_assert(11, eq, stage2("example3.txt"));
    my_assert(231, eq, stage2("gpt_tc1.txt"));
    my_assert(181, eq, stage2("gpt_tc2.txt"));
    my_assert(69, eq, stage2("gpt_tc3.txt"));
    my_assert(258, eq, stage2("gpt_tc4.txt"));
//    my_assert(54728, neq, stage2("input.txt"));
}

int one() {
    puts("EXEC: one");
    return 1;
}

int main(int argc, char *argv[]) {
    int a=1,b=2, X;
    X = 0;
    my_assert(X, neq, 0);
    my_assert(a, lt, b);
    my_assert(a, gt, b);
    my_assert(one(), neq, 1);

    //test();
//    printf("Stage1: %d\n", stage1("input.txt"));
//    printf("Stage2: %d\n", stage2("input.txt"));
    return 0;
}