#include <stdio.h>

void main(void) {
    int x, y, s, d;
    scanf("%d", &x);

    y = 0;
    if (x < 0) {
        s = -1;
    } else {
        s = 1;
    }
    d = 1;
    while (x != 0) {
        x = x - s;
        y = y + d;
        d = d + 2;
    }

    printf("%d\n", y);
}
