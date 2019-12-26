void icomplement_(short *a, int *n, short *b) {
    for(int i = 0; i < *n; i++)
        b[i] = ~a[i];
}

void lcomplement_(long *a, int *n, long *b) {
    for(int i = 0; i < *n; i++)
        b[i] = ~a[i];
}
