// Are we little or big endian?  From Harbison&Steele.
// return 1 if big, -1 if little


int is_big_endian_(void) {
    union {
        long l;
        char c[sizeof (long)];
    } u;
    u.l = 1;
    return u.c[sizeof (long) - 1] == 1 ? 1 : -1;
}


int is_big_endian__() {
    return is_big_endian_();
}

