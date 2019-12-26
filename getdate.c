/* getdate.c  - returns ASCII local time of day     */

#include <time.h>

void getdate1_(char *date) {
    long ltime;
    time(&ltime);
    struct tm *tm = localtime(&ltime);
    char *ascitime = asctime(tm);
    for (int i = 0; i <= 24; ++i)
       date[i] = ascitime[i];
}
