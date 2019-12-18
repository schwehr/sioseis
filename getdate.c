/* getdate.c  - returns ASCII local time of day     */

#include <time.h>

getdate1_(date)
   char  *date;

{
   struct tm *localtime();
   struct tm *tm;
   long ltime;
   char *asctime();
   char *ascitime;
   int   i;

   time(&ltime);
   tm = localtime(&ltime);
   ascitime = asctime(tm);
   for ( i = 0; i <= 24; ++i ) date[i] = ascitime[i];
   return;
}
