#include <stdio.h>
#include <string.h>

void mknam(char *nam) {
    // Find a blank or the end
    // TODO(schwehr): *nam++ is a bug.
    while( *nam != ' ' && *nam != '\0' ) *nam++;

    *nam = '\0';
}

void bldgname_(char *dirname, int *filen, char *pathname) {
    char num[20];

    sprintf(num,"/%d.sgd", *filen);
    mknam(dirname);
    strcpy(pathname, dirname);
    strcat(pathname, num);
}
