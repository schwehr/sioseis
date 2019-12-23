#include <stdio.h>
#include <string.h>

void mknam( nam )
     char      *nam;
{
     while( *nam != ' ' && *nam != '\0' ) *nam++;  /* find a blank or the end */
        *nam = '\0';
}

void	bldgname_( dirname, filen, pathname )
	char	*dirname;
	int	*filen;
	char	*pathname;
{
	char	num[20];
     sprintf(num,"/%d.sgd", *filen );
	mknam( dirname );
	strcpy( pathname, dirname );
	strcat( pathname, num );
	return;
}
