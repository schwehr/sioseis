#include <stdio.h>
#include <sys/types.h>

/*   return byte barray[index-1] in result
*    barray can be any fortran array (integer*2, integer*4)
*    index is the fortran index (starts with 1, not 0) of the byte within barray
*    result is a short (16 bit) signed integer  */
int	get_byte_( barray, index, result )
	char	barray[];
	int	*index, *result;
{
	*result = &barray[*index-1];
	return;
}
