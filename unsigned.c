void unsigned_( op, in1, in2, result )
	char		*op;
	unsigned	*in1;
	unsigned	*in2;
	unsigned	*result;

{
	switch ( *op ) {
		case	'+':
			*result = *in1 + *in2;
			break;
		case	'-':
			*result = *in1 - *in2;
			break;
		case	'*':
			*result = *in1 * *in2;
			break;
		case	'/':
			*result = *in1 / *in2;
			break;
		default:
			return;
	}
}

void	ushort2long_( in1, result )
	unsigned short	*in1;
	int		*result;
{
	*result	= *in1;
}

void	long2ushort_( in1, result )
	long		*in1;
	unsigned short	*result;
{
	*result	= *in1;
}

