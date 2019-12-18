icomplement_(a, n, b)
	short	*a, *b;
	int		*n;
{
	int	i;

	for( i=0; i<*n; i++) b[i] = ~a[i];
	return;
}



lcomplement_(a, n, b)
	long		*a, *b;
	int		*n;
{
	int	i;

	for( i=0; i<*n; i++) b[i] = ~a[i];
	return;
}
