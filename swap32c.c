void swap32(buf,n)
      int n;
      char buf[];
{
/*     SWAP32 SWAPS THE 4 BYTES WITHIN EACH 32 BIT WORD OF
ARRAY BUF.
C  THIS ROUTINE IS USEFUL FOR CONVERTING DEC 32 BIT INTEGERS
TO OTHER COMPUTER
C  INTEGER FORMATS (OR VICE VERSA).
C
C  ARGUMENTS:
C     BUF - THE ARRAY TO BE CONVERTED.
C     N   - THE NUMBER OF 32 BIT WORDS TO BE CONVERTED.  INTEGER*4
C
C  COPYRIGHT: PAUL HENKART, SCRIPPS INSTITUTION OF
OCEANOGRAPHY, 11 APRIL 1982
C
*/
      char a[4];
      int i, j;

      for (j = 0, i = 0; i < n; j += 4, i++) {
        a[0]=buf[j];
        a[1]=buf[j+1];
        a[2]=buf[j+2];
        a[3]=buf[j+3];
        buf[j]=a[3];
        buf[j+1]=a[2];
        buf[j+2]=a[1];
        buf[j+3]=a[0];
      }
}
