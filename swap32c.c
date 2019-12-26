// COPYRIGHT: PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 11 APRIL 1982

// SWAP32 SWAPS THE 4 BYTES WITHIN EACH 32 BIT WORD OF ARRAY BUF.
// THIS ROUTINE IS USEFUL FOR CONVERTING DEC 32 BIT INTEGERS
// TO OTHER COMPUTER
//   INTEGER FORMATS (OR VICE VERSA).
//
// ARGUMENTS:
//   BUF - THE ARRAY TO BE CONVERTED.
//   N   - THE NUMBER OF 32 BIT WORDS TO BE CONVERTED.  INTEGER*4

void swap32(char buf[], int n) {
    char a[4];

    for (int j = 0, i = 0; i < n; j += 4, i++) {
        a[0] = buf[j];
        a[1] = buf[j+1];
        a[2] = buf[j+2];
        a[3] = buf[j+3];
        buf[j] = a[3];
        buf[j+1] = a[2];
        buf[j+2] = a[1];
        buf[j+3] = a[0];
      }
}
