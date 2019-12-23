void ptrlist_( ntrlist, trlist, ltrhdr, itrhdr, trhdr )
   int	*ntrlist;
   int	trlist[];
   long ltrhdr[];
   short  itrhdr[];
   float  trhdr[];

{
   int i, ideg, min;
   float scalar, deg, rmin, sec, depth;

	for ( i=0; i<*ntrlist; i++ ) {
	    switch( trlist[i] ) {
		case 2:  /*  shotno */
			printf("%6d ",ltrhdr[2]);
			break;
		case 3:  /*  rpno  */
			printf("%6d ",ltrhdr[5]);
			break;
		case 4:    /*  GMT  */
			printf("%3d %2.2d%2.2dz ",itrhdr[79],itrhdr[80],itrhdr[81]);
			break;
		case 6:   /*  range  */
			printf("%6d ",ltrhdr[9]);
			break;
		case 7:   /*  shottr  */
			printf("%6d ",ltrhdr[3]);
			break;
		case 8:   /*  espn   */
			printf("%6d ",ltrhdr[4]);
			break;
		case 12:  /* rptr  */
			printf("%6d ",ltrhdr[6]);
			break;
		case 28:  /* Water bottom depth at source */
			depth = ltrhdr[15];
                        scalar = (float) itrhdr[35];
                        if( scalar < 0 ) scalar = -depth / scalar;
                        if( scalar > 0 ) depth = depth * scalar;
                        printf("%.6f ",depth);
			break;
		case 29:  /* Water bottom depth at receiver */
			depth = ltrhdr[16];
                        scalar = (float) itrhdr[35];
                        if( scalar < 0 ) scalar = -depth / scalar;
                        if( scalar > 0 ) depth = depth * scalar;
                        printf("%.6f ",depth);
			break;
		case 30:  /*   water bottom time  */
			printf("%.6f ",trhdr[49]);
			break;
		case 15:  /* GMT + sec  */
			printf("%2.2d%2.2dz %2.2ds ",itrhdr[80],itrhdr[81],itrhdr[82]);

			break;
		case 16:  /*   SXD  */
			deg = ltrhdr[18] / 3600.;
			scalar = (float) itrhdr[35];
			if( scalar < 0 ) scalar = -deg / scalar;
			if( scalar > 0 ) deg = deg * scalar;
			printf("%.6f ",deg);
			break;
		case 17:  /*   SYD  */
			deg = ltrhdr[19] / 3600.;
			scalar = (float) itrhdr[35];
			if( scalar < 0 ) scalar = -deg / scalar;
			if( scalar > 0 ) deg = deg * scalar;
			printf("%.6f ",deg);
			break;
		case 18:  /*   SXDM  */
			deg = ltrhdr[18] / 3600.;
			scalar = (float) itrhdr[35];
			if( scalar < 0 ) scalar = -deg / scalar;
			if( scalar > 0 ) deg = deg * scalar;
			ideg = (int) deg;
               if( deg > 0 ) {
                    deg = deg - (float) ideg;
               }else{
                    deg = -deg + (float) ideg;
               }
			rmin = deg * 60;
			printf("%4d %9.6f ",ideg,rmin);
			break;
		case 19:  /*   SYDM  */
               deg = ltrhdr[19] / 3600.;
               scalar = (float) itrhdr[35];
               if( scalar < 0 ) scalar = -1. / scalar;
               if( scalar > 0 ) deg = deg * scalar;
               ideg = (int) deg;
               if( deg > 0 ) {
                    deg = deg - (float) ideg;
               }else{
                    deg = -deg + (float) ideg;
               }
               rmin = deg * 60;
               printf("%3d %9.6f ",ideg,rmin);
			break;
		case 20:  /*   SXDMS  */
               deg = ltrhdr[18] / 3600.;
               scalar = (float) itrhdr[35];
               if( scalar < 0 ) scalar = -1. / scalar;
               if( scalar > 0 ) deg = deg * scalar;
               ideg = (int) deg;
               if( deg > 0 ) {
                    deg = deg - (float) ideg;
               }else{
                    deg = -deg + (float) ideg;
               }
               rmin = deg * 60;
			min = (int)rmin;
               deg = deg - ((float)min / 60.) ;
               sec = deg * 3600.;
               printf("%4d %2d %9.6f ",ideg,min,sec);
			break;
		case 21:  /*   SYDMS  */
               deg = ltrhdr[19] / 3600.;
               scalar = (float) itrhdr[35];
               if( scalar < 0 ) scalar = -1. / scalar;
               if( scalar > 0 ) deg = deg * scalar;
               ideg = (int) deg;
               if( deg > 0 ) {
				deg = deg - (float) ideg;
			}else{
				deg = -deg + (float) ideg;
			}
               rmin = deg * 60;
               min = (int)rmin;
               deg = deg - ((float)min / 60.) ;
               sec = deg * 3600.;
               printf("%3d %2d %9.6f ",ideg,min,sec);
			break;
		case 22:  /*   RXD  */
			deg = ltrhdr[20] / 3600.;
			scalar = (float) itrhdr[35];
			if( scalar < 0 ) scalar = -1. / scalar;
			if( scalar > 0 ) deg = deg * scalar;
			printf("%9.6f ",deg);
			break;
		case 23:  /*   RYD  */
			deg = ltrhdr[21] / 3600.;
			scalar = (float) itrhdr[35];
			if( scalar < 0 ) scalar = -1. / scalar;
			if( scalar > 0 ) deg = deg * scalar;
			printf("%9.6f ",deg);
			break;
		case 24:  /*   RXDM  */
			deg = ltrhdr[20] / 3600.;
			scalar = (float) itrhdr[35];
			if( scalar < 0 ) scalar = -1. / scalar;
			if( scalar > 0 ) deg = deg * scalar;
			ideg = (int) deg;
               if( deg > 0 ) {
                    deg = deg - (float) ideg;
               }else{
                    deg = -deg + (float) ideg;
               }
			deg = deg - (float) ideg;
			rmin = deg * 60;
			printf("%4d %9.6f ",ideg,rmin);
			break;
		case 25:  /*   RYDM  */
               deg = ltrhdr[21] / 3600.;
               scalar = (float) itrhdr[35];
               if( scalar < 0 ) scalar = -1. / scalar;
               if( scalar > 0 ) deg = deg * scalar;
               ideg = (int) deg;
               if( deg > 0 ) {
                    deg = deg - (float) ideg;
               }else{
                    deg = -deg + (float) ideg;
               }
               deg = deg - (float) ideg;
               rmin = deg * 60;
               printf("%3d %9.6f ",ideg,rmin);
			break;
		case 26:  /*   RXDMS  */
               deg = ltrhdr[20] / 3600.;
               scalar = (float) itrhdr[35];
               if( scalar < 0 ) scalar = -1. / scalar;
               if( scalar > 0 ) deg = deg * scalar;
               ideg = (int) deg;
               if( deg > 0 ) {
                    deg = deg - (float) ideg;
               }else{
                    deg = -deg + (float) ideg;
               }
               rmin = deg * 60;
			min = (int)rmin;
               deg = deg - ((float)min / 60.) ;
               sec = deg * 3600.;
               printf("%4d %2d %9.6f ",ideg,min,sec);
			break;
		case 27:  /*   RYDMS  */
               deg = ltrhdr[21] / 3600.;
               scalar = (float) itrhdr[35];
               if( scalar < 0 ) scalar = -1. / scalar;
               if( scalar > 0 ) deg = deg * scalar;
               ideg = (int) deg;
               if( deg > 0 ) {
				deg = deg - (float) ideg;
			}else{
				deg = -deg + (float) ideg;
			}
               rmin = deg * 60;
               min = (int)rmin;
               deg = deg - ((float)min / 60.) ;
               sec = deg * 3600.;
               printf("%3d %2d %9.6f ",ideg,min,sec);
			break;
	    }
	}
	printf("\n");
	return;
}
