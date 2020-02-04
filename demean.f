      SUBROUTINE DEMEAN(IN,IOUT,N,MEAN)
C     DEMEAN IS AN AP ROUTINE TO REMOVE THE MEAN VALUE OF A TRACE.  MEAN REMOVAL
C IS THE SAME AS REMOVING THE DC SHIFT OR DC BIAS.
C
C ARGUMENTS:
C  IN     - THE AP ADDRESS OF THE INPUT ARRAY (THE BIASED ARRAY).
C  IOUT   - THE AP ADDRESS OF THE OUTPUT ARRAY TO RECEIVE THE UNBIASED DATA.
C           (IOUT MAY BE THE SAME AS IN).
C  N      - THE NUMBER OF ELEMENTS IN THE IN ARRAY.
C  MEAN   - THE AP ADDRESS WHERE THE MEAN VALUE CAN BE STORED.
C
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 27 OCTOBER 1981
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
      CALL MEANV(IN,1,MEAN,N)                                           ! /* FIND THE MEAN VALUE
      CALL VNEG(MEAN,1,MEAN,1,1)                                        ! /* NEGATE THE MEAN VALUE
      CALL VSADD(IN,1,MEAN,IOUT,1,N)                                    ! /* SUBTRACT THE MEAN (ADD THE NEGATIVE)
      CALL VNEG(MEAN,1,MEAN,1,1)                                        ! /* MAKE THE MEAN AS IT WAS
      RETURN
      END
