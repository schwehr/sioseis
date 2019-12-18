      function lenstr(c)
c   lenstr returns the length of the character string c.  The string is defined
c to end by either a blank or a null character.  The end of the string is also
c detected when the length of the string is exceeded.

      character*(*) c

      m=len(c)
      n=1
   10 if(n.gt.m) go to 20
      if(c(n:n).eq.' '.or.c(n:n).eq.char(0)) go to 20
      n=n+1
      go to 10
   20 n=n-1
      lenstr=n
      end
