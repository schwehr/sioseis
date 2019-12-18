      PROGRAM mperdeg
c    Given a latitude, print the meters per deg of 
c  latitude and longitue.
c
      REAL alat, amlat, amlong

      PRINT *,' Enter a latitude:'
      READ *,alat
      CALL lendeg( alat, amlat, amlong)
      PRINT *,' There are ',amlat,' meters per degree latitude.'
      PRINT *,' There are ',amlong,' meters per degree longitude.'
      STOP
      END
