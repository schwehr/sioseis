#define	MAX_LAT_LON_STRING	10
#include <sys/param.h>
#include <sys/types.h>

	static int	strid=-1;
	static int	shotid=-1;

typedef enum
   {
	OldData = 0,
	NewData = 1,
	Stop = -1
   } dataStatus_t;

struct	seismic_plot_t
   {
   dataStatus_t status;        /* 0 means free for writing, 1 means ready/busy */
   time_t      shotTime;       /* time stamp for shot currently displayed in
                                  UNIX time_t format */
   int         shotMsecs,      /* msec field for shot time stamp */
               shotNum,        /* shot number */
               numTraces,      /* number of traces to be plotted */
               numSamples,     /* number of samples per trace */
               clipFactor,     /* integral number of channels to clip at - e.g.,
                                  if clipFactor = 1, then any data that would
                                  plot past the middle of the adjacent channels'
                                  traces is clipped at that point */
               traceFill;      /* if True, positive fill traces */
   float       dataMin,        /* min data value for scaling traces */
               dataMax;        /* max data value for scaling traces */
   char        lat[ MAX_LAT_LON_STRING ],
               lon[ MAX_LAT_LON_STRING ];
                              /* lat/lon strings for shot currently displayed -
                                 format is:
                                    latitude:   99.9999N
                                    longitude: 999.9999W
                               */
   double      samplingFreq;  /* samples/sec for the traces in this shot */
   } ;

void mkhdr_( ptr, utime, shotno, numtrcs, nsamps, srate )
	struct	seismic_plot_t	*ptr;
	long	*utime;	/* Unix time of data */
	long	*shotno;	/* the shot number */
	long	*numtrcs;	/* the number of traces in the file */
	long	*nsamps;	/* the number of samples per trace */
	double	*srate;	/* sample rate (samples per second) of the data */

{
/*	printf("%d %d %d %d %f \n",*utime, *shotno, *numtrcs, *nsamps, *srate);*/
	ptr->status = NewData;	/* ready to be plotted */
	ptr->shotTime = *utime;
	ptr->shotMsecs = 0;	/* Too hard to figure out today */
	ptr->shotNum = *shotno;
	ptr->numTraces = *numtrcs;
	ptr->numSamples = *nsamps;
	ptr->samplingFreq = *srate;
	return;
}

void strinit_()
{
	strid = 1;
	printf("setting %d\n",strid);
/*	if( (strid = initlock(NEAR_KEY)) < 0 )*/
	if( strid < 0 )
	{
		perror( "atcplot - initlock(NEAR_KEY) failed");
		exit(1);
	}
	return;
}
void shotinit_()
{
	shotid = 2;
	printf("setting %d\n",shotid);
/*	if( (shotid = initlock(NEAR_KEY)) < 0 )*/
	if( shotid < 0 )
	{
		perror( "atcplot - initlock(SHOT_KEY) failed");
		exit(1);
	}
	return;
}
void lockstr_()
{
	printf("lock %d\n",strid);
/*	while( setlock( strid ) != 0 ) sleep(1);*/
	return;
}
void lockshot_()
{
	printf("lock %d\n",shotid);
/*	while( setlock( shotid ) != 0 ) sleep(1);*/
	return;
}
void clearstr_()
{
	printf("clear %d\n",strid);
/*	if( clearlock( strid ) != 0 )*/
	if(  strid != 1 )
	{
		perror( "atcplot - clearlock(NEAR_KEY) failed");
		exit(1);
	}
	return;
}
void clearshot_()
{
	printf("clear %d\n",shotid);
/*	if( clearlock( shotid ) != 0 )*/
	if(  shotid != 2 )
	{
		perror( "atcplot - clearlock(SHOT_KEY) failed");
		exit(1);
	}
	return;
}
