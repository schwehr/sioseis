	subroutine fastf(Fr,Fi,n)
c	
c	n  = the number of data points=2**m
c	Fr = the real data set
c	Fi is the imaginary part of data set (=0.0 if only real)
c	
c	first compute m
c
	real*4 Fr(n), Fi(n), Gr, Gi, Er, Ei, Eu, Ez
	m=0
	kd=n
 1	kd=kd/2
	m=m+1
	if (kd .ge. 2) go to 1
	nd2=n/2
	nm1=n-1
	l=1
c
c	shuffle input data in binary digit reverse order
c
	do 4 k=1,nm1
	if (k .ge. l) go to 2
	Gr=Fr(l)
	Gi=Fi(l)
	Fr(l)=Fr(k)
	Fi(l)=Fi(k)
	Fr(k)=Gr
	Fi(k)=Gi
 2	nnd2=nd2
 3	if(nnd2 .ge. l) go to 4
	l=l-nnd2
	nnd2=nnd2/2
	go to 3
 4	l=l+nnd2
	pi=3.14159265
c
c	first arrange accounting of m stage
c
	do 6 j=1,m
	nj=2**j
	njd2=nj/2
	Eu=1.0
	Ez=0.0
	Er=cos(-pi/njd2)
	Ei=sin(-pi/njd2)
c
c	compute fourie transform in each m stage
c
	do 6 it=1,njd2
	do 5 iw=it,n,nj
	iwj=iw+njd2
	Gr=Fr(iwj)*Eu-Fi(iwj)*Ez
	Gi=Fi(iwj)*Eu+Fr(iwj)*Ez
	Fr(iwj)=Fr(iw)-Gr
	Fi(iwj)=Fi(iw)-Gi
	Fr(iw) =Fr(iw)+Gr
 5	Fi(iw) =Fi(iw)+Gi
	Seu=Eu
	Eu=Seu*Er-Ez*Ei
 6	Ez=Ez*Er+Seu*Ei
	Return
	End
