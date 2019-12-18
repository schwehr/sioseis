	integer*8 i,ten
	integer*4 j, k(2)
	i = 1073741823   ! 3fffffff
	ten = 10
	i = i * ten
	print *,' i=',i
	j = 1
	k(1) = 0
        k(2) = 1
	call testc (i,j)
	end
