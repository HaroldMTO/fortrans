! program test


program   ttest ! toto

	integer i_i
	character(len=20) :: s

	print '("s:",x,a)',"set 's & s' !"
	s = "! allo & 
& la terre ?!"

	print "('p:',x,a)","print 's' !"

	do 12345 i_i=1,20,-1
		if(s(i_i:i_i)=='a') & 

		then

		s(i_i:i_i) = "A"

     else
	  s(i_i:i_i) =&
	  'b'

		endif

12345 continue

	print "('',a,a,'')",s,'! ici la &
		&lune!'
	!!
!

contains


subroutine f(x)

integer x
x=x+1

endsubroutine

endprogram


! fin
