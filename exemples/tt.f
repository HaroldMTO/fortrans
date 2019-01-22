! program 'test'


program   ttest ! 'toto' &

	integer i_i,j,a(10),b(10)
	character(len=20) :: s='il l''a, elle l''a, ils l''ont',s2="tt!"

	print '("s:",x,a)',"set 's & s' !"
	s = ""! allo & 
	s = "la terre ?!"
 
a10:where(a==2) ! a=2
a=1
elsewhere
b=0
endwhere a10

	print "('p:',x,a)","affiche 's' &
	!"

do
	do 12345 i_i=1,20,-1
		if(s(i_i:i_i)=='a') & 

		then

		s(i_i:i_i) = "A"

     else
	  s(i_i:i_i) =&
	  'b'

		endif

12345 continue

exit
end do
	print *,s,'! ici la & ! comment
      lune!'
	print '(a," ",a)',s,'! ici la &
      ! lune!&
',s2
	!! 
!
 
contains
 

subroutine f(x)

integer x
x=&
x+&
&1

endsubroutine

endprogram


! fin
