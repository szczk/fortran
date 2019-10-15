! kolko i kzzyzyk - odrobine poprawiona wersja
!
!
!
!  Krzystof Szczepaniec
!
!

program p104_game
implicit none

interface
  integer function check(tab,sum)
    integer :: sum
    integer :: tab(9)
  end function check
end interface

character, dimension(3,3) :: pool
integer :: rpool(9)
integer :: i, temp, in2, end=0

character (len=1) in

do i=1,9
	pool ( int((i-1)/3) + 1 , mod(i-1,3)+1) = ' ' 
	rpool(i) = 0
end do

do
	temp = 0
	do i=1,9
		if(rpool(i).eq.4 .or. rpool(i).eq.1) temp = temp + 1
		if(rpool(i).eq.1) pool((i-1)/3+1, mod(i-1,3)+1) = "X"
		if(rpool(i).eq.4) pool((i-1)/3+1, mod(i-1,3)+1) = "O"
	end do

print *, "_________________________________"
print *, ""
print *, "     _______________________ "
print *, "    |      1|      2|     3 |"
print *, "    |   ", pool(1,1), "   |   ", pool(1,2), "   |   ", pool(1,3), "   |"
print *, "    |_______|_______|_______|"
print *, "    |      4|      5|     6 |"
print *, "    |   ", pool(2,1), "   |  ", pool(2,2), "    |  ", pool(2,3), "    |"
print *, "    |_______|_______|_______|"
print *, "    |      7|      8|     9 |"
print *, "    |   ", pool(3,1), "   |   ", pool(3,2), "   |   ", pool(3,3), "   |"
print *, "    |_______|_______|_______|"
print *, ""


  if(temp.eq.9) then
  	  print *, "~~~~~~~remis!~~~~~"
            exit
	    end if 
  if(end.eq.1) then
	  print *, "~~~~~Przegrales!~~~~~"
	  exit
  end if

print *, "Podaj numer pola: "
read(*, '(I1)'), in2
if(rpool(in2).ne.0) then
	print *, "To pole jest juz zajete, wybierz inne!"
	cycle
end if

rpool(in2) = 4

!jesli przeciwnik wlasnie rozpoczal
if(sum(rpool).eq.4) then
    !jesli rozpoczal w frogu
    if(mod(in2-1,2).eq.0 .and. in2.ne.5) then
	    !zakresl kratke w srodku
	    rpool(5) = 1
    else
	    !jesli rozpoczal na boku lub srodka to ty zacznij od rogu
	    rpool(1) = 1
    end if

    ! pierwsze ruchy zostaly juz wykonane
else
    !sprawdzamy, czy nie mozemy skonczyc gry
    temp = check(rpool,2)
    if(temp.ne.-1) then
	    rpool(temp) = 1
	    end = 1
    else
    !sprawdzamy, czy nie jestemy zagrozeni przez przeciwnika
	    temp = check(rpool,8)
	    if(temp.ne.-1) then
	    rpool(temp) = 1
	    else
		  !jesli nie jestesmy zagrozeni ani nie mozemy zakonczyc partii w tym ruchi
		  ! to probojemy konczyc juz rzopoczete "linie"
		  temp = check(rpool,1)
		  if(temp.ne.-1) then
			  rpool (temp) = 1
		  else
			!jesli nie ma linii ktore mozna by konczyc to zakreslamy pierwsza lepsza kratke
			! poprawka: nie pierwsza lepsza tylko w rogu
			do i=1,9,2
				if(rpool(i) .eq. 0) then
				rpool(i) = 1
				exit
				end if
			end do
			
			do i=2,8,2
				if(rpool(i) .eq. 0) then
				rpool(i) = 1
				exit
				end if
			end do
		  end if
	    end if
    end if
end if
end do

stop
end program p104_game


integer function CHECK(tab, sum)
integer :: sum
integer :: tab(9)
	CHECK  = -1
	if(tab(1) + tab(5) + tab(9) .eq. sum) then
		if(tab(1).eq.0) check = 1
		if(tab(9).eq.0) check = 9
		if(tab(5).eq.0) check = 5
		return
	end if
	
	if(tab(7) + tab(5) + tab(3) .eq. sum) then
		if(tab(7).eq.0) check = 7
		if(tab(5).eq.0) check = 5
		if(tab(3).eq.0) check = 3
		return
	end if


	if(tab(1) + tab(2) + tab(3) .eq. sum) then
		if(tab(1).eq.0) check = 1
		if(tab(3).eq.0) check = 3
		if(tab(2).eq.0) check = 2
		return
	end if

	if(tab(4) + tab(5) + tab(6) .eq. sum) then
		if(tab(4).eq.0) check = 4
		if(tab(6).eq.0) check = 6
		if(tab(5).eq.0) check = 5
		return
	end if

	if(tab(7) + tab(8) + tab(9) .eq. sum) then
		if(tab(7).eq.0) check = 7
		if(tab(9).eq.0) check = 9
		if(tab(8).eq.0) check = 8
		return
	end if

	if(tab(1) + tab(7) + tab(4) .eq. sum) then
		if(tab(1).eq.0) check = 1
		if(tab(7).eq.0) check = 7
		if(tab(4).eq.0) check = 4
		return
	end if

	if(tab(2) + tab(8) + tab(5) .eq. sum) then
		if(tab(2).eq.0) check = 2
		if(tab(8).eq.0) check = 8
		if(tab(5).eq.0) check = 5
		return
	end if

	if(tab(3) + tab(6) + tab(9) .eq. sum) then
		if(tab(3).eq.0) check = 3
		if(tab(9).eq.0) check = 9
		if(tab(6).eq.0) check = 6
		return
	end if

return
end function CHECK

