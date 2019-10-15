 !quicksort w fortranie
 ! Krzysztof Szczepaniec
 ! 12.06.09
 

module sort
 implicit none
 contains
  recursive subroutine qsort(input,lewy,prawy) 
  real, dimension(10) ::  input
  integer, intent(in) :: lewy, prawy
  real :: temp
  integer :: m,j
  
  if( lewy .LT. prawy ) then
    m = lewy
    
    j=lewy+1
     do  while( j .le. prawy)  
         if ( input(j) .gt. input(lewy)) then
            m = m+1
	    temp = input(m)
	    input(m) = input(j)
	    input(j) = temp	    
         end if
     j=j+1
     end do
      temp = input(lewy)
      input(lewy) = input(m)
      input(m) = temp	
     
      call qsort(input,lewy,m-1)
      call qsort(input,m+1,prawy)
         
    
  end if
   return
  end subroutine qsort

end module sort


program quicksort
 use sort
  implicit none
 real, dimension(10) :: tablica,tab2
 integer :: i
 
 do i=1,10
  tablica(i) = 2.3*i*RAND()
 end do

 print *, tablica
 call qsort(tablica,1,10)
 print *,tablica
 
end program quicksort