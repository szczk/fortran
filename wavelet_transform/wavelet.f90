!
!        WAVELETS TRANSFORM
!
! 	FORTRAN90/95
! 	Krzysztof Szczepaniec 
! 	10-12.06.2009
!
! program oblicza dyskretna transformate falkowa (waveletowa) 
! na bazie falki Haara
! z danych podanych w pliku data04.dat
!
! nastepnie wybiera 1/2, 1/4, 1/8 i 1/16 najwiekszych
! co do modulu wspolczynnikow transformaty 
! i oblicza transformaty odwrotne 
! ktore zapisuje do plikow
! rowniez 'wavelet denoising' - odszumianie 
! przy uzyciu 'hard rule' i 'mid rule'
!
! ustalamy poziom obciecia D = sqrt(2 log_2 N) * sigma
! gdzie sigma to poziom szumu
!
! hard rule
! zerujemy wszystkie wspolczynniki falkowe na modul mniejsze od D
!
!
! mid rule
!  ustawiamy wspolczynniki:
!  W   dla \W| >= 2D 
!  2 sgn(W) (|W| - D) dla  D <= \W| < 2D
!  0  dla |W| < D

!*************************************************************
! fortranowa implementacja sortowania QUICKSORT - 
! lekko zmodyfikowana na potrzeby obcinania wspolczynnikow DWT
! dokladniej: sortuje macierz 2x512 wzgledem wybranej kolumny
! sortowanie wzgledem modulu lub nie -> przelaczane ostatnim parametrem true/false
module sort
 ! quicksort w fortranie
 ! krzysztof szczepaniec
 ! 12.06.09
 implicit none
 contains
  recursive subroutine qsort(input,lewy,prawy,kolumna,modul) 
  real, dimension(2,512) ::  input
  integer, intent(in) :: lewy, prawy
  real :: temp,temp2
  integer :: m,j,kolumna, druga_kol
  logical :: modul
  
  if(kolumna .eq. 2) druga_kol = 1
  if(kolumna .eq. 1 ) druga_kol = 2
  
  
  if( lewy .LT. prawy ) then
    m = lewy  
    j=lewy+1
     do  while( j .le. prawy)  
     
        if(modul .eqv. .false.) then
	
         if ( input(kolumna,j) .gt. input(kolumna,lewy)) then  ! <--- .gt. - od najwiekszej do najmniejszej
            m = m+1 				! .lt - od najmniejszej do najwiekszej
	    temp = input(kolumna,m)
	    temp2 = input(druga_kol,m)
	    
	    input(kolumna,m) = input(kolumna,j)
	    input(druga_kol,m) = input(druga_kol,j)
	    
	    input(kolumna,j) = temp	    
	    input(druga_kol,j) = temp2
         end if
	end if !modul 
	 
	if(modul .eqv. .true.) then
	
         if ( abs(input(kolumna,j)) .gt. abs(input(kolumna,lewy))) then  ! <--- .gt. - od najwiekszej do najmniejszej
            m = m+1 				! .lt - od najmniejszej do najwiekszej
	    temp = input(kolumna,m)
	    temp2 = input(druga_kol,m)
	    
	    input(kolumna,m) = input(kolumna,j)
	    input(druga_kol,m) = input(druga_kol,j)
	    
	    input(kolumna,j) = temp	    
	    input(druga_kol,j) = temp2   
         end if
	end if !modul 
	 
	 
     j=j+1
     end do
      temp = input(kolumna,lewy)
      temp2 = input(druga_kol,lewy)
      
      input(kolumna,lewy) = input(kolumna,m)
      input(druga_kol,lewy) = input(druga_kol,m)
      input(kolumna,m) = temp	
      input(druga_kol,m) = temp2
      call qsort(input,lewy,m-1,kolumna,modul)
      call qsort(input,m+1,prawy,kolumna,modul)
  end if
   return
  end subroutine qsort

end module sort




! modul obliczajacy transformate falkowa i odwrotna transformate falkowa
! obcinajacy zadana najmniejszych wspolczynnikow
! oraz odszumianie

module DWT
 use sort
 implicit none
 contains 
 
  function TRANSFORM(dane) result(transformata)
  real, dimension(512),intent(in):: dane
  real, dimension(512) :: transformata
  integer :: i, j,k,rozmiar
  
  !tablice na sumy i roznice kolejnych dwoch wyrazow
  real, allocatable, dimension(:) :: sumy, roznice,tymczasowa,tymczasowa2

  allocate(roznice(512))
  allocate(sumy(512))
  sumy = dane
  roznice = dane
  
  print '(XX,A,/)', "obliczanie transformaty ..."
  
  do i= 9, 1,-1  ! 2^9 = 512
  
     rozmiar = size(sumy)
     allocate(tymczasowa(rozmiar/2)) !bedziemy sumowac/roznicowac po 2 wiec potrzeba
     allocate(tymczasowa2(rozmiar/2)) !tablic polowe mniejszych
     
     k = 1
     do j=1,rozmiar,2  
      ! iteracja jest od 1-do rozmiar co 2
      !  tymczasowa ma rozmiar/2 wiec nie mozna
      ! tym samym wskaznikiem
      tymczasowa(k) =  (sumy(j) + sumy(j+1))/sqrt(2.0) !normalizacja
      tymczasowa2(k) =  (sumy(j) - sumy(j+1))/sqrt(2.0) 
      k = k+1
     end do 
    
     ! zmniejszamy o polowe rozmiar tablic 
     deallocate(sumy)
     allocate(sumy(rozmiar/2))
     
     deallocate(roznice)
     allocate(roznice(rozmiar/2))
     
     sumy = tymczasowa
     roznice = tymczasowa2
     
     !roznice dolaczamy (od konca) wektora wynikowego
     do k=1,rozmiar/2
     transformata(rozmiar/2 + k ) = roznice(k)
     end do     
     
     deallocate(tymczasowa)
     deallocate(tymczasowa2)
  
  end do
  
   !tablica transformata zawiera kolejne poziomy roznic
   ! jako pierwszy element trzeba jeszcze wstawic ostateczna sume
   transformata(1) = sumy(1)
   
  return
  end function TRANSFORM

  function INVERSE_TRANSFORM(trans) result(sygnal)
  real, dimension(512),intent(in):: trans
  real, dimension(512) :: sygnal
  integer :: i, j,k,rozmiar
  
  !tablice na sumy i roznice kolejnych dwoch wyrazow
  real, allocatable, dimension(:) :: sumy, roznice,tymczasowa,tymczasowa2,t1,t2

  allocate( tymczasowa(512))
  
  tymczasowa = trans

  print '(XX,A,/)', "obliczanie transformaty odwrotnej ..."
  do i=1, 9
  
  !bierzemy pierwsze 2^i elementow 
   allocate(tymczasowa2(2**i))
    do k=1, 2**i, 1
     tymczasowa2(k) = tymczasowa(k)
    end do
    rozmiar = size(tymczasowa2)
    
    
    allocate(sumy(rozmiar/2))
    allocate(roznice(rozmiar/2))
    
    do k=1, rozmiar/2, 1
     sumy(k) = tymczasowa2(k)  !pierwsza polowa elementow
     roznice(k) = tymczasowa2( rozmiar/2  + k ) !druga polowa elementow
    end do
        
    allocate(t1(rozmiar/2))
    allocate(t2(rozmiar/2))
    
    ! z sum i roznic obliczamy prawdziwe wartosci
   do k=1, rozmiar/2, 1
    t1(k) = (sumy(k) + roznice(k))/sqrt(2.0)
    t2(k) = (sumy(k) - roznice(k))/sqrt(2.0)
   end do
    
    ! teraz trzeba je wstawic do wektora wejsciowego zamiast 
    ! sum/roznic 
    ! parami elementy t1 i t2 
   
    do k=1, rozmiar, 2
     tymczasowa(k) = t1( (k+1)/2 )
    end do
    do k=2, rozmiar, 2
     tymczasowa(k) = t2(k/2)
    end do
    
    deallocate(tymczasowa2)
    deallocate(t1)
    deallocate(t2)
    deallocate(sumy)
    deallocate(roznice)

  end do

   sygnal = tymczasowa
  return
  end function INVERSE_TRANSFORM
  
  
  function OBETNIJ(transf,ile_obciac) result(obciete)
  !funkcja obcinajaca zadana ilosc najmniejszych wspolczynnikow DWT (co do modulu) !
  !a dokladniej mowiac obcina najmniejsze elementy wektora !
  real, dimension(512),intent(in):: transf
  real, dimension(2,512) :: temp
  real, dimension(512) :: obciete
  integer, intent(in) :: ile_obciac
  integer :: i, j,k,rozmiar
  real, allocatable, dimension(:) :: tymczasowa
  
  print '(XX,A,I3,/)',"obcinam: 1/",ile_obciac
  
  ! tworzymy dwie kolumny, pierwsza jest po to zeby zapamietac peirwotna kolejnosc
  do i=1, 512
    temp(1,i) = i
    temp(2,i) = transf(i)
  end do

  !teraz sortujemy wzgledem modulu drugiej (od najwiekszej)
   call qsort(temp,1,512,2,.TRUE.) 
   
  ! i obcinamy (zerujemy) zadana wartosc 
  do i = (512/ile_obciac) +1 ,512
   temp(2,i) = 0
  end do
    
  !teraz trzeba przywrocic pierwotny porzadek 
  ! sortujemy wzgledem pierwszej kolumny
   call qsort(temp,1,512,1,.FALSE.) 
  
  !i bierzemy tylko druga kolumne (z danymi)
  ! trzeba pameitac ze tablica posortowana jest MALEJACO tzn od 512 do 
  do i=1,512
    obciete(i) =  temp(2,513- i)
  end do

  return
  end function OBETNIJ

  ! odszumianie
  
  function DENOISE(transform,rule) result(denoised)
  real, dimension(512),intent(in):: transform
  real, dimension(2,512) :: temp
  real, dimension(512) :: denoised
  real :: sigma,mediana,delta
  integer :: i,rule ! 1=hard rule,  2=mid rule. 3=soft rule
  
  !najpierw trzeba ustalic poziom szumu jako 
  ! sigma = 1/0.6745 * mediana (|w_N/2| ... |W_N|)
  !
  ! zeby policzyc mediane trzeba wziasc moduly drugiej polowy wspolczynnikow
  ! jest 512 czyli polowa= 256, parzysta liczba, wiec mediana bedzie srednia
  ! elementu 256/2 i (256+1)/2
  ! do sortowania mozna uzyc stosowanego wczesniej quicksortu
  ! trzeba tylko przygotowac odpowiednio  dane
    
  do i=1, 512
  temp(1,i) = i
  if( i .lt. 257) temp(2,i) = 0    
  if( i .gt. 256) temp(2,i) = ABS(transform(i)) !bierzemy tylko druga polowe
  end do
   
   if(rule .eq.1)  print '(XX,A)',"denoising... hard rule"
   if(rule .eq.2)  print '(XX,A)',"denoising... mid rule"
   if(rule .eq.3)  print '(XX,A)',"denoising... soft rule"
   
   call qsort(temp,1,512,2,.FALSE.)  !sortowanie od najwiekszej ale to niema znaczenia
   !polowa i tak jest 0
   ! jest posortowane czyli jako pierwsze 256 elementow 
   ! wiec mediana to bedzie srednia z elementu  128 i 129
   mediana = (temp(2,128) + temp(2,129))/2
   sigma = mediana/0.6745

   !poziom obciecia (treshold)
   ! delta = sqrt( 2* log_2(N)) * sigma
   delta = SQRT(2.0* (log(512.0)/log(2.0))) * sigma
   print '(XX,A,F8.6)',"treshold:",delta
   !denoising
   
   !hard rule
   if( rule .eq. 1) then
    do i=1,512
      if( ABS(transform(i)) .lt. delta) denoised(i) = 0
      if( ABS(transform(i)) .ge. delta) denoised(i) = transform(i)
    end do
   end if
  
  ! mid rule
   if( rule .eq. 2) then
    do i=1,512
      if( ABS(transform(i)) .lt. delta) denoised(i) = 0
       if( (ABS(transform(i)) .ge. delta) .AND. (ABS(transform(i)) .lt. 2*delta) ) then
         denoised(i) = 2* SIGN(1.0,transform(i)) * (ABS(transform(i)) - delta )
	end if
      if( ABS(transform(i)) .ge. 2*delta) denoised(i) = transform(i)
    end do
   end if
 
  return
  end function DENOISE
end module DWT






program wavelet
 use DWT
 implicit none

 real, dimension(512) :: data2,transformata, odw,obcieta, denoised_hard, denoised_mid
 real, dimension(512) :: time
 real :: t2
 integer :: i,j,temp
  print *,"*********************************************************************"
  print '(//,XXXX,A,/,XXXX,A,//)',"Dyskretna Transformata Falkowa: falka Haara","Krzysztof Szczepaniec"
  print *,"*********************************************************************"
  print *,
  print *, "otwieram plik z danymi..."
 open(unit=1,file="data04.dat", status="old",action="read",position="rewind")
  print *, "wczytuje..."
  
  !w pliku jest 512 wierwszy, 2 kolumny, mozna to wczytac do macierzy (2x512)
  ! i wziasc druga kolumne, albo odrazu czytac do osobnych
  
  do i=1,512
 read(unit=1,fmt=*) time(i), data2(i)
  end do
 
 transformata = TRANSFORM(data2)
 
 !zapisujemy do pliku transformate
 open(unit=2,file="transformata.txt",status="unknown",action="write")
 do i=1,512
  write(unit=2,fmt=*) time(i),transformata(i)
 end do

 odw = INVERSE_TRANSFORM(transformata)
  !zapisujemy do pliku transformate odwrotna
 open(unit=3,file="odwrotna.txt",status="unknown",action="write")
 do i=1,512
  write(unit=3,fmt="(X,F8.0,XXXX,F14.10)") time(i),odw(i)
 end do
 
 ! obcinaym 1/2 wspolczynnikow najwiekszych co do modulu
 obcieta = OBETNIJ(transformata,2)
 obcieta = INVERSE_TRANSFORM(obcieta)
 open(unit=4,file="obciecie2.txt",status="unknown",action="write")
 do i=1,512
  write(unit=4,fmt="(X,F8.0,XXXX,F14.10)") time(i),obcieta(i)
 end do
 
 ! 1/4
  obcieta = OBETNIJ(transformata,4)
 obcieta = INVERSE_TRANSFORM(obcieta)
 open(unit=4,file="obciecie4.txt",status="unknown",action="write")
 do i=1,512
  write(unit=4,fmt="(X,F8.0,XXXX,F14.10)") time(i),obcieta(i)
 end do
 
 ! 1/8
  obcieta = OBETNIJ(transformata,8)
 obcieta = INVERSE_TRANSFORM(obcieta)
 open(unit=4,file="obciecie8.txt",status="unknown",action="write")
 do i=1,512
  write(unit=4,fmt="(X,F8.0,XXXX,F14.10)") time(i),obcieta(i)
 end do
 
 ! 1/16
 obcieta = OBETNIJ(transformata,16)
 obcieta = INVERSE_TRANSFORM(obcieta)
 open(unit=4,file="obciecie16.txt",status="unknown",action="write")
 do i=1,512
  write(unit=4,fmt="(X,F8.0,XXXX,F14.10)") time(i),obcieta(i)
 end do
 
 denoised_hard = DENOISE(transformata,1)
 denoised_hard = INVERSE_TRANSFORM(denoised_hard)
  open(unit=5,file="denoised_hard.txt",status="unknown",action="write")
 do i=1,512
  write(unit=5,fmt="(X,F8.0,XXXX,F14.10)") time(i),denoised_hard(i)
 end do
 
 denoised_mid = DENOISE(transformata,2)
 denoised_mid = INVERSE_TRANSFORM(denoised_mid)
  open(unit=6,file="denoised_mid.txt",status="unknown",action="write")
 do i=1,512
  write(unit=6,fmt="(X,F8.0,XXXX,F14.10)") time(i),denoised_mid(i)
 end do
 
 
 end program wavelet