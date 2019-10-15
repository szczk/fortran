!
! by Krzysztof Szczepaniec
! 21.06.09

! program rozwiazuje numerycznie dwupunktowy problem brzegowy
!   y''(t) - y(t)^2 + 1 = 0
!   y(1) = 0
!   y(2) = 1
! metoda strzelania: 
! zamiana parametru koncowego y(1) = 1
! na drugi parametr poczatkowy y'(0) = Z
! rozwiazujemy i sprawdzamy ile sie rozni wartosc koncowa y(1) od zadanej
! i na podstawie tego (trzeba policzyc drugi uklad rownan rozniczkowych) 
! poprawiamy Z i liczymy od nowa



! rozwiazywanie rownan rozniczkowych:
! klasyczna 4-krokowa metoda Rungego - Kutty

! dobor i iteracyjne poprawianie
! drugiego parametru poczatkowego:  
! metoda Newtona





program metoda_strzelania
 implicit none
 ! wolne parametry (do ustalenia wlasnego) 
 real :: h = 1.0/128.0  ! krok metody R-K 
 real :: z0 = 1.0;  ! poczatkowa wartosc Z
 
 real :: h2 = 1.0/128.0  ! krok drugiej metody RK (stosowanej przed met. Newtona)
 
 real :: y_0 = 0.0  ! warunek poczatkowy y(0)
 real :: y_1 = 1.0  ! warunek koncowy y(1)
 
 
 integer :: i=1  ! licznik iteracji 
 
 real, dimension(2) :: Y   ! wektor  w rownaniu rozniczkowym
 ! skladowe; y(t), x(t), gdzie x = y'(t)
 ! (czyli zamiana rownania rozniczkowego
 ! drugiego stopnia na układ równan pierwszego stopnia)
 
 real, dimension(2) :: W ! wektor do drugiego rownania rozniczkowego
 real, dimension(2) :: k1,k2,k3,k4 ! wektory pomocnicze w met. R-K
 
 real :: t = 0.0  ! zmienna niezalezna
 real :: t2 = 0.0
 
 real :: roznicaZ  = 1.0
 
 real :: z ! druga wartosc poczatkowa ( x(0) = y'(0) = Z )
 
 z = z0 ! zaczynamy od zadanego wczesniej
  
  print '(/,XXXXXX,A)', "Krzysztof Szczepaniec, 21.06.2009"
  print '(/,XXXXXX,A)',"FORTRAN90: program rozwiazujacy rownanie rozniczkowe:"
  print '(/,XXXXXX,A,/,XXXXXX,A,/,XXXXXX,A,//)', "y''(t) - y(t)^2 + 1 = 0" ,  "y(0) = 0 ",  "y(1) = 1 "


  do while( roznicaZ .gt. 0.000000000000001)  ! glowna petla
  ! liczymy dla zadanego Z i sprawdzamy ile sie rozni y(2) od zadanego
  ! na podstawie tego poprawiamy z metoda newtona Z_k+1 = Z_k - roznicaZ
  ! gdzie Z = f(k)/f'(k)
  ! petla liczy az poprawki do Z beda mniejsze od zadanej wielkosci
  

  ! zaczynamy od wartosci poczatkowych 
  Y(1) = y_0
  Y(2) = Z
  t = 0.0
  
  do while(t .le. 1.0) ! pierwsze rownanie rozniczkowe
    k1(1) = Y(2);
    k1(2) = Y(1)*Y(1) - 1.0;
    
    k2(1) = Y(2) + (h/2.0) * k1(2);
    k2(2) = (Y(1) + (h/2.0) * k1(1))**2.0 - 1.0;
       
    k3(1) = Y(2) + (h/2.0) * k2(2);
    k3(2) = (Y(1) + (h/2.0) * k2(1))**2.0 - 1.0 ;
    
    k4(1) = Y(2) + h * k3(2);
    k4(2) = (Y(1) + h * k3(1))**2.0 -1.0;
    
    Y(1) = Y(1) + h* ((k1(1)/6.0)  + (k2(1)/3.0) + (k3(1)/3.0) + (k4(1)/6.0)); !y
    Y(2) = Y(2) + h* ((k1(2)/6.0)  + (k2(2)/3.0) + (k3(2)/3.0) + (k4(2)/6.0)); !x = y' 
  
    t = t+ h
  end do
  
  ! poprawianie parametru Z
  
  W(1) = 0.0
  W(2) = 1.0
  t2 = 0.0
  
  do while(t2 .le. 1.0) 
    k1(1) = W(2);
    k1(2) = W(1)*(2*Y(1) - 1);
    
    k2(1) = W(2) + (h2/2.0) * k1(2);
    k2(2) = (2.0*Y(1) - 1) * ( W(1) + (h2/2.0) * k1(1) );
 
    k3(1) = W(2) + (h2/2.0) * k2(2);
    k3(2) = (2.0*Y(1) - 1) * ( W(1) + (h2/2.0) * k2(1) ); 
        
    k4(1) = W(2) + h2 * k3(2);
    k4(2) = (2.0*Y(1) - 1) * ( W(1) + h2 * k3(1) ); 
    
    W(1) = W(1) + h2* ((k1(1)/6.0)  + (k2(1)/3.0) + (k3(1)/3.0) + (k4(1)/6.0)); 
    W(2) = W(2) + h2* ((k1(2)/6.0)  + (k2(2)/3.0) + (k3(2)/3.0) + (k4(2)/6.0)); 
  
    t2 = t2+ h2
  end do
  
 
  
  print '(XXXX,A,I2,XXXXXX,A,F15.13 ,XXX,A,F22.20,XXX,A,F6.4,XXX,A,F15.13)',"i: ",i,"z:", &
  Z,"y(1):",Y(1), "y'(1):",Y(2), "dZ:",ABS( ( (Y(1) - y_1 )/W(1) ) )
  
  
  
  Z = Z - (( Y(1) - y_1 )/W(1))
  i = i+1
  roznicaZ = ABS( ( (Y(1) - y_1 )/W(1) ) )
  
  end do !koniec while(roznicaZ > ...)
  
  



end program metoda_strzelania
