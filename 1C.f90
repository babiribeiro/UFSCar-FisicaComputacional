program Q1c
implicit none
integer, parameter :: n = 100
integer Lx, Ly, i,j,k, rmaximo, rminimo
real*8 Dx, Dy, Delta, V(n,n), deltaX, deltaY, a, dVmaximo, Vnovo (n,n)
real*8 r, dr, dteta,tj,ri
!abrindo programa
character*64 fname
fname=('LaPolar.dat')
open(15,file=fname,status='unknown')
!condi‡oes iniciais
rmin=1.
rmax=5.
dr=(rmax-rmin)/99.
dteta=(8*atan(1.))/99.
Lx=1.
Ly=1.
delta=10.d-6
Dx=Lx/100.
Dy=Ly/100.
V(1,:)=1.
V(100,:)=5.
Vnovo = V
!Calculando V , DV minimo
do k=1, 10000
dVmax=0.d0
do i=2,99
do j=1,100
ri=rmin+i*dr
tj=j*dteta
a=(ri**2)*(dr**2)*(dteta**2)/(2*((ri**2)*(dt eta**2)+(dr**2))))
Vnovo(i,j)=a*(((V(i+1,j)-V(i-1,j))/(2*ri*dr))+((V(i+1,j)+V(i-1,j))/(dr**2))+((V(i,j+1)+V(i,j-1))/((ri**2)*(dteta**2))))
dVmaximo=max(dVmaximo,abs(V(i,j)-Vnovo(i,j)))
!
Vnovo(i,100)=a*(((V(i+1,100)-V(i-1,100))/(2*ri*dr))+((V(i+1,100)+V(i-1,100))/(dr**2))+((V(i,1)+V(i,99))/((ri**2)*(dteta**2))))
dVmaximo=max(dVmaximo,abs(V(i,j)-Vnovo(i,j))
!
Vnovo(i,1)=a*(((V(i+1,1)-V(i-1,1))/(2*ri*dr))+((V(i+1,1)+V(i-1,1))/(dr**2))+((V(i,2)+V(i,100))/((ri**2)*(dteta**2))))
end do
end do
if (dVmaximo.lt.delta) exit
V=Vnovo
end do
!Plotando
do i=1, n
do j=1, n
ri=rmin+(i-1)*dr
tj=(j-1)*dteta
write(15,*) ri*dcos(tj), ri*dsin(tj), V(i,j)
end do
write(15,*)
enddo
close(15)
end program

