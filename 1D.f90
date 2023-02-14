program Q1d
implicit none
integer, parameter :: n = 100
integer Lx, Ly, i, j, k, rmaximo,rminimo,p
real*8 Dx, Dy, delta, V(n,n), deltax, deltay, a,dVmaximo, Vnovo(n,n)
real*8 r, dr, dteta,tj,ri,m,mt
!
character*64 fname
fname=('questaoD.dat')
open(15,file=fname,status='unknown')

rminimo=1.
rmaximo=5.
dr=(rmaximo-rminimo)/99.
dteta=(8*atan(1.))/99.
delta=10.d-6
m=5.8
V(1,:)=0.
do p=1,100
mt=m*p*dteta
V(100,p)=cos(mt)
end do
Vnovo=V

do k=1, 10000
dVmaximo=0.d0
do i=2,99
do j=1,100
ri=rminimo+i*dr
tj=j*dteta
a=(ri**2)*(dr**2)*(dteta**2)/(2*((ri**2)*(dteta**2)+(dr**2)))
Vnovo(i,j)=a*(((V(i+1,j)-V(i-1,j))/(2*ri*dr))+((V(i+1,j)+V(i-1,j))/(dr**2))+((V(i,j+1)+V(i,j-1))/((ri**2)*(dteta**2))))

Vnovo(i,100)=a*(((V(i+1,100)-V(i-1,100))/(2*ri*dr))+((V(i+1,100)+V(i-1,100))/(dr**2))+((V(i,1)+V(i,99))/((ri**2)*(dteta**2))))
dVmaximo=max(dVmaximo,abs(V(i,j)-Vnovo(i,j)))
!quando j=1
Vnovo(i,1)=a*(((V(i+1,1)-V(i-1,1))/(2*ri*dr))+((V(i+1,1)+V(i-1,1))/(dr**2))+((V(i,2)+V(i,100))/((ri**2)*(dteta**2))))
end do
end do
if (dVmaximo.lt.delta) exit
V=Vnovo
end do
!plotando os V finais
do i=1, n
do j=1, n
ri=rminimo+(i-1)*dr
tj=(j-1)*dteta
write(15,*) ri*dcos(tj), ri*dsin(tj), V(i,j)
end do
write(15,*)
enddo
close(15)
end program
