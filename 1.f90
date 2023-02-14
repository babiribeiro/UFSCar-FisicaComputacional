program meuOscilador
implicit none
integer, parameter :: ndim=2
double precision :: y(ndim), g(ndim), dt ,t ,tmax, k ,m
integer it, itmax
character*64 fname

!abrindo arquivo
fname = ('1a.dat')
open (15,file=fname,status = 'unknown')
!
tmax = 20.
itmax= 200
dt=tmax/itmax
y(1)= 1.; y(2) =0.0 ; t= 0.0
do it= 1,itmax
call calcG(g,y,t,ndim)
y(:) = y(:) + g(:)*dt
t=t+dt
write(15,'(f15.8,f15.8)') t, y(1)
end do
end

subroutine calcG (g,y,t,ndim)
implicit none
integer ndim
double precision :: g(ndim), y(ndim), t,k,m
k=1.
m=1.
g(1) = y(2)
g(2) = (-k/m)*y(1)
return
end subroutine calcG










