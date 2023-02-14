program Integral_1a
implicit none
real*8 f,a,b,h,sum,x,q,k
integer i,j,l,N,V
external f
print*, "de os intervalos de integra‡Æo"
read*, a,b
print*, "escolha o metodo de integra‡Æo"
print*," 1 - metodo dos trap‚zios"
print*," 1 - metodo de simpsons"
read*, q

sum =0.
V = 2.
if ( q==1)then
print*, "de a quantidade de intervalos"
read*, N

!metodo dos trap‚zios
h = (b-a)/N
sum = (f(a)+f(b))*h/2
do i=1, N-1
sum = sum + f(a*(i*h))*h
end do
else

!  Metodo de simpsons
do l =1, 1000
print *, " de o valor de intervalos"
read*, N
if(mod(N,V)==0) then
exit
else
print*, "Erro, N deve ser par"
end if
end do

h = (b-a)/N
sum = (f(a)+ f(b))*h/3
do j=1, N-1
if (mod( j,V)==0) then
k=0.5
else
k=1.
end if
sum = sum + ((h/3)*(f(a*j*h)*(4*k)))
end do
end if
print*, sum
pause
end
real*8 function f(x)
implicit none
real*8 x
f = x**x
return
end




