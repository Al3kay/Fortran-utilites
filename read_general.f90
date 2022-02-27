program main
implicit none

character :: dummy*1,numc*5,nombre*32,buffer*1024,mes*12,delimiter*1
real :: valor
integer :: pos,stride
delimiter=':'
open(1,file='data.txt', status='OLD')
read(1,*) dummy

read(1,'(A)') buffer

!--- INICIA------
pos=index(buffer,delimiter)
numc=buffer(1:pos-1)
stride=pos

!--- INTERNOS ------
pos=index(buffer(stride+1:),delimiter)
nombre=buffer(stride+1:pos+stride-1)
stride=pos+stride

pos=index(buffer(stride+1:),delimiter)
read(buffer(stride+1:pos+stride-1),*) valor
stride=pos+stride

!--- FINALIZA ------
mes=buffer(stride+1:)


close(1)

print*, numc,nombre,valor,mes
end program
