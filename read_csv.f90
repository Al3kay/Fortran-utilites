program main
character :: dummy*1,nomc*5,nombre*32,mes*6
real :: valor
open(1,file='data.csv',status='OLD')
read(1,*) dummy
read(1,*) nomc,nombre,valor,mes
print*,nomc,nombre,valor,mes
close(1)
end program
