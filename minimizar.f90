program minimizar

Implicit none

!Variables declaration:
Integer, parameter :: particles = 4, iteraction = 10
Integer :: i, j, k
Real, allocatable :: w, eta_1, eta_2, x(:), v(:)
Real :: pbest(particles), fitness(particles), gbest, rnd_1, rnd_2

!code:

!Abrir arquivo de saída
open(unit = 1, file = 'minimizar.txt', status = 'replace', action = 'Write')

!Inserir parametros iniciais do programa
write(1,*) 'Parâmetros iniciais do programa:'
w = 0.2
write(1,*) 'w:', w
eta_1 = 0.3
write(1,*) 'Eta_1:', Eta_1
eta_2 = 0.5
write(1,*) 'Eta_2:', Eta_2
x = [5.0, 6.0, 8.5, 11.0]
write(1,*) 'Subdomínio:', x
v = [0.0, 0.0, 0.0, 0.0]
pbest = [0.0, 0.0, 0.0, 0.0]
call random_number(rnd_1)
call random_number(rnd_2)
write(1,*) 'Termo aleatório rnd_1:', rnd_1
write(1,*) 'Termo aleatório rnd_2:', rnd_2

!Realizar iterações
i = 0
Do while (i < iteraction)
    write(1,*) 'Iteração:', i
    do j = 1, particles
        write(1,*) 'Partícula:', j
        !Calcular o fitness e encontrar gbest
        fitness(j) = cos(x(j)) + x(j)/5
        write(1,*) 'fitness:', fitness(j)
        If (j == 1) then
            gbest = fitness(j)
        Else if (fitness(j) < fitness(j-1)) then
            gbest = fitness(j)
        End if
        !Encontrar pbest
        If (x(j) > pbest(j)) then
            pbest(j) = x(j)
        End if
        write(1,*) 'pbest:', pbest(j)
    End do
    write(1,*) 'gbest:', gbest
    Do j = 1,particles
        write(1,*) 'Particula:',j,':'
        !Calcular velocidade v
        v(j) = w*v(j) + eta_1 * rnd_1 * (pbest(j) - x(j)) + eta_2 * rnd_2 * (gbest - x(j))
        write(1,*) 'Velocidade:', v(j)
        !Atualizar coordenadas
        x(j) = x(j) + v(j)
        write(1,*) 'posição atualizada:', x(j)
    End do
    !Atualizar a iteração
    i = i + 1
End do


End program minimizar