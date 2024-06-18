program mochila

Implicit none

!Variables declaration:
Integer, parameter :: particles = 3, iteraction = 4, item = 15, Peso = 275
Integer :: i, j, k, l, m, n
Integer :: rnd, c, pgbest, trocas, e
Integer :: ierror
Character(len=20) :: err_strg
Integer :: cpbest(particles), ctrocas(particles), v(particles), hold(item)
Integer :: pbest(particles,item), gbest(item), p(particles,item), val(item), weight(item)
Real :: w, eta_1, eta_2, rnd_1, rnd_2, gama, Aux_1, Aux_2
Real :: fitness(particles)

!code:

!Abrir unidade para saída de dados
open(unit = 1, file = 'mochila.txt', status = 'replace', action = 'Write', iostat = ierror, iomsg = err_strg)

!Randomizar particulas iniciais
n = 0
m = 1
write(1,*) 'Dados iniciais das partículas'
Do i = 1, particles
    Do j = 1,item
        call random(m,n,rnd)
        p(i,j) = rnd
        if (rnd == 1) cpbest(i) = cpbest(i) + 1
    End do
    write(1,*) i,':',p(i,:)
End do

!Inserir parametros iniciais do programa
write(1,*) 'Contantes e dados do programa:'
w = 0.2
write(1,*) 'w:', w
eta_1 = 0.3
write(1,*) 'Eta_1:', Eta_1
eta_2 = 0.5
write(1,*) 'Eta_2:', Eta_2
pbest = p
weight = [63.0, 21.0, 2.0, 32.0, 13.0, 80.0, 19.0, 37.0, 56.0, 41.0, 14.0, 8.0, 32.0, 42.0, 7.0] 
write(1,*) 'Peso dos objetos:', weight
val = [13.0, 2.0, 20.0, 10.0, 7.0, 14.0, 7.0, 2.0, 2.0, 4.0, 16.0, 17.0, 17.0, 3.0, 21.0]
write(1,*) 'Valor dos objetos:', val
call random_number(rnd_1)
call random_number(rnd_2)
write(1,*) 'Valor aleátorio 1:', rnd_1
write(1,*) 'Valor aleátorio 2:', rnd_2

!Calcular gama
gama = 0
do i = 1, item
    gama = gama + val(i)
End do

!Realizar iterações
i = 0
n = 1
ctrocas = 0
Do while (i < iteraction)
    write(1,*) 'Iteração:', i
    do j = 1, particles
        !Calcular o fitness
        Aux_1 = 0
        Aux_2 = 0
        Do k = 1, item
            Aux_1 = Aux_1 + p(j,k) * weight(k)
            Aux_2 = Aux_2 + p(j,k) * val(k)
        End do
        Aux_2 = Aux_2 - Peso
        If (Aux_2 < 0) Aux_2 = 0
        fitness(j) = Aux_1 + gama * Aux_2 
        write(1,*) 'fitness',j,':',fitness(j)
        !Encontrar a partícula gbest 
        If (j == 1) then
            pgbest = 1
        Else if (fitness(j) > fitness(j-1)) then
            pgbest = j
        End if
    End do
    !Atualizar valor do gbest da iteração
    gbest = p(pgbest,:)
    write(1,*) 'gbest:', gbest
    v = 0
    Do j = 1,particles
        write(1,*) 'partícula',j,':'
        !Calcular pbest
        Do k = 1,item
            c = c + p(j,m)
        End do
        If (c > cpbest(j)) then
            pbest(j,:) = p(j,:)
            cpbest(j) = c
        End if
        write(1,*) 'pbest da partícula:', pbest(j,:)
        !calcular v:
        if (j /= pgbest) then
            !Calcular quantidade de trocas a serem realizadas:
            !primeiro item de v
            trocas = w * v(j)
            v(j) = v(j) + trocas
            !segundo item de v:
            trocas = 0
            Do k = 1, item
                trocas = trocas + abs(pbest(j,k) - p(j,k))
            End do
            trocas = eta_1 * rnd_1 * trocas
            v(j) = v(j) + trocas
            !Terceiro item de v:
            call random(item,n,trocas)
            trocas = eta_2 * rnd_2 * trocas
            v(j) = v(j) + trocas
            if (v(j) == 0) v(j) = 1
            write(1,*) 'Número de trocas a serem realizadas:', v(j)
            !Igualar trocas a gbest
            e = 0
            hold = 0
            Do k = 1,v(j)
                !Definir quais termos serão substituídos
                l = 0
                e = e + 1
                If (k == 1) then 
                    call random(item,n,rnd)
                    hold(1) = rnd
                Else
                    Do while (l == 0)
                        call random(item,n,rnd)
                        hold(k) = rnd
                        l = 1
                        Do m = 1,(e - 1)
                            If (rnd == hold(m)) l = 0
                        End do
                    End do
                End if
                Do l = 1,e
                    write(1,*) 'itens a serem substituídos:', hold(k)
                End do
                !Igualar os termos a serem substituídos a gbest
                p(j,k) = gbest(k)
                write(1,*) 'Nova configuração da partícula',p(j,:)
            End do
        End if
    End do
    !Atualizar a iteração
    i = i + 1
End do

contains

subroutine random(total, inicial, rnd)

    Real :: u
    Integer, intent(in) :: total, inicial
    Integer, intent(out) :: rnd

    call random_number(u)
    rnd = inicial + floor((total + 1 - inicial) * u)

End subroutine random

End program mochila