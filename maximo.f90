program maximo

Implicit none

Integer, parameter :: n = 6, m = 2, iterations = 4
Integer :: i, j, k, l
Integer :: r_1, r_2, r_3, hold
Real, parameter :: Pcr = 0.7
Real :: Coordinates(n,m), fitness(n), temp(m), u(m), c_best(m)
Real :: S, F, fit_best, aux


!Open external files and read data
open(unit = 1, file ='coordenadas_2.txt', status = 'old', action = 'Read')
open(unit = 2, file ='maximo.txt', status = 'replace', action = 'write')


!Define scale factor
i = 0
Do while (i == 0)
    call random_number(F)
    if (F > 0.1) i = 1
End do

!Program parameters
Write(2,'(a)') "Parâmetros do programa:"
Write(2,*) ""
Write(2,'(x,a,x,F8.4)') "Probabilidade de crossover:",Pcr
Write(2,'(x,a,x,I2)') "Número de iterções:",iterations
Write(2,'(x,a,x,F8.4)') "Fator de escala:",F
Write(2,'(x,a)') "População inicial:"
Do i = 1,n 
    Read(1,*) coordinates(i,:)
    Write(2,'(3x,2(F8.4,x))') coordinates(i,:)
End do
Write(2,*) ""



!Start iterations
Do i = 1,iterations 
    Write(2,'(a,x,I2)') "Iteração:",i
    Write(2,*) ""
    !Loop over all the coordinates
    Do j = 1,n
        Write(2,'(x,a,x,I2)') "Indivíduo:",j  
        Write(2,*) ""
        temp = coordinates(j,:)
        !Calculate the fitness
        fitness = sin(coordinates(:,1)/3) + cos(6*coordinates(:,2)/5) + exp(coordinates(:,2) / 5) + 2 
        Write(2,'(2x,a,x,6(f6.4,x))') "Fitness das coordenadas:", fitness
        Write(2,*) ""
        !Find the best particle
        hold = 1
        fit_best = fitness(1)
        Do k = 2,n
            if (fitness(k) > fit_best) then 
                hold = k
                fit_best = fitness(k)
            End if
        End do
        c_best = coordinates(hold,:)
        !Find the random integers
        k = 0
        Do while (k == 0)
            call Random_int(1,n,r_1)
            Call Random_int(1,n,r_2)
            if (r_1 /= r_2 .and. r_1 /= j .and. r_2 /= j) k = 1
        End do
        Write(2,'(2x,a)') "Valores aleatórios r1 e r2:"
        Write(2,'(4x,a,x,i1)') "r1:",r_1
        Write(2,'(4x,a,x,i1)') "r2:",r_2
        Write(2,*) ""
        u = c_best + F * (coordinates(r_1,:) - coordinates(r_2,:))
        Write(2,'(2x,a,x,2(f8.4,x))') "Coordenadas do vetor de teste:", u
        Write(2,*) ""
        Write(2,'(2x,a)') "Testar probabilidade de realizar troca"
        Do k = 1,m 
            call random_number(s)
            Write(2,'(4x,a,x,f5.2)') "Valor S:",s
            if (s <= Pcr) then
                Write(2,'(4x,a,I2,x,a)') "Nova coordenada",k,"Recebe valor do vetor teste"
                temp(k) = u(k)
            End if
        End do
        Aux = sin(temp(1)/3) + cos(6*temp(2)/5) + exp(temp(2)/ 5) + 2 
        Write(2,*) ""
        if (Aux > fitness(j)) then
            Coordinates(j,:) = temp 
            Write(2,'(2x,a)') "Fitness do conjunto de novas coordenadas melhor que anterior, atualiza-se as coordenadas:"
        Else
            Write(2,'(2x,a)') "Fitness do conjunto de novas coordenadas pior que anterior, não se atualiza as coordenadas"
        End if
        Write(2,*) ""
    End do
    Write(2,'(a)') "Novo conjunto de coordenadas:"
    Write(2,*) ""
    Do k = 1,n
        Write(2,'(5x,2(f5.1, x))') coordinates(k,:)
    End do
    Write(2,*) ""
End do


Contains

    Subroutine Random_int(inicio,fim,Inteiro)

        Integer, intent(in) :: inicio, fim
        Integer, intent(out) :: Inteiro
        Real :: u 

        Call random_number(u)
        Inteiro = inicio + floor((fim + 1 - inicio) * u)

    End subroutine Random_int 

End program maximo