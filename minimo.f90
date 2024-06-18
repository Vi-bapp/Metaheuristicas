program minimo

Implicit none

Integer, parameter :: n = 6, m = 2, iterations = 4
Integer :: i, j, k, l
Integer :: r_1, r_2, r_3, hold
Real, parameter :: Pcr = 0.7
Real :: Coordinates(n,m), fitness(n), temp(m), u(m), c_best(m)
Real :: S, F, fit_best, aux


!Open external files and read data
open(unit = 1, file ='coordenadas_1.txt', status = 'old', action = 'Read')
open(unit = 2, file ='minimo.txt', status = 'old', action = 'Write')

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
    Write(2,'(a,x,i1)') "iteration:",i
    write(2,*) ""
    !Loop over all the coordinates
    Do j = 1,n 
        Write(2,'(x,a,x,i1)') "Indivíduo:",j
        write(2,*) ""
        temp = coordinates(j,:)
        !Calculate the fitness
        fitness = sin(coordinates(:,1)) + cos(coordinates(:,2)) + (coordinates(:,2) / 5) + 4 
        Write(2,'(2x,a,x,6(f8.4,x))') "Fitness:",fitness
        write(2,*) ""
        !Find the best particle
        hold = 1
        fit_best = fitness(1)
        Do k = 2,n
            if (fitness(k) < fit_best) then 
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
        Write(2,'(2x,a)') "Valores de r1 e r2:"
        Write(2,'(4x,a,x,i2)') "r1:",r_1 
        Write(2,'(4x,a,x,i2)') "r2:",r_2
        write(2,*) ""
        u = c_best + F * (coordinates(r_1,:) - coordinates(r_2,:))
        Write(2,'(2x,a,x,2(f8.4,x))') "Coordenadas do vetor teste:",u
        write(2,*) ""
        Write(2,'(2x,a)') "Testar probabilidade de ocorrer mutação:"
        Do k = 1,m 
            call random_number(s)
            Write(2,'(4x,a,x,f4.2)') "Valor aleatório s:",s
            if (s <= Pcr) then
                Write(2,'(5x,a)') "Ocorre mutação"
                temp(k) = u(k)
            Else
                Write(2,'(5x,a)') "Não ocorre no mutação"
            End if
        End do
        write(2,*) ""
        Write(2,'(2x,a,x,2(f8.4,x))') "Novas coordenadas:",temp
        Aux = sin(temp(1)) + cos(temp(2)) + (temp(2) / 5) + 4
        if (Aux < fitness(j)) then
            Coordinates(j,:) = temp 
            Write(2,'(4x,a)') "Fitness anterior menor do que coordenadas novas, portanto atualiza-se as coordenadas"
        Else 
            Write(2,'(4x,a)') "Fitness anterior maior do que coordenadas, portanto não se atualiza as coordenadas"
        End if
        write(2,*) ""
    End do
    write(2,'(x,a)') "Vetor de coordenadas atualizado:"
    Do k = 1,n 
        Write(2,'(x,2(f8.4,x))') coordinates(k,:)
    End do
    write(2,*) ""
End do


Contains

    Subroutine Random_int(inicio,fim,Inteiro)

        Integer, intent(in) :: inicio, fim
        Integer, intent(out) :: Inteiro
        Real :: u 

        Call random_number(u)
        Inteiro = inicio + floor((fim + 1 - inicio) * u)

    End subroutine Random_int 

End program minimo