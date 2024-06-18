program caixeiro

Implicit none

!Variables declaration:

Integer, parameter :: points = 10, L = 1, V = 2, It = 4
Integer :: i,j,k,m,n
Integer :: ioerror, nsucess,switch_1, switch_2
Integer :: solution_0(points+1), solution_1(points+1), solution_best(points+1)
Real :: alpha, T, P, rnd, fbest
Real :: dist(points,points), coordinates(points,2), f(2)
Character(len=20) :: err_strng

!Code

open(unit = 1, file = 'Caixeiro.txt', Status = 'old', Action = 'read', iostat = ioerror, iomsg = err_strng)
open(unit = 2, file = 'Out_1.txt', Status = 'Replace', Action = 'write')
Do i = 1, points
    Read(1,*) coordinates(i,:)
End do

!Calcular matriz de distâncias entre cidades
call distance(coordinates,dist)

!Inicialização das variáveis
Write(2,*) "Parâmetros inciais"
T = 10
Write(2,*) "T",T
alpha = 0.9
Write(2,*) "Taxa de aprendizado",alpha
Solution_0 = [1,9,7,10,3,4,8,5,2,6,1]
Write(2,*) "Solução inicial",solution_0
solution_best = solution_0

!Carcular distância da solução inicial
Do j = 2,points
    f(1) = f(1) + dist(solution_0(j - 1), solution_0(j))
End do
fbest = f(1)
Write(2,*) "Função da solução inicial:",f(1)

!Loop de iterações
i = 1
n = 1
Do while (i <= It)
    Write(2,*) "Iteração:",i
    nsucess = 0
    m = 0
    Do while (m <= V)
        m = m + 1
        Write(2,*) "Tentativa:",m
        !Gerar perturbação na solução
        call Random_integer(points,n,switch_1)
        k = 0
        Do while (k == 0)
            call Random_integer(points,n,switch_2)
            k = 1
            if (switch_2 == switch_1) k = 0
        End do
        Do j = 1, points+1
            if (solution_0(j) == switch_1) then
                solution_1(j) = switch_2
            Else if (solution_0(j) == switch_2) then
                solution_1(j) = switch_1
            Else
                solution_1(j) = solution_0(j)
            End if
        End do
        Write(2,*) "Nova solução =", solution_1
        !Calcular nova solução
        f(2) = 0
        Do j = 1,points
            f(2) = f(2) + dist(solution_1(j), solution_1(j+1))
        End do
        Write(2,*) "Distância da solução com perturbação:",f(2)
        !Calcular probabilidade
        call random_number(rnd)
        Write(2,*) "Valor aleatório",rnd
        P = exp(-(f(2) - f(1))/T)
        Write(2,*) "Probabilidade:",P
        !Verificar aceitação ou não da solução
        Write(2,*) "Delta E =",(f(2) - f(1))
        if ((f(2) - f(1) <= 0) .or. P > rnd) then
            nsucess = nsucess + 1
            solution_0 = solution_1
            f(1) = f(2)
            T = alpha * T
            Write(2,*) "Atualizaçã da temperatura:",T
            Write(2,*) "Número de sucessos:",nsucess
            If(fbest < f(2)) then
                fbest = f(2)
                solution_best = solution_1
            End if
        End if
        if(nsucess == L) Exit
    End do
    Write(2,*) "O número máximo de sucessos ou tentativas da iteração foi alcançado."
    !Atualizar iteração
    i = i + 1
End do

Write(2,*) "Melhor solução:",solution_best

Contains

subroutine distance(coordinates,dist)

    Integer :: i,j
    Real, intent(in) :: coordinates(:,:)
    Real, intent(out) :: dist(:,:)

    Do i = 1,size(dist,1)
        Do j = 1,size(dist,2)
            dist(i,j) = sqrt((coordinates(i,1) - coordinates(j,1)) ** 2 + (coordinates(i,2) - coordinates(j,2)) ** 2)
        End do
    End do

End subroutine distance

Subroutine Random_integer(m,n,rnd)

    Integer, intent(in) :: m,n
    Integer, intent(out) :: rnd
    Real :: u

    Call random_number(u)
    rnd = n + floor((m + 1 - n) * u)

End subroutine Random_integer

End program Caixeiro