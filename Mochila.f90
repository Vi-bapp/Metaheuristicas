program mochila

Implicit none

!Variables declaration:

Integer, parameter :: points = 15, L = 1, V = 3, It = 4, P = 275
Integer :: i,j,k,m,n
Integer :: ioerror, nsucess, switch, rnd_int
Integer :: solution_0(points), solution_1(points), Val(points), weight(points), solution_best(points)
Real :: alpha, T, Prob, rnd, fbest, Delt_E
Real :: f(2)
Character(len=20) :: err_strng

!Code

!Abrir saída de dados
open(unit = 1, file = 'out2.txt', Status = 'replace', action = 'write')

!Inicializar valores
Write(1,*) "Valores iniciais do programa"
Write(1,*) "Peso total da mochila",P
Write(1,*) "Numero maximo de tentativas",V
Write(1,*) "Número maximo de sucessos aceitos",L
T = 19
Write(1,*) "Temperatura inicial do problema",T
alpha = 0.9
Write(1,*) "Taxa de aprendizagem alpha",alpha
Val = [13,2,20,10,7,14,7,2,2,4,16,17,17,3,21]
Write(1,*) "Valor dos objetos",val
Weight = [63,21,2,32,13,80,19,37,56,41,14,8,32,42,7]
Write(1,*) "Peso dos objetos",weight

!Criar solução inicial aleatória
Do i = 1,points
    n = 0
    m = 1
    call Random_integer(m,n,rnd_int)
    solution_0(i) = rnd_int
End do
write(1,*) "Solução inicial",solution_0
solution_best = solution_0
solution_1 = solution_0

!Calcular função
If ((dot_product(solution_0,weight) - P) > 0) then
    f(1) = dot_product(solution_0,val) - sum(val) * (dot_product(solution_0,weight) - P)
Else
    f(1) = dot_product(solution_0,val)
End if
write(1,*) "Função da solução inicial",f(1)
fbest = f(1)

!Loops de iterações
i = 1
n = 1
Do while (i <= It)
    Write(1,*) "Iteração",i
    nsucess = 0
    m = 0
    Do while ((m < V))
        Write(1,*) "Numero de tentativas",(m+1)
        !Gerar perturbação na solução
        call Random_integer(points,n,switch)
        Write(1,*) "Troca item",switch
        Do j = 1,points
            if (j == switch .and. solution_0(j) == 1) solution_1(j) = 0
            if (j == switch .and. solution_0(j) == 0) solution_1(j) = 1
        End do
        Write(1,*) "Nova solução",solution_1
        !Calcular função da nova solução
        If ((dot_product(solution_1,weight) - P) > 0) then
            f(2) = dot_product(solution_1,val) - sum(val) * (dot_product(solution_1,weight) - P)
        Else
            f(2) = dot_product(solution_1,val)
        End if
        Write(1,*) "Função da nova solução",f(2)
        !Calcular probabilidade
        call random_number(rnd)
        Write(1,*) "Valor aleatório da tentativa",rnd
        Delt_E = (F(2) - F(1))
        Write(1,*) "Delta E",delt_E
        Prob = exp(-(delt_E/T))
        Write(1,*) "Probabilidade",Prob
        !Verificar aceitação ou não da solução
        if (f(2) - f(1) <= 0 .or. Prob < rnd) then
            nsucess = nsucess + 1
            solution_0 = solution_1
            T = alpha * T
            f(1) = f(2)
            Write(1,*) "Numero de sucessos",nsucess
            Write(1,*) "Atualização da temperatura",T
            if(f(2)>fbest) then
                fbest = f(2)
                solution_best = solution_1
            End if
        End if
        if(nsucess == L) Exit
        m = m + 1
    End do
    Write(1,*) "Número máximo de sucessos ou tentativas da iteração alcançado"
    i = i + 1
End do

Write(1,*) "Melhor solução encontrada:",solution_best
Write(1,*) "Função da melhor solução:",fbest

Contains

Subroutine Random_integer(m,n,rnd)

    Integer, intent(in) :: m,n
    Integer, intent(out) :: rnd
    Real :: u

    Call random_number(u)
    rnd = n + floor((m + 1 - n) * u)

End subroutine Random_integer

End program mochila