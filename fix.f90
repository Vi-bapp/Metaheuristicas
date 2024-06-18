Program fix

Implicit none

!Variables declaration

Integer, parameter :: items = 15, P = 275, It = 3
Integer :: weight(items),Val(items),solution(items,items),solution_best(items),L(items)
Integer :: val_max, L_best
Integer :: i, j, k, m, n, o, r
Real, parameter :: alpha = 0.5, beta = 0.5, Rho = 0.5, Q = 1
Real :: Tau(items),Eta(items),Prob(items),Delt_tau(items)
Real :: gama, Aux, summation, Prob_max, Val_best, P_hold, l_hold

!Code

!open external file
open(unit = 1, file = 'Exit.txt', Status = 'replace', action = 'write')

Write(1,*) "Váriaveis utilizadas"
Write(1,*) "Alpha:",alpha
Write(1,*) "Beta:",Beta
Write(1,*) "Rho:",Rho
Write(1,*) "Q:",Q
Write(1,*) "Peso máximo:",P
Write(1,*) "Número de items analisados:", items
Write(1,*) "Número máximo de iterações feitas pelo programa:",It

!Initialize program variables
Call random_number(Tau)
Write(1,*) "Tau:",Tau
weight = [63,21,2,32,13,80,19,37,56,41,14,8,32,42,7]
Val = [13,2,20,10,7,14,7,2,2,4,16,17,17,3,21]

!Calcular Eta
val_max = Val(1)
Do I = 2,items
    If (Val(I) > val_max) val_max = Val(I)
End do
Eta = 1/real(val_max + 1 - val)
Write(1,*) "Eta",Eta 

!Gama
gama = sum(val)
Write(1,*) "Gama",Gama

!Iterations
l_hold = 0
Do i = 1,It
    L_best = 0
    Write(1,*) "Iteração:",i
    !Reset the solutions
    solution = 0
    Do j = 1,items
        Write(1,*) "Formiga:",j
        solution(j,j) = 1
        P_hold = P - weight(j)
        Write(1,*) "Peso que a formiga ainda pode carregar:",P_hold
        r = 1
        !Test if the ant can carry other itens, if so make It carry
        Do while (r == 1) 
            if (P_hold > 0) then
                Prob_max = 0
                Prob = 0
                Write(1,*) "Probabilidade de a formiga carregar outro item"
                do k = 1,items
                    if (solution(j,k) == 0) then
                        !Calculate the probability of the ant carrying the other itens
                        summation = 0
                        Do m = 1,items
                            if (solution(j,k) == 0 .and. k /= m ) summation = summation + (Tau(m)**alpha * Eta(m)**Beta)
                        End do
                        Prob(k) = (Tau(k)**alpha * Eta(k)**Beta) / summation
                        if (Prob(k) > Prob_max) then
                            Prob_max = Prob(k)
                            n = k
                        End if
                    End if
                end do
                Write(1,*) "Vetor com as probabilidades:",Prob
                Write(1,*) "Item com maior probabilidade de ser levado:",n
                !Test if the ant can carry the item with the bigger probability
                If (P_hold - weight(n) >= 0) then
                    Write(1,*) "Item de maior probabilidade adicionado a solução"
                    solution(j,n) = 1
                    P_hold = P_hold - weight(n)
                    Write(1,*) "Peso que a formiga ainda consegue carregar:",P_hold
                    if (P_hold == 0) r = 0
                Else
                    Write(1,*) "Peso do item de maior probabilidade excede a capacidade da formiga"
                    !If It still can carry anything, find the item with the biggest value
                    val_best = 0
                    n = 0
                    do k = 1,items
                        if (solution(j,k) == 0 .and. P_hold - weight(k) >= 0 .and. val(k) > val_best) n = k
                    End do
                    if (n /= 0) then
                        Write(1,*) "Item de maior valor que ainda pode ser carregado:",n
                        solution(j,n) = 1
                    End if
                    Write(1,*) "Solução encontrada pela formiga:", solution(j,:)
                    L(j) = dot_product(solution(j,:),val)
                    Write(1,*) "Valor que a formiga carrega:",L(j)
                    if (L(j) > L_best) then
                        L_best = L(j)
                        o = j
                    End if
                    r = 0
                End if
            End if
        End do
    End do
    Write(1,*) "Formiga com o maior valor da iteração:",o
    if (l_best > l_hold) then
        l_hold = l_best
        solution_best = solution(o,:)
    End if
    Write(1,*) "Atualização dos ferômonios"
    Do j = 1,items
        Delt_tau(j) = Q / real(Gama - L(j))
    End do
    Write(1,*) "Delta Tau:",Delt_tau
    Tau = (1 - Rho) * Tau + matmul(solution,Delt_tau)
    Write(1,*) "Tau:",Tau
End do

Write(1,*) "Melhor solução encontrada:",solution_best
Write(1,*) "Valor da melhor solução:",l_hold

End program fix