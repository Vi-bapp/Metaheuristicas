program AT2

implicit none

Integer :: h, i, j, k, l, Flag_1, Flag_2, Flag_3
Integer,allocatable :: indice(:)
Integer :: solution_tabu(7), solution_alg(3,7), Tabu_list(6,6), S_test(3,7), switch(2,7)
Real :: dist(6,6), fitness, fit(3), fit_genetico(3), probability(3)

!Abrir unidade externa
open(unit = 1, file = "Atividade_dois.txt", Status ="Old", action = "Write")

!Inicializar solução inicial
solution_tabu = [2,1,3,4,5,6,2]
dist = Reshape([0.0,9.2,5.4,4.1,6.0,8.5,9.2,0.0,9.1,5.8,6.1,4.5,5.4,9.1,0.0,7.2,9.4,5.8, &
4.1,5.8,7.2,0.0,2.2,7.1,6.0,6.1,9.4,2.2,0.0,8.5,8.5,4.5,5.8,7.1,8.5,0.0],[6,6])
Tabu_list = Reshape([0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,2,0,0,0,0,2,0,0,0,0,1,0,0,0,3,0,0,0,0,3,0],[6,6])

!Calcular fitness da solução inicial
fitness = 0
do i = 1,6
    fitness = fitness + dist(solution_tabu(i),solution_tabu(i+1))
end do
Write(1,'(a,x,7(I1,x))') "Solução inicial:",solution_tabu(:)
Write(1,'(a,x,F8.3)') "fitness da solução inicial:",fitness
Write(1,*) ""


Write(1,'(a)') "Busca tabu:"
Write(1,*) ""
!Busca tabu
do i = 1,3
    Write(1,'(2x,a,x,I2)') "Iteração:",(i+3)
    Write(1,*) ""
    !Gerar soluções com pertubação e calular o fitness
    Do j = 1,3
        Write(1,'(3x,a,x,I1)') "Conjunto:",j
        S_test(j,:) = solution_tabu(:)
        !Definir quais cidades serão trocadas
        Flag_1 = 0
        call Random_integer(1,6,k)
        !O loop garante que as cidades serão diferentes e não há restrição
        Do while (Flag_1 == 0)
            Call Random_integer(1,6,l)
            if (l /= k .and. Tabu_list(k,l) == 0) Flag_1 = 1
        End do
        Write(1,'(3x,a,x,I1,x,a,x,I1)') "Cidades trocadas:",k,"e",l
        !Realizar a troca entre cidades
        Do h = 1,6
            if (S_test(j,h) == k) then
                S_test(j,h) = l
            Else if (S_test(j,h) == l) then
                S_test(j,h) = k
            End if
        End do
        S_test(j,7) = S_test(j,1)
        Write(1,'(3x,a,x,7(I1,x))') "Solução alterada:",S_test(j,:)
        !Calcular o fitness da solução teste
        fit(j) = 0
        do h = 1,6
            fit(j) = fit(j) + dist(S_test(j,h),S_test(j,h+1)) 
        End do
        Write(1,'(3x,a,x,F6.3)') "Fitness da solução alterada:",fit(j) 
        Write(1,*) ""
    End do
    !Se o maior fitness da solução teste for maior que o fitness da solução, fazer a substituição
    if (maxval(fit) > fitness) then
        fitness = maxval(fit)
        indice = maxloc(fit)
        solution_tabu(:) = S_test(indice(1),:) 
    End if 
    Write(1,'(3x,a,x,I1)') "Melhor conjunto da iteração:",Indice(1)
    Write(1,*) ""
    !Atualizar a lista tabu
    Do concurrent (j = 1:6)
        Do concurrent (h = 1:6)
            if (Tabu_list(j,h) > 0) Tabu_list(j,h) = Tabu_list(j,h) - 1
        End do
    End do 
    Tabu_list(k,l) = 3
    Tabu_list(l,k) = 3
    Write(1,'(3x,a)') "Lista tabu:"
    Write(1,*) ""
    Do h = 1,6
        Write(1,'(3x,7("|",x,I1,x))') Tabu_list(h,:)
        Write(1,'(3x,a)') "-------------------------" 
    End do
    Write(1,*) ""
    !Adicionar solução a população do algoritmo genético
    solution_alg(i,:) = solution_tabu(:)
    Write(1,'(3x,a,x,7(I1,x))') "População do algoritmo genético:",solution_alg(i,:)
    fit_genetico(i) = fitness
    Write(1,'(3x,a,x,F6.3)') "Fitness da população do algoritmo genético:",fit_genetico(i)
    Write(1,*) ""
End do 
Write(1,*) ""

!Algoritmo genético
Write(1,'(a)') "Algoritmo genético:"
Write(1,*) "" 

Write(1,'(2x,a)') "Populações e fitness"
Do i = 1,3
    Write(1,'(3x,a,x,I1,a,x,7(I1,x))') "População",i,":",solution_alg(i,:)
    Write(1,'(3x,a,x,I1,a,x,F6.3)') "fitness",i,":",fit_genetico(i)
End do
Write(1,*) ""

S_test = solution_alg
!Calcular probabilidades e definir intervalos
probability = (((maxval(fit_genetico) + 1) - fit_genetico) / sum(((maxval(fit_genetico) + 1) - fit_genetico))) * 100
Do i = 1,3
    Write(1,'(2x,a,x,I2,a,x,F6.3)') "Probabilidade de",i,":",probability(i)
End do
Write(1,*) ""
Do i = 2,3
    probability(i) = probability(i) + probability(i-1)
End do
!Gerar novos individuos
call Random_integer(0,100,h)
k = 1
Do i = 2,3
    if (h > probability(k-1) .and. h <= probability(k) ) k = i
End do 
Flag_1 = 0
Do while (Flag_1 == 0)
    call Random_integer(0,100,h)
    l = 1
    Do i = 2,3
        if (h > probability(k-1) .and. h <= probability(k) ) l = i
    End do
    if (l /= k) Flag_1 = 1 
End do
Write(1,'(2x,a,x,I1,x,a,x,I1)') "Serão reproduzidos os indivíduos:",k,"e",l
call Reproduction(S_test(k,:),S_test(l,:),switch(1,:),switch(2,:))
!Individuo com maior fitness será mantido (Elitismo), e os demais permutados visto que a população é ímpar
indice = maxloc(fit_genetico)
solution_alg(1,:) = switch(1,:)
solution_alg(2,:) = switch(2,:)
Write(1,'(2x,a)') "Novos indivíduos gerados:"
Write(1,*) ""
Do i = 1,3
    Write(1,'(3x,a,x,I1,a,x,7(I1,x))') "Individuo",i,":",solution_alg(i,:)
End do 
Write(1,*) ""
!Realizar mutação nos individuos
do i = 1,3
    call Random_integer(0,1,h)
    if (h == 1) then
        call Mutation(solution_alg(i,:))
    End if
End do
Write(1,'(2x,a)') "Indivíduos mutados:"
Write(1,'(a)') ""
Do i = 1,3
    Write(1,'(3x,a,x,I1,a,x,7(I1,x))') "Individuo",i,":",solution_alg(i,:)
End do



Contains

    Subroutine Random_integer(m,n,rnd)

        Integer, intent(in) :: m, n 
        Integer, intent(out) :: rnd 
        Real :: u 

        call random_number(u)
        rnd = m + floor((n + 1 - m) * u)

    End Subroutine Random_integer


Subroutine Reproduction(a,b,c,d)

        Integer, intent(in) :: a(:), b(:)
        Integer,intent(out) :: c(:), d(:)
        Integer :: i, j, k, l, m, n, o, p
        Integer :: menor, maior
        Integer, allocatable :: miss_1(:), miss_2(:) 
        Integer :: Counter(2,size(a)-1)

        !Criar parâmetros auxiliares cópias
        c = a
        d = b
        !Definir região de trocas de valores
        Call Random_integer(0,size(a)-1,m)
        i = 0
        Do while (i == 0)
            Call Random_integer(0,size(a)-1,n)
            if (n /= m) i = 1
        End do
        if (n < m) then
            menor = n
            maior = m
        Else
            menor = m
            maior = n
        End if
        !Fazer trocas
        Do i = 1,size(a)-1
            if (i > menor .or. i <= maior) then
                d(i) = a(i)
                c(i) = b(i)
            End if
        End do
        !Verificar inconsistências quanto a quantidade de cidades
        counter = 0
        Do i = 1,size(a)-1
            Do j = 1,size(a)-1
                if (c(i) == j) counter(1,j) = counter(1,j) + 1
                if (d(i) == j) counter(2,j) = counter(2,j) + 1
            End do
        End do
        !Verificar qual cidade falta
        if (size(a) - 1 - sum(counter(1,:)) > 0) then
            o = 1
            allocate(miss_1(size(a) - 1 - sum(counter(1,:))))
            Do i = 1,size(miss_1)
                if (counter(1,i) == 0) then 
                    miss_1(o) = a(i)
                    o = o + 1
                End if
            End do
        End if
        if (size(a) - 1 - sum(counter(2,:)) > 0) then
            p = 1
            allocate(miss_2(size(a) - 1 - sum(counter(2,:))))
            Do i = 1,size(miss_2)
                if (counter(2,i) == 0) then 
                    miss_2(p) = a(i)
                    p = p + 1
                End if
            End do
        End if
        !Substituir a cidade que sobra pela que falta
        o = 1
        p = 1
        Do i = 1,size(a)-1
            if (counter(i,1) > 1 .and. (i < menor .or. i > maior)) then
                c(i) = miss_1(o)
                o = o + 1
            End if
            if (counter(i,2) > 1 .and. (i < menor .or. i > maior)) then
                d(i) = miss_2(p)
                p = p + 1
            End if
        End do
        c(size(a)) = c(1)
        d(size(b)) = d(1)

    End subroutine Reproduction


    Subroutine Mutation(a)

        Integer, intent(in out) :: a(:)
        Integer :: n, m, Aux

        Call Random_integer(1,size(a)-1,n)
        Call Random_integer(1,size(a)-1,m)
        Aux = a(n)
        a(n) = a(m)
        a(m) = Aux
        a(size(a)) = a(1)

    End subroutine Mutation


End program AT2