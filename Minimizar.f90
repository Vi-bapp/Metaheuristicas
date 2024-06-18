program minimizar

    Implicit none

    Integer, parameter :: Population_size = 4, genotype_size = 5, Iterations  = 4
    Integer :: a(genotype_size), b(genotype_size), population(Population_size,genotype_size)
    Integer :: memory(Population_size,genotype_size), best_solution(genotype_size)
    Real :: Probability(Population_size), Aux(Population_size), solution(Population_size)
    Real :: best_value
    Integer :: Int, m, n, o, p
    Integer :: i, j, k, l

    !open external file
    open(unit=1, file='minimize.txt', status='Replace', action='Write')

    !Initialize random population
    Write(1,'(a)') "Solução inicial do problema:"
    Do i = 1,Population_size
        Do j = 1,genotype_size
            call Random_Int(0,1,Int)
            population(i,j) = Int
        End do
        Write(1,'(2x,a,I2,a,x,5(I2,x))') "Solução:",i,":",population(i,:)
    End do 

    !Melhor caminho inicial
    Aux = 0
    Do j = 1,Population_size
        Do k = 1,genotype_size
            Aux(j) = Aux(j) + population(j,k) * (2 ** l)
            l = l - 1
        End do
        Aux(j) = ((Aux(j) - 4 )** 2) + ((Aux(j) - 8 )** 3) + 5
    End do
    best_value = Aux(1)
    best_solution = population(1,:)
    Do j = 2,Population_size
        if (aux(j) < Aux(j-1)) then
            best_value = Aux(j)
            best_solution = population(j,:)
        End if
    End do

    Write(1,*) ""
    Write(1,'(a,x,I2)') "Número de iterações a serem feitas:",Iterations

    !Start iterating the solutions
    Do i = 1,Iterations
        Write(1,'(2x,a,x,I2)') "Iteração:",i
        memory = population
        !Fitness and probability
        Write(1,*) ""
        Write(1,'(2xa)') "Calcular o fitness da solução:"
        Do j = 1,Population_size
            solution(j) = 0
            l = genotype_size
            Do k = 1,genotype_size
                solution(j) = solution(j) + population(j,k) * (2 ** l)
                l = l - 1
            End do
            solution(j) = ((solution(j) - 4 )** 2) + ((solution(j) - 8 )** 3) + 5
            Write(1,'(4x,a,x,I2,a,x,f15.4)') "Solução",j,":",solution(j)
        End do
        Write(1,*) ""
        Write(1,'(2x,a)') "Probabilidade das soluções"
        Probability = 100 * ((maxval(solution) + 1 - solution) / sum((maxval(solution) + 1 - solution)))
        Do j = 1,Population_size
            Write(1,'(4x,a,x,I2,a,x,f8.4)') "Probabilidade da solução",j,":",probability(j)
        End do
        Do j = 2,Population_size
            Probability(j) = Probability(j) + Probability(j-1)
        End do
        !Reproduct individuals
        Write(1,*) ""
        Write(1,'(2x,a)') "Permutar as soluções de acordo com as probabilidades:"
        Do j = 1,Population_size,2
            Call Random_Int(1,100,m)
            o = 1
            Do k = 2,Population_size
                if (m > probability(k-1) .and. m <= probability(k)) o = k 
            End do
            k = 0
            Do while (k == 0)
                Call Random_Int(1,100,n)
                p = 1
                Do l = 2,Population_size
                    if (n > probability(l-1) .and. n <= probability(l)) p = l 
                End do
                if (o /= p) k = 1
            End do
            Write(1,'(4x,a,x,I1,x,a,x,I1)') "Permutar",o,"com",p
            call Reproduction(memory(o,:),memory(p,:),a,b)
            population(j,:) = a(:)
            population(j+1,:) = b(:)
        End do
        Write(1,*) ""
        Write(1,'(2x,a)') "Nova geração de soluções:"
        Do j = 1,Population_size
            Write(1,'(4x,a,x,I2,a,x,5(I2,x))') "Solução",j,":",population(j,:)
        End do
        !Mutate the individuals
        Write(1,*) ""
        Write(1,'(2x,a)') "Mutações nas soluções:"
        Do j = 1,Population_size
            call Random_Int(0,1,int)
            If (Int == 1) then
                Write(1,'(4x,a,x,I1)') "Ocorre mutação em",j
                call Mutation(population(Int,:))
                Write(1,'(4x,a,x,I1,a,x,5(I2,x))') "Novo conjunto de solução em",j,":",population(j,:)
            End if 
        End do
        !Try to see if theres a better solution
        Aux = 0
        Do j = 1,Population_size
            Do k = 1,genotype_size
                Aux(j) = Aux(j) + population(j,k) * (2 ** l)
                l = l - 1
            End do
            Aux(j) = ((Aux(j) - 4 )** 2) + ((Aux(j) - 8 )** 3) + 5
            if (Aux(j) < best_value) then
                best_value = Aux(j)
                best_solution = population(j,:)
            End if
        End do
    End do 

    Write(1,*) ""
    Write(1,*) ""
    Write(1,'(a,x,5(I1,x))') "Melhor solução encontrada:",best_solution
    Write(1,'(a,x,f15.4)') "Valor da melhor solução:",best_value

Contains

    Subroutine Random_Int(inicial,final,inteiro)

        Integer, intent(in) :: inicial, final
        Integer, intent(out) :: inteiro
        Real :: Aux

        Call Random_number(Aux)
        Inteiro = inicial + floor((final + 1 - inicial) * Aux)

    End subroutine Random_Int


    Subroutine Reproduction(a,b,c,d)

        Integer, intent(in) :: a(:), b(:)
        Integer, intent(out) :: c(:), d(:)
        Integer :: i, n 

        c = a
        d = b
        Call Random_Int(1,size(a),n)
        Do i = 1,n
            c(i) = b(i)
            d(i) = a(i)
        End do

    End subroutine Reproduction


    Subroutine Mutation(a)

        Integer, intent(in out) :: a(:)
        Integer :: n

        Call Random_Int(1,size(a),n)
        if (a(n) == 0) a(n) = 1
        if (a(n) == 1) a(n) = 0

    End subroutine Mutation

End program minimizar