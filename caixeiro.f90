program minimizar

    Implicit none

    Integer, parameter :: Population_size = 4, genotype_size = 10, Iterations  = 4
    Integer :: a(genotype_size+1), b(genotype_size+1), population(Population_size,genotype_size+1)
    Integer :: memory(Population_size,genotype_size+1), best_solution(genotype_size+1)
    Real :: Probability(Population_size), fitness(Population_size), coordenadas(genotype_size,2)
    Real :: distance(genotype_size,genotype_size), Aux(Population_size)
    Real :: best_dist
    Integer :: Int, m, n, o, p, q
    Integer :: i, j, k, l
    Character(len=1) :: Cidades(Population_size,genotype_size+1)

    !open external file
    open(unit=2, file='Caixeiro.txt', status='Replace', action='Write')
    open(unit=1, file='coordenadas.txt', status='old', action='Read')

    !Inicializar solução inicial
    population = reshape([1,1,8,1,2,9,1,9,6,8,5,3,4,10,2,7,&
    8,7,10,6,5,6,4,2,3,3,3,8,10,&
    4,6,4,7,5,9,10,9,2,7,5,1,1,8,1],[4,11])
    Write(2,'(a)') "Soluções iniciais do programa:"
    Write(2,*) ""
    do i = 1,Population_size
        Write(2,'(2x,a,I2,a,2x,11(I2,x))') "Solução",i,":",population(i,:)
    End do
    Write(2,*) ""

    !Ler coordenadas
    Do i = 1,genotype_size
        Read(1,*) coordenadas(i,:)
    End do

    !Carregar matriz de distâncias
    call dist(coordenadas,distance)

    Write(2,'(a,I2)') "Número de iterações a serem feitas",Iterations
    Write(2,*) ""

    !Melhor caminho inicial
    Do j = 1,Population_size
        Do k = 1,genotype_size
            Aux(j) = Aux(j) + distance(population(j,k),population(j,k+1))
        End do
    End do
    best_dist = Aux(1)
    best_solution = population(1,:)
    Do j = 2,Population_size
        if (aux(j) < Aux(j-1)) then
            best_dist = Aux(j)
            best_solution = population(j,:)
        End if
    End do

    !Start iterating the solutions
    Do i = 1,Iterations
        Write(2,'(a,I2,a)') "geração",i,":"
        Write(2,*) ""
        memory = population
        !Fitness and probability
        Write(2,'(2x,a)') "Distância das rotas:"
        Fitness = 0
        Do j = 1,Population_size
            Do k = 1,genotype_size
                fitness(j) = fitness(j) + distance(population(j,k),population(j,k+1))
            End do
            Write(2,'(4x,a,I2,a,x,f9.4)') "Solução",i,":",fitness(j)
        End do
        Probability = (((maxval(fitness) + 1) - fitness) / sum(((maxval(fitness) + 1) - fitness))) * 100
        Write(2,*) ""
        Write(2,'(2x,a)') "Probabilidades:"
        Do j = 1,Population_size
            Write(2,'(4x,a,I2,a,x,f8.4,a)') "Solução:",j,":",probability(j),"%"
        End do
        Do j = 2,Population_size
            probability(j) = probability(j) + probability(j-1)
        End do
        Write(2,*) ""
        Write(2,'(2x,a)') "Permutar soluções para gerar nova geração:"
        !Reproduct individuals
        Do j = 1,Population_size,2
            Call Random_Int(1,100,m)
            o = 1
            Do k = 2,Population_size
                if (m >= probability(k - 1) .and. m < probability(k)) o = k 
            End do
            k = 0
            Do while (k == 0)
                Call Random_Int(1,100,n)
                p = 1
                Do l = 2,Population_size
                    if (n >= probability(l - 1) .and. n < probability(l)) p = l 
                End do
                if (o /= p) k = 1
            End do
            Write(2,'(4x,a,x,I1,x,a,x,I1)') "Permutar",o,"com",p
            call Reproduction(memory(o,:),memory(p,:),a,b)
            population(j,:) = a(:)
            population(j+1,:) = b(:)
        End do
        Write(2,*) ""
        Write(2,'(2x,a)') "Novas soluções geradas:"
        Do j = 1,Population_size
            Write(2,'(4x,a,I2,a,10(I2,x))') "Solução",j,":",population(j,:)
        End do
        !Mutate the individuals
        Write(2,*) ""
        Write(2,'(2x,a)') "Mutação nas soluções:"
        Do j = 1,Population_size
            call Random_Int(0,1,int)
            If (Int == 1) then
                Write(2,'(4x,a,x,I1)') "Ocorre mutação em:",j
                call Mutation(population(j,:))
                Write(2,'(4x,a,x,10(I2,x))') "Nova solução gerada:",population(j,:)
            End if 
        End do
        Do j = 1,Population_size
            Do k = 1,genotype_size
                Aux(j) = Aux(j) + distance(population(j,k),population(j,k+1))
            End do
            if (Aux(j) < best_dist) then
                best_dist = Aux(j)
                best_solution = population(j,:)
            End if
        End do
    End do 
    Write(2,*) ""
    Write (2,'(a,x,f8.4)') "Menor distância encontrada:",best_dist
    Write (2,'(a,x,10(I2,x))') "Melhor solução encontrada:",best_solution



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
        Integer,intent(out) :: c(:), d(:)
        Integer :: i, j, k, l, m, n, o, p
        Integer :: menor, maior
        Integer :: Counter(2,size(a)-1)

        !Criar parâmetros auxiliares cópias
        c = a
        d = b
        !Definir região de trocas de valores
        Call Random_Int(0,size(a)-1,m)
        i = 0
        Do while (i == 0)
            Call Random_Int(0,size(a)-1,n)
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
        Do i = 1,size(a)-1
            if (counter(i,1) == 0) o = i
            if (counter(i,2) == 0) p = i
        End do
        !Substituir a cidade que sobra pela que falta
        Do i = 1,size(a)-1
            if (counter(i,1) > 1 .and. (i < menor .or. i > maior)) c(i) = o
            if (counter(i,2) > 1 .and. (i < menor .or. i > maior)) d(i) = p
        End do
        c(size(a)) = c(1)
        d(size(b)) = d(1)

    End subroutine Reproduction

    Subroutine Mutation(a)

        Integer, intent(in out) :: a(:)
        Integer :: n, m, Aux

        Call Random_Int(1,size(a)-1,n)
        Call Random_Int(1,size(a)-1,m)
        Aux = a(n)
        a(n) = a(m)
        a(m) = Aux
        a(size(a)) = a(1)

    End subroutine Mutation

    Subroutine dist(coordenadas,distance)

        Real, intent(in) :: Coordenadas(:,:)
        Real, intent(out) :: distance(:,:)
        Integer :: i, j, k, l, m, n 

        Do i = 1,size(distance,1)
            Do j = 1,size(distance,2)
                distance(i,j) = sqrt(((coordenadas(i,1) - coordenadas(j,1)) ** 2 )+ ((coordenadas(i,2) - coordenadas(j,2)) ** 2))
            End do
        End do
        
    End subroutine dist


End program minimizar