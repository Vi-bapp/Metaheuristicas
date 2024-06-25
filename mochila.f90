program mochila

Implicit none

Integer, parameter :: cidades = 20, block = 3, max_it = 10, P = 1425
Integer :: i, j, it, k, counter
Integer :: ierror
Character(len=20) :: io_msg
Integer :: a(3)
Integer :: Cities(cidades), aux(3,cidades), bestway(cidades)
Integer :: tabu_list(cidades), peso(cidades), valor(cidades), quantidade(cidades)
Real :: soma, hold, keep, fitness

open(unit=1,file='mochila.txt',status='replace', action='write', iostat=ierror, iomsg=io_msg)

tabu_list = 0
bestway = cities

Write(1,'(a,x,I2)') "Número de iterações:",max_it

Peso = [20, 25, 14, 46, 39, 57, 58, 47, 38, 30, 53, 57, 38, 53, 58, 48, 14, 6, 40, 10]
valor = [7, 7, 8, 3, 5, 8, 1, 4, 9, 7, 10, 8, 7, 1, 7, 9, 3, 2, 4, 2]
quantidade = [10, 12, 4, 12, 15, 6, 7, 6, 12, 15, 12, 8, 20, 7, 7, 15, 3, 4, 8, 12]
Write(1,'(a,x,20(I2,x))') "Pesos:",Peso
Write(1,'(a,x,20(I2,x))') "Valores:",Valor
Write(1,'(a,x,20(I2,x))') "Quantidades:",quantidade

Do i = 1,cidades
    call random(1,0,Cities(i))
End do
if (dot_product(cities,peso) - P > 0) then
    fitness = dot_product(cities,valor) - (dot_product(Cities,peso) - P)
Else 
    fitness = dot_product(Cities,valor)
End if
bestway = Cities
keep = fitness
Write(1,'(a,x,20(I2,x))') "Solução inicial:",cities
Write(1,'(a,x,F8.3)') "fitness inicial:",fitness
Write(1,*) ""

it = 1
keep = 0
Do while (it <= max_it)
    Write(1,'(a,x,I2)') "Valor da iteração",it 
    Write(1,*) ""
    Do j = 1,3
        Aux(j,:) = cities(:)
    End do
    Do j = 1, 3
        Write(1,'(2x,a,x,I2)') "Conjunto",j
        !gerar movimentos aleatórios
        k = 1
        Do while (k == 1)
            call random(cidades,1,a(j))
            if (tabu_list(a(j)) == 0) k = 0
        End do
        !vetor auxiliar como movimentação
        Do i = 1,cidades
            if (i == a(j)) then
                call random(quantidade(i),0,Aux(j,i))
                Write(1,'(2x,a,x,I2)') "Item a ser alterado:",a(j)
                Write(1,'(2x,a,x,I2)') "Nova quantidade atualizada:",Aux(j,i)
            End if
        End do
        Write(1,'(2x,a,x,20(I2,x))') "Solução perturbada",Aux(j,:)
        !Função de avaliação da solução
        Do i = 1,3
            if (dot_product(aux(j,:),peso) - P > 0) then
                soma = dot_product(aux(j,:),valor) - (dot_product(aux(j,:),peso) - P)
            Else 
                soma = dot_product(aux(j,:),valor)
            End if
        End do
        Write(1,'(2x,a,x,I2)') "Solução perturbada:",i
        Write(1,'(2x,a,x,F8.3)') "fitness da solução perturbada:",Soma
        Write(1,*) ""
        !Adoção do maior valor
        if (j == 1) then
            hold = soma
            counter = 1
        Else if (soma > hold) then
            hold = soma
            counter = j
        End if
    End do
    !Atualização do caminho
    if (hold >= fitness) then
        cities(:) = aux(counter,:)
        fitness = hold
    End if 
    if (fitness > keep) then
        keep = fitness
        bestway = cities
    End if
    Write(1,'(a,x,20(I2,x))') "Solução adotada na iteração:",cities
    Write(1,'(a,x,F8.3)') "Fitness da solução adotada:",keep
    !Atualizar bloqueios da lista
    Do i = 1, cidades
        if (tabu_list(i) > 0) tabu_list(i) = tabu_list(i) - 1
    End do
    !Adicionar movimento bloqueado
    tabu_list(a(counter)) = block
    Write(1,'(a,x,20(I2,x))') "Lista Tabu:",tabu_list
    !Atualizar iteração
    it = it + 1
    Write(1,*) ""
End do
Write(1,*) ""


Write(1,'(a,x,20(I2,x))') "Melhor solução encontrada:",bestway
Write(1,'(a,x,F8.3)') "Fitness da melor solução encontrada:",keep


contains

Subroutine random(m,n,j)

Real :: u
Integer,intent(in) :: m,n
Integer,intent(out) :: j

call random_number(u)
j = n + floor((m + 1 - n)*u)

End subroutine random 


End program mochila