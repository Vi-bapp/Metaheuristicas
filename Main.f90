Program SOM_1

Implicit none

!Declare variables

Integer :: treino = 5, teste = 0, entradas = 2, map = 16
Real, allocatable :: Input(:,:), W(:,:), d(:), class_w(:), class_in(:)
Real :: alpha = 0.1
Integer :: ierror
Integer :: iteration = 4
Integer :: i,j,k,l
Integer :: comp
Real :: E
character(len=20) :: io_msg

!Code

allocate(input(treino+teste,entradas))
allocate(w(map,entradas))
allocate(d(map))
allocate(class_w(map))
allocate(class_in(treino+teste))
open(unit = 1, file = 'Entrada.txt', Status = 'old', Action = 'Read', iostat = ierror, iomsg = io_msg)
open(unit = 2, file = 'saida.txt', Status = 'replace', Action = 'write', iostat = ierror, iomsg = io_msg)
do i = 1,treino+teste
    Read(1,*) input(i,:)
End do

w = reshape([0.2,0.4,0.6,0.8,0.2,0.4,0.6,0.8,0.2,0.4,0.6,0.8,0.2,0.4,0.6,0.8, &
0.8,0.8,0.8,0.8,0.6,0.6,0.6,0.6,0.4,0.4,0.4,0.4,0.2,0.2,0.2,0.2],[map,entradas])
class_w = [3,4,1,2,1,2,3,4,3,4,1,2,1,2,3,4]
class_in = [1,1,1,4,4]

Write(2,*) 'Treinamento da rede'
Do k = 1, iteration
    Write(2,*) 'iteração', k
    Do i = 1, treino
        Write(2,*) 'Conjunto', i
        d = 0
        Do l = 1, map
            Do j = 1, entradas
                d(l) = d(l) + (input(i,j)-w(l,j))**2
            End do
            if(l == 1) then
                comp = 1
            Else if (d(l)<d(comp)) then
                comp = l
            End if
        End do
        Write(2,*) 'diferenças euclidianas =', d
        Write(2,*) 'Neurônio vencedor =', comp
        E = 0
        if (class_in(i) == class_w(comp)) then
            Write(2,*) 'A classe do conjunto de entrada é igual a classe dos pesos'
            Do j = 1, entradas
                w(comp,j) = w(comp,j) + alpha*(input(i,j)-w(comp,j))
                Write(2,*) 'Pesos w atualizados =', w(comp,j)
            End do
        Else
            Write(2,*) 'A classe do conjunto de entrada é diferente da classe dos pesos'
            Do j = 1, entradas
                w(comp,j) = w(comp,j) - alpha*(input(i,j)-w(comp,j))
                Write(2,*) 'Pesos w atualizados =', w(comp,j)
                E = E + 1
            End do
        End if
    End do
    alpha = alpha * 0.95
End do
Write(2,*) 'w =', w

if (teste /= 0) then
    Write(2,*) 'teste da rede'
    Do i = treino+1, treino+teste
        Write(2,*) 'Conjunto', i
        d = 0
        Do l = 1, map
            Do j = 1, entradas
                d(l) = d(l) + (input(i,j)-w(l,j))**2
            End do
        End do
        Write(2,*) 'd =', d
    End do
End if

End Program SOM_1

