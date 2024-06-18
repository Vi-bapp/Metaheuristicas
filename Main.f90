Program SOM_1

Implicit none

!Declare variables

Integer, parameter :: treino = 6, teste = 0, entradas = 2
Integer, parameter :: map = 6
Real, allocatable :: Input(:,:), W(:,:), d(:), gaussian(:,:), top(:,:)
Real :: alpha = 0.5, r = 0.5
Integer :: ierror
Integer :: iteration = 1
Integer :: i,j,k,l
Integer :: comp 
Real :: E
character(len=20) :: io_msg, filename

!Code

allocate(input(treino+teste,entradas))
allocate(w(map,entradas))
allocate(d(map))
allocate(gaussian(map,map),top(map,map))
open(unit = 1, file = 'Entrada.txt', Status = 'old', Action = 'Read', iostat = ierror, iomsg = io_msg)
open(unit = 2, file = 'saida.txt', Status = 'replace', Action = 'write', iostat = ierror, iomsg = io_msg)
open(unit = 3, file = 'test.txt', Status = 'replace', Action = 'write', iostat = ierror, iomsg = io_msg)
do i = 1,treino+teste
    Read(1,*) input(i,:)
End do

w = reshape([1.0,0.5,0.0,0.0,0.5,1.0,1.0,1.0,1.0,0.0,0.0,0.0],[map,entradas])

call topology(w,top)

Write(2,*) 'Treinamento da rede'
Do k = 1, iteration
    E = 0
    Write(2,*) 'iteração', k
    gaussian = exp((-(top**2))/(2*(r**2)))
    Do i = 1, treino
        d = 0
        Write(2,*) 'conjunto', i
        Do l = 1, map
            Do j = 1, entradas
                d(l) = d(l) + (input(i,j)-w(l,j))**2
            End do
            Write(2,*) 'distância euclidiana entre pontos "d":', d(l)
            if(l == 1) then
                comp = 1
            Else if (d(l)<d(comp)) then
                comp = l
            End if
        End do
        Write(2,*) 'Neurônio vencedor do conjunto:', comp
        Do l = 1, map
            Do j = 1, entradas
                w(l,j) = w(l,j) + alpha * gaussian(l,comp) * (input(i,j)-w(l,j))
                E = E + (input(i,j)-w(l,j))**2
            End do
        End do
        Write(2,*) 'Correção de pesos "w":', w
    End do
    alpha = alpha * exp(real(k/iteration))
    r = r * (1 - real(k)/r)
    Write(2,*) 'Correção da taxa de aprendizagem "alpha":', alpha
    Write(2,*) 'Correção do raio "r":', r
End do
Write(2,*) 'Pesos ao fina do treinamento:', w


if (teste /= 0) then 
    Write(2,*) 'teste da rede'
    E = 0
    Do i = treino+1, treino+teste
        Write(2,*) 'Conjunto', i
        d = 0
        Do l = 1, map
            Do j = 1, entradas
                d(l) = d(l) + (input(i,j)-w(l,j))**2
                 E = E + (input(i,j)-w(l,j))**2
            End do
        End do
        Write(2,*) 'd', d
        Write(2,*) 'Erro quadrático da iteração =', (E/map)
    End do
End if

contains

Subroutine topology(w,top)

Real, intent(in) :: w(:,:)
Real, allocatable :: u(:,:)
Real, intent(out) :: top(:,:)
Integer :: i,j,k,l
Real, allocatable :: Aux_1(:), Aux_2(:)
Real :: Square


allocate(u(size(w(:,1)),size(w(1,:))))
allocate(Aux_1(size(w(1,:))),Aux_2(size(w(1,:))))
u = w
do i= 1, size(w(:,1))
    Do j = 1, size(w(1,:))
        if (i == 1) then
            Aux_1(j) = w(i,j)
            Aux_2(j) = w(i,j)
        Else if (w(i,j) > Aux_1(j)) then
            Aux_1(j) = w(i,j)
        Else if (w(i,j) < Aux_2(j)) then
            Aux_2(j) = w(i,j)
        End if
    End do
End do

do k = 1,size(w(:,1))
    do j = 1,size(w(1,:))
        u(k,j) = (u(k,j) - Aux_2(j))/(Aux_1(j) - Aux_2(j)) 
    End do
End do

do k = 1,size(top(:,1))
    do l =1,size(top(1,:))
        Square = 0
        Do j = 1,size(w(1,:))
            Square = Square + (u(k,j)-u(l,j))**2
        End do
        top(k,l) = sqrt(Square)
    End do
End do


End subroutine topology

End Program SOM_1

