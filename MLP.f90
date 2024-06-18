program MLP
    use mod_somatorio, only: soma_ent, soma
    use mod_atualizar, only: interna, inicio
    
    implicit none
    
    !Declare variables
    
    Real, allocatable :: x(:,:), w(:,:), d(:,:), z(:), v(:,:), u(:), teta_1(:), teta_2(:)
    Real :: E
    Integer :: i, j, k, l
    Integer, parameter :: treino = 5, teste = 0 !Conjuntos
    Integer, parameter :: entrada = 2 !Quantidade de entradas
    Integer, parameter :: escondida = 3 !Quantidade de camadas escondidas
    Integer, parameter :: saida = 1 !Quantidade de saídas
    Integer :: alpha
    Integer :: ierror
    Character(len=20) :: Filename, err_string
    
    !Read files 
    write (*,*) 'Insert File Name'
    Read(*,*) Filename
    open(unit = 8, File = Filename, Status = 'old', Action = 'Read', iostat = ierror, iomsg = err_string)
    open(unit = 7, File = 'tabela.txt', Status = 'replace', Action = 'write', iostat = ierror, iomsg = err_string)
    allocate (v(entrada,escondida), x(treino + teste,entrada), d(treino + teste,saida)) !camada de entrada
    allocate (z(escondida), w(escondida,saida), teta_1(escondida)) !camada escondida
    allocate (u(saida), teta_2(saida)) !camada de saida
    do i = 1,treino + teste
        read(8,*) x(i,:), d(i,saida)
    End do
    
    !initialize variables
    call RANDOM_NUMBER(teta_1)
    call RANDOM_NUMBER(teta_2)
    call RANDOM_NUMBER(w)
    call RANDOM_NUMBER(v)
    Write(7,*) 'Bias A e B, e, pesos w e v iniciais =', teta_1, teta_2, w, v
    
    !Training iteration
    Write(*,*) 'Insert iteration value'
    Read(*,*) k
    alpha = 1
    do i = 1,k
        write(7,*) 'valor da iteração', i
        E = 0
        do j = 1, treino
            write(7,*) 'valor do conjunto', j
            call soma_ent(x, v, teta_1, z, j)
            write(7,*) 'valor de z', z
            call soma(z, w, teta_2, u)
            write(7,*) 'valor de u', u
            call interna(z, u, w, teta_2, d, j, alpha)
            write(7,*) 'valor de teta_2', teta_2
            write(7,*) 'valor de w', w
            call inicio(z, u, v, teta_1, d, j, x, w, alpha)
            write(7,*) 'valor de teta_1', teta_1
            write(7,*) 'valor de v', v
        end do
        do j = 1, treino
            do l =1, saida
                call soma_ent(x, v, teta_1, z, j)
                call soma(z, w, teta_2, u)
                E = E + ((d(j,l) - u(l)) ** 2)
            end do
        end do
        Write(7,*) 'Erro da iteração =', (E/2)
        alpha = 0.9 * alpha
    end do
    
    !Calculo do erro
    do j = treino, teste
            do l = 1,saida
                call soma_ent(x, v, teta_1, z, j)
                call soma(z, w, teta_2, u)
                E = E + ((d(j,l) - u(l)) ** 2)
            end do
        end do
        Write(7,*) 'Erro do programa =', (E/2)
    
End Program MLP