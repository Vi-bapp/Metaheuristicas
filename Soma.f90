module mod_somatorio
    
    implicit none
    
    contains
    
    subroutine soma_ent(a, b, c, d, e)  !Soma os valores de entrada e retorna a função da camada seguinte
    
    Real, intent(in) :: a(:,:) !Valores de entrada
    Real, intent(in out) :: b(:,:), c(:) !Pesos e bias
    Real, intent(out) :: d(:) !Função ativada
    Real :: f(size(d)) !Função não ativada
    Integer, intent(in) :: e !indice da somatória
    Integer :: i, j
    
    do i = 1,size(c) !Indice de camadas escondidas
        f(i) = c(i)
        do j = 1,size(b(:,1)) !Indice de camadas de entrada
            f(i) = f(i) + b(j,i) * a(e,j)
        end do
        d(i) = 1/(1 + exp(-f(i)))
    end do

    end subroutine soma_ent
    
    
    subroutine soma(a, b, c, d)
    
    Real, intent(in) :: a(:) !Valores de entrada
    Real, intent(in out) :: b(:,:), c(:) !Pesos e bias
    Real, intent(out) :: d(:) !Função ativada
    Real :: e(size(d)) !Função não ativada
    Integer :: i, j
     
    do i = 1,size(c)  !Inidice de camada saída
        e(i) = c(i)
        do j = 1,size(b(:,1))   !Indice de camada escondida
            e(i) = e(i) + b(j,i) * a(j)
        end do
        d(i) = 1/(1 + exp(-e(i)))
    end do
  
    end subroutine soma
    
    
    
end module mod_somatorio