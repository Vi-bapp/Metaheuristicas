program recurrent

Implicit None

!declare variables

Integer, parameter :: train = 4, test = 0, input = 2, output = 1, middle = 2, iteraction = 3
Integer :: i, j, k, l, m, ierror
Character(len=20) ::filename, err_string
Real :: x(input), z(middle), y(output), z_0(middle), y_0(output)
Real :: z_inactive(middle), y_inactive(output), z_active(middle), y_active(output)
Real :: dz(middle), dy(output)
Real :: Wa(middle,middle), Wb(output,middle), Wc(input,middle), V(middle,output)
Real :: T_1(middle), T_2(output)
Real :: dados(train+test,input+output)
Real :: d(output)
Real :: delt_s(output), delt_t(middle), delt_0(output), u(output,output)
Real :: alpha, Erro

!Algorithm

!Inicialização das matrizes de peso, alpha e bias

wa = reshape([-0.5,0.5,-0.5,0.5],[middle,middle])
wb = reshape([1.0,1.0],[output,middle])
wc = reshape([-1.0,3.0,-3.0,-3.0],[input,middle])
V = reshape([2.0,-3.0],[middle,output])


alpha = 1
u = 1
T_1 = [-0.5,2.0]
T_2 = [0.5]
z_0 = [1.0,1.0]
y_0 = [1.0]

!Abertura do disco para leitura de dados

open(unit=1,file='val.txt',&
status='old', action='read', iostat=ierror, iomsg= err_string)
open(unit=2,file='saida.txt',&
status='replace', action='write', iostat=ierror, iomsg= err_string)
open(unit=3,file='test.txt',&
status='replace', action='write', iostat=ierror, iomsg= err_string)
Do i = 1, train 
    read(1,*) dados(i,:)
End do


!Treinamento da rede

do l = 1, iteraction
    Erro = 0
    do m = 1, train
        Write(2,*) 'conjunto:', m
        !Leitura dos valores de entrada e os valores desejados
        do i = 1, size(x)
            x(i) = dados(m,i)    
        End do
        do i = 1, size(d)
            d(i) = dados(m,size(x)+i)    
        End do
        Write(2,*) 'soma dos pesos na camada intermediária'
        z_inactive = T_1
        call soma(wa,z_0,z_inactive) 
        call soma(wc,x,z_inactive)
        call soma(wb,y_0,z_inactive)  
        Write(2,*) 'z* =', z_inactive
        !Ativar as camadas intermediárias
        call sigmoide(z_inactive,z_active,dz)
        Write(2,*) 'z =', z_active
        Write(2,*) 'soma dos pesos na camada de saida'
        y_inactive = T_2
        call soma(V,z_active,y_inactive)
        Write(2,*) 'y* =', y_inactive
        !Ativar as camadas de saida
        call sigmoide(y_inactive,y_active,dy)
        Write(2,*) 'y =', y_active
        Write(2,*) 'Atualização dos pesos das camadas de saida'
        delt_0 = (d - y_active)
        call delta(delt_0,dy,u,delt_s)
        call upbias(alpha,delt_s,T_2)
        call upweight(alpha,delt_s,z_active,v)
        Write(2,*) 'bias saida =', T_2
        Write(2,*) 'pesos saida =', v
        Write(2,*) 'Atualização dos pesos das camadas intermediárias'
        call delta(delt_s,dz,v,delt_t)
        call upbias(alpha,delt_t,T_1)
        Write(2,*) 'bias camada escondida =', T_1 
        call upweight(alpha,delt_t,z,wa)
        Write(2,*) 'wa =', wa 
        call upweight(alpha,delt_t,y,wc)
        Write(2,*) 'wc =', wc
        call upweight(alpha,delt_t,x,wb)
        Write(2,*) 'wb =', wb 
        z_0 = z_active
        y_0 = y_active
    End do
    do m = 1, train
        !Leitura dos valores de entrada e os valores desejados
        do i = 1, size(x)
            x(i) = dados(m,i)    
        End do
        do i = 1, size(d)
            d(i) = dados(m,size(x)+i)    
        End do
        !Realizar soma dos pesos na camada intermediária
        z_inactive = T_1
        call soma(wa,z_0,z_inactive)
        call soma(wb,y_0,z_inactive)
        call soma(wc,x,z_inactive)
        !Ativar as camadas intermediárias
        call sigmoide(z_inactive,z_active,dz)
        z_0 = z_active
        !Realizar soma dos pesos na camada de saída
        y_inactive = T_2
        call soma(V,z_active,y_inactive)
        !Ativar as camadas de saida
        call sigmoide(y_inactive,y_active,dy)
        y_0 = y_active
        Do i = 1, output
            Erro = Erro + (d(i) - y_active(i))**2
        End do
    End do
    Write(2,*) 'Erro da iteração =', (0.5 * Erro)
    alpha = 0.95 * alpha
End do

if (test > 0) then
    do m = train + 1, train + test
        Write(2,*) 'Conjunto:', m
        !Leitura dos valores de entrada e os valores desejados
        do i = 1, size(x)
            x(i) = dados(m,i)    
        End do
        do i = 1, size(d)
            d(i) = dados(m,size(x)+i)    
        End do
        !Realizar soma dos pesos na camada intermediária
        z_inactive = T_1
        call soma(wa,z_0,z_inactive)
        call soma(wb,y_0,z_inactive)
        call soma(wc,x,z_inactive)
        !Ativar as camadas intermediárias
        call sigmoide(z_inactive,z_active,dz)
        z_0 = z_active
        !Realizar soma dos pesos na camada de saída
        y_inactive = T_2
        call soma(V,z_active,y_inactive)
        !Ativar as camadas de saida
        call sigmoide(y_inactive,y_active,dy)
        y_0 = y_active
        Do i = 1, output
            Erro = Erro + (d(i) - y_active(i))**2
        End do
    End do
    Write(2,*) 'Erro do algoritmo =', (0.5 * Erro)
End if

contains 

!Subrotina para soma ponderada 

subroutine soma(weight,input,summation)

Real, intent(in) :: weight(:,:), input(:)
Real, intent(in out) :: summation(:)
Integer :: i, j

Do i = 1,size(summation)  
    Do j = 1, size(input)   
        summation(i) = summation(i) + input(j) * weight(j,i)
    End do
End do

End subroutine soma

!subrotina para cálculo de função sigmoidal

subroutine sigmoide(x,Fx,dFx)

Real, intent (out) :: Fx(:), dFx(:)
Real, intent (in) :: x(:)
Real, parameter :: beta = 1

Fx = 1/(1+exp(-beta*x))
dFx = beta*exp(-beta*x)/((1+exp(-beta*x))**2)

End subroutine sigmoide

!subrotina para cálculo de função hiperbólica

subroutine hyperbolic(x,Fx,dFx)

Real, intent (out) :: Fx(:), dFx(:)
Real, intent (in) :: x(:)
Real, parameter :: beta = 1

Fx = tanh(beta*x)
dFx = beta*(1-(Fx**2))

End subroutine hyperbolic

!subrotina para calculo de erro por neurônio

subroutine delta(delta_s,dFx,weight,delta_i)

Real, intent(in) :: weight(:,:) 
Real, intent(in) :: delta_s(:), dFx(:)
Real, intent(out) :: delta_i(:) 
Integer :: i,j
    
delta_i = 0
Do i = 1,size(delta_i) 
    Do j = 1,size(delta_s)
        delta_i(i) = delta_i(i) + delta_s(j) * weight(i,j) * dFx(i)
    End do
End do 

End subroutine delta

!Subrotina para atualização dos bias

subroutine upbias(alpha,delta_i,bias)

Real, intent(in out) :: bias(:)
Real, intent(in) :: alpha, delta_i(:)
Integer :: i

Do i = 1, size(bias)
    bias(i) = bias(i) + alpha * delta_i(i)
End do

End subroutine upbias

!subrotina para atualização dos pesos

subroutine upweight(alpha,delta_i,input,weight)

Real, intent(in out) :: weight(:,:)
Real, intent(in) :: alpha, delta_i(:), input(:)
Integer :: i,j

Do i = 1, size(weight,1)
    Do j = 1, size(weight,2)
        weight(i,j) = weight(i,j) + alpha * delta_i(j) * input(i) 
    End do
End do

End subroutine upweight

End program recurrent