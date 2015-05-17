
function polynome(x,q,r,s,t)              ! fonction permettant de crée un polynome de degré 0 à 3

real::x,q,r,s,t                      ! coefficients du polynôme allant de 3 à 0
real::polynome
real::res
polynome=(q*x**3)+(r*x**2)+(s*x**1)+(t*1) ! formule de l'équation

end function polynome

function trapeze(a,b,n,q,r,s,t)      ! fonction utilisant l'approximation d'intégrale par la méthode des trapèzes 
real::a,b,trapeze                    ! bornes de l'intervalle 
real::q,r,s,t                        ! coefficients du polynôme
integer::n                           ! nombre de pas
real::h,v,d                          ! variables utilisés pour les opérations de la fonction
integer::i                           ! boucle
d=0
h=(b-a)/n
v=(h/2)*(polynome(a,q,r,s,t)+polynome(b,q,r,s,t))
do i=1,n-1
d=d+polynome(a+i*h,q,r,s,t)
end do
trapeze=v+d*h
end function trapeze

function simpson(a,b,n,q,r,s,t)      ! fonction utilisant l'approximation d'intégrale par la méthode de simpson
real::a,b,simpson                    ! bornes de l'intervalle 
real::q,r,s,t                        ! coefficients du polynôme
integer::n,i                         ! nombre de pas et boucle 
real::m,sommep,sommei                ! somme en fonction de l'indice paire ou impaire de l'abscisse de l'antécédent de la fonction

m=(b-a)/(n*2)
sommep=0
sommei=0

do i=1,n-1 
sommei=sommei+polynome(a+m*2*i,q,r,s,t)
end do

do i=1,n
sommep=sommep+polynome(a+m*(2*i-1),q,r,s,t)
end do

simpson=m*(polynome(a,q,r,s,t)+polynome(b,q,r,s,t)+2*sommep+4*sommei)/3

end function



