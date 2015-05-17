!=================================================================================================
!================================  Projet Fortran année 2014-2015  ===============================
!================================LOVRIC Quentin POLYTECH Paris UPMC===============================
!================================     Méthodes de quadrature       ===============================
!================================      Trapèzes et Simpson         ===============================
!=================================================================================================


program fonc
implicit none

interface

function polynome(x,q,r,s,t)         ! équation
real::x,q,r,s,t
real::polynome
end function

function trapeze(a,b,n,q,r,s,t)     ! méthode des trapèzes
real::a,b,trapeze,q,r,s,t
integer::n
end function

function simpson(a,b,n,q,r,s,t)     ! méthode de simpson
real::a,b,q,r,s,t
integer::n
real::simpson
end function

end interface

real::a                             ! borne inférieure
real::b                             ! borne supérieure
integer::n                          ! nombre de sous intervalles
integer::choix                      ! utilisé pour que l'utilisateur puisse choisir les approximations qu'il veut faire
real::airecal,airecal2              ! aire calculée avec la méthode des trapèzes et de simpson
real::res                           ! équation 
real::q,r,s,t,x                     ! coefficients à entrer pour obtenir l'équation

type affichage                      ! type dérivé utilisé pour le case(5)
real::inf
real::sup
real::qcoef
real::rcoef
real::scoef
real::tcoef
integer::pas
end type
type(affichage)::aff

write(*,"('================================================================================')")
write(*,"('=======================  Projet Fortran année 2014-2015  =======================')")
write(*,"('=======================LOVRIC Quentin POLYTECH Paris UPMC=======================')")
write(*,"('=======================      Méthodes de quadrature      =======================')")
write(*,"('=======================        Trapèzes et Simpson       =======================')")
write(*,"('================================================================================')")
write(*,*)
write(*,"('Tapez 1 si vous voulez sortir du programme')")                                                         ! Divers choix d'actions données pour l'utilisateur lorsqu'il lance le programme
write(*,"('Tapez 2 si vous voulez seulement utiliser la méthode des trapèzes')")
write(*,"('Tapez 3 si vous voulez seulement utiliser la méthode de simpson')")
write(*,"('Tapez 4 si vous voulez utiliser les deux méthodes pour établir une comparaison')")
write(*,"('Tapez 5 si vous voulez utiliser les deux méthodes et afficher les différents paramètres')")
read(*,*)choix

call system ("clear")
select case(choix)

case(1)
stop

case(2)
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 3')")                                ! Différentes valeurs entrées par l'utilisateur pour approxime l'intégration
read(*,*)q
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 2')")
read(*,*)r
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 1')")
read(*,*)s
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 0')")
read(*,*)t
write(*,"('Voici l equation:'f8.5,3x)")
write(*,"(f7.3,'*x^3+',f7.3,'*x^2+',f7.3,'*x^1+',f7.3)")q,r,s,t
write(*,"('Veuillez entrer la valeur de la borne inférieure')")
read(*,*)a
write(*,"('Veuillez entrer la valeur de la borne supérieure')")
read(*,*)b
do while (b.LE.a)
write(*,"('la borne supérieure doit avoir une valeur plus élevée que la borne inférieure')")
read(*,*)b
end do
write(*,"('Veuillez entrer la valeur entière de n sous intervalles')")
read(*,*)n
call system("clear")
write(*,"('Resultat:')")
write(*,*)
res=polynome(q,r,s,t,x)
airecal=trapeze(a,b,n,q,r,s,t)                                                                              ! aire calculée par la méthode des trapèzes
write(*,*)airecal

case(3)
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 3')")
read(*,*)q
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 2')")
read(*,*)r
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 1')")
read(*,*)s
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 0')")
read(*,*)t
write(*,"('Voici l equation:'f8.5,3x)")
write(*,"(f7.3,'*x^3+',f7.3,'*x^2+',f7.3,'*x^1+',f7.3)")q,r,s,t
write(*,"('Veuillez entrer la valeur de la borne inférieure')")
read(*,*)a
write(*,"('Veuillez entrer la valeur de la borne supérieure')")
read(*,*)b
do while (b.LE.a)
write(*,"('la borne supérieure doit avoir une valeur plus élevée que la borne inférieure')")
read(*,*)b
end do
write(*,"('Veuillez entrer la valeur entière de n sous intervalles')")
read(*,*)n
call system("clear")
write(*,"('Resultat:')")
write(*,*)
res=polynome(q,r,s,t,x)
airecal2=simpson(a,b,n,q,r,s,t)
write(*,*)airecal2                                                                                         ! aire calculée par la méthode de simpson

case(4)
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 3')")
read(*,*)q
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 2')")
read(*,*)r
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 1')")
read(*,*)s
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 0')")
read(*,*)t
write(*,"('Voici l equation:'f8.5,3x)")
write(*,"(f7.3,'*x^3+',f7.3,'*x^2+',f7.3,'*x^1+',f7.3)")q,r,s,t
write(*,"('Veuillez entrer la valeur de la borne inférieure')")
read(*,*)a
write(*,"('Veuillez entrer la valeur de la borne supérieure')")
read(*,*)b
do while (b.LE.a)
write(*,"('la borne supérieure doit avoir une valeur plus élevée que la borne inférieure')")
read(*,*)b
end do
write(*,"('Veuillez entrer la valeur entière de n sous intervalles')")
read(*,*)n
call system("clear")
write(*,"('Resultat:')")
write(*,*)
res=polynome(q,r,s,t,x)
airecal=trapeze(a,b,n,q,r,s,t)                                                                            ! aire calculée par les 2 méthodes
write(*,*)airecal
airecal2=simpson(a,b,n,q,r,s,t)
write(*,*)airecal2
write(*,*)


case(5)
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 3')")
read(*,*)q
aff%qcoef=q
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 2')")
read(*,*)r
aff%rcoef=r
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 1')")
read(*,*)s
aff%scoef=s
write(*,"('Veuillez rentrer la valeur du coefficient pour le degré 0')")
read(*,*)t
aff%tcoef=t
write(*,"('Voici l equation:'f8.5,3x)")
write(*,"(f7.3,'*x^3+',f7.3,'*x^2+',f7.3,'*x^1+',f7.3)")q,r,s,t
write(*,"('Veuillez entrer la valeur de la borne inférieure')")
read(*,*)a
aff%inf=a
write(*,"('Veuillez entrer la valeur de la borne supérieure')")
read(*,*)b
do while (b.LE.a)
write(*,"('la borne supérieure doit avoir une valeur plus élevée que la borne inférieure')")
read(*,*)b
end do
aff%sup=b
write(*,"('Veuillez entrer la valeur entière de n sous intervalles')")
read(*,*)n
aff%pas=n
call system("clear")
res=polynome(q,r,s,t,x)                                                     ! aire calculée avec les 2 méthodes mais avec le détail des informations rentrées 
airecal=trapeze(a,b,n,q,r,s,t) 
write(*,*)                                             
write(*,"('le pas :')",advance='no');write(*,*)aff%pas
write(*,"('borne inférieure :')",advance='no');write(*,*)aff%inf
write(*,"('borne supérieure :')",advance='no');write(*,*)aff%sup
write(*,"('l équation :',f7.3,'*x^3+',f7.3,'*x^2+',f7.3,'*x^1+',f7.3)")aff%qcoef,aff%rcoef,aff%scoef,aff%tcoef
write(*,*)
write(*,"('Resultat:')")
write(*,*)
write(*,*)airecal
airecal2=simpson(a,b,n,q,r,s,t)
write(*,*)airecal2
write(*,*)

case default
write(*,"('Choix incorrect')")
end select

stop
end program
