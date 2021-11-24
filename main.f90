module projet

    type tas 
        !! Type dérivé
        INTEGER :: rayon 
            !! Nombre de piles
        INTEGER :: hmax 
            !! Hauteur max du tas de sable
        INTEGER, DIMENSION(:), ALLOCATABLE :: pile 
            !! Tableau du nombre de grains par pile
        CHARACTER(1), DIMENSION(:,:), ALLOCATABLE :: grille 
            !! Tableau destiné à l'affichage du tas de sable
    end type tas

    contains

    INTEGER function lecture_controlee(nmin, nmax)
        !! Retourne une valeur saisie par l'utilisateur 
        !! en s'assurant qu'elle est comprise entre nmin et nmax
        implicit none
        integer, INTENT(IN) :: nmin, nmax
        integer :: valeur = 0

        do
            print*, "Entrez une valeur comprise entre : ", nmin, "et", nmax
            read*, valeur
            if (valeur > nmin .AND. valeur < nmax) exit 
        end do
        lecture_controlee = valeur

    end function lecture_controlee

    subroutine affiche(un_tas) 
        !! Affiche l'état du tas
        implicit none
        type(tas),intent(in) :: un_tas
        INTEGER :: line , colonne
        !CHARACTER, DIMENSION(un_tas%hmax,un_tas%rayon) :: tab
        
        do line = 1, un_tas%hmax
            do colonne = 1, (un_tas%rayon)
                write(*, fmt="(1x,a,i0)",advance= "no") un_tas%grille(line,colonne)
            end do
            print *,""
        end do
        
    
    end subroutine affiche

    !subroutine transfert_grain(pile)
    !    !! Modifie le tableau pile pour simuler la chute des grains
    !    INTEGER, DIMENSION(:), intent(inout) :: pile
    !
    !
    !end subroutine transfert_grain

end module projet

program projet_esteban_nemo
    use projet
    implicit none
    TYPE(tas) :: mon_tas
    integer, PARAMETER :: borne_inf = 3, borne_sup = 40, nt = 10
    !INTEGER :: i = 0
    INTEGER :: ok
    
    ! /**/ Demande à l'user les valeurs du rayon et la hauteur /**/
    !print *, "Rentrez le rayon maximum du tas" 
    mon_tas%rayon = 10
    !mon_tas%rayon = lecture_controlee(borne_inf, borne_sup)
    !print *, "Rentrez la hauteur maximum du tas"
    !mon_tas%hmax = lecture_controlee(borne_inf, borne_sup)
    mon_tas%hmax = 5

    ALLOCATE ( mon_tas%pile(0:(mon_tas%rayon - 1)) , mon_tas%grille(mon_tas%hmax,mon_tas%rayon ) , stat = ok) 
    IF (ok /= 0) STOP "Problème allocation !" 

    mon_tas%grille = "0" ; mon_tas%grille(1,2:) = " " ; mon_tas%grille(2,3:) = " " ; mon_tas%grille(3,4:) = " "
    mon_tas%grille(4,6:) = " " ; mon_tas%grille(5,8:) = " "

    call affiche(mon_tas)

    !do
    !    if (maxval(mon_tas%pile) >= mon_tas%hmax) exit 
    !        !! Quitte la boucle si la hauteur max est atteinte
    !    if (mod(i,nt) /= 0) mon_tas%pile(0) = mon_tas%pile(0) + 1 
    !        !! Ajout d'un grain tout les nt
    !    call transfert_grain(mon_tas%pile)
    !    call affiche(mon_tas)
    !    i = i + 1
    !end do


    DEALLOCATE (mon_tas%pile, mon_tas%grille)
end program projet_esteban_nemo