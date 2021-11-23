!! ESTEBAN BRANCH

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
            !! Tableau déstiné à l'affichage du tas de sable
    end type tas

    contains

    integer function lecture_controlee(nmin, nmax)
        !! Retourne une valeur saisie par l'utilisateur 
        !! en s'assurant qu'elle est comprise entre nmin et nmax

        implicit none
        integer :: nmin, nmax
        integer :: valeur

        !/**********************/
        !* Corps de la fonction *
        !/**********************/
        
    end function lecture_controlee

    logical function lecture_hauteur(pile, r_max)
        implicit none
        integer, DIMENSION(:), INTENT(IN) :: pile
        INTEGER, INTENT(IN) :: r_max
        INTEGER :: i, h_max

        h_max = pile(1)
        
        do i=2, r_max
            if (pile(i) > pile(i-1)) h_max = pile(i)
        end do

        lecture_hauteur = h_max
        
        
    end function lecture_hauteur

    subroutine affiche(un_tas) 
        !! Affiche l'état du tas
        implicit none
        type(tas),intent(in) :: un_tas

        !/************************/
        !* Corps de la subroutine *
        !/************************/
    
    end subroutine affiche
end module projet

program projet_esteban_nemo
    use projet
    implicit none
    TYPE(tas) :: mon_tas
    integer, PARAMETER :: borne_inf = 3, borne_sup = 40

    mon_tas%rayon = lecture_controlee(borne_inf, borne_sup)
    mon_tas%hmax = lecture_controlee(borne_inf, borne_sup)

    ALLOCATE(mon_tas%pile(mon_tas%rayon), mon_tas%grille(mon_tas%rayon,mon_tas%hmax))

    do
        
    end do

end program projet_esteban_nemo