!! Nemo's branch

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
        integer :: valeur

        valeur = 0

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

        !/************************/
        !* Corps de la subroutine *
        !/************************/
    
    end subroutine affiche
end module projet

program projet_esteban_nemo
    use projet
    implicit none
    TYPE(tas) :: mon_tas
    integer :: nmin = 4, nmax = 32

    lecture_controlee(nmin, nmax)

end program projet_esteban_nemo