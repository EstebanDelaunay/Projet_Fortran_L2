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

    function lecture_controlee(nmin, nmax) result(valeur)
        !! Retourne une valeur saisie par l'utilisateur 
        !! en s'assurant qu'elle est comprise entre nmin et nmax

        implicit none
        integer :: nmin, nmax
        integer :: valeur

        !/**********************/
        !* Corps de la fonction *
        !/**********************/
        
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
    
    !/********************/
    !* Corps du programme *
    !/********************/

end program projet_esteban_nemo