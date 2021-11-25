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
            !! Tableau destiné à l'affichage du tas de sable
    end type tas

    contains

    INTEGER function lecture_controlee(nmin, nmax)
        !! Retourne une valeur saisie par l'utilisateur 
        !! en s'assurant qu'elle est comprise entre nmin et nmax

        implicit none
        INTEGER, INTENT(IN) :: nmin, nmax
        INTEGER :: valeur

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
        TYPE(tas),INTENT(IN) :: un_tas

        !/************************/
        !* Corps de la subroutine *
        !/************************/
    
    end subroutine affiche

    subroutine transfert_grain(pile)
        !! Modifie le tableau pile pour simuler la chute des grains
        !! Ajoute un grain dans une des colonnes de façon aléatoire
        INTEGER, DIMENSION(:), INTENT(INOUT) :: pile
        INTEGER, INTENT(IN) :: rmax = montas%rayon !taille du tableau où sont affichés les nombres de grains
        REAL :: r
        INTEGER :: i

        CALL random_seed
        CALL random_number(r)

        r = r * (rmax + 1)
        i = floor(r)

        pile = pile(i) + 1

    end subroutine transfert_grain

end module projet

program projet_esteban_nemo !Ajouter les tableaux mon_tas%pile et mon_tas%grille dans la déclaration 
    use projet
    implicit none
    TYPE(tas) :: mon_tas
    INTEGER, PARAMETER :: borne_inf = 3, borne_sup = 40, nt = 10
    INTEGER :: i = 0 !compteur du nombre de grains ajoutés

    mon_tas%rayon = lecture_controlee(borne_inf, borne_sup)
    mon_tas%hmax = lecture_controlee(borne_inf, borne_sup)

    ALLOCATE(mon_tas%pile(0 : mon_tas%rayon - 1), mon_tas%grille(mon_tas%rayon,mon_tas%hmax))

    do
        if (maxval(mon_tas%pile) >= mon_tas%hmax) exit 
            !! Quitte la boucle si la hauteur max est atteinte
        if (mod(i,nt) /= 0) mon_tas%pile(0) = mon_tas%pile(0) + 1 
            !! Ajout d'un grain tout les nt
        call transfert_grain(mon_tas%pile)
        call affiche(mon_tas)
        i = i + 1
    end do

end program projet_esteban_nemo