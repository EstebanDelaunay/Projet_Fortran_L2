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
        !! Affichage du tas
        implicit none
        type(tas),intent(inout) :: un_tas
        INTEGER :: line , colonne

        !Remplissage de la grille de caractère        
        do colonne = 0, un_tas%rayon -1
            if (un_tas%pile(colonne) /= 0) then
                do line = un_tas%hmax - un_tas%pile(colonne), un_tas%hmax-1 !Pars de la fin de la grille pour rajouter les grains
                    un_tas%grille(line,colonne) = "O"
                end do
            end if
        end do   
        
        !Affichage de la grille
        do line = 0, un_tas%hmax-1
            do colonne = 0, (un_tas%rayon-1)
                write(*, fmt="(1x,a,i0)",advance= "no") un_tas%grille(line,colonne)
            end do
            print *,""
        end do

    end subroutine affiche

    subroutine transfert_grain(pile, rmax)
        !! Modifie le tableau pile pour simuler la chute des grains
        !! Ajoute un grain dans une des colonnes de façon aléatoire
        INTEGER, DIMENSION(:), INTENT(INOUT) :: pile
        INTEGER, INTENT(IN) :: rmax 
            !! Taille du tableau où sont affichés les nombres de grains
        REAL :: r
        INTEGER :: i

        CALL random_seed
        CALL random_number(r)

        r = r * (rmax + 1)
        i = floor(r)

        pile = pile(i) + 1

    end subroutine transfert_grain

end module projet

program projet_esteban_nemo
    use projet
    implicit none
    TYPE(tas) :: mon_tas
    integer, PARAMETER :: borne_inf = 3, borne_sup = 40, nt = 10
    INTEGER :: ok, i = 0
    
    ! Demande à l'utilisateur les valeurs du rayon et la hauteur
    print *, "Rentrez le rayon maximum du tas" 
    mon_tas%rayon = lecture_controlee(borne_inf, borne_sup)
    print *, "Rentrez la hauteur maximum du tas"
    mon_tas%hmax = lecture_controlee(borne_inf, borne_sup)

    ! Allocation de tableaux et vérification
    ALLOCATE (mon_tas%pile(0:(mon_tas%rayon-1)) , mon_tas%grille(0:(mon_tas%hmax-1),0:(mon_tas%rayon-1)) , stat = ok) 
    IF (ok /= 0) STOP "Problème allocation !" 

    ! Initilisation des tableaux
    mon_tas%grille = " "; mon_tas%pile = 0

    ! Boucle principale du programme
    do
        if (maxval(mon_tas%pile) >= mon_tas%hmax) exit !Quitte la boucle si la hauteur max est atteinte
        if (mod(i,nt) == 0) mon_tas%pile(0) = mon_tas%pile(0) + 1 !Ajout d'un grain tout les nt
        call transfert_grain(mon_tas%pile, mon_tas%rayon)

        call affiche(mon_tas)
        print *, "========================="
        print *, " Affichage n", i
        print *, ""

        i = i + 1
    end do

    DEALLOCATE (mon_tas%pile, mon_tas%grille)
end program projet_esteban_nemo