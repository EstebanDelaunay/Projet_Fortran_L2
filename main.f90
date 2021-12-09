module projet
    implicit none
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
        logical :: paire
    end type tas

    contains

    INTEGER function lecture_controlee(nmin, nmax)
        !! Retourne une valeur saisie par l'utilisateur 
        !! en s'assurant qu'elle est comprise entre nmin et nmax
        integer, INTENT(IN) :: nmin, nmax
        integer :: valeur

        do
            print*, "Entrez une valeur comprise entre : ", nmin, "et", nmax
            read*, valeur
            if (valeur >= nmin .AND. valeur <= nmax) exit 
        end do

        lecture_controlee = valeur
    end function lecture_controlee

    subroutine affiche(un_tas) 
        !! Affichage du tas
        type(tas),intent(inout) :: un_tas
        INTEGER :: line , colonne

        !Remise à zéro de la grille
        un_tas%grille = " "
        !call system("sleep 0.1")
        call system("cls")

        !Remplissage de la grille de caractère        
        do colonne = 0, un_tas%rayon -1
            do line = un_tas%hmax - un_tas%pile(colonne), un_tas%hmax-1 !Pars de la fin de la grille pour rajouter les grains
                un_tas%grille(line,colonne) = "O"
            end do
        end do
        
        !Affichage de la grille
        do line = 0, un_tas%hmax-1
            do colonne = 0, (un_tas%rayon-1)
                write(*, fmt="(1x,A1)",advance= "no") un_tas%grille(line,colonne)
            end do
            print *,""
        end do
    end subroutine affiche

    subroutine transfert_grain(un_tas, modif)
        !! Modifie le tableau pile pour simuler la chute des grains
        type(tas),intent(inout) :: un_tas
        REAL :: r, ng_real
        INTEGER :: compteur, ng
        logical, INTENT(OUT) :: modif

        if (un_tas%paire) then ! Demande si le tas est paire

            do compteur = 1, un_tas%rayon-2
                if(compteur <= un_tas%rayon/2) then !On est avant le milieu

                    if (un_tas%pile(compteur) >= un_tas%pile(compteur -1) +2 ) then !Est-ce que la pile du centre est plus grande que celle d'avant
                        CALL random_number(r)
                        ng_real = 1 + (0.5 * (2 + un_tas%pile(compteur) - un_tas%pile(compteur - 1)) * r)
                        ng = floor(ng_real)
                        un_tas%pile(compteur) = un_tas%pile(compteur) - ng
                        un_tas%pile(compteur-1) = un_tas%pile(compteur -1) + ng
                        modif = .true.
                    end if

                else !On est après le milieu

                    if (un_tas%pile(compteur) >= un_tas%pile(compteur +1) +2 ) then !Est-ce que la pile du centre est plus grande que celle d'après
                        CALL random_number(r)
                        ng_real = 1 + (0.5 * (2 + un_tas%pile(compteur) - un_tas%pile(compteur +1)) * r)
                        ng = floor(ng_real)
                        un_tas%pile(compteur) = un_tas%pile(compteur) - ng
                        un_tas%pile(compteur +1) = un_tas%pile(compteur +1) + ng
                        modif = .true.
                    end if
                    
                end if
            end do

        else !Si le tas est impaire
            do compteur = 1, un_tas%rayon -2
                if (compteur == un_tas%rayon/2) then !Si on est au milieu du tas

                    call random_number(r) !Nous choisissons aléatoirement si on va à gauche ou à droite
                    if (r < 0.5) then !On déplace le grain du milieu vers la gauche

                        if (un_tas%pile(compteur) >= un_tas%pile(compteur -1) +2 ) then !Est-ce que la pile du centre est plus grande que celle d'avant
                            CALL random_number(r)
                            ng_real = 1 + (0.5 * (2 + un_tas%pile(compteur) - un_tas%pile(compteur - 1)) * r)
                            ng = floor(ng_real)
                            un_tas%pile(compteur) = un_tas%pile(compteur) - ng
                            un_tas%pile(compteur-1) = un_tas%pile(compteur -1) + ng
                            modif = .true.
                        end if

                    else !On déplace le grain du milieu vers la droite

                        if (un_tas%pile(compteur) >= un_tas%pile(compteur +1) +2 ) then !Est-ce que la pile du centre est plus grande que celle d'après
                            CALL random_number(r)
                            ng_real = 1 + (0.5 * (2 + un_tas%pile(compteur) - un_tas%pile(compteur +1)) * r)
                            ng = floor(ng_real)
                            un_tas%pile(compteur) = un_tas%pile(compteur) - ng
                            un_tas%pile(compteur +1) = un_tas%pile(compteur +1) + ng
                            modif = .true.
                        end if

                    end if 
                
                else if(compteur < un_tas%rayon/2) then !On est avant le milieu

                    if (un_tas%pile(compteur) >= un_tas%pile(compteur -1) +2 ) then !Est-ce que la pile du centre est plus grande que celle d'avant
                        CALL random_number(r)
                        ng_real = 1 + (0.5 * (2 + un_tas%pile(compteur) - un_tas%pile(compteur - 1)) * r)
                        ng = floor(ng_real)
                        un_tas%pile(compteur) = un_tas%pile(compteur) - ng
                        un_tas%pile(compteur-1) = un_tas%pile(compteur -1) + ng
                        modif = .true.
                    end if

                else !On est après le milieu

                    if (un_tas%pile(compteur) >= un_tas%pile(compteur +1) +2 ) then !Est-ce que la pile du centre est plus grande que celle d'après
                        CALL random_number(r)
                        ng_real = 1 + (0.5 * (2 + un_tas%pile(compteur) - un_tas%pile(compteur +1)) * r)
                        ng = floor(ng_real)
                        un_tas%pile(compteur) = un_tas%pile(compteur) - ng
                        un_tas%pile(compteur +1) = un_tas%pile(compteur +1) + ng
                        modif = .true.
                    end if

                end if
            end do
        end if

    end subroutine transfert_grain

    SUBROUTINE init_rand
        INTEGER :: s , i , ok
        INTEGER, DIMENSION (:) , ALLOCATABLE :: seed

        CALL RANDOM_SEED(SIZE = s) ! Renvoie la taille du germe dans s
        ALLOCATE(seed(s) , STAT=ok) ! Alloue le tableau du germe
        IF (ok /= 0) STOP "init_rand : echec allocation seed !"
        
        DO i =1 , s
            CALL SYSTEM_CLOCK (COUNT= seed(i)) ! Initialise le germe avec l’horloge
        END DO
        
        CALL RANDOM_SEED(PUT= seed) ! Initialise l’aleatoire avec le germe
        DEALLOCATE(seed)
    END SUBROUTINE init_rand

    subroutine ask_affiche(logi_in)
        implicit none
        logical,intent(out) :: logi_in
        CHARACTER :: input

        do 
            print *, "Souhaitez-vous un affichage (o/n)"
            read *, input
            select case(input)
                case("o")
                    logi_in = .true.
                    exit
                case("n")
                    logi_in = .false.
                    exit
                case default
            end select
        end do
    
    end subroutine ask_affiche

end module projet

program projet_esteban_nemo
    use projet
    implicit none
    
    TYPE(tas) :: mon_tas
        !! Initialisation de la variable de type dérivé "tas"
    INTEGER, PARAMETER :: borne_inf = 3, borne_sup = 40, nt = 10
        !! Initialisations des constantes
    INTEGER :: ok, compteur = 0, i
    real :: rn
        !! Compteur du nombre de grains ajoutés
    logical :: affichage = .false., modif = .false.
    CHARACTER :: choix_affichage
    CHARACTER(50) :: nom_resultat

    call init_rand

    OPEN(unit = 10, file = "param.dat", ACTION = "READ", IOSTAT=ok)
    IF (ok/=0) STOP "Erreur ouverture pour le READ"

    READ(unit = 10, fmt = *) mon_tas%rayon
    READ(unit = 10, fmt = *) mon_tas%hmax
    READ(unit = 10, fmt = *) choix_affichage
    READ(unit = 10, fmt = *) nom_resultat

    select case (choix_affichage)
        case("o","y")
            affichage = .true.
        case("n")
            affichage = .false.
        case default    
            call ask_affiche(affichage)
    end select

    if (mon_tas%rayon < 3 .or. mon_tas%rayon > 40) then
        mon_tas%rayon=lecture_controlee(borne_inf,borne_sup)
    end if

    if (mon_tas%hmax < 3 .or. mon_tas%hmax > 40) then
        mon_tas%hmax=lecture_controlee(borne_inf,borne_sup)
    end if

    ! Allocation de tableaux et vérification
    ALLOCATE (mon_tas%pile(0:(mon_tas%rayon-1)) , mon_tas%grille(0:(mon_tas%hmax-1),0:(mon_tas%rayon-1)) , stat = ok) 
    IF (ok /= 0) STOP "Problème allocation !" 

    ! Initilisation des tableaux
    mon_tas%grille = " "; mon_tas%pile = 0

    ! Initilisation de la variable Paire du tas
    if (mod(mon_tas%rayon,2) == 0) mon_tas%paire = .true.

    ! Boucle principale du programme
    do
        if (maxval(mon_tas%pile) >= mon_tas%hmax) exit !Quitte la boucle si la hauteur max est atteinte

        if (mod(compteur,nt) == 0) then
            if (mon_tas%paire) then !Si le tas est pair
                call random_number(rn)
                if (rn < 0.5) then
                    mon_tas%pile((mon_tas%rayon/2)) = mon_tas%pile((mon_tas%rayon/2)) + 1
                else
                    mon_tas%pile((mon_tas%rayon/2)+1) = mon_tas%pile((mon_tas%rayon/2)+1) + 1
                end if
            else
                mon_tas%pile(mon_tas%rayon/2) = mon_tas%pile(mon_tas%rayon/2) + 1 !Ajout d'un grain au centre du tas
            end if
        end if

        call transfert_grain(mon_tas, modif) !Déplace les grains

        if (affichage .and. modif) then ! Affichage
            call affiche(mon_tas)
            print *, "========================="
            print *, " Affichage n", compteur
        end if

        modif = .false.
        compteur = compteur + 1
    end do

    !Création d'un fichier résultat avec une colonne affichant le  
    !nombre de pile et une autre le nb de grains dans cette pile.
    OPEN(unit=11 ,file = nom_resultat, ACTION = "WRITE", IOSTAT=ok)
    IF (ok/=0) STOP "Erreur ouverture pour le WRITE"
    do i = 0, mon_tas%rayon - 1
        WRITE(unit=11, fmt=*) i, mon_tas%pile(i)
    end do
    CLOSE(unit=11)

    DEALLOCATE (mon_tas%pile, mon_tas%grille)

    print *, "Pressez Entree pour fermer le programe"
    read *
end program projet_esteban_nemo 