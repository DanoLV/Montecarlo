<<<<<<< HEAD
!-----------------------------------------------------------------
! Trabajo practico: Guia de Ising
! Año: 2023
! Curso: Introducción a la simulación computacional
! Docente: Claudio Pastorino
! URL: https://www.tandar.cnea.gov.ar/~pastorin/cursos/intro_sims/
!-----------------------------------------------------------------

program ising 
    use ziggurat
    use IsingModule
    implicit none
    logical :: es, aceptar
    integer :: seed, i, num = 0, stepsMC, nstepscalc, nprint, nstepsnosave
    real (kind=8) :: Energia, E_media, Magnetizacion, Mag_media, T=0, JJ, dE, &
                    Mag_cuad, E_cuad, sigmaE2, sigmaM2, cv, XMag, faceptado
    INTEGER , allocatable :: M(:,:)
    character(len=50) :: infile = '', outfile = ''

!************************************************
! Manejar argumentos de linea de comandos
    integer :: num_args, ix, stat
    character(len=50), dimension(:), allocatable :: args

    num_args = command_argument_count()
    if ( num_args > 0 ) then
        

        allocate(args(num_args))  

        do ix = 1, num_args

            call get_command_argument(ix,args(ix))

            ! Parser de opciones
            select case(args(ix))
                ! dimension de la matriz
                case('-n')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  num
                ! Archivo de entrada de matriz
                case('-i')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  infile
                ! Archivo de salida de matriz    M1(num,num)
                case('-o')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  outfile
                case('-T')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  T    
                case('-s')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  stepsMC     
            end select   

        end do
    end if

!************************************************    
![NO TOCAR] Inicializa generador de número random

    inquire(file='seed.dat',exist=es)
    if(es) then
        open(unit=10,file='seed.dat',status='old')
        read(10,*) seed
        close(10)
        ! print *,"  * Leyendo semilla de archivo seed.dat"
    else
        seed = 24583490
    end if

    call zigset(seed)
![FIN NO TOCAR]    
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!! Inicializar variables
    if ( T == 0 ) then 
        T = 0.1
    end if

    if ( num == 0 ) then 
        num = 20
    end if
    if ( stepsMC == 0 ) then 
        stepsMC = 500000
    end if

    JJ = 1

    if ( infile .ne. '' ) then

        num = 0
        open(unit=100,file=infile,status='old',iostat=stat)
        read(100,*,iostat=stat) num
        allocate(M(num,num))

        do ix = 1, num
            ! print * , ix
            read(100,*,iostat=stat) M(ix,:)
        end do

        close(100)
        ! call PrintMat(num,M, stdout)
    else
        allocate(M(num,num))
        call InitMat(num,M)             
    end if

    ! if ( outfile .ne. '' ) then
    !     open(unit=100,file=outfile,status='old',iostat=stat)
    !     write(100,*) num
    !     ! call PrintMat(num,M, 100)
    !     close(100)
    ! end if

    Magnetizacion =  CalcMagnet( num, M )
    Energia =  CalcEnergy(num, M, JJ)
    Mag_media =  Magnetizacion/num**2 
    E_media = Energia 
    dE=0
    nstepscalc= 200
    nprint = 0
    nstepsnosave= 200000
    
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!! Pasos de Montecarlo 
    
    !Pasos que no voy a guardar
    do i = 1, nstepsnosave
        ! Dar vuelta spin y calcular nuevos valores de Energia, Magnetizacion, deltas, etc
        call SpinFlip(num, M, 1/T, JJ, Energia, Magnetizacion, dE, aceptar)
    end do

    Magnetizacion =  CalcMagnet( num, M )
    Energia =  CalcEnergy(num, M, JJ)
    Mag_media =  0 !Magnetizacion/num**2 
    E_media = 0 !Energia 
    dE=0
    faceptado = 0

    do i = 1, stepsMC

        ! Dar vuelta spin y calcular nuevos valores de Energia, Magnetizacion, deltas, etc
        call SpinFlip(num, M, 1/T, JJ, Energia, Magnetizacion, dE, aceptar)
        !Acumulo valores
        E_media = Energia + E_media
        Mag_media = Magnetizacion/num**2 + Mag_media
        E_cuad = E_cuad + Energia**2
        Mag_cuad = Mag_cuad + (Magnetizacion/num**2)**2

        if (aceptar ) then
            faceptado = faceptado + 1
        end if

        if ( MOD(i,nstepscalc)== 0 ) then
            !Calculo medias en los pasos dados
            E_media = E_media/(nstepscalc)
            E_cuad = E_cuad/(nstepscalc)
            Mag_media = Mag_media/(nstepscalc)
            Mag_cuad = Mag_cuad/(nstepscalc)
            faceptado = faceptado/nstepscalc

            ! sigmaE2 = E_cuad - E_media**2 
            ! sigmaM2 = Mag_cuad - Mag_media**2 
            ! cv = (sigmaE2 * (1/T)**2)/ num**2
            ! XMag = sigmaM2 /T

            nprint = nprint + 1
            print * ,nprint, E_media, Mag_media, T, num, faceptado
            Mag_media =  0!Magnetizacion/num**2
            E_media = 0!Energia
            E_cuad = 0!Energia**2
            Mag_cuad = 0!(Magnetizacion/num**2)**2
            faceptado = 0
            
        end if
    end do

    if ( outfile .ne. '' ) then
        open(unit=100,file=outfile,status='old',iostat=stat)
        write(100,*) num
        call PrintMat(num,M, 100)
        close(100)
    end if
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

        open(unit=10,file='seed.dat',status='unknown')
        seed = shr3() 
        write(10,*) seed
        close(10)
![FIN no Tocar]        


=======
!-----------------------------------------------------------------
! Trabajo practico: Guia de Ising
! Año: 2023
! Curso: Introducción a la simulación computacional
! Docente: Claudio Pastorino
! URL: https://www.tandar.cnea.gov.ar/~pastorin/cursos/intro_sims/
!-----------------------------------------------------------------

program ising 
    use ziggurat
    use IsingModule
    implicit none
    logical :: es, aceptar
    integer :: seed,i ,j,k,vec(10), num = 0, stepsMC, nstepscalc, nprint, nstepsnosave
    real (kind=8) :: x,y,r(10),rr(3,10), Energia, E_media, Magnetizacion, Mag_media, T=0, JJ, dE, dM,difE, &
                    Mag_cuad, E_cuad, sigmaE2, sigmaM2, cv, XMag
    INTEGER , allocatable :: M(:,:)
    character(len=50) :: infile = '', outfile = '', string

!************************************************
! Manejar argumentos de linea de comandos
    integer :: num_args, ix, stat
    character(len=50), dimension(:), allocatable :: args

    num_args = command_argument_count()
    if ( num_args > 0 ) then
        

        allocate(args(num_args))  

        do ix = 1, num_args

            call get_command_argument(ix,args(ix))

            ! Parser de opciones
            select case(args(ix))
                ! dimension de la matriz
                case('-n')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  num
                ! Archivo de entrada de matriz
                case('-i')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  infile
                ! Archivo de salida de matriz    M1(num,num)
                case('-o')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  outfile
                case('-T')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  T    
                case('-s')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  stepsMC     
            end select   

        end do
    end if

!************************************************    
![NO TOCAR] Inicializa generador de número random

    inquire(file='seed.dat',exist=es)
    if(es) then
        open(unit=10,file='seed.dat',status='old')
        read(10,*) seed
        close(10)
        ! print *,"  * Leyendo semilla de archivo seed.dat"
    else
        seed = 24583490
    end if

    call zigset(seed)
![FIN NO TOCAR]    
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!! Inicializar variables
    if ( T == 0 ) then 
        T = 0.1
    end if

    if ( num == 0 ) then 
        num = 20
    end if
    if ( stepsMC == 0 ) then 
        stepsMC = 500000
    end if

    JJ = 1

    if ( infile .ne. '' ) then

        num = 0
        open(unit=100,file=infile,status='old',iostat=stat)
        read(100,*,iostat=stat) num
        allocate(M(num,num))

        do ix = 1, num
            ! print * , ix
            read(100,*,iostat=stat) M(ix,:)
        end do

        close(100)
        ! call PrintMat(num,M, stdout)
    else
        allocate(M(num,num))
        call InitMat(num,M)             
    end if

    ! if ( outfile .ne. '' ) then
    !     open(unit=100,file=outfile,status='old',iostat=stat)
    !     write(100,*) num
    !     ! call PrintMat(num,M, 100)
    !     close(100)
    ! end if

    Magnetizacion =  CalcMagnet( num, M )
    Energia =  CalcEnergy(num, M, JJ)
    Mag_media =  Magnetizacion/num**2 
    E_media = Energia 
    dE=0
    nstepscalc= 200
    nprint = 0
    nstepsnosave= 200000
    
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!! Pasos de Montecarlo 
    
    !Pasos que no voy a guardar
    do i = 1, nstepsnosave
        ! Dar vuelta spin y calcular nuevos valores de Energia, Magnetizacion, deltas, etc
        call SpinFlip(num, M, 1/T, JJ, Energia, Magnetizacion, dE, aceptar)
    end do

    Magnetizacion =  CalcMagnet( num, M )
    Energia =  CalcEnergy(num, M, JJ)
    Mag_media =  Magnetizacion/num**2 
    E_media = Energia 
    dE=0

    do i = 1, stepsMC

        ! Dar vuelta spin y calcular nuevos valores de Energia, Magnetizacion, deltas, etc
        call SpinFlip(num, M, 1/T, JJ, Energia, Magnetizacion, dE, aceptar)
        E_media = Energia + E_media
        Mag_media = Magnetizacion/num**2 + Mag_media
        E_cuad = E_cuad + Energia**2
        Mag_cuad = Mag_cuad + (Magnetizacion/num**2)**2
        
        if ( MOD(i,nstepscalc)== 0 ) then
            E_media = E_media/(nstepscalc+1)
            E_cuad = E_cuad/(nstepscalc+1)
            Mag_media = Mag_media/(nstepscalc+1)
            Mag_cuad = Mag_cuad/(nstepscalc+1)
            

            sigmaE2 = E_cuad - E_media**2 
            sigmaM2 = Mag_cuad - Mag_media**2 
            cv = (sigmaE2 * (1/T)**2)/ num**2
            XMag = sigmaM2 /T

            nprint = nprint + 1
            print * ,nprint, E_media, Mag_media, T, num !sigmaE2, sigmaM2,E_cuad/(nstepscalc+1), Mag_cuad/(nstepscalc+1),beta
            Mag_media =  Magnetizacion/num**2
            E_media = Energia
            E_cuad = Energia**2
            Mag_cuad = (Magnetizacion/num**2)**2

        end if
    end do

    if ( outfile .ne. '' ) then
        open(unit=100,file=outfile,status='old',iostat=stat)
        write(100,*) num
        call PrintMat(num,M, 100)
        close(100)
    end if
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

        open(unit=10,file='seed.dat',status='unknown')
        seed = shr3() 
        write(10,*) seed
        close(10)
![FIN no Tocar]        


>>>>>>> 1c434cdd02e62efc0723d409370c0a1fa82b19cf
end program ising