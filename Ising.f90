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
    integer :: seed, i, num = 0, stepsMC=0, nstepscalc, nprint, nstepsnosave=0
    real (kind=8) :: Energia, E_media, Magnetizacion, Mag_media, T=0, JJ, dE, &
                    Mag_cuad, E_cuad, faceptado !sigmaE2, sigmaM2, cv, XMag, faceptado
    INTEGER , allocatable :: M(:,:)
    character(len=100) :: infile = '', outfile = '', msg

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

                ! Archivo de salida de matriz 
                case('-o')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  outfile

                ! Temperatura
                case('-T')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  T  

                ! Pasos de montecarlo a ejecutar  
                case('-s')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  stepsMC 

                ! Pasos de MC para termalizar
                case('-sns')
                    call get_command_argument(ix + 1,args(ix + 1))
                    read(args(ix+1),*,iostat=stat)  nstepsnosave  
                    
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

    if (nstepsnosave == 0) then
        nstepsnosave= 200000
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

    else
        allocate(M(num,num))
        call InitMat(num,M)             
    end if

    Magnetizacion =  CalcMagnet( num, M )
    Energia =  CalcEnergy(num, M, JJ)
    Mag_media =  Magnetizacion/num**2 
    E_media = Energia 
    dE=0
    nstepscalc= 200
    nprint = 0
    
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!! Pasos de Montecarlo 
    
    !Pasos que no voy a guardar y son para termalizar
    do i = 1, nstepsnosave
        ! Dar vuelta spin y calcular nuevos valores de Energia, Magnetizacion, deltas, etc
        call SpinFlip(num, M, 1/T, JJ, Energia, Magnetizacion, dE, aceptar)
    end do

    ! Inicializo valores antes de los pasos de MC que se guardan
    Magnetizacion =  CalcMagnet( num, M )
    Energia =  CalcEnergy(num, M, JJ)
    Mag_media =  0 
    E_media = 0 
    dE=0
    faceptado = 0

    do i = 1, stepsMC

        ! Dar vuelta spin y calcular nuevos valores de Energia, Magnetizacion, deltas, etc
        call SpinFlip(num, M, 1/T, JJ, Energia, Magnetizacion, dE, aceptar)
        !Acumulo valores
        E_media = Energia + E_media
        Mag_media = Magnetizacion + Mag_media
        ! E_cuad = E_cuad + Energia**2
        ! Mag_cuad = Mag_cuad + (Magnetizacion)**2

        ! Contabilizar spin flips aceptados
        if (aceptar ) then
            faceptado = faceptado + 1
        end if

        ! Cualdo hice nstepscalc pasos hago los calculos a guardar
        if ( MOD(i,nstepscalc)== 0 ) then
            !Calculo medias en los pasos dados
            E_media = E_media/(nstepscalc)
            ! E_cuad = E_cuad/(nstepscalc)
            Mag_media = Mag_media/(nstepscalc)
            ! Mag_cuad = Mag_cuad/(nstepscalc)
            faceptado = faceptado/nstepscalc

            nprint = nprint + 1
            print * ,nprint, E_media, Mag_media, T, num, faceptado

            ! Inicializo valores para el proximo ciclo
            Mag_media =  0
            E_media = 0
            ! E_cuad = 0
            ! Mag_cuad = 0
            faceptado = 0
            
        end if
    end do

    ! Guardo ultima matriz utilizada a un archivo 
    if ( outfile .ne. '' ) then
        open(unit=100,file=outfile,status='replace',access='sequential',form='formatted',iostat=stat, iomsg=msg)
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

end program ising