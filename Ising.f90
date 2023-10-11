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
    logical :: es
    integer :: seed,i ,j,k,vec(10), num = 0, stepsMC
    real (kind=8) :: x,y,r(10),rr(3,10), Energia, beta=0, JJ
    INTEGER , allocatable :: M(:,:)
    character(len=50) :: infile = '', outfile = '', string
!************************************************
! Manejar argumentos        
    integer :: num_args, ix, stat
    character(len=50), dimension(:), allocatable :: args

    num_args = command_argument_count()
    allocate(args(num_args))  ! I've omitted checking the return status of the allocation 

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
            case('-b')
                call get_command_argument(ix + 1,args(ix + 1))
                read(args(ix+1),*,iostat=stat)  beta    
            case('-s')
                call get_command_argument(ix + 1,args(ix + 1))
                read(args(ix+1),*,iostat=stat)  stepsMC     
        end select   

    end do
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
    if ( beta == 0 ) then 
        beta = 0.01
    end if
    
    if ( num == 0 ) then 
        num = 20
    end if
    if ( stepsMC == 0 ) then 
        stepsMC = 1000000
    end if

    JJ = 1

    if ( infile .ne. '' ) then

        num = 0
        print *, infile
        open(unit=100,file=infile,status='old',iostat=stat)
        print *, num, stat
        read(100,*,iostat=stat) num
        print *, num, stat
        allocate(M(num,num))
        do ix = 1, num
            print * , ix
            read(100,*,iostat=stat) M(ix,:)
        end do
        close(100)
    else
        allocate(M(num,num))
        call InitMat(num,M)             
    end if

    open(unit=100,file=outfile,status='old',iostat=stat)
    write(100,*) num
    call PrintMat(num,M, 100)
    close(100)

    do i = 1, stepsMC
        call SpinFlip(num, M, beta, JJ)
        if ( MOD(i,1000)== 0 ) then
            Energia = CalcEnergy(num, M, JJ) 
    
        end if
    end do

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
![No TOCAR]
! Escribir la última semilla para continuar con la cadena de numeros aleatorios 

        open(unit=10,file='seed.dat',status='unknown')
        seed = shr3() 
        write(10,*) seed
        close(10)
![FIN no Tocar]        


end program ising