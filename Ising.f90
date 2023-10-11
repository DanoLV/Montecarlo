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
    integer :: seed,i ,j,k,vec(10), num, stepsMC
    real (kind=8) :: x,y,r(10),rr(3,10), Energia, beta, JJ
    INTEGER , allocatable :: M(:,:)
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

    beta = 0.01
    JJ = 1
    num = 20
    stepsMC = 1000000
    allocate(M(num,num))
    call InitMat(num,M)

    do i = 1, stepsMC
        call SpinFlip(num, M, beta, JJ)
        if ( MOD(i,1000)== 0 ) then
            ! call PrintMat(num,M)
            Energia = CalcEnergy(num, M, JJ) 
            print *, i,";",Energia        
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