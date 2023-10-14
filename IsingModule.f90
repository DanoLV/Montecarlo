MODULE IsingModule
    use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stdin=>input_unit, stderr=>error_unit
    use ziggurat
    IMPLICIT NONE

 CONTAINS

! Inicializar la matriz
 SUBROUTINE InitMat( n, Mat )
    INTEGER, INTENT(IN)  :: n
    INTEGER :: Mat(n,n)
    INTEGER:: i,j

    do i = 1, n
        do j =1, n
            if ( uni() >0.5 ) then
                Mat(i,j) = 1
            else
                Mat(i,j) = -1
            end if
             
        end do 
    end do
   
 END SUBROUTINE InitMat
 
! Imprimir en pantalla
 SUBROUTINE PrintMat( n, Mat , out)
    INTEGER, INTENT(IN):: n, Mat(n,n), out
    INTEGER:: j

    do j = 1, n
        ! print *,Mat(j,:)
        write(out,*) Mat(j,:)
    end do 
   
 END SUBROUTINE PrintMat
 
! Imprimir en pantalla
 SUBROUTINE SpinFlip( n, Mat, beta, JJ, E, Mag, dE, actept)
    INTEGER, INTENT(IN):: n
    INTEGER, INTENT(INOUT) :: Mat(n,n)
    real (kind=8), INTENT(IN) :: beta, JJ
    real (kind=8), INTENT(INOUT) :: E, Mag
    real (kind=8), INTENT(INOUT) :: dE
    LOGICAL, INTENT(OUT) :: actept
    INTEGER:: i, j
    real (kind=8) :: dMag
    
    !Selecciono indices al azar entre 1 y n
    i = n*uni()+1
    j = n*uni()+1

    !Calculo la diferencia de energia
    dE = CalcDiffEnergy(n, Mat, i, j, JJ)

    !Acepto o no el cambio y hago operaciones pertinentes
    actept = Aceptacion (dE, beta)
    if ( actept ) then
        !Calculo nueva energia
        E = E + dE
        !invierto el spin
        Mat(i,j) = -Mat(i,j)
        !Calculo nueva marnetizacion
        dMag = CalcDiffMag(n, Mat, i, j, JJ)
        Mag = Mag + dMag
    end if 
    
 END SUBROUTINE SpinFlip
 
 FUNCTION CalcEnergy(n, Mat, JJ) RESULT( Esist )

    INTEGER, INTENT(IN):: n, Mat(n,n)
    real (kind=8) , INTENT(IN):: JJ
    real (kind=8) :: Esist
    INTEGER:: i,j

    do i = 1, n
        do j =1, n
            Esist = -0.5*Mat(i,j)*( &
                Mat(i,Indice(n,j,1))+ &
                Mat(i,Indice(n,j,-1))+ &
                Mat(Indice(n,i,1),j)+ &
                Mat(Indice(n,i,-1),j))+ &
                Esist
        end do 
    end do

    RETURN
 END FUNCTION CalcEnergy
 
 FUNCTION CalcDiffEnergy(n, Mat, i,j,JJ) RESULT( dE )

    INTEGER, INTENT(IN):: n, Mat(n,n), i, j
    real (kind=8) , INTENT(IN):: JJ
    real (kind=8) :: dE

    dE = 2*JJ*Mat(i,j)*( &
                Mat(i,Indice(n,j,1))+ &
                Mat(i,Indice(n,j,-1))+ &
                Mat(Indice(n,i,1),j)+ &
                Mat(Indice(n,i,-1),j))

    RETURN

 END FUNCTION CalcDiffEnergy

 FUNCTION CalcDiffMag(n, Mat, i,j,JJ) RESULT( dM )

    INTEGER, INTENT(IN):: n, Mat(n,n), i, j
    real (kind=8) , INTENT(IN):: JJ
    real (kind=8) :: dM

    dM = - Mat(i,j)*( &
            Mat(i,Indice(n,j,1))+ &
            Mat(i,Indice(n,j,-1))+ &
            Mat(Indice(n,i,1),j)+ &
            Mat(Indice(n,i,-1),j))

    RETURN
 END FUNCTION CalcDiffMag

 FUNCTION Indice (n,i,j) RESULT( k )
    INTEGER :: n,i,j,k

    k = i+j
    if ( k < 1 ) then
        k = n
    else if ( k > n ) then
        k = 1
    end if 

 END FUNCTION Indice

 !Acceptance
FUNCTION Aceptacion ( dE, beta) RESULT (acc)

	REAL (kind=8), INTENT(IN) :: dE, beta
	LOGICAL :: acc
	
	IF (dE < 0.0) THEN
		acc = .TRUE.
	ELSE
		IF (uni()<exp(-beta*dE)) then
			acc = .TRUE.
		ELSE
			acc = .FALSE.
        END IF
	END IF
	
RETURN
END FUNCTION Aceptacion

FUNCTION CalcMagnet (N, M) RESULT (mm)

	INTEGER, INTENT(IN)   :: N
	INTEGER, INTENT(IN)	  :: M(N,N)
	REAL(kind=8)          :: mm
	INTEGER				  :: i, j

	mm = 0

	DO j = 1, N
		DO i = 1, N
			mm = mm + M(i,j)
		END DO
	END DO

RETURN
END FUNCTION

END MODULE IsingModule