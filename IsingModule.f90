MODULE IsingModule
    use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stdin=>input_unit, stderr=>error_unit
    use ziggurat
    IMPLICIT NONE
 
    ! PRIVATE
 
    ! INTEGER,  PARAMETER  ::  DP=SELECTED_REAL_KIND( 12, 60 )
    ! REAL(DP), PARAMETER  ::  m1=2147483648.0_DP,   m2=2147483648.0_DP,      &
    !                          half=0.5_DP
    ! REAL(DP)             ::  dn=3.442619855899_DP, tn=3.442619855899_DP,    &
    !                          vn=0.00991256303526217_DP,                     &
    !                          q,                    de=7.697117470131487_DP, &
    !                          te=7.697117470131487_DP,                       &
    !                          ve=0.003949659822581572_DP
    ! INTEGER,  SAVE       ::  iz, jz, jsr=123456789, kn(0:127),              &
    !                          ke(0:255), hz
    ! REAL(DP), SAVE       ::  wn(0:127), fn(0:127), we(0:255), fe(0:255)
    ! LOGICAL,  SAVE       ::  initialized=.FALSE.
 
    ! PUBLIC  :: zigset, shr3, uni, rnor, rexp

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
 SUBROUTINE SpinFlip( n, Mat, beta, JJ)
    INTEGER, INTENT(IN):: n
    INTEGER, INTENT(INOUT) :: Mat(n,n)
    real (kind=8), INTENT(IN) :: beta, JJ
    real (kind=8) :: E_nu
    INTEGER:: i, j

    i = n*uni()+1
    j = n*uni()+1

    Mat(i,j) = -Mat(i,j)

    if ( .NOT.(Aceptacion (CalcDiffEnergy(n, Mat, i, j, JJ), beta)) ) then
        Mat(i,j) = -Mat(i,j)
    end if 
    
 END SUBROUTINE SpinFlip
 
 FUNCTION CalcEnergy(n, Mat, JJ) RESULT( Esist )

    INTEGER, INTENT(IN):: n, Mat(n,n)
    real (kind=8) , INTENT(IN):: JJ
    real (kind=8) :: Esist
    INTEGER:: i,j

    do i = 1, n
        do j =1, n
            Esist = -Mat(i,j)*( &
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

    dE = -Mat(i,j)*( &
            Mat(i,Indice(n,j,1))+ &
            Mat(i,Indice(n,j,-1))+ &
            Mat(Indice(n,i,1),j)+ &
            Mat(Indice(n,i,-1),j))

    RETURN
 END FUNCTION CalcDiffEnergy

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

FUNCTION CalcInitMagnet ( M, N ) RESULT (mm)

	INTEGER, INTENT(IN)   :: N
	INTEGER, INTENT(IN)	  :: M(N,N)
	INTEGER               :: mm
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