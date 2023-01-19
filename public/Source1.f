!
!
!                             Online Fortran Compiler.
!                 Code, Compile, Run and Debug Fortran program online.
! Write your code in this editor and press "Run" button to execute it.
!
!


       IMPLICIT NONE
       REAL:: ERROR, DELTA, X0, X1, FUNC, DFUNC, pH
       INTEGER:: I, N
       WRITE(*,*) 'GIVE THE VALUE OF ERROR'
       READ(*,*) ERROR
       WRITE(*,*)'GIVE THE VALUE OF DELTA'
       READ(*,*)DELTA
       WRITE(*,*) 'GIVE THE VALUE OF ITERATIONS'
       READ(*,*) N
       WRITE(*,*) 'GIVE THE VALUE OF X0'
       READ(*,*) X0

       DO I=1, N
       IF ((ABS(DFUNC(X0))) .LE. DELTA) EXIT
       X1=X0 - (FUNC(X0)/DFUNC(X0))
       X0=X1

       WRITE(*,*) 'X0' , X0
       END DO
       pH= -LOG10(X1)
       WRITE(*,*) 'X1=' , X1
       WRITE(*,*) 'I=' , I
       WRITE(*,*) 'pH=' , pH
       PAUSE
       STOP
       END

       FUNCTION FUNC(X)
       REAL:: FUNC, KA, KW, C
       REAL:: X
       KA=0.00001754
       KW=10**(-14)
       C=0.00001
       FUNC=(X**3) + (KA*X*X) - ((C*KA)+KW)*X -KW*KA

       END FUNCTION FUNC
       FUNCTION DFUNC(X)
       REAL::DFUNC, KA, KW, C
       REAL:: X

       KA=0.00001754
       KW=10**(-14)
       C=0.00001
       DFUNC=(3*X*X)+(2*KA*X)-((C*KA)+KW)

       END FUNCTION DFUNC

