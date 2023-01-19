       IMPLICIT NONE
       REAL A, B, Xn, R, T, V0, H
       REAL P(100000),V(100000), dPdV(100000), dPdV_EXACT(100000)
       INTEGER I

       OPEN (2, FILE = 'DIFF_VdW.TXT', STATUS = 'UNKNOWN')      !OUTPUT OPENED

       R=0.082
       A=3.64
       B=0.04267
       Xn=1.0
       T=273.0

       WRITE (*,*) "GIVE THE VALUE OF INITIAL VOLUME"
       READ (*,*) V0
       WRITE (*,*) "GIVE THE DEL V"
       READ (*,*) H
       WRITE(2,*)"GIVEN INITIAL VOLUME", V0
       WRITE(2,*) "DEL V", H

       WRITE (2,*)"          V          P     dPdV(NUM)   dPdV(EXT)"
       WRITE (2,*)"_______________________"

       DO I = 1, 50
       V(I) = V0 + (I-1) * H            !CALCULATING EACH INCREMENT IN VOLUME
       P(I) = (Xn*R*T)/(V(I)-(Xn*B)) - ((A*Xn**2)/(V(I)**2))
       END DO
C       CALCULATING THE DERIVATIVE
           DO I = 2, 50-1
C       dPdV(I) STORES THE FIRST DERIVATIVE WITH CENTRAL DIFFERENCE FORMULA
       dPdV(I) = (P(I+1) - P(I-1))/(2*H)
C      dPdV_EXACT(I) STORES THE FIRST DERIVATIVE WITH ANALYTICAL FORMULA
       dPdV_EXACT(I) = (-(Xn*R*T)/(V(I)- Xn*B)**2) +2*A*Xn**2/V(I)**3
       WRITE (2,*) V(I), P(I), dPdV(I), dPdV_EXACT(I)
         END DO
         
       PAUSE
       STOP
       END


