C      USING TRAPIZOIDAL RULE: DISTRIBUTION AND MEAN VALUES FOR CO2
C      THE PROGRAM FOR 3D DISTRIBUTION ; USING TRAPIZOIDAL RULE

       PI=ATAN(1.0)*4.0
       AM=44.0
       R=8.314
       WRITE(*,*)"GIVE N,CL,CH,T"
       READ(*,*)N,CL,CH,T
       H=(CH-CL)/N
       FAC=AM/(2.0*PI*R*T)
       FAC=4.0*PI*FAC**1.5
       CALL FCAL(AM,T,CL,FC,FC1,FC2)
       Y00=FC
       Y02=FC2
       CALL FCAL(AM,T,CH,FC,FC1,FC2)
       YN0=FC
       YN1=FC1
       YN2=FC2
       SUM=0.0
       SUM=0.0
       SUM=0.0
       DO 10 I=1,N-1
       C=CL+I*H
       CALL FCAL(AM,T,C,FC,FC1,FC2)
       SUM=SUM+FC
       SUM=SUM1+FC1
       SUM2=SUM2+FC2
  10    CONTINUE
       SUM2=2.0*SUM
       SUM1=2.0*SUM1
       SUM2=2.0*SUM2
       
       SUM=Y00+SUM+YN0
       SUM=SUM*FAC*H/2.0
        SUM1=Y01+SUM1+YN1
        SUM1=SUM1*H*FAC/2.0
        SUM2=Y02+SUM2+YN2
        SUM2=SUM2*H*FAC/2.0
        WRITE(*,*)"INTIGRALS 1 2 3"
        WRITE(*,*)
        WRITE(*,*)SUM,SUM1,SUM2
        X=(8.0*R*T)/(PI*AM)
        CAV=X**0.5
        CAV=3.0*R*T/AM
        WRITE(*,*)"PREDICTED VALUES ARE"
        WRITE(*,*)1.0,CAV,CAV2
        PAUSE
        STOP
        END
        SUBROUTINE FCAL(AM,T,C,FC,FC1,FC2)
        R=8.314
        FF=(AM*C*C)/(2.0*R*T)
        FC=EXP(-FF)
        FC=FC*C*C
        FC1=C*FC
        FC2=C*C*FC
        RETURN
        END
        
        
        
        
        
        
        
        
       
       
       
       
       
       
       
       

