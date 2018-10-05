C*   NORTHROP GRUMMAN PROPRIETARY
C  THIS IS  "F32\FORT\FORT6PC\ABMISC.FOR"
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES THERMAL CONDUCTIVITY FOR LIQUIDS AND              
C      GAS                                                                      
C                                                                               
C**********************************************************************         
C                                                                               
                                                                                
      FUNCTION COND (NL,P,T)                                                    
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFB,C(55)), (TL,C(81))                                       
     *, (BIG,C(31))                                                             
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
                                                                                
      LOC = ID(IFB+NL)                                                          
      IFT = ID(LOC+1)                                                           
      IN = ID(LOC+3)                                                            
      TA = T                                                                    
      IF (TA.LT.TL) TA = TL                                                     
      IF (IFT-2) 1,2,3                                                          
    1 CONTINUE                                                                  
      COND = POLYI(2,D(IN+10),TA)                                               
      GO TO 99                                                                  
    2 CONTINUE                                                                  
      COND = POLYI(5,D(IN+14),TA)                                               
      GO TO 99                                                                  
    3 CONTINUE                                                                  
      COND = BIG                                                                
   99 CONTINUE                                                                  
      RETURN                                                                    
C     COND                                                                      
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES DENSITY FOR LIQUIDS AND GASES                     
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION DEN (NL,P,T)                                                     
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFB,C(55)), (PL,C(80)), (TL,C(81))                           
     *, (BIG,C(31))                                                             
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      LOC = ID(IFB+NL)                                                          
      IFT = ID(LOC+1)                                                           
      IN = ID(LOC+3)                                                            
      TA = T                                                                    
      IF (TA.LT.TL) TA = TL                                                     
      IF (IFT-2) 1,2,3                                                          
    1 CONTINUE                                                                  
      DEN = POLYI(2,D(IN+1),TA)                                                 
      GO TO 99                                                                  
    2 CONTINUE                                                                  
      PA = P                                                                    
      IF (PA.LT.PL) PA = PL                                                     
      DEN = PA/(D(IN+1)*TA)                                                     
      GO TO 99                                                                  
    3 CONTINUE                                                                  
      DEN = BIG                                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     DEN                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES TABLE INTERPOLATION POLYNOMIAL                    
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION FLAGRA(XA,X,Y,N)                                                 
      DIMENSION X(1), Y(1)                                                      
                                                                                
      SUM=0.0                                                                   
                                                                                
      DO 2 I=1,N                                                                
         PROD=Y(I)                                                              
         DO 1 J=1,N                                                             
            A=X(I)-X(J)                                                         
            IF (A.NE.0.0) THEN                                                  
               PROD=PROD*(XA-X(J))/A                                            
            ENDIF                                                               
                                                                                
 1       CONTINUE                                                               
                                                                                
         SUM=SUM+PROD                                                           
 2    CONTINUE                                                                  
                                                                                
      FLAGRA=SUM                                                                
      RETURN
C   FLAGRA                                                                    
      END
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE LOCATES FLUID PROPERTY TABLE AND COMPUTES                  
C      STANDARD DENSITY                                                         
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE FLUIDP (NL)                                                    
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFB,C(55)), (P0,C(64)), (T0,C(65))                           
     *, (ISP,C(42)), (BIG,C(31))                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      LOC = ID(IFB+NL)                                                          
      IF (NL.EQ.0 .AND. LOC.EQ.0) GO TO 99                                      
      IF (ID(LOC+4).NE.0) GO TO 99                                              
      IFT = ID(LOC+1)                                                           
      IN = ID(LOC+3)                                                            
      IN = ITLUP(IN)                                                            
      ID(LOC+3) = IN                                                            
      IF (IFT-2) 1,2,3                                                          
    1 CONTINUE                                                                  
      D(LOC+4) = BIG                                                            
      GO TO 99                                                                  
    2 CONTINUE                                                                  
      D(LOC+4) = DEN(NL,P0,T0)                                                  
      GO TO 99                                                                  
    3 CONTINUE                                                                  
      D(LOC+4) = BIG                                                            
   99 CONTINUE                                                                  
      RETURN                                                                    
C     FLUIDP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE  DEFINES FLUID (EXCEPT FREESTREAM) FOR                     
C      PERFORMANCE CASE                                                         
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE FLUIDZ (NL,IFT,IRN)                                            
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (NLEG,C(25)), (CERR,C(16)), (OUT,C(7)), (IFB,C(55))           
      INTEGER CERR, OUT                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      J = 1                                                                     
   10 IF (IFB.NE.0) GO TO 11                                                    
      CALL GDCU(NLEG+1,4,0,D,IFB)                                               
      IFB = IFB+1                                                               
   11 IERR = 0                                                                  
      GO TO (1,4), J                                                            
    1 IF (NL.NE.0) GO TO 2                                                      
      IERR = IERR+1                                                             
      GO TO 3                                                                   
    2 IF (ID(IFB+NL).EQ.0) GO TO 3                                              
      IERR = IERR+1                                                             
    3 IF (IFT.GE.1.AND.IFT.LE.3) GO TO 4                                        
      IERR = IERR+1                                                             
      CERR = CERR+1                                                             
      WRITE (OUT,1001) CERR,IFT                                                 
 1001 FORMAT(6H0ERROR,I6,5X,18HINVALID FLUID TYPE,I6)                           
    4 IF (IRN.NE.0) GO TO 5                                                     
      IERR = IERR+1                                                             
    5 INPT = ITIDN(IRN,10)                                                      
      IF (IERR.NE.0) GO TO 99                                                   
      II = 0                                                                    
      DO 6 I=II,NLEG                                                            
      LOC = ID(IFB+I)                                                           
      IF (LOC.EQ.0) GO TO 6                                                     
      IFTI = ID(LOC+1)                                                          
      IF (IFTI.NE.IFT) GO TO 6                                                  
      INPTI = ID(LOC+3)                                                         
      IF (INPTI.EQ.INPT) GO TO 7                                                
    6 CONTINUE                                                                  
      CALL GDCU(4,4,0,D,LOC)                                                    
      ID(LOC+1) = IFT                                                           
      ID(LOC+2) = IRN                                                           
      ID(LOC+3) = INPT                                                          
    7 CONTINUE                                                                  
      ID(IFB+NL) = LOC                                                          
   99 CONTINUE                                                                  
      RETURN                                                                    
      ENTRY FLDZF (NL,IFT,IRN)                                                  
      J = 2                                                                     
      GO TO 10                                                                  
      ENTRY FLUIDS (NL,IFT,IRN)                                                 
      J = 3                                                                     
      LOC = ID(IFB+NL)                                                          
      ID(LOC+3) = ITIDN(IRN,10)                                                 
      ID(LOC+4) = 0                                                             
      GO TO 99                                                                  
C     FLUIDZ, FLDZF, FLUIDS                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE FLUPPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (PL,C(80)), (TL,C(81)), (BIG,C(31))             
     *, (IFB,C(55)), (CPV,C(132)), (OUT,C(7)), (PASS,C(17))                     
     *, (SCR(1),C(151))                                                         
      INTEGER PASS, OUT                                                         
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (SCR(1),NL), (SCR(2),PJK), (SCR(3),TJK)                       
     *, (SCR(4),DP), (SCR(5),LOCT), (SCR(6),DT), (SCR(7),DSH)                   
     *, (SCR(8),NC)                                                             
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 P                                                                         
C   4 DP                                                                        
C   5 N                                                                         
C   6 T                                                                         
C   7 DT                                                                        
C   8 M                                                                         
C   9 HUM                                                                       
C  10 DSH                                                                       
      IRCD = IRCDB(10)                                                          
      NL = ID(IRCD+2)                                                           
      IF (PASS.EQ.1) CALL FLUIDP(NL)                                            
C$ ONLY WRITE ONE TIME                                                          
      IF (PASS.NE.1) GO TO 99                                                   
      WRITE(OUT,0555)                                                           
  555 FORMAT(//,1X,' WRITE FROM FLUPP COMPONENT')                               
      LOCT = ID(IFB+NL)                                                         
      NC = ID(LOCT+1)                                                           
      LOCT = ID(LOCT+3)                                                         
      PJK = D(ID(IRCD+3))                                                       
      DP = D(ID(IRCD+4))                                                        
      N = D(ID(IRCD+5))                                                         
      TJK = D(ID(IRCD+6))                                                       
      DT = D(ID(IRCD+7))                                                        
      M = D(ID(IRCD+8))                                                         
      IF((N.GT.999).OR.(M.GT.999)) GO TO 99                                     
      HUM = D(ID(IRCD+9))                                                       
      DO 20 K = 1,N                                                             
      DO 40 J =1,M                                                              
      DENJK = DEN(NL,PJK,TJK)                                                   
      HJK = HFT(NL,PJK,TJK)
      TJKFH = TFH (NL,PJK,HJK)                                                     
C$   SHP, COND, VIS, DEN, HFT   RETURN "BIG"(C31) FOR A REFRIGERANT             
      CPJK = SHP(NL,PJK,TJK,HUM)                                                
      CDJK = COND(NL,PJK,TJK)                                                   
      VISJK = VIS(NL,PJK,TJK)                                                   
      IF(NC.NE.2) GO TO 50                                                      
      GJK = GAM(NL,PJK,TJK)                                                     
      CJK = SOS(NL,PJK,TJK)                                                     
      WRITE(OUT,1001) PJK,TJK,HUM,DENJK,HJK,CPJK,CDJK,VISJK,GJK,CJK             
 1001 FORMAT(1H0,2X,1HP,F6.2,2X,1HT,F8.2,2X,3HHUM,F7.3,2X,3HDEN,F8.5,2X         
     *,1HH,F8.3/2X,2HCP,F7.4,2X,4HCOND,F8.5,2X,2HMU,E12.4,2X,3HGAM              
     *,F7.4,2X,1HC,F8.2)                                                        
      GO TO 60                                                                  
C$   CHANGE FORMAT                                                              
   50 WRITE(OUT,1002) PJK,TJK,HJK,DENJK,CPJK,CDJK,VISJK, TJKFH                         
 1002 FORMAT(5H **P=,F12.2,5H   T=,F12.2,5H   H=,F12.3                          
     *,5H DEN=,F12.4,11H   ******** /  , 7H    CP=,F12.4,4H  K=,F12.5           
     *,6H   MU=,F12.8, 14H   T=F(HCALC)=,F12.3)                                                          
   60 TJK = TJK+DT                                                              
   40 CONTINUE                                                                  
      PJK = PJK+DP                                                              
      TJK = D(ID(IRCD+6))                                                       
   20 CONTINUE                                                                  
      GO TO 99                                                                  
   99 CONTINUE                                                                  
      RETURN                                                                    
C     FLUPPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE FLUPPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151)),(OUT,C(7))                   
      REAL OUT                                                                  
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL), (SCR(2),NS), (SCR(3),IOP)                        
     *, (SCR(4),NC)                                                             
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 P                                                                         
C   4 DP                                                                        
C   5 N                                                                         
C   6 T                                                                         
C   7 M                                                                         
C   8 DT                                                                        
C   9 HUM                                                                       
C  10 DSH                                                                       
      I = IACDB(10)                                                             
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      ID(I+3) = IPARM(ICV(3))                                                   
      ID(I+4) = IPARM(ICV(4))                                                   
      ID(I+5) = IPARM(ICV(5))                                                   
      ID(I+6) = IPARM(ICV(6))                                                   
      ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      CALL LEGRS(NL)                                                            
      CALL FLUIDZ(NL,ICV(10),ICV(11))                                           
      CALL FTR(NL,NC)                                                           
   99 CONTINUE                                                                  
      RETURN                                                                    
C     FLUPPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE SETS FLUID BLOCK LOCATION                                  
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE FRS (NL,NC)                                                    
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (NLEG,C(25)), (CERR,C(16)), (OUT,C(7)), (IFB,C(55))           
      INTEGER CERR, OUT, NL, NC                 ! ADD NL, NC SFW JUN '03                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      I = 1                                                                     
      IF (IFB.EQ.0) GO TO 98                                                    
    1 IF (NL.EQ.0) GO TO 99                                                     
      IF (ID(IFB+NL).NE.0) GO TO 99                                             
      ID(IFB+NL) = NC                                                           
      GO TO 99                                                                  
      ENTRY FRR (NL,NC)                                                         
      I = 2                                                                     
      IF (IFB.EQ.0) GO TO 98                                                    
    2 IF (NL.EQ.0) GO TO 20                                                     
      NC = ID(IFB+NL)                                                           
      GO TO 99                                                                  
   20 NC = 0                                                                    
      GO TO 99                                                                  
      ENTRY FTR (NL,NC)                                                         
      I = 3                                                                     
      IF (IFB.EQ.0) GO TO 98                                                    
    3 IF (NL.EQ.0) GO TO 30                                                     
      NC = ID(IFB+NL)                                                           
      NC = ID(NC+1)                                                             
      GO TO 99                                                                  
   30 NC = 0                                                                    
      GO TO 99                                                                  
      ENTRY FTL (NL,NC)                                                         
      I = 4                                                                     
      IF (IFB.EQ.0) GO TO 98                                                    
    4 IF (NL.EQ.0) GO TO 99                                                     
      J = ID(IFB+NL)                                                            
      IF (J.EQ.0) GO TO 99                                                      
      J = ID(J+1)                                                               
      IF (J-2) 41,42,43                                                         
   41 J = NC-10*(NC/10)                                                         
      IF (J.NE.0) GO TO 99                                                      
      GO TO 40                                                                  
   42 J = 10*(NC/10)                                                            
      J = J-100*(J/100)                                                         
      IF (J.NE.0) GO TO 99                                                      
      GO TO 40                                                                  
   43 J = 100*(NC/100)                                                          
      J = J-1000*(J/1000)                                                       
      IF (J.NE.0) GO TO 99                                                      
   40 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1001) CERR                                                     
 1001 FORMAT(6H0ERROR,I6,5X,18HINVALID FLUID TYPE)                              
      GO TO 99                                                                  
   98 CONTINUE                                                                  
      CALL GDCU(NLEG+1,4,0,D,IFB)                                               
      IFB = IFB+1                                                               
      GO TO (1,2,3,4), I                                                        
   99 CONTINUE                                                                  
      RETURN                                                                    
C     FRS, FRR, FTR, FTL                                                        
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE FUNCPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (SCR(1),C(151)), (IPAR,C(33)), (IGA,C(35)), (IFP,C(22))                 
     *, (ICPP,C(88)), (IIOP,C(90)), (OUT,C(7)), (PASS,C(17))                    
     *, (IH,C(30)), (P0,C(64)), (T0,C(65)), (IFB,C(55))                         
      INTEGER OUT,PASS                                                          
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (SCR(1),IRT), (SCR(2),IRN), (SCR(3),IOPT)                     
     *, (SCR(4),IFNC), (SCR(5),NL), (SCR(6),NS), (SCR(7),IAP,A)                 
     *, (SCR(8),ICDT,CD), (SCR(30),I), (SCR(29),J), (SCR(28),VX)                
     *, (SCR(27),PR), (SCR(26),GMA), (SCR(25),PRES), (SCR(24),TEMP)             
     *, (SCR(23),IFL), (SCR(24),TEMP0)                                          
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 IRT                                                                       
C   3 IRN                                                                       
C   4 IOPT                                                                      
C   5 IFNC                                                                      
C   6 NL                                                                        
C   7 NS                                                                        
C   8 CROSS SECTIONAL AREA OR PRESSURE                                          
C   9 FLOW COEFFICIENT OR TERPERATURE                                           
      IRCD = IRCDB(9)                                                           
      IF (IOPT.EQ.1.AND.PASS.NE.1 .OR.IOPT.EQ.-1.AND.IIOP.NE.0)                 
     * GO TO 99                                                                 
      IF (IOPT.EQ.2 .AND. IFP.EQ.0) GO TO 99                                    
   21 I = IFNC - 10                                                             
      GO TO (1,2,2,2,5), I                                                      
    1 VX = D(IW+NL)*SQRT(D(IT+NS)/T0)/(D(IP+NS)/P0)                             
      GO TO 90                                                                  
    2 A = D(IAP)                                                                
      IF (IFNC.NE.13 .AND. A.EQ.0.0) A = 1.0                                    
      CD = D(ICDT)                                                              
      IF (CD .EQ. 0.0) CD = 1.0                                                 
      IF (NS .NE. 0) GO TO 3                                                    
      PRES = D(IGA+7)                                                           
      TEMP = D(IGA+8)                                                           
      GO TO 14                                                                  
    3 PRES = D(IP+NS)                                                           
      TEMP = D(IT+NS)                                                           
   14 IF (I-3) 12,13,13                                                         
   12 VX = D(IW+NL)*SQRT(TEMP)/(CD*A*PRES)                                      
      GO TO 90                                                                  
   13 GMA = GAM(NL,PRES,TEMP)                                                   
      PR = (2.0/(GMA+1.0))**(GMA/(GMA-1.0))                                     
      VX = 40.124*CD*A*SQRT(GMA*DEN(NL,PRES,TEMP)*PRES*                         
     * (PR**(2.0/GMA) - PR**((GMA+1.0)/GMA))/(GMA-1.0))                         
      IF (IFNC .EQ. 13) GO TO 90                                                
      VX = D(IW+NL)/VX                                                          
      GO TO 90                                                                  
    5 VX = 2.4*D(IW+NL)/(DEN(0,D(IGA+5),D(IGA+6))*D(IAP)*D(IGA+4))              
      GO TO 90                                                                  
   90 IF (IRT .EQ. 0) GO TO 91                                                  
      D(IPAR+IRN) = VX                                                          
      GO TO 92                                                                  
   91 D(IGA+IRN) = VX                                                           
   92 D(IRCD-6) = VX                                                            
      IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      CALL PIOP(0,0,0,0)                                                        
      CALL LINES(2)                                                             
      WRITE(OUT,1000) VX,IRT,IRN,IFNC,IOPT                                      
 1000 FORMAT(1H0,5X,2HVX,1PE15.6,3X,3HIRT,I7,3X,3HIRN,I7,3X,                    
     *  4HIFNC,I6,3X,4HIOPT,I6)                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     FUNCPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE FUNCPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151)), (CERR,C(16))                
     *, (OUT,C(7)), (NPV,C(32)), (NGA,C(36)), (IFB,C(55))                       
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(30),NL), (SCR(29),NS), (SCR(28),LOC)                     
      INTEGER CERR,OUT                                                          
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 IRT                                                                       
C   3 IRN                                                                       
C   4 IOPT                                                                      
C   5 IFNC                                                                      
C   6 NL                                                                        
C   7 NS                                                                        
C   8 CROSS SECTIONAL AREA OR PRESSURE                                          
C   9 FLOW COEFFICIENT OR TERPERATURE                                           
      I =  IACDB(9)                                                             
      ID(I+1) = ICV(1)                                                          
      ID(I+2) = ICV(2)                                                          
      ID(I+3) = ICV(3)                                                          
      IF (ICV(2) .EQ. 0) GO TO 10                                               
      IF (ICV(2) .EQ. -1) GO TO 11                                              
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE(OUT,1000) CERR                                                      
 1000 FORMAT(6H0ERROR,I6,5X,11HIRT INVALID)                                     
      GO TO 12                                                                  
   10 IF (ICV(3).GT.100 .AND. ICV(3).LE.NGA) GO TO 12                           
      GO TO 13                                                                  
   11 IF (ICV(3).GT.0 .AND. ICV(3).LE.NPV) GO TO 12                             
   13 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE(OUT,1001) CERR                                                      
 1001 FORMAT(6H0ERROR,I6,5X,11HIRN INVALID)                                     
   12 ID(I+4) = ICV(4)                                                          
      IF (ICV(4).GE.-1 .AND. ICV(4).LE.2) GO TO 15                              
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE(OUT,1002) CERR                                                      
 1002 FORMAT(6H0ERROR,I6,5X,12HIOPT INVALID)                                    
   15 ID(I+5) = ICV(5)                                                          
      IF (ICV(5).GE.11 .AND. ICV(5).LE.15) GO TO 20                             
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE(OUT,1003) CERR                                                      
 1003 FORMAT(6H0ERROR,I6,5X,12HIFNC INVALID)                                    
      GO TO 99                                                                  
   20 NL = ICV(6)                                                               
      IF (ICV(5).EQ.13 .AND. ICV(6).EQ.0) GO TO 22                              
      NL = ILEGN(ICV(6))                                                        
      CALL LEGRT(NL)                                                            
      ID(I+6) = NL                                                              
   22 NS = ICV(7)                                                               
      IF (ICV(5).GE.12 .AND. ICV(7).EQ.0) GO TO 24                              
      NS = ISTAN(ICV(7))                                                        
      CALL START(NS)                                                            
   23 ID(I+7) = NS                                                              
   24 ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      LOC = ID(IFB+NL)                                                          
      IF (ID(LOC+1) .EQ. 2) GO TO 99                                            
   30 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE(OUT,1004) CERR                                                      
 1004 FORMAT(6H0ERROR,I6,5X,18HFLUID TYPE INVALID)                              
   99 CONTINUE                                                                  
      RETURN                                                                    
C     FUNCPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES ISENTORPIC EXPONENT FOR GASSES                    
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION GAM (NL,P,T)                                                     
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFB,C(55)), (TL,C(81))                                       
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      LOC = ID(IFB+NL)                                                          
      IN = ID(LOC+3)                                                            
      TA = T                                                                    
      IF (TA.LT.TL) TA = TL                                                     
      GAM = POLYI(5,D(IN+20),TA)                                                
      RETURN                                                                    
C     GAM                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION HFT (NL,P,T)                                                     
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFB,C(55)), (TL,C(81)), (BIG,C(31))                          
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      LOC = ID(IFB+NL)                                                          
      IFT = ID(LOC+1)                                                           
      IN = ID(LOC+3)                                                            
      TA = T                                                                    
      IF (TA.LT.TL) TA = TL                                                     
      IF (IFT-2) 1,2,3                                                          
    1 CONTINUE                                                                  
      HFT = POLYI(3,D(IN+13),TA)                                                
      GO TO 99                                                                  
    2 CONTINUE                                                                  
      HFT = POLYI(5,D(IN+26),TA)                                                
      GO TO 99                                                                  
    3 CONTINUE                                                                  
      HFT = BIG                                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     HFT                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE OUTPUTS CONDENSATION MESSAGE FOR PORFORMANCE               
C      PRINTOUT                                                                 
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE HSOP (HS)                                                      
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7))                                                    
      INTEGER OUT                                                               
      CALL LINES(2)                                                             
      WRITE (OUT,1011) HS                                                       
 1011 FORMAT(1H0,5X,20H***CONDENSATION - HS,F8.5)                               
      RETURN                                                                    
C     HSOP                                                                      
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE OUTPUTS INGERGER NUMBERS WITH INDEXES IN                   
C       PARENTHESES                                                             
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE IPAREN (NUM,IN,IVAL)                                           
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7))                                                    
      INTEGER OUT                                                               
      DIMENSION CD(200)                                                         
      DIMENSION IN(1), IVAL(1)                                                  
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (BLANKS,CD(23))                                               
      real*8 cd, blanks 
      DO 1 I=1,NUM,10                                                           
      J = I+9                                                                   
      IF (J.GT.NUM) J = NUM                                                     
      CALL LINES(1)                                                             
    1 WRITE (OUT,1000) (BLANKS,IN(K),IVAL(K),K=I,J)                             
 1000 FORMAT(10(A1,1H(,I4,1H),I5))                                              
      RETURN                                                                    
C     IPAREN                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE RETURNS PARAMETER TABLE LOCATION                           
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION IPARM (N)                                                        
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,  C(7)),                                                 
     *            (CERR, C(16)),                                                
     *            (BIG,  C(31)),                                                
     *            (NPV,  C(32)),                                                
     *            (IPAR, C(33))                                                 
      INTEGER CERR, OUT                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
                                                                                
      IF (N.LT.0 .OR. N.GT.NPV) THEN                                            
         CERR = CERR+1                                                          
         CALL LINES(2)                                                          
         WRITE (OUT,1000) CERR,N                                                
 1000    FORMAT(6H0ERROR,I6,5X,23HINVALID PARAMETER INDEX,I6)                   
         IPARM = -1                                                             
         RETURN                                                                 
      ENDIF                                                                     
                                                                                
      IF (N.EQ.0) THEN                                                          
         IPARM = 0                                                              
         RETURN                                                                 
      ENDIF                                                                     
                                                                                
      IPARM = IPAR+N                                                            
      VAL = D(IPARM)                                                            
      IF (VAL.EQ.BIG) THEN                                                      
         CERR = CERR+1                                                          
         CALL LINES(2)                                                          
         WRITE (OUT,1001) CERR,N                                                
 1001    FORMAT(6H0ERROR,I6,5X,25HPARAMETER NOT INITIALIZED,I6)                 
      ENDIF                                                                     
                                                                                
      RETURN                                                                    
C   IPARM
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE OUTPUTS INTGER NUMBERS WITH INDEXES IN                     
C      PARENTHESES                                                              
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE IPRNTH (NO,IVAL)                                               
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7))                                                    
      INTEGER OUT                                                               
      DIMENSION CD(200)                                                         
       REAL*8  CD                                                             
       REAL*8  BLANKS                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (BLANKS,CD(23))                                               
      DIMENSION IVAL(1)                                                         
      DO 1 I=1,NO,10                                                            
      J = I+9                                                                   
      IF (J.GT.NO) J = NO                                                       
      CALL LINES(1)                                                             
    1 WRITE (OUT,1000) (BLANKS,K,IVAL(K),K=I,J)                                 
 1000 FORMAT(10(A1,'(',I4,')',I5))                                              
      RETURN                                                                    
C     IPRNTH                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE RETURNS LOCATION OF COMPONENT DATA BLOCK                   
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION IRCDB (NUM)                                                      
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICDB,C(43)), (SCR(1),C(151))                                 
      DIMENSION SCR(30)                                                         
                                                                                
      COMMON /CCA/ CA(150)                                                      
      EQUIVALENCE (NOC,CA(1))                                                   
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
                                                                                
      LOC = ID(ICDB+NOC)                                                        
      DO 1 J=2,NUM                                                              
         SCR(J-1) = D(LOC+J)                                                    
    1 CONTINUE                                                                  
                                                                                
      IRCDB = LOC                                                               
      RETURN
C   IRCDB                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE ASSIGNS INTERNAL STATION NUMBER                            
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION ITIDN (N1,N2)                                                    
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7)), (CERR,C(16)), (ITN,C(39)), (NTU,C(40))            
     *, (MNTAB,C(41)), (ISP,C(42))                                              
      INTEGER OUT, CERR                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      IF (N1.NE.0 .AND. N2.GE.0 .AND. N2.LT.100) GO TO 5                        
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1001) CERR,N1,N2                                               
 1001 FORMAT(6H0ERROR,I6,5X,16HINVALID TABLE ID,2I6)                            
      K = 0                                                                     
      GO TO 99                                                                  
    5 IDT = 100*N1+ISIGN(N2,N1)                                                 
      IF (ITN.NE.0) GO TO 1                                                     
      CALL GDCU(MNTAB,4,ISP,D,ITN)                                              
      NTU = 0                                                                   
      GO TO 3                                                                   
    1 IF (NTU.EQ.0) GO TO 3                                                     
      DO 2 I=1,NTU                                                              
      K = I                                                                     
      IF (IDT.EQ.ID(ITN+I)) GO TO 99                                            
    2 CONTINUE                                                                  
      IF (NTU.LT.MNTAB) GO TO 3                                                 
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,N1,N2                                               
 1000 FORMAT(6H0ERROR,I6,5X,20HTOO MANY TABLES - ID,2I6)                        
      K = 0                                                                     
      GO TO 99                                                                  
    3 NTU = NTU+1                                                               
      ID(ITN+NTU) = IDT                                                         
      K = NTU                                                                   
   99 ITIDN  = K                                                                
      RETURN                                                                    
C     ITIDN                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE ARRAY TABLE LOCATIONS LOOKUP                               
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION ITLUP (N)                                                        
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ITAD,C(54)), (OUT,C(7)), (ITN,C(39))                         
     *, (CERR,C(16))                                                            
      INTEGER CERR, OUT                                                         
      DIMENSION CD(200)                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84))                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      REAL*8 cd, cn
      IAD = ID(ITAD+N)                                                          
      NDIM = ID(IAD+1)                                                          
      IF (NDIM.NE.-1) GO TO 101                                                 
      IVAL = ID(IAD+3)                                                          
      ITLUP = IVAL                                                              
      GO TO 99                                                                  
  101 IVAL = 0                                                                  
      CERR = CERR+1                                                             
      IDR = ID(ITN+N)/100                                                       
      IDT = ID(ITN+N)-IDR*100                                                   
      CALL LINES(2)                                                             
      WRITE (OUT,1002) CERR,CN,IDR,IDT                                          
 1002 FORMAT(6H0ERROR,I6,5X,10HCOMPONENT ,A6,14H INVALID TABLE,2I6)             
   99 CONTINUE                                                                  
      RETURN                                                                    
C     ITLUP                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE SETS LEG DEFINITION                                        
C                                                                               
C                                                                               
C$         S-TYPE LINES REPAIRED                                                
C$     GAM BELOW ADDED,      OTHER MODS ALSO                                    
C$ LEGRS DOES INITIAL LEG CHECKS                                                
C$ ADDED   IF (ID(ILR+N).EQ.1000) RETURN                                        
C**********************************************************************         
C                                                                               
      SUBROUTINE LEGRS (N)                                                      
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,  C(7)),                                                 
     *            (CERR, C(16)),                                                
     *            (ILR,  C(52)),                                                
     *            (ISR,  C(53)),                                                
     *            (NLEG, C(25)),                                                
     *            (NSTA, C(26)),                                                
     *            (ISP,  C(42)),                                                
     *            (ILN,  C(37)),                                                
     *            (ISN,  C(38))                                                 
      INTEGER OUT, CERR                                                         
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
                                                                                
      IF (N.EQ.0) RETURN                                                        
      I = 1                                                                     
      IF (ILR.EQ.0) GO TO 98                                                    
    1 ID(ILR+N) = ID(ILR+N)+1                                                   
      IF (ID(ILR+N).EQ.1) RETURN                                                
C$ LINE BELOW ADDED                                                             
      IF (ID(ILR+N).EQ.1000) RETURN                                             
      CERR = CERR+1                                                             
      NN = ID(ILN+N)                                                            
      CALL LINES(2)                                                             
      WRITE (OUT,1003) CERR,NN                                                  
 1003 FORMAT(6H0ERROR,I6,5X,23HMULTIPLE LEG DEFINITION,I6)                      
      RETURN                                                                    
                                                                                
      ENTRY STARS (N)                                                           
      IF (N.EQ.0) RETURN                                                        
      I = 2                                                                     
      IF (ISR.EQ.0) GO TO 98                                                    
    2 ID(ISR+N) = ID(ISR+N)+1                                                   
      IF (ID(ISR+N).EQ.1) RETURN                                                
C$ LINE BELOW ADDED                                                             
      IF (ID(ISR+N).EQ.1000) RETURN                                             
      CERR = CERR+1                                                             
      NN = ID(ISN+N)                                                            
      CALL LINES(2)                                                             
      WRITE (OUT,1004) CERR,NN                                                  
 1004 FORMAT(6H0ERROR,I6,5X,27HMULTIPLE STATION DEFINITION,I6)                  
      RETURN                                                                    
      ENTRY LEGRT (N)                                                           
      IF (N.EQ.0) RETURN                                                        
      I = 3                                                                     
      IF (ILR.EQ.0) GO TO 98                                                    
    3 IR = ID(ILR+N)                                                            
      IF (IR.GT.0) RETURN                                                       
      CERR = CERR+1                                                             
      NN = ID(ILN+N)                                                            
      CALL LINES(2)                                                             
      WRITE (OUT,1001) CERR,NN                                                  
 1001 FORMAT(6H0ERROR,I6,5X,13HUNDEFINED LEG,I6)                                
      RETURN                                                                    
      ENTRY START (N)                                                           
      IF (N.EQ.0) RETURN                                                        
      I = 4                                                                     
      IF (ISR.EQ.0) GO TO 98                                                    
    4 IR = ID(ISR+N)                                                            
      IF (IR.GT.0) RETURN                                                       
      CERR = CERR+1                                                             
      NN = ID(ISN+N)                                                            
      CALL LINES(2)                                                             
      WRITE (OUT,1002) CERR,NN                                                  
 1002 FORMAT(6H0ERROR,I6,5X,17HUNDEFINED STATION,I6)                            
      RETURN                                                                    
   98 CALL GDCU(NLEG,4,ISP,D,ILR)                                               
      CALL GDCU(NSTA,4,ISP,D,ISR)                                               
      GO TO (1,2,3,4),I                                                         
C     LEGRS, LEGRT, STARS, START                                                
      RETURN                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CHECKS MATRIX FOR ZERO ROW OR COLUMNS                      
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MCHK (A,N,B)                                                   
      DIMENSION A(N,N), B(N)                                                    
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7)), (CERR,C(16))                                      
      INTEGER CERR, OUT                                                         
      DO 1 I=1,N                                                                
      IF (A(I,I).NE.0.0) GO TO 1                                                
      DO 6 J=1,N                                                                
      IF (A(I,J).NE.0) GO TO 1                                                  
    6 CONTINUE                                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1004) CERR,I                                                   
 1004 FORMAT(6H0ERROR,I6,5X, 'MATRIX ROW (EV)', I4,5H ZERO)                          
    1 CONTINUE                                                                  
      DO 2 J=1,N                                                                
      IF (A(J,J).NE.0.0) GO TO 2                                                
      DO 7 I=1,N                                                                
      IF (A(I,J).NE.0) GO TO 2                                                  
    7 CONTINUE                                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1008) CERR,J                                                   
 1008 FORMAT(6H0ERROR,I6,5X, 'MATRIX COLUMN (SV)', I4,5H ZERO)                       
    2 CONTINUE                                                                  
      RETURN                                                                    
C     MCHK                                                                      
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE SEARCHES TABLE VALUES FOR TABLE LOOKUP SUBROUTINES         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MDISSR(XA,TAB,I,NX,ID,NPX,KEY,IND)                             
      DIMENSION TAB(1)                                                          
                                                                                
      IF(XA-TAB(I))71,73,72                                                     
 71   IND=IND+1                                                                 
      IF (KEY.EQ.0 .OR. KEY.EQ.3) GO TO 74                                      
 73   XA=TAB(I)                                                                 
 74   NPX=I                                                                     
      GO TO 99                                                                  
 72   J=I+NX-1                                                                  
      IF(XA-TAB(J))1,77,76                                                      
   76 IF (ID.NE.0) IND = IND + 1                                                
      IF (KEY.EQ.0 .OR. KEY.EQ.2) GO TO 78                                      
 77   XA=TAB(J)                                                                 
 78   NPX=J-ID                                                                  
      GO TO 99                                                                  
 1    NPT=ID+1                                                                  
      NPB=NPT/2                                                                 
      NPU=NPT-NPB                                                               
      IF (NX-NPT)   4 ,5,10                                                     
 4    ID=NX-1                                                                   
      GO TO 1                                                                   
 5    NPX=I                                                                     
      GO TO 99                                                                  
 10   NLOW=I+NPB                                                                
      NUPP=I+NX-(NPU+1)                                                         
      IF (NX.LE.20) GO TO 18                                                    
      NXX=NX/2+I                                                                
      IF(XA-TAB(NXX)) 12,17,13                                                  
 12   NXX=NXX-NX/4                                                              
      IF (XA.GE.TAB(NXX)) GO TO 17                                              
      GO TO 18                                                                  
 13   NXX=NXX+NX/4                                                              
      IF (XA.GE.TAB(NXX)) GO TO 17                                              
      NLOW=NXX-NX/4                                                             
      GO TO 18                                                                  
 17   NLOW=NXX                                                                  
 18   DO 19 II=NLOW,NUPP                                                        
      NLOC=II                                                                   
      IF (TAB(II).GE.XA ) GO TO 20                                              
 19   CONTINUE                                                                  
      NPX=NUPP-NPB+1                                                            
      GO TO 50                                                                  
 20   NL=NLOC-NPB                                                               
      NU=NL+ID                                                                  
      DO 25 JJ=NL,NU                                                            
      NDIS=JJ                                                                   
      IF (TAB(JJ).EQ.TAB(JJ+1)) GO TO 30                                        
 25   CONTINUE                                                                  
      NPX = NL                                                                  
      IF (XA.EQ.TAB(NPX)) GO TO 99                                              
      GO TO 50                                                                  
 30   IF (TAB(NDIS).LT.XA ) GO TO 40                                            
      NPX=NDIS-ID                                                               
      IF (XA.EQ.TAB(NPX)) GO TO 99                                              
      GO TO 50                                                                  
 40   NPX=NDIS+1                                                                
   50 CONTINUE                                                                  
      IF (ID.EQ.0) NPX= NPX-1                                                   
   99 RETURN                                                                    
C     MDISSR                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE MULTI-DIMENSIONAL TABLE LOOKUP CONTROL                     
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MDSCT (NDIM,IND,Y,TABY,X,TABX,NX,IX,Z,TABZ,NZ,IZ      )        
                                                                                
      DIMENSION TABX(1), TABY(1), TABZ(1)                                       
      DIMENSION  ZX(8),ZY(8),ZZ(8),NPX(8),NPY(8),XA(8),YA(8)                    
                                                                                
                                                                                
      IF (NDIM.EQ.2) THEN                                                       
         IND=0                                                                  
         IEX = IX/100                                                           
         IIX = IX-IEX*100                                                       
         INX = IIX/10                                                           
         IIX = IIX-10*INX                                                       
         XA(1) = X                                                              
         CALL MDISSR (XA(1),TABX,1,NX,IIX,NPX,IEX,IND)                          
         NPX1=NPX(1)                                                            
         Y = FLAGRA (XA(1),TABX(NPX1),TABY(NPX1),IIX+1)                         
         RETURN                                                                 
      ENDIF                                                                     
                                                                                
      IND=0                                                                     
      IEX = IX/100                                                              
      IIX = IX-IEX*100                                                          
      INX = IIX/10                                                              
      IIX = IIX-10*INX                                                          
      IEZ = IZ/100                                                              
      IIZ = IZ-IEZ*100                                                          
      INZ = IIZ/10                                                              
      IIZ = IIZ-10*INZ                                                          
      ZA=Z                                                                      
      CALL MDISSR(ZA,TABZ,1,NZ,IIZ,NPZ,IEZ,IND)                                 
      IND=IND*10                                                                
      NPZL = NPZ + IIZ                                                          
      I=1                                                                       
      IF (INX.NE.0.OR.INZ.NE.0.AND.NDIM.GT.3) GO TO 160                         
      IF (NDIM.EQ.33) GO TO 140                                                 
      XA(1) = X                                                                 
      CALL MDISSR (XA(1),TABX,1,NX,IIX,NPX,IEX,IND)                             
      DO 135 JJ=NPZ,NPZL                                                        
      NPY(I) = (JJ-1)*NX+NPX(1)                                                 
      NPX(I)=NPX(1)                                                             
  135 I=I+1                                                                     
      I = 1                                                                     
      GO TO 150                                                                 
  140 DO 145 JJ=NPZ,NPZL                                                        
      XA(I) = X                                                                 
      IS = (JJ-1)*NX+1                                                          
      CALL MDISSR (XA(I),TABX,IS,NX,IIX,NPX(I),IEX,IND)                         
      NPY(I)=NPX(I)                                                             
  145 I=I+1                                                                     
  150 IZL = IIZ+1                                                               
      DO 155 JJ=1,IZL                                                           
      NLOC = NPX(JJ)                                                            
      NOCY = NPY(JJ)                                                            
      IF (NDIM.EQ.33) I=JJ                                                      
  155 YA(JJ) = FLAGRA (XA(I),TABX(NLOC),TABY(NOCY),IIX+1)                       
      Y = FLAGRA (ZA,TABZ(NPZ),YA,IZL)                                          
      GO TO 99                                                                  
  160 CONTINUE                                                                  
      NPT = IIX+1                                                               
      IF (IIX.EQ.0) NPT=2                                                       
      NPB = NPT/2                                                               
      J = 0                                                                     
      DO 220 IXY=1,NX                                                           
      I = 0                                                                     
      IF (J.LT.NPT) GO TO 200                                                   
      IF (X.LE.XA(1)) GO TO 230                                                 
      IF (X.LE.XA(NPB)) GO TO 230                                               
      IF (X.LE.XA(NPT)) GO TO 180                                               
  165 IS = 2                                                                    
  170 J = 0                                                                     
      DO 175 II=IS,NPT                                                          
      J = J+1                                                                   
      XA(J) = XA(II)                                                            
      YA(J) = YA(II)                                                            
  175 CONTINUE                                                                  
      GO TO 200                                                                 
  180 DO 185 II=1,NPT                                                           
      IS = NPT-II                                                               
      IF (IS.EQ.NPB) GO TO 230                                                  
      IF (X.GE.XA(IS).AND.X.LE.XA(IS+1)) GO TO 190                              
  185 CONTINUE                                                                  
  190 NPN = IS-1                                                                
      IF ((IXY+NPN).LE.NX) GO TO 170                                            
      GO TO 165                                                                 
  200 I = 0                                                                     
      DO 205 JJ=NPZ,NPZL                                                        
      IS = (JJ-1)*NX+IXY                                                        
      I = I+1                                                                   
      ZX(I) = TABX(IS)                                                          
      ZY(I) = TABY(IS)                                                          
      ZZ(I)=TABZ(JJ)                                                            
  205 CONTINUE                                                                  
      J = J+1                                                                   
      XA(J) = FLAGRA (ZA,ZZ(1),ZX(1),I)                                         
      YA(J) = FLAGRA (ZA,ZZ(1),ZY(1),I)                                         
  220 CONTINUE                                                                  
  230 XO = X                                                                    
      CALL MDISSR (XO,XA,1,NPT,IIX,NPX1,IEX,IND)                                
      Y = FLAGRA (XO,XA(NPX1),YA(NPX1),IIX+1)                                   
   99 RETURN                                                                    
C     MDSCT                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C$        ADD TO WRITES TO IDENTIFY EV'S AS ROWS AND SV'S AS COLS               
C**********************************************************************         
C                                                                               
      SUBROUTINE MPRNT (A,N,B)                                                  
      DIMENSION A(N,N), B(N)                                                    
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7))                                                    
      INTEGER OUT                                                               
      CALL LINES(1)                                                             
      WRITE (OUT,1001)                                                          
 1001 FORMAT(      ' MATRIX SV1    SV2         SV3         SV4         '        
     *  ,'SV5         SV6         SV7         SV8         SV9         '         
     *  ,'SV10')                                                                
      DO 1 I=1,N                                                                
      CALL LINES(2)                                                             
      WRITE (OUT,1005)                                                          
 1005 FORMAT(' ')                                                               
      WRITE (OUT,1003) I,I                                                      
 1003 FORMAT(' EQUATION FOR EV ROW ', I2, ' VALUES ARE:'                        
     *  ,'  DEL(EV(ROW ',I2,'))/DEL(SV(COLUMN))')                               
      CALL LINES(N/10+1)                                                        
      WRITE (OUT,1002) (A(I,J),J=1,N),B(I)                                      
 1002 FORMAT(1X,10E12.5)                                                        
    1 CONTINUE                                                                  
      RETURN                                                                    
C     MPRNT                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C$  ALL NEW PER COSTELLO                                                        
C$  RETURN METHOD MODIFIED SFW 4/9/90                                           
C**********************************************************************         
C                                                                               
      SUBROUTINE MRDSLN (IRETRN)                                                
      COMMON /CC/C(600)                                                         
      EQUIVALENCE (IDS,C(5)),(OUT,C(7)),(CERR,C(16)),(PASS,C(17))               
     * ,(ISVS,C(92)),(NLEG,C(25)),(NSTA,C(26)),(IGA,C(35))                      
     * ,(MRDNIT,C(373)),(ISP,C(42)),(ISVT,C(46)),(ISV,C(47))                    
     * ,(IEVT,C(48)),(IEV,C(49)),(NSV,C(50)),(NEV,C(51))                        
     * ,(IFP,C(22)),(MRD,C(372)),(IW,C(27)),(IP,C(28)),(IT,C(29))               
     * ,(IH,C(30)),(IMP,C(66)),(IME,C(67)),(ILN,C(37)),(ISN,C(38))              
     * ,(NIT,C(76)),(PF,C(77)),(NWT,C(78)),(ITAD,C(54))                         
     * ,(SVLL(1),C(181)),(SVUL(1),C(191)),(ICONV,C(79)),(IIOP,C(90))            
     * ,(ERRL,C(83)),(BIG,C(31))                                                
      INTEGER OUT, PASS, CERR                                                   
C$    COMMON /CCA/ CA(100)                                                      
      COMMON /CCA/ CA(150)                                                      
      EQUIVALENCE (LIST(1),CA(11)),(SVSCLE(1),CA(31)),(EVSCLE(1),CA(51))        
     * ,(DF(1),CA(71)),(DFO(1),CA(91)),(ITER,CA(111)),(RST,CA(112))             
     * ,(STEP,CA(113)),(IEVS,CA(122)),(ESUM,CA(114)),(DSUM,CA(115))             
     * ,(SUM,CA(116)),(ITBL50,CA(117)),(ITBL51,CA(118)),(NORD,CA(119))          
     * ,(KVAR,CA(120)),(IDONE,CA(121)),(IWS,CA(130)),(IPS,CA(131))              
     * ,(ITS,CA(132)),(IHS,CA(133))                                             
      DIMENSION LIST(20),DF(20),DFO(20),SVSCLE(20),EVSCLE(20)                   
      DIMENSION SVLL(10),SVUL(10),SV(100),EV(100),YY(100)                       
      DIMENSION CD(100)                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84))                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      IEND = KVAR                                                               
      IF(IEND.GT.NSV) IEND = NSV                                                
C*** INITIALIZE AND SELECT THE PATH FOR THE GIVEN MRD OPTION                    
      ITER = ITER+1                                                             
      IBEGIN = 1                                                                
      IF(MRD.EQ.1) IBEGIN = IEND                                                
      IF(MRD.EQ.2) IBEGIN = KVAR-1                                              
      IDONE = 0                                                                 
C*** WRITE THE VALUES OF THE SCALED STATE VARIABLES                             
      DO 70 J = 1,NSV                                                           
      SV(J) = D(ISV+LIST(J))/SVSCLE(LIST(J))                                    
   70 CONTINUE                                                                  
      WRITE(OUT,390) (SV(J),J=1,NSV)                                            
  390 FORMAT(' VALUES OF THE SCALED STATE VARIABLES',/,1X,5E18.7)               
C*** WRITE THE VALUES OF THE SCALED ERROR VARIABLES                             
      DO 71 J = 1,NEV                                                           
      EV(J) = -D(IME+LIST(J))/EVSCLE(LIST(J))                                   
   71 CONTINUE                                                                  
      WRITE(OUT,396) (EV(J),J=1,NEV)                                            
  396 FORMAT(' VALUES OF THE SCALED ERROR VARIABLES',/,1X,5E18.7)               
      DO 5 J = 1,NSV                                                            
    5 DF(J) = 0.0                                                               
C*** COMPUTE THE SUM OF THE SQUARES OF THE ERRORS                               
      ESUM = 0.0                                                                
      DSUM = 0.0                                                                
      SUM = 0.0                                                                 
      DO 20 J = 1,NEV                                                           
      XX = (D(IME+J)**2)/(EVSCLE(J)**2)                                         
      IF(XX.GT.1.0) IDONE = IDONE+1                                             
   20 ESUM = ESUM+XX                                                            
      RMSSV = SQRT(ESUM/NEV)                                                    
      WRITE(OUT,2031) RMSSV                                                     
 2031 FORMAT(' ROOT-MEAN-SQUARE OF SCALED ERROR VARIABLES =',                   
     * E15.7)                                                                   
C*** COMPUTE DERIVATIVES OF ERROR VARIABLES W.R.T STATE VARIABLES               
      DO 35 L = IBEGIN,IEND                                                     
      J = LIST(L)                                                               
      DO 30 K = 1,NEV                                                           
      I = IMP+(J-1)*NSV                                                         
   30 DF(J) = DF(J)+2.*SVSCLE(J)*D(IME+K)*D(I+K)/EVSCLE(K)**2                   
C*** COMPUTE THE SUM OF THE SQUARES OF THE DERIVATIVES                          
   35 SUM = SUM+DF(J)**2                                                        
C     IF(IDS.LT.2) GO TO 35                                                     
      CALL LINES(4)                                                             
      WRITE(OUT,300)                                                            
  300 FORMAT('0',10X,'DERIVATIVE OF SCALED E.V. WITH RESPECT ',                 
     * 'TO SCALED S.V.')                                                        
      WRITE(OUT,305)                                                            
  305 FORMAT('0  S.V.    E.V.:',5X,'1',13X,'2',13X,'3',13X,'4',13X,'5')         
      DO 31 L = 1,NSV                                                           
      I = IMP+(LIST(L)-1)*NSV                                                   
      DO 31 II = 1,NEV,5                                                        
      JJ = II+4                                                                 
      IF(JJ.GT.NEV) JJ = NEV                                                    
      DO 72 KK = II,JJ                                                          
      YY(KK) = D(I+KK)*SVSCLE(LIST(L))/EVSCLE(KK)                               
   72 CONTINUE                                                                  
      CALL LINES(1)                                                             
      WRITE(OUT,310) LIST(L),II,(YY(KK),KK=II,JJ)                               
  310 FORMAT(' ',I4,4X,I4,5E14.7)                                               
   31 CONTINUE                                                                  
      SDF = SQRT(SUM)                                                           
C***COMPUTE AND SAVE DERIVATIVES DIVIDED BY SQRT OF SUM OF THEIR SQUARES        
      DO 40 L = IBEGIN,IEND                                                     
      J = LIST(L)                                                               
      DF(J) = DF(J)/SDF                                                         
C*** COMPUTE THE DIRECTION COSINE RELATIVE TO THE PREVIOUS STEP                 
      DSUM = DSUM+DFO(J)*DF(J)                                                  
   40 DFO(J) = DF(J)                                                            
C*** COMPUTE THE NEW STEP SIZE                                                  
      STEP = STEP*RST**DSUM                                                     
C*** COMPUTE THE NEW VALUES OF THE STATE VARIABLES                              
      DO 80 I = IBEGIN,IEND                                                     
      J = LIST(I)                                                               
      D(ISV+J) = D(ISVS+J)+STEP*SVSCLE(J)*DF(J)                                 
      IF(MRD.NE.3) D(ISVS+J)=D(ISV+J)                                           
      IF(MRD.NE.3) D(IEVS+J)=D(IEV+J)                                           
      K = ID(ISVT+J)                                                            
C*** CORRECT THE NEW VALUES SO THEY ARE WITHIN THE PERMISSIBLE LIMITS           
      IF(D(ISV+J)-SVLL(K)) 55,55,60                                             
   55 CALL LINES(3)                                                             
      WRITE(OUT,100)                                                            
  100 FORMAT('0*+* WARNING FROM MRD SOLUTION')                                  
      WRITE(OUT,110)J,SVLL(K),D(ISV+J)                                          
  110 FORMAT(' STATE VARIABLE',I5,' RESET TO',E15.8,' FROM',E15.8)              
      D(ISV+J) = SVLL(K)                                                        
      GO TO 80                                                                  
   60 IF(D(ISV+J)-SVUL(K)) 80,80,65                                             
   65 CALL LINES(3)                                                             
      WRITE(OUT,100)                                                            
      WRITE(OUT,110)J,SVUL(K),D(ISV+J)                                          
      D(ISV+J) = SVUL(K)                                                        
   80 CONTINUE                                                                  
      WRITE(OUT,2032)ITER,STEP,DSUM,(LIST(J),DF(LIST(J)),J=1,NSV)               
 2032 FORMAT(' MRD TRIAL NUMBER:',I2,/,' SOLN STEP SIZE = ',E15.7,              
     *' DIRECTION COSINE = ',E15.7,/,' DERIVATIVES OF THE ',                    
     *'RMS ERROR WITH RESPECT TO THE SCALED STATE VARIABLES',/,                 
     *' S.V. NO.     DERIVATIVES',/, 20(I10,E16.8,/))                           
C*** DEPENDING ON WHICH OPTION IS INVOKED, UPDATE THE STORED BASE               
C    POINT OF THE SOLN1 SOLUTION                                                
C$                                                                              
      IF(MRD.EQ.3) IRETRN=2                                                     
      IF(MRD.EQ.3) RETURN                                                       
      DO 2 I = 1,NSV                                                            
      D(ISVS+I) = D(ISV+I)                                                      
    2 CONTINUE                                                                  
      DO 3 I = 1,NLEG                                                           
    3 D(IWS+I) = D(IW+I)                                                        
      DO 4 I = 1,NSTA                                                           
      D(IPS+I) = D(IP+I)                                                        
      D(ITS+I) = D(IT+I)                                                        
    4 D(IHS+1) = D(IH+I)                                                        
C*** SOLVE THE SYSTEM EQUATIONS FOR THE MRD METHODS                             
      CALL PCOMPP                                                               
      DO 88 I = 1,NSV                                                           
   88 D(IEVS+I) = D(IEV+I)                                                      
C$                                                                              
      IF(ITER.GE.MRDNIT) IRETRN=2                                               
      IF(ITER.GE.MRDNIT) RETURN                                                 
      IF(MRD.EQ.1.AND.ITER.EQ.1) IRETRN=1                                       
      IF(MRD.EQ.1.AND.ITER.EQ.1) RETURN                                         
      IF(IDONE.EQ.0) IRETRN=2                                                   
      IF(IDONE.EQ.0) RETURN                                                     
      IF(MRD.EQ.1) IRETRN=1                                                     
      IF(MRD.EQ.1) RETURN                                                       
      IF(MOD(KVAR,2).NE.0) IRETRN=3                                             
      IF(MOD(KVAR,2).NE.0) RETURN                                               
      KVAR = KVAR-1                                                             
      IRETRN=3                                                                  
      RETURN                                                                    
C    MRDSLN                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MSOL (A,NA,B,E,IND)                                            
      DIMENSION A(NA,NA), B(NA), E(NA)                                          
      INTEGER E                                                                 
      EQUIVALENCE (AMAX,SWAP,MSWAP,G)                                           
      IF (NA.GT.1) GO TO 1                                                      
      E(1) = 1                                                                  
      IF (A(1,1).EQ.0.0) GO TO 257                                              
      B(1) = B(1)/A(1,1)                                                        
      A(1,1) = B(1)                                                             
      GO TO 250                                                                 
    1 CONTINUE                                                                  
      DO 5 I=1,NA                                                               
 5    E(I)=I                                                                    
      NA1 = NA-1                                                                
      DO 142 K=1,NA1                                                            
      AMAX = ABS(A(K,K))                                                        
      II=K                                                                      
      JJ=K                                                                      
      DO 18 J=K,NA                                                              
      DO 18 I=K,NA                                                              
      IF (AMAX.GE.ABS(A(I,J))) GO TO 18                                         
C     WRITE(6,8999) ABS(A(I,J)),I,J                                             
C8999 FORMAT(' ','ABS(A(I,J))=',E16.8,'I=',I5,'J=',I5)                          
      II = I                                                                    
      JJ=J                                                                      
      AMAX=ABS(A(I,J))                                                          
 18   CONTINUE                                                                  
      IF (K.EQ.II) GO TO 61                                                     
      DO 44 I=1,NA                                                              
      SWAP = A(K,I)                                                             
      A(K,I) = A(II,I)                                                          
C     WRITE(6,8998) A(K,I)                                                      
C8998 FORMAT(' ','18-44A(K,I)=',E16.8)                                          
 44   A(II,I) = SWAP                                                            
      SWAP = B(K)                                                               
      B(K) = B(II)                                                              
C     WRITE(6,8997) B(K)                                                        
C8997 FORMAT(' ','44-61B(K)=',E16.8)                                            
      B(II) = SWAP                                                              
   61 IF (K.EQ.JJ) GO TO 89                                                     
      DO 81 I=1,NA                                                              
      SWAP = A(I,K)                                                             
      A(I,K) = A(I,JJ)                                                          
C     WRITE(6,8996) A(I,K)                                                      
C8996 FORMAT(' ','61-81A(I,K)=',E16.8)                                          
 81   A(I,JJ) = SWAP                                                            
      MSWAP = E(JJ)                                                             
      E(JJ) = E(K)                                                              
C     WRITE(6,8995) E(JJ)                                                       
C8995 FORMAT(' ','81-89E(JJ)=',I5)                                              
      E(K)=MSWAP                                                                
   89 IF (A(K,K).EQ.0.0) GO TO 257                                              
      J = K+1                                                                   
      DO 142 I=J,NA                                                             
C     WRITE(6,8994) A(I,K)                                                      
C8994 FORMAT(' ','89-123A(I,K)=',E16.8)                                         
      IF (A(I,K).EQ.0.0) GO TO 142                                              
      G = A(I,K)/A(K,K)                                                         
C     WRITE(6,9998) G,A(I,K),A(K,K)                                             
C9998 FORMAT(' ','G=',E12.5,'A(I,K)=',E16.8,'A(K,K)=',E16.8)                    
      DO 123 II=J,NA                                                            
C     WRITE(6,9997) I,II,A(I,II),A(K,II)                                        
C9997 FORMAT(' ','I=',I5,'II=',I5,'A(I,II)=',E16.8,'A(K,II)=',E16.8)            
      A(I,II) = A(I,II)-G*A(K,II)                                               
C     WRITE(6,9981) A(I,II)                                                     
C9981 FORMAT (' ','A(I,II) = A(I,II)-G*A(K,II)=',E16.8)                         
 123  CONTINUE                                                                  
C     WRITE(6,9996) I,K,B(I),B(K)                                               
C9996 FORMAT(' ','I=',I5,'K=',I5,'B(I)=',E12.5,'B(K)=',E16.8)                   
      B(I) = B(I)-G*B(K)                                                        
C     WRITE(6,9995) B(I)                                                        
C9995 FORMAT(' ','B(I)=B(I)-G*B(K)=',E16.8)                                     
 142  CONTINUE                                                                  
      IF (A(NA,NA).EQ.0.0) GO TO 257                                            
C     WRITE(6,9984) NA,B(NA),A(NA,NA)                                           
C9984 FORMAT(' ','NA=',I5,'B(NA)=',E16.8,'A(NA,NA)=',E16.8)                     
      B(NA) = B(NA)/A(NA,NA)                                                    
C     WRITE(6,9994) B(NA)                                                       
C9994 FORMAT(' ','B(NA)=B(NA)/A(NA,NA)=',E16.8)                                 
      DO 205 I=2,NA                                                             
      J = NA-I+1                                                                
      K = I-1                                                                   
      G = 0.0                                                                   
      DO 202 II=1,K                                                             
      JJ = NA-II+1                                                              
C     WRITE(6,9993) G,J,JJ,A(J,JJ),B(JJ)                                        
C9993 FORMAT(' G=',E16.8,'J=',I5,'JJ=',I5,                                      
C    C           'IA(J,JJ)=',E16.8,'B(JJ)=',E16.8)                              
 202  G = G+A(J,JJ)*B(JJ)                                                       
C     G = G+A(J,JJ)*B(JJ)                                                       
C202  WRITE(6,9992) G                                                           
C9992 FORMAT(' ','G = G+A(J,JJ)*B(JJ)=',E16.8)                                  
C     WRITE(6,9962) B(J),G,A(J,J)                                               
C9962 FORMAT(' B(J)=',E16.8,'G=',E16.8,'A(J,J)=',E16.8)                         
      B(J) = (B(J)-G)/A(J,J)                                                    
C     WRITE(6,9972) B(J)                                                        
C9972 FORMAT(' ','B(J)=(B(J)-G)/A(J,J)=',E16.8)                                 
 205  CONTINUE                                                                  
      DO 233 I=1,NA                                                             
      J = E(I)                                                                  
  233 A(J,1) = B(I)                                                             
 250  IND=1                                                                     
   99 CONTINUE                                                                  
      RETURN                                                                    
 257  IND=3                                                                     
      GO TO 99                                                                  
C     MSOL                                                                      
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MSTO (I,J,VAL)                                                 
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IMP,C(66)), (IME,C(67)), (NSV,C(50))                         
      COMMON /DC/ DZ(2),D(128001)                                               
      IF (J.EQ.0) GO TO 1                                                       
      I1 = IMP+(J-1)*NSV+I                                                      
      D(I1) = VAL                                                               
      GO TO 99                                                                  
    1 I1 = IME+I                                                                
      D(I1) = -VAL                                                              
   99 CONTINUE                                                                  
      RETURN                                                                    
C      MSTO                                                                     
      END                                                                       
      SUBROUTINE OUTPTA                                                 01100000
      COMMON /CC/ C(600)                                                01110000
      COMMON /TWMAT/ TW(9999)                                           01120000
      EQUIVALENCE (OUT,C(7)), (NLEG,C(25)), (ILN,C(37)), (IW,C(27))     01130000
     *, (NSTA,C(26)), (ISN,C(38)), (IP,C(28)), (IT,C(29)), (IH,C(30))   01140000
     *, (PASS,C(17)), (ISP,C(42)), (ICONV,C(79))                        01150000
     *, (NSV,C(50)), (NEV,C(51)), (ISVT,C(46)), (IEVT,C(48))            01160000
     *, (ISV,C(47)), (IEV,C(49)), (CERR,C(16)), (IGA,C(35))             01170000
C$                                                                      01180000
      INTEGER OUT, PASS, CERR                                           01190000
      DIMENSION CD(200)                                                 01200000
      EQUIVALENCE (CD(1),C(201))                                        01210000
      EQUIVALENCE (DATE,CD(83)), (CN,CD(84))                            01220000
      COMMON /DC/ DZ(2),D(128001)                                       01230001
      DIMENSION ID(128001)                                              01240001
      EQUIVALENCE (ID(1),D(1))                                          01250001
      EQUIVALENCE (BLANKS,CD(23))
      REAL*8 CD, SN, DATE, CN, BLANKS                                      01260000
      DATA SN/8HOUTPTA  /                                               01270000
C@      CALL SECOND(TIME)                                                 01280000
      CALL LINES(100)                                                   01290000
      WRITE (OUT,1000) TIME                                             01300000
 1000 FORMAT(8H TIME E ,F10.2)                                          01310000
      CN = SN                                                           01320000
      IF (ILN.EQ.0.OR.ISN.EQ.0) GO TO 99                                01330000
      CALL GDCU(NLEG,4,ISP,D,L)                                         01340000
      CALL GDCU(NLEG,4,ISP,D,LL)                                        01350000
      CALL GDCU(NLEG,4,ISP,D,LLL)                                       01360000
      NX = NLEG                                                         01370000
      IXN = ILN                                                         01380000
      ASSIGN 10 TO IS                                                   01390000
      GO TO 4                                                           01400000
   10 CONTINUE                                                          01410000
      CALL LINES(3)                                                     01420000
      WRITE (OUT,1001)                                                  01430000
 1001 FORMAT(20H0FLOW RATE(S)-LB/MIN)                                   01440000
      IXN = IW                                                          01450000
      ASSIGN 11 TO IS                                                   01460000
      GO TO 6                                                           01470000
   11 CONTINUE                                                          01480000
      CALL PARNF2(NLEG,ID(LLL+1),D(L+1))                                01490000
      CALL FDC(NLEG,4,ISP,D,LLL)                                        01500000
      CALL FDC(NLEG,4,ISP,D,LL)                                         01510000
      CALL FDC(NLEG,4,ISP,D,L)                                          01520000
      CALL GDCU(NSTA,4,ISP,D,L)                                         01530000
      CALL GDCU(NSTA,4,ISP,D,LL)                                        01540000
      CALL GDCU(NSTA,4,ISP,D,LLL)                                       01550000
      NX = NSTA                                                         01560000
      IXN = ISN                                                         01570000
      ASSIGN 20 TO IS                                                   01580000
      GO TO 4                                                           01590000
   20 CONTINUE                                                          01600000
      CALL LINES(3)                                                     01610000
      WRITE (OUT,1002)                                                  01620000
 1002 FORMAT(16H0PRESSURE(S)-PSI)                                       01630000
      IXN = IP                                                          01640000
      ASSIGN 21 TO IS                                                   01650000
      GO TO 6                                                           01660000
   21 CONTINUE                                                          01670000
      CALL PARNF2(NSTA,ID(LLL+1),D(L+1))                                01680000
C                                                                       01690000
      CALL LINES(3)                                                     01700000
      WRITE (OUT,1003)                                                  01710000
 1003 FORMAT(21H0TEMPERATURE(S)-DEG R)                                  01720000
      IXN = IT                                                          01730000
      ASSIGN 22 TO IS                                                   01740000
      GO TO 6                                                           01750000
   22 CONTINUE                                                          01760000
      CALL PARNF2(NSTA,ID(LLL+1),D(L+1))                                01770000
C                                                                       01780000
      CALL LINES(3)                                                     01790000
      WRITE (OUT,1004)                                                  01800000
 1004 FORMAT(41H0HUMIDITY(S)/ENTHALPY(S)-LBW/LBA / BTU/LB)              01810000
      IXN = IH                                                          01820000
      ASSIGN 23 TO IS                                                   01830000
      GO TO 6                                                           01840000
   23 CONTINUE                                                          01850000
      CALL PARNF5(NSTA,ID(LLL+1),D(L+1))                                01860000
C$ WALL TEMP                                                            01870000
      CALL LINES(3)                                                     01880000
      WRITE (OUT,1005)                                                  01890000
 1005 FORMAT(30H0WALL TEMP(S)- DEGREES RANKINE)                         01900000
      CALL PARNF9(NSTA,ID(LLL+1),TW)                                    01910000
C$                                                                      01920000
      CALL FDC(NSTA,4,ISP,D,LLL)                                        01930000
      CALL FDC(NSTA,4,ISP,D,LL)                                         01940000
      CALL FDC(NSTA,4,ISP,D,L)                                          01950000
      IF (NSV.EQ.0) GO TO 99                                            01960000
      CALL LINES(2)                                                     01970000
      WRITE (OUT,1103)                                                  01980000
 1103 FORMAT(23H0STATE VARIABLE TYPE(S))                                01990000
      CALL IPRNTH(NSV,ID(ISVT+1))                                       02000000
      CALL LINES(2)                                                     02010000
      WRITE (OUT,1102)                                                  02020000
 1102 FORMAT(18H0STATE VARIABLE(S))                                     02030000
      CALL PARNTH(NSV,D(ISV+1))                                         02040000
      CALL LINES(2)                                                     02050000
      WRITE (OUT,1105)                                                  02060000
 1105 FORMAT(23H0ERROR VARIABLE TYPE(S))                                02070000
      CALL IPRNTH(NEV,ID(IEVT+1))                                       02080000
      CALL LINES(2)                                                     02090000
      WRITE (OUT,1104)                                                  02100000
 1104 FORMAT(18H0ERROR VARIABLE(S))                                     02110000
      CALL PARNTH(NEV,D(IEV+1))                                         02120000
      IF (ICONV.NE.0) GO TO 24                                          02130000
      I = IFIX(D(IGA+1))                                                02140000
      CALL LINES(2)                                                     02150000
      WRITE (OUT,1100) I                                                02160000
 1100 FORMAT(23H0SOLUTION CONVERGED IN ,I6,7H TRY(S))                   02170000
      GO TO 99                                                          02180000
   24 CERR = CERR+1                                                     02190000
      CALL LINES (2)                                                    02200000
      WRITE (OUT,1101) CERR                                             02210000
 1101 FORMAT(6H0ERROR,I6,5X,15HNON-CONVERGENCE)                         02220000
   99 CONTINUE                                                          02230000
      CALL LINES(2)                                                     02240000
      WRITE (OUT,1006) CERR                                             02250000
 1006 FORMAT(1H0,I6,18H ERROR(S) DETECTED)                              02260000
      RETURN                                                            02270000
    4 IN = 10000                                                        02280000
      DO 1 I=1,NX                                                       02290000
      ID(L+I) = ID(IXN+I)                                               02300000
      IF (ID(L+I).NE.0) GO TO 1                                         02310000
      IN = IN+1                                                         02320000
      ID(L+I) = IN                                                      02330000
    1 CONTINUE                                                          02340000
      DO 2 I=1,NX                                                       02350000
      N1 = ID(L+1)                                                      02360000
      K = 1                                                             02370000
      DO 3 J=1,NX                                                       02380000
      N2 = ID(L+J)                                                      02390000
      IF (N2.GE.N1) GO TO 3                                             02400000
      N1 = N2                                                           02410000
      K = J                                                             02420000
    3 CONTINUE                                                          02430000
      ID(L+K) = 100000000                                               02440000
      ID(LL+I) = K                                                      02450000
      ID(LLL+I) = ID(IXN+K)                                             02460000
    2 CONTINUE                                                          02470000
      GO TO IS, (10,20)                                                 02480000
    6 DO 5 I=1,NX                                                       02490000
      J = ID(LL+I)                                                      02500000
      D(L+I) = D(IXN+J)                                                 02510000
    5 CONTINUE                                                          02520000
C$    GO TO IS, (11,21,22,23,231)                                       02530000
      GO TO IS, (11,21,22,23)                                           02540000
C     OUTPTA                                                            02550000
      END                                                               02560000
      SUBROUTINE OUTPTB                                                 00010000
C$         WRITES IN HXSP ARE ON                                        00020000
C$    IN OUTPTB; ADD WRITES TO INVESTIGATE                              00030000
C$    IN APUSP ; ADD MINIMUM TO FUEL (LB/HR)/HP                         00040000
C$    IN GAM;                                                           00050000
C$    IN SHP;  ADDED TLUP CALL TO GET CP FOR AIR AT LOW TEMP,HI PRESSURE00060000
C$    IN LINEPP;                                                        00070000
C$    IN HXSP  ; CHANGE L GES'S; ADD VLC, VLH WRITES FOR GAS HX'S       00080000
C$    IN TABR  ; FIX 'TOO MANY VALUES' COMPLAINT FOR NDIM=4 TABLES      00090000
C$    IN ORIFPP;                                                        00100000
C$    IN TDB   ; IS PRINTING GOOFY MESSAGE:                             00110000
C$                                  ***CONDENSATION - HS 0.00000        00120000
C$    IN SUBROUTINE TLUP  : FIX NDIM=4                                  00130000
C$    IN SUBROUTINE NZZLPP: ADD WRITES                                  00140000
C$                                                                      00150000
C$                                                                      00160000
      COMMON /CC/ C(600)                                                00170000
      EQUIVALENCE (OUT,C(7)), (CERR,C(16)), (ISSP,C(123)), (ISPP,C(124))00180000
     *, (WTS,C(101)), (CUS,C(103)), (RIS,C(105)), (DRS,C(107))          00190000
     *, (WTDS,C(110)), (POWES,C(112)), (POWSS,C(113)), (POWHS,C(114))   00200000
     *, (BAES,C(115)), (FC,C(121)), (SDRAG,C(122))                      00210000
     *, (IFCW,C(119)), (IFCP,C(129)), (OBAE,C(98)), (OSHP,C(99))        00220000
     *, (GWHP,C(118)), (ELL,C(116)), (HWHP,C(100)), (HLL,C(117))        00230000
     *, (WTF,C(96)), (WTE,C(97)), (WTG,C(95)), (DRAG,C(94))             00240000
C$WAS*, (TIME,C(23)),.. NOT TOO SWIFT TO USE CLOCK TIME IN PSR EQ BELOW 00250000
     *, (MSNTIM,C(23)), (EXWT,C(128)), (LDR,C(93)), (THR,C(130))        00260000
     *, (IGA,C(35))                                                     00270000
      INTEGER OUT, CERR                                                 00280000
      REAL LDR, MSNTIM                                                  00290000
      DIMENSION CD(200)                                                 00300000
       REAL*8  CD                                                       00310000
       REAL*8  DATE, CN                                                 00320000
      EQUIVALENCE (CD(1),C(201))                                        00330000
      EQUIVALENCE (DATE,CD(83)), (CN,CD(84))                            00340000
      COMMON /DC/ DZ(2),D(128001)                                       00350001
      DIMENSION ID(128001)                                              00360001
      EQUIVALENCE (ID(1),D(1))                                          00370001
       REAL*8 SN                                                        00380000
      DATA SN/8HOUTPTB  /                                               00390000
C@      CALL SECOND(TIME)                                                 00400000
      CALL LINES(100)                                                   00410000
      WRITE (OUT,1000) TIME                                             00420000
 1000 FORMAT(8H TIME E ,F10.2)                                          00430000
      CN = SN                                                           00440000
      IF (ISSP.NE.0 .AND. ISPP.NE.0) GO TO 99                           00450000
      ESHP = POWSS+POWES/0.85+POWHS/0.9                                 00460000
      IF (ISSP.NE.0) GO TO 10                                           00470000
      IF (DRS.GT.1000.0) DRS = 1000.0                                   00480000
      WTDS = SQRT(WTDS)                                                 00490000
      CALL LINES(8)                                                     00500000
      WRITE (OUT,1001) WTS,CUS,RIS,DRS,WTDS                             00510000
 1001 FORMAT(9H0********/9H *SYSTEM*/9H ********/                       00520000
     *1H0,5X,7HWEIGHT ,F7.0,5X,11HCOST UNITS ,F7.0,5X,                  00530000
     *18HRELIABILITY INDEX ,F7.4,5X,17HDEVELOPMENT RISK ,F5.2/1H0,5X,   00540000
     *22HWEIGHT STANDARD ERROR ,F7.1)                                   00550000
      CALL LINES(6)                                                     00560000
C$ ADD EXWT TO PRINT; CHANGE "EWT" TO "EXWT" TO AVOID CONFUSION W/ "WTE"00570000
      WRITE (OUT,1002) POWSS,POWHS,POWES,ESHP,BAES,FC,SDRAG,EXWT        00580000
 1002 FORMAT(1H0,5X,12HSHAFT POWER ,F7.1,5X,16HHYDRAULIC POWER ,F7.1,5X,00590000
     *17HELECTRICAL POWER ,F7.1/                                        00600000
     *1H0,5X,29HTOTAL EQUIVALENT SHAFT POWER ,F7.0/                     00610000
     *1H0,5X,21HBLEED AIR EXTRACTION ,F7.0,5X,17HFUEL CONSUMPTION ,F7.0,00620000
     *5X,9HECS DRAG ,F7.0, 5X,14HEXPENDABLE WT ,F7.0)                   00630000
   10 CONTINUE                                                          00640000
      IF (ISPP.NE.0) GO TO 20                                           00650000
      POWEQQ = POWES                                                    00660000
      POWHQQ = POWHS                                                    00670000
      POWES = POWES/0.85                                                00671000
      POWHS = POWHS/0.9                                                 00672000
      D(IGA+62) = THR                                                   00673000
      D(IGA+63) = 0.0                                                   00674000
      SFCI = TLUP(IFCW)                                                 00675000
      D(IGA+63) = OBAE                                                  00676000
      D(IGA+64) = OSHP                                                  00677000
      SFCR = TLUP(IFCW)+TLUP(IFCP)-SFCI                                 00678000
      D(IGA+63) = OBAE+BAES                                             00679000
      D(IGA+64) = ESHP+OSHP                                             00680000
      SFC = FC/THR+TLUP(IFCW)+TLUP(IFCP)-SFCI                           00690000
      SFCSAV = SFC                                                      00700000
      WTT = WTS+(GWHP+7.8225E-4*ELL)*POWES+(HWHP+3.75E-3*HLL)*POWHS     00710000
      WTT=MAX(WTT,.01)
      WTG=MAX(WTG,.01)
	X = ALOG(1.0/(1.0-(WTF+EXWT)/(WTG+WTT)))                        
	XR = 1.0-WTF/WTG                                                  00730000
      XA = ALOG(1.0/XR)                                                 00740000
      PSR = 100.0*(1.0-SFCR*DRAG*X/((SFC*(DRAG+SDRAG)                   00750000
     *        +60.0*EXWT/MSNTIM)*XA))                                   00760000
C$WAS*        +60.0*EXWT/TIME)*XA))                                     00770000
      SFC =SFC-FC/THR                                                   00780000
      DECS = SDRAG+(SFC-SFCR)*THR/SFCR                                  00790000
      PST = 100.0*(WTT/(XR*WTG)+XA*DECS/DRAG)                           00800000
      PSP = 100.0*(XR*XA*DECS/DRAG+WTT/WTG)/(XR-WTE/WTG)                00810000
      PSD = 100.0*(DECS+WTT/LDR)/DRAG                                   00820000
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX00830000
      WRITE (6  ,*) 'VEHICLE MISSION/CONFIGURATION INPUTS'              00840000
      WRITE (6  ,9010) MSNTIM, THR   , DRAG  , LDR   , WTE   , WTF      00850000
     *               , WTG   , ELL   , HLL   , GWHP  , HWHP  , OBAE     00860000
     *               , OSHP                                             00870000
 9010 FORMAT('  MSNTIM=',F10.2,   '  THR   =',F10.2,  '  DRAG  =',F10.2 00880000
     *     ,'  LDR   =',F10.2, /, '  WTE   =',F10.2,  '  WTF   =',F10.2 00890000
     *     ,'  WTG   =',F10.2, /, '  ELL   =',F10.2,  '  HLL   =',F10.2 00900000
     *     ,'  GWHP  =',F10.2,    '  HWHP  =',F10.2,  '  OBAE  =',F10.2 00910000
     *     ,'  OSHP  =',F10.2)                                          00920000
      WRITE (6  ,*) 'SUBSYSTEM ADDITIONAL INPUTS'                       00930000
      WRITE (6  ,9020) WTS   , POWEQQ, POWSS , POWHQQ, ESHP  , BAES     00940000
     *               , FC    , SDRAG , EXWT                             00950000
 9020 FORMAT('  WTS   =',F10.2,   '  POWES =',F10.2,  '  POWSS =',F10.2 00951000
     *     ,'  POWHS =',F10.2,    '  ESHP  =',F10.2,/,'  BAES  =',F10.2 00952000
     *     ,'  FC    =',F10.2,    '  SDRAG =',F10.2,  '  EXWT  =',F10.2)00953000
      WRITE (6  ,*) 'VEHICLE AND SUBSYSTEM OUTPUTS'                     00954000
      WRITE (6  ,9030) SFCI  , SFCR  , SFCSAV, SFC   , WTT   , X        00955000
     *               , XR    , XA    , DECS                             00956000
     *               , PST   , PSP   , PSR   , PSD                      00957000
 9030 FORMAT('  SFCI  =',F10.4,   '  SFCR  =',F10.4,  '  SFCECS=',F10.4 00957100
     *     ,'  SFC   =',F10.4, /, '  WTT   =',F10.4,  '  X     =',F10.4 00957200
     *     ,'  XR    =',F10.4,    '  XA    =',F10.4,  '  DECS  =',F10.4 00957300
     *   ,/,'  PST   =',F10.2,    '  PSP   =',F10.2,  '  PSR   =',F10.2 00957400
     *     ,'  PSD   =',F10.2)                                          00957500
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX00957600
      CALL LINES(12)                                                    00957700
      WRITE (OUT,2001) PST,PSP,PSR,PSD                                  00957800
 2001 FORMAT(12H0***********/12H *PENALTIES*/12H ***********/           00957900
     *1H0,5X,37HRELATIVE GROSS TAKEOFF WEIGHT PENALTY,5X,F5.2,          00958000
     *8H PERCENT/                                                       00959000
     *1H0,5X,24HRELATIVE PAYLOAD PENALTY,18X,F5.2,                      00960000
     *8H PERCENT/                                                       00970000
     *1H0,5X,22HRELATIVE RANGE PENALTY,20X,F5.2,                        00980000
     *8H PERCENT/                                                       00990000
     *1H0,5X,32HRELATIVE EQUIVALENT DRAG PENALTY,10X,F5.2,              01000000
     *8H PERCENT)                                                       01010000
   20 CONTINUE                                                          01020000
   99 CONTINUE                                                          01030000
      CALL LINES(2)                                                     01040000
      WRITE (OUT,1006) CERR                                             01050000
 1006 FORMAT(1H0,I6,18H ERROR(S) DETECTED)                              01060000
      RETURN                                                            01070000
C     OUTPTB                                                            01080000
      END                                                               01090000
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS THE PARAMETER TABLE CARDS                            
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE PARAMR                                                         
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,   C(7)),                                                
     *            (LC,    C(13)),                                               
     *            (CERR,  C(16)),                                               
     *            (IAUR,  C(19)),                                               
     *            (BIG,   C(31)),                                               
     *            (NOV,   C(32)),                                               
     *            (IPAR,  C(33)),                                               
     *            (MDPC,  C(34)),                                               
     *            (ICV(1),C(133))                                               
      DIMENSION ICV(18)                                                         
      INTEGER CERR, OUT                                                         
      DIMENSION CD(200)                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (PAREND,CD(13)), (VALU,CD(37)), (CN,CD(84))                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION VALUE(5)                                                        
                                                                                
      COMMON /DB/ DEBUG                                                         
      LOGICAL DEBUG                                                             
      REAL*8    CD                                                      00092240
      REAL*8    PAREND, VALU, CN                                        00092250                                                                                
      IF (DEBUG) THEN                                                           
         WRITE(23,7601)                                                         
 7601    FORMAT(' ------ ENTERING SUBROUTINE PARAMR ------ ')                   
      ENDIF                                                                     
                                                                                
C                                                                               
C  SET IPRNT TO EQUAL THIRD LEAST SIGINIFICANT DIGIT                            
C                             = 0 PRINT PARAMETER DATA                          
C                             = 1 DO NOT PRINT PARAMETER DATA                   
C                                                                               
      IPRNT = 100*(MDPC/100)                                                    
      IPRNT = IPRNT-1000*(IPRNT/1000)                                           
C                                                                               
C  IF PRINT CONTROL IS NEGATIVE SET IT TO -1                                    
C                                                                               
      IF (MDPC.LT.0) IPRNT = -1                                                 
C                                                                               
C  READ PARAMETER CARD AND THE NUMBER OF PARAMETERS                             
C                                                                               
      READ (IAUR,1000) CN,NOV                                                   
 1000 FORMAT (A6,I4)                                                            
C                                                                               
C IF NUMBER OF PARAMETERS IS NEGATIVE THEN SET NUMBER TO 1                      
C                                                                               
      IF (NOV.LE.0) NOV = 1                                                     
C                                                                               
C                                                                               
      IF (IPRNT.EQ.0) THEN                                                      
         CALL LINES(100)                                                        
         LC = LC+1                                                              
         WRITE (OUT,1005) NOV                                                   
 1005    FORMAT(16H PARAMETER TABLE,I10,9H VALUE(S))                            
      ENDIF                                                                     
                                                                                
      CALL GDCU(NOV,4,1,D,IPAR)                                                 
C                                                                               
C  SET THE D VALUES EQUAL TO 1.0E75 IE. A VERY LARGE NUMBER                     
C                                                                               
      DO 10 I=1,NOV                                                             
   10 D(IPAR+I) = BIG                                                           
C                                                                               
C  READ A VALUE CARD, TITLE, PARAMETER INDEX AND UP TO 18 VALUES                
C                                                                               
   11 READ (IAUR,1002) CN,IX,ICV                                                
 1002 FORMAT(A6,I4,17A4,A2)                                                     
C                                                                               
C  IF CARD IS A 'PAREND' CARD, IT IS THE LAST                                   
C                                                                               
      IF (CN.EQ.PAREND) THEN                                                    
         IF (IPRNT.NE.0) RETURN                                                 
         CALL LINES(1)                                                          
         WRITE (OUT,1001)                                                       
 1001    FORMAT(1H )                                                            
         CALL PARNTH(NOV,D(IPAR+1))                                             
         RETURN                                                                 
      ENDIF                                                                     
                                                                                
C                                                                               
C  THE PARAMETER INDEX MUST BE BETWEEN 1 AND NOV, OR ERROR                      
C                                                                               
      IF (IX.LT.1.OR.IX.GT.NOV) THEN                                            
         CERR = CERR+1                                                          
         CALL LINES(2)                                                          
         WRITE (OUT,1006) CERR,CN,IX,ICV                                        
 1006    FORMAT(6H0ERROR,I6,5X,22HPARAMETER INDEX WRONG ,A6,I4,17A4,A2)         
         GO TO 11                                                               
      ENDIF                                                                     
C                                                                               
C  BACKSPACE TO READ THE VALUES INTO DECIMAL FORMAT                             
C                                                                               
      BACKSPACE IAUR                                                            
C                                                                               
C  IF IT IS A "VALUE" CARD, READ THE SINGLE VALUE                               
C             "VALUES"      READ FIVE VALUES                                    
C                                                                               
      IF (CN.EQ.VALU) THEN                                                      
         READ (IAUR,1004) CN,IX,VALUE(1)                                        
         IJ = 1                                                                 
      ELSE                                                                      
         READ (IAUR,1004) CN,IX,VALUE                                           
 1004    FORMAT (A6,I4,5E10.4)                                                  
         IJ = 5                                                                 
         IF ((IX+4).GT.NOV) IJ = NOV-IX+1                                       
      ENDIF                                                                     
                                                                                
   14 CONTINUE                                                                  
C                                                                               
C CALCULATE WHERE TO STORE THE PARAMETER VALUES                                 
C                                                                               
      IK = IPAR+IX-1                                                            
C                                                                               
C  STORE THE VALUES IN THE "D" ARRAY                                            
C                                                                               
      DO 24 I=1,IJ                                                              
   24 D(IK+I) = VALUE(I)                                                        
C                                                                               
C GO READ ANOTHER PARAMETER CARD                                                
C                                                                               
      GO TO 11                                                                  
C   PARAMR
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE OUTPUTS REAL NUMBERS IN E FORMAT WITH INCESES IN           
C       PARENTHESES                                                             
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE PAREN (NUM,IN,VAL)                                             
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7))                                                    
      INTEGER OUT                                                               
      DIMENSION CD(200)                                                         
      DIMENSION IN(1), VAL(1)                                                   
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (BLANKS,CD(23))
      REAL*8 cd, blanks                                               
      L = 1                                                                     
    2 CONTINUE                                                                  
      DO 1 I=1,NUM,6                                                            
      J = I+5                                                                   
      IF (J.GT.NUM) J = NUM                                                     
      CALL LINES(1)                                                             
      IF (L.EQ.4) GO TO 40                                                      
      IF (L-2) 10,20,30                                                         
   10 CONTINUE                                                                  
      WRITE (OUT,1000) (BLANKS,IN(K),VAL(K),K=I,J)                              
c1000 FORMAT(6(A1,1H(,I4,1H),1PE12.5)) !orig
 1000 FORMAT(6(A1,'(', I4, ')', f14.5)) ! writes WPTH blocks 
      GO TO 1                                                                   
   20 CONTINUE                                                                  
C$      WRITE (OUT,1001) (BLANKS,IN(K),VAL(K),K=I,J)                              
C$ 1001 FORMAT(6(A1,1H(,I4,1H),F8.3))                                             
      WRITE (OUT,1001) (IN(K),VAL(K),K=I,J)                              
c 1001 FORMAT(1H ,6(1H(,I4,1H),F9.3))   !was 
 1001 FORMAT(1H ,6('(', I4, ')', F9.3))                                             
      GO TO 1                                                                   
   30 CONTINUE                                                                  
C$      WRITE (OUT,1002) (BLANKS,IN(K),VAL(K),K=I,J)                              
C$ 1002 FORMAT(6(A1,1H(,I4,1H),F8.4))  !FIXED ON 1/5/00                                           
      WRITE (OUT,1002) (IN(K),VAL(K),K=I,J)                              
c 1002 FORMAT(1H ,6(1H(,I4,1H),F9.4))  !was til June01 
 1002 FORMAT(1H ,6('(', I4, ')', F9.4))                                             
      GO TO 1                                                                   
   40 CONTINUE                                                                  
C$ WRITE WALL TEMPS                                                             
C$      WRITE (OUT,1003) (BLANKS,IN(K),VAL(IN(K)),K=I,J)                          
C$ 1003 FORMAT(6(A1,1H(,I4,1H),F8.3))                                             
      WRITE (OUT,1003) (IN(K),VAL(IN(K)),K=I,J)                          
 1003 FORMAT(1H ,6('(', I4, ')', F9.3))                                             
    1 CONTINUE                                                                  
      RETURN                                                                    
      ENTRY PARNF2 (NUM,IN,VAL)                                                 
      L = 2                                                                     
      GO TO 2                                                                   
      ENTRY PARNF5 (NUM,IN,VAL)                                                 
      L = 3                                                                     
      GO TO 2                                                                   
      ENTRY PARNF9 (NUM,IN,VAL)                                                 
      L = 4                                                                     
      GO TO 2                                                                   
C     PAREN, PARNF2, PARNF5                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE  OUTPUT REAL NUMBERS IN E FORMAT WITH INDEXES              
C      IN PARENTHESES, USED TO OUTPUT THE PARAMETERS 6 PER LINE                 
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE PARNTH (NO,VAL)                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7))                                                    
      INTEGER OUT                                                               
      DIMENSION CD(200)                                                         
      REAL*8  CD, BLANKS                                                      
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (BLANKS,CD(23))                                               
      DIMENSION VAL(1)                                                          
                                                                                
      COMMON /DB/ DEBUG                                                         
      LOGICAL DEBUG                                                             
                                                                                
      IF (DEBUG) THEN                                                           
         WRITE(23,7601)                                                         
 7601    FORMAT(' ------ ENTERING SUBROUTINE PARNTH ------ ')                   
      ENDIF                                                                     
                                                                                
      DO 1 I=1,NO,6                                                             
         J = I+5                                                                
         IF (J.GT.NO) J = NO                                                    
         CALL LINES(1)                                                          
         WRITE (OUT,1000) (BLANKS,K,VAL(K),K=I,J)                               
c 1000    FORMAT(6(A1, '(', I4, ')', 1PE15.8)) !ORIGINAL 
 1000    FORMAT(6(A1, '(' ,I4, ')', f15.8)) ! writes SV's and EV's in dump mode at least 
    1 CONTINUE                                                               
      RETURN                                                                    
C   PARNTH
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS PERFORMANCE CASE DATA CARDS                          
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE PCASER                                                         
                                                                                
      COMMON /CCA/ CA(150)                                                      
      EQUIVALENCE (IOPC,  CA(3)),                                               
     *            (IATMH, CA(4))                                                
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE                                                               
     *            (OUT,    C(7)),                                               
     *            (LC,     C(13)),                                              
     *            (IAUR,   C(19)),                                              
     *            (NLEG,   C(25)),                                              
     *            (NSTA,   C(26)),                                              
     *            (IW,     C(27)),                                              
     *            (IP,     C(28)),                                              
     *            (IT,     C(29)),                                              
     *            (IH,     C(30)),                                              
     *            (BIG,    C(31)),                                              
     *            (MDPC,   C(34)),                                              
     *            (IGA,    C(35)),                                              
     *            (IATMP,  C(84)),                                              
     *            (IATMT,  C(85)),                                              
     *            (IVM,    C(86)),                                              
     *            (IAP,    C(87)),                                              
     *            (ICPP,   C(88)),                                              
     *            (IWPT,   C(89)),                                              
     *            (SCR(1), C(151))                                              
                                                                                
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE (SCR(1),ALT),                                                 
     *            (SCR(2),PAMB),                                                
     *            (SCR(2),PAMB),                                                
     *            (SCR(4),VOM),                                                 
     *            (SCR(5),IATMPC),                                              
     *            (SCR(6),IATMTC),                                              
     *            (SCR(7),HAMB)                                                 
                                                                                
      DIMENSION CD(200)                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CASN,CD(82)), (CN,CD(84))                                    
      COMMON /DC/ DZ(2),D(128001)                                               
                                                                                
      COMMON /DB/ DEBUG                                                         
      LOGICAL DEBUG
      REAL*8 CD, CASN, CN   !8/98 OR EARLIER                                               
                                                             
                                                                                
      IF (DEBUG) THEN                                                           
         WRITE(23,7601)                                                         
 7601    FORMAT(' ------ ENTERING SUBROUTINE PCASER ------ ')                   
      ENDIF                                                                     
                                                                                
C                                                                               
C  READ CASEA CARD                                                              
C                    CN - "CASEA"                                               
C                  CASN - CASE NAME                                             
C                  MDPC = INPUT DATA PRINT CONTROL, IJKL                        
C                          I = 0 PRINT CASE DATA                                
C                          J = 0 PRINT PARAMETER DATA                           
C                          K = 0 PRINT COMPONENT DATA                           
C                          L = 0 PRINT TABLE DATA                               
C                  NLEG - NUMBER OF LEGS IN SYSTEM                              
C                  NSTA - NUMBER OF STATIONS IN MODEL                           
C                  IOPC - = 0 PRINT COMPONENT PERFORMANCE                       
C                                                                               
C                                                                               
C                                                                               
      READ (IAUR,1001) CN,CASN,MDPC,NLEG,NSTA,IOPC                              
 1001 FORMAT(A6,2X,A8,16I4)                                                     
C                                                                               
C  READ CASEB CARD                                                              
C                    CN - "CASEB"                                               
C                IATMPC - FREESTREAM PRESSURE TABLE RELATIVE NUMBER             
C                         (TYPE 14)                                             
C                IATMTC - FREESTREAM TEMPERATURE TABLE RELATIVE NUMBER          
C                         (TYPE 15)                                             
C                   IVM - VELOCITY OR MACH NUMBER OPTION                        
C                         = -1, INPUT VELOCITY ON CASED CARD                    
C                         = 0,  INPUT NOT REQUIRED                              
C                         = 1,  INPUT MACH NUMBER ON CASED CARD                 
C                   IAP - FREESTREAM FLUID PROPERTY TABLE RELATIVE              
C                         NUMBER (TYPE 10)                                      
C                  IWPT - WATER PROPERTY TABLE RELATIVE NUMBER                  
C                         (TYPE 10), WET AIR ONLY                               
C                IATMHC - HUMIDITY PROPERTY TABLE RELATIVE NUMBER               
C                                                                               
      READ(IAUR,1002) CN,IATMPC,IATMTC,IVM,IAP,IWPT,IATMHC                      
 1002 FORMAT(A6,2X,18I4)                                                        
C                                                                               
C  READ CASEC CARD, ALTITUDE, AMBINET PRESSURE, AMBIENT TEMPERATURE AND         
C              ALT - ALTITUDE                                                   
C             PAMB - AMBINET PRESSURE (IF IPA = 0)                              
C             TAMB - AMBIENT TEMPERATURE (IF ITA = 0)                           
C             HAMB - AMBINET HUMIDITY (IF IATMHC = 0)                           
C                                                                               
      READ (IAUR,1003) CN,ALT,PAMB,TAMB,HAMB                                    
 1003 FORMAT(A6,4X,7E10.0)                                                      
C                                                                               
C  READ CASED CARD, VELOCITY,FT/SEC. OR MACH NUBMER                             
C                                                                               
      READ (IAUR,1003) CN,VOM                                                   
C                                                                               
C  STRIP LOW ORDER DECIMAL DIGIT IE. IF IOPC = 124 ICPP = 4                     
C                                                                               
      ICPP = IOPC-10*(IOPC/10)                                                  
C                                                                               
C  OBTAIN HIGH ORDER (1000'S LEVEL DECIMAT DIGIT TO INDICATE                    
C    PRINT OF CASE DATA, 0 PRINT, 1 NO PRINT                                    
C                                                                               
      IPRNT = MDPC/1000                                                         
                                                                                
      IF (MDPC.LT.0) IPRNT = -1                                                 
C                                                                               
C IF THE NUMBER OF LEGS IS NEGATIVE, SET TO 1                                   
C                                                                               
      IF (NLEG.LT.1) NLEG = 1                                                   
C                                                                               
C  IF NUMBER OF STATIONS ARE NEGATIVE, SET TO 1                                 
C                                                                               
      IF (NSTA.LT.1) NSTA = 1                                                   
                                                                                
      IATMP = IATMPC                                                            
      IATMT = IATMTC                                                            
      IATMH = IATMHC                                                            
      IF (IAP.NE.0) CALL FLDZF(0,2,IAP)                                         
      D(IGA+2) = ALT                                                            
      D(IGA+5) = PAMB                                                           
      D(IGA+6) = TAMB                                                           
      D(IGA+9) = HAMB                                                           
                                                                                
      IF (IATMP.NE.0) IATMP = ITIDN(IATMP,14)                                   
      IF (IATMT.NE.0) IATMT = ITIDN(IATMT,15)                                   
      IF (IATMH.NE.0) IATMH = ITIDN(IATMH,16)                                   
      D(IGA+3) = BIG                                                            
      D(IGA+4) = BIG                                                            
C                                                                               
C IF VELOCITY/MACH NUMBER OPTION = -1 VELOCITY ON CASED CARD                    
C                                   0 NOT NEDED                                 
C                                   1 MACH NUMBER ON CASED CARD                 
      IF (IVM.LT.0) D(IGA+4) = VOM                                              
      IF (IVM.GT.0) D(IGA+3) = VOM                                              
                                                                                
C                                                                               
C  PRINT CASE DATA                                                              
C                                                                               
      IF (IPRNT.EQ.0) THEN                                                      
         CALL LINES(100)                                                        
         LC = LC+2                                                              
         WRITE (OUT,1004) CASN,NLEG,NSTA                                        
 1004 FORMAT(1H0,10X,5HCASE ,A8,10X,I4,7H LEG(S),10X,I4,11H STATION(S))         
C                                                                               
C  IF THERE IS ON AMBIENT PRESSURE TABLE, THEN PRINT AMBEINT PRESSURE           
C                             FROM CASEC CARD                                   
C                           ELSE PRINT TABLE RELATIVE NUMBER                    
C                                                                               
         IF (IATMP.EQ.0) THEN                                                   
            LC = LC+2                                                           
            WRITE (OUT,1005) PAMB                                               
 1005       FORMAT(7H0ATM P ,F8.2)                                              
         ELSE                                                                   
            LC = LC+2                                                           
            WRITE (OUT,1006) IATMPC                                             
 1006       FORMAT(13H0ATM P TABLE ,I6)                                         
         ENDIF                                                                  
                                                                                
C                                                                               
C  IF THERE IS ON AMBIENT TEMPERATUER TABLE, THEN PRINT AMBEINT                 
C                             TEMPERATURE FROM CASEC CARD                       
C                           ELSE PRINT TABLE RELATIVE NUMBER                    
C                                                                               
    5    IF (IATMT.EQ.0) THEN                                                   
            LC = LC+2                                                           
            WRITE (OUT,1007) TAMB                                               
 1007       FORMAT(7H0ATM T ,F8.2)                                              
         ELSE                                                                   
            LC = LC+2                                                           
            WRITE (OUT,1008) IATMTC                                             
 1008       FORMAT(13H0ATM T TABLE ,I6)                                         
         ENDIF                                                                  
                                                                                
C                                                                               
C  IF THERE IS ON AMBIENT HUMIDITY TABLE, THEN PRINT AMBEINT                    
C                             HUMIDITY FROM CASEC CARD                          
C                           ELSE PRINT TABLE RELATIVE NUMBER                    
C                                                                               
    7    IF (IATMH.EQ.0) THEN                                                   
            LC = LC+2                                                           
            WRITE (OUT,1011) HAMB                                               
 1011       FORMAT(7H0ATM H ,F8.5)                                              
         ELSE                                                                   
            LC = LC+2                                                           
            WRITE (OUT,1012) IATMHC                                             
 1012       FORMAT (13H0ATM H TABLE ,I6)                                        
         ENDIF                                                                  
                                                                                
C                                                                               
C  IF AMBIENT PROPERTY TABLE EXISTS THEN PRINT                                  
C                                                                               
         IF (IAP.NE.0) THEN                                                     
            LC = LC+2                                                           
            WRITE (OUT,1009) IAP                                                
 1009       FORMAT(16H0ATM PROP TABLE ,I6)                                      
         ENDIF                                                                  
                                                                                
C                                                                               
C PRINT ALTITUDE                                                                
C                                                                               
         LC = LC+2                                                              
         WRITE (OUT,1010) ALT                                                   
 1010    FORMAT(10H0ALTITUDE ,F10.0)                                            
C                                                                               
C PRINT VELOCITY IF IVM = -1, MACH NUMBER IF IVM = 1                            
C                                                                               
         IF (IVM.LT.0) THEN                                                     
            LC = LC+2                                                           
            WRITE (OUT,1021) VOM                                                
 1021       FORMAT(10H0VELOCITY ,F10.0)                                         
         ENDIF                                                                  
                                                                                
         IF (IVM.GT.0) THEN                                                     
            LC = LC+2                                                           
            WRITE (OUT,1022) VOM                                                
 1022       FORMAT(13H0MACH NUMBER ,F6.3)                                       
         ENDIF                                                                  
                                                                                
   12    IF (IWPT.NE.0) THEN                                                    
            LC = LC+2                                                           
            WRITE (OUT,1023) IWPT                                               
 1023       FORMAT(18H0WATER PROP TABLE ,I6)                                    
         ENDIF                                                                  
                                                                                
      ENDIF                                                                     
                                                                                
      CALL GDCU(NLEG,4,0,D,IW)                                                  
      CALL GDCU(NSTA,4,0,D,IP)                                                  
      CALL GDCU(NSTA,4,0,D,IT)                                                  
      CALL GDCU(NSTA,4,0,D,IH)                                                  
C                                                                               
C  READ CASEND CARD                                                             
C                                                                               
      READ (IAUR,1000) CN                                                       
 1000 FORMAT(A6)                                                                
      RETURN                                                                    
C     PCASER                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE OUTPUTS THE COMPONENT PERFORMANCE                          
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE PIOP (IC,NL,NSI,NSO)                                           
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,  C(7)),                                                 
     *            (IW,   C(27)),                                                
     *            (IP,   C(28)),                                                
     *            (IT,   C(29)),                                                
     *            (IH,   C(30)),                                                
     *            (ILN,  C(37)),                                                
     *            (ISN,  C(38)),                                                
     *            (IRCD, C(45)),                                                
     *            (ICDB, C(43)),                                                
     *            (IFB,  C(55))                                                 
      INTEGER OUT                                                               
      DIMENSION CD(200)                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84))                                                   
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
                                                                                
      COMMON /CCA/ CA(150)                                                      
      EQUIVALENCE  (NOC,CA(1))
      REAL*8 cd, cn                                             
                                                  
                                                                                
      LOC = ID(ICDB+NOC)                                                        
      NC = ID(LOC-1)                                                            
      IF (IC.EQ.2 .OR. IC.EQ.-2) GO TO 10                                       
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CN,NC                                                    
 1000 FORMAT (11H0COMPONENT ,A6,2X,I4)                                          
      IF (IC.EQ.0) RETURN                                                       
   10 CONTINUE                                                                  
      IFBL = ID(IFB+NL)                                                         
                                                                                
      IF (IC.LE.0) THEN                                                         
         CALL LINES(2)                                                          
         WRITE (OUT,1002) ID(ILN+NL),ID(ISN+NSI),ID(IFBL+1),ID(IFBL+2)          
 1002    FORMAT(1H0,5X,2HNL,I8,3X,2HNS,I8,3X,2HFT,I8,3X,2HFN,I8)                
         CALL LINES(2)                                                          
         WRITE (OUT,1004) D(IW+NL),D(IP+NSI),D(IT+NSI),D(IH+NSI)                
 1004    FORMAT(1H0,5X,1HW,F9.2,3X,1HP,F9.2,3X,1HT,F9.2,3X,1HH,F9.4)            
         RETURN                                                                 
      ENDIF                                                                     
                                                                                
      CALL LINES(2)                                                             
      WRITE (OUT,1001) ID(ILN+NL),ID(ISN+NSI),ID(ISN+NSO),ID(IFBL+1)            
     *,ID(IFBL+2)                                                               
 1001 FORMAT(1H0,5X,2HNL,I8,3X,3HNSI,I7,3X,3HNSO,I7,3X,2HFT,I8,3X,2HFN,         
     *I8)                                                                       
      CALL LINES(2)                                                             
      WRITE (OUT,1005) D(IW+NL),D(IP+NSI),D(IP+NSO),D(IT+NSI),D(IT+NSO)         
     *,D(IH+NSI),D(IH+NSO)                                                      
 1005 FORMAT(1H0,5X,1HW,F9.2,3X,2HPI,F8.2,3X,2HPO,F8.2,3X,2HTI,F8.2,3X,         
     *2HTO,F8.2,3X,2HHI,F8.4,3X,2HHO,F8.4)                                      
      RETURN
C   PIOP                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES POLYNOMIAL FOR PROPERTY EVALATION                 
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION POLYI (N,C,X)                                                    
      DIMENSION C(1)                                                            
      M = N+1                                                                   
      POLYI = C(M)                                                              
      DO 1 I=1,N                                                                
    1 POLYI = POLYI*X+C(M-I)                                                    
      RETURN                                                                    
C     POLYI                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE OUTPUTS SIZING PERFORMANCE SPECIFICATIONS                  
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SCI (NL,NSI,NSO)                                               
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7)), (IW,C(27)), (IP,C(28)), (IT,C(29))                
     *, (IH,C(30)), (IFB,C(55))                                                 
      INTEGER OUT                                                               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      IFBL = ID(IFB+NL)                                                         
      CALL LINES(2)                                                             
      WRITE (OUT,1000) D(IW+NL),D(IP+NSI),D(IP+NSO),D(IT+NSI),D(IT+NSO)         
     *,D(IH+NSI),D(IH+NSO),ID(IFBL+1),ID(IFBL+2)                                
 1000 FORMAT(1H0,5X,1HW,F9.2,3X,2HPI,F8.2,3X,2HPO,F8.2,3X,2HTI,F8.2,3X,         
     *2HTO,F8.2,3X,2HHI,F8.4,3X,2HHO,F8.4,3X,2HFT,I8,3X,2HFN,I8)                
      RETURN                                                                    
C     SCI                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE OUTPUTS COMPONENT SIZE                                     
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SCO                                                            
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7)), (WTC,C(102)), (CUC,C(104)), (RIC,C(106))          
     *, (DRC,C(108)), (IRCD,C(45))                                              
      INTEGER OUT                                                               
      DIMENSION CD(200)                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84))                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))
      REAL*8 cd, CN                                                  
      IE = 1                                                                    
      GO TO 1                                                                   
      ENTRY SCOA                                                                
      IE = 2                                                                    
    1 CONTINUE                                                                  
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CN                                                       
 1000 FORMAT(11H0COMPONENT ,A6)                                                 
      IF (IE.EQ.2) GO TO 99                                                     
      CALL LINES(2)                                                             
      WRITE (OUT,1001) WTC,CUC,RIC,DRC                                          
 1001 FORMAT(1H0,5X,7HWEIGHT ,F7.2,5X,11HCOST UNITS ,F7.2,5X,18HRELIABIL        
     *ITY INDEX ,F7.5,5X,17HDEVELOPMENT RISK ,F5.2)                             
   99 CONTINUE                                                                  
      RETURN                                                                    
C     SCO                                                                       
      END                                                                       
      FUNCTION SHP (NL,P,T,H)                                                   
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFB,C(55)), (TL,C(81))                                       
     *, (BIG,C(31)), (CPV,C(132))                                               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      LOC = ID(IFB+NL)                                                          
      IFT = ID(LOC+1)                                                           
      IN = ID(LOC+3)                                                            
      TA = T                                                                    
      IF (TA.LT.TL) TA = TL                                                     
      IF (IFT-2) 1,2,3                                                          
    1 CONTINUE                                                                  
      SHP = POLYI(2,D(IN+4),TA)                                                 
      GO TO 99                                                                  
    2 CONTINUE                                                                  
      SHP = (POLYI(5,D(IN+2),TA) + CPV*H) / (1.0+H)                             
      GO TO 99                                                                  
    3 CONTINUE                                                                  
      SHP = BIG                                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     SHP                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES STANDARD DENSITY RATIO FOR LIQUIDS AND            
C      GASES                                                                    
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION SIG (NL,P,T)                                                     
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFB,C(55)), (PL,C(80)), (TL,C(81))                           
     *, (BIG,C(31))                                                             
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      LOC = ID(IFB+NL)                                                          
      IFT = ID(LOC+1)                                                           
      IN = ID(LOC+3)                                                            
      TA = T                                                                    
      IF (TA.LT.TL) TA = TL                                                     
      IF (IFT-2) 1,2,3                                                          
    1 CONTINUE                                                                  
      SIG = POLYI(2,D(IN+1),TA)/D(LOC+4)                                        
      GO TO 99                                                                  
    2 CONTINUE                                                                  
      PA = P                                                                    
      IF (PA.LT.PL) PA = PL                                                     
      SIG = PA/(D(IN+1)*TA*D(LOC+4))                                            
      GO TO 99                                                                  
    3 CONTINUE                                                                  
      SIG = BIG                                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     SIG                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CONTROLS THE PERFORMANCE ITERATION SOLUTION                
C                                                                               
C                                                                               
C$  ALL NEW PER COSTELLO                                                        
C**********************************************************************         
C                                                                               
      SUBROUTINE SOLN1                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IDS,    C(5)),                                               
     *            (OUT,    C(7)),                                               
     *            (CERR,   C(16)),                                              
     *            (PASS,   C(17)),                                              
     *            (IFP,    C(22)),                                              
     *            (NLEG,   C(25)),                                              
     *            (NSTA,   C(26)),                                              
     *            (IW,     C(27)),                                              
     *            (IP,     C(28)),                                              
     *            (IT,     C(29)),                                              
     *            (IH,     C(30)),                                              
     *            (BIG,    C(31)),                                              
     *            (IGA,    C(35)),                                              
     *            (ILN,    C(37)),                                              
     *            (ISN,    C(38)),                                              
     *            (ISP,    C(42)),                                              
     *            (ISVT,   C(46)),                                              
     *            (ISV,    C(47)),                                              
     *            (IEVT,   C(48)),                                              
     *            (IEV,    C(49)),                                              
     *            (NSV,    C(50)),                                              
     *            (NEV,    C(51)),                                              
     *            (ITAD,   C(54)),                                              
     *            (IMP,    C(66)),                                              
     *            (IME,    C(67)),                                              
     *            (NIT,    C(76)),                                              
     *            (PF,     C(77)),                                              
     *            (NWT,    C(78)),                                              
     *            (ICONV,  C(79)),                                              
     *            (ERRL,   C(83)),                                              
     *            (IIOP,   C(90)),                                              
     *            (ISVS,   C(92)),                                              
     *            (SVLL(1),C(181)),                                             
     *            (SVUL(1),C(191)),                                             
     *            (MRD,    C(372)),                                             
     *            (MRDNIT, C(373))                                              
      INTEGER OUT, CERR, PASS, IRETRN                                           
                                                                                
      COMMON /CCA/ CA(150)                                                      
      EQUIVALENCE (LIST(1),CA(11)),(SVSCLE(1),CA(31))                           
     *, (EVSCLE(1),CA(51)),(DF(1),CA(71)),(DFO(1),CA(91))                       
     *, (ITER,CA(111)),(RST,CA(112)),(STEP,CA(113)),(IEVS,CA(122))              
     *, (ESUM,CA(114)),(DSUM,CA(115)),(SUM,CA(116)),(ITBL50,CA(117))            
     *, (ITBL51,CA(118)),(NORD,CA(119)),(KVAR,CA(120)),(IDONE,CA(121))          
     *, (IWS,CA(130)),(IPS,CA(131)),(ITS,CA(132)),(IHS,CA(133))                 
C$ FOR WALL TEMP                                                                
C$   *, (ITWS,CA(134))                                                          
      DIMENSION LIST(20),DF(20),DFO(20),SVSCLE(20),EVSCLE(20)                   
      DIMENSION SVLL(10), SVUL(10)                                              
      DIMENSION CD(200)                                                         
      REAL*8  CD                                                              
       REAL*8  SN, CN                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84))                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA SN/5HSOLN1/                                                          
                                                                                
      CN = SN                                                                   
      ITER = 0                                                                  
      IDSS = IDS                                                                
      NUM = NSV*NSV                                                             
      IF (ISV.EQ.0) THEN                                                        
         CALL GDCU(NSV,4,ISP,D,ISV)                                             
         CALL GDCU(NSV,4,ISP,D,ISVT)                                            
         CALL GDCU(NEV,4,ISP,D,IEV)                                             
         CALL GDCU(NEV,4,ISP,D,IEVT)                                            
         CALL GDCU(NSV,4,ISP,D,ISVS)                                            
         CALL GDCU(NEV,4,ISP,D,IEVS)                                            
         CALL GDCU(NLEG,4,ISP,D,IWS)                                            
         CALL GDCU(NSTA,4,ISP,D,IPS)                                            
         CALL GDCU(NSTA,4,ISP,D,ITS)                                            
         CALL GDCU(NSTA,4,ISP,D,IHS)                                            
         CALL GDCU(NUM,4,ISP,D,IMP)                                             
         CALL GDCU(NSV,4,ISP,D,IME)                                             
         CALL GDCU(NSV,4,ISP,D,IEM)                                             
         WRITE(6,111)SVSCLE,STEP,RST                                            
  111    FORMAT('SVSCLE'/(4E20.8),F8.4,F8.4)                                    
C                                                                               
C    IF MRD WAS SELECTED, CALL THE SOLNPP COMPONENT                             
C                                                                               
         IF(MRD.GT.0) CALL SOLNPP                                               
                                                                                
      ELSE                                                                      
                                                                                
         DO 51 I=1,NSV                                                          
            ID(ISV+I) = 0                                                       
            ID(ISVT+I) = 0                                                      
            ID(IEV+I) = 0                                                       
            ID(IEVT+I) = 0                                                      
            ID(ISVS+I) = 0                                                      
            ID(IEVS+I) = 0                                                      
            ID(IME+I) = 0                                                       
            ID(IEM+I) = 0                                                       
   51    CONTINUE                                                               
                                                                                
         DO 52 I=1,NLEG                                                         
            ID(IWS+I) = 0                                                       
   52    CONTINUE                                                               
                                                                                
         DO 53 I=1,NSTA                                                         
            ID(IPS+I) = 0                                                       
            ID(ITS+I) = 0                                                       
            ID(IHS+I) = 0                                                       
   53    CONTINUE                                                               
                                                                                
         DO 54 I=1,NUM                                                          
            ID(IMP+I) = 0                                                       
   54    CONTINUE                                                               
                                                                                
      ENDIF                                                                     
                                                                                
      IFP = 0                                                                   
      IIOP = 0                                                                  
      ICONV = -1                                                                
C                                                                               
C    IDONE TRACKS THE MRD SOLUTION TO SEE IF IT HAS CONVERGED                   
C                                                                               
      IDONE = 1                                                                 
C                                                                               
C    PERFORM AN INTITIAL SOLUTION TO THE COMPONENTS TO CORRECT INDATA           
C                                                                               
                                                                                
      CALL PCOMPP                                                               
                                                                                
      DO 42 I=1,NSV                                                             
                                                                                
         IF (D(ISV+I).LE.0.0) THEN                                              
            CERR = CERR+1                                                       
            CALL LINES(2)                                                       
            WRITE (OUT,1006) CERR,I                                             
 1006    FORMAT(6H0ERROR,I6,5X,15HSTATE VARIABLE ,I3,14H INITIAL VALUE)         
         ENDIF                                                                  
                                                                                
   42 CONTINUE                                                                  
                                                                                
      ASSIGN 5 TO IS                                                            
      GO TO 100                                                                 
    5 CONTINUE                                                                  
      ASSIGN 1 TO IS                                                            
      GO TO 102                                                                 
    1 CONTINUE                                                                  
                                                                                
      IF (CERR.NE.0) THEN                                                       
        IDS = IDSS                                                              
        RETURN                                                                  
      ENDIF                                                                     
                                                                                
      IF (NIT.EQ.0) THEN                                                        
        IDS = IDSS                                                              
        RETURN                                                                  
      ENDIF                                                                     
                                                                                
C*** SET THE WEIGHTING FACTOR = 1 (CHANGED BELOW IF NEWTON-RALPHSON IS          
C*** USED)                                                                      
      WT = 1.0                                                                  
      DO 10 II=1,NIT                                                            
      IF (IDS.LT.0) IDS = 2                                                     
      IF (IDSS.LT.0 .AND. II.GT.IABS(IDSS)) IDS = 0                             
      IF (II.EQ.NIT) IDS = 2                                                    
      D(IGA+1) = FLOAT(PASS)                                                    
      PASS = PASS+1                                                             
      IF (ID(ITAD+NWT).NE.0) WT = TLUP(NWT)                                     
                                                                                
      DO 2 I=1,NSV                                                              
         D(ISVS+I) = D(ISV+I)                                                   
    2 CONTINUE                                                                  
                                                                                
      DO 3 I=1,NLEG                                                             
         D(IWS+I) = D(IW+I)                                                     
    3 CONTINUE                                                                  
                                                                                
      DO 4 I=1,NSTA                                                             
         D(IPS+I) = D(IP+I)                                                     
         D(ITS+I) = D(IT+I)                                                     
         D(IHS+I) = D(IH+I)                                                     
    4 CONTINUE                                                                  
                                                                                
      CALL PCOMPP                                                               
      DO 88 I = 1,NSV                                                           
   88 D(IEVS+I) = D(IEV+I)                                                      
      ICONV = 1                                                                 
C*** START THE SOLUTION PROCESS: EITHER MRD OR NEWTON RAPHSON                   
  300 KVAR = 0                                                                  
  310 KVAR = KVAR+1                                                             
      IF(KVAR.GT.NSV) GO TO 11                                                  
      J = KVAR                                                                  
  312 IF(MRD.GT.0) J = LIST(KVAR)                                               
      IIOP = J                                                                  
  315 DO 12 K=1,NLEG                                                            
   12 D(IW+K) = D(IWS+K)                                                        
      DO 13 K=1,NSTA                                                            
      D(IP+K) = D(IPS+K)                                                        
      D(IT+K) = D(ITS+K)                                                        
C$    D(ITW+K) = D(ITWS+K)      !$                                              
   13 D(IH+K) = D(IHS+K)                                                        
C*** MAKE THE PERTURBATIONIN THE STATE VARIABLE                                 
      D(ISV+J) = D(ISVS+J)*PF                                                   
C*** SOLVE THE SYSTEM EQUATIONS                                                 
                                                                                
      CALL PCOMPP                                                               
      DSV = D(ISV+J)-D(ISVS+J)                                                  
      IF (CERR.NE.0) GO TO 99                                                   
      ASSIGN 22 TO IS                                                           
      IF (IDS.EQ.3) GO TO 101                                                   
   22 CONTINUE                                                                  
      ASSIGN 21 TO IS                                                           
      IF (IDS.EQ.3) GO TO 102                                                   
   21 CONTINUE                                                                  
      D(ISV+J) = D(ISVS+J)                                                      
      DO 14 K=1,NEV                                                             
C$ ADD SIX NEXT LINES                                                           
C***  EEEEEEEEEEEEEEEEEEEEEEEEEEEEE                                             
C     IEVK=IEV+K                                                                
C     IEVSK=IEVS+K                                                              
C     WRITE(6,112)PAR,IEVK,IEVSK,D(IEV+K),D(IEVS+K),DSV                         
C 112 FORMAT('PAR,IEV+K,IEVS+K,D(IEV+K),D(IEVS+K),DSV'/1E20.8,2I12/             
C    *        3E20.8)                                                           
C$                                                                              
C*** COMPUTE THE DERIVATIVES FROM THE RESULTS OF THE PERTURBATION               
      PAR = (D(IEV+K)-D(IEVS+K))/DSV                                            
      CALL MSTO(K,J,PAR)                                                        
      CALL MSTO(K,0,D(IEVS+K))                                                  
   14 CONTINUE                                                                  
C*** BRANCH ACCORDING TO WHICH SOLUTION OPTION SELECTED                         
      IF(MRD.EQ.0.OR.MRD.EQ.3) GO TO 310                                        
CCCCCCCCCCC                                                                     
C     IF(MRD.EQ.1) CALL MRDSLN RETURNS(315,330,41)                              
C     IF(MOD(KVAR,2).EQ.0) CALL MRDSLN RETURNS(315,330,312)                     
C     IF(KVAR.EQ.NSV) CALL MRDSLN RETURNS(315,330,312)                          
CCCCCCCCCCC                                                                     
C     IF(MRD.EQ.1) CALL MRDSLN (&315,&330,&41)                                  
C     IF(MOD(KVAR,2).EQ.0) CALL MRDSLN (&315,&330,&312)                         
C     IF(KVAR.EQ.NSV) CALL MRDSLN (&315,&330,&312)                              
CCCCCCCCCCC                                                                     
C     IF(MRD.EQ.1) CALL MRDSLN (*315,*330,*41)                                  
C     IF(MOD(KVAR,2).EQ.0) CALL MRDSLN (*315,*330,*312)                         
C     IF(KVAR.EQ.NSV) CALL MRDSLN (*315,*330,*312)                              
CCCCCCCCCCC                                                                     
      IF(MRD.NE.1) GO TO 9991                                                   
      CALL MRDSLN (IRETRN)                                                      
      IF (IRETRN.EQ.1) GO TO 315                                                
      IF (IRETRN.EQ.2) GO TO 330                                                
      IF (IRETRN.EQ.3) GO TO 41                                                 
 9991 IF(MOD(KVAR,2).NE.0) GO TO 9992                                           
      CALL MRDSLN (IRETRN)                                                      
      IF (IRETRN.EQ.1) GO TO 315                                                
      IF (IRETRN.EQ.2) GO TO 330                                                
      IF (IRETRN.EQ.3) GO TO 312                                                
 9992 IF(KVAR.NE.NSV) GO TO 9993                                                
      CALL MRDSLN (IRETRN)                                                      
      IF (IRETRN.EQ.1) GO TO 315                                                
      IF (IRETRN.EQ.2) GO TO 330                                                
      IF (IRETRN.EQ.3) GO TO 312                                                
 9993 CONTINUE                                                                  
CCCCCCCCCCC                                                                     
      D(ISV+J) = D(ISVS+J)                                                      
      GO TO 310                                                                 
CCCCCCCCCCC                                                                     
C  11 IF(MRD.GT.0) CALL MRDSLN RETURNS(315,330,312)                             
C  11 IF(MRD.GT.0) CALL MRDSLN (&315,&330,&312)                                 
C  11 IF(MRD.GT.0) CALL MRDSLN (*315,*330,*312)                                 
   11 IF(MRD.LE.0) GO TO 9994                                                   
      CALL MRDSLN (IRETRN)                                                      
      IF (IRETRN.EQ.1) GO TO 315                                                
      IF (IRETRN.EQ.2) GO TO 330                                                
      IF (IRETRN.EQ.3) GO TO 312                                                
 9994 CONTINUE                                                                  
CCCCCCCCCCC                                                                     
C*** SOLVE THE SYSTEM BY THE NEWTON-RAPHSON METHOD                              
      IF (IDS.GE.2) CALL MPRNT(D(IMP+1),NSV,D(IME+1))                           
      CALL MCHK(D(IMP+1),NSV,D(IME+1))                                          
      CALL MSOL(D(IMP+1),NSV,D(IME+1),D(IEM+1),IND)                             
      IF (IND.EQ.1) GO TO 41                                                    
      GO TO 40                                                                  
   41 CONTINUE                                                                  
      DO 16 J=1,NSV                                                             
      D(ISV+J) = D(ISVS+J) + WT*D(IMP+J)                                        
      K = ID(ISVT+J)                                                            
      IF (D(ISV+J).LT.SVLL(K)) D(ISV+J) = SVLL(K)                               
      IF (D(ISV+J).GT.SVUL(K)) D(ISV+J) = SVUL(K)                               
   16 CONTINUE                                                                  
  330 IF(IDS.LT.2) GO TO 165                                                    
      CALL LINES(2)                                                             
      WRITE(OUT,2165)                                                           
 2165 FORMAT(7H0ALTERS)                                                         
      CALL PARNTH(NSV,D(IMP+1))                                                 
C*** RECOMPUTE THE BASE POINT                                                   
  165 IIOP = 0                                                                  
      CALL PCOMPP                                                               
C*** BRANCH FOR THE EXTRA PRINTED OUTPUT                                        
      IF (CERR.NE.0) GO TO 99                                                   
      ASSIGN 17 TO IS                                                           
      IF (IDS.GE.1) GO TO 101                                                   
   17 CONTINUE                                                                  
      ASSIGN 20 TO IS                                                           
      IF (IDS.GE.1) GO TO 102                                                   
   20 CONTINUE                                                                  
      ICONV = 0                                                                 
      DO 15 J=1,NEV                                                             
      IF (ABS(D(IEV+J)).GT.ERRL) ICONV = ICONV+1                                
   15 CONTINUE                                                                  
      ITER = 0                                                                  
      IF (ICONV.EQ.0) GO TO 99                                                  
C*** IF NOT FINISHED GO BACK AND COMPLETE THE MRD ANALYSIS                      
      IF(KVAR.LT.NSV) GO TO 310                                                 
      IF(MRD.GT.0) ICONV = IDONE                                                
      IF(IDONE.EQ.0) GO TO 99                                                   
   10 CONTINUE                                                                  
   99 CONTINUE                                                                  
      IDS = IDSS                                                                
      RETURN                                                                    
   40 CONTINUE                                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1009) CERR,PASS                                                
 1009 FORMAT(6H0ERROR,I6,5X,22HSINGULAR MATRIX - PASS,I6)                       
      GO TO 99                                                                  
                                                                                
  100 CONTINUE                                                                  
                                                                                
      CALL LINES(2)                                                             
      WRITE (OUT,1001)                                                          
 1001 FORMAT(4H0SVT)                                                            
      CALL IPRNTH(NSV,D(ISVT+1))                                                
      CALL LINES(2)                                                             
      WRITE (OUT,1002)                                                          
 1002 FORMAT(4H0EVT)                                                            
      CALL IPRNTH(NEV,D(IEVT+1))                                                
  101 CONTINUE                                                                  
      CALL LINES(2)                                                             
      WRITE (OUT,1003) PASS                                                     
 1003 FORMAT(6H0PASS ,I6)                                                       
      CALL LINES(2)                                                             
      WRITE (OUT,1004)                                                          
 1004 FORMAT(5H0S.V.)                                                           
      CALL PARNTH(NSV,D(ISV+1))                                                 
      GO TO IS, (5,22,17)                                                       
                                                                                
  102 CONTINUE                                                                  
      CALL LINES(2)                                                             
      WRITE (OUT,1005)                                                          
 1005 FORMAT(5H0E.V.)                                                           
      CALL PARNTH(NEV,D(IEV+1))                                                 
      CALL LINES(2)                                                             
      WRITE (OUT,1101)                                                          
 1101 FORMAT(2H0W)                                                              
      CALL PAREN(NLEG,D(ILN+1),D(IW+1))                                         
      CALL LINES(2)                                                             
      WRITE (OUT,1102)                                                          
 1102 FORMAT(2H0P)                                                              
      CALL PAREN(NSTA,D(ISN+1),D(IP+1))                                         
      CALL LINES(2)                                                             
      WRITE (OUT,1103)                                                          
 1103 FORMAT(2H0T)                                                              
      CALL PAREN(NSTA,D(ISN+1),D(IT+1))                                         
      CALL LINES(2)                                                             
      WRITE (OUT,1104)                                                          
 1104 FORMAT(2H0H)                                                              
      CALL PAREN(NSTA,D(ISN+1),D(IH+1))                                         
C$ WRITE DUCT WALL TEMPS                                                        
C     CALL LINES(2)                                                             
C     WRITE (OUT,1105)                                                          
C1105 FORMAT(10H0WALL TEMP)                                                     
C     CALL PAREN(NSTA,D(ISN+1),D(ITW+1))                                        
C                                                                               
      GO TO IS, (1,20,21)                                                       
C     SOLN1                                                                     
      END                                                                       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C$  ALL NEW PER COSTELLO                                                        
      SUBROUTINE SOLNPP                                                         
      INTEGER OUT                                                               
      COMMON /CC/C(600)                                                         
      EQUIVALENCE (OUT,C(7)),(NSV,C(50))                                        
C$    COMMON /CCA/ CA(100)                                                      
      COMMON /CCA/ CA(150)                                                      
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      EQUIVALENCE (LIST(1),CA(11)),(SVSCLE(1),CA(31))                           
     *, (EVSCLE(1),CA(51)),(DF(1),CA(71)),(DFO(1),CA(91))                       
     *, (ITER,CA(111)),(RST,CA(112)),(STEP,CA(113))                             
     *, (ESUM,CA(114)),(DSUM,CA(115)),(SUM,CA(116))                             
     *, (ITBL50,CA(117)),(ITBL51,CA(118)),(NORD,CA(119))                        
     *, (KVAR,CA(120)),(IDONE,CA(121))                                          
      DIMENSION LIST(20),DF(20),DFO(20),SVSCLE(20),EVSCLE(20)                   
C   1 COMP CODE                                                                 
C   2 SOLUTION OPTION                                                           
C   3 STATE VARIABLE SCALE TABLE                                                
C   4 ERROR VARIABLE SCALE TABLE                                                
C   5 STEP                                                                      
C   6 RST                                                                       
C   7 STATE VARIABLE SOLUTION ORDER                                             
C  17 NUMBER OF STATE VARIABLE ORDERED                                          
      IF(NORD.EQ.0.OR.NORD.EQ.NSV) GO TO 60                                     
      J = NORD                                                                  
      DO 30 I = 1,NSV                                                           
      DO 40 K = 1,NSV                                                           
C*** SET UP ORDER IN WHICH THE STATE VARIABLES ARE TO                           
C    BE CONSIDERED                                                              
      IF(LIST(I).EQ.K) GO TO 30                                                 
   40 CONTINUE                                                                  
      J = J+1                                                                   
      LIST(J) = I                                                               
   30 CONTINUE                                                                  
C*** SET UP STATE AND ERROR VARIABLE SCALE FACTORS                              
   60 ITL = ITLUP(ITBL50)                                                       
      ITM = ITLUP(ITBL51)                                                       
      DO 70 I = 1,NSV                                                           
      SVSCLE(I) = D(ITL+I)                                                      
      EVSCLE(I) = D(ITM+I)                                                      
   70 CONTINUE                                                                  
      RETURN                                                                    
C     SOLNPP                                                                    
      END                                                                       
C$    SUBROUTINE SOLNPZ IS NEW                                                  
      SUBROUTINE SOLNPZ                                                         
      COMMON/CC/C(600)                                                          
      INTEGER CERR,OUT                                                          
      EQUIVALENCE (MRD,C(372)),(ICV(1),C(133)),(CERR,C(16)),(OUT,C(7))          
C$    COMMON /CCA/ CA(100)                                                      
      COMMON/CCA/  CA(150)                                                      
      EQUIVALENCE (LIST(1),CA(11)),(SVSCLE(1),CA(31)),                          
     *(EVSCL(1),CA(51)),(DF(1),CA(71)),(DFO(1),CA(91)),                         
     *(ITER,CA(111)),(RST,CA(112)),(STEP,CA(113)),                              
     *(ESUM,CA(114)),(DSUM,CA(115)),(SUM,CA(116)),(ITBL50,CA(117)),             
     *(ITBL51,CA(118)),(NORD,CA(119)),(KVAR,CA(120)),(IDONE,CA(121))            
      DIMENSION LIST(20),DF(20),DFO(20),SVSCLE(20),EVSCL(20)                    
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001),ICV(18)                                              
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 SOLUTION TECHNIQUE OPTION                                                 
C   3 STATE VARIABLE SCALE TABLE                                                
C   4 ERROR VARIABLE SCALE TABLE                                                
C   5 STEP VALUE                                                                
C   6 RST                                                                       
C   7 STATE VARIABLE ORDER (DIMENSIONED FOR MAXIMUM OF 20)                      
C  17 NUMBER OF ORDERED STATE VARIABLES                                         
      I=IACDB(1)                                                                
      ID(I+1)=ICV(1)                                                            
      MRD=ICV(2)                                                                
C$ THIS LINE NOT IN AECS.FORT(WSEP) VERSION                                     
      IF(MRD.LE.0) GO TO 20                                                     
C$ ABOVE LINE NOT IN AECS.FORT(NEWV)  AND BELOW WAS "J=I,20" NOT 1,20           
      DO 5 J=1,20                                                               
      DF(J)=0.0                                                                 
      DFO(J)=0.0                                                                
    5 LIST(J)=J                                                                 
      ITBL50=ITIDN(ICV(3),50)                                                   
      ITBL51=ITIDN(ICV(4),51)                                                   
      J=IPARM(ICV(5))                                                           
      STEP=D(J)                                                                 
      J=IPARM(ICV(6))                                                           
      RST=D(J)                                                                  
      NORD=0                                                                    
      DO 10 J=7,16                                                              
      IF(ICV(J).EQ.0) GO TO 20                                                  
      NORD=NORD+1                                                               
   10 LIST(J-6)=ICV(J)                                                          
   20 IF(MRD.GE.0.AND.MRD.LE.3) GO TO 99                                        
      CERR=CERR+1                                                               
      CALL LINES(2)                                                             
      WRITE(OUT,100) CERR,MRD                                                   
  100 FORMAT(6H0ERROR,I6,5X,19HINVALID SOLN OPTION,I6)                          
   99 CONTINUE                                                                  
      RETURN                                                                    
C     SOLNPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES THE SPEED OF SOUND FOR GAS                        
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION SOS (NL,P,T)                                                     
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFB,C(55)), (TL,C(81))                                       
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      LOC = ID(IFB+NL)                                                          
      IN = ID(LOC+3)                                                            
      TA = T                                                                    
      IF (TA.LT.TL) TA = TL                                                     
      SOS = 68.067*SQRT(POLYI(5,D(IN+20),TA)*D(IN+1)*TA)                        
      RETURN                                                                    
C     SOS                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE DEFINES SHAFT NUMBER                                       
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SRS (NST)                                                      
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7)), (CERR,C(16)), (ISTR,C(82)), (ISP,C(42))           
      INTEGER OUT, CERR                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      IF (NST.LT.1.OR.NST.GT.10) GO TO 100                                      
      I = 1                                                                     
      IF (ISTR.EQ.0) GO TO 200                                                  
      IF (ID(ISTR+NST).EQ.1) GO TO 10                                           
      IF (ID(ISTR+NST).EQ.-1) GO TO 20                                          
    1 ID(ISTR+NST) = 1                                                          
   99 CONTINUE                                                                  
      RETURN                                                                    
   10 CONTINUE                                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1001) CERR,NST                                                 
 1001 FORMAT(6H0ERROR,I6,5X,25HMULTIPLE SHAFT DEFINITION,I6)                    
      GO TO 99                                                                  
   20 CONTINUE                                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1004) CERR,NST                                                 
 1004 FORMAT(6H0ERROR,I6,5X,11HSHAFT ENDED,I6)                                  
      GO TO 99                                                                  
      ENTRY SRT (NST)                                                           
      IF (NST.LT.1.OR.NST.GT.10) GO TO 100                                      
      I = 2                                                                     
      IF (ISTR.EQ.0) GO TO 200                                                  
    2 IF (ID(ISTR+NST).EQ.0) GO TO 30                                           
      IF (ID(ISTR+NST).EQ.-1) GO TO 20                                          
      GO TO 99                                                                  
   30 CONTINUE                                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1002) CERR,NST                                                 
 1002 FORMAT(6H0ERROR,I6,5X,15HUNDEFINED SHAFT,I6)                              
      GO TO 99                                                                  
      ENTRY SRE (NST)                                                           
      IF (NST.LT.1.OR.NST.GT.10) GO TO 99                                       
      I = 3                                                                     
      IF (ISTR.EQ.0) GO TO 200                                                  
    3 ID(ISTR+NST) = -1                                                         
      GO TO 99                                                                  
  100 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1003) CERR,NST                                                 
 1003 FORMAT(6H0ERROR,I6,5X,20HINVALID SHAFT NUMBER,I6)                         
      GO TO 99                                                                  
  200 CALL GDCU(10,4,ISP,D,ISTR)                                                
      GO TO (1,2,3), I                                                          
C     SRS, SRT, SRE                                                             
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE SUMS COMPONENT SIZE INTO SYSTEM SIZE                       
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SSA                                                            
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (WTS,C(101)), (WTC,C(102)), (CUS,C(103)), (CUC,C(104))        
     *, (RIS,C(105)), (RIC,C(106)), (DRS,C(107)), (DRC,C(108))                  
     *, (WTIC,C(109)), (WTDS,C(110)), (WTDC,C(111))                             
      WTS = WTS+WTC+WTIC                                                        
      CUS = CUS+CUC                                                             
      RIS = RIS+RIC                                                             
      DRS = DRS*DRC                                                             
      WTDS = WTDS+WTDC*WTDC                                                     
      RETURN                                                                    
C     SSA                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CHECKS THE SHAFT NUMBER IN SIZING                          
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SSR(NST)                                                       
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (CERR,C(16)), (OUT,C(7))                                      
      INTEGER CERR, OUT                                                         
      IF (NST.GE.1 .AND. NST.LE.10) GO TO 99                                    
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,NST                                                 
 1000 FORMAT(6H0ERROR,I6,5X,20HINVALID SHAFT NUMBER,I6)                         
   99 CONTINUE                                                                  
      RETURN                                                                    
C     SSR                                                                       
      END                                                                       
*DECK,TDAHS                                                                     
      SUBROUTINE TDAHS (NL,NSI,NSO,TDAX)                                        
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IP,C(28)), (IT,C(29)), (IH,C(30)), (IW,C(27))                
     *, (OUT,C(7)), (IFB,C(55)), (IFP,C(22)), (BIG,C(31))                       
     *, (ILN,C(37)), (ISN,C(38))                                                
      INTEGER OUT                                                               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      REAL M,MV,ML,M1,M2                                                        
      DIMENSION  C1(19),C2(19),C3(94),C4(94)                                    
      DATA C1/ 300., 360., 410., 460., 510., 560., 610., 660., 710.             
     *, 760., 810., 860., 910., 960., 1010., 1060., 1110., 1160., 3000./        
C                                                                               
C     THE CONSTANTS FOR C2 HAVE BEEN CHANGED TO BE THE SAME AS WE               
C     USE AT HAMILTON STANDARD.                                                 
C                                                                               
      DATA C2 /  989.987,  1016.803,  1039.150,  1061.498,  1083.845,           
     1          1106.192,  1128.540,  1150.887,  1173.234,  1195.581,           
     2          1217.928,  1240.276,  1262.623,  1284.970,  1307.318,           
     3          1329.665,  1352.012,  1374.359,  2196.738/                      
C                                                                               
C     DATA C2/ 990.38, 1016.89, 1039.0, 1061.09, 1083.06, 1104.74               
C    *, 1125.79, 1145.9, 1164.0, 1179.7, 1192.3, 1201.0, 1204.6                 
C    *, 1201.7, 1190.0, 1165.5, 1118.5, 938., 938. /                            
      DATA C3/ 300., 310., 320., 330., 340., 350., 360., 370.                   
     *, 380., 390., 400., 410., 420., 430., 440., 450., 460.                    
     *, 465., 470., 475., 480., 485., 490., 495., 500., 505.                    
     *, 510., 515., 520., 525., 530., 535., 540., 545., 550.                    
     *, 555., 560., 565., 570., 575., 580., 585., 590., 595.                    
     *, 600., 605., 610., 615., 620., 625., 630., 635., 640.                    
     *, 645., 650., 655., 660., 670., 680., 690., 700., 710., 720.              
     *, 730., 740., 750., 760., 770., 780., 790., 800., 810., 820.              
     *, 830., 840., 850., 860., 870., 880., 890., 900., 910., 920.              
     *, 930., 940., 950., 960., 970., 980., 990., 1000., 1010., 1060.           
     *, 1165. /                                                                 
      DATA C4 / 4.949E-8, 1.620E-7, 4.928E-7, 1.403E-6, 3.757E-6                
     *, 9.517E-6, 2.291E-5, 5.260E-5, 1.157E-4, 2.443E-4, 4.972E-4              
     *, 9.776E-4, .00186, .00344, .006181, .01082, .01948, .02396               
     *, .03087, .03957, .05045, .0640, .08080, .099908, .12164                  
     *, .14746, .17799, .21397, .25618, .30554, .36304, .42979                  
     *, .50701, .59604, .69838, .81564, .94959, 1.1021, 1.2754                  
     *, 1.4717, 1.6933, 1.9430, 2.2237, 2.5382, 2.890, 3.2825                   
     *, 3.7194, 4.2046, 4.724, 5.3372, 5.9936, 6.7168, 7.5119                   
     *, 8.3845, 9.3403, 10.386, 11.526, 14.123, 17.186, 20.780, 24.969          
     *, 29.825, 35.429, 41.858, 49.203, 57.556, 67.013, 77.68, 89.66            
     *, 103.06, 118.01, 134.63, 153.04, 173.37, 195.77, 220.37, 247.31          
     *, 276.75, 308.83, 343.72, 381.59, 422.6 , 466.9 ,  514.7,  566.1          
     *, 621.4, 680.8, 744.3, 812.4, 885.0, 962.5,  1045.2, 1542., 3197./        
C                                                                               
      TDAX = D(IT+NSO)                                                          
      IF (D(IH+NSO).EQ.0.0) GO TO 99                                            
    1 CONTINUE                                                                  
      ICNT = 0                                                                  
      LOC1 = ID(IFB+NL)                                                         
      LOC1 = ID(LOC1+3)                                                         
      C5 = 1.678835*D(LOC1+1)                                                   
C                                                                               
      ASSIGN 29 TO IK                                                           
      T = D(IT+NSO)                                                             
      P = D(IP+NSO)                                                             
      M = D(IH+NSO)                                                             
      GO TO 7                                                                   
C                                                                               
   29 H2 = H                                                                    
      T2 = T                                                                    
      M2 = M                                                                    
      HRS2 = HRS                                                                
      ASSIGN 30 TO IK                                                           
      T = D(IT+NSI)                                                             
      P = D(IP+NSI)                                                             
      M = D(IH+NSI)                                                             
      GO TO 7                                                                   
C                                                                               
    7 IF (P.LE.0.0) GO TO 98                                                    
      CALL MDSCT(2,IND,PS,C4,T,C3,94,1,DX,DX,IDX,IDX)                             
      IF(PS.GE.P) GO TO 23                                                      
      HRS = C5 * PS / (P-PS)                                                    
   24 CALL MDSCT(2,IND,HV,C2,T,C1,19,1,DX,DX,IDX,IDX)                             
      IF(M.LE.HRS) GO TO 8                                                      
      MV = HRS                                                                  
      ML = M-HRS                                                                
      GO TO 9                                                                   
    8 MV = M                                                                    
      ML = 0.                                                                   
    9 HL = T - 492.0                                                            
      CP = SHP(NL,P,T,0.0)                                                      
      H = (CP*(T-460.0) + HV*MV + HL*ML) / (1.0+M)                              
      GO TO IK, (29,30)                                                         
   23 HRS = BIG                                                                 
      GO TO 24                                                                  
   30 H1 = H                                                                    
      P1 = P                                                                    
      T1 = T                                                                    
      M1 = M                                                                    
      HRS1 = HRS                                                                
      IF (M1.LE.HRS1 .AND. M2.LE.HRS2) GO TO 99                                 
      CPAVG = SHP(NL,(P1+P2)/2.,(T1+T2)/2.,M)                                   
      TDAX = ((D(IW+NL) * CPAVG * T1) - (D(IW+NL) * (H1 - H2))) /               
     1       (D(IW+NL) * CPAVG)                                                 
      GO TO 99                                                                  
   98 HRS = BIG                                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     TDAHS                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES DRY BULB OUTLET TEMPERATURE                       
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE TDB (NL,NSI,NSO,HRS)                                           
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IP,C(28)), (IT,C(29)), (IH,C(30)), (IW,C(27))                
     *, (OUT,C(7)), (IFB,C(55)), (IFP,C(22)), (BIG,C(31))                       
     *, (ILN,C(37)), (ISN,C(38))                                                
      INTEGER OUT                                                               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      REAL M,MV,ML,M1,M2                                                        
      DIMENSION  C1(19),C2(19),C3(94),C4(94)                                    
      DATA C1/ 300., 360., 410., 460., 510., 560., 610., 660., 710.             
     *, 760., 810., 860., 910., 960., 1010., 1060., 1110., 1160., 3000./        
      DATA C2/ 990.38, 1016.89, 1039.0, 1061.09, 1083.06, 1104.74               
     *, 1125.79, 1145.9, 1164.0, 1179.7, 1192.3, 1201.0, 1204.6                 
     *, 1201.7, 1190.0, 1165.5, 1118.5, 938., 938. /                            
      DATA C3/ 300., 310., 320., 330., 340., 350., 360., 370.                   
     *, 380., 390., 400., 410., 420., 430., 440., 450., 460.                    
     *, 465., 470., 475., 480., 485., 490., 495., 500., 505.                    
     *, 510., 515., 520., 525., 530., 535., 540., 545., 550.                    
     *, 555., 560., 565., 570., 575., 580., 585., 590., 595.                    
     *, 600., 605., 610., 615., 620., 625., 630., 635., 640.                    
     *, 645., 650., 655., 660., 670., 680., 690., 700., 710., 720.              
     *, 730., 740., 750., 760., 770., 780., 790., 800., 810., 820.              
     *, 830., 840., 850., 860., 870., 880., 890., 900., 910., 920.              
     *, 930., 940., 950., 960., 970., 980., 990., 1000., 1010., 1060.           
     *, 1165. /                                                                 
      DATA C4 / 4.949E-8, 1.620E-7, 4.928E-7, 1.403E-6, 3.757E-6                
     *, 9.517E-6, 2.291E-5, 5.260E-5, 1.157E-4, 2.443E-4, 4.972E-4              
     *, 9.776E-4, .00186, .00344, .006181, .01082, .01948, .02396               
     *, .03087, .03957, .05045, .0640, .08080, .099908, .12164                  
     *, .14746, .17799, .21397, .25618, .30554, .36304, .42979                  
     *, .50701, .59604, .69838, .81564, .94959, 1.1021, 1.2754                  
     *, 1.4717, 1.6933, 1.9430, 2.2237, 2.5382, 2.890, 3.2825                   
     *, 3.7194, 4.2046, 4.724, 5.3372, 5.9936, 6.7168, 7.5119                   
     *, 8.3845, 9.3403, 10.386, 11.526, 14.123, 17.186, 20.780, 24.969          
     *, 29.825, 35.429, 41.858, 49.203, 57.556, 67.013, 77.68, 89.66            
     *, 103.06, 118.01, 134.63, 153.04, 173.37, 195.77, 220.37, 247.31          
     *, 276.75, 308.83, 343.72, 381.59, 422.6 , 466.9 ,  514.7,  566.1          
     *, 621.4, 680.8, 744.3, 812.4, 885.0, 962.5,  1045.2, 1542., 3197./        
      IE = 1                                                                    
      GO TO 1                                                                   
C     TDB1 - WSEP,DRAIN,INJECT                                                  
      ENTRY TDB1 (NL,NSI,NSO,HRS)                                               
      IE = 2                                                                    
      NS = NSO                                                                  
      GO TO 1                                                                   
C     TDB2 - MERGE,EJECT,INJECT,WSEP                                            
      ENTRY TDB2 (NL,NSI,NSO,HRS)                                               
      IE = 2                                                                    
      NS = NSI                                                                  
      GO TO 1                                                                   
C     TDB3 - MERGE,EJECT                                                        
      ENTRY TDB3 (NL,NSI,NSO,HRS)                                               
      IE = 3                                                                    
      NS = NSI                                                                  
      GO TO 1                                                                   
C     TDB4 - MERGE,EJECT,INJECT                                                 
      ENTRY TDB4 (NL,NSI,NSO,HRS)                                               
      IE = 4                                                                    
      GO TO 1                                                                   
C     TDB5 - INJECT,WSEP                                                        
      ENTRY TDB5 (NL,NSI,NSO,HRS)                                               
      IE = 5                                                                    
      NS = NSO                                                                  
      GO TO 1                                                                   
C     TDB6 - WSEP                                                               
      ENTRY TDB6 (NL,NSI,NSO,HRS)                                               
      IE = 6                                                                    
      GO TO 1                                                                   
C     TDA - HX,BOILER,COND,EVAP                                                 
      ENTRY TDA  (NL,NSI,NSO,HRS)                                               
      IE = 7                                                                    
      TDAX = D(IT+NSO)                                                          
      IF (D(IH+NSO).EQ.0.0) GO TO 99                                            
    1 CONTINUE                                                                  
      TOL = 0.0001                                                              
      TOLD = BIG                                                                
      ICNT = 0                                                                  
      LOC1 = ID(IFB+NL)                                                         
      LOC1 = ID(LOC1+3)                                                         
      C5 = 1.678835*D(LOC1+1)                                                   
      GO TO (2,3,4,5,6,27,28), IE                                               
    2 ASSIGN 10 TO IK                                                           
      T = D(IT+NSI)                                                             
      P = D(IP+NSI)                                                             
      M = D(IH+NSI)                                                             
      GO TO 7                                                                   
   10 M1 = M                                                                    
      P1 = P                                                                    
      HRS1 = HRS                                                                
      ASSIGN 60 TO IK                                                           
      P = D(IP+NSO)                                                             
      GO TO 7                                                                   
   60 H1 = H                                                                    
      T1 = T                                                                    
      ASSIGN 11 TO IK                                                           
      T = D(IT+NSO)                                                             
      GO TO 7                                                                   
   11 H2 = H                                                                    
      T2 = T                                                                    
      M2 = M                                                                    
      P2 = P                                                                    
      HRS2 = HRS                                                                
      IF (M1.LE.HRS1 .AND. M2.LE.HRS2) GO TO 99                                 
      CP = SHP(NL,P,T,0.0)                                                      
      CALL MDSCT(2,IND,HV,C2,T,C1,19,1,DX,DX,IDX,IDX)                             
      HL = T - 492.0                                                            
      IF (M1.LE.HRS1) GO TO 25                                                  
      MV = HRS1                                                                 
      ML = M1-HRS1                                                              
      GO TO 26                                                                  
   25 MV = M1                                                                   
      ML = 0.0                                                                  
   26 HDB = (CP*(T-460.0) + HV*MV + HL*ML) / (1.0+M)                            
   65 ASSIGN 12 TO IK                                                           
      P = D(IP+NSO)                                                             
      M = D(IH+NSO)                                                             
      IF (T1.EQ.T2) GO TO 53                                                    
   13 IF (H1.EQ.H2) GO TO 18                                                    
      T = T2 + (HDB-H2) / (H1-H2) * (T1-T2)                                     
      GO TO 19                                                                  
   18 T = 0.5*(T1+T2)                                                           
   19 ICNT = ICNT + 1                                                           
      IF(ICNT.GT.20) GO TO 15                                                   
      GO TO 7                                                                   
   12 CONTINUE                                                                  
      IF (ABS(H-HDB).LE.TOL) GO TO 22                                           
      IF (ABS(T-TOLD).EQ.0.0 .AND. ABS(H-HDB).LT.(10.0*TOL)) GO TO 22           
      TOLD = T                                                                  
      IF (ABS(HDB-H1).GT.ABS(HDB-H2)) GO TO 14                                  
   52 H2 = H                                                                    
      T2 = T                                                                    
      GO TO 13                                                                  
   14 H1 = H                                                                    
      T1 = T                                                                    
      GO TO 13                                                                  
   53 IF (P2.GT.P1) GO TO 54                                                    
      T = T1-5.0                                                                
      GO TO 19                                                                  
   54 T = T1+5.0                                                                
      GO TO 19                                                                  
   15 IF (IFP.NE.1) GO TO 22                                                    
      I1 = ILN+NL                                                               
      I2 = ISN+NSO                                                              
      CALL LINES(2)                                                             
      WRITE (OUT,1000) ID(I1),ID(I2)                                            
 1000 FORMAT(1H0,5X,44H***NON-CONVERGENCE DRY BULB TEMPERATURE - NL,I6,         
     *3X,2HNS,I6)                                                               
   22 D(IT+NSO) = T                                                             
      GO TO 99                                                                  
    3 ASSIGN 16 TO IK                                                           
      T = D(IT+NS)                                                              
      P = D(IP+NS)                                                              
      M = D(IH+NS)                                                              
      GO TO 7                                                                   
   16 H1 = H                                                                    
      P1 = P                                                                    
      T1 = T                                                                    
      W1 = D(IW+NL)                                                             
      GO TO 99                                                                  
    4 ASSIGN 17 TO IK                                                           
      T = D(IT+NS)                                                              
      P = D(IP+NS)                                                              
      M = D(IH+NS)                                                              
      GO TO 7                                                                   
   17 H2 = H                                                                    
      P2 = P                                                                    
      T2 = T                                                                    
      W2 = D(IW+NL)                                                             
      GO TO 99                                                                  
    6 P2 = D(IP+NS)                                                             
      T2 = D(IT+NS)                                                             
      H2 = T2 - 492.0                                                           
      W2 = D(IW+NL)                                                             
      GO TO 99                                                                  
    5 IF (P1.LE.0.0 .OR. P2.LE.0.0) GO TO 98                                    
      HDB = (H1*W1+H2*W2)/(W1+W2)                                               
      P = D(IP+NSO)                                                             
      M = D(IH+NSO)                                                             
      T = T1                                                                    
      ASSIGN 61 TO IK                                                           
      GO TO 7                                                                   
   61 H1 = H                                                                    
      T = T2                                                                    
      ASSIGN 62 TO IK                                                           
      GO TO 7                                                                   
   62 H2 = H                                                                    
      GO TO 65                                                                  
   27 IF (P1.LE.0.0 .OR. P2.LE.0.0) GO TO 98                                    
      HDB = (H1*W1-H2*W2)/(W1+W2)                                               
      P = D(IP+NSO)                                                             
      M = D(IH+NSO)                                                             
      T = T1                                                                    
      ASSIGN 63 TO IK                                                           
      GO TO 7                                                                   
   63 H1 = H                                                                    
      T = T2                                                                    
      ASSIGN 64 TO IK                                                           
      GO TO 7                                                                   
   64 H2 = H                                                                    
      GO TO 65                                                                  
   98 HRS = BIG                                                                 
   99 CONTINUE                                                                  
      IF (IE.EQ.7) HRS = TDAX                                                   
      RETURN                                                                    
    7 CONTINUE                                                                  
      IF (P.LE.0.0) GO TO 98                                                    
      CALL MDSCT(2,IND,PS,C4,T,C3,94,1,DX,DX,IDX,IDX)                             
      IF(PS.GE.P) GO TO 23                                                      
      HRS = C5 * PS / (P-PS)                                                    
   24 CALL MDSCT(2,IND,HV,C2,T,C1,19,1,DX,DX,IDX,IDX)                             
      IF(M.LE.HRS) GO TO 8                                                      
      MV = HRS                                                                  
      ML = M-HRS                                                                
      GO TO 9                                                                   
    8 MV = M                                                                    
      ML = 0.                                                                   
    9 HL = T - 492.0                                                            
      CP = SHP(NL,P,T,0.0)                                                      
      H = (CP*(T-460.0) + HV*MV + HL*ML) / (1.0+M)                              
C     GO TO IK, (10,60,11,12,16,17,61,62,63,64,29,30)                           
      GO TO IK, (10,60,11,12,16,17,61,62,63,64,29,30)                           
   23 HRS = BIG                                                                 
      GO TO 24                                                                  
   28 ASSIGN 29 TO IK                                                           
      T = D(IT+NSO)                                                             
      P = D(IP+NSO)                                                             
      M = D(IH+NSO)                                                             
      GO TO 7                                                                   
   29 H2 = H                                                                    
      T2 = T                                                                    
      M2 = M                                                                    
      HRS2 = HRS                                                                
      ASSIGN 30 TO IK                                                           
      T = D(IT+NSI)                                                             
      P = D(IP+NSI)                                                             
      M = D(IH+NSI)                                                             
      GO TO 7                                                                   
   30 H1 = H                                                                    
      P1 = P                                                                    
      T1 = T                                                                    
      M1 = M                                                                    
      HRS1 = HRS                                                                
      IF (M1.LE.HRS1 .AND. M2.LE.HRS2) GO TO 99                                 
      HDB = H2                                                                  
      TDAX = T2                                                                 
   31 CP = SHP(NL,P2,TDAX,0.0)                                                  
      CALL MDSCT(2,IND,HV,C2,TDAX,C1,19,1,DX,DX,IDX,IDX)                          
      IF (M1.LE.HRS1) GO TO 32                                                  
      MV = HRS1                                                                 
      ML = M1-HRS1                                                              
      GO TO 33                                                                  
   32 MV = M1                                                                   
      ML = 0.0                                                                  
   33 HDAR = (CP*(TDAX-460.0) + HV*MV +HL*ML) / (1.0+M2)                        
      ICNT = ICNT+1                                                             
      IF (ICNT.EQ.20) GO TO 34                                                  
      IF (ABS(HDAR-HDB).LE.TOL) GO TO 99                                        
      TDAX = (HDB*(1.0+M2) - HV*MV - HL*ML) / CP + 460.0                        
      GO TO 31                                                                  
   34 IF (IFP.NE.1) GO TO 99                                                    
      I1 = ILN+NL                                                               
      I2 = ISN+NSO                                                              
      CALL LINES(2)                                                             
      WRITE (OUT,1001) ID(I1),ID(I2)                                            
 1001 FORMAT(1H0,5X,49H***NON-CONVERGENCE DRY AIR RATED TEMPERATURE - NL        
     *,I6,3X,2HNS,I6)                                                           
      GO TO 99                                                                  
C     TDB                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES DRY AIR RATED OUTLET TEMPERATURE                  
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION TDBI (NL,NSI,NSO)                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IP,C(28)), (IT,C(29)), (IH,C(30)), (IW,C(27))                
     *, (OUT,C(7)), (IFB,C(55)), (IFP,C(22)), (BIG,C(31))                       
     *, (ILN,C(37)), (ISN,C(38))                                                
      INTEGER OUT                                                               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      REAL M,MWT,MV,ML,M1,M2                                                    
      DIMENSION  C1(19),C2(19),C3(94),C4(94)                                    
      DATA C1/ 300., 360., 410., 460., 510., 560., 610., 660., 710.             
     *, 760., 810., 860., 910., 960., 1010., 1060., 1110., 1160., 3000./        
      DATA C2/ 990.38, 1016.89, 1039.0, 1061.09, 1083.06, 1104.74               
     *, 1125.79, 1145.9, 1164.0, 1179.7, 1192.3, 1201.0, 1204.6                 
     *, 1201.7, 1190.0, 1165.5, 1118.5, 938., 938. /                            
      DATA C3/ 300., 310., 320., 330., 340., 350., 360., 370.                   
     *, 380., 390., 400., 410., 420., 430., 440., 450., 460.                    
     *, 465., 470., 475., 480., 485., 490., 495., 500., 505.                    
     *, 510., 515., 520., 525., 530., 535., 540., 545., 550.                    
     *, 555., 560., 565., 570., 575., 580., 585., 590., 595.                    
     *, 600., 605., 610., 615., 620., 625., 630., 635., 640.                    
     *, 645., 650., 655., 660., 670., 680., 690., 700., 710., 720.              
     *, 730., 740., 750., 760., 770., 780., 790., 800., 810., 820.              
     *, 830., 840., 850., 860., 870., 880., 890., 900., 910., 920.              
     *, 930., 940., 950., 960., 970., 980., 990., 1000., 1010., 1060.           
     *, 1165. /                                                                 
      DATA C4 / 4.949E-8, 1.620E-7, 4.928E-7, 1.403E-6, 3.757E-6                
     *, 9.517E-6, 2.291E-5, 5.260E-5, 1.157E-4, 2.443E-4, 4.972E-4              
     *, 9.776E-4, .00186, .00344, .006181, .01082, .01948, .02396               
     *, .03087, .03957, .05045, .0640, .08080, .099908, .12164                  
     *, .14746, .17799, .21397, .25618, .30554, .36304, .42979                  
     *, .50701, .59604, .69838, .81564, .94959, 1.1021, 1.2754                  
     *, 1.4717, 1.6933, 1.9430, 2.2237, 2.5382, 2.890, 3.2825                   
     *, 3.7194, 4.2046, 4.724, 5.3372, 5.9936, 6.7168, 7.5119                   
     *, 8.3845, 9.3403, 10.386, 11.526, 14.123, 17.186, 20.780, 24.969          
     *, 29.825, 35.429, 41.858, 49.203, 57.556, 67.013, 77.68, 89.66            
     *, 103.06, 118.01, 134.63, 153.04, 173.37, 195.77, 220.37, 247.31          
     *, 276.75, 308.83, 343.72, 381.59, 422.6 , 466.9 ,  514.7,  566.1          
     *, 621.4, 680.8, 744.3, 812.4, 885.0, 962.5,  1045.2, 1542., 3197./        
      IF (D(IH+NSO).EQ.0.0) GO TO 10                                            
      TOL = 0.0001                                                              
      ITER = 0                                                                  
      LOC1 = ID(IFB+NL)                                                         
      LOC1 = ID(LOC1+3)                                                         
      C5 = 1.678835 * D(LOC1+1)                                                 
    2 ASSIGN 3 TO IK                                                            
      T = D(IT+NSO)                                                             
      P = D(IP+NSO)                                                             
      M = D(IH+NSO)                                                             
      GO TO 5                                                                   
    3 H2 = H                                                                    
      P2 = P                                                                    
      T2 = T                                                                    
      M2 = M                                                                    
      HRS2 = HRS                                                                
      ASSIGN 4 TO IK                                                            
      T = D(IT+NSI)                                                             
      P = D(IP+NSI)                                                             
      M = D(IH+NSI)                                                             
      GO TO 5                                                                   
    4 H1 = H                                                                    
      P1 = P                                                                    
      T1 = T                                                                    
      M1 = M                                                                    
      HRS1 = HRS                                                                
      IF (M1.LE.HRS1 .AND. M2.LE.HRS2) GO TO 10                                 
      HDB = H2                                                                  
      TDAR = T2                                                                 
   12 CP = SHP(NL,P2,TDAR,0.0)                                                  
      CALL MDSCT(2,IND,HV,C2,TDAR,C1,19,1,DX,DX,IDX,IDX)                          
      IF (M1.LE.HRS1) GO TO 13                                                  
      MV = HRS1                                                                 
      ML = M1-HRS1                                                              
      GO TO 14                                                                  
   13 MV = M1                                                                   
      ML = 0.0                                                                  
   14 HDAR = (CP*(TDAR-460.0) + HV*MV + HL*ML) / (1.0+M2)                       
      ITER = ITER + 1                                                           
      IF (ITER.EQ.20) GO TO 15                                                  
      IF (ABS(HDAR-HDB).LE.TOL) GO TO 11                                        
      TDAR = (HDB*(1.0+M2) - HV*MV - HL*ML) / CP + 460.0                        
      GO TO 12                                                                  
   15 IF (IFP.NE.1) GO TO 11                                                    
      I1 = ILN+NL                                                               
      I2 = ISN+NSO                                                              
      CALL LINES(2)                                                             
      WRITE (OUT,1000) ID(I1),ID(I2)                                            
 1000 FORMAT(1H0,5X,50H***NON-CONVERGENCE DRY AIR RATED TEMPERATURE - NL        
     *,I6,3X,2HNS,I6)                                                           
   11 TDBI = TDAR                                                               
      GO TO 99                                                                  
   10 TDBI = D(IT+NSO)                                                          
   99 CONTINUE                                                                  
      RETURN                                                                    
    5 CONTINUE                                                                  
      CALL MDSCT(2,IND,PS,C4,T,C3,94,1,DX,DX,IDX,IDX)                             
      IF (PS.GE.P) GO TO 7                                                      
      HRS = C5 * PS / (P-PS)                                                    
    6 CALL MDSCT(2,IND,HV,C2,T,C1,19,1,DX,DX,IDX,IDX)                             
      IF(M.LE.HRS) GO TO 8                                                      
      MV = HRS                                                                  
      ML = M-HRS                                                                
      GO TO 9                                                                   
    8 MV = M                                                                    
      ML = 0.                                                                   
    9 HL = T - 492.0                                                            
      CP = SHP(NL,P,T,0.0)                                                      
      H = (CP*(T-460.0) + HV*MV + HL*ML) / (1.0+M)                              
      GO TO IK, (3,4)                                                           
    7 HRS = BIG                                                                 
      GO TO 6                                                                   
C     TDBI                                                                      
      END                                                                       
*DECK,TFH                                                                       
      FUNCTION TFH (NL,P,H)                                                     
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFB,C(55)), (BIG,C(31))                                      
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      LOC = ID(IFB+NL)                                                          
      IFT = ID(LOC+1)                                                           
      IN = ID(LOC+3)                                                            
      IF (IFT-2) 1,2,3                                                          
    1 CONTINUE                                                                  
      TFH = POLYI(3,D(IN+17),H)                                                 
      GO TO 99                                                                  
    2 CONTINUE                                                                  
      TFH = POLYI(5,D(IN+32),H)                                                 
      GO TO 99                                                                  
    3 CONTINUE                                                                  
      TFH = BIG                                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     TFH                                                                       
      END                                                                       

C*   NORTHROP GRUMMAN PROPRIETARY