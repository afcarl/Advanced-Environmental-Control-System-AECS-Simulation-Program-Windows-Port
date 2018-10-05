C*   NORTHROP GRUMMAN PROPRIETARY
C$    SUBROUTINE REFPPZ                                                         
C$        COMPUTES REFRIGERANT QUALITY GIVEN PRESSURE AND ENTHALPY              
      SUBROUTINE REFPPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IP,C(28)), (IT,C(29)), (IH,C(30))              
     *, (IW,C(27)), (PASS,C(17)), (ISVT,C(46)), (ISV,C(47))                     
     *, (SCR(1),C(151)), (IFB,C(55)), (OUT,C(7))                                
      INTEGER PASS, OUT                                                         
      REAL KF,KV,MUF,MUVJK,KVJK                                                 
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (SCR(1),NL), (SCR(2),NS), (SCR(3),JSV)                        
     *, (SCR(4),NC), (SCR(5),LOCT), (SCR(6),P), (SCR(7),T)                      
     *, (SCR(8),TS), (SCR(9),V), (SCR(10),DSH)                                  
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 P                                                                         
C   4 DP                                                                        
C   5 N                                                                         
C   6 DT                                                                        
C   7 M                                                                         
C   8 IOP(0 & 1 FOR VAPOR, 2 FOR LIQUID)                                        
C   9 T IF IOP=1                                                                
C   10 DSH                                                                      
C$  10  NOT USED                                                                
C$  11 REFRIG NUMBER (-13 ETC)                                                  
      IRCD = IRCDB(10)                                                          
      NL = ID(IRCD+2)                                                           
C$    IF (PASS.EQ.1) CALL FLUIDP(NL)                                            
      IF (PASS.NE.1) GO TO 99                                                   
      LOCT = ID(IFB+NL)                                                         
C     WRITE(OUT,8900) NL, LOCT, IFB, IRCD                                       
C8900 FORMAT(6X,6H   NL=,I8,   5X,6H LOCT=,I8,   5X,6H  IFB=,I8                 
C    *          ,5X,6H IRCD=,I8)                                                
      NC = ID(LOCT+1)                                                           
      LOCT = ID(LOCT+3)                                                         
C     WRITE(OUT,8910) LOCT                                                      
C8910 FORMAT(6X,6H LOCT=,I8,   5X,' AFTER LOCT=ID(LOCT+3)'   )                  
      PJK = D(ID(IRCD+3))                                                       
      DP = D(ID(IRCD+4))                                                        
      N = D(ID(IRCD+5))                                                         
      DT = D(ID(IRCD+6))                                                        
      M = D(ID(IRCD+7))                                                         
      IF((N.GT.100).OR.(M.GT.100)) GO TO 99                                     
      NUMREF =D(ID(IRCD+11))                                                    
      IOP = ID(IRCD+8)                                                          
      IF(IOP-1.) 49,40,60                                                       
C$$$$    VAPOR                                                                  
C$$$$    VAPOR                                                                  
C$$$$    VAPOR                                                                  
C$$$$    VAPOR, USE COL XX FOR TEMP                                             
   40 TJKINI = D(ID(IRCD+9))                                                    
      TJK = D(ID(IRCD+9))                                                       
C$$$$    VAPOR, USE TSAT FOR START TEMP                                         
      WRITE(OUT,1111)                                                           
      WRITE(OUT,1111)                                                           
      WRITE(OUT,1111)                                                           
      WRITE(OUT,1111)                                                           
 1111 FORMAT(1X)                                                                
   49 WRITE(OUT,9000) NUMREF                                                    
 9000 FORMAT(5X, 42H VAPOR PROPERTIES FROM ROUTINE "REFP" FOR                   
     *      ,    20HREFRIGERANT NUMBER= , I4)                                   
   50 DO 20 J = 1,N  ! pressure steps                                                           
C$    K = 0                                                                     
      IF(IOP.EQ.0.) TJK = VTS(LOCT,PJK)                                         
	if(tjk.lt. vts(loct,pjk)) write(out,1003)
	if(tjk.lt. vts(loct,pjk)) TJK=VTS(LOCT,PJK) !C$
	if(tjk.lt. vts(loct,pjk)) GO TO 21          !C$
      VJK = VSV('REFPPP 1',LOCT,PJK,TJK)                                        
      HJK = VH(LOCT,PJK,TJK,VJK)                                                
      SJK = VS(LOCT,TJK,VJK)                                                    
      DPJK = 0.0                                                                
      IF(IOP.EQ.0.) DPJK = VDPDT(LOCT,PJK)                                      
      MUVJK = VVISCV(LOCT,PJK,TJK)                                              
      ST = VST(LOCT,TJK)                                                        
      KF = VCONDF(LOCT,PJK,TJK)                                                 
      KVJK = VCONDV(LOCT,PJK,TJK)                                               
      HFG = VHFG(LOCT,PJK,TJK,VJK)                                              
      MUF = VVISCF(LOCT,PJK,TJK)                                                
      CPVAP = VCPV(LOCT,PJK,TJK)                                                
C                                                                               
      WRITE(OUT,1111)                                                           
      IF(IOP.EQ.0.) WRITE(OUT,1002) KF, HFG, DPJK, ST, MUF                      
 1002 FORMAT(18X, 3HKF=,F10.6,1X,  4HHFG=,F8.3,1X,  6HVDPDT=,F9.4,1X,           
     *            5HSRFT=,1PE10.3,1X,  4HMUF=,1PE10.3)                          
C                                                                               
      WRITE(OUT,1001) PJK,TJK,VJK,HJK,SJK,CPVAP,KVJK,MUVJK                      
 1003 FORMAT(' T LESS THAN TSAT')
   21 CONTINUE                                                                               
C                                                                               
C$    K = 1                                                                     
C$    TJK PREVIOUSLY CLIMBED THROUGH PRESSURE STEPS                             
      DO 30 K = 1,M   !temperature steps                                                          
      TJK = TJK+DT                                                              
      VJK = VSV('REFPPP 2',LOCT,PJK,TJK)                                        
      HJK = VH(LOCT,PJK,TJK,VJK)                                                
      SJK = VS(LOCT,TJK,VJK)                                                    
      MUVJK = VVISCV(LOCT,PJK,TJK)                                              
      KVJK = VCONDV(LOCT,PJK,TJK)                                               
      CPVAP = VCPV(LOCT,PJK,TJK)                                                
   30 WRITE(OUT,1001) PJK, TJK, VJK, HJK, SJK, CPVAP, KVJK, MUVJK               
 1001 FORMAT(1X,3HPV=,F7.2,1X,  2HT=,F7.2,1X,  2HV=,F6.2,1X,                    
     *2HH=,F7.2,1X,  2HS=,F7.4,1X,  4HCPV=,F8.3,1X, 3HKV=,F8.5,1X               
     *,4HMUV=,1PE10.3)                                                          
      TJK = TJKINI                                                              
      PJK = PJK+DP                                                              
   20 CONTINUE                                                                  
      GO TO 99                                                                  
C$$$$    LIQUID                                                                 
C$$$$    LIQUID                                                                 
C$$$$    LIQUID                                                                 
C$$$$    LIQUID                                                                 
   60 J = 0                                                                     
      WRITE(OUT,1111)                                                           
      WRITE(OUT,1111)                                                           
      WRITE(OUT,1111)                                                           
      WRITE(OUT,1111)                                                           
      WRITE(OUT,9010) NUMREF                                                    
 9010 FORMAT(5X, 43H LIQUID PROPERTIES FROM ROUTINE "REFP" FOR                  
     *      ,    20HREFRIGERANT NUMBER= , I4)                                   
      DO 70 J =1,N                                                              
C$    K = 0                                                                     
      TJK = VTS(LOCT,PJK)                                                       
      HJK = VHLIQ(LOCT,PJK,TJK)                                                 
      VJK = VDL(LOCT,TJK)                                                       
C     MUF = VVISCF(LOCT,PJK,TJK)                                                
C     KF = VCONDF(LOCT,PJK,TJK)                                                 
C     WRITE(OUT,1003) PJK,TJK,MUF,KF                                            
C1003 FORMAT(1X, 3HPF=,F7.2,1X, 2HT=,F7.2,1X, 4HMUF=,1PE10.3                    
C    *,1X,  3HKF=,1PE10.3)                                                      
      DO 80 K = 1,M                                                             
      HJK = VHLIQ(LOCT,PJK,TJK)                                                 
      VJK = VDL(LOCT,TJK)                                                       
      CPLIQ = VCPF(LOCT,PJK,TJK)                                                
      MUF = VVISCF(LOCT,PJK,TJK)                                                
      KF = VCONDF(LOCT,PJK,TJK)                                                 
      WRITE(OUT,1004) PJK,TJK,HJK,VJK,CPLIQ,MUF,KF                              
 1004 FORMAT(1X, 3HPF=,F7.2,1X, 2HT=,F7.2,1X, 2HH=,F7.2,                        
     * 1X,  5HRHOF=,F7.2,  1X,4HCPF=,F6.3,                                      
     * 1X,  4HMUF=,1PE10.3,1X,  3HKF=,1PE10.3)                                  
      TJK = TJK - DT                                                            
   80 CONTINUE                                                                  
      PJK = PJK + DP                                                            
      WRITE(OUT,1111)                                                           
   70 CONTINUE                                                                  
   99 CONTINUE                                                                  
      RETURN                                                                    
C     REFPPP                                                                    
      END                                                                       
      SUBROUTINE REFPPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
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
C   6 DT                                                                        
C   7 M                                                                         
C   8 IOP(0 & 1 FOR VAPOR, 2 FOR LIQUID)                                        
C   9 T IF IOP=1                                                                
C   10 DSH                                                                      
      I = IACDB(10)                                                             
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      ID(I+3) = IPARM(ICV(3))                                                   
      ID(I+4) = IPARM(ICV(4))                                                   
      ID(I+5) = IPARM(ICV(5))                                                   
      ID(I+6) = IPARM(ICV(6))                                                   
      ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = ICV(8)                                                          
      ID(I+9) = IPARM(ICV(9))                                                   
      CALL FLUIDZ(NL,ICV(10),ICV(11))                                           
      CALL FTR(NL,NC)                                                           
   99 CONTINUE                                                                  
      RETURN                                                                    
C     REFPPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      A VAPOR CYCLE COMPRESSOR                                                 
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE VCMPPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)),(IW,C(27)),(IP,C(28)),(IT,C(29))                 
     *,(IH,C(30)),(SCR(1),C(151)),(IFB,C(55)),(IGA,C(35))                       
     *,(PASS,C(17)),(ISVT,C(46)),(ISV,C(47)),(IFP,C(22))                        
     *,(ICPP,C(88)),(OUT,C(7)),(QJC,C(371)),(IEV,C(49)),(IEVT,C(48))            
      DIMENSION SCR(30)                                                         
      INTEGER OUT, PASS                                                         
      EQUIVALENCE (SCR(1),NLI),(SCR(2),NSI),(SCR(3),NSO),(SCR(4),NST)           
     *,(SCR(5),IOP),(SCR(6),PR),(SCR(9),EFF),(SCR(10),JSVI)                     
     *,(SCR(11),KT),(SCR(12),IDISP),(SCR(13),NSP),(SCR(14),NLP)                 
     *,(SCR(15),NLO),(SCR(16),FH),(SCR(17),T),(SCR(18),HP)                      
     *,(SCR(19),WT),(SCR(20),PS),(SCR(21),TS),(SCR(22),HS)                      
     *,(SCR(23),VP),(SCR(24),PI),(SCR(25),TI),(SCR(26),HI)                      
     *,(SCR(27),PO),(SCR(28),VO),(SCR(29),HO)                                   
      EQUIVALENCE (SCR(30),I1)                                                  
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      EQUIVALENCE                                                               
     * (D(43),H1),(D(44),T1),(D(45),V1),(D(46),V2)                              
     *,(D(47),T2T),(D(48),ITRATE)                                               
     *,(D(49),X2),(D(50),X1),(D(51),EV),(D(52),DR)                              
     *,(D(53),DI),(D(54),DL),(D(55),WI),(D(56),WS)                              
C   1 COMP CODE                                                                 
C   2 LEG NO FOR INLET                                                          
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 SHAFT NO                                                                  
C   6 STATE VARIABLE OPTION (0 IF NO STATE VARIABLE)                            
C   7 PRESSURE RATIO                                                            
C   8 PRESSURE RATIO TABLE NO                                                   
C   9 EFFICIENCY TABLE NO                                                       
C  10 MECHANICAL EFFICIENCY                                                     
C  11 STATE VARIABLE INDEX                                                      
C  12 COMPRESSOR-TYPE OPTION                                                    
C*** IF ROOTS TYPE COMPRESSOR, INSERT ADDITIONAL DATA                           
C  13 DISPLACEMENT IN CUBIC FEET                                                
C*** INSERT A NEW STATION IDENTIFICATION FOR POCKET FEED                        
C  14 POCKET-FEED INLET STATION                                                 
C  15 POCKET-FEED LEG NO                                                        
C  16 LEG NO OF OUTLET                                                          
C  17 FRACTION OF LEAKAGE FLOW TO POCKET                                        
C*** THIS SUBROUTINE DETERMINES THE OUTLET P,T,H AND, IF POCKET FEED,           
C    THE INLET AND POCKET-FEED FLOWS (OUTLET FLOW MUST BE KNOWN                 
      IRCD = IRCDB(18)                                                          
C*** SET UP THE INTERNAL VARIABLES IN TERMS OF THE STORED ARRAY                 
      NLI = ID(IRCD+2)                                                          
      IOP=ID(IRCD+6)                                                            
      KT=ID(IRCD+12)                                                            
C*** IF POSITIVE DISPLACEMENT, INCLUDE THE DISPLACEMENT                         
      IF (KT.NE.0) DISP=D(ID(IRCD+13))                                          
      IF (KT.NE.2) GO TO 5                                                      
C*** IF A ROOTS COMPRESSOR WITH POCKET FEED, ADD FOUR PARAMETERS                
      FH=D(ID(IRCD+17))                                                         
      NLO=ID(IRCD+16)                                                           
      NLP=ID(IRCD+15)                                                           
      NSP=ID(IRCD+14)                                                           
    5 NST=ID(IRCD+5)                                                            
      LOCT = ID(IFB+NLI)                                                        
      NSI=ID(IRCD+3)                                                            
      NSO=ID(IRCD+4)                                                            
      LOCT = ID(LOCT+3)                                                         
      D(IGA+41) = D(IW+NLI)                                                     
      JSVI = ID(IRCD+ 11)                                                       
      JEVI = ID(IRCD + 18)                                                      
      NPA = NST + 90                                                            
      EFF=D(ID(IRCD+10))                                                        
      NSA=NST+80                                                                
      ITRATE=0                                                                  
C*** SHAFT SPEED AS OBTAINED FROM SHAFT                                         
C------- RPM IS A TLUP PARAMETER.                                               
      D(IGA+101)=D(IGA+NSA)                                                     
      RPM=D(IGA+NSA)                                                            
C*** VOLUMETRIC FLOW RATE FOR USE IN COMPRESSOR CHARACTERISTIC (TLUP)           
      V = VSV('VCMPPP 1',LOCT,D(IP+NSI),D(IT+NSI) )                             
      D(IGA+28) = D(IW+NLI) * V                                                 
      I1=ID(IRCD+7)                                                             
C*** SET UP THE STATE VARIABLE, THE PRESSURE RATIO,IF IOP=1                     
      IF(IOP-1) 9,10,8                                                          
C*** PRESSURE RATIO AS FUNCTION OF INLET CFM AND RPM                            
C*** SYSTEM WILL NOT CONVERGE IF D(PR)/D(FLOW)=0 OR INFINITY                    
    8 PR=TLUP(ID(IRCD+8))                                                       
      IF(PR.LT.1.0)PR=1.0+PR/1000.                                              
      IF(PR.GT.1000.)PR=1000.+PR/1000.                                          
      GO TO 12                                                                  
    9 PR=D(I1)                                                                  
      GO TO 12                                                                  
   10 IF(PASS.NE.1) GO TO 11                                                    
      ID(ISVT+JSVI) = 8                                                         
      D(ISV+JSVI) = D(I1)                                                       
C*** WITH PR A STATE VARIABLE, THE FLOW MAP IS NOT USED.  THE                   
C    COMPRESSOR CHARACTERISTIC IS ASSUMED TO BE SUCH THAT THE                   
C     FLOW CAN BE DELIVERED AT THE VALUE OF THE PRESSURE RATIO USED             
   11 PR=D(ISV+JSVI)                                                            
C*** ETA IS THE COMPRESSOR EFFICIENCY AS A FUNCTION OF FLOW AND RPM             
   12 ETA=TLUP(ID(IRCD+9))                                                      
      IF(ETA.LE.0.) ETA=0.01+ETA/1000.                                          
      IF(ETA.GE.1.) ETA=1.+ETA/1000.                                            
      D(IP+NSO)=PR*D(IP+NSI)                                                    
      PI=D(IP+NSI)                                                              
      PO=D(IP+NSO)                                                              
      IF(KT-1) 64,57,22                                                         
C*** ROOTS COMPRESSOR WITH POCKET FEED                                          
C    VARY PS TO GET INLET FLOW VALUES CORRECT                                   
C    THEN PS WILL NOT AGREE WITH D(IP+NSP), ERROR VAR RESULTS                   
   22 D(IW+NLO)=D(IW+NLI)+D(IW+NLP)                                             
      DEVHI= -D(IW+NLP)/D(IW+NLI)                                               
      DEVLOW=1. + DEVHI                                                         
      PLOW=D(IP+NSI)                                                            
      PHI=D(IP+NSO)                                                             
      WT=D(IW+NLP)                                                              
      WO=D(IW+NLO)                                                              
      PS=D(IP+NSP)                                                              
      TS=D(IT+NSP)                                                              
      HS=D(IH+NSP)                                                              
      VP=VSV('VCMPPP 2',LOCT,PS,TS)                                             
      TI=D(IT+NSI)                                                              
      HI=D(IH+NSI)                                                              
      DISP1=DISP                                                                
      EV1=D(IW+NLI)*V/(DISP*RPM)                                                
C*** IF THE VOLUMETRIC EFFICIENCY IS GT. 0.99, RESET IT                         
      IF(EV1.LT.0.99) GO TO 21                                                  
      DISPL1=D(IW+NLI)*V/RPM                                                    
      EV1=1.                                                                    
      WRITE(OUT,1003) EV1,DISP1                                                 
 1003 FORMAT(1H0,5X,43HWARNING IN VCMPPP: VOLUMETRIC EFFICIENCY IS,             
     * E15.7,/,5X,47H AS COMPUTED FROM RPM AND FLOW.',                          
     * /,' DISPLACEMENT IS TEMPORARILY RESET TO ',E14.7,' SO V.E.=1.0')         
C*** AS A FIRST APPROXIMATION WE LET                                            
   21 H1=HI                                                                     
      T1T=TI                                                                    
      T2T=TS                                                                    
      V1=V                                                                      
      V2=VP                                                                     
      H2=HS                                                                     
      W=RPM*DISP1*(PO-PI)*0.185053/ETA                                          
      HO=HI+W/D(IW+NLI)                                                         
      CALL VTAV2(LOCT,TOUT,VO,PO,HO)                                            
C*** BEGIN THE ITERATIONS                                                       
      EV=1.-(1.-EV1)/EV1*D(IW+NLI)/D(IW+NLO)                                    
      X1=FH*(1.-EV)/(2.-EV)                                                     
      X2=(1.-FH)*(1.-EV)/(2.-EV)                                                
   30 V1=HI/(H1/V1-X2*(HO-HI)/V2)                                               
C*** NEWTON-RAPHSON METHOD T1 AND HI OF P1, V1                                  
   31 V1T=VSV('VCMPPP 3',LOCT,PI,T1T)                                           
      DV=VSV('VCMPPP 4',LOCT,PI,T1T+1.)-V1T                                     
      T1=T1T*(V1/V1T)**(V1T/(T1T*DV))                                           
      IF(ABS(T1-T1T).LT.0.05) GO TO 311                                         
      T1T=T1                                                                    
      GO TO 31                                                                  
  311 H1=VH(LOCT,PI,T1,V1)                                                      
      DDEN=(H2/V2-H1/V1-0.185053*(PS-PI)-X1*(HO-HS)/V1)                         
     *     /(HS+X1*(HO-HS))                                                     
      V2=1./(1./V1+DDEN)                                                        
      HO=V2*(2.-EV)*(W/(RPM*DISP1)+(1./V1-X2/V2)*HI+(DDEN-X1/V2)*HS)            
C*** NEWTON-RAPHSON METHOD FOR T2, H2 VS PS, V2                                 
   32 V2T=VSV('VCMPPP 5',LOCT,PS,T2T)                                           
      DV=VSV('VCMPPP 6',LOCT,PS,T2T+1.)-V2T                                     
      T2=T2T*(V2/V2T)**(V2T/(T2T*DV))                                           
      IF(ABS(T2-T2T).LT.0.05) GO TO 33                                          
      T2T=T2                                                                    
      GO TO 32                                                                  
   33 H2=VH(LOCT,PS,T2,V2)                                                      
      CALL VTAV2(LOCT,TOUT,VO,PO,HO)                                            
      WO=RPM*DISP1/(V2*(2.-EV))                                                 
      WI=RPM*DISP1/V1-X2*WO                                                     
      WO=WO/(2.-EV)                                                             
      WS=WO-WI                                                                  
C*** FLOW RATE MUST BALANCE WITHIN 0.1%                                         
      IF (ABS(WS-WT)/D(IW+NLO).LT.0.001) GO TO 36                               
      WT=WS                                                                     
      GO TO 30                                                                  
   36 WS=WO-WI                                                                  
      IF (WS.GE.0..AND.WI.GE.0.) GO TO 38                                       
      WRITE (OUT,1002) WI,WS,WO                                                 
 1002 FORMAT (1H0,5X,42HIN ROOTS VCMPPP, ONE FLOW WAS NEGATIVE BUT,             
     * 14H RESET TO ZERO,/,10X,15HINLET FLOW WAS ,E14.7,/,10X,                  
     * 15HPOCKET FLOW WAS,E14.7,/,10X,15HOUTLET FLOW WAS,E14.7,/)               
      IF (WI.LT.0.0) GO TO 37                                                   
      WS=0.                                                                     
      WI=WO                                                                     
      GO TO 38                                                                  
C*** WE CANNOT LET THE MAIN INLET FLOW BE ZERO                                  
   37 WI=0.001*WO
c@      WS=.999*W0  !must have meant wo, not w0                                                                
      WS=.999*Wo                                                                
   38 CONTINUE                                                                  
C     IF(MOD(ITRATE,20).NE.0)GO TO 7698                                         
 7698 D(IT+NSO)=TOUT                                                            
      D(IH+NSO)=HO                                                              
C*** IN ITERATING FOR THE POCKET PRESSURE, WE CONVERGE ON THE RATIO             
C    OF THE FLOW RATES (POCKET TO MAIN) MATCHING THE RATIO OF THE               
C    LEG FLOWS.  WE CANNOT MATCH THE INDIVIDUAL FLOWS IF PR IS GIVEN            
C    BECAUSE THE COMPRESSOR SIZE IS NOT GIVEN IN THAT CASE                      
      DEV=-WS/WI+D(IW+NLP)/D(IW+NLI)                                            
C     WRITE(OUT,888) DEV,PS                                                     
  888 FORMAT(' *+* DEV=',E12.4,5X,' PS=',F10.3)                                 
C*** POCKET PRESSURE HAS CONVERGED IF THE DEVIATION LT 0.01%                    
      IF (ABS(DEV).LT.0.0001) GO TO 39                                          
C*** USE STRADDLING PROCEDURE TO GET CONVERGENCE                                
      IF (DEV.GT.0.) GO TO 41                                                   
      PHI=PS                                                                    
      DEVHI=DEV                                                                 
      GO TO 42                                                                  
   41 PLOW=PS                                                                   
      DEVLOW=DEV                                                                
   42 ITRATE=ITRATE+1                                                           
      PS=PLOW-(PHI-PLOW)*(DEVLOW)/(DEVHI-DEVLOW)                                
      IF(ITRATE-300)21,21,43                                                    
   43 WRITE(OUT,1089)                                                           
 1089 FORMAT(' *+* EXCEEDED 300 ITERATION LIMIT IN VCOMP'/                      
     *' *+* THE SPECIFIED INLET FLOW DID NOT MATCH THE'/                        
     *' *+* OUTLET FLOW AS A FRACTION. WHERE:'/                                 
     *' *+* FRAC = (SPECFLOW - FLOWIN) / OUTFLOW')                              
      WRITE(OUT,1090)D(IW+NLI),WI,D(IW+NLO),PR                                  
 1090 FORMAT(' *+* FLOW SPEC = ',E14.7,' IN = ',E14.7,' OUT = ',E14.7,/,        
     * ' CONTINUING AS IF FLOWS BALANCE, WITH PR = ',E14.7)                     
   39 IF(PASS.NE.1) GO TO 55                                                    
      ID(IEVT+JEVI)=2                                                           
   55 D(IEV+JEVI)=PS-D(IP+NSP)                                                  
      HP=-.02356*(D(IW+NLO)*D(IH+NSO)-D(IW+NLI)*D(IH+NSI)-                      
     * D(IW+NLP)*D(IH+NSP))/EFF                                                 
      GO TO 67                                                                  
C*** CONVENTIONAL ROOTS COMPRESSOR                                              
   57 WORK=RPM*DISP*(PO-PI)*144./QJC/ETA                                        
      D(IH+NSO)=D(IH+NSI)+WORK/D(IW+NLI)                                        
      GO TO 66                                                                  
C*** ISENTROPIC COMPRESSION FOR IDEAL COMPRESSOR                                
   64 S = VS(LOCT,D(IT+NSI),V )                                                 
      TISAT = VTS(LOCT,D(IP+NSI))                                               
      VSATI = VSV('VCMPPP 7',LOCT,D(IP+NSI),TISAT)                              
C$WAS HGI = VH(LOCT,D(IP+NSI),TISAT,VISAT)                                      
      HGI = VH(LOCT,D(IP+NSI),TISAT,VSATI)                                      
      TOSAT = VTS(LOCT,D(IP+NSO))                                               
      VSAT = VSV('VCMPPP 8',LOCT,D(IP+NSO),TOSAT)                               
      HG = VH(LOCT,D(IP+NSO),TOSAT,VSAT)                                        
      SOSAT = VS(LOCT,TOSAT,VSAT)                                               
      IF(ABS(S-SOSAT).LT.0.00001) GO TO 70                                      
      IF((S-SOSAT).LT.0.0) GO TO 70                                             
      IF((S-SOSAT).GT.0.0) GO TO 71                                             
   70 HFG = VHFG(LOCT,D(IP+NSO),TOSAT,VSAT)                                     
      SLIG = SOSAT-(HFG/TOSAT)                                                  
      QUAL = (S-SLIG)/(SOSAT-SLIG)                                              
      H = HG-(1.-QUAL)*HFG                                                      
      D(IH+NSO) = D(IH+NSI)+(H-D(IH+NSI))/ETA                                   
      QUALF = (D(IH+NSO)-HG+HFG)/HFG                                            
      IF(ABS(QUALF-1.).LT.0.00001) GO TO 72                                     
      IF((QUALF-1.).LT.0.0) GO TO 72                                            
      IF((QUALF-1.).GT.0.0) GO TO 73                                            
   72 D(IT+NSO) = TOSAT                                                         
      GO TO 66                                                                  
   73 CALL VTAV2(LOCT,D(IT+NSO),V,D(IP+NSO),D(IH+NSO))                          
      GO TO 66                                                                  
   71 CALL VTAV1(LOCT,T,V,D(IP+NSO),S)                                          
      H = VH(LOCT,D(IP+NSO),T,V )                                               
      D(IH+NSO) = D(IH+NSI) +  (H - D(IH+NSI))/ ETA                             
      CALL VTAV2(LOCT,D(IT+NSO),V,D(IP+NSO),D(IH+NSO) )                         
   66 HP = -0.02356 * D(IW+NLI)*(D(IH+NSO) - D(IH+NSI))/EFF                     
   67 D(IGA+NPA) = D(IGA+NPA) + HP                                              
      IF(IFP.NE.1. OR. ICPP.NE.0) GO TO 99                                      
      CALL PIOP(1,NLI,NSI,NSO)                                                  
      IF(KT.EQ.0) GO TO 68                                                      
      CALL PIOP(2,NLP,NSP,NSO)                                                  
   68 WRITE(OUT,1001) NST,PR,ETA,HP,HGI                                         
C$        HGI IS COMING OUT AS ******* WITH F7.2                                
 1001 FORMAT(1H0,5X,6HSHAFT ,I4,3X,2HPR,F8.4,3X,3HEFF,F7.4,3X,                  
C$WAS* 2HHP,F8.2,3X,3HHGI,F7.2)                                                 
C$ IS                                                                           
     * 2HHP,F8.2,3X,3HHGI,E14.6)                                                
      IF ((S-SOSAT).GT.0.0) GO TO 99                                            
      IF (KT.NE.0) GO TO 99                                                     
      CALL LINES (2)                                                            
      WRITE(OUT,1004) TOSAT,VSAT,SOSAT,HFG,QUAL,QUALF                           
 1004 FORMAT(1H0,5X,2HTS,F8.2,3X,2HVG,F8.5,3X,2HSG,F8.5,3X,3HHFG,               
     *F9.3,3X,2HXS,F7.3,3X,2HXR,F7.3)                                           
   99 CONTINUE                                                                  
      RETURN                                                                    
C     VCMPPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A VAPOR CYCLE COMPERSSOR                                  
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE VCMPPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
      DIMENSION ICV(18), SCR(30)                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/100/                                                            
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 SHAFT NO                                                                  
C   6 OPTION                                                                    
C   7 PRESSURE RATIO                                                            
C   8 PRESSURE RATIO TABLE NO                                                   
C   9 EFFICIENCY TABLE NO                                                       
C  10 MECHANICAL EFFICIENCY                                                     
C  11 STATE VARIABLE INDEX                                                      
C  12 COMPRESSOR TYPE OPTION                                                    
C  13 DISPLACEMENT IN CUBIC FEET                                                
C  14 POCKET-FEED INLET STATION                                                 
C  15 POCKET-FEED LEG NUMBER                                                    
C  16 LEG NUMBER OF OUTLET                                                      
C  17 FRACTION OF LEAKAGE FLOW TO POCKET                                        
C  18 ERROR VARIABLE INDEX                                                      
      I = IACDB(18)                                                             
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      CALL LEGRT(NL)                                                            
      NSI = ISTAN(ICV(3))                                                       
      ID(I+3) = NSI                                                             
      CALL START(NSI)                                                           
      NSO = ISTAN(ICV(4))                                                       
      ID(I+4) = NSO                                                             
      CALL STARS(NSO)                                                           
      CALL FTL(NL,IFTA)                                                         
      CALL FRR(NL,NF1)                                                          
      ID(I+5) = ICV(5)                                                          
      CALL SRT(ICV(5))                                                          
      ID(I+6) = ICV(6)                                                          
      ID(I+7) = IPARM(ICV(7))                                                   
      IF(ICV(6).EQ.2) ID(I+8) = ITIDN(ICV(8),4)                                 
      ID(I+9) = ITIDN(ICV(9),6)                                                 
      ID(I+10) = IPARM(ICV(10))                                                 
      IF(ICV(6).NE.1) GO TO 80                                                  
      ID(I+11) = IASV(8)                                                        
   80 ID(I+12) = ICV(11)                                                        
      IF(ID(I+12).EQ.0) GO TO 99                                                
      ID(I+13)=IPARM(ICV(12))                                                   
      IF(ID(I+12).EQ.1) GO TO 99                                                
      NSP=ISTAN(ICV(13))                                                        
      ID(I+14)=NSP                                                              
      CALL START(NSP)                                                           
      NLP=ILEGN(ICV(14))                                                        
      CALL LEGRT(NLP)                                                           
      NLO=ILEGN(ICV(15))                                                        
      CALL LEGRS(NLO)                                                           
      CALL FRS(NLO,NF1)                                                         
      ID(I+15)=NLP                                                              
      ID(I+16)=NLO                                                              
      ID(I+17)=IPARM(ICV(16))                                                   
      ID(I+18)=IAEV(2)                                                          
   99 CONTINUE                                                                  
      RETURN                                                                    
C     VCMPPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C       A VAPOR CYLCLE COMPRESSOR                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE VCMPSP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151)), (IW,C(27))         
C$  ADDED                                                   (IH,C(30))          
     *, (IP,C(28)), (IT,C(29)), (WTC,C(102)), (CUC,C(104)), (IH,C(30))          
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))                
     *, (IGA,C(35)), (HPE,C(112)), (HPH,C(114)), (IFB,C(55))                    
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE         (SCR( 1),WTD   ), (SCR( 2),WTR   )                    
     *, (SCR( 3),WTRC  ), (SCR( 4),VR    ), (SCR( 5),HP    )                    
     *, (SCR( 6),ED    ), (SCR( 7),DPR   ), (SCR( 8),DPRU  )                    
     *, (SCR( 9),DPRL  ), (SCR(10),NL    ), (SCR(11),NSI   )                    
     *, (SCR(12),NSO   ), (SCR(13),NST   ), (SCR(14),LOC   )                    
     *, (SCR(15),VI    ), (SCR(16),VO    ), (SCR(17),SN    )                    
     *, (SCR(18),IDT   ), (SCR(19),WTF   ), (SCR(20),CUF   )                    
     *, (SCR(21),RI    ), (SCR(22),DRF   ), (SCR(23),WTL   )                    
     *, (SCR(24),HPD   ), (SCR(25),WTCD  ), (SCR(26),EC    )                    
     *, (SCR(27),HI    ), (SCR(28),HO    ), (SCR(29),SI    )                    
     *, (SCR(30),HIS   )                                                        
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DIMENSION TABE(4),TABN(4)                                                 
      DATA TABE/0.98,1.0,1.02,1.04/, TABN/5500.0,7500.0,11500.0,22000.0/        
C   1 COMP CODE                                                                 
C   2 NL                                                                        
C   3 NSI                                                                       
C   4 NSO                                                                       
C   5 NST                                                                       
C   6 WTM                                                                       
C   7 CUM                                                                       
C   8 RI                                                                        
C   9 DRM                                                                       
C  10 VE                                                                        
C  11 VC                                                                        
C  12 IDT                                                                       
C  13 DIA                                                                       
C  14 MAP                                                                       
C  15 L                                                                         
C  16 CWT                                                                       
C  17 EWT                                                                       
      IRCD = IRCDB(17)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      NST = ID(IRCD+5)                                                          
      LOC = ID(IFB+NL)                                                          
      LOC = ID(LOC+3)                                                           
C$ ADD THE TWO LINES BELOW                                                      
      HI = D(IH+NSI)                                                            
      HO = D(IH+NSO)                                                            
      VI = VSV('VCMPSP 1',LOC,D(IP+NSI),D(IT+NSI))                              
C$ MORE ADDITIONS HERE BY COSTELLO                                              
      SI = VS(LOC,D(IT+NSI),VI)                                                 
      TOSAT = VTS(LOC,D(IP+NSO))                                                
      VSAT = VSV('VCMPSP 2',LOC,D(IP+NSO),TOSAT)                                
      SOSAT = VS(LOC,TOSAT,VSAT)                                                
      IF (ABS(SI-SOSAT).LT.0.00001) GO TO 5                                     
      IF ((SI-SOSAT).LT.0) GO TO 5                                              
      IF ((SI-SOSAT).GT.0) GO TO 6                                              
    5 HFG = VHFG(LOC,D(IP+NSO),TOSAT,VSAT)                                      
      SLIG = SOSAT-HFG/TOSAT                                                    
      QUAL = (SI-SLIG)/(SOSAT-SLIG)                                             
      HG = VH(LOC,D(IP+NSO),TOSAT,VSAT)                                         
      HIS = HG-(1-QUAL)*HFG                                                     
C$ SAME FROM HERE DOWN                                                          
      GO TO 7                                                                   
    6 CALL VTAV1(LOC,TIS,VIS,D(IP+NSO),SI)                                      
      HIS = VH(LOC,D(IP+NSO),TIS,VIS)                                           
    7 EC = (HIS-HI)/(HO-HI)                                                     
      HP = D(IW+NL)*(HO-HI)*0.02356                                             
      I1 = ID(IRCD+10)                                                          
      I2 = ID(IRCD+11)                                                          
      VR = 0.3*D(I1)+0.6*D(I2)+5.184*D(IW+NL)*VI                                
      WTR = 0.012*VR                                                            
      WTRC = VR*VDL(LOC,D(IT+NSI)-15.0)/1728.0                                  
      SN = D(IGA+80+NST)                                                        
      I1 = ID(IRCD+6)                                                           
      WTF = D(I1)                                                               
      IDT = ID(IRCD+12)                                                         
      GO TO (1,2,3,4), IDT                                                      
    1 I1 = ID(IRCD+13)                                                          
      WTD = 0.4*D(I1)**2                                                        
      WTCD = 0.5*WTD                                                            
      IF (WTF.NE.0.0) WTCD = WTCD*WTF                                           
      CUC = 5.5*WTCD                                                            
      RIC = 0.02576                                                             
      DRC = 1.15                                                                
      WTDC = 0.164*WTCD                                                         
      VOL = 0.2*D(I1)**4                                                        
      GO TO 10                                                                  
    2 WTD = 2.0+2.3E05*HP**0.8333/SN**1.25                                      
      WTDC = 0.1665*WTD                                                         
c@      CALL MDSCT(2,IND,ED,TABE,SN,TABN,4,0,DV,DV,DV)                            
      CALL MDSCT(2,IND,ED,TABE,SN,TABN,4,0,DV,DV,iDV,idv)                            
   21 WTCD = 1.5*WTD                                                            
      IF (WTF.NE.0.0) WTCD = WTCD*WTF                                           
      CUC = 34.7+17.3*WTCD                                                      
      RIC = 0.00873                                                             
      DRC = 1.0                                                                 
      VOL = 10.0*WTCD                                                           
      ED = (1.0-0.281/HP**0.169)*ED                                             
      HPD = HP/ED                                                               
      HPE = HPE+HPD                                                             
      GO TO 10                                                                  
    3 WTD = 1.5+3.83E05*HP**0.8333/SN**1.25                                     
      WTDC = 0.388*WTD                                                          
c@      CALL MDSCT(2,IND,ED,TABE,SN,TABN,4,1,DV,DV,DV)                            
      CALL MDSCT(2,IND,ED,TABE,SN,TABN,4,1,DV,DV,iDV,idv)                            
      GO TO 21                                                                  
    4 DPR = 139.094*HP/SN                                                       
      DPRU = EXP(2.706-3.453E-4*SN)                                             
      DPRL = EXP(-1.519-3.784E-4*SN)                                            
      WTD = 11.37*SQRT(DPR)                                                     
      IF (DPR.GT.DPRU) WTD = WTD*2.121                                          
      IF (DPR.LT.DPRL) WTD = WTD*1.0605                                         
      IF (WTF.NE.0.0) WTCD = WTCD*WTF                                           
      WTCD = 1.5*WTD                                                            
      IF (WTF.NE.0.0) WTCD = WTCD*WTF                                           
      CUC = 17.0+4.88*WTCD                                                      
      RIC = 0.02576                                                             
      DRC = 1.0                                                                 
      WTDC = 0.3*WTD                                                            
      VOL = 20.0*WTCD                                                           
      ED = 0.82                                                                 
      HPD = HP/ED                                                               
      HPH = HPH+HPD                                                             
   10 I1 = ID(IRCD+16)                                                          
      I2 = ID(IRCD+17)                                                          
      I3 = ID(IRCD+15)                                                          
      WTL = (D(I1)+D(I2)+WTCD+WTR)*(0.1+0.01*D(I3))                             
      WTC = WTCD+WTR+WTRC+WTL                                                   
      WTIC = 0.205*(WTCD+WTR)                                                   
      WTDC = SQRT(WTDC**2+0.0144*(WTR**2+WTRC**2+WTL**2))                       
      I1 = ID(IRCD+7)                                                           
      CUF = D(I1)                                                               
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      I1 = ID(IRCD+8)                                                           
      RI = D(I1)                                                                
      IF (RI.NE.0.0) RIC = RI                                                   
      I1 = ID(IRCD+9)                                                           
      DRF = D(I1)                                                               
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      CALL SSA                                                                  
      CALL SCO                                                                  
      CALL SCI(NL,NSI,NSO)                                                      
      CALL LINES(2)                                                             
      WRITE (OUT,1000) WTCD,WTR,WTRC,WTL,VOL,SN,EC,HP                           
 1000 FORMAT(1H0,5X,4HWTCD,F6.2,3X,3HWTR,F7.2,3X,4HWTRC,F6.2,3X,3HWTL,          
     *F7.2,3X,3HVOL,F7.0,3X,1HN,F9.0,3X,3HEFF,F7.4,3X,2HHP,F8.2)                
      CALL LINES(2)                                                             
      WRITE (OUT,1010) IDT                                                      
 1010 FORMAT(1H0,5X,6HDRIVE ,I4)                                                
      IF (IDT.NE.1) WRITE (OUT,1011) ED,HPD,WTD                                 
 1011 FORMAT(1H+,18X,3HEFF,F7.4,3X,2HHP,F8.2,3X,2HWT,F8.2)                      
      D(IGA+71) = VOL                                                           
   99 CONTINUE                                                                  
      RETURN                                                                    
C     VCMPSP                                                                    
      END                                                                       
*DECK,VCMPSZ                                                                    
      SUBROUTINE VCMPSZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
     *, (SCR(1),C(151)), (IGA,C(35))                                            
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER OUT,CERR                                                          
      EQUIVALENCE (SCR(1),NL)                                                   
      EQUIVALENCE (SCR(30),I1)                                                  
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA /100/                                                           
C   1 COMP CODE                                                                 
C   2 NL                                                                        
C   3 NSI                                                                       
C   4 NSO                                                                       
C   5 NST                                                                       
C   6 WTM                                                                       
C   7 CUM                                                                       
C   8 RI                                                                        
C   9 DRM                                                                       
C  10 VE                                                                        
C  11 VC                                                                        
C  12 IDT                                                                       
C  13 DIA                                                                       
C  14 MAP                                                                       
C  15 L                                                                         
C  16 CWT                                                                       
C  17 EWT                                                                       
      I = IACDB(17)                                                             
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      ID(I+3) = ISTAN(ICV(3))                                                   
      ID(I+4) = ISTAN(ICV(4))                                                   
      CALL FTL(NL,IFTA)                                                         
      ID(I+5) = ICV(5)                                                          
      CALL SSR(ICV(5))                                                          
      ID(I+6) = IPARM(ICV(6))                                                   
      ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      ID(I+10) = IPARM(ICV(10))                                                 
      ID(I+11) = IPARM(ICV(11))                                                 
      IF (ICV(12).GT.0 .AND. ICV(12).LE.4) GO TO 10                             
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR                                                     
 1000 FORMAT(6H0ERROR,I6,18HINVALID DRIVE TYPE)                                 
   10 ID(I+12) = ICV(12)                                                        
      ID(I+13) = IPARM(ICV(13))                                                 
      ID(I+14) = ICV(14)                                                        
      ID(I+15) = IPARM(ICV(15))                                                 
      ID(I+16) = IPARM(ICV(16))                                                 
      ID(I+17) = IPARM(ICV(17))                                                 
      I1 = ICV(5)                                                               
      IF (D(IGA+80+I1).NE.0.0) GO TO 99                                         
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1010) CERR                                                     
 1010 FORMAT(6H0ERROR,I6,23HSHAFT SPEED NOT DEFINED)                            
   99 CONTINUE                                                                  
      RETURN                                                                    
C     VCMPSZ                                                                    
      END                                                                       
C$        DID NOT EXIST IN ORIGINAL FORTRAN                                     
C$        CALCULATES THE THERMAL CONDUCTIVITY OF REFRIGERANT LIQUID             
      FUNCTION VCONDF(L,P,T)                                                    
C CALCULATES THE THERMAL CONDUCTIVITY OF REFRIGERANT LIQUID                     
C UNITS ARE BTU/(HR-FT-F)                                                       
      COMMON/DC/DZ(2),D(128001)                                                  
      VCONDF=1.E-04*(D(L+44)+D(L+45)*T)                                         
      RETURN                                                                    
C  vCONDF                                                                        
      END                                                                       
C$        DID NOT EXIST IN ORIGINAL FORTRAN                                     
C$        CALCULATES THE THERMAL CONDUCTIVITY OF REFRIGERANT VAPOR              
      FUNCTION VCONDV(L,P,T)                                                    
C CALCULATES THE THERMAL CONDUCTIVITY OF REFRIGERANT VAPOR                      
C UNITS ARE BTU/(HR-FT-F)                                                       
      COMMON /DC/ DZ(2),D(128001)                                               
      VCONDV=1.E-05*(D(L+49)+D(L+50)*T+D(L+51)*T*T)                             
      RETURN                                                                    
C  vCONDV                                                                        
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C$    FUNCTION VCPF(L,P,T)   DID NOT EXIST IN ORIGINAL FORTRAN                  
C$        COMPUTES CP OF SUPERHEATED VAPOR FOR GIVEN P AND T                    
C$        COMPUTES CP OF LIQUID FOR GIVEN P AND T                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VCPF(L,P,T)                                                      
      COMMON/DC/DZ(2),D(128001)                                                 
      IF(T.GE.D(L+2)) GO TO 99                                                  
      VCPF=VHLIQ(L,P,T)-VHLIQ(L,P,T-1.0)                                        
      RETURN                                                                    
   99 VCPF=0.0                                                                  
      RETURN                                                                    
C  VCPF                                                                         
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C$    FUNCTION VCPV(L,P,T)                                                      
C$        COMPUTES CP OF SUPERHEATED VAPOR FOR GIVEN P AND T                    
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VCPV(L,P,T)                                                      
      COMMON /DC/ DZ(2),D(128001)                                               
      TH=T+0.5                                                                  
      TL=T-0.5                                                                  
      VU=VSV('VCPV 2  ',L,P,TH)                                                 
      VL=VSV('VCPV 1  ',L,P,TL)                                                 
      HH=VH(L,P,TH,VU)                                                          
      HL=VH(L,P,TL,VL)                                                          
      VCPV=HH-HL                                                                
      RETURN                                                                    
C  VCPV                                                                         
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES REFRIGERANT LIQUID DENSITY                        
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VDL(L,T)                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      A = 1.0-T/D(L+2)                                                          
      VDL = D(L+3)                                                              
      DO 2 I=1,4                                                                
    2 VDL = VDL+D(L+2+2*I)*A**D(L+3+2*I)                                        
      RETURN                                                                    
C     VDL                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE                                                            
C$    FUNCTION VDPDT(L,P)    DID NOT EXIST IN ORIGINAL FORTRAN                  
C$        COMPUTES CP OF LIQUID FOR GIVEN P AND T                               
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VDPDT(L,P)                                                       
C UNITS ARE PSI/F                                                               
      COMMON/DC/DZ(2),D(128001)                                                 
      T = VTS(L,P)                                                              
C$       SAME TYPE FIX AS WAS REQD IN VTS                                       
C$     CHANGED SIGNS FOR B AND C COEFF (L+13,L+14) TO MATCH CONVENTION          
      VDPDT=P*(2.302585*(-D(L+13)-D(L+16)*D(L+17)*ALOG10                        
     *(ABS(D(L+17)-T)))/T**2-(-D(L+14)+D(L+16))/T+2.302585*D(L+15))             
      RETURN                                                                    
C  VDPDT                                                                        
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE                                                            
C$        CALCULATES REFRIGERANT ENTHALPY FOR SUPERHEATED VAPOR                 
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VH(L,P,T,V)                                                      
      COMMON /DC/ DZ(2),D(128001)                                               
      VMB = V-D(L+18)                                                           
      A = D(L+31)*T/D(L+2)                                                      
      A = .185053*EXP(-A)*(1.0+A)                                               
      VH = -D(L+36)/T+((((0.25*D(L+35)*T+.3333333*D(L+34))*T+0.5*D(L+33)        
     *)*T+D(L+32))*T)                                                           
     *+.185053*(P*V+((((0.25*D(L+28)/VMB+.3333333*D(L+25))/VMB+0.5*             
     *D(L+22))/VMB+D(L+19))/VMB))                                               
     *+A*((((0.25*D(L+30)/VMB+.3333333*D(L+27))/VMB+0.5*D(L+24))/VMB+           
     *D(L+21))/VMB)                                                             
     *+D(L+37)                                                                  
      RETURN                                                                    
C     VH                                                                        
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE                                                            
C$        COMPUTES REFRIGERANT HEAT OF VAPORIZATION                             
C$        MODIFIED TO COMPUTE HFG FOR TWO-PHASE COMPRESSOR OUTLET               
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VHFG(L,P,T,V)                                                    
      COMMON/DC/DZ(2),D(128001)                                                 
      IF(T.GE.D(L+2)) GO TO 10                                                  
      DEN = VDL(L,T)                                                            
      EFT = 0.0                                                                 
      IF (D(L+17).NE.0.0) EFT =                                                 
     *(D(L+16)/T+D(L+16)*D(L+17)*ALOG(ABS(D(L+17)-T))/T**2)/2.302585            
C$       SAME TYPE FIX AS WAS REQD IN VTS                                       
C$     CHANGED SIGNS FOR B AND C COEFF (L+13,L+14) TO MATCH CONVENTION          
      VHFG = .185053*(V-1.0/DEN)*T*P*2.302585*(-D(L+13)/T**2                    
     *+D(L+14)/(T*2.302585)+D(L+15)-EFT)                                        
      RETURN                                                                    
   10 VHFG = 0.0                                                                
      RETURN                                                                    
C  VHFG                                                                         
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C$    FUNCTION VHLIQ(L,P,T)                                                     
C$        CALLED BY FTB1'S CVLVPP,NRLRPP,INLTPP,REFPPP                          
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VHLIQ(L,P,T)                                                     
      COMMON /CC/ C(600)                                                        
      COMMON/DC/DZ(2),D(128001)                                                 
      EQUIVALENCE (OUT,C(7))                                                    
      INTEGER OUT                                                               
      PSAT=VPS(L,T)                                                             
      V=VSV('VHLIQ 1 ',L,PSAT,T)                                                
      H=VH(L,PSAT,T,V)                                                          
      HFG=VHFG(L,PSAT,T,V)                                                      
      VL=1./VDL(L,T)                                                            
      VHLIQ=H - HFG + (P - PSAT)*VL*0.185053                                    
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9099                                              
      WRITE (OUT,9000)                                                          
 9000 FORMAT(' FROM VHLIQ    INPUTS ARE L,P,T ')                                
      WRITE (OUT,9010) L     , P     , T     , PSAT                             
     *               , V     , H     , HFG   , VL                               
     *               , VHLIQ                                                    
 9010 FORMAT(' L     =', I8,4X,  ' P     =', E12.5,  ' T     =', E12.5          
     *      ,' PSATVPS', E12.5,/,' V VSV =', E12.5,  ' H  VH =', E12.5          
     *      ,' HFGVHFG', E12.5,  ' VL/VDL=', E12.5,/,' VHLIQ =', E12.5          
     *      ,' VHLIQ=H - HFG + (P - PSAT)*VL*0.185053')                         
 9099 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      RETURN                                                                    
C  VHLIQ                                                                        
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES VISCOSITY FOR LIQUIDS AND GASES                   
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VIS (NL,P,T)                                                     
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
      VIS = 10.0**POLYI(2,D(IN+7),TA)                                           
      GO TO 99                                                                  
    2 CONTINUE                                                                  
      VIS = POLYI(5,D(IN+8),TA)                                                 
      GO TO 99                                                                  
    3 CONTINUE                                                                  
      VIS = BIG                                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     VIS                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      A VAPOR LINE                                                             
C                                                                               
C$               NEEDS FUNCTIONS "P", "VV", "M", BROKEN OUT OR                  
C$                INCORPORATED                                                  
C**********************************************************************         
C                                                                               
      SUBROUTINE VLNEPP                                                         
      REAL K,L,LE,LI,M,MI,MO                                                    
      COMMON /CC/C(600)                                                         
      EQUIVALENCE (IRCD,C(45)),(IW,C(27)),(IP,C(28)),(IT,C(29))                 
     *,(IH,C(30)),(SCR(1),C(151)),(IFB,C(55)),(IGA,C(35))                       
     *,(OUT,C(7))                                                               
C$       ADD FOR WRITE OF VLINE COMPONENT AT END OF CASE BELOW                  
     *,(IFP,C(22)),(ICPP,C(88))                                                 
      EQUIVALENCE (SCR(1),NL),(SCR(2),NSI),(SCR(3),NSO),(SCR(4),ISIG)           
     *,(SCR(5),LOCT),(SCR(6),UAPDL),(SCR(7),DH),(SCR(8),L)                      
     *,(SCR(9),K),(SCR(10),LE),(SCR(11),TS)                                     
      DIMENSION SCR(30)                                                         
      INTEGER PASS,OUT                                                          
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 TABLE/EQUATION OPTION                                                     
C   6 FRICTION FACTOR TABLE NO                                                  
C   7 HEAT LOSS FACTOR (UAPDL) IN BTU/HR-F DIVIDED BY PI*DH*LENGTH              
C   8 HYDRAULIC DIAMETER IN FEET (DH)                                           
C   9 LINE LENGTH IN FEET (L)                                                   
C  10 VELOCITY HEAD LOSS COEFFICIENT (K)                                        
C  11 EQUIVALENT-LENGTH LOSS COEFFICIENT (LE)                                   
C  12 SOURCE/SINK TEMPERATURE RANKINE (TS)                                      
C*** THE FOLLOWING FUNCTION COMPUTES THE AVERAGE TWO-PHASE FRICTION LOSS        
C    BASED ON A SMALL CHANGE IN QUALITY (X)                                     
C$ FUNCTION "PFXPB" IS USED TWICE (AT LABELS 400 AND 521)                       
      P(X,PB)=-PB*X**1.65+(1+PB)*(X**.7+X*(1.-X)**1.3)                          
C*** THE FOLLOWING FUNCTION COMPUTES THE VOID FRACTION                          
C$ FUNCTION "VV" IS USED TWICE (JUST AFTER LABELS 400 AND 521)                  
      VV(X)=X/(X+SL*(1.-X)/VR)                                                  
C*** THE FOLLOWING FUNCTION COMPUTES THE MOMENTUM OF THE FLOW                   
C$ FUNCTION "M" IS USED TWICE (JUST AFTER VV FUNCTIONS)                         
      M(X)=((1.-X)/(1.-V))**2*(1.-V*(1.-SL*SL/VR))                              
C$                                                                              
C$ FUNCTION "FAKE" USED TO CHECK THIS OUT                                       
C     FAKE(FKTEMP,IPOWER)=FKTEMP**IPOWER                                        
C*** SET UP THE INTERNAL VARIABLES IN TERMS OF THE STORED ARRAY                 
      IRCD = IRCDB(12)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      LOCT = ID(IFB+NL)                                                         
      LOCT = ID(LOCT+3)                                                         
      UAPDL=D(ID(IRCD+7))                                                       
      DH=D(ID(IRCD+8))                                                          
      L=D(ID(IRCD+9))                                                           
      K=D(ID(IRCD+10))                                                          
      LE=D(ID(IRCD+11))                                                         
      TS=D(ID(IRCD+12))                                                         
      W=D(IW+NL)                                                                
      PI=D(IP+NSI)                                                              
      TI=D(IT+NSI)                                                              
      HIN=D(IH+NSI)                                                             
C$                                                                              
C$    FKTEMP=500.                                                               
C$    IPOWER=2.                                                                 
C$    FAKVAL=FAKE(FKTEMP,IPOWER)                                                
C$                                                                              
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C     IRITE=0                                                                   
C     IF (IRITE .NE. 1) GO TO 9099                                              
C     WRITE (OUT,9000) FKTEMP, IPOWER, FAKVAL                                   
C9000 FORMAT('  FKTEMP=', E11.4, '  IPOWER=', I6,5X, '  FAKVAL=', E11.4)        
C9099 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C*** COMPUTE THE STATURATION DENSITIES                                          
      TISAT=VTS(LOCT,D(IP+NSI))                                                 
      IF(TI .LE. TISAT) TISAT=TI                                                
      VF=1.0/VDL(LOCT,TISAT)                                                    
      VG=VSV('VLNEPP  ',LOCT,PI,TI)                                             
C*** COMPUTE THE REYNOLDS NUMBER FOR THE LIQUID-ONLY FLOWING                    
      GD=W/(60.*3.14159*DH/4.)                                                  
      G=GD/DH                                                                   
      D(IGA+21)=GD/VVISCF(LOCT,PI,TI)                                           
      REYNF=D(IGA+21)                                                           
C*** LOOK UP THE FRICTION FACTOR                                                
      F=TLUP(ID(IRCD+6))                                                        
      FF=F                                                                      
C*** COMPUTE THE INLET PRESSURE DROP IF LIQUID-ONLY FLOWS                       
C$       SHOULD BE "G**2" BELOW, WAS "GD**2"                                    
C$       LI (AND GI BELOW) ARE IN LBF/IN**2, WAS PER FOOT SQUARED               
C$WAS LI=(4.*F*(L+LE)/DH+K)*VF*GD**2/(2*32.2)                                   
      LI=(4.*F*(L+LE)/DH+K)*(VF*G**2)/(2*32.2*144.)                             
C*** REPEAT FOR VAPOR-ONLY FLOWING                                              
      D(IGA+21)=GD/VVISCV(LOCT,PI,TI)                                           
      REYNV=D(IGA+21)                                                           
      F=TLUP(ID(IRCD+6))                                                        
      FV=F                                                                      
C*** COMPUTE THE INLET PRESSURE DROP IF VAPOR-ONLY FLOWS                        
C$       SHOULD BE "G**2" BELOW, WAS "GD**2"                                    
C$WAS GI=(4.*F*(L+LE)/DH+K)*VG*GD**2/(2*32.2)                                   
      GI=(4.*F*(L+LE)/DH+K)*VG*G**2/(2*32.2*144.)                               
C*** COMPUTE THE TWO-PHASE FLOW PARAMETERS: DENSITY RATIO,                      
C    PRESSURE-DROP RATIO, SLIP AND QUALITY:                                     
      VR=VG/VF                                                                  
      PB=.25*.4343*ALOG(LI/GI)                                                  
      IF(PB.LT.-1.)PB=-1.                                                       
      SL=SQRT(SQRT(VR-1.))                                                      
      X=VQUALH('VLNEPP 1',LOCT,PI,HIN)                                          
      XIN=X                                                                     
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9098                                              
      XMUF=VVISCF(LOCT,PI,TI)                                                   
      XMUV=VVISCV(LOCT,PI,TI)                                                   
      F4LOVD= (4.*F*(L+LE)/DH)                                                  
      QLIPSI=(VF*G**2)/(2*32.2*144.)                                            
      WRITE (OUT,8999)                                                          
 8999 FORMAT('      WRITES FROM VLNEPP     ')                                   
      WRITE (OUT,9100) VF    , VG    , GD    , REYNF                            
 9100 FORMAT('  VF    =', E11.4, '  VG    =', E11.4, '  GD    =', E11.4         
     *      ,'  REYNF =', E11.4)                                                
      WRITE (OUT,9101) FF    , LI    , REYNV , FV                               
 9101 FORMAT('  FF    =', E11.4, '  LI    =', E11.4, '  REYNV =', E11.4         
     *      ,'  FV    =', E11.4)                                                
      WRITE (OUT,9102) GI    , VR    , PB    , SL                               
 9102 FORMAT('  GI    =', E11.4, '  VR    =', E11.4, '  PB    =', E11.4         
     *      ,'  SL    =', E11.4)                                                
      WRITE (OUT,9103) X     , W     , HIN   , LE                               
 9103 FORMAT('  X     =', E11.4, '  W     =', E11.4, '  HIN   =', E11.4         
     *      ,'  LE    =', E11.4)                                                
      WRITE (OUT,9104) XMUF  , XMUV  , F4LOVD, QLIPSI                           
 9104 FORMAT('  XMUF  =', E11.4, '  XMUV  =', E11.4, '  F4LOVD=', E11.4         
     *      ,'  QLIPSI=', E11.4)                                                
 9098 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF(X)380,380,390                                                          
C*** LIQUID-ONLY IS FLOWING (SINGLE PHASE)                                      
  380 DI=LI                                                                     
      MI=1.                                                                     
      GO TO 460                                                                 
  390 IF(X-1.)400,395,395                                                       
C*** VAPOR-ONLY IS FLOWING (SINGLE-PHASE)                                       
  395 DI=GI                                                                     
      MI=VR                                                                     
      GO TO 460                                                                 
C*** TWO-PHASE FLOW                                                             
C*** ADD P(X,PB) LINE HERE                                                      
C$400 PFXPB=-PB*X**1.65+(1+PB)*(X**.7+X*(1.-X)**1.3)                            
  400 DI=LI+P(X,PB)*(GI-LI)                                                     
      XPICHK=P(X,PB)                                                            
C*** NEXT COMPUTE THE MOMENTUM LOSS (OR GAIN)                                   
      V=VV(X)                                                                   
      VIN=V                                                                     
      MI=M(X)                                                                   
C*** COMPUTE THE HEAT LOSS (OR GAIN) FROM THE LINE                              
  460 QQ=UAPDL*3.14159*DH*L*(TS-TI)                                             
      HOUT=HIN+QQ/(W*60.)                                                       
      X=VQUALH('VLNEPP 2',LOCT,PI,HOUT)                                         
      XOUT=X                                                                    
      IF(X)510,510,520                                                          
C*** LIQUID-ONLY IS FLOWING AT THE OUTLET                                       
  510 DO=LI                                                                     
      MO=1                                                                      
      GO TO 560                                                                 
  520 IF(X.LT.1.)GO TO 521                                                      
C*** VAPOR-ONLY IS FLOWING AT THE OUTLET                                        
      DO=GI                                                                     
      MO=VR                                                                     
      GO TO 560                                                                 
C*** THE FLOW IS TWO-PHASE AT THE OUTLET                                        
C    COMPUTE THE FRICTION AND THEN THE MOMENTUM LOSS                            
  521 DO=LI+P(X,PB)*(GI-LI)                                                     
      XPOCHK=P(X,PB)                                                            
      V=VV(X)                                                                   
      VOUT=V                                                                    
      MO=M(X)                                                                   
C*** COMBINE THE FOREGOING TO OBTAIN THE OUTLET PRESSURE                        
C$ WAS  "GD" BELOW CHANGE TO "G"                                                
C$ WAS  "MI-MO" BELOW CHANGE TO "MO-MI" (IF MOMENTUM GOES UP,DP GOES UP)        
C$      "MO-MI" TERM IS TIMES "RHO*VEE**2" NOT "1/2*RHO*VEE**2"                 
C$      THAT IS WHY "QHEADF" IS MULTIPLIED BY TWO IN THE "DPMOME" TERM.         
  560 QHEADF=G**2*VF/(2*32.2*144.)                                              
      DPFRIC=(DI+DO)/2.                                                         
      DPMOME=(MO-MI)*(QHEADF*2.)                                                
      DPTOTL= DPFRIC+DPMOME                                                     
      POUT=PI - DPTOTL                                                          
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9198                                              
      WRITE (OUT,9110) UAPDL , DH    , L     , TS, TI                           
 9110 FORMAT('  UAPDL =', E11.4, '  DH    =', E11.4, '  L     =', E11.4         
     *      ,' TS=', F7.3, ' TI=',F7.3)                                         
      WRITE (OUT,9111) DI    , VIN   , MI    , QQ                               
 9111 FORMAT('  DI    =', E11.4, '  VIN   =', E11.4, '  MI    =', E11.4         
     *      ,'  QQ    =', E11.4)                                                
      WRITE (OUT,9112) HOUT  , XOUT  , DO    , VOUT                             
 9112 FORMAT('  HOUT  =', E11.4, '  XOUT  =', E11.4, '  DO    =', E11.4         
     *      ,'  VOUT  =', E11.4)                                                
      WRITE (OUT,9113) MO    , POUT  , V     , XPICHK                           
 9113 FORMAT('  MO    =', E11.4, '  POUT  =', F11.4, '  V     =', E11.4         
     *      ,'  XPICHK=', E11.4)                                                
      WRITE (OUT,9114) XPOCHK, G     , DPFRIC, DPMOME                           
 9114 FORMAT('  XPOCHK=', E11.4, '  G     =', E11.4, '  DPFRIC=', E11.4         
     *      ,'  DPMOME=', E11.4)                                                
      WRITE (OUT,9115) QHEADF, DPTOTL                                           
 9115 FORMAT('  QHEADF=', E11.4, '  DPTOTL=', E11.4)                            
 9198 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF(POUT)570,570,580                                                       
  570 WRITE(OUT,2200)                                                           
 2200 FORMAT(' *+* PRESSURE OUT OF VLINE WAS LESS THAN 0. *+*')                 
      WRITE(OUT,2201)POUT,PI                                                    
 2201 FORMAT(' *+* OUTLET PRESSURE ',1PE12.5,' WAS SET TO INLET PRESSURE        
     * ',1PE12.5)                                                               
      POUT=PI                                                                   
  580 D(IP+NSO)=POUT                                                            
  590 D(IH+NSO)=HOUT                                                            
      D(IW+NL)=W                                                                
C$                                                                              
C$ WAS  D(IT+NSO)=TI                                                            
      IF(X.GE.0.)GO TO 5001                                                     
C$       ADD CALC OF OUTLET TEMP FOR LIQUID-ONLY OUTLET                         
      D(IT+NSO)=VTLIQ(LOCT,D(IP+NSO),HOUT)                                      
      GO TO 5003                                                                
 5001 IF(X.LE.1.)GO TO 5002                                                     
C$       ADD CALC OF OUTLET TEMP FOR VAPOR-ONLY OUTLET                          
      CALL VTAV2(LOCT,D(IT+NSO),VOLSV,D(IP+NSO),D(IH+NSO))                      
      GO TO 5003                                                                
C$       ADD CALC OF OUT TEMP FOR TWO-PHASE OUTLT                               
 5002 D(IT+NSO)=VTS(LOCT,D(IP+NSO))                                             
 5003 CONTINUE                                                                  
C$                                                                              
C$       WRITE IT OUT!!                                                         
C$    IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
C$    IF (IFP.NE.1) GO TO 99                                                    
C$    NEITHER OF THE 2 LINES JUST ABOVE EVER WROTE ANYTHING OUT                 
C$    IF (ICPP.NE.0) GO TO 99                                                   
C$    WITH ABOVE IF IT WROTE FOR ALL PASSES                                     
C$    WITHOUT ANY "IF" IT WROTE FOR ALL PASSES                                  
C$    ADD EQUIVALENCES FOR IFP AND ICPP ABOVE AS IN OTHER SUBROUTINES           
      IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      CALL PIOP(1,NL,NSI,NSO)                                                   
      CALL LINES(2)                                                             
      POUTPI = POUT-PI                                                          
      HLOSS = D(IH+NSI)-D(IH+NSO)                                               
      QLOSS = W * HLOSS                                                         
      WRITE(OUT,591) XIN,XOUT,POUTPI,QLOSS                                      
  591 FORMAT(6X,'QUALI=', E12.5, '   QUALO=', E12.5,                            
     *'    DELTA P=',E12.5,'    Q LOSS=',E12.5,                                 
     *' BTU/MIN.')                                                              
   99 CONTINUE                                                                  
      RETURN                                                                    
C     VLNEPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A VAPOR LINE                                              
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE VLNEPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
C$                                            (SCR(3),NSO)                      
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(30),NSO)                     
      DIMENSION ICV(18), SCR(30)                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/100/                                                            
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 TABLE/EQUATION OPTION                                                     
C$         6 WAS LAST ICV AND WAS "TABLE NO"                                    
C   6 FRICTION FACTOR TABLE NUMBER                                              
C   7 HEAT LOSS FACTOR (UAPOL)                                                  
C   8 HYDRAULIC DIAMETER (DH)                                                   
C   9 LINE LENGTH IN FEET (L)                                                   
C  10 VELOCITY HEAD LOSS COEFFICIENT (K)                                        
C  11 EQUIVALENT LENGTH LOSS COEDDICEIENT (LE)                                  
C  12 SOURCE/SINK TEMPERATURE RANKINE (TS)                                      
      I = IACDB(12)                                                             
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      CALL LEGRT(NL)                                                            
      NSI = ISTAN(ICV(3))                                                       
      ID(I+3) = NSI                                                             
      CALL START(NSI)                                                           
      NSO = ISTAN(ICV(4))                                                       
      ID(I+4) = NSO                                                             
      CALL STARS(NSO)                                                           
      CALL FTL(NL,IFTA)                                                         
      ID(I+5) = ICV(5)                                                          
      ID(I+6) = ITIDN(ICV(6),3)                                                 
      ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      ID(I+10)= IPARM(ICV(10))                                                  
      ID(I+11)= IPARM(ICV(11))                                                  
      ID(I+12)= IPARM(ICV(12))                                                  
   99 CONTINUE                                                                  
      RETURN                                                                    
C     VLNEPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C     A FIXED VALVE                                                             
C                                                                               
C$         MODIFY PREG TO WORK WITH VAPOR COMPONENTS TO REPLACE                 
C$              IDIOTIC AND UNSTABLE CVALVE/SENSOR COMBINATION WHICH            
C$              IS USED TO MODEL AN EXPANSION VALVE                             
C**********************************************************************         
C                                                                               
      SUBROUTINE VLVPP                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)), (SCR(1),C(151)), (ICPP,C(88)), (OUT,C(7))                   
     *, (IFP,C(22)), (IGA,C(35))                                                
C$        ADD IFB FOR FLUID TYPE                                                
     *, (IFB,C(55))                                                             
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
     *, (SCR(4),PD), (SCR(5),PREG), (SCR(6),IOP)                                
     *, (SCR(7),HS)                                                             
C$ ADD                                                                          
     *, (SCR(10),V)                                                             
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C20 3 STATION IN                                                                
C24 4 STATION OUT                                                               
C28 5 SIGMA OPTION                                                              
C32 6 EQUATION-TABLE OPTION                                                     
C36 7 PD TABLE / KT / K                                                         
C40 8 DIAMETER                                                                  
C44 9 PREG                                                                      
C48 10 efficiency     ! added for hyd actuators 6/01 sfw
      IE = 1                                                                    
      GO TO 4                                                                   
      ENTRY PREGPP                                                              
      IE = 2                                                                    
    4 IRCD = IRCDB(10)  ! NUMBER OF PARAMS ABOVE                                
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      D(IGA+41) = D(IW+NL)                                                      
      D(IGA+35) = D(IW+NL)*SQRT(D(IT+NSI))/D(IP+NSI)                            
      IOP = ID(IRCD+6)                                                          
C$        ADD FLUID TYPE DETERMINATION                                          
      NC=ID(IFB+NL)                                                             
      LOCT=ID(NC+3)                                                             
      NC=ID(NC+1)                                                               
      IF (IOP.EQ.1) GO TO 5                                                     
      IF (IOP.EQ.2) GO TO 6                                                     
      GO TO 1                                                                   
    5 PD = 1.008E-3*D(ID(IRCD+7))*D(IW+NL)**2/(DEN(NL,D(IP+NSI),                
     *D(IT+NSI))*D(ID(IRCD+8))**4)                                              
      GO TO 2                                                                   
    6 PD = D(ID(IRCD+7))*D(IW+NL)**2                                            
      GO TO 7                                                                   
    1 PD = TLUP(ID(IRCD+7))                                                     
      IF (ID(IRCD+5).LT.0) PD = PD*D(IP+NSI)                                    
    7 IF (ID(IRCD+5).GT.0) PD = PD/SIG(NL,D(IP+NSI),D(IT+NSI))                  
    2 D(IP+NSO) = D(IP+NSI) - PD                                                
      IF (IE.EQ.1) GO TO 3                                                      
      PREG = D(ID(IRCD+9))                                                      
      IF (D(IP+NSO).GT.PREG) D(IP+NSO) = PREG                                   
      D(IH+NSO) = D(IH+NSI)                                                     
C$        DETERMINE OUTLET TEMP BASED ON OUTLET P&H (QUAL) AS IN CVLVPP         
C$        NEED TO ADD LOCT, V                                                   
    3 IF (NC .NE. 3) GO TO 12                                                   
      QUAL=VQUALH('CVLVPP  ',LOCT,D(IP+NSO),D(IH+NSI))                          
      IF(QUAL.LT.0.0)D(IT+NSO)=VTLIQ(LOCT,D(IP+NSO),D(IH+NSI))                  
      IF(QUAL.GE.0..AND.QUAL.LE.1.)D(IT+NSO)=VTS(LOCT,D(IP+NSO))                
      IF(QUAL.GT.1.)CALL VTAV2(LOCT,D(IT+NSO),V,D(IP+NSO),D(IH+NSI))            
C$        ALSO DO PRINTOUT FOR PREG WHEN USED WITH REFRIGERANT                  
      IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      CALL PIOP(1,NL,NSI,NSO)                                                   
      GO TO 99                                                                  
C   12 D(IT+NSO) = D(IT+NSI)                                                     
   12 IF (NC .NE. 1) GO TO 912 
      CPIN  = SHP(NL,D(IP+NSI), D(IT+NSI), D(IH+NSI))                                    
      DENSIN= DEN(NL,D(IP+NSI),D(IT+NSI))                                       
C   IT IS LIQUID, ADD HEAT RISE DUE TO PRESSURE LOSS TERM
	IF(ID(IRCD+10).EQ.0) GO TO 111
	EFF = TLUP(ID(IRCD+10))                                                     
 111	D(IT+NSO)=D(IT+NSI)+(((1.0000-EFF) * D(IP+NSI)-D(IP+NSO))*144.   !effieciency from table 
     *	/(CPIN*778.*DENSIN))
      GO TO 13	 
  912 D(IT+NSO) = D(IT+NSI) 
   13 IF (D(IH+NSI).NE.0.0) CALL TDB(NL,NSI,NSO,HS)                             
      IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      CALL PIOP(1,NL,NSI,NSO)                                                   
      WRITE(6,100) EFF
  100 FORMAT('      EFFICIENCY=', F8.3, ' (FRACTION OF TEMP RISE'
     * , ' DUE TO PRESSURE DROP NOT APPEARING AS HEAT)' )
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HS) CALL HSOP(HS)                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     VLVPP, PREGPP                                                             
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A FIXED VALVE                                             
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE VLVPZ                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C$          CHANGE IFTA FROM 011 TO 111 TO ALLOW REFRIGERANT                    
C$ "111" IS IN REVERSE ORDER; THE FIRST "1" REPRESENTS FLUID TYPE 3,            
C$         A REFRIGERANT, WHILE THE THIRD "1" REPRESENTS TYPE 1,A LIQUID        
      DATA IFTA/111/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C20 3 STATION IN                                                                
C24 4 STATION OUT                                                               
C28 5 SIGMA OPTION                                                              
C32 6 EQUATION-TABLE OPTION                                                     
C36 7 PD TABLE / KT / K                                                         
C40 8 DIAMETER                                                                  
C44 9 PREG                                                                      
C48 10 efficiency     ! added for hyd actuators 6/01 sfw
      IE = 1                                                                    
      GO TO 10                                                                  
      ENTRY PREGPZ                                                              
      IE = 2                                                                    
   10 I = IACDB(10)         ! 10 PARAMS NOW                                                              
	IF(ICV(10).EQ.0) GO TO 11
	ID(I+10)= ITIDN(ICV(10),17)     !type 17 is efficiency                                                 
11    ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      CALL LEGRT(NL)                                                            
      NSI = ISTAN(ICV(3))                                                       
      ID(I+3) = NSI                                                             
      CALL START(NSI)                                                           
      NSO = ISTAN(ICV(4))                                                       
      ID(I+4) = NSO                                                             
      CALL STARS(NSO)                                                           
      CALL FTL(NL,IFTA)                                                         
      ID(I+5) = ICV(5)                                                          
      ID(I+6) = ICV(6)                                                          
      IF (IE.EQ.2) ID(I+9) = IPARM(ICV(9))                                      
      IF (ICV(6).EQ.1) GO TO 1                                                  
      IF (ICV(6).EQ.2) GO TO 2                                                  
      ID(I+7) = ITIDN(ICV(7),1)                                                 
C     GO TO 99                                                                  
    1 ID(I+8) = IPARM(ICV(8))                                                   
    2 ID(I+7) = IPARM(ICV(7))                                                   
   99 CONTINUE                                                                  
      RETURN                                                                    
C$       WRITES BELOW ARE AFTER STATEMENT 5 IN OLD VCOMPP                       
C$           ADD WRITES TO FIND OUT WHY THE OUTLET PRESS GOES                   
C$            TO ZERO FOR VARIABLE PRESSURE RATIO                               
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C     IRITE=0                                                                   
C     IF (IRITE .NE. 1) GO TO 9099                                              
C     WRITE (OUT,9000) I1    , D(I1) , PR    , D(IP+NSI)                        
C9000 FORMAT('  I1    =', I6,5X, '  D(I1) =', E11.4, '  PR    =', E11.4         
C    *      ,'  D(IP+NSI', E11.4)                                               
C     WRITE (OUT,9010) PASS  , IOP   , ID(ISVT+JSVI), D(ISV+JSVI)               
C9010 FORMAT('  PASS  =', I6,5X, '  IOP   =', I6,5X, 'ID(ISVT+JSVI)=',          
C    *   I6,5X  ,/'  D(ISV+JSVI)=', E11.4)                                      
C     WRITE (OUT,9020) ISV   , JSVI  , ISVT                                     
C9020 FORMAT('  ISV   =', I6,5X, '  JSVI  =', I6,5X, '  ISVT  =',               
C    *   I6,5X  )                                                               
C9099 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C     VLVPZ, PREGPZ                                                             
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      VALVES                                                                   
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE VLVSP                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151))                     
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (IFB,C(55)), (WTC,C(102))           
     *, (CUC,C(104)), (RIC,C(106)), (DRC,C(108)), (WTIC,C(109))                 
     *, (WTDC,C(111)), (TLM,C(120))                                             
     *, (IGA,C(35))                                                             
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO), (SCR(4),DIA)         
     *, (SCR(5),IVT), (SCR(6),AKT), (SCR(7),RHO), (SCR(8),VEL)                  
     *, (SCR(9),IFT), (SCR(10),AMN), (SCR(11),C1), (SCR(12),C2)                 
     *, (SCR(13),WTF), (SCR(14),DW), (SCR(15),TEMP), (SCR(16),PRES)             
     *, (SCR(17),DRF), (SCR(18),CUF), (SCR(19),RI)                              
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 WTF                                                                       
C   6 CUF                                                                       
C   7 RI                                                                        
C   8 DRF                                                                       
C   9 DIA                                                                       
C  10 IVT                                                                       
      IRCD = IRCDB(10)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      DIA = D(ID(IRCD+9))                                                       
      IVT = ID(IRCD+10)                                                         
      GO TO (1,1,3,4,5,6), IVT                                                  
    1 AKT = 0.43                                                                
      GO TO 11                                                                  
    3 AKT = 2.74                                                                
      GO TO 11                                                                  
    4 AKT = 3.4                                                                 
      GO TO 11                                                                  
    5 AKT = 6.5                                                                 
      GO TO 11                                                                  
    6 AKT = 0.68                                                                
   11 RHO = DEN(NL,D(IP+NSI),D(IT+NSI))                                         
      IF (DIA.NE.0.0) GO TO 10                                                  
      DIA = (1.008E-3*AKT*D(IW+NL)**2/(RHO*(D(IP+NSI)-D(IP+NSO))))**0.25        
   10 VEL = 3.055774*D(IW+NL)/(RHO*DIA**2)                                      
      IFT = ID(IFB+NL)                                                          
      IFT = ID(IFT+1)                                                           
      IF (IFT.EQ.1) GO TO 12                                                    
      AMN = VEL/SOS(NL,D(IP+NSI),D(IT+NSI))                                     
      GO TO (21,22,23,23,23,26), IVT                                            
   21 IF (D(IT+NSI).GE.TLM) GO TO 31                                            
C  21 WRITE(6,9999) D(IT+NSI),TLM                                               
C9999 FORMAT(' ','D(IT+NSI)=',E16.8,'TLM=',E16.8)                               
C     IF (D(IT+NSI).GE.TLM) GO TO 31                                            
      IF (DIA.GT.2.5) GO TO 32                                                  
      C1 = 0.70                                                                 
      C2 = 0.35                                                                 
      GO TO 33                                                                  
   32 C1 = 1.26                                                                 
      C2 = -1.05                                                                
      GO TO 33                                                                  
   31 C1 = 2.68                                                                 
      C2 = -1.0                                                                 
      GO TO 33                                                                  
   22 IF (D(IT+NSI).GE.TLM) GO TO 34                                            
      C1 =0.56                                                                  
      C2 = 1.1                                                                  
      GO TO 33                                                                  
   34 C1 = 1.14                                                                 
      C2 = 0.9                                                                  
      GO TO 33                                                                  
   23 IF (D(IT+NSI).GE.TLM) GO TO 35                                            
      C1 = 0.833                                                                
      C2 = -0.05                                                                
      GO TO 33                                                                  
   35 C1 = 2.04                                                                 
      C2 = 0.225                                                                
      GO TO 33                                                                  
   26 IF (D(IT+NSI).GE.TLM) GO TO 36                                            
      C1 = 0.33                                                                 
      C2 = -0.38                                                                
      GO TO 33                                                                  
   36 C1 = 0.625                                                                
      C2 = -0.625                                                               
   33 WTC = C1*DIA+C2                                                           
      IF (IVT.EQ.1 .AND. WTC.LT.0.5) WTC = 0.5                                  
      IF (IVT.GE.3 .AND. IVT.LE.5 .AND. WTC.LT.0.3) WTC = 0.3                   
      IF (IVT.EQ.6 .AND. WTC.LT.0.25) WTC = 0.25                                
      WTF = D(ID(IRCD+5))                                                       
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      GO TO (41,42,43,43,43,46), IVT                                            
   41 RIC = 0.0461                                                              
      IF (D(IT+NSI).GE.TLM) GO TO 51                                            
      WTDC = 0.339*WTC                                                          
      GO TO 61                                                                  
   51 WTDC = 0.35*WTC                                                           
      GO TO 61                                                                  
   42 RIC = 0.07649                                                             
      IF (D(IT+NSI).GE.TLM) GO TO 52                                            
      WTDC = 0.174*WTC                                                          
      GO TO 61                                                                  
   52 WTDC = 0.201*WTC                                                          
      GO TO 61                                                                  
   43 RIC = 0.05291                                                             
      IF (D(IT+NSI).GE.TLM) GO TO 53                                            
      WTDC = 0.348*WTC                                                          
      GO TO 61                                                                  
   53 WTDC = 0.303*WTC                                                          
      GO TO 61                                                                  
   46 RIC = 0.0571                                                              
      IF (D(IT+NSI).GE.TLM) GO TO 54                                            
      WTDC = 0.359*WTC                                                          
      GO TO 61                                                                  
   54 WTDC = 0.3*WTC                                                            
   61 WTIC = 0.0                                                                
      DW=DIA*WTC                                                                
      GO TO (71,71,73,73,73,76), IVT                                            
   71 IF (DW.LE.17.5) GO TO 81                                                  
      CUC = 1.13*DW**1.11                                                       
      GO TO 91                                                                  
   81 CUC = 27.0                                                                
      GO TO 91                                                                  
   73 IF (DW.LE.0.1) GO TO 82                                                   
      CUC = 8.4*DW**0.192                                                       
      GO TO 91                                                                  
   82 CUC = 5.4                                                                 
      GO TO 91                                                                  
   76 IF (DW.LE.0.1) GO TO 83                                                   
      CUC = 3.7*DW**0.193                                                       
      GO TO 91                                                                  
   83 CUC = 2.37                                                                
      GO TO 91                                                                  
   12 WTC = 1.1                                                                 
      WTIC = 0.0                                                                
      CUC = 5.4                                                                 
      RIC = 0.00219                                                             
      WTF = D(ID(IRCD+5))                                                       
      IF (WTF.NE.0.0) WTC=WTC*WTF                                               
      IF (D(IT+NSI).GE.TLM) GO TO 94                                            
      WTDC = 0.348*WTC                                                          
      GO TO 91                                                                  
   94 WTDC = 0.303*WTC                                                          
   91 TEMP = D(IT+NSI)                                                          
      PRES = D(IP+NSI)                                                          
      IF (D(IT+NSI).LT.1460.0) TEMP = 1460.0                                    
      IF (D(IP+NSI).LT.364.7) PRES = 364.7                                      
      DRC = 1.0+((TEMP-1460.0)/450.0)**2+((PRES-364.7)/585.0)**2                
      DRF = D(ID(IRCD+8))                                                       
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      CUF = D(ID(IRCD+6))                                                       
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      RI = D(ID(IRCD+7))                                                        
      IF (RI.NE.0.0) RIC = RI                                                   
      CALL SSA                                                                  
      CALL SCO                                                                  
      CALL SCI(NL,NSI,NSO)                                                      
      CALL LINES(2)                                                             
      WRITE (OUT,1000) DIA,AKT,VEL                                              
 1000 FORMAT(1H0,5X,1HD,F9.2,3X,2HKT,F8.2,3X,1HV,F9.2)                          
      IF (IFT.EQ.2) WRITE (OUT,1001) AMN                                        
 1001 FORMAT(1H+,44X,1HM,F9.4)                                                  
      D(IGA+71) = DIA                                                           
      D(IGA+72) = VEL                                                           
      D(IGA+73) = AMN                                                           
      RETURN                                                                    
C     VLVSP                                                                     
      END                                                                       
*DECK,133105                                                                    
      SUBROUTINE VLVSZ                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
     *, (SCR(1),C(151))                                                         
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER CERR, OUT                                                         
      EQUIVALENCE (SCR(1),NL)                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/011/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 WTF                                                                       
C   6 CUF                                                                       
C   7 RI                                                                        
C   8 DRF                                                                       
C   9 DIA                                                                       
C  10 IVT                                                                       
      I = IACDB(10)                                                             
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      ID(I+3) = ISTAN(ICV(3))                                                   
      ID(I+4) = ISTAN(ICV(4))                                                   
      CALL FTL(NL,IFTA)                                                         
      ID(I+5) = IPARM(ICV(5))                                                   
      ID(I+6) = IPARM(ICV(6))                                                   
      ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      ID(I+10) = ICV(10)                                                        
      IF (ICV(10).GT.0 .AND. ICV(10).LE.6) GO TO 99                             
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,ICV(10)                                             
 1000 FORMAT(6H0ERROR,I6,5X,18HINVALID VALVE TYPE,I6)                           
   99 CONTINUE                                                                  
      RETURN                                                                    
C     VLVSZ                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES REFRIGERANT SATURATED VAPOR PRESSURE              
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
C$         GIVE T, GET PSAT                                                     
      FUNCTION VPS(L,T)                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFP,C(22)), (OUT,C(7))                                       
      INTEGER OUT                                                               
C$       SAME TYPE FIX AS WAS REQD IN VTS                                       
C$       WAS (CHANGE SIGN FOR B AND C COEFFICIENTS TO MATCH CONVENTION)         
      VPS = D(L+12)+D(L+13)/T+D(L+14)*ALOG10(T)+D(L+15)*T                       
      IF (D(L+17).NE.0.0) VPS = VPS                                             
     * +D(L+16)*(D(L+17)-T)/T*ALOG10(ABS(D(L+17)-T)) 
      VPS = 10.0**VPS                                                           
C$XX    ADD "COMMON","INTEGER OUT" AND "EQUIV.." ABOVE FOR WRITES BELOW         
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9099                                              
      WRITE (OUT,8990)                                                          
 8990 FORMAT(' FROM VPS... GIVE L,T   GET PSAT       ***************'           
     *      ,'***********************')                                         
      WRITE (OUT,9000) L     , T     ,D(L+12), D(L+13)                          
     *               ,D(L+14),D(L+15),D(L+16), D(L+17)                          
 9000 FORMAT(' L     =', I6,6X,  ' T     =', E12.5, ' D(L+12=', E12.5           
     *      ,' D(L+13=', E12.5,/,' D(L+14=', E12.5, ' D(L+15=', E12.5           
     *      ,' D(L+16=', E12.5,  ' D(L+17=', E12.5)                             
      WRITE (OUT,9010) VPS                                                      
 9010 FORMAT(' VPS   =', E12.5)                                                 
 9099 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      RETURN                                                                    
C     VPS                                                                       
      END                                                                       
C                                                                               
C*********************************************************************          
C                                                                               
C$        COMPUTES VISCOSITY OF SATURATED LIQUID, UNITS ARE LBM/FT-SEC.         
C                                                                               
C*********************************************************************          
C                                                                               
      FUNCTION VQUALH(CALLER, L,P,H)                                            
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7))                                                    
      INTEGER OUT                                                               
      DIMENSION IC(200)                                                         
      EQUIVALENCE (IC(1),C(1))                                                  
c@      REAL * 8 CALLER                                                           
      CHARACTER*8 CALLER                                                           
                                                                                
      COMMON/DC/DZ(2),D(128001)                                                 
                                                                                
      IF(P .GE. D(L+53)) GO TO 10                                                 
      T=VTS(L,P)                                                                
      V=VSV('VQUALH  ',L,P,T)                                                   
      HG=VH(L,P,T,V)                                                            
      HFG=VHFG(L,P,T,V)                                                         
      HL=HG-HFG                                                                 
      VQUALH=(H-HL)/HFG                                                         
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9098                                              
      WRITE(OUT,9000) CALLER                                                    
 9000 FORMAT(' VQUALH CALLED BY ', A8, 8X, '*************************')         
      WRITE (OUT,9010) L     , P     , H , D(L+53)                              
     *               , T     , V                                                
 9010 FORMAT(' L     =', I6,6X, ' P     =', E12.5, ' H     =', E12.5            
     *      ,' PCRIT =', E12.5,/,' TSTVTS=', E12.5, ' V VSV =', E12.5)          
      WRITE (OUT,9020) HG    , HFG   , HL    ,VQUALH                            
 9020 FORMAT(' HG VH =', E12.5,  ' HFGVHFG', E12.5, ' HL    =', E12.5           
     *      ,' VQUALH=', E12.5)                                                 
 9098 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      RETURN                                                                    
   10 VQUALH= 1.0                                                               
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF (IRITE .NE. 1) GO TO 9099                                              
      WRITE(OUT,9100) CALLER                                                    
 9100 FORMAT(' VQUALH CALLED BY ', A8, 8X, '**   P> PCRIT SO QUAL=1. ')         
 9099 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      RETURN                                                                    
C  VQUALH                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C$    FUNCTION VQUALS(L,P,S)                                                    
C$        COMPUTES QUALITY FOR LIQUID, 2-PHASE, GAS FOR GIVEN P AND S           
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VQUALS(L,P,S)                                                    
      COMMON/DC/DZ(2),D(128001)                                                 
      IF(P.GE.D(L+53)) GO TO 10                                                 
      T=VTS(L,P)                                                                
      V=VSV('VQUALS 1',L,P,T)                                                   
      SG=VS(L,T,V)                                                              
      IF(S.GE.SG) GO TO 10                                                      
      VQUALS=1.-(SG-S)*T/VHFG(L,P,T,V)                                          
      IF(VQUALS.LE.0) VQUALS=0.                                                 
      RETURN                                                                    
   10 VQUALS=1.0                                                                
      RETURN                                                                    
C  VQUALS                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES REFRIGERANT ENTROPY FOR SUPERHEATED VAPOR         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VS(L,T,V)                                                        
      COMMON /DC/ DZ(2),D(128001)                                               
      VMB = V-D(L+18)                                                           
      A = .185053*D(L+31)/D(L+2)*EXP(-D(L+31)*T/D(L+2))                         
      VS = D(L+32)*ALOG(T)-0.5*D(L+36)/T**2+(((.3333333*D(L+35)*T+0.5*          
     *D(L+34))*T+D(L+33))*T)                                                    
     *+.185053*(D(L+1)*ALOG(VMB)-((((0.25*D(L+29)/VMB+.3333333*D(L+26))/        
     *VMB+0.5*D(L+23))/VMB+D(L+20))/VMB))                                       
     *+A*((((0.25*D(L+30)/VMB+.3333333*D(L+27))/VMB+0.5*D(L+24))/VMB+           
     *D(L+21))/VMB)                                                             
     *+D(L+38)                                                                  
      RETURN                                                                    
C     VS                                                                        
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE                                                            
C$        CALCULATES REFRIGERANT SPEED OF SOUND IN A SUPERHEATED VAPOR          
C                                                                               
C                                                                               
C$         S-TYPE LINES REPAIRED                                                
C$     GAM BELOW ADDED,      OTHER MODS ALSO                                    
C**********************************************************************         
C                                                                               
      FUNCTION VSOS(L,P,T)                                                      
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (BIG,C(31)),(GAM,C(375))                                      
      COMMON /DC/ DZ(2),D(128001)                                               
      IF (T.GE.D(L+2)) GO TO 2                                                  
      PS = VPS(L,T)                                                             
      TS = VTS(L,P)                                                             
      IF (PS.LT.P) GO TO 1                                                      
    2 V = VSV('VSOS 5  ',L,P,T)                                                 
      IF (P.LT.100.) DP = 0.1                                                   
      IF (P.GE.100.) DP = 0.001*P                                               
      P0 = P-DP                                                                 
      IF (ABS(P-PS).LT.0.1) P0 = PS                                             
      T0 = T-0.5                                                                
      IF (ABS(T-TS).LT.0.5) T0 = TS                                             
      P1 = P+DP                                                                 
      T1 = T+0.5                                                                
      V0 = VSV('VSOS 4  ',L,P0,T0)                                              
C$         S-TYPE LINES REPAIRED                                                
      S0 = VS(L,T0,V0)                                                          
      V1 = VSV('VSOS 3  ',L,P0,T1)                                              
      S1 = VS(L,T1,V1)                                                          
      V2 = VSV('VSOS 2  ',L,P1,T0)                                              
      S2 = VS(L,T0,V2)                                                          
      V3 = VSV('VSOS 1  ',L,P1,T1)                                              
      S3 = VS(L,T1,V3)                                                          
      PVT = ((P1-P0)/(V2-V0)+(P1-P0)/(V3-V1))/2.                                
      SVT = ((S2-S0)/(V2-V0)+(S3-S1)/(V3-V1))/2.                                
      STP = ((S1-S0)/(T1-T0)+(S3-S2)/(T1-T0))/2.                                
      VTP = ((V1-V0)/(T1-T0)+(V3-V2)/(T1-T0))/2.                                
      STV = STP*778.0+(VTP**2.)*PVT*144.0                                       
      GAM = STP*778.0/STV                                                       
      VSOS = V*SQRT(32.174*(((SVT*778.0)**2.)/STV-PVT*144.0))                   
   99 CONTINUE                                                                  
      RETURN                                                                    
    1 VSOS = BIG                                                                
      GO TO 99                                                                  
C     VSOS                                                                      
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C$    FUNCTION VST(L,T)                                                         
C$        CALCULATES REFRIGERANT SURFACE TENSION IN LB/FT.                      
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VST(L,T)                                                         
      COMMON/DC/DZ(2),D(128001)                                                 
      IF(T.GE.D(L+2)) GO TO 99                                                  
      P=VPS(L,T)                                                                
      VST=4.5282E-12*(D(L+52)*(VDL(L,T)-1./VSV('CONDF 1 ',L,P,T)))**4           
      RETURN                                                                    
   99 VST=0.0                                                                   
      RETURN                                                                    
C  VST                                                                          
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE                                                            
C$        COMPUTES REFRIGERANT SPECIFIC VOLUME FOR SUPERHEATED VAPOR            
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VSV(CALLER,L,P,T)                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFP,C(22)), (OUT,C(7))                                       
c@      REAL * 8 CALLER                                                           
      CHARACTER*8 CALLER                                                           
      INTEGER OUT                                                               
      COMMON /DC/ DZ(2),D(128001)                                               
C$ THE 3 LINES BELOW WERE :       EX = EXP(-D(L+31)*T/D(L+2))                   
      XKTTC=-D(L+31)*T/D(L+2)                                                   
      EX = 0.0                                                                  
      IF(XKTTC.GT.-100.) EX=EXP(XKTTC)                                          
      CF1 = D(L+1)*T                                                            
      CF2 = D(L+19)+D(L+20)*T+D(L+21)*EX                                        
      CF3 = D(L+22)+D(L+23)*T+D(L+24)*EX                                        
      CF4 = D(L+25)+D(L+26)*T+D(L+27)*EX                                        
      CF5 = D(L+28)+D(L+29)*T+D(L+30)*EX                                        
C$WAS VMB = CF1/P                                                               
C$  MISC OTHER CHANGES TO VSV                                                   
      VMB = CF1/P-D(L+18)                                                       
      ITER = 1                                                                  
   10 ERR = (((((CF5/VMB+CF4)/VMB+CF3)/VMB+CF2)/VMB+CF1)/VMB)-P                 
      TEST = ABS(ERR)/P                                                         
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9099                                              
      WRITE (OUT,8980) CALLER                                                   
 8980 FORMAT(' VSV CALLED BY ', A8)                                             
      WRITE (OUT,8990) ITER                                                     
 8990 FORMAT(' FROM VSV      ITER  =', I4   , '      ***************'           
     *                               ,'*******************************')        
      WRITE (OUT,9000) L     , P     , T     , EX    , IFP                      
 9000 FORMAT(' L     =', I6,6X,  ' P     =', E12.5, ' T     =', E12.5           
     *      ,' EX    =', E12.5,/,' IFP   =', I6,6X)                             
      WRITE (OUT,9010) CF1   , CF2   , CF3   , CF4   , CF5                      
 9010 FORMAT(' CF1   =', E12.5,  ' CF2   =', E12.5, ' CF3   =', E12.5           
     *      ,' CF4   =', E12.5,/,' CF5   =', E12.5)                             
      WRITE (OUT,9020) ITER  , VMB   , ERR   , TEST  , WMB                      
 9020 FORMAT(' ITER  =', I6,6X,  ' VMB   =', E12.5, ' ERR   =', E12.5           
     *      ,' TEST  =', E12.5,/,' WMB   =', E12.5)                             
 9099 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF (TEST.LE.1.E-6) GO TO 2                                                
      IF(ITER.EQ.20) GO TO 3                                                    
      WMB = 1./VMB                                                              
      WMB = WMB+WMB*WMB*ERR/((((((-5.0*CF5/VMB-4.0*CF4)/                        
     *VMB-3.0*CF3)/VMB-2.0*CF2)/VMB-CF1)/VMB)/VMB)                              
      VMB = 1./WMB                                                              
      ITER = ITER+1                                                             
      GO TO 10                                                                  
    3 IF (TEST.LE.1.E-5) GO TO 2                                                
      GO TO 4                                                                   
    2 VSV = VMB+D(L+18)                                                         
      IF(VSV.LE.0.) GO TO 4                                                     
      RETURN                                                                    
    4 CALL LINES(1)                                                             
      WRITE(OUT,1010)                                                           
 1010 FORMAT(' NON-CONVERGENCE VSV--VSV RETURNED AS V-CRITICAL')                
      VSV = VMB+D(L+54)                                                         
      CALL LINES(2)                                                             
      WRITE (OUT,1020) P,VSV,T,D(L+54),ERR                                      
 1020 FORMAT(' P=',E12.4,'  VSV=',E12.4,'  T=',E12.4,                           
     *'   V-CRITICAL=',F10.4,'  P(TRIAL)-P=',E14.7)                             
      RETURN                                                                    
C     VSV          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC            
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE COMPUTES REFRIGERANT SUPERHEATED VAPOR                     
C                                                                               
C                                                                               
C$        THIS IS THE ORIGINAL VTAV1 FROM "H017"                                
C$        MODIFY WITH NEW VSV CALL TO PREVENT RUN-TIME 0C4 ERROR                
C**********************************************************************         
C                                                                               
      SUBROUTINE VTAV1(L,TX,VX,P,S)                                             
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (BIG,C(31)), (IFP,C(22)), (OUT,C(7))                          
      INTEGER OUT                                                               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION TXS(2), SXS(2)                                                  
      TX = VTS(L,P)                                                             
C$        THIS IS THE NEW VSV CALL, THREE PLACES IN VTAV1 AND VTAV2             
      VX = VSV('VTAV1 1 ',L,P,TX)                                               
      SX = VS(L,TX,VX)-S                                                        
      IF(SX.GT.0.0) GO TO 1                                                     
      TXS(1) = TX                                                               
      SXS(1) = SX                                                               
      TX = D(L+2)                                                               
      VX = VSV('VTAV1 2 ',L,P,TX)                                               
      SX = VS(L,TX,VX)-S                                                        
      TXS(2) = TX                                                               
      SXS(2) = SX                                                               
      ITER = 1                                                                  
   13 TX = TXS(1)-SXS(1)*(TXS(2)-TXS(1))/(SXS(2)-SXS(1))                        
      VX = VSV('VTAV1 3 ',L,P,TX)                                               
      SX = VS(L,TX,VX)-S                                                        
      TEST = ABS(SX)/S                                                          
      IF (TEST.LE.1.E-6) GO TO 99                                               
      IF(ITER.EQ.20) GO TO 3                                                    
      ITER = ITER+1                                                             
      IF (ABS(SXS(1)).GT.ABS(SXS(2))) GO TO 11                                  
      IF (TX.GT.TXS(1)) GO TO 12                                                
      TXS(2) = TXS(1)                                                           
      SXS(2) = SXS(1)                                                           
   12 TXS(1) = TX                                                               
      SXS(1) = SX                                                               
      GO TO 13                                                                  
   11 IF (TX.LT.TXS(2)) GO TO 14                                                
      TXS(1) = TXS(2)                                                           
      SXS(1) = SXS(2)                                                           
   14 TXS(2) = TX                                                               
      SXS(2) = SX                                                               
      GO TO 13                                                                  
    3 IF (TEST.LE.1.E-5) GO TO 99                                               
      IF (IFP.NE.1) GO TO 99                                                    
      CALL LINES(2)                                                             
      WRITE (OUT,1000)                                                          
 1000 FORMAT(1H0,5X,24H***NON-CONVERGENCE VTAV1)                                
   99 CONTINUE                                                                  
      RETURN                                                                    
    1 TX = -BIG                                                                 
      VX = -BIG                                                                 
      GO TO 99                                                                  
C     VTAV1                                                                     
      END                                                                       
C$        THIS IS THE ORIGINAL VTAV2 FROM "H017"                                
      SUBROUTINE VTAV2(L,TX,VX,P,H)                                             
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (BIG,C(31)), (IFP,C(22)), (OUT,C(7))                          
      INTEGER OUT                                                               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION TXS(2), HXS(2)                                                  
      TX = VTS(L,P)                                                             
C$        THIS IS THE NEW VSV CALL, THREE PLACES IN VTAV1 AND VTAV2             
      VX = VSV('VTAV2 1 ',L,P,TX)                                               
      HX = VH(L,P,TX,VX)-H                                                      
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9079                                              
      XXVH = VH(L,P,TX,VX)                                                      
      WRITE (OUT,8970)                                                          
 8970 FORMAT(' VTAV2 TOP WRITING  CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC')         
      WRITE (OUT,9070) VX    , H     , HX    , XXVH                             
 9070 FORMAT(' VX    =', E12.5,  ' H     =', E12.5, ' HX    =', E12.5           
     *      ,' XXVH  =', E12.5)                                                 
 9079 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF(HX.GT.0.0) GO TO 1                                                     
      TXS(1) = TX                                                               
      HXS(1) = HX                                                               
      TX = D(L+2)                                                               
      VX = VSV('VTAV2 2 ',L,P,TX)                                               
      HX = VH(L,P,TX,VX)-H                                                      
      TXS(2) = TX                                                               
      HXS(2) = HX                                                               
      ITER = 1                                                                  
   13 TX = TXS(1)-HXS(1)*(TXS(2)-TXS(1))/(HXS(2)-HXS(1))                        
      VX = VSV('VTAV2 3 ',L,P,TX)                                               
      HX = VH(L,P,TX,VX)-H                                                      
      TEST = ABS(HX)/H                                                          
      IF (TEST.LE.1.E-6) GO TO 99                                               
      IF(ITER.EQ.20) GO TO 3                                                    
      ITER = ITER+1                                                             
      IF (ABS(HXS(1)).GT.ABS(HXS(2))) GO TO 11                                  
      IF (TX.GT.TXS(1)) GO TO 12                                                
      TXS(2) = TXS(1)                                                           
      HXS(2) = HXS(1)                                                           
   12 TXS(1) = TX                                                               
      HXS(1) = HX                                                               
      GO TO 13                                                                  
   11 IF (TX.LT.TXS(2)) GO TO 14                                                
      TXS(1) = TXS(2)                                                           
      HXS(1) = HXS(2)                                                           
   14 TXS(2) = TX                                                               
      HXS(2) = HX                                                               
      GO TO 13                                                                  
    3 IF (TEST.LE.1.E-5) GO TO 99                                               
      IF (IFP.NE.1) GO TO 99                                                    
      CALL LINES(2)                                                             
      WRITE (OUT,1000)                                                          
 1000 FORMAT(1H0,5X,24H***NON-CONVERGENCE VTAV2)                                
   99 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9099                                              
      WRITE (OUT,8980)                                                          
 8980 FORMAT(' VTAV2 WRITING CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC')         
      WRITE (OUT,9000) L     , TX    , VX    , P                                
 9000 FORMAT(' L     =', I6,6X,  ' TX    =', E12.5, ' VX    =', E12.5           
     *      ,' P     =', E12.5)                                                 
      WRITE (OUT,9010) H     , TEST  , ITER                                     
 9010 FORMAT(' H     =', E12.5,  ' TEST  =', E12.5, ' ITER  =', I6,6X           
     *      ,'       =', E12.5)                                                 
 9099 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      RETURN                                                                    
    1 TX = -BIG                                                                 
      VX = -BIG                                                                 
      GO TO 99                                                                  
C     VTAV2                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C$    FUNCTION VTLIQ(L,P,H)     DID NOT EXIST IN ORIGINAL FORTRAN               
C$        CALCULATES VAPOR PRESSURE CHANGE AS A FUNCTION OF TEMPERATURE         
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VTLIQ(L,P,H)                                                     
      EQUIVALENCE (D(11),ITER),(D(12),HSAT),(D(13),T),(D(14),ERR)               
      COMMON/DC/DZ(2),D(128001)                                                 
      COMMON /CC/C(600)                                                         
      EQUIVALENCE (C(7),OUT)                                                    
      INTEGER OUT                                                               
      T=VTS(L,P)                                                                
      ITER=1                                                                    
   10 HSAT=VHLIQ(L,P,T)                                                         
      IF((H-HSAT).LT.0.05) GO TO 15                                             
      WRITE(OUT,12) H,HSAT,P                                                    
   12 FORMAT(' *+* WARNING FROM VTLIQ: CALLED WITH H= ',E14.7,                  
     *'WHICH IS GREATER THAN HSATLIQ ',E14.7,' AT P=',E14.7)                    
   15 ERR= (H-HSAT)/VCPF(L,P,T)                                                 
      IF(ABS(ERR).LE.1.E-2) GO TO 20                                            
      IF(ITER.EQ.3) GO TO 20                                                    
      T=T+ERR                                                                   
      ITER=ITER+1                                                               
      GO TO 10                                                                  
   20 VTLIQ=T                                                                   
      RETURN                                                                    
C  VTLIQ                                                                        
      END                                                                       
C$        COMPUTES REFRIGERANT SATURATED VAPOR TEMPERATURE                      
C$        MODIFIED WRITE AT END...      CHANGED SIGN ON L+13,L+14               
C$        GO 100% OF WAY WITH ERROR FOR NOW                                     
      FUNCTION VTS(L,P)                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IFP,C(22)), (OUT,C(7))                                       
      INTEGER OUT                                                               
      COMMON /DC/ DZ(2),D(128001)                                               
      IF (P .LE. D(L+53)) GO TO 222                                             
      VTS= D(L+2)                                                               
      WRITE (OUT,1010) P,VTS                                                    
 1010 FORMAT('    ***PRESSURE >PCRIT, SO TSAT =  TCRIT (FROM VTS):',/           
     * '  P(INPUT)=',E14.7,'  TCRIT=',E14.7)                                    
      GO TO 99                                                                  
  222 PL = ALOG10(P)                                                            
      EFT = 0.0                                                                 
      DEFT = 0.0                                                                
      VTS = D(L+2)                                                              
      ITER = 1                                                                  
   10 IF (D(L+17).NE.0.0) EFT =                                                 
     * D(L+16)*(D(L+17)-VTS)/VTS*ALOG10(ABS(D(L+17)-vts)) 
C$     CHANGED SIGNS FOR B AND C COEFF (L+13,L+14) TO MATCH CONVENTION          
C$SIGNS OF B AND C COEFF (L+13,L+14) SHOULD BE POSITIVE IN ALOG10(P) EQN        
C$SIGNS OF B COEFF ONLY (L+13) SHOULD BE NEGATIVE IN DP/DT EQN                  
      ERR = D(L+12)+D(L+13)/VTS+D(L+14)*ALOG10(VTS)+D(L+15)*VTS+EFT-PL          
      TEST = ABS(ERR) /PL                                                       
      IF(TEST.LE.1.E-8) GO TO 99                                                
      IF(ITER.EQ.40) GO TO 3                                                    
      IF (D(L+17).NE.0.0) DEFT =                                                
     *(D(L+16)/VTS+D(L+16)*D(L+17)*ALOG(ABS(D(L+17)-vts))/VTS**2)/2.302585           
C$     CHANGED SIGNS FOR B AND C COEFF (L+13,L+14) TO MATCH CONVENTION          
      VTS = VTS-ERR/(-D(L+13)/VTS**2+D(L+14)/(VTS*2.302585)+D(L+15)             
     *                -DEFT)                                                    
      ITER = ITER+1                                                             
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9099                                              
      WRITE (OUT,8990) ITER                                                     
 8990 FORMAT(' FROM VTS      ITER  =', I4   , '      ***************'           
     *                               ,'*******************************')        
      WRITE (OUT,9000) L     , P     , PL    , IFP                              
     *               , STUFF , EFT   , DR1   , DR                               
 9000 FORMAT(' L     =', I6,6X,  ' P     =', E12.5, ' PL    =', E12.5           
     *      ,' IFP   =', I6,6X,/,' STUFF =', E12.5, ' EFT   =', E12.5           
     *      ,' DR1   =', E12.5,  ' DR    =', E12.5)                             
      VTSINV=1./VTS                                                             
      WRITE (OUT,9010) STUFF2, ERR   , ERR2  , TEST                             
     *               , DEFT  , VTS   , VTSINV                                   
 9010 FORMAT(' STUFF2=', E12.5,  ' ERR   =', E12.5, ' ERR2  =', E12.5           
     *      ,' TEST  =', E12.5,/,' DEFT  =', E12.5, ' VTS   =', E12.5           
     *      ,' 1/VTS =', E12.5)                                                 
 9099 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      GO TO 10                                                                  
    3 IF(TEST .LE. 1.E-4) GO TO 99                                              
      ERRACT = P*(10.**ERR-1.)                                                  
      CALL LINES(2)                                                             
      WRITE (OUT,1000) P,VTS,ERRACT                                             
 1000 FORMAT(1H0,5X,23H***NON-CONVERGENCE VTS:,/,                               
     *'  P=',E14.7,'  TSAT COMPUTED=',E14.7,'  P(TRIAL)-P=',E14.7)              
   99 CONTINUE                                                                  
      RETURN                                                                    
C     VTS                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C$    FUNCTION VVISCF(L,P,T)                                                    
C$        COMPUTES VISCOSITY OF REFRIGERANT VAPOR,UNITS ARE LBM/FT-SEC.         
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VVISCF(L,P,T)                                                    
      COMMON/DC/DZ(2),D(128001)                                                 
      VVISCF=(EXP(D(L+41)+D(L+42)/T+D(L+43)/T/T))/3600.                         
      RETURN                                                                    
C  VVISCF                                                                       
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C$    FUNCTION VVISCV(L,P,T)                                                    
C$        COMPUTES REFRIGERANT HEAT OF VAPORIZATION                             
C$        MODIFIED TO COMPUTE HFG FOR TWO-PHASE COMPRESSOR OUTLET               
C                                                                               
C**********************************************************************         
C                                                                               
      FUNCTION VVISCV(L,P,T)                                                    
      COMMON/DC/DZ(2),D(128001)                                                 
      VVISCV=1.E-04*SQRT(T)*(D(L+46)+D(L+47)/T+D(L+48)/T/T)/3600.               
      RETURN                                                                    
C  VVISCV                                                                       
      END                                                                       

C*   NORTHROP GRUMMAN PROPRIETARY