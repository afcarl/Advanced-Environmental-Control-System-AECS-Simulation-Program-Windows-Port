C*   NORTHROP GRUMMAN PROPRIETARY
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C       A LINE (CIRCULAR OR NON-CIRCULAR FLOW PASSAGE)                          
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE LINEPP                                                         
                                                                                
      COMMON /TWMAT/ TW(9999)                                                   
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,   C(7)),                                                
     *            (CERR,  C(16)),                                               
     *            (PASS,  C(17)),                                               
     *            (IFP,   C(22)),                                               
     *            (IW,    C(27)),                                               
     *            (IP,    C(28)),                                               
     *            (IT,    C(29)),                                               
     *            (IH,    C(30)),                                               
     *            (IGA,   C(35)),                                               
     *            (ILN,   C(37)),                                               
     *            (ISN,   C(38)),                                               
     *            (IRCD,  C(45)),                                               
     *            (IFB,   C(55)),                                               
     *            (ICPP,  C(88)),                                               
     *            (IIOP,  C(90)),                                               
     *            (SCR(1),C(151))                                               
      DIMENSION SCR(30)                                                         
      INTEGER CERR, PASS, OUT                                                   
      REAL KFLU, KT, LHYDRO, LTHERM, NUTLMENT                                                             
      EQUIVALENCE  (SCR(1),NL),         !LEG NUMBER                             
     *             (SCR(2),NSI),        !INLET STATION NUMBER                   
     *             (SCR(3),NSO),        !OUTLET STATION NUMBER                  
     *             (SCR(4),A),                                                  
     *             (SCR(5),PD),                                                 
     *             (SCR(6),TS),                                                 
     *             (SCR(7),UA),                                                 
     *             (SCR(8),Q),                                                  
     *             (SCR(9),CP),                                                 
     *             (SCR(10),TA),                                                
     *             (SCR(11),TDAR),                                              
     *             (SCR(12),IOP),                                               
     *             (SCR(13),VEL),                                               
     *             (SCR(14),AM),                                                
     *             (SCR(15),NFT),                                               
     *             (SCR(16),HS),                                                
     *             (SCR(17),SHR),                                               
     *             (SCR(18),SHRN),                                              
     *             (SCR(19),IFPS),                                              
     *             (SCR(20),ITER),                                              
     *             (SCR(21),IDC),                                               
     *             (SCR(22),IDCN),                                              
     *             (SCR(23),IS),                                                
     *             (SCR(24),II),                                                
     *             (SCR(25),IFL),                                               
     *             (SCR(26),IJ),                                                
     *             (SCR(27),TAN),                                               
     *             (SCR(28),HOUT), (P0,C(64)), (T0,C(65))                                               
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
                                                                                
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 PRESS. DROP OPTION                                                        
C   6 EQUATION/TABLE OPTION                                                     
C   7 TABLE NO. - PRESS. DROP / FRICTION FACTOR                                 
C   8 CONSTANT                                                                  
C   9 KT TABLE / K                                                              
C  10 LENGTH                                                                    
C  11 DIAMETER                                                                  
C  12 CROSSECTIONAL AREA                                                        
C  13 UA TABLE NO.                                                              
C  14 TS TABLE NO.                                                              
C  15 B1                                                                        
C  16 Q TABLE NO.                                                               
C  17 B2                                                                        
C  18 H OUTSIDE DUCT                                                            
                                                                                
      IRCD = IRCDB(18)    ! NUMBER OF PARAMS ABOVE
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      IFL = ID(IFB+NL)                                                          
      D(IGA+41) = D(IW+NL)                                                      
      IF (D(IT+NSI).GT.0.)   ROOTT=SQRT(D(IT+NSI))                              
      IF (D(IT+NSI).LE.0.)   ROOTT=1.                                           
      D(IGA+35) = D(IW+NL)*ROOTT/D(IP+NSI)                                      
      IOP = ID(IRCD+6)                                                          
      IF (IOP.EQ.0) GO TO 302                                                   
                                                                                
C                                                                               
C$ ADD WARNINGS  (SKIP FOR TABLE DELTAP)                                        
C                                                                               
      IF ((D(ID(IRCD+10)) .LT. 1.E-20) .OR.                                     
     *    (D(ID(IRCD+10)) .GT. 1.E+10)) WRITE(OUT,298) ID(ISN+NSI)              
  298 FORMAT(' WARNING- LENGTH LESS THAN 1.E-20 OR GREATER THAN'                
     *      ,' 1.E10 IN LINEPP NSI=', I7 )                                     
                                                                                
      IF ((D(ID(IRCD+11)) .LT. 1.E-20) .OR.                                     
     *    (D(ID(IRCD+11)) .GT. 1.E+10)) WRITE(OUT,299) ID(ISN+NSI)              
  299 FORMAT(' WARNING- DIAMETER LESS THAN 1.E-20 OR GREATER THAN'              
     *      ,' 1.E10 IN LINEPP NSI=', I7 )                                     
                                                                                
      IF ((D(IW+NL      ) .LT. 1.E-20) .OR.                                     
     *    (D(IW+NL      ) .GT. 1.E+10)) WRITE(OUT,300) ID(ISN+NSI)              
  300 FORMAT(' WARNING- FLOW IS LESS THAN 1.E-20 OR GREATER THAN'               
     *      ,' 1.E10 IN LINEPP NSI=', I7 )                                     
                                                                                
      IF ((D(IP+NSI     ) .LT. 1.E-20) .OR.                                     
     *    (D(IP+NSI     ) .GT. 1.E+10)) WRITE(OUT,301) ID(ISN+NSI)              
  301 FORMAT(' WARNING- PRESSURE IS LESS THAN 1.E-20 OR GREATER THAN'           
     *      ,' 1.E10 IN LINEPP NSI=', I7 )                                     
                                                                                
  302 CONTINUE                                                                  
C		 !WROOTTHETA/DELTA ADDED FOR INLET RECOV 9/21/2000	SFW
	D(IGA+22)  = D(IW+NL)* ROOTT/SQRT(T0)/ (D(IP+NSI)/P0)
C$ ADD FOR UA VS FLOW AND TAVE LOOKUP                                           
      D(IGA+36) = D(IT+NSI)                                                     
      D(IGA+38) = D(ID(IRCD+18))                                                
      D(IH+NSO) = D(IH+NSI)                                                     
                                                                                
                                                                                
      IF (PASS.EQ.1) THEN                                                       
         SHR = 1.0                                                              
      ELSE                                                                      
         SHR = D(IRCD-9)                                                        
      ENDIF                                                                     
                                                                                
      A = D(ID(IRCD+12))                                                        
                                                                                
      IF(A.EQ.0.0)  A = 0.7854 * (D(ID(IRCD+11)))**2                            
                                                                                
      IF (ID(IRCD+13).NE.0 .AND. ID(IRCD+14).NE.0) GO TO 11                     
      IF (ID(IRCD+16).NE.0) GO TO 21                                            
      Q = 0.0                                                                   
      D(IT+NSO) = D(IT+NSI)                                                     
                                                                                
      TA = D(IT+NSI)                                                            
      ASSIGN 22 TO IS                                                           
      GO TO 31                                                                  
   11 CONTINUE
C	IF (D(ID(IRCD+15)) .EQ. 0.0)  D(ID(IRCD+15))=1.0    !WAS THIS NECESSARY? SFW 5/2001                      
      UA = TLUP(ID(IRCD+13))*D(ID(IRCD+15))   !UA TABLE * BFAC 
      TS = TLUP(ID(IRCD+14))                                                    
      D(IT+NSO) = D(IT+NSI)                                                     
                                                                                
      II = 0                                                                    
      ITER = 0                                                                  
      IFPS = IFP                                                                
      IFP = 0                                                                   
      IDCN = 0                                                                  
   13 IF (ID(IFL+1).EQ.1) GO TO 40                                              
   41 TAN = D(IT+NSI)                                                           
      DO 42 IJ=1,4                                                              
      TA = TAN                                                                  
      D(IT+NSO) = D(IT+NSI)+UA*(TS-D(IT+NSI))/(D(IW+NL)*SHP(NL,D(IP+NSI)        
     *,TA,D(IH+NSI))*SHR+0.5*UA)                                                
                                                                                
      TAN = 0.5*(D(IT+NSI)+D(IT+NSO))                                           
C$ ADD TAN FOR UA VS FLOW AND TAVE LOOKUP                                       
      D(IGA+36) = TAN                                                           
C$ was lt. 1.0                                                                                 
      IF (ABS(TAN-TA).LT. 0.1) GO TO 43                                          
   42 CONTINUE                                                                  
   43 TA = TAN                                                                  
      GO TO 46                                                                  
   40 IF (ID(ID(IFL+3)-1).EQ.12) GO TO 41                                       
      TAN = D(IT+NSI)                                                           
      DO 44 IJ=1,4                                                              
      TA = TAN                                                                  
      HOUT = HFT(NL,D(IP+NSI),D(IT+NSI))+UA*(TS-TA)/D(IW+NL)                    
      D(IT+NSO) = TFH(NL,D(IP+NSI),HOUT)                                        
                                                                                
      TAN = 0.5*(D(IT+NSI)+D(IT+NSO))                                           
      IF (ABS(TAN-TA).LT.0.1) GO TO 45                                          
   44 CONTINUE                                                                  
   45 TA = TAN                                                                  
   46 ASSIGN 12 TO IS                                                           
      GO TO 31                                                                  
   12 Q = UA*(TS-TA)                                                            
      CALL TDAHS(NL,NSI,NSO,TDAR)                                               
C                                                                               
C       ADD TDB1 CALLS FOR UADUCTS (12), WERE JUST IN QDUCTS(22)                
C$     MIMIC THE HXAPPH TDB1 CALLS                                              
      IF (D(IH+NSI).NE.0.0) CALL TDB1(NL,NSI,NSI,HSI)                           
      IF (D(IH+NSI).NE.0.0) CALL TDB1(NL,NSO,NSO,HSO)                           
      IF (D(IT+NSI).EQ.D(IT+NSO)) GO TO 280                                     
      DELTLABS = ABS(D(IT+NSO)-D(IT+NSI))                                       
                                                                                
      IF (DELTLABS .LT. 1.E-20)   WRITE(OUT,312) ID(ISN+NSI)                    
  312 FORMAT(' ABS(DELTATL)  IS LESS THAN 1.E-20 IN '                           
     *      ,' LINEPP NSI=', I7 )                                               
                                                                                
      IF (DELTLABS .LT. 1.E-20) GO TO 280                                       
      SHRN = (D(IT+NSI)-TDAR)/(D(IT+NSI)-D(IT+NSO))                             
      GO TO 281                                                                 
  280 SHRN = 1.                                                                 
  281 CONTINUE                                                                  
      IDC = IDCN                                                                
      IDCN = 1                                                                  
      IF (SHRN.LE.SHR) IDCN = -1                                                
      IF (ABS(SHRN-SHR).LE.1.E-4) GO TO 16                                      
      ITER = ITER+1                                                             
      IF (ITER.GT.20) GO TO 14                                                  
      IF (IDC.NE.IDCN) GO TO 15                                                 
      SHR = SHRN                                                                
      GO TO 13                                                                  
   15 SHR = 0.5*(SHR+SHRN)                                                      
      GO TO 13                                                                  
   14 IF (IFP.EQ.0) GO TO 16                                                    
      CALL LINES(2)                                                             
      WRITE (OUT,1013) ID(ILN+NL)                                               
 1013 FORMAT(1H0,5X,35H***NON-CONVERGENCE SHR - LINE - LEG,I6)                  
   16 IF (II.NE.0) GO TO 90                                                     
      IF (IFPS.EQ.0) GO TO 90                                                   
      II = II+1                                                                 
      IFP = IFPS                                                                
      GO TO 13                                                                  
   21 Q = TLUP(ID(IRCD+16))*D(ID(IRCD+17))                                      
      IF (ID(IFL+1).EQ.1) GO TO 47 ! LIQUID                                              
   48 TAN = D(IT+NSI)     !we have a gas                                                      
      DO 49 IJ=1,4                                                              
      TA = TAN                                                                  
      SHPPP =  SHP(NL,D(IP+NSI),TA,D(IH+NSI))                                   
      IF ((SHPPP    .LT. 1.E-20) .OR.                                           
     *    (SHPPP    .GT. 1.E+10)) WRITE(OUT,303) ID(ISN+NSI)                    
  303 FORMAT(' SHP      IS LESS THAN 1.E-20 OR GREATER THAN 1.E+10 IN '         
     *      ,' LINEPP NSI=', I7 )                                               
      D(IT+NSO) = D(IT+NSI)+Q/(D(IW+NL)*SHP(NL,D(IP+NSI),TA,D(IH+NSI)))         
      TAN = 0.5*(D(IT+NSI)+D(IT+NSO))                                           
      IF (ABS(TAN-TA).LT.0.1) GO TO 50                                          
   49 CONTINUE                                                                  
   50 TA = TAN                                                                  
      GO TO 51                                                                  
   47 IF (ID(ID(IFL+3)-1).EQ.12) GO TO 48                                       
      HOUT = HFT(NL,D(IP+NSI),D(IT+NSI))+Q/D(IW+NL)   !we have a liquid                               
      D(IT+NSO) = TFH(NL,D(IP+NSI),HOUT)
      IF(ABS(D(IT+NSO)-D(IT+NSI)).GT. 5.) GO TO 445                                        
      D(IT+NSO)=D(IT+NSI)+Q/(D(IW+NL)*SHP(NL,D(IP+NSI),D(IT+NSI),
     *   D(IH+NSI)))                                  !USE CP,NOT ENTHALPY FOR SMALL LIQUID DELTAT'S   	
  445 TA = 0.5*(D(IT+NSI)+D(IT+NSO))                                            
   51 ASSIGN 22 TO IS                                                           
      GO TO 31  !20 lines down 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
   22 TDAR = D(IT+NSO)                                                          
C$     MIMIC THE HXAPPH TDB1 CALLS                                              
C$WAS IF (D(IH+NSI).NE.0.0) CALL TDB(NL,NSI,NSO,HS)                             
      IF (D(IH+NSI).NE.0.0) CALL TDB1(NL,NSI,NSI,HSI)                           
      IF (D(IH+NSI).NE.0.0) CALL TDB1(NL,NSO,NSO,HSO)                           
C     WRITE (OUT,9110) HS, HSI, HSO                                             
C9110 FORMAT(' HS    =',F10.5,  '  HSI   =',F10.5,  '  HSO   =',F10.5)          
      IF (D(IT+NSI).EQ.D(IT+NSO)) GO TO 23                                      
      DELTLABS = ABS(D(IT+NSO)-D(IT+NSI))                                       
      IF (DELTLABS .LT. 1.E-20)  WRITE(OUT,3022) ID(ISN+NSI)                    
 3022 FORMAT(' ABS(DELTATL)  PART 2 IS LESS THAN 1.E-20 IN '                    
     *      ,' LINEPP NSI=', I7 )                                               
      IF (DELTLABS .LT. 1.E-20)  GO TO 23                                       
      SHR = (D(IT+NSI)-TDAR)/(D(IT+NSI)-D(IT+NSO))                              
      GO TO 90                                                                  
   23 SHR = 1.0                                                                 
      GO TO 90                                                                  
CCCCCC     IOP=1=PD EQN      IOP=2=KW**2   CCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
   31 IF (IOP.EQ.1) GO TO 3                                                     
      IF (IOP.EQ.2) GO TO 4                                                     
      GO TO 1                                                                   
    3 VISCCHK= VIS(NL,D(IP+NSI),TA)                                             
      IF ((VISCCHK  .LT. 1.E-20) .OR.                                           
     *    (VISCCHK  .GT. 1.E+10)) WRITE(OUT,304) ID(ISN+NSI)                    
  304 FORMAT(' VISC     IS LESS THAN 1.E-20 OR GREATER THAN 1.E+10 IN '         
     *      ,' LINEPP NSI=', I7 )                                               
      IF ((A        .LT. 1.E-20) .OR.                                           
     *    (A        .GT. 1.E+10)) WRITE(OUT,305) ID(ISN+NSI)                    
  305 FORMAT(' AREA     IS LESS THAN 1.E-20 OR GREATER THAN 1.E+10 IN '         
     *      ,' LINEPP NSI=', I7 )                                               
      D(IGA+21) = D(IW+NL)*D(ID(IRCD+11)) / (5.0*VIS(NL,D(IP+NSI),TA)*A)        
                                                                                
      ONEF=TLUP(ID(IRCD+7))                                                     
      KT    = TLUP( ID(IRCD+9))                                                 
      PD = 6.216E-4 * (4.0 * ONEF * D(ID(IRCD+10))/                             
     * D(ID(IRCD+11)) + D(ID(IRCD+8)) * KT)  * (D(IW+NL))                       
     ***2/(DEN(NL,D(IP+NSI),TA)*A**2)                                           
      GO TO 2                                                                   
C$WAS "IRDC+7" BELOW... OOPS                                                    
    4 PD = D(ID(IRCD+7))*D(IW+NL)**2                                            
      GO TO 5                                                                   
    1 PD = TLUP(ID(IRCD+7))                                                     
C$ ADD -2 OPTION TO LOOKUP DELTAP/P VS DOWNSTREAM WCORRECTED (-1 USES UPSTREAM)
      IF (ID(IRCD+5).EQ.-1) THEN
		PD = PD*D(IP+NSI)
		GO TO 5                                    
      ENDIF
	IF (ID(IRCD+5).LE.-2) THEN  !ITERATE TO GET THE RIGHT POUT
		PD = PD*D(IP+NSI)
		D(IP+NSO) = D(IP+NSI) - PD 
		D(IGA+22)  = D(IW+NL)* ROOTT/SQRT(T0)/ (D(IP+NSO)/P0)
		PD = TLUP(ID(IRCD+7))
		PD = PD*D(IP+NSI)
		D(IP+NSO) = D(IP+NSI) - PD 
		D(IGA+22)  = D(IW+NL)* ROOTT/SQRT(T0)/ (D(IP+NSO)/P0)
		PD = TLUP(ID(IRCD+7))
		PD = PD*D(IP+NSI)
		D(IP+NSO) = D(IP+NSI) - PD 
		D(IGA+22)  = D(IW+NL)* ROOTT/SQRT(T0)/ (D(IP+NSO)/P0)
		PD = TLUP(ID(IRCD+7))
		PD = PD*D(IP+NSI)
		D(IP+NSO) = D(IP+NSI) - PD 
		D(IGA+22)  = D(IW+NL)* ROOTT/SQRT(T0)/ (D(IP+NSO)/P0)
		PD = TLUP(ID(IRCD+7))
		PD = PD*D(IP+NSI)
		D(IP+NSO) = D(IP+NSI) - PD 
		D(IGA+22)  = D(IW+NL)* ROOTT/SQRT(T0)/ (D(IP+NSO)/P0)
		PD = TLUP(ID(IRCD+7))
		PD = PD*D(IP+NSI)
		D(IP+NSO) = D(IP+NSI) - PD 
		D(IGA+22)  = D(IW+NL)* ROOTT/SQRT(T0)/ (D(IP+NSO)/P0)
		PD = TLUP(ID(IRCD+7))
		PD = PD*D(IP+NSI)
	ENDIF
    5 SIGGG=SIG(NL,D(IP+NSI),TA)                                                
      IF ((SIGGG    .LT. 1.E-20) .OR.                                           
     *    (SIGGG    .GT. 1.E+10)) WRITE(OUT,306) ID(ISN+NSI)                    
  306 FORMAT(' SIGMA    IS LESS THAN 1.E-20 OR GREATER THAN 1.E+10 IN '         
     *      ,' LINEPP NSI=', I7 )                                               
      IF (ID(IRCD+5).GT.0) PD = PD/SIG(NL,D(IP+NSI),TA)                         
C   5 IF (ID(IRCD+5).GT.0) PD = PD/SIG(NL,D(IP+NSI),TA)                         
    2 D(IP+NSO) = D(IP+NSI) - PD                                                
                                                                                
      GO TO IS, (12,22)                                                         
C                                                                               
C THIS IS WHERE THE VALUES ARE BEING OVERWRITTEN, WHY????????                   
C                                                                               
   90 CONTINUE                                                                  
      IF (IIOP.EQ.0) D(IRCD-9) = SHR                                            
      D(IRCD-6) = Q                                                             
                                                                                
      IF (IFP.NE.1 .OR. ICPP.NE.0) RETURN                                       
C*****************************************************************************************
C	COPIED ALL THIS STUFF UP HERE TO GET RID OF TEMPS BEING CHANGED AFTER [EV CALLS?]
C		NOT ALL OF IT IS NECESSARY HERE, CLEAN UP LATER SFW 6/22/01
C$ ADD REFLU,HCONV WRITES                                                       
      BFAC  = D(ID(IRCD+15))                                                    
      HO    = D(IGA+38)                                                         
      PI    = 3.14159                                                           
      FLOW  = D(IW+NL)                                                          
      XLINCH= D(ID(IRCD+10))                                                    
      DIAMIN= D(ID(IRCD+11))                                                    
      DIAMFT= DIAMIN/12.                                                        
      VISC  = VIS(NL,D(IP+NSI),TA)                                              
      REFLU = FLOW*D(ID(IRCD+11)) / (5.0*VISC*A)                                
      CP    = SHP(NL,D(IP+NSI),TA,D(IH+NSI))                                    
      KFLU  = COND(NL,D(IP+NSI),TA)                                             
      PRNDTL= 3600.*VISC*CP/KFLU                                                
C  PUT LAM XNUAV BETWEEN CONST TEMP(4.3?) AND CONST QDOT(3.66) FIGURES          
      XNUAV = 4.00                                                              
      IF (REFLU .GT. 2000.) XNUAV= .023*REFLU**.8*PRNDTL**.4                    
C          HCNV=FTU/FT**2-HR-DEGF                                               
      HCNV=KFLU/DIAMFT*XNUAV                                                    
C          IF DH=4A/WP... WP=4A/DH                                              
      IF (A.EQ.0) A=PI*DIAMIN**2/4.                                             
      WETPRM= 4*A/DIAMIN                                                        
      AWALHT = WETPRM*XLINCH                                                    
      IF(BFAC .GT. 1.E-3) AWALHT = WETPRM*BFAC                                  
C          Q=BTU/MIN                                                            
      DT2WAL= Q/(HCNV/60.*AWALHT/144.)                                          
      IF ((DT2WAL.LT.0.) .AND. (DT2WAL.LT.(TS-TA))                              
     *      .AND. (TS .GT. 0.1)) DT2WAL=TS-TA                                   
      IF ((DT2WAL.GT.0.) .AND. (DT2WAL.GT.(TS-TA))                              
     *      .AND. (TS .GT. 0.1)) DT2WAL=TS-TA                                   
      IF (DT2WAL.EQ.0.) GO TO 1233                                              
      TW(ID(ISN+NSI)) = D(IT+NSI)+DT2WAL                                        
      TW(ID(ISN+NSO)) = D(IT+NSO)+DT2WAL                                        
 1233 DELTL = D(IT+NSO)-D(IT+NSI)                                               
      DELTAP= D(IP+NSI)-D(IP+NSO)                                               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                              
      CFAC= D(ID(IRCD+8))                                                       
      PAV= (D(IP+NSI)+D(IP+NSO))/2.                                             
      DENSAV= DEN(NL,PAV,TA)                                                    
      DENSEX= DEN(NL,D(IP+NSO),D(IT+NSO))                                       
      VEL = 2.4*FLOW/(DENSAV*A)                                                 
      VELEX = 2.4*FLOW/(DENSEX*A)                                               
      D(IRCD-7) = VEL                                                           
      CFM = VEL*A/2.4                                                           
      GPM = CFM*7.4805                                                          
      QPSI= 1.078E-04 * DENSAV * VEL**2                                         
      NFT = ID(IFB+NL)                                                          
C*****************************************************************************************
      IF (NFT.NE.1) GO TO 443
	HPLOST= .0043636 * CFM * (D(IP+NSI)-D(IP+NSO))              
C		HEAT OF PRESSURE LOSS FOR LIQUIDS
C PREG CAN DO LIQUID DELTAP WITHOUT TEMP RISE (AS IN HYDRAULIC MOTOR OR EFFICIENT ACTUATOR)
C     D(IT+NSO)=D(IT+NSO)+((D(IP+NSI)-D(IP+NSO))*144./(CP*778.*DENSAV))   !efficiency=0 FOR A LINE
      QDELTAP=HPLOST*42.4     !HEAT OF PRESSURE LOSS FOR LIQUIDS
      D(IT+NSO)=D(IT+NSO)+ (QDELTAP/(CP*FLOW))          
C     D(IT+NSO)= D(IT+NSO)+(HPLOST*42.4/(CP*D(IW+NL)))
C     D(IT+NSO)=D(IT+NSO)+((D(IP+NSI)-D(IP+NSO))*144./(CP*778.*DENSAV))              
      CALL PIOP(1,NL,NSI,NSO)    !MOVED FROM ABOVE TO BE AFTER TEMP RISE DUE TO DELTAP CORRECTION
  443 CONTINUE 
C*****************************************************************************************
      CALL PIOP(1,NL,NSI,NSO)      !MOVE BELOW TO AFTER TEMP RISE DUE TO DELTAP CORRECTION  
C$    IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HS) CALL HSOP(HS)                 
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSI).GT.HSI) CALL HSOP(HSI)               
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HSO) CALL HSOP(HSO)               
C$CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                              
                                                                                
      IF (IOP.EQ.0 .OR. A.EQ.0.0) RETURN                                        
C$ ADD REFLU,HCONV WRITES                                                       
      BFAC  = D(ID(IRCD+15))                                                    
      HO    = D(IGA+38)                                                         
      PI    = 3.14159                                                           
      FLOW  = D(IW+NL)                                                          
      XLINCH= D(ID(IRCD+10))                                                    
      DIAMIN= D(ID(IRCD+11))                                                    
      DIAMFT= DIAMIN/12.                                                        
      VISC  = VIS(NL,D(IP+NSI),TA)                                              
      REFLU = FLOW*D(ID(IRCD+11)) / (5.0*VISC*A)                                
      CP    = SHP(NL,D(IP+NSI),TA,D(IH+NSI))                                    
      KFLU  = COND(NL,D(IP+NSI),TA)                                             
      PRNDTL= 3600.*VISC*CP/KFLU                                                
C  PUT LAM XNUAV BETWEEN CONST TEMP(4.3?) AND CONST QDOT(3.66) FIGURES          
      XNUAV = 4.00                                                              
      IF (REFLU .GT. 2000.) XNUAV= .023*REFLU**.8*PRNDTL**.4                    
C          HCNV=FTU/FT**2-HR-DEGF                                               
      HCNV=KFLU/DIAMFT*XNUAV                                                    
C          IF DH=4A/WP... WP=4A/DH                                              
      IF (A.EQ.0) A=PI*DIAMIN**2/4.                                             
      WETPRM= 4*A/DIAMIN                                                        
      AWALHT = WETPRM*XLINCH                                                    
      IF(BFAC .GT. 1.E-3) AWALHT = WETPRM*BFAC                                  
C          Q=BTU/MIN                                                            
      DT2WAL= Q/(HCNV/60.*AWALHT/144.)                                          
      IF ((DT2WAL.LT.0.) .AND. (DT2WAL.LT.(TS-TA))                              
     *      .AND. (TS .GT. 0.1)) DT2WAL=TS-TA                                   
      IF ((DT2WAL.GT.0.) .AND. (DT2WAL.GT.(TS-TA))                              
     *      .AND. (TS .GT. 0.1)) DT2WAL=TS-TA                                   
      IF (DT2WAL.EQ.0.) GO TO 1234                                              
      TW(ID(ISN+NSI)) = D(IT+NSI)+DT2WAL                                        
      TW(ID(ISN+NSO)) = D(IT+NSO)+DT2WAL                                        
 1234 DELTL = D(IT+NSO)-D(IT+NSI)                                               
      DELTAP= D(IP+NSI)-D(IP+NSO)                                               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                              
      CFAC= D(ID(IRCD+8))                                                       
      PAV= (D(IP+NSI)+D(IP+NSO))/2.                                             
      DENSAV= DEN(NL,PAV,TA)                                                    
      DENSEX= DEN(NL,D(IP+NSO),D(IT+NSO))                                       
      VEL = 2.4*FLOW/(DENSAV*A)                                                 
      VELEX = 2.4*FLOW/(DENSEX*A)                                               
      D(IRCD-7) = VEL                                                           
      CFM = VEL*A/2.4                                                           
      GPM = CFM*7.4805                                                          
      QPSI= 1.078E-04 * DENSAV * VEL**2                                         
      NFT = ID(IFB+NL)                                                          
      NFT = ID(NFT+1)    ! FLUID TYPE=1 IS LIQUID  
C*****************************************************************************************
C      IF (NFT.NE.1) GO TO 443
C	HPLOST= .0043636 * CFM * (D(IP+NSI)-D(IP+NSO))              
CC		HEAT OF PRESSURE LOSS FOR LIQUIDS
CC PREG CAN DO LIQUID DELTAP WITHOUT TEMP RISE (AS IN HYDRAULIC MOTOR OR EFFICIENT ACTUATOR)
CC     D(IT+NSO)=D(IT+NSO)+((D(IP+NSI)-D(IP+NSO))*144./(CP*778.*DENSAV))   !efficiency=0 FOR A LINE
C      QDELTAP=HPLOST*42.4     !HEAT OF PRESSURE LOSS FOR LIQUIDS
C      D(IT+NSO)=D(IT+NSO)+ (QDELTAP/(CP*FLOW))          
CC     D(IT+NSO)= D(IT+NSO)+(HPLOST*42.4/(CP*D(IW+NL)))
CC     D(IT+NSO)=D(IT+NSO)+((D(IP+NSI)-D(IP+NSO))*144./(CP*778.*DENSAV))              
C      CALL PIOP(1,NL,NSI,NSO)    !MOVED FROM ABOVE TO BE AFTER TEMP RISE DUE TO DELTAP CORRECTION
C  443 CONTINUE 
C*****************************************************************************************
      IF (NFT.NE.2) GO TO 444                                                   
      AM = VEL/SOS(NL,PAV,TA)                                                   
      AMEX= VELEX/SOS(NL,D(IP+NSO),D(IT+NSO))                                   
      D(IRCD-8) = AM                                                            
C     HPLOST IS ISENTROPIC HP REQD TO COMPRESS THE FLUID BACK UP TO PIN         
      GAMMA = GAM(NL,D(IP+NSI),TA)                                              
      IF ((D(IP+NSI) .LT. 0.) .OR. (D(IP+NSO) .LT. 0.)) GO TO 444               
      HPLOST= D(IT+NSO)*((D(IP+NSI)/D(IP+NSO))**((GAMMA-1.)/GAMMA)-1.)          
     * *CP*FLOW/42.42                                                           
  444 CONTINUE
C$* LAMINAR ENTRY LENGTH EQNS FROM WHITE "HEAT AND MASS TRANSFER" P312-313                                                              
      LHYDRO  = 0.05*REFLU*DIAMIN
      LTHERM  = LHYDRO*PRNDTL
C$*      IF ((LHYDRO .LT. 0.1*XLINCH) .AND. (LTHERM .LT. 0.1*XLINCH)) GO TO 405
      ZETA    = (XLINCH/DIAMIN)/REFLU
      CFAPPRE = 3.44/ZETA**.5 + (.31/ZETA + 16 - 3.44/ZETA**.5)/
     *          (1 + .0021/ZETA**2)
      CFAPP   = CFAPPRE/REFLU
      DPLMENT = 4. * CFAPP * XLINCH/DIAMIN * QPSI
      GRAETZ  = ZETA/PRNDTL
      NUTLMENT= 3.66 + (0.0688/GRAETZ)/(1.+.04/GRAETZ**.6667)
      DT2WALENT= DT2WAL*XNUAV/NUTLMENT 
      NUTLMENTfirstqtr= 3.66 + (0.0688/(GRAETZ/4.))/
     *    	(1.+.04/(GRAETZ/4.)**.6667)
      DT2WALENTfirstqtr= DT2WAL*XNUAV/NUTLMENTfirstqtr 
      DT2WALENTLAST3QTRS=(4*DT2WALENT-DT2WALENTfirstqtr)/3. 
C ORIG
      CALL LINES(2)
      QSEC=Q/60.                                                             
      IF ((Q.LT.-9999.).OR.(Q.GT.99999))THEN 
          WRITE(OUT,1002) Q,QDELTAP,SHR,QSEC 
      ELSE
          WRITE(OUT,1001) Q,QDELTAP,SHR
      ENDIF
 1001 FORMAT(1H0,5X,6HQTWALL,F8.2,  3X,7HQDELTAP,F8.2, 3X,3HSHR,F7.4)                                  
 1002 FORMAT(1H0,5X,6HQTWALL,F8.0,  3X,7HQDELTAP,F8.2, 3X,3HSHR,F7.4
     * , 3X,10HQ(BTU/SEC),F9.0)                                  
C 
	WRITE (OUT,9000)                                                          
     *   DIAMIN,  REFLU ,  FLOW  ,  CP    ,  UA    ,  HCNV  ,  VEL              
     * , A     ,  AWALHT,  CFM   ,  KFLU  ,  TS    ,  XNUAV ,  AM               
     * , CFAC  ,  WETPRM,  GPM   ,  VISC  ,  BFAC  ,  DT2WAL,  AMEX             
     * , KT    ,  ONEF  ,  DENSAV,  GAMMA ,  HO    ,  HSO                       
     * , XLINCH,  QPSI  ,  HPLOST,  PRNDTL,  DELTL ,  DELTAP                    
      IF (REFLU .LT. 2000.) WRITE (OUT,9010)                                                          
     *   ZETA  ,  CFAPPRE, GRAETZ                    
     * , DT2WALENTfirstqtr, DT2WALENTLAST3QTRS 
     * , LHYDRO,  LTHERM,  CFAPP ,  DPLMENT, NUTLMENT,DT2WALENT                 
 9000 FORMAT(                                                                   
     *   ' DIAM  =',F10.4  , ' RE    =',F10.1  , ' FLOW  =',F10.4  ,            
     *   ' CP    =',F10.4  , ' UATBL =',F10.5  , ' HCNV  =',F10.3  ,            
     *   ' V-FT/S=',F10.3,/, ' A-SQIN=',F10.4  , ' AWALHT=',F10.2  ,            
     *   ' CFM   =',F10.3  , ' KFLU  =',F10.5  , ' TSINK =',F10.2  ,            
     *   ' XNUAV =',F10.3  , ' MACHAV=',F10.5,/, ' CFACTR=',F10.3  ,            
     *   ' WETPRM=',F10.3  , ' GPM   =',F10.3  , ' VISC  =',F10.8  ,            
     *   ' BFAC  =',F10.3  , ' DT2WAL=',F10.4  , ' MACHEX=',F10.5,/,            
     *   ' KT    =',F10.4  , ' ONEF  =',F10.5  , ' DENSAV=',F10.5  ,            
     *   ' GAMMA =',F10.4  , ' HO    =',F10.3  , ' HSATO =',F10.5,/,            
     *   ' L-INCH=',F10.3  , ' QPSIAV=',F10.4  , ' HPLOST=',F10.4  ,            
     *   ' PRNDTL=',F10.3  , ' DELTL =',F10.4  , ' DELTAP=',F10.5)
 9010 FORMAT(' THESE LAMINAR ENTRY LENGTH CALCS SUPERSEDE VALUES ' , 
     *    'ABOVE:' ,                                                                   
     *  /,' ZETA  =',F10.5  , ' CFAPPRE',F10.2  , ' GRAETZ=',F10.5  ,            
     *    ' DT2WALENT 1ST QTR OF L=',F10.5  , 
     *    ' DT2WALENT LAST 3/4''S OF L=',F10.5  , 
     *  /,' LHYDRO=',F10.3  , ' LTHERM=',F10.3  , ' CFAPP =',F10.5  ,            
     *    ' DPLMENT',F10.3  , ' NUTLMENT',F10.3 , ' DT2WALENT',F10.4 )             
      WRITE (OUT,9099)                                                          
 9099 FORMAT(' ')                                                               
                                                                                
      RETURN                                                                    
      END                                                                       
C*   NORTHROP GRUMMAN PROPRIETARY