C*   NORTHROP GRUMMAN PROPRIETARY
C
C**********************************************************************
C
C  THIS SUBUROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR
C      AN AUXILIARY POWER UNIT (APU)
C
C**********************************************************************
C
      SUBROUTINE APUPP

      COMMON /CC/ C(600)
      EQUIVALENCE (OUT,   C(7)),
     *            (IFP,   C(22)),
     *            (IW,    C(27)),
     *            (IP,    C(28)),
     *            (IT,    C(29)),
     *            (IH,    C(30)),
     *            (IGA,   C(35)),
     *            (IRCD,  C(45)),
     *            (ICPP,  C(88)),
     *            (SCR(1),C(151))
      DIMENSION SCR(30)
      INTEGER OUT
      EQUIVALENCE  (SCR(1),NL),
     *             (SCR(2),NSI),
     *             (SCR(3),NSO),
     *             (SCR(4),PR),
     *             (SCR(5),EFF),
     *             (SCR(6),NST),
     *             (SCR(7),HP),
     *             (SCR(8),GAMMA),
     *             (SCR(9),HS)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))

C   1 COMP CODE
C   2 LEG NO
C   3 INLET STATION NO
C   4 OUTLET STATION NO
C   5 SHAFT NO
C   6 PRESS RATIO
C   7 POLYTROPIC EFF
C   8 POWER

      IRCD = IRCDB(8)
      NL = ID(IRCD+2)
      IF (NL.EQ.0) GO TO 1
      NSI = ID(IRCD+3)
      NSO = ID(IRCD+4)
      PR  = D(ID(IRCD+6))
      D(IRCD-6) = PR
      EFF = D(ID(IRCD+7))
      D(IRCD-7) = EFF
      GAMMA = GAM(NL,D(IP+NSI),D(IT+NSI))
      D(IT+NSO) = D(IT+NSI) * PR ** ((GAMMA-1.0)/(GAMMA*EFF))
      D(IP+NSO) = D(IP+NSI) * PR
      D(IH+NSO) = D(IH+NSI)
      IF (D(IH+NSI).NE.0.0) CALL TDB(NL,NSI,NSO,HS)
    1 NST = ID(IRCD+5)
      IF (NST.EQ.0) GO TO 98
      HP = D(ID(IRCD+8))
      IF (IFP.EQ.1) GO TO 98
      D(IGA+NST+90) = D(IGA+NST+90)+HP
   98 IF (IFP.NE.1.OR.ICPP.NE.0) GO TO 99
      CALL PIOP(0,NL,NSI,NSI)
      IF(NL.EQ.0)  GO TO 2
      CALL PIOP(2,NL,NSI,NSO)
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HS) CALL HSOP(HS)
      CALL LINES(2)
      WRITE (OUT,1000) PR,EFF
 1000 FORMAT (1H0,5X,2HPR,F8.4,3X,3HEFF,F7.4     )
    2 IF(NST.EQ.0) GO TO 99
      CALL LINES(2)
      WRITE (OUT,1001) NST,HP
 1001 FORMAT(1H0,5X,6HSHAFT ,I4,3X,2HHP,F8.2)
   99 CONTINUE
      RETURN
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE
C      PERFORMANCE OF AN APU
C
C**********************************************************************
C
      SUBROUTINE APUPZ

      COMMON /CC/ C(600)
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))
      DIMENSION ICV(18), SCR(30)
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))
      DATA IFTA/010/

C   1 COMP CODE
C   2 LEG NO
C   3 INLET STATION NO
C   4 OUTLET STATION NO
C   5 SHAFT NO
C   6 PRESS RATIO
C   7 POLYTROPIC EFF
C   8 POWER

      I = IACDB(8)
      ID(I+1) = ICV(1)
      IF (ICV(2).EQ.0) GO TO 1
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
      ID(I+6) = IPARM(ICV(6))
      ID(I+7) = IPARM(ICV(7))
    1 IF (ICV(5).EQ.0) GO TO 2
      ID(I+5) = ICV(5)
      CALL SRT(ICV(5))
      ID(I+8) = IPARM(ICV(8))
    2 CONTINUE
      RETURN
C     APUPZ
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR
C       THE APU
C
C**********************************************************************
C
      SUBROUTINE APUSP

      COMMON /CC/ C(600)
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151)), (IW,C(27))
     *, (WTC,C(102)), (CUC,C(104)), (RIC,C(106)), (DRC,C(108))
     *, (WTIC,C(109)), (WTDC,C(111)), (FC,C(121)), (IGA,C(35))
     *, (CERR,C(16)), (IP,C(28)), (IT,C(29)), (IH,C(30))
      DIMENSION SCR(30)
      INTEGER OUT,CERR
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO), (SCR(4),NST)
     *, (SCR(5),WTF), (SCR(6),CUF), (SCR(7),RI), (SCR(8),DRF)
     *, (SCR(9),EHP), (SCR(10),PR), (SCR(11),SHPP), (SCR(12),EHPT)
     *, (SCR(13),FCAPU), (SCR(14),IAPUT), (SCR(15),GMA), (SCR(16),VOL)
     *, (SCR(17),PA), (SCR(18),TA), (SCR(19),WTBT)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))

C   1 COMP CODE
C   2 LEG
C   3 STATION IN
C   4 STATION OUT
C   5 SHAFT
C   6 WTF
C   7 CUF
C   8 RI
C   9 DRF
C  10 IAPUT
C  11 EHP

      IRCD = IRCDB(11)
      NL = ID(IRCD+2)
      NST = ID(IRCD+5)
      WTF = D(ID(IRCD+6))
      CUF = D(ID(IRCD+7))
      RI = D(ID(IRCD+8))
      DRF = D(ID(IRCD+9))
      EHP = 0.0
      IF (NL.EQ.0) GO TO 1
      NSI = ID(IRCD+3)
      NSO = ID(IRCD+4)
      PA = 0.5*(D(IP+NSI)+D(IP+NSO))
      TA = 0.5*(D(IT+NSI)+D(IT+NSO))
      PR = D(IP+NSO)/D(IP+NSI)
      GMA = GAM(NL,PA,TA)
      EHP = EHP + D(IW+NL) * SHP(NL,PA,TA,D(IH+NSI)) * D(IT+NSO) *
     *(1.0-PR**((1.0-GMA)/GMA))/42.42
    1 IF (NST.EQ.0) GO TO 2
      SHPP = D(IGA+90+NST)
      IF (SHPP.LE.0.0) GO TO 3
      CERR = CERR+1
      CALL LINES(2)
      WRITE (OUT,1000) CERR,SHPP
 1000 FORMAT(6H0ERROR,I6,5X,23HINVALID APU SHAFT POWER,E15.6)
      GO TO 99
    3 EHP = EHP - SHPP
    2 EHPT = EHP+D(ID(IRCD+11))
      IF (EHPT.EQ.0.0) GO TO 99
      IAPUT = ID(IRCD+10)
      IF (IAPUT.EQ.2) GO TO 12
      WTBT = 0.398*EHPT/(4.215E-4*EHPT+0.159)
      WTC = WTBT*EHP/EHPT
      IF (WTF.NE.0.0) WTC = WTC*WTF
      WTDC = 0.1809*WTC
      VOL = EHP/(1.343E-5*EHPT+2.95E-3)
C$ DONT LET FUEL FLOW DROP FOR BIG HP
C$WAS FCAPU = EHP*(1.5-1.158E-3*EHPT) DONT LET FUEL FLOW DROP FOR BIG HP
      FCAPU = EHP*AMAX1((1.5-1.158E-3*EHPT), 0.6)
      GO TO 13
   12 WTBT = 0.398*EHPT/(6.29E-4*EHPT+0.253)
      WTC = WTBT*EHP/EHPT
      IF (WTF.NE.0.0) WTC = WTC*WTF
      WTDC = 0.3241*WTC
      VOL = EHP/(9.919E-6*EHPT+6.83E-3)
C$WAS FCAPU = EHP*(1.55-2.43E-3*EHPT)
      FCAPU = EHP* AMAX1((1.55-2.43E-3*EHPT), 0.6)
   13 WTIC = 1.5126*WTC
      CUC = 7.3*EHP
      IF (CUF.NE.0.0) CUC = CUC*CUF
      RIC = 0.24971
      IF (RI.NE.0.0) RIC = RI
      DRC = 1.15
      IF (DRF.NE.0.0) DRC = DRC*DRF
      IF (DRC.GT.10.0) DRC =10.0
      CALL SSA
      FC = FC+FCAPU
      CALL SCO
      IF (NL.EQ.0) GO TO 91
      CALL SCI(NL,NSI,NSO)
   91 IF (NST.EQ.0) GO TO 92
      CALL LINES(2)
      WRITE (OUT,1010) NST,SHPP
 1010 FORMAT(1H0,5X,6HSHAFT ,I4,3X,3HSHP,F8.2)
   92 CALL LINES(2)
      WRITE (OUT,1011) VOL,WTBT,EHPT,FCAPU
 1011 FORMAT(1H0,5X,3HVOL,F7.0,3X,4HWTBT,F6.1,3X,4HEHPT,F8.2,3X,2HFC,
     *F8.0)
      D(IGA+71) = VOL
   99 CONTINUE
      RETURN
C     APUSP
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE READS, CHECKS AND STORES THE DATA FOR THE
C       SIZING Z PHASE FOR AN APU
C
C**********************************************************************
C
      SUBROUTINE APUSZ

      COMMON /CC/ C(600)
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))
     *, (SCR(1),C(151))
      DIMENSION ICV(18), SCR(30)
      INTEGER CERR,OUT
      EQUIVALENCE (SCR(1),NL)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))
      DATA IFTA/010/

C   1 COMP CODE
C   2 LEG
C   3 STATION IN
C   4 STATION OUT
C   5 SHAFT
C   6 WTF
C   7 CUF
C   8 RI
C   9 DRF
C  10 IAPUT
C  11 EHP

      I = IACDB(11)
      ID(I+1) = ICV(1)
      IF (ICV(2).EQ.0) GO TO 1
      NL = ILEGN(ICV(2))
      ID(I+2) = NL
      ID(I+3) = ISTAN(ICV(3))
      ID(I+4) = ISTAN(ICV(4))
      CALL FTL(NL,IFTA)
    1 IF (ICV(5).EQ.0) GO TO 2
      ID(I+5) = ICV(5)
      CALL SSR(ICV(5))
    2 ID(I+6) = IPARM(ICV(6))
      ID(I+7) = IPARM(ICV(7))
      ID(I+8) = IPARM(ICV(8))
      ID(I+9) = IPARM(ICV(9))
      ID(I+11) = IPARM(ICV(11))
      ID(I+10) = ICV(10)
      IF (ICV(10).GT.0 .AND. ICV(10).LE.2) GO TO 99
      CERR = CERR+1
      CALL LINES(2)
      WRITE (OUT,1000) CERR,ICV(10)
 1000 FORMAT(6H0ERROR,I6,5X,16HINVALID APU TYPE,I6)
   99 CONTINUE
      RETURN
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR
C       A BOILER
C
C**********************************************************************
C
      SUBROUTINE BOILPP

      COMMON /CC/ C(600)
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))
     *, (IH,C(30)), (IGA,C(35)), (ICPP,C(88)), (IFP,C(22)), (OUT,C(7))
     *, (SCR(1),C(151)), (ILN,C(37)), (PASS,C(17)), (CERR,C(16))
     *, (IIOP,C(90)), (IFB,C(55))
      INTEGER PASS, OUT, CERR
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)
     *, (SCR(4),TB), (SCR(5),EFF), (SCR(6),PD), (SCR(7),TAVG)
     *, (SCR(8),Q), (SCR(9),WB), (SCR(10),HS), (SCR(11),SHR)
     *, (SCR(12),TDAR), (SCR(13),ITER), (SCR(14),II), (SCR(15),IFPS)
     *, (SCR(16),SHRN), (SCR(17),IDC), (SCR(18),IDCN)
     *, (SCR(19),IFL)
      DIMENSION SCR(30)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))

C   1 COMP CODE
C   2 LEG
C   3 STATION IN
C   4 STATION OUT
C   5 SIGMA OPTION
C   6 PD TABLE NO.
C   7 BOILING TEMPERATURE
C   8 EFF TABLE NO.
C   9 HFG

      IRCD = IRCDB(9)
      NL = ID(IRCD+2)
      NSI = ID(IRCD+3)
      NSO = ID(IRCD+4)
      D(IH+NSO) = D(IH+NSI)
      D(IGA+43) = D(IP+NSI)
      TB = D(ID(IRCD+7))
      D(IGA+26) = TB
      IF (PASS.NE.1) GO TO 9
      SHR = 1.0
      GO TO 11
    9 SHR = D(IRCD-9)
   11 EFF = 0.0
      Q = 0.0
      IF(D(IT+NSI).LE.TB) GO TO 1
      IF (D(IH+NSI).EQ.0.0) GO TO 10
      II = 0
      ITER = 0
      IFPS = IFP
      IFP = 0
      IDCN = 0
      GO TO 4
   10 II = -1
    4 D(IGA+41) = D(IW+NL)*SHR
      EFF = TLUP(ID(IRCD+8))
      D(IT+NSO) = D(IT+NSI) - EFF * (D(IT+NSI) - TB)
      GO TO 3
    1 D(IT+NSO) = D(IT+NSI)
      II = -2
    3 D(IGA+41) = D(IW+NL)
      TAVG = 0.5 * (D(IT+NSI) + D(IT+NSO) )
      PD = TLUP(ID(IRCD+6))
      IF(ID(IRCD+5).EQ.0) GO TO 2
      PD = PD/SIG(NL,D(IP+NSI),TAVG)
    2 D(IP+NSO) = D(IP+NSI) - PD
      IF (II.EQ.-2) GO TO 8
      IF (II.EQ.-1) GO TO 7
      CALL TDA(NL,NSI,NSO,TDAR)
      IF (D(IT+NSI).EQ.D(IT+NSO)) GO TO 12
      SHRN = (D(IT+NSI)-TDAR)/(D(IT+NSI)-D(IT+NSO))
      GO TO 13
   12 SHRN = 1.0
   13 IDC = IDCN
      IDCN = 1
      IF (SHRN.LE.SHR) IDCN = -1
      IF (ABS(SHR-SHRN).LE.5.E-5) GO TO 5
      ITER = ITER+1
      IF (ITER.GT.20) GO TO 6
      IF (IDC.NE.IDCN) GO TO 30
      SHR = SHRN
      GO TO 4
   30 SHR = 0.5*(SHR+SHRN)
      GO TO 4
    6 IF (IFP.EQ.0) GO TO 5
      WRITE (OUT,1013) ID(ILN+NL)
 1013 FORMAT(1H0,5X,37H***NON-CONVERGENCE SHR - BOILER - LEG,I6)
    5 IF (II.NE.0) GO TO 7
      IF (IFPS.EQ.0) GO TO 7
      II = II+1
      IFP = IFPS
      GO TO 4
    7 IFL = ID(IFB+NL)
      IF (ID(IFL+1).EQ.1) GO TO 14
   15 Q = D(IW+NL) * SHP(NL,D(IP+NSI),TAVG,D(IH+NSI)) * (D(IT+NSO) -
     * D(IT+NSI)) * SHR
      GO TO 8
   14 IF (ID(ID(IFL+3)-1).EQ.12) GO TO 15
      Q = D(IW+NL) * (HFT(NL,D(IP+NSI),D(IT+NSO)) - HFT(NL,D(IP+NSI),
     * D(IT+NSI)))
    8 D(IRCD-6) = TB
      D(IRCD-7) = EFF
      D(IRCD-8) = Q
      IF (IIOP.EQ.0) D(IRCD-9) = SHR
      IF (IFP.NE.1) GO TO 99
      IF (EFF.GE.0.0 .AND. EFF.LE.1.0) GO TO 98
      CERR = CERR+1
      CALL LINES(2)
      WRITE(OUT,1011) CERR
 1011 FORMAT(6H0ERROR,I6,5X,21HINVALID EFFECTIVENESS)
      GO TO 97
   98 CONTINUE
      IF (ICPP.NE.0) GO TO 99
   97 CONTINUE
      CALL PIOP(1,NL,NSI,NSO)
      IF (D(IH+NSI).EQ.0.0) GO TO 96
      CALL TDB1(NL,NSO,NSO,HS)
      IF (D(IH+NSO).GT.HS) CALL HSOP(HS)
   96 CONTINUE
      CALL LINES(2)
      WRITE (OUT,1001) TB,EFF,Q,SHR
 1001 FORMAT(1H0,5X,2HTB,F8.2,3X,3HEFF,F7.4,3X,1HQ,F9.2,3X,3HSHR,F7.4)
      IF (ID(IRCD+9).EQ.0) GO TO 99
      WB = Q/D(ID(IRCD+9))
      WRITE (OUT,1002) WB
 1002 FORMAT(1H+,57X,2HWB,F8.2)
   99 CONTINUE
      RETURN
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE
C      PERFORMANCE OF BOILER
C
C**********************************************************************
C
      SUBROUTINE BOILPZ

      COMMON /CC/ C(600)
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))
      DIMENSION ICV(18), SCR(30)
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))
      DATA IFTA/011/

C   1 COMP CODE
C   2 LEG
C   3 STATION IN
C   4 STATION OUT
C   5 SIGMA OPTION
C   6 PD TABLE NO.
C   7 BOILING TEMPERATURE
C   8 EFF TABLE NO.
C   9 HFG

      I = IACDB(9)
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
      ID(I+6) = ITIDN(ICV(6),1)
      ID(I+7) = IPARM(ICV(7))
      ID(I+8) = ITIDN(ICV(8),5)
      ID(I+9) = IPARM(ICV(9))
      RETURN
C     BOILPZ
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR
C      A BOILER
C
C**********************************************************************
C
      SUBROUTINE BOILSP

      COMMON /CC/ C(600)
      COMMON /DT/ DTES(64001)
      EQUIVALENCE (IDS,C(5)), (PASS,C(17)), (IGA,C(35))
     *, (ISV,C(47)), (IEV,C(49)), (NSV,C(50)), (NEV,C(51))
     *, (IMP,C(66)), (IME,C(67)), (NIT,C(76)), (PF,C(77)), (NWT,C(78))
     *, (ITAD,C(54)), (ICONV,C(79)), (OUT,C(7)), (CERR,C(16))
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (IFB,C(55)), (WTC,C(102))
     *, (CUC,C(104)), (RIC,C(106)), (DRC,C(108)), (WTIC,C(109))
     *, (WTDC,C(111)), (SCR(1),C(151)), (IRCD,C(45))
     *, (ICV(1),C(133)), (IFP,C(22)), (WTS,C(101))
     *, (ILN,C(37)), (PUN,C(91))
     *, (EWT,C(128)), (IH,C(30))
      DIMENSION SCR(30), ICV(18)
      INTEGER OUT, CERR, PASS, PUN
      EQUIVALENCE         (ICV( 1),NL    ), (ICV( 2),NSI   )
     *, (ICV( 3),NSO   ), (ICV( 4),LH    ), (ICV( 5),LC    )
     *, (ICV( 6),LN    ), (ICV( 7),IFGH  ), (ICV( 8),IFGC  )
     *, (ICV( 9),TSP   ), (ICV(10),IMH   ), (ICV(11),IMC   )
     *, (ICV(12),NP    ), (ICV(13),PSI   ), (ICV(14),DENI  )
     *, (ICV(15),DENO  ), (ICV(16),PA    ), (ICV(17),TA    )
     *, (ICV(18),VSC   )
      EQUIVALENCE         (SCR( 1),CP    ), (SCR( 2),PR    )
     *, (SCR( 3),IDSS  ), (SCR( 4),ISVS  ), (SCR( 5),IEVS  )
     *, (SCR( 6),IEM   ), (SCR( 7),DSV   ), (SCR( 8),PAR   )
     *, (SCR( 9),JS    ), (SCR(10),IS    ), (SCR(11),WT    )
     *, (SCR(12),NTU   ), (SCR(13),EFF   ), (SCR(14),ETA   )
     *, (SCR(15),REP   ), (SCR(16),EV1P1 ), (SCR(17),II    )
     *, (SCR(18),I     ), (SCR(19),EFFP  ), (SCR(20),K     )
     *, (SCR(21),J     ), (SCR(22),IND   ), (SCR(23),JSV1  )
     *, (SCR(24),JSV2  ), (SCR(25),JEV1  ), (SCR(26),JEV2  )
     *, (SCR(27),IX    ), (SCR(28),NH    ), (SCR(29),NC   )
     *, (SCR(30),RHOF  )
      DIMENSION CD(200)
      real*8 cd, cn
      EQUIVALENCE (CD(1),C(201))
      EQUIVALENCE (CN,CD(84))

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))
      REAL LH,LC,LN,NTU
      REAL NH, NC
      DATA SVLL/0.1/, SVUL/100.0/, SVIG/ 6.0/, ERL1/0.01/, ERL2/0.0001/
     *, AR/1.0/
      DATA SN/4HBOIL/

C   1 COMP CODE
C   2 NL
C   3 NSI
C   4 NSO
C   5 WTF
C   6 CUF
C   7 RI
C   8 DRF
C   9 BT
C  10 DEN
C  11 HFG
C  12 TIME
C  13 WTE
C  14 IBT
C  15 LH
C  16 LC
C  17 LN
C  18 NPH
C  19 IMH
C  20 IMC
C  21 IFGH
C  22 IFH
C  23 IJH
C  24 IFGC
C  25 IMSP
C  26 ITSP
C  27 IMW
C  28 MAP
      IRCD = IRCDB(28)
      CN = SN
      NL = ID(IRCD+2)
      NSI = ID(IRCD+3)
      NSO = ID(IRCD+4)
      LH = D(ID(IRCD+15))
      LC = D(ID(IRCD+16))
      LN = D(ID(IRCD+17))
      IMH = ITLUP(ID(IRCD+19))
      IMC = ITLUP(ID(IRCD+20))
      IFGH = ITLUP(ID(IRCD+21))
      IFGC = ITLUP(ID(IRCD+24))
      TSP = TLUP(ID(IRCD+26))
      PA = 0.5*(D(IP+NSI)+D(IP+NSO))
      TA = 0.5*(D(IT+NSI)+D(IT+NSO))
      SHR = (D(IT+NSI)-TDBI(NL,NSI,NSO))/(D(IT+NSI)-D(IT+NSO))
      CP = SHP(NL,PA,TA,D(IH+NSI))
     * *SHR
      NP = ID(IRCD+18)
      DENI = DEN(NL,D(IP+NSI),D(IT+NSI))
      DENO = DEN(NL,D(IP+NSO),D(IT+NSO))
      VSC = VIS(NL,PA,TA)
      PR = (3600.0*VSC*CP/COND(NL,PA,TA))**.6666667
      IF (D(IT+NSI).LE.D(ID(IRCD+9))) GO TO 96
      REP = 0.8*D(IFGH+4)*D(IW+NL)*NP/(VSC*AR)
      EV1P1 = VSC**2/(1029.568*D(IFGH+4)**2*DENI)
      EV1P2 = NP*(DENI/DENO+1.0)/(2.0*D(IFGH+4))
      EP = CP*VSC*(1.0+D(IFGH+5)/D(IFGH+7))/(D(IFGH+4)*D(IFGH+5)*PR*
     *POLYI(2,D(IMH+5),TA))*1800.0
      IF (D(IFGH+8).EQ.3.0) GO TO 301
      EFL = D(IFGH+3)/2.0
      GO TO 302
  301 EFL = SQRT(D(IFGH+3)**2+1.0/D(IFGH+2)**2)/2.0
  302 CONTINUE
      IDSS = IDS
      ICONV = 0
      IF (LN.NE.0.0) GO TO 200
      PASS = 1
      IFP = 0
      NSV = 2
      NEV = 2
      NUM = 4
      EFFP = (D(IT+NSI)-D(IT+NSO))/(D(IT+NSI)-D(ID(IRCD+9)))
      CALL GDCU(NSV,4,2,D,ISV)
      CALL GDCU(NEV,4,2,D,IEV)
      CALL GDCU(NSV,4,2,D,ISVS)
      CALL GDCU(NEV,4,2,D,IEVS)
      CALL GDCU(NUM,4,2,D,IMP)
      CALL GDCU(NSV,4,2,D,IME)
      CALL GDCU(NSV,4,2,D,IEM)
      D(ISV+1) = SVIG
      D(ISV+2) = SVIG
      ASSIGN 4 TO JS
      GO TO 300
    4 IF (IDS.EQ.0) GO TO 1
      ASSIGN 1 TO IS
      GO TO  101
    1 CONTINUE
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
    2   D(IEVS+I) = D(IEV+I)

      DO 11 J=1,NSV
         D(ISV+J) = D(ISVS+J)*PF
         DSV = D(ISV+J)-D(ISVS+J)
         ASSIGN 5 TO JS
         GO TO 300
    5    DO 14 K=1,NEV
            PAR = (D(IEV+K)-D(IEVS+K))/DSV
            CALL MSTO(K,J,PAR)
   14    CONTINUE
         CALL MSTO(J,0,D(IEVS+J))
         ASSIGN 21 TO IS
         IF (IDS.EQ.3) GO TO 101
   21    CONTINUE
         D(ISV+J) = D(ISVS+J)
   11 CONTINUE
      IF (IDS.GE.2) CALL MPRNT(D(IMP+1),NSV,D(IME+1))
      CALL MSOL(D(IMP+1),NSV,D(IME+1),D(IEM+1),IND)
      IF (IND.NE.1) GO TO 40
      DO 16 J=1,NSV
         D(ISV+J) = D(ISV+J)+WT*D(IMP+J)
         IF (D(ISV+J).GT.SVUL) D(ISV+J) = SVUL
         IF (D(ISV+J).LT.SVLL) D(ISV+J) = SVLL
   16 CONTINUE
      ASSIGN 6 TO JS
      GO TO 300
    6 ASSIGN 20 TO IS
      IF (IDS.GE.1) GO TO 101
   20 CONTINUE
      ICONV = 0
      IF (ABS(D(IEV+1)).GT.ERL1 .OR. ABS(D(IEV+2)).GT.ERL2) ICONV = 1
      IF (ICONV.EQ.0) GO TO 199
   10 CONTINUE
   17 WTC = 0.0
      RIC = 0.0
      CUC = 0.0
      DRC = 0.0
      CALL FDC(0,4,2,D,0)
      GO TO 98
   40 CERR = CERR+1
      CALL LINES(2)
      WRITE (OUT,1009) CERR
 1009 FORMAT(6H0ERROR,I6,5X,15HSINGULAR MATRIX)
      ICONV = 1
      GO TO 17
  101 CONTINUE
      JSV1 = ISV+1
      JSV2 = ISV+NSV
      JEV1 = IEV+1
      JEV2 = IEV+NEV
      CALL LINES(10)
      WRITE (OUT,1003) PASS
 1003 FORMAT(6H0PASS ,I6)
      WRITE (OUT,1004) (D(IX),IX=JSV1,JSV2)
 1004 FORMAT(5H0S.V.,/, (1X,10E12.5))
      WRITE (OUT,1005) (D(IX),IX=JEV1,JEV2)
 1005 FORMAT(6H0E.V.1,/,(1X,10E12.5))
C1005 FORMAT(5H0E.V.,/, (1X,10E12.5))
      WRITE (OUT,1000) D(IGA+21),NTU,EFF,ETA
 1000 FORMAT(1H0,4E12.5)
      GO TO IS, (1,20,21)
  300 CONTINUE
      NH = (AR*D(ISV+2)-D(IFGC+3)-2.0*TSP)/(D(IFGH+3)+D(IFGC+3)+2.0*TSP)
      NC = NH+1.0
      PSI = D(IFGH+3)*D(IFGH+4)*D(IFGH+6)*NH/((D(IFGH+3)+D(IFGC+3)+
     *2.0*TSP)*NH+D(IFGC+3)+2.0*TSP)
      D(IGA+21) = REP/(PSI*D(ISV+2)**2)
      D(IEV+1) = EV1P1*(1.0+PSI**2)*D(IGA+21)**2*(D(ISV+1)*
     *TLUP(ID(IRCD+22))*EV1P2/(1.0+PSI**2)
     * +DENI/DENO-1.0) - (D(IP+NSI)-D(IP+NSO))
      IF (D(IP+NSI)-D(IP+NSO).NE.0)
     CDTES(IEV+1) = EV1P1*(1.0+PSI**2)*D(IGA+21)**2*(D(ISV+1)*
     CTLUP(ID(IRCD+22))*EV1P2/(1.0+PSI**2)
     C +DENI/DENO-1.0) / (D(IP+NSI)-D(IP+NSO))
      AJ = TLUP(ID(IRCD+23))
      ETA = SQRT(EP*D(IGA+21)*AJ)
      ETA = 1.0-D(IFGH+1)*(1.0-TANH(ETA*EFL)/(ETA*EFL))
      NTU = ETA*AJ*D(ISV+1)*NP/(D(IFGH+4)*PR)
      EFF = 1.0-EXP(-NTU)
      D(IEV+2) = EFF-EFFP
      IF(EFFP.NE.0) DTES(IEV+2) = EFF/EFFP
      GO TO JS, (4,5,6,7)
  199 CONTINUE
      ASSIGN 7 TO JS
      IFP = 1
      GO TO 300
    7 LH = D(ISV+1)
      LC = D(ISV+2)
      LN = AR*D(ISV+2)
      CALL FDC(0,4,2,D,0)
  200 NH = (LN-D(IFGC+3)-2.0*TSP)/(D(IFGH+3)+D(IFGC+3)+2.0*TSP)
      NC = NH+1.0
      PSI = D(IFGH+3)*D(IFGH+4)*D(IFGH+6)*NH/((D(IFGH+3)+D(IFGC+3)+
     *2.0*TSP)*NH+D(IFGC+3)+2.0*TSP)
      VOL = LH*LC*LN
      IFG = IFGH
      IM = IMH
      JJ = 0
   70 IFS = D(IFG+8)+0.1
      BB = (D(IFG+3)-(D(IFG+9)-1.0)*TSP)/D(IFG+9)
      IF (IFS-2) 71,72,73
   71 RHOF = D(IFG+5)*D(IFG+2)*D(IM+1)*(BB+1.0/D(IFG+2)
     * -2.71682*D(IFG+5))/BB
      GO TO 74
   72 RHOF = D(IFG+5)*D(IFG+2)*D(IM+1)*(BB+0.5708/D(IFG+2)-D(IFG+5))/BB
      GO TO 74
   73 RHOF = (6.0*D(IFG+5)*D(IFG+2)**2*BB-1.0+SQRT(1.0+D(IFG+2)**2*BB*BB
     * -12.0*D(IFG+5)*D(IFG+2)**2*BB))/(0.3333/D(IFG+5)-12.0*D(IFG+5)
     * *D(IFG+2)**2)
      PHI = 2.0*ACOS(3.0*D(IFG+5)/(RHOF+3.0*D(IFG+5)))
      RHOF = 1.0/(D(IFG+2)*COS(PHI/2.0))-6.0*D(IFG+5)*D(IFG+2)
     * *(BB+2.0*RHOF)
      RHOF = D(IFG+2)*D(IFG+5)*D(IM+1)*(RHOF+3.0*D(IFG+5)*PHI)/BB
   74 IF (JJ.EQ.0) GO TO 75
      WTFC = RHOF*NC*(D(IFGC+3)-(D(IFGC+9)-1.0)*TSP)*LH*LC/1728.0
      GO TO 80
   75 WTFH = RHOF*NH*(D(IFGH+3)-(D(IFGH+9)-1.0)*TSP)*LH*LC/1728.0
      IM = IMC
      JJ = 1
      IFG = IFGC
      GO TO 70
   80 IMSP = ITLUP(ID(IRCD+25))
      WTCO = WTFH+WTFC+D(IMSP+1)*LH*LC*TSP*(NH*D(IFGH+9)+NC*D(IFGC+9)
     * +1.0)/1728.0
      IF (D(IMSP+1).GT.460.0) WTCO = WTCO+6.44E-4*LH*LC
     * *(NH*D(IFGH+9)+NC*D(IFGC+9)+1.0)
      WTW = D(IW+NL)*CP*(D(IT+NSI)-D(IT+NSO))*D(ID(IRCD+12))
     * /D(ID(IRCD+11))+D(ID(IRCD+13))
      EWT = EWT+WTW
      WTS = WTS+WTW
      VW = WTW/D(ID(IRCD+10))*1728.0
      IMW = ITLUP(ID(IRCD+27))
      IBT = ID(IRCD+14)
      IF (IBT.EQ.2) GO TO 150
      WTC = WTCO+3.4722E-4*D(IMW+1)*(VW+VOL)**.6666667
      GO TO 160
  150 WTC = WTCO+3.0382E-4*D(IMW+1)*VW**.6666667
  160 WTF = D(ID(IRCD+5))
      IF (WTF.NE.0.0) WTC = WTC*WTF
      WTIC = 0.205*WTC
      WTDC = 0.12*WTC
      RIC = 0.02165
      RI = D(ID(IRCD+7))
      IF (RI.NE.0.0) RIC = RI
      CUC = 29.7+2.72*WTC
      CUF = D(ID(IRCD+6))
      IF (CUF.NE.0.0) CUC = CUC*CUF
      PRES = PA
      IF (PRES.LT.314.7) PRES = 314.7
      DRC = 1.15*(1.0+((PRES-314.7)/283.0)**2)
      DRF = D(ID(IRCD+8))
      IF(DRF.NE.0.0) DRC = DRC*DRF
      IF(DRC.GT.10.0) DRC = 10.0
      IFT = ID(IFB+NL)
      IFT = ID(IFT+1)
      IF (IFT.EQ.2) GO TO 97
      VL = PSI*VOL
   97 CALL SSA
   98 CALL SCO
      IDS = IDSS
      CALL SCI(NL,NSI,NSO)
      IF (ICONV.EQ.0) GO TO 85
      CERR = CERR+1
      CALL LINES(2)
      WRITE (OUT,1010) CERR
 1010 FORMAT(6H0ERROR,I6,5X,14HNON CONVERGENT)
      GO TO 99
   85 CONTINUE
      CALL LINES(2)
      WRITE (OUT,1002) LH,LC,LN,VOL,WTCO,WTW
 1002 FORMAT(1H0,5X,2HLH,F8.2,3X,2HLC,F8.2,3X,2HLN,F8.2,3X,3HVOL,F7.0,3X
     *,3HWTC,F7.1,3X,3HEWT,F7.1)
      IF (D(ID(IRCD+17)).NE.0.0) GO TO 86
      WRITE (OUT,1006) EFF,NTU
 1006 FORMAT(1H+,83X,3HEFF,F7.4,3X,3HNTU,F7.3)
   86 IF (IFT.EQ.1) WRITE (OUT,1007) VL
 1007 FORMAT(1H+,109X,2HVL,F8.1)
      D(IGA+71) = LH
      D(IGA+72 ) = LC
      D(IGA+73 ) =LN
      D(IGA+74) = VOL
      D(IGA+75) = VL
      MAP = ID(IRCD+28)
      IF (MAP.EQ.0) GO TO 99
      IFP = 0
      CALL GDCU(48,4,2,D,LOC)
      LOCE = LOC+16
      LOCP = LOC+32
      CP = CP/SHR
      PR = PR/(SHR**.6666667)
      EP = EP/(SHR**.3333333)
      REP = 0.8*D(IFGH+4)*NP/(VSC*PSI*AR*LC**2)
      EV1P1 = EV1P1*(1.0+PSI**2)
      EV1P2 = EV1P2/(1.0+PSI**2)
      IFTC = ID(IFB+NL)
      IFTC = ID(IFTC+1)
      IF (IFTC.EQ.2) SIGA = SIG(NL,PA,TA)
      DO 3050 I=1,16
      W = I*0.2*D(IW+NL)
      D(IGA+21) = REP*W
      PD = EV1P1*D(IGA+21)**2*(LH*TLUP(ID(IRCD+22))*EV1P2+DENI/DENO-1.0)
      IF (IFTC.EQ.2) PD = PD*SIGA
      AJ = TLUP(ID(IRCD+23))
      ETA = SQRT(EP*D(IGA+21)*AJ)
      ETA = 1.0-D(IFGH+1)*(1.0-TANH(ETA*EFL)/(ETA*EFL))
      NTU = ETA*AJ*LH*NP/(D(IFGH+4)*PR)
      EFF = 1.0-EXP(-NTU)
      IF (EFF.GT.1.0) EFF = 1.0
      D(LOC+I) = W
      D(LOCE+I) = EFF
      D(LOCP+I) = PD
 3050 CONTINUE
      I1 = LOC+1
      I2 = LOC+16
      IF (MAP.EQ.2) GO TO 3001
      CALL LINES(12)
      WRITE (OUT,2009)
 2009 FORMAT(11H BOILER MAP)
      WRITE (OUT,2011)
 2011 FORMAT(6H0    *,40HTABID      1   5   2   0   0  41 0 1  16,40X,1H
     **)
      WRITE (OUT,2012)
 2012 FORMAT(5X,1H*,16HTABT  BOILER EFF,64X,1H*)
      WRITE (OUT,2010) (D(I),D(I+16),I=I1,I2)
 2010 FORMAT(5X,1H*,6HTABV  ,4X,4F10.4,30X,1H*)
 3001 CONTINUE
      IF (MAP.EQ.1) GO TO 3011
      WRITE (PUN,2001)
 2001 FORMAT(40HTABID      1   5   2   0   0  41 0 1  16,40X)
      WRITE (PUN,2002)
 2002 FORMAT(16HTABT  BOILER EFF,64X)
      WRITE (PUN,2000) (D(I),D(I+16),I=I1,I2)
 2000 FORMAT(6HTABV  ,4X,4F10.4,30X)
 3011 CONTINUE
      IF (MAP.EQ.2) GO TO 3004
      CALL LINES(11)
      WRITE (OUT,2013)
 2013 FORMAT(6H0    *,40HTABID      1   1   2   3   0  41 0 1  16,40X,1H
     **)
      WRITE (OUT,2014)
 2014 FORMAT(5X,1H*,15HTABT  BOILER PD,65X,1H*)
      WRITE (OUT,2010) (D(I),D(I+32),I=I1,I2)
 3004 CONTINUE
      IF (MAP.EQ.1) GO TO 3014
      WRITE (PUN,2003)
 2003 FORMAT(40HTABID      1   1   2   3   0  41 0 1  16,40X)
      WRITE (PUN,2004)
 2004 FORMAT(15HTABT  BOILER PD,65X)
      WRITE (PUN,2000) (D(I),D(I+32),I=I1,I2)
 3014 CONTINUE
      CALL FDC(0,4,2,D,0)
      GO TO 99
   96 CERR = CERR+1
      CALL LINES(2)
      WRITE (OUT,1008) CERR
 1008 FORMAT(6H0ERROR,I6,5X,28HBOILING TEMPERATURE TOO HIGH)
      WTC = 0.0
      RIC = 0.0
      CUC = 0.0
      DRC = 0.0
      CALL SCO
      CALL SCI(NL,NSI,NSO)
   99 CONTINUE
      RETURN
C     BOILSP
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE READS, CHECKS, AND STORES THE DATA FOR THE Z
C       PHASE FOR A BOILER
C
C**********************************************************************
C
      SUBROUTINE BOL1SZ

      COMMON /CC/ C(600)
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))
     *, (IAUR,C(19)), (SCR(1),C(151)), (MDPC,C(34))
     *, (ISP,C(42))
      DIMENSION ICV(18), SCR(30)
      INTEGER OUT,CERR
      EQUIVALENCE (SCR(1),IPRNT), (SCR(2),NL), (SCR(3),CC), (SCR(4),NC)
     *, (SCR(5),LOC)
      DIMENSION CD(200)
      real*8 cd, cn, sd
      EQUIVALENCE (CD(1),C(201))
      EQUIVALENCE (CN,CD(84))

      COMMON /CS/ SD(100)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))
      DATA IFTA/011/

C   1 COMP CODE
C   2 NL
C   3 NSI
C   4 NSO
C   5 WTF
C   6 CUF
C   7 RI
C   8 DRF
C   9 BT
C  10 DEN
C  11 HFG
C  12 TIME
C  13 WTE
C  14 IBT
C  15 LH
C  16 LC
C  17 LN
C  18 NPH
C  19 IMH
C  20 IMC
C  21 IFGH
C  22 IFH
C  23 IJH
C  24 IFGC
C  25 IMSP
C  26 ITSP
C  27 IMW
C  28 MAP

      IE = 1
      I = IACDB(28)
      IPRNT = 10*(MDPC/10)
      IPRNT = IPRNT-100*(MDPC/100)
      IF (MDPC.LT.0) IPRNT = -1
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
      ID(I+10) = IPARM(ICV(10))
      ID(I+11) = IPARM(ICV(11))
      ID(I+12) = IPARM(ICV(12))
      ID(I+13) = IPARM(ICV(13))
      ID(I+14) = ICV(14)
      ID(I+28) = ICV(15)
      IF (ICV(14).GT.0 .AND. ICV(14).LE.2) GO TO 1
      CERR = CERR+1
      CALL LINES(2)
      WRITE (OUT,1005) CERR,ICV(14)
 1005 FORMAT(6H0ERROR,I6,5X,19HINVALID BOILER TYPE,I6)
    1 READ (IAUR,1200) CN,CC,NC,(ICV(II),II=2,18)
 1200 FORMAT(A6,A2,I4,17A4)
      BACKSPACE IAUR
      READ (IAUR,1000) CN,CC,NC,(ICV(II),II=2,18)
 1000 FORMAT(A6,A2,18I4)
      IF (CN.EQ.SD(ICV(1)+1)) GO TO 5
      CERR = CERR+1
      CALL LINES(2)
      WRITE (OUT,1002) CERR
 1002 FORMAT(6H0ERROR,I6,5X,13HNO BOIL2 CARD)
      BACKSPACE IAUR
      GO TO 99
    5 IF (IPRNT.NE.0) GO TO 51
      CALL LINES(1)
      WRITE (OUT,1004) CN,CC,NC,(ICV(II),II = 2,18)
 1004 FORMAT(5X,A6,1X,A2,18(1X,I4))
   51 ID(I+15) = IPARM(ICV(2))
      ID(I+16) = IPARM(ICV(3))
      ID(I+17) = IPARM(ICV(4))
      ID(I+18) = ICV(5)
      ID(I+19) = ITIDN(ICV(6),20)
      ID(I+20) = ITIDN(ICV(7),20)
      ID(I+21) = ITIDN(ICV(8),31)
      ID(I+22) = ITIDN(ICV(8),32)
      ID(I+23) = ITIDN(ICV(8),33)
      ID(I+24) = ITIDN(ICV(9),31)
      ID(I+25) = ITIDN(ICV(10),20)
      ID(I+26) = ITIDN(ICV(11),34)
      ID(I+27) = ITIDN(ICV(12),20)
      IF (ICV(2).EQ.0 .AND. ICV(3).EQ.0 .AND. ICV(4).EQ.0) GO TO 99
      IF (ICV(2).NE.0 .AND. ICV(3).NE.0 .AND. ICV(4).NE.0) GO TO 99
      CALL LINES(2)
      CERR = CERR+1
      WRITE (OUT,1010) CERR
 1010 FORMAT(6H0ERROR,I6,5X,20HMIXED SIZE SPECIFIED)
      GO TO 99
      ENTRY BOL2SZ
      IE = 2
      CERR = CERR+1
      CALL LINES(2)
      WRITE (OUT,1001) CERR,SD(ICV(1)),SD(ICV(1)-1)
 1001 FORMAT(6H0ERROR,I6,5X,A6,21HCARD NOT PRECEDED BY ,A6,4HCARD)
      I = IACDB(28)
      GO TO 51
   99 CONTINUE
      RETURN
C     BOL1SZ
      END

C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE CCSPZ                                                          
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
     *, (CERR,C(16)), (OUT,C(7)), (IAUR,C(19)), (MDPC,C(34))                    
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER CERR, OUT                                                         
      DIMENSION CD(200)                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84)), (COMEND,CD(14)), (CMPNNT,CD(39))                 
      EQUIVALENCE (SCR(1),IND), (SCR(2),VAL), (SCR(3),CC), (SCR(4),NC)          
     *, (SCR(5),IPRNT), (SCR(6),NCS), (SCR(7),IOPR)                             
                                                                                
      COMMON /CCA/ CA(150)                                                      
      EQUIVALENCE (NOC,CA(1))                                                   
                                                                                
      COMMON /CP/ PD(100)                                                       
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      REAL*8    CN, COMEND, CMPNNT, cd, pd                                                                                                     
      IPRNT = 10*(MDPC/10)                                                      
      IPRNT = IPRNT-100*(IPRNT/100)                                             
      IF (MDPC.LT.0) IPRNT = -1                                                 
      IF (IPRNT.NE.0) GO TO 4                                                   
      CALL LINES(2)                                                             
      WRITE (OUT,1003)                                                          
 1003 FORMAT(11H0SKIP******)                                                    
    4 IF (ICV(2).GE.0 .AND. ICV(3).GE.0) GO TO 1                                
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE(OUT,1000) CERR                                                      
 1000 FORMAT(6H0ERROR,I6,5X,17HINVALID SKIP DATA)                               
      GO TO 97                                                                  
    1 IND = IPARM(ICV(3))                                                       
      VAL = D(IND)                                                              
      IOPR = ICV(4)                                                             
      IF (IOPR.LE.0 .OR. IOPR.GT.5) GO TO 10                                    
      GO TO (11,12,13,14,15), IOPR                                              
   10 IF (VAL.NE.0.0) GO TO 5                                                   
      GO TO 97                                                                  
   11 IF (VAL.EQ.0.0) GO TO 5                                                   
      GO TO 97                                                                  
   12 IF (VAL.GE.0.0) GO TO 5                                                   
      GO TO 97                                                                  
   13 IF (VAL.LE.0.0) GO TO 5                                                   
      GO TO 97                                                                  
   14 IF (VAL.GT.0.0) GO TO 5                                                   
      GO TO 97                                                                  
   15 IF (VAL.LT.0.0) GO TO 5                                                   
      GO TO 97                                                                  
    5 NCS = ICV(2)                                                              
    2 IF (NCS.EQ.0) GO TO 97                                                    
      READ (IAUR,1001) CN,CC,NC,(ICV(II),II=2,18)                               
 1001 FORMAT(A6,A2,18A4)                                                        
      IF (CN.EQ.COMEND) GO TO 98                                                
      IF (CN.EQ.CMPNNT) GO TO 3                                                 
      NOC = NOC+1                                                               
      IF (IPRNT.NE.0) GO TO 3                                                   
      CALL LINES(1)                                                             
      WRITE (OUT,1002) CN,CC,NC,(ICV(II),II=2,18)                               
 1002 FORMAT(2H *,3X,A6,1X,A2,18(1X,A4))                                        
    3 NCS = NCS-1                                                               
      GO TO 2                                                                   
   98 CONTINUE                                                                  
      BACKSPACE IAUR                                                            
   97 CONTINUE                                                                  
      IF (IPRNT.NE.0) GO TO 99                                                  
      CALL LINES(2)                                                             
      WRITE (OUT,1004)                                                          
 1004 FORMAT(11H **********/1H )                                                
   99 CONTINUE                                                                  
      RETURN                                                                    
C     CCSPZ                                                                     
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR
C      LEG AND STATION CONNECTOR
C
C**********************************************************************
C
      SUBROUTINE CNNTPP

      COMMON /CC/ C(600)
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))
     *, (IH,C(30)), (PASS,C(17)), (SCR(1),C(151))
      DIMENSION SCR(30)
      INTEGER PASS
      EQUIVALENCE (SCR(1),NLI), (SCR(2),NSI), (SCR(3),NLO), (SCR(4),NSO)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))

C   1 COMP CODE
C   2 NLI
C   3 NSI
C   4 NLO
C   5 NSO

      IRCD = IRCDB(5)
      NLI = ID(IRCD+2)
      NSI = ID(IRCD+3)
      NLO = ID(IRCD+4)
      NSO = ID(IRCD+5)
      IF (NLI.EQ.NLO) GO TO 1
      D(IW+NLO) = D(IW+NLI)
      IF (PASS.EQ.1) CALL FLUIDP(NLO)
    1 IF (NSI.EQ.NSO) GO TO 99
      D(IP+NSO) = D(IP+NSI)
      D(IT+NSO) = D(IT+NSI)
      D(IH+NSO) = D(IH+NSI)
   99 CONTINUE
      RETURN
C     CNNCTPP
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE
C      PERFORMANCE OF A LEG AND STATION CONNECTOR
C
C**********************************************************************
C
      SUBROUTINE CNNTPZ

      COMMON /CC/ C(600)
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))
     *, (ILR,C(52)), (ISR,C(53))
      DIMENSION ICV(18), SCR(30)
      EQUIVALENCE (SCR(1),NLI), (SCR(2),NSI), (SCR(3),NLO), (SCR(4),NSO)
     *, (SCR(5),IFBL)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))

C   1 COMP CODE
C   2 NLI
C   3 NSI
C   4 NLO
C   5 NSO

      I = IACDB(5)
      ID(I+1) = ICV(1)
      NLI = ILEGN(ICV(2))
      NSI = ISTAN(ICV(3))
      NLO = ILEGN(ICV(4))
      NSO = ISTAN(ICV(5))
      CALL LEGRT(NLI)
      IF (NLI.EQ.NLO) GO TO 1
      IF (ID(ILR+NLO).EQ.0) CALL LEGRS(NLO)
      CALL FRR(NLI,IFBL)
      CALL FRS(NLO,IFBL)
    1 CALL START(NSI)
      IF (NSI.EQ.NSO) GO TO 2
      IF (ID(ISR+NSO).EQ.0) CALL STARS(NSO)
    2 ID(I+2) = NLI
      ID(I+3) = NSI
      ID(I+4) = NLO
      ID(I+5) = NSO
      RETURN
C     CNNTPZ
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR
C      THE SYSTEM CONTROL
C
C**********************************************************************
C
      SUBROUTINE CNTRSP

      COMMON /CC/ C(600)
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151)), (IW,C(27))
     *, (IP,C(28)), (IT,C(29)), (WTC,C(102)), (CUC,C(104))
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))
     *, (SDR,C(122)), (IGA,C(35))
      DIMENSION SCR(30)
      INTEGER OUT
      EQUIVALENCE         (SCR( 1),NL    ), (SCR( 2),NS    )
     *, (SCR( 3),VOL   ), (SCR( 4),WL    ), (SCR( 5),THRUST)
     *, (SCR( 6),WJ    ), (SCR( 7),GMA   ), (SCR( 8),GM    )
     *, (SCR( 9),PR    ), (SCR(10),WTF   ), (SCR(11),CUF   )
     *, (SCR(12),RI    ), (SCR(13),DRF   ), (SCR(14),ISCT  )

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))

C   1 COMP CODE
C   2 WTM
C   3 CUM
C   4 RI
C   5 DRM
C   6 CNTRL TYPE
C   7 KT,NL
C   8 KP,NS
C   9 WTS,DIA,CA
C  10 NSNSR
C  11 NIAO

      IRCD = IRCDB(11)
      WTF = D(ID(IRCD+2))
      ISCT = ID(IRCD+6)
      GO TO (1,1,3,4,5), ISCT
    1 WTC = D(ID(IRCD+7))*D(ID(IRCD+8))*(ID(IRCD+11)/3.0
     * +D(ID(IRCD+9))+0.2*ID(IRCD+10))
      VOL = D(ID(IRCD+7))*D(ID(IRCD+8))*(11.43*ID(IRCD+11)+25.71)
      IF (WTF.NE.0.0) WTC = WTC*WTF
      WTDC = 0.41*WTC
      RIC = 0.04628+0.03781*ID(IRCD+10)
      IF (ISCT.EQ.2) GO TO 10
      CUC = D(ID(IRCD+7))*D(ID(IRCD+8))*WTC*(14.6+0.545*ID(IRCD+11))
      GO TO 50
   10 CUC = D(ID(IRCD+7))*D(ID(IRCD+8))*WTC*(14.85+1.04*ID(IRCD+11))
      GO TO 50
    3 WTC = D(ID(IRCD+7))*D(ID(IRCD+8))*(0.5*D(ID(IRCD+9))+1.75)
      VOL = D(ID(IRCD+7))*D(ID(IRCD+8))*(40.0*D(ID(IRCD+9))+20.0)
      RIC = 0.00582+0.03781*ID(IRCD+10)
      CUC = D(ID(IRCD+7))*D(ID(IRCD+8))*(2.48*D(ID(IRCD+9))+13.6)
      IF (WTF.NE.0.0) WTC = WTC*WTF
      WTDC = 0.41*WTC
      GO TO 50
    4 WTC = 6.0
      VOL = 250.0
      CUC = 49.5
      RIC = 0.24448
      IF (WTF.NE.0.0) WTC = WTC*WTF
      WTDC = 0.24*WTC
      GO TO 50
    5 NL = ID(IRCD+7)
      NS = ID(IRCD+8)
      WTC = 0.0967*D(IW+NL)+51.45
      VOL = 8.065*D(IW+NL)+5671.0
      IF (WTF.NE.0.0) WTC = WTC*WTF
      CUC = WTC*(6.45+0.0176*D(IW+NL))
      RIC = 0.24448
      WTDC = 0.24*WTC
      IF (NS.EQ.0) GO TO 50
      WL = 31.82*D(ID(IRCD+9))*D(IP+NS)/SQRT(D(IT+NS))
      WJ = D(IW+NL)-WL
      GMA = GAM(NL,D(IP+NS),D(IT+NS))
      PR = D(IP+NS)/D(IGA+5)
      GM = (GMA-1.0)/GMA
      THRUST = 6.587E-4*WJ*SQRT((PR**GM-1.0)/(GMA-1.0))*SOS(NL,D(IGA+5)
     *,D(IT+NS)*(1.0/PR)**GM)
      SDR = SDR-THRUST
   50 WTIC = 0.205*WTC
      DRC  = 1.0
      DRF  = D(ID(IRCD+5))
      IF (DRF.NE.0.0) DRC = DRC*DRF
      IF (DRC.GT.10.0) DRC = 10.0
      CUF = D(ID(IRCD+3))
      IF (CUF.NE.0.0) CUC = CUC*CUF
      RI = D(ID(IRCD+4))
      IF (RI.NE.0.0) RIC = RI
      CALL SSA
      CALL SCO
      CALL LINES(2)
      WRITE (OUT,1000) ISCT,VOL
 1000 FORMAT(1H0,5X,5HTYPE ,I5,3X,3HVOL,F7.0)
      IF (ISCT.EQ.5 .AND. NS.NE.0) WRITE (OUT,1001) THRUST
 1001 FORMAT(1H+,31X,4HTHRC,F6.1)
   99 CONTINUE
      RETURN
C     CNTRSP
      END
C
C**********************************************************************
C
C
C**********************************************************************
C
      SUBROUTINE CNTRSZ

      COMMON /CC/ C(600)
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))
     *, (SCR(1),C(151))
      DIMENSION ICV(18),SCR(30)
      INTEGER OUT,CERR
      EQUIVALENCE (SCR(1),NL)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))
      DATA IFTA/010/

C   1 COMP CODE
C   2 WTM
C   3 CUM
C   4 RI
C   5 DRM
C   6 CNTRL TYPE
C   7 KT,NL
C   8 KP,NS
C   9 WTS,DIA,CA
C  10 NSNSR
C  11 NIAO

      I = IACDB(11)
      ID(I+1) = ICV(1)
      ID(I+2) = IPARM(ICV(2))
      ID(I+3) = IPARM(ICV(3))
      ID(I+4) = IPARM(ICV(4))
      ID(I+5) = IPARM(ICV(5))
      ID(I+6) = ICV(6)
      IF (ICV(6).GT.0 .AND. ICV(6).LE.5) GO TO 10
      CERR = CERR+1
      CALL LINES(2)
      WRITE (OUT,1000) CERR
 1000 FORMAT(6H0ERROR,5X,20HINVALID CONTROL TYPE,I6)
      GO TO 99
   10 IF (ICV(6).EQ.5) GO TO 15
      ID(I+7) = IPARM(ICV(7))
      ID(I+8) = IPARM(ICV(8))
      ID(I+9) = IPARM(ICV(9))
      IF (ICV(6).EQ.4) GO TO 99
      ID(I+10) = ICV(10)
      ID(I+11) = ICV(11)
      GO TO 99
   15 NL = ILEGN(ICV(7))
      ID(I+7) = NL
      CALL FTL(NL,IFTA)
      IF (ICV(8).NE.0) ID(I+8) = ISTAN(ICV(8))
      ID(I+9) = IPARM(ICV(9))
   99 CONTINUE
      RETURN
C     CNTRSZ
      END
C
C**********************************************************************
C
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR
C      COMPRESSOR
C
C**********************************************************************
C
      SUBROUTINE COMPPP

      COMMON /CC/ C(600)

      COMMON /DT/ DTES(64001)
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))
     *, (IH,C(30)), (SCR(1),C(151)),(ISV,C(47)), (ISVT,C(46))
     *, (IGA,C(35)), (P0,C(64)), (T0,C(65))
     *, (PASS,C(17)), (ICPP,C(88)), (IFP,C(22)), (OUT,C(7))
     *, (IEVT,C(48)), (IEV,C(49))
      DIMENSION SCR(30)
      INTEGER PASS, OUT
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)
     *, (SCR(4),NPA), (SCR(5),IOP), (SCR(6),JSVI), (SCR(7),HP)
     *, (SCR(8),NST), (SCR(9),PR), (SCR(10),ETA), (SCR(11),GAMMA)
     *, (SCR(12),TAV), (SCR(13),HS), (SCR(14),WCOR), (SCR(15),WCALC)
     *, (SCR(16),JEVI), (SCR(17),IOT), (SCR(18),DTOT), (SCR(19),TRF)
     *, (SCR(20),FH)

      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(128001)
      EQUIVALENCE (ID(1),D(1))

C   1 COMP CODE
C   2 LEG NO
C   3 STATION IN
C   4 STATION OUT
C   5 SHAFT NO
C   6 OPTION
C   7 PRESSURE RATIO
C   8 PRESSURE RATIO TABLE NO
C   9 EFFICIENCY TABLE NO
C  10 MECHANICAL EFFICIENCY
C  11 STATE VARIABLE INDEX
C  12 ERROR VARIABLE INDEX

      IRCD = IRCDB(12)
      NL = ID(IRCD+2)
      NSI = ID(IRCD+3)
      NSO = ID(IRCD+4)
      NST = ID(IRCD+5)
      D(IGA+41) = D(IW+NL)
      NPA = NST + 90
      IOT = ID(IRCD+6)/10
      IOP = ID(IRCD+6)-IOT*10
      JSVI = ID(IRCD+11)
      JEVI = ID(IRCD+12)
      GAMMA = GAM(NL,D(IP+NSI),D(IT+NSI))
	D(IGA+28) = D(IW+NL) / DEN(NL,D(IP+NSI),D(IT+NSI))  !CFM ADDED 9/25/00 SFW
	D(IGA+112) = D(IGA+28)/D(IGA+NST+80)   !GA112=CFM/RPM ADDED 9/25/00 SFW	
      IF (PASS.NE.1 .OR. (IOP.NE.1 .AND. IOP.NE.3)) GO TO 1
      ID(ISVT+JSVI) = 8
      D(ISV+JSVI) = D(ID(IRCD+7))
    1 IF (IOP.EQ.2 .OR. IOP.EQ.4  .OR. IOP.EQ.5) GO TO 5
      IF (IOP.EQ.1 .OR. IOP.EQ.3) GO TO 3
      PR = D(ID(IRCD+7))
      GO TO 4
    3 PR = D(ISV+JSVI)
      IF (IOP.NE.3) GO TO 4
    5 D(IGA+25) = D(IGA+NST+80) / SQRT(D(IT+NSI) / T0 )
      D(IGA+22)  = D(IW+NL)* SQRT(D(IT+NSI)/T0) / (D(IP+NSI)/P0)
      IF (IOP.EQ.2) GO TO 8
      IF (IOP.EQ.4) GO TO 9
      IF (IOP.EQ.5) GO TO 10
      GO TO 4
    8 PR = TLUP(ID(IRCD+8))
      GO TO 4
    9 FH = TLUP(ID(IRCD+8))
      ETA = TLUP(ID(IRCD+9))
      PR = (FH*ETA/T0+1.0)**(GAMMA/(GAMMA-1.0))
      GO TO 4 	
C		CFM/RPM VS DELTAP TOTAL AND POWER COEFF MAP OPTION "IOP=5" ADDED 9/2000 SFW
   10 PRISECOEFF = TLUP(ID(IRCD+8))
      D(IGA+111) = PRISECOEFF
	PRISE=PRISECOEFF*(DEN(NL,D(IP+NSI),D(IT+NSI))/.0765)
     *	*(D(IGA+NST+80))**2/1.E8/27.61
	PR=(D(IP+NSI)+PRISE)/D(IP+NSI)	
	GO TO 4
    4 D(IP+NSO) = PR * D(IP+NSI)
      D(IGA+24) = PR
      D(IRCD-6) = PR
      TRF = PR**((GAMMA-1.0)/GAMMA)-1.0
      IF (IOT.NE.0) GO TO 6
      IF (IOP.NE.4) ETA = TLUP(ID(IRCD+9))
      IF (IOP.EQ.5) THEN
		D(IGA+113) = TLUP(ID(IRCD+9)) !GA113=POWER COEFF
		DELTAT = TLUP(ID(IRCD+9))*
     *	(DEN(NL,D(IP+NSI),D(IT+NSI))/.0765)*(D(IGA+NST+80))**3/1.E12
     *	*42.4/(D(IW+NL)*SHP(NL,D(IP+NSI),TAV,D(IH+NSI))) 
		ETA=(TRF*D(IT+NSI))/DELTAT
		ENDIF
c windmill     IF (ABS(ETA).LT.0.01) ETA = SIGN(0.01,ETA)
c windmill     IF (PR.GT.1.0 .AND. ETA.LT.0.0) ETA = 0.01
c windmill     IF (PR.LT.1.0 .AND. ETA.GT.0.0) ETA = -0.01
      D(IT+NSO) = D(IT+NSI)+D(IT+NSI)*TRF/ETA
      GO TO 7
    6 DTOT = TLUP(ID(IRCD+9))
      D(IT+NSO) = D(IT+NSI)+DTOT*D(IT+NSI)
      IF (DTOT.EQ.0.0) DTOT = 1.E-6
      ETA = TRF/DTOT
    7 D(IRCD-7) = ETA
      D(IH+NSO) = D(IH+NSI)
      TAV = 0.5*(D(IT+NSI)+D(IT+NSO))
      HP = -0.02356*D(IW+NL)* SHP(NL,D(IP+NSI),TAV,D(IH+NSI))
     * *(D(IT+NSO)-D(IT+NSI))/D(ID(IRCD+10))
      D(IRCD-8) = HP
      D(IGA+NPA) = D(IGA+NPA)+HP
      IF (D(IH+NSI).NE.0.0) CALL TDB(NL,NSI,NSO,HS)
      IF (IOP.NE.3) GO TO 2
      WCOR = TLUP(ID(IRCD+8))
      WCALC = WCOR * D(IP+NSI) / (P0 * SQRT( D(IT+NSI) / T0))
      IF (PASS.EQ.1) ID(IEVT+JEVI) = 1
      D(IEV+JEVI) = D(IW+NL)-WCALC
      IF (WCALC.NE.0) DTES(IEV+JEVI) = D(IW+NL)/WCALC
    2 IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99
      CALL PIOP(1,NL,NSI,NSO)
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HS) CALL HSOP(HS)
      CALL LINES(2)
      WRITE (OUT,1001) NST,PR,ETA,HP
 1001 FORMAT(1H0,5X,6HSHAFT ,I4,3X,2HPR,F8.4,3X,3HEFF,F7.4,3X,2HHP,F9.2)
C GA111=FAN TOTAL PRESS RISE COEFF ADDED 9/25/00 SFW	
C	D(IGA+111) = 27.61 * (D(IP+NSO)-D(IP+NSI))*1.E8/
C     *	(D(IGA+NST+80)**2 * (DEN(NL,D(IP+NSI),D(IT+NSI))/.0765))
   99 CONTINUE
      RETURN
c   comppp
      END
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A COMPRESSOR                                              
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE COMPPZ                                                         
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
     *, (CERR,C(16)), (OUT,C(7))                                                
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER CERR, OUT                                                         
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
     *, (SCR(4),IOP), (SCR(5),IOT), (SCR(6),ISIG), (SCR(7),IBP)                 
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
                                                                                
C  -9 COMPRESSOR/BYPASS VALVE SPLIT RATIO STORED                                
C  -8 COMPRESSOR HORSEPOWER STORED                                              
C  -7 COMPRESSOR EFFICIENCY STORED                                              
C  -6 COMPRESSOR PRESSURE RATIO STORED                                          
C  -5 COMPRESSOR FLOW STORED                                                    
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 SHAFT NO                                                                  
C   6 OPTION                                                                    
C   7 PRESSURE RATIO                                                            
C   8 PRESSURE RATIO TABLE NO                                                   
C   9 EFFICIENCY TABLE NO                                                       
C  10 MECHANICAL EFFICIENCY                                                     
C  11 STATE VARIABLE INDEX                                                      
C  12 ERROR VARIABLE INDEX                                                      
C  13 FLOW SCALE FACTOR, C1                                                     
C  14 FLOW SCALE FACTOR, C2                                                     
C  15 SPEED SCALE FACTOR, C3                                                    
C  16 PRESSURE RATIO SCALE FACTOR, C4                                           
C  17 EFFICIENCY SCALE FACTOR, C5                                               
C  18 COMPRESSOR BYPASS VALVE PRESSURE LOSS VALUE/TABLE REL NO (OPTIONAL        
C  19 COMPRESSOR MINIMUM FLOW FOR PR = 1.0 TABLE REL NO (OPTIONAL)              
                                                                                
      I = IACDB(19)                                                             
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
      CALL SRT(ICV(5))                                                          
      ID(I+6) = ICV(6)                                                          
      IBP = ICV(6)/1000                                                         
      ISIG = ICV(6)/100-IBP*10                                                  
      IOT = ICV(6)/10-10*(ICV(6)/100)                                           
      IOP = ICV(6)-10*(ICV(6)/10)                                               
      ID(I+7) = IPARM(ICV(7))                                                   
      IF (IOP.LT.0 .OR. IOP.GT.5) GO TO 1                                       
      IF (IOT.LT.0 .OR. IOT.GT.1) GO TO 1                                       
      IF (IOP.EQ.4 .AND. IOT.NE.0) GO TO 1                                      
      IF (ISIG.LT.0 .OR. ISIG.GT.2) GO TO 1                                     
      IF (IBP.LT.0 .OR. IBP.GT.2) GO TO 1                                       
      GO TO 2                                                                   
    1 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,IBP,ISIG,IOT,IOP                                    
 1000 FORMAT(6H0ERROR,I6,5X,14HINVALID OPTION 8H  IBP = ,I2,9H  ISIG =          
     * ,8H  IOT = ,I2,8H  IOP = ,I2)                                            
    2 CONTINUE                                                                  
      IF (IOP.EQ.2) ID(I+8) = ITIDN(ICV(8),4)                                   
      IF (IOP.EQ.5) ID(I+8) = ITIDN(ICV(8),4)                                   
      IF (IBP.NE.0 .AND. IOP.EQ.2) ID(I+19) = ITIDN(ICV(8),41)                  
      IF (IBP.NE.0 .AND. IOP.EQ.5) ID(I+19) = ITIDN(ICV(8),41)                  
      IF (IOP.EQ.3) ID(I+8) = ITIDN(ICV(8),21)                                  
      IF (IOP.EQ.4) ID(I+8) = ITIDN(ICV(8),25)                                  
      IF (IBP.NE.0 .AND. IOP.EQ.4) ID(I+19) = ITIDN(ICV(8),42)                  
      IF (IBP.EQ.1) ID(I+18) = IPARM(ICV(15))                                   
      IF (IBP.NE.2) GO TO 3                                                     
      IF (IOP.EQ.3) ID(I+18) = ITIDN(ICV(16),18)                                
      IF (IOP.NE.3) ID(I+18) = ITIDN(ICV(16),1)                                 
    3 CONTINUE                                                                  
      ID(I+13) = IPARM(ICV(10))                                                 
      ID(I+14) = IPARM(ICV(11))                                                 
      ID(I+15) = IPARM(ICV(12))                                                 
      ID(I+16) = IPARM(ICV(13))                                                 
      ID(I+17) = IPARM(ICV(14))                                                 
      IF (IOT.EQ.0) ID(I+9) = ITIDN(ICV(8),6)                                   
      IF (IOT.EQ.1) ID(I+9) = ITIDN(ICV(8),24)                                  
      ID(I+10) = IPARM(ICV(9))                                                  
      IF (IOP.NE.1 .AND. IOP.NE.3) GO TO 99                                     
      IF (IBP.GE.1) ID(I+11) = IASV(10)                                         
      IF (IBP.EQ.0) ID(I+11) = IASV(8)                                          
      IF (IOP.EQ.3) ID(I+12) = IAEV(1)                                          
   99 CONTINUE                                                                  
      RETURN                                                                    
C     COMPPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C     A COMPRESSOR                                                              
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE COMPSP                                                         
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151)), (IW,C(27))         
     *, (IP,C(28)), (IT,C(29)), (WTC,C(102)), (CUC,C(104))                      
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))                
     *, (ILN,C(37)), (IFP,C(22)), (IGA,C(35)), (IDS,C(5))                       
     *, (PASS,C(17)), (NIT,C(76)), (PF,C(77)), (NWT,C(78)), (ITAD,C(54))        
     *, (ICONV,C(79)), (CERR,C(16)), (PUN,C(91)), (ICV(1),C(133))               
     *, (LN,C(125)), (LUC,C(126)), (LUT,C(127)), (HPE,C(112))                   
     *, (HPS,C(113)), (HPH,C(114)), (IH,C(30))                                  
      DIMENSION SCR(30), ICV(18)                                                
      EQUIVALENCE         (ICV( 1),NL    ), (ICV( 2),NSI   )                    
     *, (ICV( 3),NSO   ), (ICV( 4),IE    ), (ICV( 5),II    )                    
     *, (ICV( 6),MAP   ), (ICV( 7),IS    ), (ICV( 8),JS    )                    
     *, (ICV( 9),EVP   ), (ICV(10),EVS   ), (ICV(11),NSS   )                    
     *, (ICV(12),IDSS  ), (ICV(13),PR    ), (ICV(14),H     )                    
     *, (ICV(15),E     ), (ICV(16),Q     ), (ICV(17),DENS  )                    
     *, (ICV(18),NST   )                                                        
      EQUIVALENCE         (SCR( 1),DIA   ), (SCR( 2),N     )                    
     *, (SCR( 3),U     ), (SCR( 4),VOL   ), (SCR( 5),NS    )                    
     *, (SCR( 6),DS    ), (SCR( 7),VLD   ), (SCR( 8),HP    )                    
     *, (SCR( 9),EVPP  ), (SCR(10),IDT   ), (SCR(11),DPR   )                    
     *, (SCR(12),DPRU  ), (SCR(13),DPRL  ), (SCR(14),EF    )                    
     *, (SCR(15),TT    ), (SCR(16),UU    ), (SCR(17),NN    )                    
     *, (SCR(18),NSIG  ), (SCR(19),NM    ), (SCR(20),WTF   )                    
     *, (SCR(21),CUF   ), (SCR(22),RI    ), (SCR(23),DRF   )                    
     *, (SCR(24),GMA   ), (SCR(25),GM    ), (SCR(26),CP    )                    
     *, (SCR(27),TOUT  ), (SCR(28),EV    ), (SCR(29),HPC   )                    
     *, (SCR(30),JJ    )                                                        
      INTEGER OUT,PASS,CERR,PUN                                                 
      REAL N,NS,NSS,NSUL,NSLL,NN,NM,NSIG,LUC,LUT,LN                             
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DIMENSION TABE(4), TABN(4)                                                
      DATA ERL/0.001/, NSUL/10000.0/, NSLL/1.0/                                 
      DATA TABE/0.98,1.0,1.02,1.04/, TABN/5500.0,7500.0,11500.0,22000.0/        
                                                                                
C   1 COMP CODE                                                                 
C   2 NL                                                                        
C   3 NSI                                                                       
C   4 NSO                                                                       
C   5 NST                                                                       
C   6 WTF                                                                       
C   7 CUF                                                                       
C   8 RI                                                                        
C   9 DRF                                                                       
C  10 DIA                                                                       
C  11 NS-DS TABLE                                                               
C  12 ICT,ITT                                                                   
C  13 MAP                                                                       
C  14 IDT,Z                                                                     
                                                                                
      IE = 1                                                                    
      GO TO 1                                                                   
      ENTRY TURBSP                                                              
      IE = 2                                                                    
    1 IRCD = IRCDB(14)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      PR = D(IP+NSO)/D(IP+NSI)                                                  
      TOUT = TDBI(NL,NSI,NSO)                                                   
      GMA = GAM(NL,D(IP+NSI),D(IT+NSI))                                         
      GM = (GMA-1.0)/GMA                                                        
      CP = SHP(NL,D(IP+NSI),0.5*(D(IT+NSI)+TOUT),D(IH+NSI))                     
      IF (IE.EQ.2) GO TO 2                                                      
      H = 778.0*CP*D(IT+NSI)*(PR**GM-1.0)                                       
      E = D(IT+NSI)*(PR**GM-1.0)/(TOUT-D(IT+NSI))                               
      DENS = DEN(NL,D(IP+NSI),D(IT+NSI))                                        
      Q = D(IW+NL)/(60.0*DENS)                                                  
      IF (ID(IRCD+12).EQ.1) NSIG = 100.0                                        
      IF (ID(IRCD+12).EQ.2) NSIG = 300.0                                        
      GO TO 3                                                                   
    2 H = 778.0*CP*D(IT+NSI)*(1.0-PR**GM)                                       
      E = (D(IT+NSI)-TOUT)/(D(IT+NSI)*(1.0-PR**GM))                             
      DENS = DEN(NL,D(IP+NSO),D(IT+NSO))                                        
      Q = D(IW+NL)/(60.0*DENS)                                                  
      NSIG = 80.0                                                               
    3 NST = ID(IRCD+5)                                                          
      DIA = D(ID(IRCD+10))                                                      
      D(IGA+60) = E                                                             
      IF (IE.EQ.2) D(IGA+61) = 1.0/PR                                           
      IDSS = IDS                                                                
      ICONV = 0                                                                 
      JJ = 0                                                                    
      IF (D(ID(IRCD+10)).EQ.0.0) GO TO 50                                       
      IF (D(IGA+80+NST).EQ.0.0) GO TO 4                                         
      N = D(IGA+80+NST)                                                         
      U = 0.004363*DIA*N                                                        
      NS = N*SQRT(Q)/H**0.75                                                    
      DS = DIA*H**0.25/(12.0*SQRT(Q))                                           
      GO TO 26                                                                  
    4 EVP = DIA*H**0.25/(12.0*SQRT(Q))                                          
   10 NS = NSIG                                                                 
      PASS = 1                                                                  
      IFP = 0                                                                   
      ASSIGN 7 TO IS                                                            
      GO TO 300                                                                 
    7 IF (IDS.EQ.0) GO TO 71                                                    
      ASSIGN 71 TO JS                                                           
      GO TO 400                                                                 
   71 CONTINUE                                                                  
      WT = 1.0                                                                  
      DO 15 II = 1,NIT                                                          
      IF (IDS.LT.0) IDS = 2                                                     
      IF (IDSS.LT.0 .AND. II.GT.IABS(IDSS)) IDS = 0                             
      IF (II.EQ.NIT) IDS = 2                                                    
      D(IGA+1) = FLOAT(PASS)                                                    
      PASS = PASS+1                                                             
      IF (ID(ITAD+NWT).NE.0) WT = TLUP(NWT)                                     
      NSS = NS                                                                  
      EVS = EV                                                                  
      NS = NSS*PF                                                               
      ASSIGN 8 TO IS                                                            
      GO TO 300                                                                 
    8 IF (IDS.LE.2) GO TO 81                                                    
      ASSIGN 81 TO JS                                                           
      GO TO 400                                                                 
   81 CONTINUE                                                                  
      IF ((EV-EVS).EQ.0.0) GO TO 16                                             
      NS = NSS-WT*EVS*(NS-NSS)/(EV-EVS)                                         
      IF (NS.GT.NSUL) NS = NSUL                                                 
      IF (NS.LT.NSLL) NS = NSLL                                                 
      ASSIGN 9 TO IS                                                            
      GO TO 300                                                                 
    9 IF (IDS.EQ.0) GO TO 91                                                    
      ASSIGN 91 TO JS                                                           
      GO TO 400                                                                 
   91 CONTINUE                                                                  
      ICONV = 0                                                                 
      IF (ABS(EV).GT.ERL) ICONV = 1                                             
      IF (ICONV.EQ.0) GO TO 150                                                 
   15 CONTINUE                                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1030) CERR                                                     
 1030 FORMAT(6H0ERRER,I6,5X,14HNON CONVERGENT)                                  
      GO TO 13                                                                  
  199 N = NS*H**0.75/SQRT(Q)                                                    
      IF (D(ID(IRCD+10)).NE.0.0) GO TO 11                                       
      DIA = U/(0.004363*N)                                                      
      GO TO 200                                                                 
   11 IF (N.GT.LN) JJ = 1                                                       
      U = 0.004363*DIA*N                                                        
      GO TO 26                                                                  
   16 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1009) CERR                                                     
 1009 FORMAT(6H0ERROR,I6,5X,15HSINGULAR MATRIX)                                 
   13 WTC = 0.0                                                                 
      CUC = 0.0                                                                 
      RIC = 0.0                                                                 
      DRC = 0.0                                                                 
      GO TO 201                                                                 
  400 CALL LINES(4)                                                             
      WRITE (OUT,1020) PASS                                                     
 1020 FORMAT(6H0PASS ,I6)                                                       
      WRITE (OUT,1021) NS,EV                                                    
 1021 FORMAT(6H0S.V. ,E12.5,5X,5HE.V.8,E12.5)                                   
C1021 FORMAT(6H0S.V. ,E12.5,5X,5HE.V. ,E12.5)                                   
      GO TO JS, (71,81,91)                                                      
  300 D(IGA+59) = NS                                                            
      DS = TLUP(ID(IRCD+11))                                                    
      IF (D(ID(IRCD+10)).NE.0.0) GO TO 30                                       
      IF (IE.EQ.1) GO TO 31                                                     
      D(IGA+60) = E+5.0E-9*NS**3*DS**5*(1.0/PR**(1.0/GMA)                       
     * -6.0**(1.0/GMA))                                                         
      DS = TLUP(ID(IRCD+11))                                                    
   31 EV = NS*DS*EVPP-U                                                         
      GO TO IS, (7,8,9,199)                                                     
   30 EV = DS-EVP                                                               
  150 ASSIGN 199 TO IS                                                          
      IFP = 1                                                                   
      GO TO 300                                                                 
   50 N = LN                                                                    
      IF (D(IGA+80+NST).NE.0.0) N = D(IGA+80+NST)                               
      NS = N*SQRT(Q)/H**0.75                                                    
      D(IGA+59) = NS                                                            
      DS = TLUP(ID(IRCD+11))                                                    
      IF (IE.EQ.1) GO TO 25                                                     
      D(IGA+60) = E+5.0E-9*NS**3*DS**5*(1.0/PR**(1.0/GMA)                       
     * -6.0**(1.0/GMA))                                                         
      DS = TLUP(ID(IRCD+11))                                                    
   25 DIA = 12.0*DS*SQRT(Q)/H**0.25                                             
      U = 0.004363*DIA*N                                                        
   26 IF (IE.EQ.1 .AND. U.LE.LUC) GO TO 200                                     
      IF (IE.EQ.2 .AND. U.LE.LUT) GO TO 200                                     
      JJ = 2                                                                    
      IF (D(IGA+80+NST).NE.0.0 .OR. D(ID(IRCD+10)).NE.0.0) GO TO 200            
      EVPP = SQRT(H)/19.0986                                                    
      U = LUC                                                                   
      IF (IE.EQ.2) U = LUT                                                      
      GO TO 10                                                                  
  200 IF (D(IGA+80+NST).EQ.0.0) D(IGA+80+NST) = N                               
      WTC = 0.4*DIA**2                                                          
      WTF = D(ID(IRCD+6))                                                       
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      RIC = 0.0                                                                 
      IF (IE.EQ.1) RIC = 0.06793                                                
      RI = D(ID(IRCD+8))                                                        
      IF (RI.NE.0.0) RIC = RI                                                   
      CUC = 5.5*WTC                                                             
      CUF = D(ID(IRCD+7))                                                       
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      IF (IE.EQ.1) GO TO 40                                                     
      DRC = 1.0                                                                 
      GO TO 41                                                                  
   40 TT = D(IT+NSO)                                                            
      UU = U                                                                    
      NN = N                                                                    
      IF (TT.LT.1460.0) TT = 1460.0                                             
      IF (UU.LT.1500.0) UU = 1500.0                                             
      IF (NN.LT.60000.0) NN = N                                                 
      DRC = (1.0+((TT-1460.0)/1000.0)**2)*(1.0+((UU-1500.0)/707.0)**2)          
     * *(1.0+((NN-60000.0)/42500.0)**2)                                         
   41 DRF = D(ID(IRCD+9))                                                       
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      VOL =.6666667*DIA**4                                                      
      WTDC = 0.164*WTC                                                          
      WTIC = 0.205*WTC                                                          
      HP = 0.02356*D(IW+NL)*CP*(D(IT+NSI)-TOUT)                                 
      CALL SSA                                                                  
  201 CALL SCO                                                                  
      IDS = IDSS                                                                
      CALL SCI(NL,NSI,NSO)                                                      
      IF (ICONV.NE.0) GO TO 99                                                  
      CALL LINES(2)                                                             
      WRITE (OUT,1022) DIA,VOL,N,E,HP,U,NS,DS                                   
 1022 FORMAT(1H0,5X,1HD,F9.2,3X,3HVOL,F7.0,3X,1HN,F9.0,3X,3HEFF,F7.4,3X,        
     *2HHP,F9.2,2X,1HU,F9.0,3X,2HNS,F8.2,3X,2HDS,F8.2)                          
      D(IGA+71) = DIA                                                           
      D(IGA+72) = VOL                                                           
      IF (JJ.EQ.0) GO TO 203                                                    
      IF (JJ.EQ.2) GO TO 206                                                    
      CALL LINES(2)                                                             
      WRITE (OUT,1010)                                                          
 1010 FORMAT(1H0,5X,34HSHAFT SPEED EXCEEDS LIMITING VALUE)                      
      GO TO 203                                                                 
  206 CALL LINES(2)                                                             
      WRITE (OUT,1011)                                                          
 1011 FORMAT(1H0,5X,32HTIP SPEED EXCEEDS LIMITING VALUE)                        
  203 IF (IE.EQ.2) GO TO 99                                                     
      IDT = ID(IRCD+14)                                                         
      CALL LINES(2)                                                             
      WRITE (OUT,1035) IDT                                                      
 1035 FORMAT(1H0,5X,6HDRIVE ,I4)                                                
      IF (IDT.EQ.0) GO TO 99      ! NO DRIVE TYPE SAME AS USING TURBINE DRIVE                                              
      HPC = -HP                                                                 
      GO TO (99,42,43,44,45), IDT                                               
   42 IF (N.GT.22000.0) GO TO 52                                                
      EF = 1.0-0.281/HPC**0.169                                                 
      IF (N.GE.11000.0 .AND. N.LE.22000.0) GO TO 53                             
      IF (N.GE.7200.0 .AND. N.LT.11000.0) GO TO 54                              
      NM = 5500.0                                                               
      EF = 0.98*EF                                                              
      GO TO 55                                                                  
   53 NM = 11500.0                                                              
      EF = 1.02*EF                                                              
      GO TO 55                                                                  
   54 NM = 7500.0                                                               
   55 WTC = 2.0+2.3E05*HPC**0.8333/NM**1.25                                     
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      WTDC = 0.111*WTC                                                          
      CUC = 4.0+0.6*WTC                                                         
      VLD = 10.0*WTC                                                            
      HPC = HPC/EF                                                              
      HPE = HPE+HPC                                                             
      GO TO 250                                                                 
   52 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1050) CERR                                                     
 1050 FORMAT(6H0ERROR,I6,5X,36HSHAFT SPEED TOO HIGH FOR MOTOR DRIVE)            
      GO TO 99                                                                  
   43 IF (N.GT.22000.0) GO TO 52                                                
      WTC = 1.5+3.83E05*HPC**0.8333/N**1.25                                     
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      WTDC = 0.259*WTC                                                          
      CUC = 3.0+WTC                                                             
      VLD = 10.0*WTC                                                            
c@      CALL MDSCT(2,IND,EF,TABE,N,TABN,4,1,DV,DV,DV)                             
      CALL MDSCT(2,IND,EF,TABE,N,TABN,4,1,DV,DV,IDV,Idummy)                             
      EF = (1.0-0.281/HPC**0.169)*EF                                            
      HPC = HPC/EF                                                              
      HPE = HPE+HPC                                                             
      GO TO 250                                                                 
   44 IF (N.LE.20000.0) GO TO 57                                                
      WTC = 2.0113*SQRT(HPC)                                                    
      GO TO 56                                                                  
   57 DPRU = EXP(2.706-3.453E-4*N)                                              
      DPRL = EXP(-1.519-3.784E-4*N)                                             
      DPR = 139.094*HPC/N                                                       
      WTC = 11.37*SQRT(DPR)                                                     
      IF (DPR.GT.DPRU) WTC = WTC*2.121                                          
      IF (DPR.LT.DPRL) WTC = WTC*1.0605                                         
   56 IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      WTDC = 0.2*WTC                                                            
      CUC = 17.0+4.57*WTC                                                       
      VLD = 20.0*WTC                                                            
      EF = 0.82                                                                 
      HPC = HPC/EF                                                              
      HPH = HPH+HPC                                                             
      GO TO 250                                                                 
   45 CUC = 0.0                                                                 
      WTDC = 0.2*WTC                                                            
      EF = 0.97                                                                 
      HPC = HPC/EF                                                              
      HPS = HPS+HPC                                                             
  250 WTIC = 0.205*WTC                                                          
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      RIC = 0.0                                                                 
      DRC = 1.0                                                                 
      CALL SSA                                                                  
      WRITE (OUT,1036) EF,HPC, WTC,CUC                                                    
 1036 FORMAT(1H+,18X,3HEFF,F7.4,3X,2HHP,F9.2, 4X,2HWT,F8.2,3X,2HCU,F8.2)                                   
C     WRITE (OUT,1037) WTC,CUC                                                  
C1037 FORMAT(1H+,44X,2HWT,F8.2,3X,2HCU,F8.2)                                    
      IF (IDT.NE.5) WRITE (OUT,1038) VLD                                        
 1038 FORMAT(1H+,10X,3HVOL,F7.0)                                                
      IF(.NOT.(IDT.EQ.4.AND.N.GT.20000.0)) GO TO 99                             
      CALL LINES(2)                                                             
      WRITE (OUT,1051)                                                          
 1051 FORMAT(1H0,5X,22HHYDRAULIC MOTOR GEARED)                                  
   99 CONTINUE                                                                  
      RETURN                                                                    
C     COMPSP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE COMPSZ                                                         
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
     *, (SCR(1),C(151)), (IGA,C(35))                                            
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER OUT, CERR                                                         
      EQUIVALENCE (SCR(1),NL)                                                   
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
                                                                                
C   1 COMP CODE                                                                 
C   2 NL                                                                        
C   3 NSI                                                                       
C   4 NSO                                                                       
C   5 NST                                                                       
C   6 WTF                                                                       
C   7 CUF                                                                       
C   8 RI                                                                        
C   9 DRF                                                                       
C  10 DIA                                                                       
C  11 NS-DS TABLE                                                               
C  12 ICT,ITT                                                                   
C  13 MAP                                                                       
C  14 IDT,Z                                                                     
                                                                                
      IE = 1                                                                    
      GO TO 1                                                                   
      ENTRY TURBSZ                                                              
      IE = 2                                                                    
    1 I = IACDB(14)                                                             
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
      IF (IE.EQ.2) GO TO 2                                                      
      ID(I+11) = ITIDN(ICV(11),36)                                              
      IF (ICV(12).GT.0. .AND. ICV(12).LE.2) GO TO 3                             
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,ICV(12)                                             
 1000 FORMAT(6H0ERROR,I6,5X,23HINVALID COMPRESSOR TYPE,I6)                      
    3 ID(I+12) = ICV(12)                                                        
      IF (ICV(14).GE.0 .AND. ICV(14).LE.5) GO TO 7                              
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1002) CERR,ICV(14)                                             
 1002 FORMAT(6H0ERROR,I6,5X,18HINVALID DRIVE TYPE,I6)                           
    7 ID(I+14) = ICV(14)                                                        
      GO TO 99                                                                  
    2 ID(I+11) = ITIDN(ICV(11),37)                                              
      IF (ICV(12).GT.0 .AND. ICV(12).LE.2) GO TO 8                              
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1003) CERR,ICV(12)                                             
 1003 FORMAT(6H0ERROR,I6,5X,20HINVALID TURBINE TYPE,I6)                         
    8 ID(I+12) = ICV(12)                                                        
      ID(I+13) = ICV(13)                                                        
      IF (ICV(13).NE.0) ID(I+14) = ITIDN(ICV(14),38)                            
   99 CONTINUE                                                                  
      RETURN                                                                    
C     COMPSZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE CON1SZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
     *, (IAUR,C(19)), (SCR(1),C(151)), (MDPC,C(34))                             
     *, (IT,C(29))                                                              
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER OUT,CERR                                                          
      EQUIVALENCE (SCR(1),IPRNT), (SCR(2),NL), (SCR(3),CC), (SCR(4),NC)         
      EQUIVALENCE (SCR(30),I1), (SCR(29),I2)                                    
      DIMENSION CD(200)                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84))                                                   
      COMMON /CS/ SD(100)                                                       
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      REAL*8  cd, cn, sd            
      DATA IFTA/011/                                                            
C   1 COMP CODE                                                                 
C   2 NLS                                                                       
C   3 NSIS                                                                      
C   4 NSOS                                                                      
C   5 NSOR                                                                      
C   6 WTF                                                                       
C   7 CUF                                                                       
C   8 RI                                                                        
C   9 DRF                                                                       
C  10 LS                                                                        
C  11 LR                                                                        
C  12 LN                                                                        
C  13 NPS                                                                       
C  14 IMS                                                                       
C  15 IMR                                                                       
C  16 IFGS                                                                      
C  17 IFS                                                                       
C  18 IJS                                                                       
C  19 IFGR                                                                      
C  20 IMSP                                                                      
C  21 ITSP                                                                      
C  22 MAP                                                                       
      IE = 1                                                                    
      GO TO 1                                                                   
      ENTRY EVA1SZ                                                              
      IE = 2                                                                    
    1 I = IACDB(22)                                                             
      IPRNT = 10*(MDPC/10)                                                      
      IPRNT = IPRNT-100*(MDPC/100)                                              
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      ID(I+3) = ISTAN(ICV(3))                                                   
      ID(I+4) = ISTAN(ICV(4))                                                   
      ID(I+5) = ISTAN(ICV(5))                                                   
      CALL FTL(NL,IFTA)                                                         
      ID(I+6) = IPARM(ICV(6))                                                   
      ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      ID(I+22) = ICV(10)                                                        
      IF (ID(I+5).EQ.0 .OR. ID(I+3).EQ.0) GO TO 8                               
      I1 = ID(I+5)                                                              
      I2 = ID(I+3)                                                              
      IF (IE.EQ.2) GO TO 9                                                      
      IF (D(IT+I1).GT.D(IT+I2)) GO TO 8                                         
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1011) CERR                                                     
 1011 FORMAT(6H0ERROR,I6,5X,25HSINK TEMPERATURE TOO HIGH)                       
      GO TO 8                                                                   
    9 IF (D(IT+I1).LT.D(IT+I2)) GO TO 8                                         
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1012) CERR                                                     
 1012 FORMAT(6H0ERROR,I6,5X,26HSOURCE TEMPERATURE TOO LOW)                      
    8 CONTINUE                                                                  
      READ (IAUR,1000) CN,CC,NC,(ICV(II),II = 2,18)                             
 1000 FORMAT(A6,A2,18I4)                                                        
      I1 = ICV(1)+1                                                             
      IF (CN.EQ.SD(I1)) GO TO 5                                                 
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      IF (IE.EQ.2) GO TO 3                                                      
      WRITE (OUT,1002) CERR                                                     
 1002 FORMAT(6H0ERROR,I6,5X,13HNO COND2 CARD)                                   
      GO TO 4                                                                   
    3 WRITE (OUT,1001) CERR                                                     
 1001 FORMAT(6H0ERROR,I6,5X,13HNO EVAP2 CARD)                                   
    4 BACKSPACE IAUR                                                            
      GO TO 99                                                                  
    5 IF (IPRNT.NE.0) GO TO 51                                                  
      CALL LINES(1)                                                             
      WRITE (OUT,1004) CN,CC,NC,(ICV(II),II = 2,18)                             
 1004 FORMAT(5X,A6,1X,A2,18(1X,I4))                                             
   51 ID(I+10) = IPARM(ICV(2))                                                  
      ID(I+11) = IPARM(ICV(3))                                                  
      ID(I+12) = IPARM(ICV(4))                                                  
      ID(I+13) = ICV(5)                                                         
      ID(I+14) = ITIDN(ICV(6),20)                                               
      ID(I+15) = ITIDN(ICV(7),20)                                               
      ID(I+16) = ITIDN(ICV(8),31)                                               
      ID(I+17) = ITIDN(ICV(8),32)                                               
      ID(I+18) = ITIDN(ICV(8),33)                                               
      ID(I+19) = ITIDN(ICV(9),31)                                               
      ID(I+20) = ITIDN(ICV(10),20)                                              
      ID(I+21) = ITIDN(ICV(11),34)                                              
      IF (ICV(2).EQ.0 .AND. ICV(3).EQ.0 .AND. ICV(4).EQ.0) GO TO 99             
      IF (ICV(2).NE.0 .AND. ICV(3).NE.0 .AND. ICV(4).NE.0) GO TO 99             
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1010) CERR                                                     
 1010 FORMAT(6H0ERROR,I6,5X,20HMIXED SIZE SPECIFIED)                            
      GO TO 99                                                                  
      ENTRY CON2SZ                                                              
      IE = 3                                                                    
      GO TO 7                                                                   
      ENTRY EVA2SZ                                                              
      IE = 4                                                                    
    7 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      I1 = ICV(1)                                                               
      I2 = ICV(1)-1                                                             
      WRITE (OUT,1006) CERR,SD(I1),SD(I2)                                       
 1006 FORMAT(6H0ERROR,I6,5X,A6,21HCARD NOT PRECEDED BY ,A6,4HCARD)              
      I = IACDB(22)                                                             
      GO TO 51                                                                  
   99 CONTINUE                                                                  
      RETURN                                                                    
C     CON1SZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      CONDENSER                                                                
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE CONDPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)),(IW,C(27)),(IP,C(28)),(IT,C(29))                 
     *,(IH,C(30)),(SCR(1),C(151)),(IFB,C(55)),(IGA,C(35))                       
     *,(IEVT,C(48)),(IEV,C(49)),(IFP,C(22)),(ICPP,C(88))                        
     *,(PASS,C(17)),(OUT,C(7)),(GC,C(370))                                      
      INTEGER PASS,OUT                                                          
      EQUIVALENCE (SCR(1),NLR),(SCR(2),NSIR),(SCR(3),NSOR)                      
     *,(SCR(4),NLS),(SCR(5),NSIS),(SCR(6),NSOS),(SCR(7),JEVI)                   
      DIMENSION SCR(30),ERR(3),EL(3)                                            
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      EQUIVALENCE (D(51),H),(D(52),QUAL),(D(53),VOUT),(D(54),PB),               
     *(D(55),PDGO),(D(56),PDR),(D(57),VRATIO),(D(58),PDLO),                     
     *(D(59),HFG),(D(60),PHI),(D(61),TC),(D(62),V),(D(63),HSV),                 
     *(D(64),VMOMIN),(D(65),HSL),(D(66),EFF),                                   
     *(D(68),Q),(D(69),TOUT),(D(70),TAVG),(D(71),PD)                            
C   1 COMP CODE                                                                 
C   2 LEG NO R                                                                  
C   3 INLET STATION NO R                                                        
C   4 OUTLET STATION NO R                                                       
C   5 LEG NO S                                                                  
C   6 INLET STATION NO S                                                        
C   7 OUTLET STATION NO S                                                       
C   8 PRESS DROP OPTION                                                         
C   9 PRESS DROP TABLE NO                                                       
C  10 EFFECTIVENESS TABLE NO                                                    
C***TWO NEW PARAMETERS FOR THE COMPONENT CARD                                   
C  11 REFRIG-SIDE PRESS DROP TABLE(FT OF VAPOR VS CFM, VAPOR FLOW ONLY)         
C  12 FLOW AREA ON REFRIGERANT SIDE, SQ FT                                      
C***SET UP THE INTERNAL VARIABLES IN TERMS OF THE STORED ARRAY                  
      IRCD = IRCDB(13)                                                          
      NLR = ID(IRCD+2)                                                          
      NSIR = ID(IRCD+3)                                                         
      NSOR = ID(IRCD+4)                                                         
      NLS = ID(IRCD+5)                                                          
      NSIS = ID(IRCD+6)                                                         
      NSOS =ID(IRCD+7)                                                          
      D(IH+NSOS) = D(IH+NSIS)                                                   
      LOCT = ID(IFB+NLR)                                                        
      LOCS = ID(IFB+NLS)                                                        
      LOCS = ID(LOCS+1)                                                         
      LOCT = ID(LOCT+3)                                                         
C***SET UP THE THREE TLUP VARIABLES                                             
      D(IGA+41) = D(IW+NLS)                                                     
      D(IGA+42) = D(IW+NLR)                                                     
      D(IGA+43) = D(IP+NSIS)                                                    
C***COMPUTE THE SATURATION CONDITIONS                                           
C     WRITE(OUT,*) D(IT+NSIR),D(IP+NSIR),D(IH+NSIR)                             
      TC = VTS(LOCT,D(IP+NSIR))                                                 
      V = VSV('CONDPP  ',LOCT,D(IP+NSIR),TC)                                    
      D(IGA+42)=D(IGA+42)*V                                                     
      HSV = VH(LOCT,D(IP+NSIR),TC,V)                                            
      HFG = VHFG(LOCT,D(IP+NSIR),TC,V)                                          
      HSL = HSV - HFG                                                           
C     WRITE(OUT,*) HFG,D(IT+NSIS),TC,HSV,HSL                                    
      IF(TC.LT.D(IT+NSIS)) GO TO 25                                             
C***DETERMINE SINK-SIDE EFFECTIVENESS FROM TLUP WITH EFF A                      
C   FUNCTION OF SINK-SIDE FLOW RATE ONLY                                        
      EFF = TLUP(ID(IRCD+10))                                                   
C***COMPUTE SINK-SIDE OUTLET TEMPERATURE FROM EFFECTIVENESS                     
      D(IT+NSOS) = D(IT+NSIS) + EFF * (TC - D(IT+NSIS) )                        
C***COMPUTE THE REFRIG OUTLET ENTHALPY BY ENERGY BALANCE                        
      CPSNK = D(IW+NLS)*SHP(NLS,D(IP+NSIS),D(IT+NSIS),D(IH+NSIS))               
      Q = CPSNK*EFF*(TC-D(IT+NSIS))                                             
      H = D(IH+NSIR)-Q/D(IW+NLR)                                                
      D(IH+NSOR)=H                                                              
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 4098                                              
      XCP = SHP(NLS,D(IP+NSIS),D(IT+NSIS),D(IH+NSIS))                           
      WRITE (OUT,4999)                                                          
 4999 FORMAT('  *** WRITES FROM CONDPP  EFFECTIVENESS CALCS *********')         
      WRITE (OUT,4000) XCP   , CPSNK , TC    , H                                
 4000 FORMAT('  XCP   =', E11.4, '  CPSNK =', E11.4, '  TC    =', E11.4         
     *      ,'  H     =', E11.4)                                                
 4098 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C     WRITE(OUT,*) EFF,V,CPSNK,Q,H,D(IP+NSIR),D(IT+NSIR)                        
C***DETERMINE IF IN SINGLE-PHASE REGION AT THE OUTLET CONDITIONS                
      IF (H.LE.HSL) GO TO 20                                                    
      IF (H.GE.HSV) GO TO 30                                                    
C***IN TWO-PHASE REGION                                                         
      QUAL = (H-HSL)/HFG                                                        
      TOUT=TC                                                                   
      VOUT = QUAL * V + (1.-QUAL)/VDL(LOCT,TOUT)                                
      GO TO 40                                                                  
C***(EXIT IS) IN SINGLE-PHASE REGION, DETERMINE WHERE TWO PHASE ENDED           
   20 TOUT=VTLIQ(LOCT,D(IP+NSIR),H)                                             
      QUAL=0.0                                                                  
      CPREF=(HSL-H)/(TC-TOUT)                                                   
      CAPR=D(IW+NLR)*CPREF/CPSNK                                                
C***SET UP FOR STRADDLE SOLUTION TO LOCATION OF POSTION (EL) WHERE              
C   THE TWO-PHASE FLOW ENDED                                                    
      EMAX=D(IW+NLR)*(D(IH+NSIR)-HSL)/(CPSNK*(TC-D(IT+NSIS)))                   
      EL(1)=EMAX                                                                
      EL(2)=EMAX/(EMAX+CAPR)                                                    
      IF (EL(2).EQ.EL(1)) EL(2)=1.1*EL(1)                                       
      I=1                                                                       
C***TAKE UP TO 30 ITERATIONS TO FIND EL, COMPUTING THE EFFECTIVENESS            
C   IN THE SINGLE-PHASE(EFF1) AND TWO-PHASE (EFF2) REGIONS                      
      DO 570 ITER=1,30                                                          
      EFF1=1.-(1.-EFF)**((1.-EL(I))/(2.*CAPR))                                  
      EFF2=1.-(1.-EFF)**EL(I)                                                   
      ERR(I)=EMAX-EFF2*(1.-CAPR*EFF1)                                           
      IF(ITER.GT.2) GO TO 559                                                   
      I=I+1                                                                     
      IF(ITER.LT.2) GO TO 570                                                   
      GO TO 563                                                                 
C***IF THE EFFECTIVENESS IS BALANCES WITHIN 0.001,END ITERATION                 
  559 IF(ABS(ERR(3)).LT.0.00001) GO TO 571                                      
      IF(ERR(3).GT.0.0) GO TO 561                                               
      ERR(2)=ERR(3)                                                             
      EL(2)=EL(3)                                                               
      GO TO 563                                                                 
  561 EL(1)=EL(3)                                                               
      ERR(1)=ERR(3)                                                             
  563 EL(3)=EL(1)-ERR(1)*(EL(2)-EL(1))/(ERR(2)-ERR(1))                          
  570 CONTINUE                                                                  
C***PASSED ON THE BALANCED EFFECTIVENESS, COMPUTE THE OUTLET CONDITIONS         
  571 EFF=EMAX+CAPR*EFF1                                                        
      Q=CPSNK*EFF*(TC-D(IT+NSIS))                                               
      H=D(IH+NSIR)-Q/D(IW+NLR)                                                  
      TOUT=VTLIQ(LOCT,D(IP+NSIR),H)                                             
C***IF TOUT.LT.TSINK, WRITE A WARNING                                           
      IF(TOUT.LT.D(IT+NSIS)) GO TO 27                                           
      D(IH+NSOR)=H                                                              
      IF(ITER.GE.30) WRITE(OUT,640) D(IT+NSIS),TOUT,TC,EL(1),                   
     * EL(2),EL(3)                                                              
  640 FORMAT(' *+* WARNING FROM CONDPP: ITERATION FAILED'/                      
     *'    SINK TIN =',F8.2,'    REFRIG TOUT=',                                 
     *F8.2,'    CONDENSING T=',F8.2/' RATIO OF TWO-PHASE',                      
     *' TO TOTAL LENGTH >=' ,E12.5,                                             
     *' AND <=',E12.5, ', LAST ESTIMATE=',E12.5)                                
C***IN NUCLEATE REGION AT OUTLET                                                
   26 VOUT=1./VDL(LOCT,TOUT)                                                    
      GO TO 40                                                                  
  25  TOUT=D(IT+NSIS)                                                           
      H=HSV                                                                     
      Q=D(IW+NLR)*(D(IH+NSIR)-H)                                                
      D(IT+NSOS)=D(IT+NSIS)+Q/(D(IW+NLS)*SHP(NLS,D(IP+NSIS),D(IT+NSIS),         
     * D(ITH+NSIS)))                                                            
      QUAL=1.                                                                   
      WRITE(OUT,1103)TC,D(IT+NSIS)                                              
 1103 FORMAT(' *+* WARNING IN CONDPP REFRIGERANT COND. TEMP = ',F10.3/          
     * ' *** LESS THAN INLET SINK TEMPERATURE ',F10.3/                          
     * ' *** REFRIGERANT OUTLET TEMP RESET TO INLET SINK TEMP AND'/             
     * ' *** OUTLET SINK TEMPERATURE IS RECOMPUTED')                            
      GO TO 40                                                                  
   27 WRITE(OUT,1105)TOUT,ITER,D(IT+NSIS),TC                                    
      TOUT=D(IT+NSIS)                                                           
      H=VHLIQ(LOCT,D(IP+NSIR),TOUT)                                             
      D(IH+NSOR)=H                                                              
      Q=D(IW+NLR)*(D(IH+NSIR)-H)                                                
      D(IT+NSOS)=D(IT+NSIS)+Q/(D(IW+NLS)*SHP(NLS,D(IP+NSIS),                    
     *D(IT+NSIS),D(IH+NSIS)))                                                   
 1105 FORMAT(' *+* WARNING IN CONDPP REFRIGERANT OUTLET TEMP =',F10.3/          
     *' *+* EVEN AFTER',I8,' ITERATIONS, IS STILL'/                             
     *' *+* LESS THAN INLET SINK TEMPERATURE ',F10.3/                           
     *' *+* CONDENSING TEMPERATURE + ',F10.3/                                   
     *' *+* REFRIGERANT OUTLET TEMP RESET TO INLET SINK TEMP AND'/              
     *' *+* OUTLET SINK TEMPERATURE IS RECOMPUTED')                             
      VOUT=1./VDL(LOCT,TOUT)                                                    
      GO TO 40                                                                  
   30 QUAL = 1.                                                                 
C***IN SUPERHEAT REGION AT OUTLET                                               
      CALL VTAV2(LOCT,TOUT,VOUT,D(IP+NSIR),H)                                   
C***REFIG-SIDE PRESS DROP VS MASS FLOW RATE AS IF VAPOR ONLY                    
   40 D(IT+NSOR)=TOUT                                                           
      PDGO = TLUP(ID(IRCD+11))                                                  
      PDR=PDGO                                                                  
      IF (QUAL.GE.1.) GO TO 80                                                  
      VRATIO = V*VDL(LOCT,TC)                                                   
      SLIP = (VRATIO-1.)**0.25                                                  
C$    USE "V" CALCULATED EARLIER INSTEAD OF CALL TO VTAV2                       
C$    CALL VTAV2(LOCT,TIN,VIN,D(IP+NSIR),D(IH+NSIR))                            
C***REFRIG-SIDE PRESS DROP LIQUID ONLY USING SAME FRICTION FACTOR               
      PDLO=PDGO/VRATIO                                                          
C***MOMENTUM AT INLET BASED ON VAPOR-ONLY FLOW                                  
C$       CHANGE VIN TO V AND ADD DIVISOR OF 2                                   
C$           (AS IN  "QHEAD=ONE-HALF RHO VEE**2")                               
C$ WAS  VMOMIN=(D(IW+NLR)/D(ID(IRCD+12))/60.)**2*VIN/(GC*144.)                  
      VMOMIN=(D(IW+NLR)/D(ID(IRCD+12))/60.)**2*V/(2.*GC*144.)                   
C***TWO-PHASE PRESSURE DROP PER FAC CORRELATION OF BAROCZY                      
      PB=-0.25*ALOG10(VRATIO)                                                   
      IF (PB.LT.-1) PB=-1.                                                      
      PHI=-PB*(1.-QUAL**2.65)/2.65+(1.+PB)*((1-QUAL**1.7)/1.7+                  
     *    (1.-QUAL)**2.3*QUAL/2.3+(1.-QUAL)**3.3/2.3/3.3)                       
      PHI=PHI/(1.-QUAL)                                                         
      PDR=PDLO+PHI*(PDGO-PDLO)                                                  
C***PRESSURE RECOVERY DUE TO MOMENTUM DECREASE                                  
      VOIDF = QUAL/(QUAL+SLIP*(1.-QUAL)/VRATIO)                                 
      XMOM=((1.-QUAL)/(1.-VOIDF))**2*(1.-VOIDF*(1.-SLIP*SLIP/VRATIO))           
      DELTAM=(XMOM/VRATIO-1.)*VMOMIN                                            
      PDR=PDR+DELTAM                                                            
C***OUTLET PRESSURE                                                             
   80 D(IP+NSOR) = D(IP+NSIR) - PDR                                             
      IF(D(IP+NSOR).GT.0.0) GO TO 82                                            
       WRITE(OUT,7655)D(IP+NSOR),D(IP+NSIR)                                     
 7655 FORMAT(' *+* OUTLET PRESSURE IN COND WAS ',E14.7,                         
     * ' RESET PRESSURE TO INLET VALUE ',E14.7)                                 
      D(IP+NSOR)=D(IP+NSIR)                                                     
   82 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9098                                              
      XXIPR=D(IP+NSIR)                                                          
      XXIHR=D(IH+NSIR)                                                          
      WRITE (OUT,8999)                                                          
 8999 FORMAT('  *** WRITES FROM CONDPP ******************************')         
      WRITE (OUT,9000) TOUT  , VOUT  , DELTAM, H                                
 9000 FORMAT('  TOUT  =', E11.4, '  VOUT  =', E11.4, '  DELTAM=', E11.4         
     *      ,'  H     =', E11.4)                                                
      WRITE (OUT,9001) TIN   , VIN   , XXIPR , XXIHR                            
 9001 FORMAT('  TIN   =', E11.4, '  VIN   =', E11.4, '  XXIPR =', E11.4         
     *      ,'  XXIHR =', E11.4)                                                
      WRITE (OUT,9100) VOUT  , QUAL  , V                                        
 9100 FORMAT('  VOUT  =', E11.4, '  QUAL  =', E11.4, '  V     =', E11.4         
     *      ,'        =', E11.4)                                                
      WRITE (OUT,9101) TC    , SLIP  , VRATIO, TIN                              
 9101 FORMAT('  TC    =', E11.4, '  SLIP  =', E11.4, '  VRATIO=', E11.4         
     *      ,'  TIN   =', E11.4)                                                
      WRITE (OUT,9102) VIN   , PDGO  , PDLO  , VMOMIN                           
 9102 FORMAT('  VIN   =', E11.4, '  PDGO  =', E11.4, '  PDLO  =', E11.4         
     *      ,'  VMOMIN=', E11.4)                                                
      AREF= D(ID(IRCD+12))                                                      
      WRITE (OUT,9103) AREF  , PB    , PHI   , PDR                              
 9103 FORMAT('  AREF  =', E11.4, '  PB    =', E11.4, '  PHI   =', E11.4         
     *      ,'  PDR   =', E11.4)                                                
      WRITE (OUT,9104) VOIDF , XMOM                                             
 9104 FORMAT('  VOIDF =', E11.4, '  XMOM  =', E11.4, '        =', E11.4         
     *      ,'        =', E11.4)                                                
 9098 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C***PRESSURE DROP ON SINK SIDE                                                  
      TAVG = (D(IT+NSIS) + D(IT+NSOS)) / 2.                                     
      PD = TLUP(ID(IRCD+9))                                                     
      IF(ID(IRCD+8).NE.0) PD = PD / SIG(NLS,D(IP+NSIS),TAVG)                    
      D(IP+NSOS) = D(IP+NSIS) - PD                                              
C***BECAUSE WE NOW BALANCE THE ENERGY, THE SINK OUTLET TEMPERATURE              
C   WILL BE COMPUTED CORRECTLY, SO THERE WILL BE NO ERROR.                      
C   THE PRIOR METHOD INCURRED AN ERROR BECAUSE THE REFRIGERANT                  
C   ENTHALPY WAS ALWAYS HFG AND THE REFRIG MASS FLOW WAS COMPUTED               
C   EXTERNALLY FOR THE LEG.                                                     
C                                                                               
C         TO MAINTAIN THE PARALLEL WITH THE PRIOR METHOD,                       
C   WE ASSUME THE OUTLET TEMPERATURE WAS COMPUTED EXTERNALLY,                   
C   SO THE ERROR VARIABLE IS THE DIFFERENCE BETWEEN WHAT IS                     
C   COMPUTED HERE AND WHAT WAS COMPUTED EXTERNALLY.                             
      IF(D(IH+NSIS).NE.0.0.AND.LOCS.EQ.2) CALL TDB(NLS,NSIS,NSOS,HS)            
      IF(IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                      
      CALL PIOP(1,NLR,NSIR,NSOR)                                                
      CALL PIOP(2,NLS,NSIS,NSOS)                                                
      IF (D(IH+NSIS).EQ.0.0 ) GO TO 90                                          
      IF (D(IH+NSOS).GT.HS) CALL HSOP(HS)                                       
   90 CONTINUE                                                                  
      CALL LINES(2)                                                             
      WRITE(OUT,1001) EFF,Q                                                     
 1001 FORMAT(1H0,5X,3HEFF,F7.4,3X,1HQ,F9.2 )                                    
   99 CONTINUE                                                                  
      RETURN                                                                    
C     CONDPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A CONDINSOR                                               
C                                                                               
C**********************************************************************         
C                                                                               
C$          CONDPZ, EVAPPZ, VCMPPZ                                              
C$          CONDPP, EVAPPP, VCMPPP  ROUTINES BELOW                              
      SUBROUTINE CONDPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      EQUIVALENCE (SCR(1),NLR), (SCR(2),NSIR), (SCR(3),NSOR)                    
     *, (SCR(4),NLS), (SCR(5),NSIS), (SCR(6),NSOS)                              
      DIMENSION ICV(18), SCR(30)                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/100/, IFTB/011/                                                 
C   1 COMP CODE                                                                 
C   2 LEG NO R                                                                  
C   3 INLET STATION NO R                                                        
C   4 OUTLET STATION NO R                                                       
C   5 LEG NO S                                                                  
C   6 INLET STATION NO S                                                        
C   7 OUTLET STATION NO S                                                       
C   8 PRESS DROP OPTION                                                         
C   9 PRESS DROP TABLE NO                                                       
C  10 EFFECTIVENESS TABLE NO                                                    
C  11 REFRIG-SIDE PRESSURE DROP TABLE NUMBER                                    
C  12 FLOW AREA ON REFRIGERANT SIDE (SQ.FT.)                                    
      I = IACDB(12)                                                             
      ID(I+1) = ICV(1)                                                          
      NLR = ILEGN(ICV(2))                                                       
      ID(I+2) = NLR                                                             
      CALL LEGRT(NLR)                                                           
      NSIR = ISTAN(ICV(3))                                                      
      ID(I+3) = NSIR                                                            
      CALL START(NSIR)                                                          
      NSOR = ISTAN(ICV(4))                                                      
      ID(I+4) = NSOR                                                            
      CALL STARS(NSOR)                                                          
      CALL FTL(NLR,IFTA)                                                        
      NLS=ILEGN(ICV(5))                                                         
      ID(I+5) = NLS                                                             
      CALL LEGRT(NLS)                                                           
      NSIS = ISTAN(ICV(6))                                                      
      ID(I+6) = NSIS                                                            
      CALL START(NSIS)                                                          
      NSOS = ISTAN(ICV(7))                                                      
      ID(I+7) = NSOS                                                            
      CALL STARS(NSOS)                                                          
      CALL FTL(NLS,IFTB)                                                        
      ID(I+8) = ICV(8)                                                          
      ID(I+9) = ITIDN(ICV(9),1)                                                 
      ID(I+10) = ITIDN(ICV(10),5)                                               
      ID(I+11) = ITIDN(ICV(11),1)                                               
      ID(I+12) = IPARM(ICV(12))                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     CONDPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      A CONDENSER                                                              
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE CONDSP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IDS,C(5)), (PASS,C(17)), (IGA,C(35))                         
     *, (ISV,C(47)), (IEV,C(49)), (NSV,C(50)), (NEV,C(51))                      
     *, (IMP,C(66)), (IME,C(67)), (NIT,C(76)), (PF,C(77)), (NWT,C(78))          
     *, (ITAD,C(54)), (ICONV,C(79)), (OUT,C(7)), (CERR,C(16))                   
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (IFB,C(55)), (WTC,C(102))           
     *, (CUC,C(104)), (RIC,C(106)), (DRC,C(108)), (WTIC,C(109))                 
     *, (WTDC,C(111)), (SCR(1),C(151)), (IRCD,C(45))                            
     *, (ICV(1),C(133)), (IFP,C(22))                                            
     *, (ILN,C(37)), (PUN,C(91))                                                
     *, (IH,C(30))                                                              
      DIMENSION SCR(30), ICV(18)                                                
      INTEGER OUT, CERR, PASS, PUN                                              
      EQUIVALENCE         (ICV( 1),NL    ), (ICV( 2),NSI   )                    
     *, (ICV( 3),NSO   ), (ICV( 4),LS    ), (ICV( 5),LR    )                    
     *, (ICV( 6),LN    ), (ICV( 7),IFGS  ), (ICV( 8),IFGR  )                    
     *, (ICV( 9),TSP   ), (ICV(10),IMS   ), (ICV(11),IMR   )                    
     *, (ICV(12),NP    ), (ICV(13),PSIS  ), (ICV(14),DENI  )                    
     *, (ICV(15),DENO  ), (ICV(16),PSIR  ), (ICV(17),TA    )                    
     *, (ICV(18),PA    )                                                        
      EQUIVALENCE         (SCR( 1),CP    ), (SCR( 2),PR    )                    
     *, (SCR( 3),IDSS  ), (SCR( 4),ISVS  ), (SCR( 5),IEVS  )                    
     *, (SCR( 6),IEM   ), (SCR( 7),DSV   ), (SCR( 8),PAR   )                    
     *, (SCR( 9),JS    ), (SCR(10),IS    ), (SCR(11),WT    )                    
     *, (SCR(12),NTU   ), (SCR(13),EFF   ), (SCR(14),ETA   )                    
     *, (SCR(15),JJ    ), (SCR(16),NSOR  ), (SCR(17),II    )                    
     *, (SCR(18),I     ), (SCR(19),EFFP  ), (SCR(20),K     )                    
     *, (SCR(21),J     ), (SCR(22),IND   ), (SCR(23),JSV1  )                    
     *, (SCR(24),JSV2  ), (SCR(25),JEV1  ), (SCR(26),JEV2  )                    
     *, (SCR(27),IX    ), (SCR(28),NS    ), (SCR(29),NR   )                     
     *, (SCR(30),RHOF  )                                                        
      DIMENSION CD(200)                                                         
       REAL*8 CD, CN                                                          
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84))                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      REAL LS, LR, LN, NTU                                                      
      DIMENSION SN(2)                                                           
      DATA SVLL/0.1/, SVUL/100.0/, SVIG/12.0/, ERL1/0.01/, ERL2/0.0001/         
      DATA SN/4HCOND,4HEVAP/                                                    
C   1 COMP CODE                                                                 
C   2 NLS                                                                       
C   3 NSIS                                                                      
C   4 NSOS                                                                      
C   5 NSOR                                                                      
C   6 WTF                                                                       
C   7 CUF                                                                       
C   8 RI                                                                        
C   9 DRF                                                                       
C  10 LS                                                                        
C  11 LR                                                                        
C  12 LN                                                                        
C  13 NPS                                                                       
C  14 IMS                                                                       
C  15 IMR                                                                       
C  16 IFGS                                                                      
C  17 IFS                                                                       
C  18 IJS                                                                       
C  19 IFGR                                                                      
C  20 IMSP                                                                      
C  21 ITSP                                                                      
C  22 MAP                                                                       
      IE = 1                                                                    
      GO TO 35                                                                  
      ENTRY EVAPSP                                                              
      IE = 2                                                                    
   35 IRCD = IRCDB(22)                                                          
      CN = SN(IE)                                                               
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      NSOR = ID(IRCD+5)                                                         
      IMS = ITLUP(ID(IRCD+14))                                                  
      IMR = ITLUP(ID(IRCD+15))                                                  
      IFGS = ITLUP(ID(IRCD+16))                                                 
      IFGR = ITLUP(ID(IRCD+19))                                                 
    8 TSP = TLUP(ID(IRCD+21))                                                   
      PA = 0.5*(D(IP+NSI)+D(IP+NSO))                                            
      I1 = ID(IRCD+10)                                                          
      LS = D(I1)                                                                
      I1 = ID(IRCD+11)                                                          
      LR = D(I1)                                                                
      I1 = ID(IRCD+12)                                                          
      LN = D(I1)                                                                
      PSIS = D(IFGS+3)*D(IFGS+4)*D(IFGS+6)/(D(IFGS+3)+D(IFGR+3)+2.0*TSP)        
      PSIR = D(IFGR+3)*D(IFGR+4)*D(IFGR+6)/(D(IFGS+3)+D(IFGR+3)+2.0*TSP)        
      DENI = DEN(NL,D(IP+NSI),D(IT+NSI))                                        
      DENO = DEN(NL,D(IP+NSO),D(IT+NSO))                                        
      TA = 0.5*(D(IT+NSI)+D(IT+NSO))                                            
      VSC = VIS(NL,PA,TA)                                                       
      CP = SHP(NL,PA,TA,D(IH+NSI))                                              
      PR = (3600.0*VSC*CP/COND(NL,PA,TA))**.6666667                             
      NP = ID(IRCD+13)                                                          
      IDSS = IDS                                                                
      ICONV = 0                                                                 
      REP = 0.8*D(IFGS+4)*D(IW+NL)*NP/(VSC*PSIS)                                
      EV1P1 = VSC**2*(1.0+PSIS**2)/(1029.568*D(IFGS+4)**2*DENI)                 
      EV1P2 = NP*(DENI/DENO+1.0)/(2.0*D(IFGS+4)*(1.0+PSIS**2))                  
      EP  = CP*VSC/(D(IFGS+4)*D(IFGS+5)*PR*POLYI(2,D(IMS+5),TA))*1800.0         
      EV2P1 = 0.8*D(IW+NL)*D(IFGS+4)**2*PR/(VSC*PSIS)                           
      IF (D(IFGS+8).EQ.3.0) GO TO 303                                           
      EFL = D(IFGS+3)/2.0                                                       
      GO TO 304                                                                 
  303 EFL = SQRT(D(IFGS+3)**2+1.0/D(IFGS+2)**2)/2.0                             
  304 CONTINUE                                                                  
      IF (IE.EQ.1) GO TO 30                                                     
      EV2P2 = D(IW+NL)*CP*D(IFGR+4)/(0.017361*PSIR)                             
      GO TO 31                                                                  
   30 EV2P2 = D(IW+NL)*CP*D(IFGR+4)/(0.023148*PSIR)                             
   31 IF (LN.NE.0.0) GO TO 200                                                  
      EFFP = (D(IT+NSI)-TDBI(NL,NSI,NSO))/(D(IT+NSI)-D(IT+NSOR))                
      PASS = 1                                                                  
      IFP = 0                                                                   
      NSV = 2                                                                   
      NEV = 2                                                                   
      NUM = 4                                                                   
      CALL GDCU(NSV,4,2,D,ISV)                                                  
      CALL GDCU(NEV,4,2,D,IEV)                                                  
      CALL GDCU(NSV,4,2,D,ISVS)                                                 
      CALL GDCU(NEV,4,2,D,IEVS)                                                 
      CALL GDCU(NUM,4,2,D,IMP)                                                  
      CALL GDCU(NSV,4,2,D,IME)                                                  
      CALL GDCU(NSV,4,2,D,IEM)                                                  
      D(ISV+1) = SVIG                                                           
      D(ISV+2) = SVIG                                                           
      ASSIGN 4 TO JS                                                            
      GO TO 300                                                                 
    4 IF (IDS.EQ.0) GO TO 1                                                     
      ASSIGN 1 TO IS                                                            
      GO TO  101                                                                
    1 CONTINUE                                                                  
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
    2 D(IEVS+I) = D(IEV+I)                                                      
      DO 11 J=1,NSV                                                             
      D(ISV+J) = D(ISVS+J)*PF                                                   
      DSV = D(ISV+J)-D(ISVS+J)                                                  
      ASSIGN 5 TO JS                                                            
      GO TO 300                                                                 
    5 DO 14 K=1,NEV                                                             
      PAR = (D(IEV+K)-D(IEVS+K))/DSV                                            
      CALL MSTO(K,J,PAR)                                                        
   14 CONTINUE                                                                  
      CALL MSTO(J,0,D(IEVS+J))                                                  
      ASSIGN 21 TO IS                                                           
      IF (IDS.EQ.3) GO TO 101                                                   
   21 CONTINUE                                                                  
      D(ISV+J) = D(ISVS+J)                                                      
   11 CONTINUE                                                                  
      IF (IDS.GE.2) CALL MPRNT(D(IMP+1),NSV,D(IME+1))                           
      CALL MSOL(D(IMP+1),NSV,D(IME+1),D(IEM+1),IND)                             
      IF (IND.NE.1) GO TO 40                                                    
      DO 16 J=1,NSV                                                             
      D(ISV+J) = D(ISV+J)+WT*D(IMP+J)                                           
      IF (D(ISV+J).GT.SVUL) D(ISV+J) = SVUL                                     
      IF (D(ISV+J).LT.SVLL) D(ISV+J) = SVLL                                     
   16 CONTINUE                                                                  
      ASSIGN 6 TO JS                                                            
      GO TO 300                                                                 
    6 ASSIGN 20 TO IS                                                           
      IF (IDS.GE.1) GO TO 101                                                   
   20 CONTINUE                                                                  
      ICONV = 0                                                                 
      IF (ABS(D(IEV+1)).GT.ERL1 .OR. ABS(D(IEV+2)).GT.ERL2) ICONV = 1           
      IF (ICONV.EQ.0) GO TO 199                                                 
   10 CONTINUE                                                                  
   17 WTC = 0.0                                                                 
      RIC = 0.0                                                                 
      CUC = 0.0                                                                 
      DRC = 0.0                                                                 
      CALL FDC(0,4,2,D,0)                                                       
      GO TO 98                                                                  
   40 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1009) CERR                                                     
 1009 FORMAT(6H0ERROR,I6,5X,15HSINGULAR MATRIX)                                 
      ICONV = 1                                                                 
      GO TO 17                                                                  
  101 CONTINUE                                                                  
      JSV1 = ISV+1                                                              
      JSV2 = ISV+NSV                                                            
      JEV1 = IEV+1                                                              
      JEV2 = IEV+NEV                                                            
      CALL LINES(10)                                                            
      WRITE (OUT,1003) PASS                                                     
 1003 FORMAT(6H0PASS ,I6)                                                       
      WRITE (OUT,1004) (D(IX),IX=JSV1,JSV2)                                     
 1004 FORMAT(5H0S.V./(1X,10E12.5))                                              
      WRITE (OUT,1005) (D(IX),IX=JEV1,JEV2)                                     
 1005 FORMAT(5H0E.V./(1X,10E12.5))                                              
      WRITE (OUT,1000) D(IGA+21),NTU,EFF,ETA                                    
 1000 FORMAT(1H0,4E12.5)                                                        
      GO TO IS, (1,20,21)                                                       
  300 IF (IE.EQ.1) GO TO 301                                                    
      LR = 3.0+0.18*D(ISV+2)+SQRT(1.08*D(ISV+2)+0.0324*D(ISV+2)**2)             
      GO TO 302                                                                 
  301 LR = 6.0+0.08*D(ISV+2)+SQRT(0.96*D(ISV+2)+0.0064*D(ISV+2)**2)             
  302 D(IGA+21) = REP/(D(ISV+2)*LR)                                             
      D(IEV+1) = EV1P1*D(IGA+21)**2*(D(ISV+1)*TLUP(ID(IRCD+17))*EV1P2           
     * +DENI/DENO-1.0) - (D(IP+NSI)-D(IP+NSO))                                  
      AJ = TLUP(ID(IRCD+18))                                                    
      ETA = SQRT(EP*D(IGA+21)*AJ*(1.0+D(IFGS+5)/D(IFGS+7)))                     
      ETA = 1.0-D(IFGS+1)*(1.0-TANH(ETA*EFL)/(ETA*EFL))                         
      NTU = D(ISV+1)*D(ISV+2)*LR/(EV2P1/(D(IGA+21)*AJ*ETA)+EV2P2)               
      EFF = 1.0-EXP(-NTU)                                                       
      D(IEV+2) = EFF-EFFP                                                       
      GO TO JS, (4,5,6,7)                                                       
  199 CONTINUE                                                                  
      ASSIGN 7 TO JS                                                            
      IFP = 1                                                                   
      GO TO 300                                                                 
    7 LS = D(ISV+1)                                                             
      LN = D(ISV+2)                                                             
      CALL FDC(0,4,2,D,0)                                                       
  200 NS = (LN-D(IFGR+3)-2.0*TSP)/(D(IFGS+3)+D(IFGR+3)+2.0*TSP)                 
      NR = NS+1                                                                 
      VOL = LS*LN*LR                                                            
      IFG = IFGS                                                                
      IM = IMS                                                                  
      JJ = 0                                                                    
   70 IFS = D(IFG+8)+0.1                                                        
      BB = (D(IFG+3)-(D(IFG+9)-1.0)*TSP)/D(IFG+9)                               
      IF (IFS-2) 71,72,73                                                       
   71 RHOF = D(IFG+5)*D(IFG+2)*D(IM+1)*(BB+1.0/D(IFG+2)                         
     * -2.71682*D(IFG+5))/BB                                                    
      GO TO 74                                                                  
   72 RHOF = D(IFG+5)*D(IFG+2)*D(IM+1)*(BB+0.5708/D(IFG+2)-D(IFG+5))/BB         
      GO TO 74                                                                  
   73 RHOF = (6.0*D(IFG+5)*D(IFG+2)**2*BB-1.0+SQRT(1.0+D(IFG+2)**2*BB*BB        
     * -12.0*D(IFG+5)*D(IFG+2)**2*BB))/(0.3333/D(IFG+5)-12.0*D(IFG+5)           
     * *D(IFG+2)**2)                                                            
      PHI = 2.0*ACOS(3.0*D(IFG+5)/(RHOF+3.0*D(IFG+5)))                          
      RHOF = 1.0/(D(IFG+2)*COS(PHI/2.0))-6.0*D(IFG+5)*D(IFG+2)                  
     * *(BB+2.0*RHOF)                                                           
      RHOF = D(IFG+2)*D(IFG+5)*D(IM+1)*(RHOF+3.0*D(IFG+5)*PHI)/BB               
   74 IF (JJ.EQ.0) GO TO 75                                                     
      WTFR = RHOF*NR*(D(IFGR+3)-(D(IFGR+9)-1.0)*TSP)*LS*LR/1728.0               
      GO TO 80                                                                  
   75 WTFS = RHOF*NS*(D(IFGS+3)-(D(IFGS+9)-1.0)*TSP)*LS*LR/1728.0               
      IFG = IFGR                                                                
      IM = IMR                                                                  
      JJ = 1                                                                    
      GO TO 70                                                                  
   80 IMSP = ITLUP(ID(IRCD+20))                                                 
      WTCO = WTFS+WTFR+D(IMSP+1)*LS*LR*TSP*(NS*D(IFGS+9)+NR*D(IFGR+9)           
     * +1.0)/1728.0                                                             
      IF (D(IMSP+1).GT.460.0) WTCO = WTCO+6.44E-4*LS*LR                         
     * *(NS*D(IFGS+9)+NR*D(IFGR+9)+1.0)                                         
      WTC = 3.23126*WTCO/VOL**0.118                                             
      I1 = ID(IRCD+6)                                                           
      WTF = D(I1)                                                               
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      WTIC = 0.205*WTC                                                          
      WTDC = 0.12*WTC                                                           
      RIC = 0.00216                                                             
      IF (IE.EQ.1) RIC = 0.00306                                                
      I1 = ID(IRCD+8)                                                           
      RI = D(I1)                                                                
      IF (RI.NE.0.0) RIC = RI                                                   
      CUC = 29.7+2.72*WTC                                                       
      I1 = ID(IRCD+7)                                                           
      CUF = D(I1)                                                               
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      PRES = PA                                                                 
      IF (PRES.LT.314.7) PRES = 314.7                                           
      DRC = 1.0+((PRES-314.7)/283.0)**2                                         
      IF (IE.EQ.2) DRC = 1.2*DRC                                                
      I1 = ID(IRCD+9)                                                           
      DRF = D(I1)                                                               
      IF(DRF.NE.0.0) DRC = DRC*DRF                                              
      IF(DRC.GT.10.0) DRC = 10.0                                                
      IFT = ID(IFB+NL)                                                          
      IFT = ID(IFT+1)                                                           
      IF (IFT.EQ.2) GO TO 97                                                    
      VL = PSIS*VOL                                                             
   97 CALL SSA                                                                  
   98 CALL SCO                                                                  
      IDS = IDSS                                                                
      CALL SCI(NL,NSI,NSO)                                                      
      IF (ICONV.EQ.0) GO TO 85                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1010) CERR                                                     
 1010 FORMAT(6H0ERROR,I6,5X,14HNON CONVERGENT)                                  
      GO TO 99                                                                  
   85 CONTINUE                                                                  
      CALL LINES(2)                                                             
      WRITE (OUT,1002) LS,LR,LN,VOL,WTCO                                        
 1002 FORMAT(1H0,5X,2HLS,F8.2,3X,2HLR,F8.2,3X,2HLN,F8.2,3X,3HVOL,F7.0,3X        
     *,3HWTC,F7.1)                                                              
      I1 = ID(IRCD+12)                                                          
      IF (D(I1).NE.0.0) GO TO 86                                                
      WRITE (OUT,1008) EFF,NTU                                                  
C$ FIRST CHANGE IN CONDSP IS HERE                                               
 1008 FORMAT(1H+,70X,3HEFF,F7.4,3X,3HNTU,F10.3)                                 
      REP = D(IGA+21)                                                           
C     WRITE (OUT,1200) REP,EV1P1,EV1P2,DENI,DENO                                
 1200 FORMAT (1H0,5X,3HREP,F8.2,3X,5HEV1P1,E8.3,3X,5HEV1P2,F8.3,3X,             
     *4HDENI,F8.3,3X,4HDENO,F8.3)                                               
   86 IF (IFT.EQ.1) WRITE (OUT,1006) VL                                         
 1006 FORMAT(1H+,76X,3HVL=,F8.1)                                                
      D(IGA+71) = LS                                                            
      D(IGA+72) = LR                                                            
      D(IGA+73) = LN                                                            
      D(IGA+74) = VOL                                                           
      D(IGA+75) = VL                                                            
      D(IGA+76) = WTC                                                           
      MAP = ID(IRCD+22)                                                         
      IF (MAP.EQ.0) GO TO 99                                                    
      IFP = 0                                                                   
      CALL GDCU(48,4,2,D,LOC)                                                   
      LOCE = LOC+40                                                             
      LOCP = LOC+80                                                             
      REP = 0.8*D(IFGS+4)*NP/(VSC*PSIS*LN*LR)                                   
      EP = CP*VSC*(1.0+D(IFGS+5)/D(IFGS+7))/(D(IFGS+4)*D(IFGS+5)*PR*            
     *POLYI(2,D(IMS+5),TA))*1800.0                                              
      EV2P1 = 0.8*D(IFGS+4)**2*PR/(VSC*PSIS)                                    
      IF (IE.EQ.1) GO TO 87                                                     
      EV2P2 = CP*D(IFGR+4)/(0.017361*PSIR)                                      
      GO TO 88                                                                  
   87 EV2P2 = CP*D(IFGR+4)/(0.023148*PSIR)                                      
   88 IFTC = ID(IFB+NL)                                                         
      IFTC = ID(IFTC+1)                                                         
      IF (IFTC.EQ.2) SIGA = SIG(NL,PA,TA)                                       
C$ WAS 16 ITERATIONS                                                            
      DO 3050 I=1,40                                                            
C$WAS W = I*0.2*D(IW+NL)                                                        
      W = I*0.2*D(IW+NL)/5.0                                                    
      D(IGA+21) = REP*W                                                         
      PD = EV1P1*D(IGA+21)**2*(LS*TLUP(ID(IRCD+17))*EV1P2+DENI/DENO-1.0)        
      IF (IFTC.EQ.2) PD = PD*SIGA                                               
      AJ = TLUP(ID(IRCD+18))                                                    
      ETA = SQRT(EP*D(IGA+21)*AJ)                                               
      ETA = 1.0-D(IFGS+1)*(1.0-TANH(ETA*EFL)/(ETA*EFL))                         
      NTU = LS*LN*LR/(EV2P1*W/(D(IGA+21)*AJ*ETA)+EV2P2*W)                       
      EFF = 1.0-EXP(-NTU)                                                       
      IF (EFF.GT.1.0) EFF = 1.0                                                 
C$ THESE TWO WRITES ADDED                                                       
      WRITE(12) W, EFF                                                          
      WRITE(13) W, PD                                                           
      D(LOC+I) = W                                                              
      D(LOCE+I) = EFF                                                           
      D(LOCP+I) = PD                                                            
 3050 CONTINUE                                                                  
      I1 = LOC+1                                                                
C$ WAS LOC+16                                                                   
      I2 = LOC+40                                                               
      IF (MAP.EQ.2) GO TO 3001                                                  
      CALL LINES(12)                                                            
      WRITE (OUT,2009) SN(IE)                                                   
 2009 FORMAT(1H ,A4,4H MAP)                                                     
      WRITE (OUT,2011) ID(ILN+NL)                                               
 2011 FORMAT(6H0    *,28HTABID          5   2   0   1,I4,8H 0 1  40,            
     * 40X,1H*)                                                                 
      WRITE (OUT,2012) SN(IE)                                                   
 2012 FORMAT(5X,1H*,6HTABT  ,A4,4H EFF,66X,1H*)                                 
C$ WAS   I+16                                                                   
      WRITE (OUT,2010) (D(I),D(I+40),I=I1,I2)                                   
 2010 FORMAT(5X,1H*,6HTABV  ,4X,4F10.4,30X,1H*)                                 
 3001 CONTINUE                                                                  
      IF (MAP.EQ.1) GO TO 3011                                                  
      WRITE (PUN,2001) ID(ILN+NL)                                               
C$ WAS                                                  16                      
 2001 FORMAT(28HTABID          5   2   0   1,I4,8H 0 1  40,24X)                 
      WRITE (PUN,2002) SN(IE)                                                   
 2002 FORMAT(6HTABT  ,A4,4H EFF,66X)                                            
C$ WAS   I+16                                                                   
      WRITE (PUN,2000) (D(I),D(I+40),I=I1,I2)                                   
 2000 FORMAT(6HTABV  ,4X,4F10.4,30X)                                            
 3011 CONTINUE                                                                  
      IF (MAP.EQ.2) GO TO 3004                                                  
      CALL LINES(11)                                                            
      WRITE (OUT,2013) ID(ILN+NL)                                               
 2013 FORMAT(6H0    *,28HTABID          1   2   3   1,I4,8H 0 1  40,            
     * 40X,1H*)                                                                 
      WRITE (OUT,2014) SN(IE)                                                   
 2014 FORMAT(5X,1H*,6HTABT  ,A4,3H PD,67X,1H*)                                  
C$ WAS   I+32                                                                   
      WRITE (OUT,2010) (D(I),D(I+80),I=I1,I2)                                   
 3004 CONTINUE                                                                  
      IF (MAP.EQ.1) GO TO 3014                                                  
      WRITE (PUN,2003) ID(ILN+NL)                                               
C$ WAS                                                  16                      
 2003 FORMAT(28HTABID          1   2   3   1,I4,8H 0 1  40,40X)                 
      WRITE (PUN,2004) SN(IE)                                                   
 2004 FORMAT(6HTABT  ,A4,3H PD,67X)                                             
C$ WAS   I+32                                                                   
      WRITE (PUN,2000) (D(I),D(I+80),I=I1,I2)                                   
 3014 CONTINUE                                                                  
      CALL FDC(0,4,2,D,0)                                                       
      GO TO 99                                                                  
   99 CONTINUE                                                                  
      RETURN                                                                    
C     CONDSP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE COPPP                                                          
      COMMON/CC/C(600)                                                          
      EQUIVALENCE (OUT,C(7)),(IW,C(27)),                                        
     * (IH,C(30)),(ISN,C(38)),(ICDB,C(43)),                                     
     * (NCOMP,C(44)), (IRCD,C(45)), (IFP,C(22)), (ICPP,C(88)),                  
     * (SCR(1),C(151))                                                          
      INTEGER OUT                                                               
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (SCR(1),NE), (SCR(2),NVC), (SCR(3),QCOOL),                    
     * (SCR(4),QWORK), (SCR(5),IRET), (SCR(6),ISTA), (SCR(7),LOC),              
     * (SCR(8),LCOMP), (SCR(9),NSI), (SCR(10),NSO), (SCR(11),NL)                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 NUMBER OF EVAPORATORS                                                     
C   3 NUMBER OF COMPRESSORS                                                     
C 4-18 INLET STATION NUMBERS OF EVAPORATORS AND VAPOR COMPRESSORS               
      IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      IRCD=IRCDB(18)                                                            
      NE=ID(IRCD+2)                                                             
      NVC=ID(IRCD+3)                                                            
      QCOOL=0.                                                                  
      QWORK=0.                                                                  
      IRET=0                                                                    
      IF (NE.EQ.0) GO TO 100                                                    
      IF (NVC.EQ.0) GO TO 200                                                   
    5 DO 50 J=1,NE                                                              
      ISTA=ID(IRCD+3+J)                                                         
      DO 40 K=1,NCOMP                                                           
      LOC=ID(ICDB+K)                                                            
      LCOMP=ID(LOC+1)                                                           
      IF (LCOMP.EQ.37. OR. LCOMP.EQ.39 .OR. LCOMP.EQ.40) GO TO 10               
      GO TO 40                                                                  
   10 NSI=ID(LOC+3)                                                             
      NSO=ID(LOC+4)                                                             
      NL=ID(LOC+2)                                                              
      IF (ID(ISN+NSI).NE.ISTA) GO TO 40                                         
      QCOOL=QCOOL+D(IW+NL)*(D(IH+NSO)-D(IH+NSI))                                
      GO TO 50                                                                  
   40 CONTINUE                                                                  
      WRITE(OUT,1000) ISTA                                                      
 1000 FORMAT(' WARNING IN COP - NO EVAPORATOR FOUND FOR STATION',I4)            
   50 CONTINUE                                                                  
      IF (IRET.EQ.1) GO TO 210                                                  
   52 DO 80 J=1,NVC                                                             
      ISTA=ID(IRCD+3+NE+J)                                                      
      DO 60 K=1,NCOMP                                                           
      LOC=ID(ICDB+K)                                                            
      LCOMP=ID(LOC+1)                                                           
      IF(LCOMP.EQ.35) GO TO 55                                                  
      GO TO 60                                                                  
   55 NSI=ID(LOC+3)                                                             
      NSO=ID(LOC+4)                                                             
      NL=ID(LOC+2)                                                              
      IF (ID(ISN+NSI).NE.ISTA) GO TO 60                                         
      QWORK=QWORK+D(IW+NL)*(D(IH+NSO)-D(IH+NSI))                                
      GO TO 80                                                                  
   60 CONTINUE                                                                  
      WRITE(OUT,1100) ISTA                                                      
 1100 FORMAT(' WARNING IN COP - NO VAPOR COMPRESSOR FOUND FOR STATION',         
     * I4)                                                                      
      RETURN                                                                    
   80 CONTINUE                                                                  
      IF (IRET.EQ.1) GO TO 110                                                  
      CP=QCOOL/QWORK                                                            
      CALL LINES(5)                                                             
      WRITE(OUT,1200) CP                                                        
 1200 FORMAT(1H0,'COEFFICIENT OF PERFORMANCE =',G10.4)                          
      WRITE(OUT,1300) (ID(IRCD+3+J),J=1,NE)                                     
 1300 FORMAT(' EVAPORATOR STATIONS',8X,15I4)                                    
      WRITE(OUT,1400) (ID(IRCD+3+NE+J),J=1,NVC)                                 
 1400 FORMAT(' VAPOR COMPRESSOR STATIONS',15I4)                                 
      RETURN                                                                    
  100 IRET=1                                                                    
      GO TO 52                                                                  
  110 CALL LINES(2)                                                             
      WRITE(OUT,1500) QWORK                                                     
 1500 FORMAT(' VAPOR COMPRESSOR WORK =',G10.4,' BTU/MIN')                       
      WRITE(OUT,1400) (ID(IRCD+3+J),J=1,NVC)                                    
      RETURN                                                                    
  200 IRET=1                                                                    
      GO TO 5                                                                   
  210 CALL LINES(2)                                                             
      WRITE(OUT,1600) QCOOL                                                     
 1600 FORMAT(' COOLING LOAD =',G10.4,' BTU/MIN')                                
      WRITE(OUT,1300) (ID(IRCD+3+J),J=1,NE)                                     
   99 RETURN                                                                    
C     COPPP                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE COPPZ                                                          
      COMMON/CC/C(600)                                                          
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18),SCR(30)                                                 
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 NUMBER OF EVAPORATORS                                                     
C   3 NUMBER OF VAPOR COMPRESSORS                                               
C4-18 INLET STATION NUMBERS OF EVAPORATORS AND VAPOR COMPRESSORS                
      I=IACDB(18)                                                               
      DO 10 J=1,18                                                              
      ID(I+J)=ICV(J)                                                            
   10 CONTINUE                                                                  
      RETURN                                                                    
C     COPPZ                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C       A CONTROL VALVE                                                         
C                                                                               
C$    SUBROUTINE CVLVPP IS MODIFIED AS IN MANUAL                                
C**********************************************************************         
C                                                                               
      SUBROUTINE CVLVPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)), (SCR(1),C(151)), (ICPP,C(88)), (OUT,C(7))                   
     *, (ISV,C(47)), (PASS,C(17)), (ISVT,C(46))                                 
     *, (IFP,C(22)), (ISVS,C(92)), (IIOP,C(90))                                 
     *, (IFB,C(55))                                                             
      DIMENSION SCR(30)                                                         
      INTEGER PASS, OUT                                                         
      EQUIVALENCE (SCR(1),NL),(SCR(2),NSI),(SCR(3),NSO)                         
     *, (SCR(4),JSVI),(SCR(5),PD),(SCR(6),AK),(SCR(7),HS)                       
     *, (SCR(8),NC),(SCR(9),LOCT),(SCR(10),V),(SCR(11),H)                       
     *, (SCR(12),PO), (SCR(13),TO)                                              
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 PRESS. DROP OPTION                                                        
C   6 MIN. K                                                                    
C   7 INITIAL K                                                                 
C   8 STATE VARIABLE INDEX                                                      
      IRCD = IRCDB(8)                                                           
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      JSVI = ID(IRCD+8)                                                         
      NC = ID(IFB+NL)                                                           
      LOCT = ID(NC+3)                                                           
      NC = ID(NC+1)                                                             
      IF(PASS.NE.1) GO TO 3                                                     
      ID(ISVT+JSVI) = 6                                                         
      D(ISV+JSVI) = D(ID(IRCD+7))                                               
    3 IF (IIOP.NE.0.OR. D(ISV+JSVI).GE.D(ID(IRCD+6))) GO TO 4                   
      D(ISV+JSVI) = D(ID(IRCD+6))                                               
      D(ISVS+JSVI) = D(ISV+JSVI)                                                
    4 AK = D(ISV+JSVI)                                                          
      PD = AK*D(IW+NL)**2                                                       
      IF (NC.NE.3 .AND. ID(IRCD+5).NE.0 ) PD =PD/SIG(NL,D(IP+NSI),              
     * D(IT+NSI) )                                                              
      IF (IIOP.NE.JSVI) GO TO 1                                                 
      ITER = 0                                                                  
    2 IF (PD.GE.0.01.AND. PD.GE.0.001*D(IP+NSI)) GO TO 1                        
      PD = 2.0*PD                                                               
      D(ISV+JSVI) = 2.0*D(ISV+JSVI)                                             
      ITER = ITER+1                                                             
      IF (ITER.EQ.100) GO TO 1                                                  
      GO TO 2                                                                   
    1 D(IP+NSO) = D(IP+NSI)-PD                                                  
      D(IRCD-6) = AK                                                            
      IF (D(IP+NSI).GT.0.0.AND.D(IP+NSO).LE.0.0)                                
     * GO TO 9                                                                  
      IF(NC.EQ.3) GO TO 5                                                       
      D(IT+NSO) = D(IT+NSI)                                                     
    6 D(IH+NSO)=D(IH+NSI)                                                       
      IF (NC.EQ.3 ) GO TO 7                                                     
      IF (D(IH+NSI).NE.0.0) CALL TDB(NL,NSI,NSO,HS)                             
    7 IF(IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                      
      CALL PIOP(1,NL,NSI,NSO)                                                   
      IF (NC.NE.3 .AND. D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HS)                 
     * CALL HSOP(HS)                                                            
      CALL LINES(2)                                                             
      WRITE (OUT,1001) AK                                                       
 1001 FORMAT(1H0,5X,1HK,1PE15.8)                                                
   99 CONTINUE                                                                  
      RETURN                                                                    
    5 QUAL=VQUALH('CVLVPP  ',LOCT,D(IP+NSO),D(IH+NSI))                          
      IF(QUAL.LT.0.0)D(IT+NSO)=VTLIQ(LOCT,D(IP+NSO),D(IH+NSI))                  
      IF(QUAL.GE.0..AND.QUAL.LE.1.)D(IT+NSO)=VTS(LOCT,D(IP+NSO))                
      IF(QUAL.GT.1.)CALL VTAV2(LOCT,D(IT+NSO),V,D(IP+NSO),D(IH+NSI))            
      GO TO 6                                                                   
    9 PO =0.0                                                                   
    8 D(ISV+JSVI) = 0.9*(D(IP+NSI)-PO)/D(IW+NL)**2                              
      IF (NC.NE.3 .AND. ID(IRCD+5).NE.0) D(ISV+JSVI) = D(ISV+JSVI)*             
     * SIG(NL,D(IP+NSI),D(IT+NSI))                                              
      GO TO 4                                                                   
C     CVLVPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A CONTROL VALVE                                           
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE CVLVPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                       
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/111/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 PRESS.DROP OPTION                                                         
C   6 MIN. K                                                                    
C   7 INITIAL K                                                                 
C   8 STATE VARIABLE INDEX                                                      
      I = IACDB(8)                                                              
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
      ID(I+6) = IPARM(ICV(6))                                                   
      ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = IASV(6)                                                         
      RETURN                                                                    
C     CVLVPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      A DUST SEPERATOR                                                         
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE DSEPSP                                                         
      COMMON  /CC/  C(600)                                                      
      EQUIVALENCE (IRCD,C(45)), (SCR(1),C(151)), (IW,C(27))                     
     *, (IP,C(28)), (IT,C(29)), (WTC,C(102)), (CUC,C(104))                      
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))                
     *, (OUT,C(7))                                                              
     *, (IGA,C(35))                                                             
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO), (SCR(4),WTF)         
     *, (SCR(5),CUF), (SCR(6),RI), (SCR(7),DRF), (SCR(8),IDT)                   
     *, (SCR(9),VOL)                                                            
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
C   9 IDT                                                                       
      IRCD = IRCDB(9)                                                           
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      WTF = D(ID(IRCD+5))                                                       
      CUF = D(ID(IRCD+6))                                                       
      RI = D(ID(IRCD+7))                                                        
      DRF = D(ID(IRCD+8))                                                       
      IDT = ID(IRCD+9)                                                          
      IF (IDT.EQ.2) GO TO 2                                                     
    1 WTC = 0.195*D(IW+NL)**0.79                                                
      GO TO 3                                                                   
    2 WTC = 0.5+0.0346*D(IW+NL)                                                 
    3 IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      IF (IDT.EQ.2) GO TO 12                                                    
   11 WTDC = 0.04*WTC                                                           
      VOL = 7.3*WTC**1.28                                                       
      CUC = 5.4+0.71*WTC                                                        
      GO TO 13                                                                  
   12 WTDC = 0.1225*WTC                                                         
      VOL = 19.0*WTC**1.5                                                       
      CUC = 9.11+1.07*WTC                                                       
   13 WTIC = 0.205*WTC                                                          
      DRC = 1.0                                                                 
      RIC = 0.04135                                                             
      IF (RI.NE.0.0) RIC = RI                                                   
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      CALL SSA                                                                  
      CALL SCO                                                                  
      CALL SCI(NL,NSI,NSO)                                                      
      CALL LINES(2)                                                             
      WRITE (OUT,1000) VOL                                                      
 1000 FORMAT (1H0,5X,3HVOL,F7.0)                                                
      D(IGA+71) = VOL                                                           
      RETURN                                                                    
C     DSEPSP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE DSEPSZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
     *, (OUT,C(7)), (CERR,C(16))                                                
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER OUT,CERR                                                          
      EQUIVALENCE (SCR(1),NL)                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 WTF                                                                       
C   6 CUF                                                                       
C   7 RI                                                                        
C   8 DRF                                                                       
C   9 IDT                                                                       
      I = IACDB(9)                                                              
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
      ID(I+9) = ICV(9)                                                          
      IF (ICV(9).GE.1 .OR. ICV(9).LE.2) GO TO 99                                
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,ICV(9)                                              
 1000 FORMAT(6H0ERROR,I6,5X,27HINVALID DUST SEPARATOR TYPE,I6)                  
   99 CONTINUE                                                                  
      RETURN                                                                    
C     DSEPSZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE EHTRSP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151)), (IW,C(27))         
     *, (IP,C(28)), (IT,C(29)), (WTC,C(102)), (CUC,C(104))                      
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))                
     *, (ILN,C(37)), (IFP,C(22)), (IGA,C(35)), (IDS,C(5))                       
     *, (PASS,C(17)), (NIT,C(76)), (PF,C(77)), (NWT,C(78)), (ITAD,C(54))        
     *, (ICONV,C(79)), (CERR,C(16)), (PUN,C(91))                                
     *, (ICV(1),C(133)), (IH,C(30))                                             
      DIMENSION SCR(30),ICV(18), WP(8)                                          
      INTEGER OUT,PASS,CERR,PUN                                                 
      EQUIVALENCE         (ICV( 1),NL    ), (ICV( 2),NSI   )                    
     *, (ICV( 3),NSO   ), (ICV( 4),VOL   ), (ICV( 5),AL    )                    
     *, (ICV( 6),MAP   ), (ICV( 7),IS    ), (ICV( 8),JS    )                    
     *, (ICV( 9),EVP   ), (ICV(10),EVS   ), (ICV(11),ALS   )                    
     *, (ICV(12),IFG   ), (ICV(13),VSC   ), (ICV(14),DENI  )                    
     *, (ICV(15),DENO  ), (ICV(16),PSI   ), (ICV(17),EVP1  )                    
     *, (ICV(18),EVP2  )                                                        
      EQUIVALENCE         (SCR( 1),REP   ), (SCR( 2),I     )                    
     *, (SCR( 3),J     ), (SCR( 4),WP(1) ), (SCR(12),PA    )                    
     *, (SCR(13),TA    ), (SCR(14),WTF   ), (SCR(15),CUF   )                    
     *, (SCR(16),RI    ), (SCR(17),DRF   ), (SCR(18),IDSS  )                    
     *, (SCR(18),IND   ), (SCR(19),DH    ), (SCR(20),DV    )                    
     *, (SCR(21),WT    ), (SCR(22),II    ), (SCR(23),EV    )                    
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DIMENSION TABD(3), TABV(3)                                                
      DATA ALUL/100.0/, ALLL/0.1/, ALIG/6.0/, ERL/0.01/                         
      DATA TABD /1.302,1.302,1.042/, TABV /1.0,2.9,3.493/                       
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 NSI                                                                       
C   4 NSO                                                                       
C   5 WTF                                                                       
C   6 CUF                                                                       
C   7 RI                                                                        
C   8 DRF                                                                       
C   9 L                                                                         
C  10 IHEL                                                                      
C  11 IFG                                                                       
C  12 IF                                                                        
C  13 MAP                                                                       
      IRCD = IRCDB(13)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      PA = 0.5*(D(IP+NSI)+D(IP+NSO))                                            
      TA = 0.5*(D(IT+NSI)+D(IT+NSO))                                            
      VOL = 1.21444 * D(IW+NL) * SHP(NL,PA,TA,D(IH+NSI)) *                      
     * (TDBI(NL,NSI,NSO) - D(IT+NSI))                                           
c@      CALL MDSCT(2,IND,DH,TABD,ALOG10(VOL),TABV,3,1,DV,DV,DV)                   
      CALL MDSCT(2,IND,DH,TABD,ALOG10(VOL),TABV,3,1,DV,DV,IDV,Idummy)                   
      WTC = VOL*10.0**DH/1728.0                                                 
      WTF = D(ID(IRCD+5))                                                       
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      CUC = 2.08*WTC                                                            
      CUF = D(ID(IRCD+6))                                                       
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      RIC = 0.01109                                                             
      RI = D(ID(IRCD+7))                                                        
      IF (RI.NE.0.0) RIC = RI                                                   
      DRC = 1.0                                                                 
      DRF = D(ID(IRCD+8))                                                       
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      WTIC = 0.205*WTC                                                          
      WTDC = 0.12*WTC                                                           
      CALL SSA                                                                  
      CALL SCO                                                                  
      CALL SCI(NL,NSI,NSO)                                                      
      CALL LINES(2)                                                             
      WRITE (OUT,1010) VOL                                                      
 1010 FORMAT(1H0,5X,3HVOL,F7.0)                                                 
      MAP = ID(IRCD+13)                                                         
      IF (MAP.EQ.0) GO TO 99                                                    
      IDSS = IDS                                                                
      ICONV = 0                                                                 
      AL = D(ID(IRCD+9))                                                        
      IFG = ITLUP(ID(IRCD+11))                                                  
      VSC = VIS(NL,PA,TA)                                                       
      DENI = DEN(NL,D(IP+NSI),D(IT+NSI))                                        
      DENO = DEN(NL,D(IP+NSO),D(IT+NSO))                                        
      PSI = D(IFG+3)*D(IFG+6)*D(IFG+4)/(D(IFG+3)+D(ID(IRCD+10))+0.012)          
      EVP1 = VSC**2*(1.0+PSI**2)/(1029.568*D(IFG+4)**2*DENI)                    
      EVP2 = (DENI/DENO+1.0)/(2.0*D(IFG+4)*(1.0+PSI**2))                        
      IF (AL.NE.0.0) GO TO 200                                                  
      REP = 0.8*D(IW+NL)*D(IFG+4)/(VSC*VOL*PSI)                                 
      EVP = -D(IP+NSI)+D(IP+NSO)                                                
      AL = ALIG                                                                 
      PASS = 1                                                                  
      IFP = 0                                                                   
      ASSIGN 7 TO IS                                                            
      GO TO 300                                                                 
    7 IF (IDS.EQ.0) GO TO 71                                                    
      ASSIGN 71 TO JS                                                           
      GO TO 400                                                                 
   71 CONTINUE                                                                  
      WT = 1.0                                                                  
      DO 15 II = 1,NIT                                                          
      IF (IDS.LT.0) IDS = 2                                                     
      IF (IDSS.LT.0 .AND. II.GT.IABS(IDSS)) IDS = 0                             
      IF (II.EQ.NIT) IDS = 2                                                    
      D(IGA+1) = FLOAT(PASS)                                                    
      PASS = PASS+1                                                             
      IF (ID(ITAD+NWT).NE.0) WT = TLUP(NWT)                                     
      ALS = AL                                                                  
      EVS = EV                                                                  
      AL = ALS*PF                                                               
      ASSIGN 8 TO IS                                                            
      GO TO 300                                                                 
    8 IF (IDS.LE.2) GO TO 81                                                    
      ASSIGN 81 TO JS                                                           
      GO TO 400                                                                 
   81 CONTINUE                                                                  
      IF ((EV-EVS).EQ.0.0) GO TO 16                                             
      AL = ALS-WT*EVS*(AL-ALS)/(EV-EVS)                                         
      IF (AL.GT.ALUL) AL = ALUL                                                 
      IF (AL.LT.ALLL) AL = ALLL                                                 
      ASSIGN 9 TO IS                                                            
      GO TO 300                                                                 
    9 IF (IDS.EQ.0) GO TO 91                                                    
      ASSIGN 91 TO JS                                                           
      GO TO 400                                                                 
   91 CONTINUE                                                                  
      ICONV = 0                                                                 
      IF (ABS(EV).GT.ERL) ICONV = 1                                             
      IF (ICONV.EQ.0) GO TO 150                                                 
   15 CONTINUE                                                                  
      GO TO 200                                                                 
   16 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1009) CERR                                                     
 1009 FORMAT(6H0ERROR,I6,5X,15HSINGULAR MATRIX)                                 
      GO TO 99                                                                  
  300 D(IGA+21) = REP*AL                                                        
      EV = D(IGA+21)**2*EVP1*(AL*TLUP(ID(IRCD+12))*EVP2+DENI/DENO-1.0)          
     * +EVP                                                                     
      GO TO IS, (7,8,9,200)                                                     
  400 CALL LINES(4)                                                             
      WRITE (OUT,1020) PASS                                                     
 1020 FORMAT(6H0PASS ,I6)                                                       
      WRITE (OUT,1021) AL,EV,D(IGA+21)                                          
 1021 FORMAT(6H0S.V. ,E12.5,5X,5HE.V.6,E12.5,5X,3HRE ,E12.5)                    
C1021 FORMAT(6H0S.V. ,E12.5,5X,5HE.V. ,E12.5,5X,3HRE ,E12.5)                    
      GO TO JS, (71,81,91)                                                      
  150 ASSIGN 200 TO IS                                                          
      IFP = 1                                                                   
      GO TO 300                                                                 
   10 REP = 0.8*AL*D(IFG+4)/(VSC*VOL*PSI)                                       
      J = 1                                                                     
      I = 1                                                                     
      GO TO 12                                                                  
   11 IF (J.EQ.2) I = 5                                                         
      IF (J.EQ.3) I = 15                                                        
      IF (J.EQ.4) I = 25                                                        
   12 WP(2*J-1) = I*0.2*D(IW+NL)                                                
      D(IGA+21) = WP(2*J-1)*REP                                                 
      WP(2*J) = D(IGA+21)**2*EVP1*(AL*TLUP(ID(IRCD+12))*EVP2+DENI/DENO          
     * -1.0)*SIG(NL,D(IP+NSI),TA)                                               
      IF (J.EQ.4) GO TO 13                                                      
      J = J+1                                                                   
      GO TO 11                                                                  
   13 IF (MAP.EQ.2) GO TO 20                                                    
      CALL LINES(7)                                                             
      WRITE (OUT,1000)                                                          
 1000 FORMAT(20H0ELECTRIC HEATER MAP)                                           
      WRITE (OUT,1001)                                                          
 1001 FORMAT(6H0    *,40HTABID      1   1   2   3   0  41 0 1   4,40X,1H        
     **)                                                                        
      WRITE (OUT,1002)                                                          
 1002 FORMAT(5X,1H*,24HTABT  ELECTRIC HEATER PD,56X,1H*)                        
      WRITE (OUT,1003) (WP(I),I = 1,8)                                          
 1003 FORMAT(5X,1H*,6HTABV  ,4X,4F10.4,30X,1H*)                                 
      IF (MAP.EQ.1) GO TO 99                                                    
   20 WRITE (PUN,1004)                                                          
 1004 FORMAT(40HTABID      1   1   2   3   0  41 0 1   4,40X)                   
      WRITE (PUN,1005)                                                          
 1005 FORMAT(24HTABT  ELECTRIC HEATER PD,56X)                                   
      WRITE (PUN,1006) (WP(I),I = 1,8)                                          
 1006 FORMAT(6HTABV  ,4X,4F10.4,30X)                                            
      GO TO 99                                                                  
  200 CONTINUE                                                                  
      IDS = IDSS                                                                
      IF (ICONV.NE.0) GO TO 98                                                  
      CALL LINES(2)                                                             
      WRITE (OUT,1011) AL                                                       
 1011 FORMAT(1H0,5X,1HL,F9.2)                                                   
      GO TO 10                                                                  
   98 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1030) CERR                                                     
 1030 FORMAT(6H0ERROR,I6,5X,14HNON CONVERGENT)                                  
   99 CONTINUE                                                                  
      D(IGA+71) = AL                                                            
      D(IGA+72) = VOL                                                           
      RETURN                                                                    
C     EHTRSP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE EHTRSZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL)                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 NSI                                                                       
C   4 NSO                                                                       
C   5 WTF                                                                       
C   6 CUF                                                                       
C   7 RI                                                                        
C   8 DRF                                                                       
C   9 L                                                                         
C  10 IHEL                                                                      
C  11 IFG                                                                       
C  12 IF                                                                        
C  13 MAP                                                                       
      I = IACDB(13)                                                             
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
      ID(I+10) = IPARM(ICV(10))                                                 
      IF (ICV(12).NE.0) ID(I+11) = ITIDN(ICV(11),31)                            
      IF (ICV(12).NE.0) ID(I+12) = ITIDN(ICV(11),32)                            
      ID(I+13) = ICV(12)                                                        
      RETURN                                                                    
C     EHTRSZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      AN EJECTOR                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE EJCTPP                                                         
      COMMON /CC/ C(600)                                                        
      COMMON /DT/ DTES(64001)                                                   
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30))     ,(SCR(1),C(151))                                         
     *, (PASS,C(17)), (IEV,C(49)), (IEVT,C(48)), (IGA,C(35))                    
     * ,(OUT,C( 7)), (IFP,C(22)), (ICPP,C(88))                                  
      DIMENSION SCR(30)                                                         
      INTEGER PASS, OUT                                                         
      EQUIVALENCE  (SCR(1),NLP), (SCR(2),NSIP), (SCR(3),NLS)                    
     *, (SCR(4),NSIS), (SCR(5),NLO), (SCR(6),NSO), (SCR(7),JEVI)                
     *, (SCR(8),GAMMA), (SCR(9),PRCR), (SCR(10),PR), (SCR(11),A)                
     *, (SCR(12),IOP), (SCR(13),TPX), (SCR(14),WCALC)                           
     *, (SCR(15),SPR), (SCR(16),WR), (SCR(17),W), (SCR(18),HS)                  
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 PRIMARY LEG NO                                                            
C   3 PRIMARY STATION NO                                                        
C   4 SECONDARY LEG NO                                                          
C   5 SECONDARY STATION NO                                                      
C   6 OUTLET LEG NO                                                             
C   7 OUTLET STATION NO                                                         
C   8 SECONDARY PRESS RATIO TABLE NO                                            
C   9 OPTION                                                                    
C  10 CD TABLE NO OR PRI TO SEC FLOW RATIO                                      
C  11 THROAT DIA                                                                
C  12 THROAT AREA                                                               
C  13 ERROR VARIABLE INDEX                                                      
      IRCD = IRCDB(13)                                                          
      IOP = ID(IRCD+9)                                                          
      NLP = ID(IRCD+2)                                                          
      NSIP = ID(IRCD+3)                                                         
      NLS = ID(IRCD+4)                                                          
      NSIS = ID(IRCD+5)                                                         
      NLO = ID(IRCD+6)                                                          
      NSO = ID(IRCD+7)                                                          
      D(IGA+41) = D(IW+NLP)                                                     
      D(IGA+42) = D(IW+NLS)                                                     
      JEVI = ID(IRCD+13)                                                        
      GAMMA = GAM(NLP,D(IP+NSIP),D(IT+NSIP))                                    
      PRCR  = ( 2. / (GAMMA +1.0))** (GAMMA / (GAMMA -1.0))                     
      PR = D(IP+NSIS) / D(IP+NSIP)                                              
      D(IGA+24) = 1.0 /PR                                                       
      D(IRCD-6) = D(IGA+24)                                                     
      IF(PR.LE.PRCR.AND.PR.GE.0.0) PR = PRCR                                    
      IF(PR.LT.0.0.OR.PR.GT.1.0) PR = 1.0                                       
      TPX = D(IT+NSIP) * PR ** ((GAMMA -1.0) / GAMMA )                          
      IF(IOP.EQ.1) GO TO 1                                                      
      WCALC = D(IW+NLS) * TLUP(ID(IRCD+10))                                     
      GO TO 2                                                                   
    1 IF(ID(IRCD+12).EQ.0 ) GO TO 3                                             
      A = D(ID(IRCD+12))                                                        
      GO TO 4                                                                   
    3 A = 0.7854 * D(ID(IRCD+11)) ** 2                                          
    4 IF (ID(IRCD+10).NE.0) A = A*TLUP(ID(IRCD+10))                             
      WCALC = 40.124 * A * SQRT(GAMMA/(GAMMA-1.0)                               
     *  * DEN(NLP,D(IP+NSIP),D(IT+NSIP)) * D(IP+NSIP) * ( PR **(2.0/            
     *  GAMMA) - PR ** ((GAMMA +1.0) /GAMMA) ) )                                
    2 D(IGA+34) = SQRT(D(IT+NSIS)/D(IT+NSIP)) * D(IW+NLS)/D(IW+NLP)             
      D(IGA+27) = D(IGA+34) * D(IP+NSIP)/D(IP+NSIS)                             
      SPR = TLUP(ID(IRCD+8))                                                    
      D(IRCD-7) = SPR                                                           
      D(IP+NSO) = D(IP+NSIS)* SPR                                               
      D(IW+NLO) = D(IW+NLS) + D(IW+NLP)                                         
      D(IT+NSO) =(D(IW+NLS) * D(IT+NSIS) + D(IW+NLP)* TPX ) / D(IW+NLO)         
      W = D(IH+NSIP)/(1.0+D(IH+NSIP))*D(IW+NLP)+D(IH+NSIS)/                     
     *(1.0+D(IH+NSIS))*D(IW+NLS)                                                
      D(IH+NSO) = W/(D(IW+NLO)-W)                                               
      WR = D(IW+NLS) / D(IW+NLP)                                                
      D(IRCD-8) = WR                                                            
      IF(PASS.NE.1) GO TO 5                                                     
      ID(IEVT+JEVI) = 1                                                         
    5 D(IEV+JEVI) = D(IW+NLP) - WCALC                                           
      IF(WCALC.NE.0) DTES(IEV+JEVI) = D(IW+NLP)/WCALC                           
      IF(D(IH+NSIP).EQ.0.0.AND.D(IH+NSIS).EQ.0.0) GO TO 98                      
      CALL TDB2(NLP,NSIP,NSIP,HS)                                               
      CALL TDB3(NLS,NSIS,NSIS,HS)                                               
      CALL TDB4(NLO,NSO,NSO,HS)                                                 
   98 IF(IFP.NE.1.OR.ICPP.NE.0) GO TO 99                                        
      CALL PIOP (-1,NLP,NSIP,NSIP)                                              
      CALL PIOP (-2,NLS,NSIS,NSIS)                                              
      CALL PIOP (-2,NLO,NSO,NSO)                                                
      IF(D(IH+NSIP).NE.0.0.AND.D(IH+NSIS).NE.0.0.AND.D(IH+NSO).GT.HS)           
     * CALL HSOP(HS)                                                            
      CALL LINES(2)                                                             
      WRITE (OUT,1000) D(IGA+24),SPR,WR                                         
 1000 FORMAT (1H0,5X,3HPPR,F7.4,3X,3HSPR,F7.4,3X,2HWR,F8.4 )                    
   99 CONTINUE                                                                  
      RETURN                                                                    
C     EJCTPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF AN EJECTOR                                                
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE EJCTPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
     *, (CERR,C(16)), (OUT,C( 7))                                               
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NLP), (SCR(2),NSIP), (SCR(3),NLS)                     
     *, (SCR(4),NSIS), (SCR(5),NLO), (SCR(6),NSO), (SCR(7),NF1)                 
     *, (SCR(8),NF2)                                                            
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      INTEGER CERR,OUT                                                          
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 PRIMARY LEG NO                                                            
C   3 PRIMARY STATION NO                                                        
C   4 SECONDARY LEG NO                                                          
C   5 SECONDARY STATION NO                                                      
C   6 OUTLET LEG NO                                                             
C   7 OUTLET STATION NO                                                         
C   8 SECONDARY PRESS RATIO TABLE NO                                            
C   9 OPTION                                                                    
C  10 CD TABLE NO OR PRI TO SEC FLOW RATIO                                      
C  11 THROAT DIA                                                                
C  12 THROAT AREA                                                               
C  13 ERROR VARIABLE INDEX                                                      
      I = IACDB(13)                                                             
      ID(I+1) = ICV(1)                                                          
      NLP = ILEGN(ICV(2))                                                       
      ID(I+2) = NLP                                                             
      CALL LEGRT(NLP)                                                           
      NSIP = ISTAN(ICV(3))                                                      
      ID(I+3) = NSIP                                                            
      CALL START(NSIP)                                                          
      CALL FTL(NLP,IFTA)                                                        
      CALL FRR(NLP,NF1)                                                         
      NLS = ILEGN(ICV(4))                                                       
      ID(I+4) = NLS                                                             
      CALL LEGRT(NLS)                                                           
      NSIS = ISTAN(ICV(5))                                                      
      ID(I+5) = NSIS                                                            
      CALL START(NSIS)                                                          
      CALL FTL(NLS,IFTA)                                                        
      CALL FRR(NLS,NF2)                                                         
      IF (NF1.EQ.NF2) GO TO 1                                                   
      CERR = CERR + 1                                                           
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR                                                     
 1000 FORMAT(6H0ERROR,I6,5X,22HEJECTOR INVALID FLUIDS)                          
    1 NLO = ILEGN(ICV(6))                                                       
      ID(I+6) = NLO                                                             
      CALL LEGRS(NLO)                                                           
      CALL FRS(NLO,NF1)                                                         
      NSO = ISTAN(ICV(7))                                                       
      ID(I+7) = NSO                                                             
      CALL STARS(NSO)                                                           
      ID(I+8) = ITIDN(ICV(8),4)                                                 
      ID(I+9) = ICV(9)                                                          
      IF(ICV(9).EQ.1) GO TO 2                                                   
      ID(I+10) = ITIDN(ICV(10),19)                                              
      GO TO 3                                                                   
    2 IF (ICV(10).NE.0) ID(I+10) = ITIDN(ICV(10),9)                             
      ID(I+11) = IPARM(ICV(11))                                                 
      ID(I+12) = IPARM(ICV(12))                                                 
    3 ID(I+13) = IAEV(1)                                                        
      RETURN                                                                    
C     EJCTPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      A EJECTOR                                                                
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE EJCTSP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151))                     
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (WTC,C(102))                        
     *, (CUC,C(104)), (RIC,C(106)), (DRC,C(108)), (WTIC,C(109))                 
     *, (WTDC,C(111))                                                           
     *, (IGA,C(35))                                                             
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO), (SCR(4),WTF)         
     *, (SCR(5),CUF), (SCR(6),RI), (SCR(7),DRF), (SCR(8),IDENS)                 
     *, (SCR(9),VOL), (SCR(10),PP), (SCR(11),RHO), (SCR(12),GMA)                
     *, (SCR(13),AREN), (SCR(14),NEM)                                           
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
C   9 THK                                                                       
C  10 DENS                                                                      
      IRCD = IRCDB(10)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      WTF = D(ID(IRCD+5))                                                       
      CUF = D(ID(IRCD+6))                                                       
      RI = D(ID(IRCD+7))                                                        
      DRF = D(ID(IRCD+8))                                                       
      PP = D(IP+NSO)/D(IP+NSI)                                                  
      RHO = DEN(NL,D(IP+NSI),D(IT+NSI))                                         
      GMA = GAM(NL,D(IP+NSI),D(IT+NSI))                                         
      IF (PP.GT.0.5283) GO TO 1                                                 
      AREN = D(IW+NL)/(28.372*SQRT(GMA*D(IP+NSI)*RHO*(2.0/(1.0+GMA))**          
     * ((GMA+1.0)/(GMA-1.0))))                                                  
      GO TO 2                                                                   
    1 AREN = D(IW+NL)/(40.1248*SQRT(GMA/(GMA-1.0)*D(IP+NSI)*RHO*                
     * (PP**(2.0/GMA)-PP**((GMA+1.0)/GMA))))                                    
    2 NEM = AREN/0.0371                                                         
      IF (MOD(NEM,2).NE.0) NEM = NEM+1                                          
      IDENS = ITLUP(ID(IRCD+10))                                                
      WTC = 20.00*(2.5*NEM+2.0)*D(ID(IRCD+9))*D(IDENS+1)/1728.0                 
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      VOL = 38.6*NEM                                                            
      WTIC = 0.205*WTC                                                          
      WTDC = 0.25*WTC                                                           
      RIC = 0.00177                                                             
      IF (RI.NE.0.0) RIC = RI                                                   
      CUC = 17.96+5.36*(WTC)**0.64+0.139*WTC**1.65                              
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      DRC = 1.0                                                                 
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      CALL SSA                                                                  
      CALL SCO                                                                  
      CALL SCI(NL,NSI,NSO)                                                      
      CALL LINES(2)                                                             
      WRITE (OUT,1000) AREN,VOL                                                 
 1000 FORMAT(1H0,5X,2HAN,F8.3,3X,3HVOL,F7.0)                                    
      D(IGA+71) = VOL                                                           
      RETURN                                                                    
C     EJCTSP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE EJCTSZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL)                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 WTF                                                                       
C   6 CUF                                                                       
C   7 RI                                                                        
C   8 DRF                                                                       
C   9 THK                                                                       
C  10 DENS                                                                      
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
      ID(I+10) = ITIDN(ICV(10),20)                                              
      RETURN                                                                    
C     EJCTSZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      AN EVAPORATOR                                                            
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE EVAPPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)),(IW,C(27)),(IP,C(28)),(IT,C(29))                 
     *,(IH,C(30)),(SCR(1),C(151)),(IFB,C(55)),(IGA,C(35))                       
     *,(IEVT,C(48)),(IEV,C(49)),(IFP,C(22)),(ICPP,C(88))                        
     *,(PASS,C(17)),(OUT,C(7)),(GC,C(370))                                      
      INTEGER PASS,OUT                                                          
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (SCR(1),NLR),(SCR(2),LOCT),(SCR(3),NSIR)                      
     *,(SCR(4),NSOR),(SCR(5),NLS),(SCR(6),NSIS),(SCR(7),NSOS)                   
     *,(SCR(8),JEVI),(SCR(9),V),(SCR(10),Q),(SCR(11),TOUT)                      
     *,(SCR(12),EFF),(SCR(13),TAVG),(SCR(14),PD)                                
     *,(SCR(15),HS)                                                             
      EQUIVALENCE (D(50),TE),(D(52),HSV),(D(53),HFG),                           
     *(D(54),HSL),(D(55),QUALIN),(D(56),I1),(D(57),BF),                         
     *(D(58),TAIRO),(D(59),PW),(D(60),HCOIL),(D(61),HDIFF),                     
     *(D(62),CPWV),(D(64),H),(D(65),QUAL),                                      
     *(D(67),VOUT),(D(68),VDGO),(D(69),PDGO),(D(70),VRATIO),                    
     *(D(71),PDLO),(D(72),PDR),(D(73),SLIP),(D(74),PB),                         
     *(D(75),PHI),(D(76),XMOMO),(D(77),XMOMI)                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C*** PRESSURE-DROP FUNCTION FOR TWO-PHASE FLOW BASED ON FAC.INC.,               
C    CORRELATION OF BAROCZY'S METHOD                                            
      DP2PH(X)=-PB*X**2.65/2.65+(1.+PB)*(X**1.7/1.7-(1.-X)**2.3*X               
     *         /2.3 - (1.-X)**3.3/2.3/3.3)                                      
C*** VOID-FRACTION CORRELATION                                                  
      VOIDF(X)=X/(X+SLIP*(1.-X)/VRATIO)                                         
C*** RATIO OF MOMENTUM FLUX TO MOMENTUM OF LIQUID-ONLY FLOW                     
      VMOM(X)=((1.-X)/(1.-VOIDF(X)))**2*(1.-VOIDF(X)*                           
     * (1.-SLIP*SLIP/VRATIO)) - 1.                                              
C   1 COMP CODE                                                                 
C   2 LEG NO R                                                                  
C   3 INLET STATION NO R                                                        
C   4 OUTLET STATION NO R                                                       
C   5 LEG NO S                                                                  
C   6 INLET STATION NO S                                                        
C   7 OUTLET STATION NO S                                                       
C   8 PRESS DROP OPTION                                                         
C   9 PRESS DROP TABLE NO                                                       
C  10 BYPASS-FACTOR TABLE NO                                                    
C  11 DEGRESS SUPERHEAT                                                         
C  12 ERROR VARIABLE INDEX                                                      
C  13 R-SIDE PRESS DROP TABLE(FT OF VAPOR VS CFM, VAPOR FLOW ONLY)              
C  14 FLOW AREA ON REFRIGERANT SIDE (AF) IN SQ.FT.                              
C*** NO ERROR OPTION IF OPTION=0; SUPERHEAT & ERROR IF OPTION=1                 
C  15 ERROR OPTION                                                              
C*** SET UP INTERNAL VARIABLES IN TERMS OF THE STORED ARRAY                     
      IRCD = IRCDB(15)                                                          
      NLR = ID(IRCD+2)                                                          
      NSIR = ID(IRCD+3)                                                         
      NSOR = ID(IRCD+4)                                                         
      NLS  = ID(IRCD+5)                                                         
      NSIS = ID(IRCD+6)                                                         
      NSOS = ID(IRCD+7)                                                         
      LOCT = ID(IFB+NLR)                                                        
      LOCS = ID(IFB+NLS)                                                        
      LOCS = ID(LOCS+1)                                                         
      LOCT = ID(LOCT+3)                                                         
      IOP=ID(IRCD+15)                                                           
C*** DEFINE THREE INDEPENDENT VARIABLES FOR TABLE LOOK-UP                       
      ITER=0                                                                    
      D(IGA+41) = D(IW+NLS)                                                     
      D(IGA+42) = D(IW+NLR)                                                     
      D(IGA+43) = D(IP+NSIS)                                                    
      JEVI = ID(IRCD+12)                                                        
C*** COMPUTE THE SATURATION PROPERTIES                                          
      TE=VTS(LOCT,D(IP+NSIR))                                                   
      V = VSV('EVAPPP 1',LOCT,D(IP+NSIR),TE)                                    
      AF=D(ID(IRCD+14))                                                         
      HSV=VH(LOCT,D(IP+NSIR),TE,V)                                              
      HFG=VHFG(LOCT,D(IP+NSIR),TE,V)                                            
      HSL=HSV-HFG                                                               
      D(IGA+42)=D(IGA+42)*V                                                     
      QUALIN=(D(IH+NSIR)-HSL)/HFG                                               
      IF(QUALIN.GT.1.0)QUALIN=1.0                                               
      IF(QUALIN.LT.0.0)QUALIN=0.0                                               
      I1 = ID(IRCD+11)                                                          
C*** COMPUTE THE TEMPERATURE OF THE SUPERHEATED OUTLET FLOW                     
      TSH = TE + D(I1)                                                          
C*** USE TULP FOR THE SOURCE-SIDE BYPASS FACTOR                                 
      BF = TLUP(ID(IRCD+10))                                                    
C*** COMPUTE THE OUTLET SOURCE TEMPERATURE FROM THE BYPASS FACTOR               
    5 CONTINUE                                                                  
      D(IT+NSOS) = TE- BF * (TE-D(IT+NSIS) )                                    
C*** COMPUTE THE OUTLET HUMIDITY BASED ON AIR/WATER PROPERTIES                  
      PW=EXP(-8083./TE+2.2625*ALOG(TE))                                         
      HCOIL=18.*PW/(29.*(D(IP+NSIS)-PW))                                        
      D(IH+NSOS)=D(IH+NSIS)                                                     
C*** COMPUTE THE DRY-SOURCE ENTHALPY DIFFERENCE                                 
      HDIFF=SHP(NLS,D(IP+NSIS),D(IT+NSIS),D(IH+NSIS))*                          
     * (D(IT+NSIS)-D(IT+NSOS))                                                  
C*** IF THE FLUID IS NOT A GAS, NO DEHUMIDIFICATION OCCURS                      
      IF(LOCS.NE.2) GO TO 10                                                    
C*** IF THE COIL HUMIDITY GT THE INLET HUMIDITY, NO                             
C    DEHUMIFICATION OCCURS                                                      
      IF(D(IH+NSIS).LT.HCOIL) GO TO 10                                          
      D(IH+NSOS)=HCOIL+BF*(D(IH+NSIS)-HCOIL)                                    
C*** USE CP OF WATER VAOPR = 0.42 TO COMPUTE THE VAPOR ENTHALPY                 
      CPWV=0.42                                                                 
C*** ADD THE ENTHALPY DIFFERENCE OF THE WATER REMOVED                           
      HCOND=1075.5-(1.-CPWV)*(TE-491.67)                                        
      HDIFF=HDIFF+(D(IH+NSIS)-D(IH+NSOS))*(HCOND+CPWV*(D(IT+NSOS)-TE))          
   10 CONTINUE                                                                  
C*** COMPUTE THE HEAT TRANSFER BASED ON THE SOURCE-SIDE PERFORMANCE             
      Q=D(IW+NLS)*HDIFF                                                         
C*** COMPUTE THE REFRIGERANT OUTLET ENTHALPY BY AN ENERGY BALANCE               
      H = D(IH+NSIR)+Q/D(IW+NLR)                                                
      D(IH+NSOR)=H                                                              
C*** CHECK THE STATE OF THE REFRIGERANT AT THE OUTLET                           
C    IF H.LE.HSL, IT IS A LIQUID; IF H.GE.HSV, IT IS A VAPOR                    
      IF (H.LE.HSL) GO TO 20                                                    
      IF (H.GE.HSV) GO TO 30                                                    
C*** IN THE TW0-PHASE REGION                                                    
      QUAL=(H-HSL)/HFG                                                          
      TOUT=TE                                                                   
      VOUT=QUAL*V+(1.-QUAL)/VDL(LOCT,TOUT)                                      
      GO TO 40                                                                  
   20 TOUT=VTLIQ(LOCT,D(IP+NSIR),H)                                             
C*** IN THE LIQUID REGION                                                       
      QUAL=0.                                                                   
      VOUT=1./VDL(LOCT,TOUT)                                                    
      GO TO 40                                                                  
C*** IN THE VAPOR REGION                                                        
   30 QUAL=1.                                                                   
      CALL VTAV2(LOCT,TOUT,VOUT,D(IP+NSIR),H)                                   
C*** IF THE REFRIG OUTLET TEMP.LE.SOURCE TEMP, THE PERFORMANCE IS               
C    REASONABLE, SO GO TO 50                                                    
   40 IF(TOUT.LE.D(IT+NSIS))GO TO 50                                            
C*** IF THE BYPASS FACTOR WAS PREVIOUSLY ADJUSTED, PROCEED AS IF                
C    THE REFRIG TEMP IS LOWER THAN THE SOURCE TEMPERATURE                       
      IF(ITER.GT.0) GO TO 50                                                    
C*** RECOMPUTE THE BYPASS FACTOR BECAUSE REFRIG OUTLET TEMP                     
C    IS HIGHER THAN SOURCE TEMPERATURE                                          
      TAVG=TOUT                                                                 
      BF1=1.                                                                    
      ITER=ITER+1                                                               
C*** SET THE REFRIGERANT OUTLET TEMP EQUAL TO THE MAX OF TEVAP, TSOURCE         
      TOUT=D(IT+NSIS)                                                           
      IF(TOUT.LT.TE) TOUT=TE                                                    
      VOUT=VSV('EVAPPP 2',LOCT,D(IP+NSIR),TOUT)                                 
      H=VH(LOCT,D(IP+NSIR),TOUT,VOUT)                                           
C*** COMPUTE THE AMOUNT OF HEAT TRANSFERRED FOR THIS TOUT                       
C    THIS CAN BE USED TO DEDUCE THE EXCESS AMOUNT OF HEAT TRANSFER              
      Q1=D(IW+NLR)*(H-D(IH+NSIR))                                               
C*** ADJUST THE BYPASS FACTOR TO GIVE TOUT JUST 1% HIGHER THAN TSOURCE          
      BF1=BF+0.99*(1.-Q1/Q)*(1.-BF)                                             
   12 WRITE(OUT,1103)TAVG,D(IT+NSIS),BF,BF1                                     
 1103 FORMAT(' *+* WARNING IN EVAPP - REFRIGERANT OUTLET TEMP ',F10.3,          
     */,' *+* GREATER THAN SOURCE TEMPERATURE   ',F10.3/                        
     *' *+* BYPASS FACTOR OF',F10.3,' RESET TO ',F10.3/,'   TO MAKE ',          
     *'TOUT=TSOURCE+0.01*(TOUT-TSOURCE)'/)                                      
      BF=BF1                                                                    
C*** WITH THE INCREASED BYPASS FACTOR, RECOMPUTE THE PERFORMANCE                
      GO TO 5                                                                   
C*** COMPUTE THE PRESSURE DROPS FOR LIQUID ONLY AND VAPOR ONLY                  
   50 PDGO=TLUP(ID(IRCD+13))                                                    
      VRATIO=V*VDL(LOCT,TE)                                                     
C*** ASSUME THE SINGLE-PHASE PRESSURE DROP IS PROPORTIONAL TO                   
C    THE SPECIFIC VOLUME                                                        
      PDLO=PDGO/VRATIO                                                          
      PDR=PDLO                                                                  
      IF (QUAL.LE.0.) GO TO 80                                                  
      SLIP=(VRATIO-1.)**0.25                                                    
      PB=-0.25*ALOG10(VRATIO)                                                   
      IF (PB.LT.-1.) PB=-1.                                                     
      QDUM=QUALIN                                                               
C*** COMPUTE THE AVERAGE PHI,BUT WATCH FOR TOO SMALL                            
C    A DIFFERENCE                                                               
      IF(QUALIN.EQ.QUAL)QDUM=ABS(QUAL-.01)                                      
      PHI=(DP2PH(QUAL)-DP2PH(QDUM))/(QUAL-QDUM)                                 
C*** COMPUTE THE FRICTION PRESSURE DROP, IGNORING THE PRESSURE                  
C    DROP IN THE SINGLE-PHASE REGIONS                                           
      PDR=PDLO+PHI*(PDGO-PDLO)                                                  
C*** COMPUTE THE MOMENTUM CHANGES (PRESSURE DROP)                               
      XMOMI=VRATIO                                                              
      IF (QUALIN.LT.1.) XMOMI=VMOM(QUALIN)                                      
      XMOMO=VRATIO                                                              
      IF (QUAL.LT.1.) XMOMO=VMOM(QUAL)                                          
C*** ADD THE MOMENTUM AND FRICTION PRESSURE DROPS                               
C$                                                                              
C$ IS THE LINE BELOW MISSING A DIVISOR OF 2,I.E. "ONE-HALF-RHO-VEE**2"          
C$ OR G**2/(2-G-RHO)                                                            
C$        NEEDS FIXING                                                          
      PDR=PDR+(XMOMO-XMOMI)*(D(IW+NLR)/AF/60.)**2/(VDL(LOCT,TE)*                
     * GC*144.)                                                                 
   80 D(IP+NSOR)=D(IP+NSIR)-PDR                                                 
C*** IF THE OUTLET PRESSURE IS NEGATIVE, SET DP TO ZERO                         
      IF(D(IP+NSOR).GT.0.0)GO TO 82                                             
C     WRITE(OUT,7655)D(IP+NSOR),D(IP+NSIR)                                      
 7655 FORMAT(' *+* OUTLET PRESSURE IN EVAP WAS ',E14.7,                         
     * ' RESET PRESSURE TO INLET VALUE ',E14.7)                                 
      D(IP+NSOR)=D(IP+NSIR)                                                     
   82 CONTINUE                                                                  
      IF(IOP.EQ.0)GO TO 83                                                      
      IF(PASS.NE.1) GO TO 1                                                     
      ID(IEVT+JEVI) = 3                                                         
C*** COMPUTE THE ERROR VARIABLE AS THE SUPERHEAT DISCREPANCY                    
C$WAS    1 D(IEV+JEVI)=(D(IH+NSOR)-HSV)/VCPV(LOCT,D(IP+NSIR),TE)-D(I1)          
C$IS  (USE OUTLET PRESSURE AND V TO GET SUPERHEAT, NOT INLET CONDITIONS)        
      VOUT  = VSV('EVAPPP1B',LOCT,D(IP+NSOR),TE)                                
      HSVOUT=VH(LOCT,D(IP+NSOR),TE,VOUT)                                        
    1 D(IEV+JEVI)=(D(IH+NSOR)-HSVOUT)/VCPV(LOCT,D(IP+NSOR),TE)-D(I1)            
C$                                                                              
   83 TAVG = 0.5 * (D(IT+NSIS) + D(IT+NSOS) )                                   
C*** COMPUTE THE SOURCE-SIDE PRESSURE DROP                                      
      D(IT+NSOR)=TOUT                                                           
      PD = TLUP(ID(IRCD+9))                                                     
      IF(ID(IRCD+8).NE.0) PD=PD/SIG(NLS,D(IP+NSIS),D(IT+NSIS))                  
      D(IP+NSOS) = D(IP+NSIS) - PD                                              
      IF(IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                      
      CALL PIOP(1,NLR,NSIR,NSOR)                                                
      CALL PIOP(2,NLS,NSIS,NSOS)                                                
   85 CALL LINES(2)                                                             
      WRITE(OUT,1001) BF,Q                                                      
 1001 FORMAT(1H0,5X,14H BYPASS FACTOR,F7.4,3X,1HQ,F9.2  )                       
   99 CONTINUE                                                                  
      RETURN                                                                    
C     EVAPPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF EVAPORATOR                                                
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE EVAPPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      EQUIVALENCE (SCR(1),NLR), (SCR(2),NSIR), (SCR(3),NSOR)                    
     *, (SCR(4),NLS), (SCR(5),NSIS), (SCR(6),NSOS)                              
      DIMENSION ICV(18), SCR(30)                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/100/, IFTB/011/                                                 
C   1 COMP CODE                                                                 
C   2 LEG NO R                                                                  
C   3 INLET STATION NO R                                                        
C   4 OUTLET STATION NO R                                                       
C   5 LEG NO S                                                                  
C   6 INLET STATION NO S                                                        
C   7 OUTLET STATION NO S                                                       
C   8 PRESS DROUP OPTION                                                        
C   9 PRESS DROP TABLE NO                                                       
C   10 BYPASS FACTOR                                                            
C  11 DEGRESS SUPERHEAT                                                         
C  12 ERROR VARIABLE INDEX                                                      
C  13 R SIDE PRESSURE DROP TABLE (PSI VAPOR ONLY VS FLOW)                       
C  14 FLOW AREA ON REFRIG SIDE (AF) SQUARE FEET                                 
C  15 ERROR VARIABLE OPTION                                                     
      I = IACDB(15)                                                             
      ID(I+1) = ICV(1)                                                          
      NLR = ILEGN(ICV(2))                                                       
      ID(I+2) = NLR                                                             
      CALL LEGRT(NLR)                                                           
      NSIR = ISTAN(ICV(3))                                                      
      ID(I+3) = NSIR                                                            
      CALL START(NSIR)                                                          
      NSOR = ISTAN(ICV(4))                                                      
      ID(I+4) = NSOR                                                            
      CALL STARS(NSOR)                                                          
      CALL FTL(NLR,IFTA)                                                        
      NLS = ILEGN(ICV(5))                                                       
      ID(I+5) = NLS                                                             
      CALL LEGRT(NLS)                                                           
      NSIS = ISTAN(ICV(6))                                                      
      ID(I+6) = NSIS                                                            
      CALL START(NSIS)                                                          
      NSOS = ISTAN(ICV(7))                                                      
      ID(I+7) = NSOS                                                            
      CALL STARS(NSOS)                                                          
      CALL FTL(NLS,IFTB)                                                        
      ID(I+8) = ICV(8)                                                          
      ID(I+9) = ITIDN(ICV(9),1)                                                 
      ID(I+10) = ITIDN(ICV(10),5)                                               
      ID(I+11) = IPARM(ICV(11))                                                 
      ID(I+15)=ICV(14)                                                          
      IF(ICV(14).EQ.0) GO TO 10                                                 
      ID(I+12) = IAEV(3)                                                        
   10 ID(I+13)=ITIDN(ICV(12),2)                                                 
      ID(I+14)=IPARM(ICV(13))                                                   
   99 CONTINUE                                                                  
      RETURN                                                                    
C     EVAPPZ                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C        A FAN                                                                  
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE FANSP                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151)), (IW,C(27))         
     *, (IP,C(28)), (IT,C(29)), (WTC,C(102)), (CUC,C(104))                      
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))                
     *, (IGA,C(35)), (HPE,C(112)), (HPH,C(114))                                 
     *, (IFP,C(22)), (IDS,C(5)), (PASS,C(17)), (NIT,C(76)), (PF,C(77))          
     *, (NWT,C(78)), (ITAD,C(54)), (ICONV,C(79)), (LN,C(125))                   
     *, (LUT,C(127)), (CERR,C(16)), (IH,C(30))                                  
      DIMENSION SCR(30)                                                         
      INTEGER OUT,CERR,PASS                                                     
      EQUIVALENCE         (SCR( 1),NLF   ), (SCR( 2),NLT   )                    
     *, (SCR( 3),NSIF  ), (SCR( 4),NSIT  ), (SCR( 5),NSOF  )                    
     *, (SCR( 6),NSOT  ), (SCR( 7),DT    ), (SCR( 8),HP    )                    
     *, (SCR( 9),VOL   ), (SCR(10),SN    ), (SCR(11),ED    )                    
     *, (SCR(12),IDT   ), (SCR(13),NST   ), (SCR(14),WTF   )                    
     *, (SCR(15),CUF   ), (SCR(16),RI    ), (SCR(17),DRF   )                    
     *, (SCR(18),HPD   ), (SCR(19),EF    ), (SCR(20),GAMMA )                    
     *, (SCR(21),IND   ), (SCR(22),DV    ), (SCR(23),TOUT  )                    
     *, (SCR(24),GM    ), (SCR(25),CP    ), (SCR(26),PR    )                    
     *, (SCR(27),Q     ), (SCR(28),H     ), (SCR(29),IDSS  )                    
     *, (SCR(30),NS    )                                                        
      REAL NS,NSS,NSIG,NSLL,NSUL,LUT,LN                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DIMENSION TABE(4), TABN(4)                                                
      DATA NSLL/1.0/, NSUL/1000.0/, NSIG/40.0 /, ERL/0.001/                     
      DATA TABE/0.98,1.0,1.02,1.04/, TABN/5500.0,7500.0,11500.0,22000.0/        
C   1 COMP CODE                                                                 
C   2 NLF                                                                       
C   3 NSIF                                                                      
C   4 NSOF                                                                      
C   5 NST                                                                       
C   6 WTM                                                                       
C   7 CUM                                                                       
C   8 RI                                                                        
C   9 DRM                                                                       
C  10 DIA                                                                       
C  11 IDT                                                                       
C  12 NLT                                                                       
C  13 NSIT                                                                      
C  14 NSOT                                                                      
C  15 NDET                                                                      
C  16 NDHT                                                                      
      IRCD = IRCDB(16)                                                          
      NLF = ID(IRCD+2)                                                          
      NSIF = ID(IRCD+3)                                                         
      NSOF = ID(IRCD+4)                                                         
      NST = ID(IRCD+5)                                                          
      GAMMA = GAM(NLF,D(IP+NSIF),D(IT+NSIF))                                    
      EF = D(IT+NSIF)*((D(IP+NSOF)/D(IP+NSIF))**((GAMMA-1.0)/GAMMA)-1.0)        
     */(D(IT+NSOF)-D(IT+NSIF))                                                  
      HP = D(IW+NLF) * SHP(NLF,D(IP+NSIF),D(IT+NSOF),D(IH+NSIF))                
     * * (D(IT+NSOF) - D(IT+NSIF)) * 0.02356                                    
      WTF = D(ID(IRCD+6))                                                       
      DT = D(ID(IRCD+10))                                                       
      SN = D(IGA+80+NST)                                                        
      ICONV = 0                                                                 
      IDSS = IDS                                                                
      IDT = ID(IRCD+11)                                                         
      IF (IDT.EQ.6) GO TO 1                                                     
      IF (DT.NE.0.0) GO TO 23                                                   
      DT = 16.8*(D(IW+NLF)/(SN*DEN(NLF,D(IP+NSIF),D(IT+NSIF))))**.333333        
   23 GO TO (99,2,2,4), IDT                                                     
    2 WTC = 0.26*DT**2                                                          
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      WTDC = 0.25*WTC                                                           
      RIC = 0.00546                                                             
      VOL = 1.3*DT**3                                                           
      IF (IDT.EQ.2) GO TO 21                                                    
      CUC = 4.87+0.76*WTC                                                       
c@      CALL MDSCT(2,IND,ED,TABE,SN,TABN,4,1,DV,DV,DV)                            
      CALL MDSCT(2,IND,ED,TABE,SN,TABN,4,1,DV,DV,IDV,Idummy)                            
      GO TO 22                                                                  
   21 CUC = 14.29+0.46*WTC                                                      
      CALL MDSCT(2,IND,ED,TABE,SN,TABN,4,0,DV,DV,IDV,Idummy)                            
   22 ED = (1.0-0.281/HP**0.169)*ED                                             
      HPD = HP/ED                                                               
      HPE = HPE+HPD                                                             
      GO TO 10                                                                  
    4 WTC = 0.178*DT**2                                                         
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      WTDC = 0.109*WTC                                                          
      RIC = 0.01162                                                             
      CUC = 53.587+1.725*WTC                                                    
      VOL = 1.3*DT**3                                                           
      ED = 0.82                                                                 
      HPD = HP/ED                                                               
      HPH = HPH+HPD                                                             
      GO TO 10                                                                  
    1 NLT = ID(IRCD+12)                                                         
      NSIT = ID(IRCD+13)                                                        
      NSOT = ID(IRCD+14)                                                        
      TOUT = TDBI(NLT,NSIT,NSOT)                                                
      GAMMA = GAM(NLT,D(IP+NSIT),D(IT+NSIT))                                    
      GM = (GAMMA-1.0)/GAMMA                                                    
      CP = SHP(NLT,D(IP+NSIT),0.5*(D(IT+NSIT)+TOUT),D(IH+NSIT))                 
      PR = D(IP+NSOT)/D(IP+NSIT)                                                
      ED = (D(IT+NSIT)-TOUT)/(D(IT+NSIT)*(1.0-PR**GM))                          
      HPD = 0.02356*D(IW+NLT)*CP*(D(IT+NSIT)-TOUT)                              
      IF (DT.NE.0.0) GO TO 205                                                  
      H = 778.0*CP*D(IT+NSIT)*(1.0-PR**GM)                                      
      Q = SQRT(D(IW+NLT)/(60.0*DEN(NLT,D(IP+NSOT),D(IT+NSOT))))                 
      IFP = 0                                                                   
      D(IGA+60) = ED                                                            
      NS = NSIG                                                                 
      PASS = 1                                                                  
      ASSIGN 7 TO IS                                                            
      GO TO 300                                                                 
    7 IF (IDS.EQ.0) GO TO 71                                                    
      ASSIGN 71 TO JS                                                           
      GO TO 400                                                                 
   71 CONTINUE                                                                  
      WT = 1.0                                                                  
      DO 15 II = 1,NIT                                                          
      IF (IDS.LT.0) IDS = 2                                                     
      IF (IDSS.LT.0 .AND. II.GT.IABS(IDSS)) IDS = 0                             
      IF (II.EQ.NIT) IDS = 2                                                    
      D(IGA+1) = FLOAT(PASS)                                                    
      PASS = PASS+1                                                             
      IF (ID(ITAD+NWT).NE.0) WT = TLUP(NWT)                                     
      NSS = NS                                                                  
      EVS = EV                                                                  
      NS = NSS*PF                                                               
      ASSIGN 8 TO IS                                                            
      GO TO 300                                                                 
    8 IF (IDS.LE.2) GO TO 81                                                    
      ASSIGN 81 TO JS                                                           
      GO TO 400                                                                 
   81 CONTINUE                                                                  
      IF ((EV-EVS).EQ.0.0) GO TO 16                                             
      NS = NSS-WT*EVS*(NS-NSS)/(EV-EVS)                                         
      IF (NS.GT.NSUL) NS = NSUL                                                 
      IF (NS.LT.NSLL) NS = NSLL                                                 
      ASSIGN 9 TO IS                                                            
      GO TO 300                                                                 
    9 IF (IDS.EQ.0) GO TO 91                                                    
      ASSIGN 91 TO JS                                                           
      GO TO 400                                                                 
   91 CONTINUE                                                                  
      ICONV = 0                                                                 
      IF (ABS(EV).GT.ERL) ICONV = 1                                             
      IF (ICONV.EQ.0) GO TO 150                                                 
   15 CONTINUE                                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1030) CERR                                                     
 1030 FORMAT(6H0ERROR,I6,5X,14HNON CONVERGENT)                                  
      GO TO 13                                                                  
   16 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1009) CERR                                                     
 1009 FORMAT(6H0ERROR,I6,5X,15HSINGULAR MATRIX)                                 
      ICONV = 1                                                                 
   13 WTC = 0.0                                                                 
      CUC = 0.0                                                                 
      RIC = 0.0                                                                 
      DRC = 0.0                                                                 
      GO TO 201                                                                 
  400 CALL LINES(4)                                                             
      WRITE (OUT,1020) PASS                                                     
 1020 FORMAT(6H0PASS ,I6)                                                       
      WRITE (OUT,1021) NS,EV                                                    
 1021 FORMAT(6H0S.V. ,E12.5,5X,5HE.V.5,E12.5)                                   
C1021 FORMAT(6H0S.V. ,E12.5,5X,5HE.V. ,E12.5)                                   
      GO TO JS, (71,81,91)                                                      
 300  D(IGA+59) = NS                                                            
      DT = TLUP(ID(IRCD+15))*Q/H**0.25                                          
      EV = DT*(1.0-2.0*TLUP(ID(IRCD+16)))-1.4*(D(IW+NLF)*Q/(H**0.75*NS*         
     *DEN(NLF,D(IP+NSIF),D(IT+NSIF))))**.333333                                 
      DT = 12.0*DT                                                              
      GO TO IS, (7,8,9,205)                                                     
  150 ASSIGN 205 TO IS                                                          
      IFP = 1                                                                   
      GO TO 300                                                                 
  205 WTC = 0.109*DT**2                                                         
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      WTDC = 0.199*WTC                                                          
      CUC = 5.5*WTC                                                             
      RIC = 0.00707                                                             
      VOL = .8666667*DT**3                                                      
      IF (SN.NE.0.0) GO TO 10                                                   
      SN = H**0.75*NS/Q                                                         
      IF (SN.LE.LN) GO TO 11                                                    
      CALL LINES(2)                                                             
      WRITE (OUT,1005)                                                          
 1005 FORMAT(1H0,5X,34HSHAFT SPEED EXCEEDS LIMITING VALUE)                      
   11 IF (0.004363*DT*SN .LE. LUT) GO TO 10                                     
      CALL LINES(2)                                                             
      WRITE (OUT,1015)                                                          
 1015 FORMAT(1H0,5X,40HTURBINE TIP SPEED EXCEEDS LIMITING VALUE)                
   10 WTIC = 0.205*WTC                                                          
      CUF = D(ID(IRCD+7))                                                       
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      RI = D(ID(IRCD+8))                                                        
      IF (RI.NE.0.0) RIC = RI                                                   
      DRC = 1.0                                                                 
      DRF = D(ID(IRCD+9))                                                       
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      CALL SSA                                                                  
  201 CALL SCO                                                                  
      IDS = IDSS                                                                
      CALL SCI(NLF,NSIF,NSOF)                                                   
      IF (ICONV.NE.0) GO TO 99                                                  
      XINCFM = D(IW+NLF) / DEN(NLF,D(IP+NSIF),D(IT+NSIF))                        
      VTIP=DT/12.*3.14159 * SN/60.
      CALL LINES(2)                                                             
      WRITE (OUT,1000) DT,VOL,SN,EF,HP, XINCFM, VTIP                                          
 1000 FORMAT(1H0,5X,1HD,F9.2,3X,3HVOL,F7.0,3X,1HN,F9.0,3X,4HFEFF,F7.4,2X,        
     *3HSHP,F8.2, 2X, 3HCFM, F8.1, 2X, 4HVTIP, F7.1)                                                                
      CALL LINES(2)                                                             
      WRITE (OUT,1010) IDT,ED,HPD                                               
 1010 FORMAT(1H0,5X,6HDRIVE I4,3X,4HDEFF,F7.4,2X,3HDHP,F8.2)                      
      IF (IDT.EQ.6) CALL SCI(NLT,NSIT,NSOT)                                     
      D(IGA+71) = DT                                                            
      D(IGA+72) = VOL                                                           
   99 CONTINUE                                                                  
      RETURN                                                                    
C     FANSP                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE FANSZ                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
     *, (SCR(1),C(151)), (IGA,C(35))                                            
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER OUT,CERR                                                          
      EQUIVALENCE (SCR(1),NLF), (SCR(2),NLT)                                    
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 NLF                                                                       
C   3 NSIF                                                                      
C   4 NSOF                                                                      
C   5 NST                                                                       
C   6 WTM                                                                       
C   7 CUM                                                                       
C   8 RI                                                                        
C   9 DRM                                                                       
C  10 DIA                                                                       
C  11 IDT                                                                       
C  12 NLT                                                                       
C  13 NSIT                                                                      
C  14 NSOT                                                                      
C  15 NDET                                                                      
C  16 NDHT                                                                      
      I = IACDB(16)                                                             
      ID(I+1) = ICV(1)                                                          
      NLF = ILEGN(ICV(2))                                                       
      ID(I+2) = NLF                                                             
      ID(I+3) = ISTAN(ICV(3))                                                   
      ID(I+4) = ISTAN(ICV(4))                                                   
      CALL FTL(NLF,IFTA)                                                        
      ID(I+5) = ICV(5)                                                          
      CALL SSR(ICV(5))                                                          
      ID(I+6) = IPARM(ICV(6))                                                   
      ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      ID(I+10) = IPARM(ICV(10))                                                 
      ID(I+11) = ICV(11)                                                        
      IF (ICV(11).GE.2 .AND. ICV(11).LE.4 .OR. ICV(11).EQ.6) GO TO 20           
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1010) CERR,ICV(11)                                             
 1010 FORMAT(6H0ERROR,I6,5X,18HINVALID DRIVE TYPE)                              
   20 IF (ICV(11).NE.6) GO TO 30                                                
      NLT = ILEGN(ICV(12))                                                      
      ID(I+12) = NLT                                                            
      ID(I+13) = ISTAN(ICV(13))                                                 
      ID(I+14) = ISTAN(ICV(14))                                                 
      CALL FTL(NLT,IFTA)                                                        
      ID(I+15) = ITIDN(ICV(15),37)                                              
      ID(I+16) = ITIDN(ICV(15),38)                                              
      IF (D(IGA+80+ICV(5)).EQ.0.0 .AND. ICV(10).EQ.0) GO TO 99                  
      IF (D(IGA+80+ICV(5)).NE.0.0 .AND. ICV(10).NE.0) GO TO 99                  
      CERR = CERR+1                                                             
      WRITE (OUT,1020) CERR                                                     
 1020 FORMAT(6H0ERROR,I6,5X,27HDIAMETER OR SPEED SPECIFIED)                     
      GO TO 99                                                                  
   30 IF (D(IGA+80+ICV(5)).NE.0.0) GO TO 99                                     
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR                                                     
 1000 FORMAT(6H0ERROR,I6,5X,25HSHAFT SPEED NOT SPECIFIED)                       
   99 CONTINUE                                                                  
      RETURN                                                                    
C     FANSZ                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C$   WAS CALLED  SUBROUTINE HXAPP (COPIED FROM FORT(AECSCRAY)                   
C$   NEED IMPROVED VERSION TO RUN SINK MODELS...                                
C$   OLD HXAPPH DOES NOT COMPUTE GEN ARG 55 AND 56 FOR NTU,CRATIO LOOKUP        
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE HXAPPH                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)), (SCR(1),C(151)), (ICPP,C(88)), (OUT,C(7))                   
     *, (IFP,C(22)), (IGA,C(35)), (ILN,C(37)), (PASS,C(17))                     
     *, (CERR,C(16)), (IIOP,C(90))                                              
     *, (IFB,C(55))                                                             
      DIMENSION SCR(30)                                                         
      INTEGER PASS,OUT,CERR                                                     
      REAL MUR1,MUR2,MUAVG                                                      
      EQUIVALENCE  (SCR(1),NL1), (SCR(2),NSI1), (SCR(3),NSO1)                   
     *, (SCR(4),NL2), (SCR(5),NSI2), (SCR(6),NSO2), (SCR(7),IOP)                
     *, (SCR(8),PD1), (SCR(9),PD2), (SCR(10),EFF1), (SCR(11),ETAHA1)            
     *, (SCR(12),ETAHA2), (SCR(13),CH), (SCR(14),CC), (SCR(15),EFF2)            
     *, (SCR(16),TAVG1), (SCR(17),HS1), (SCR(18),HS2), (SCR(19),IFPS)           
     *, (SCR(20),II), (SCR(21),IJ), (SCR(22),TAVG2), (SCR(23),ITER)             
     *, (SCR(24),MUR1), (SCR(25),MUR2), (SCR(26),MUAVG)                         
     *, (SCR(27),SHRH), (SCR(28),SHRC), (SCR(29),TDAH), (SCR(30),TDAC)          
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C$    WRITE (6,5999)                                                            
C5999 FORMAT(' ','ENTERING SUBROUTINE HXAPP')                                   
C   1 COMP CODE                                                                 
C   2 LEG 1                                                                     
C   3 STATION IN 1                                                              
C   4 STATION OUT 1                                                             
C   5 LEG 2                                                                     
C   6 STATION IN 2                                                              
C   7 STATION OUT 2                                                             
C   8 PRESS. DROP OPTION                                                        
C   9 PRESS. DROP TABLE NO. 1                                                   
C  10 PRESS. DROP TABLE NO. 2                                                   
C  11 EFFEC. TABLE NO.                                                          
C  12 ETAHA TABLE NO 1                                                          
C  13 REF TEMP-SIDE 1                                                           
C  14 ETAHA TABLE NO 2                                                          
C  15 REF TEMP-SIDE 2                                                           
C$ ADD MULT FACTORS FOR HA1 AND HA2                                             
C  16 MULTIPLIER FOR ETAHA TABLE NO 1                                           
C  17 MULTIPLIER FOR ETAHA TABLE NO 2                                           
      IRCD = IRCDB(17)                                                          
      NL1  = ID(IRCD+2)                                                         
      NSI1 = ID(IRCD+3)                                                         
      NSO1 = ID(IRCD+4)                                                         
      NL2  = ID(IRCD+5)                                                         
      NSI2 = ID(IRCD+6)                                                         
      NSO2 = ID(IRCD+7)                                                         
      IFL1 = ID(IFB+NL1)                                                        
      IF (ID(IFL1+1).EQ.1 .AND. ID(ID(IFL1+3)-1).EQ.12) IFL1 = 0                
      IFL2 = ID(IFB+NL2)                                                        
      IF (ID(IFL2+1).EQ.1 .AND. ID(ID(IFL2+3)-1).EQ.12) IFL2 = 0                
      D(IGA+43) = D(IP+NSI1)                                                    
      D(IGA+44) = D(IP+NSI2)                                                    
      D(IH+NSO1) = D(IH+NSI1)                                                   
      D(IH+NSO2) = D(IH+NSI2)                                                   
      IF (PASS.NE.1) GO TO 9                                                    
      SHRH = 1.0                                                                
      SHRC = 1.0                                                                
      GO TO 10                                                                  
    9 SHRH = D(IRCD-8)                                                          
      SHRC = D(IRCD-9)                                                          
   10 IOP = ID(IRCD+8)                                                          
      IF (D(IH+NSI1).EQ.0.0 .AND. D(IH+NSI2).EQ.0.0) GO TO 24                   
      II = 0                                                                    
      ITER = 0                                                                  
      IFPS = IFP                                                                
      IFP = 0                                                                   
      IDCH = 0                                                                  
      IDCC = 0                                                                  
      GO TO 25                                                                  
   24 II = -1                                                                   
   25 IF (ID(IRCD+12).NE.0) GO TO 3                                             
      ASSIGN 21 TO IS                                                           
   21 D(IGA+41) = D(IW+NL1)*SHRH                                                
      D(IGA+42) = D(IW+NL2)*SHRC                                                
      EFF1 = TLUP(ID(IRCD+11))                                                  
      D(IGA+41) = D(IW+NL1)                                                     
      D(IGA+42) = D(IW+NL2)                                                     
      D(IT+NSO1) = D(IT+NSI1)-EFF1*(D(IT+NSI1)-D(IT+NSI2))                      
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9100                                              
      WRITE (6,7999)                                                            
 7999 FORMAT(' WRITING FROM SUBROUTINE HXAPP')                                  
      WRITE (6,5998) D(IT+NSO1)                                                 
 5998 FORMAT(' ','D(IT+NSO1) PREV TO 18 = DIT1-EFF1*DIT1-DIT2  '                
     *            ,E16.8)                                                       
      WRITE (6,5997) D(IT+NSI1),EFF1,D(IT+NSI2)                                 
 5997 FORMAT(' ','D(IT+NSI1)=',E16.8,'  EFF1=',E16.8,'  D(IT+NSI2)='            
     *            ,E16.8)                                                       
      WRITE (6,5996) D(IT+NSO1)                                                 
 5996 FORMAT(' ','D(IT+NSO1)=',E16.8)                                           
 9100 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      TAVG1 = 0.5 * (D(IT+NSI1) + D(IT+NSO1))                                   
C     CH = MDOT*CP                                                              
      CH = D(IW+NL1) * SHP(NL1,D(IP+NSI1),TAVG1,D(IH+NSI1))                     
      PD1 = TLUP(ID(IRCD+9))                                                    
      IF (ID(IFL1+1).EQ.1) GO TO 18                                             
      Q = CH*(D(IT+NSO1)-D(IT+NSI1))*SHRH                                       
      GO TO 29                                                                  
   18 Q = D(IW+NL1)*(HFT(NL1,D(IP+NSI1),D(IT+NSO1))-HFT(NL1,D(IP+NSI1),         
     *D(IT+NSI1)))                                                              
   29 IF (ID(IFL2+1).EQ.1) GO TO 20                                             
      EFFP = EFF1*CH*SHRH/SHRC                                                  
      TAN2 = D(IT+NSI2)                                                         
      DO 26 IJ=1,4                                                              
      TAVG2 = TAN2                                                              
      CC = D(IW+NL2) * SHP(NL2,D(IP+NSI2),TAVG2,D(IH+NSI2))                     
      EFF2 = EFFP/CC                                                            
      D(IT+NSO2) = D(IT+NSI2)+EFF2*(D(IT+NSI1)-D(IT+NSI2))                      
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9099                                              
      WRITE (6,5994) Q, EFFP, CC                                                
 5994 FORMAT(' ','  Q       =',E16.8,'  EFFP1  =',E16.8,'       CC   ='         
     *            ,E16.8)                                                       
      WRITE (6,5993) EFF2, CH, SHRH                                             
 5993 FORMAT(' ','  EFF2    =',E16.8,'    CH   =',E16.8,'     SHRH   ='         
     *            ,E16.8)                                                       
      WRITE (6,5992) SHRC, D(IW+NL1), D(IW+NL2)                                 
 5992 FORMAT(' ','  SHRC    =',E16.8,'  W LEG1 =',E16.8,'    WLEG2   ='         
     *            ,E16.8)                                                       
      WRITE (6,5991) D(IT+NSO2)                                                 
 5991 FORMAT(' ','D(IT+NSO2) = DITNSI2+ EFF2*DITNSI1 -DITNSI2='                 
     *            ,E16.8)                                                       
CXXXXX                                                                          
      WRITE (6,3651) NL1, D(IP+NSI1), TAVG1                                     
 3651 FORMAT(' ','  NL1    =',I4,12X,' D(IPNSI1=',E16.8,'    TAVG1   ='         
     *            ,E16.8)                                                       
      SHPXX1=SHP(NL1,D(IP+NSI1),TAVG1,D(IH+NSI1))                               
      WRITE (6,3650) D(IH+NSI1), SHPXX1                                         
 3650 FORMAT(' ',' D(IHNSI1)=',E16.8,'  SHPXX1 =',E16.8)                        
CXXXXX                                                                          
      WRITE (6,3751) NL2, D(IP+NSI2), TAVG2                                     
 3751 FORMAT(' ','  NL2    =',I4,12X,' D(IPNSI2)=',E16.8,'   TAVG2   ='         
     *            ,E16.8)                                                       
      SHPXX2=SHP(NL2,D(IP+NSI2),TAVG2,D(IH+NSI2))                               
      WRITE (6,3750) D(IH+NSI2), SHPXX2, TAN2                                   
 3750 FORMAT(' ',' D(IH+NSI2=',E16.8,'  SHPXX2 =',E16.8,'   TAN2    ='          
     *            ,E16.8)                                                       
 9099 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      TAN2 = 0.5*(D(IT+NSI2)+D(IT+NSO2))                                        
      IF (ABS(TAN2-TAVG2).LT.1.0) GO TO 27                                      
   26 CONTINUE                                                                  
   27 CONTINUE                                                                  
      TAVG2 = TAN2                                                              
      GO TO 19                                                                  
   20 HOUT = HFT(NL2,D(IP+NSI2),D(IT+NSI2))-Q/D(IW+NL2)                         
      D(IT+NSO2) = TFH(NL2,D(IP+NSI2),HOUT)                                     
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9098                                              
      WRITE (6,5984) D(IT+NSO2)                                                 
 5984 FORMAT(' ','D(IT+NSO2)=THF(NL2,DIP+NSI2,HOUT)=',E16.8)                    
 9098 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      TAVG2 = 0.5*(D(IT+NSI2)+D(IT+NSO2))                                       
      CC = D(IW+NL2) * SHP(NL2,D(IP+NSI2),TAVG2,D(IH+NSI2))                     
   19 PD2 = TLUP(ID(IRCD+10))                                                   
      EFF2 = (D(IT+NSO2)-D(IT+NSI2))/(D(IT+NSI1)-D(IT+NSI2))                    
      GO TO 7                                                                   
    3 IJ = 0                                                                    
      ASSIGN 4 TO IS                                                            
      TR1 = D(ID(IRCD+13))                                                      
      IF (ID(IRCD+13).EQ.0) TR1 = 0.0                                           
      TR2 = D(ID(IRCD+15))                                                      
      IF (ID(IRCD+15).EQ.0) TR2 = 0.0                                           
      IF (TR1.NE.0.0) MUR1 = VIS(NL1,D(IP+NSI1),TR1)                            
      IF (TR2.NE.0.0) MUR2 = VIS(NL2,D(IP+NSI2),TR2)                            
    4 TAN1 = D(IT+NSI1)                                                         
      TAN2 = D(IT+NSI2)                                                         
   16 TAVG1 = TAN1                                                              
      TAVG2 = TAN2                                                              
      MUAVG = VIS(NL1,D(IP+NSI1),TAVG1)                                         
      D(IGA+41) = D(IW+NL1)*SHRH                                                
      IF (TR1.NE.0.0) D(IGA+41) = D(IGA+41)*MUR1/MUAVG                          
      D(IGA+45) = TAVG1                                                         
      ETAHA1 = TLUP(ID(IRCD+12))                                                
C$                                                                              
      BHA1 = D(ID(IRCD+16))                                                     
      BHA2 = D(ID(IRCD+17))                                                     
      IF (BHA1 .NE. 0.0) ETAHA1 = BHA1 * TLUP(ID(IRCD+12))                      
      IF (TR1.NE.0.0) ETAHA1 = ETAHA1*(MUAVG/MUR1)*                             
     *(MUR1*COND(NL1,D(IP+NSI1),TAVG1)/(MUAVG*COND(NL1,D(IP+NSI1),              
     *TR1)))**.6666667*                                                         
     *(SHP(NL1,D(IP+NSI1),TAVG1,D(IH+NSI1))/SHP(NL1,D(IP+NSI1),                 
     *TR1,D(IH+NSI1)))**.3333333                                                
      MUAVG = VIS(NL2,D(IP+NSI2),TAVG2)                                         
      D(IGA+42) = D(IW+NL2)*SHRC                                                
      IF (TR2.NE.0.0) D(IGA+42) = D(IGA+42)*MUR2/MUAVG                          
      D(IGA+46) = TAVG2                                                         
      ETAHA2 = TLUP(ID(IRCD+14))                                                
      IF (BHA2 .NE. 0.0) ETAHA2 = BHA2 * TLUP(ID(IRCD+14))                      
      IF (TR2.NE.0.0) ETAHA2 = ETAHA2*(MUAVG/MUR2)*                             
     *(MUR2*COND(NL2,D(IP+NSI2),TAVG2)/(MUAVG*COND(NL2,D(IP+NSI2),              
     *TR2)))**.6666667*                                                         
     *(SHP(NL2,D(IP+NSI2),TAVG2,D(IH+NSI2))/SHP(NL2,D(IP+NSI2),                 
     *TR2,D(IH+NSI2)))**.3333333                                                
      CH = D(IW+NL1) * SHP(NL1,D(IP+NSI1),TAVG1,D(IH+NSI1)) * SHRH              
      CC = D(IW+NL2) * SHP(NL2,D(IP+NSI2),TAVG2,D(IH+NSI2)) * SHRC              
      IF (CH.GT.CC) GO TO 5                                                     
      D(IGA+56) = CH/CC                                                         
      D(IGA+55) = ETAHA1*ETAHA2/(ETAHA1+ETAHA2)/CH                              
      EFF1 = TLUP(ID(IRCD+11))                                                  
      D(IT+NSO1) = D(IT+NSI1)-EFF1*(D(IT+NSI1)-D(IT+NSI2))                      
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9097                                              
      WRITE (6,5983) D(IT+NSO1)                                                 
 5983 FORMAT(' ','5983D(IT+NSO1)=',E16.8)                                       
 9097 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF (ID(IFL1+1).EQ.1) GO TO 31                                             
      Q = CH*(D(IT+NSO1)-D(IT+NSI1))                                            
      GO TO 32                                                                  
   31 Q = D(IW+NL1)*(HFT(NL1,D(IP+NSI1),D(IT+NSO1))-HFT(NL1,D(IP+NSI1),         
     *D(IT+NSI1)))                                                              
   32 IF (ID(IFL2+1).EQ.1) GO TO 33                                             
      D(IT+NSO2) = D(IT+NSI2)-Q/CC                                              
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9096                                              
      WRITE (6,5982) D(IT+NSO2)                                                 
 5982 FORMAT(' ','5982D(IT+NSO2)=D(IT+NSI2)-Q/CC',E16.8)                        
 9096 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      GO TO 34                                                                  
   33 HOUT = HFT(NL2,D(IP+NSI2),D(IT+NSI2))-Q/D(IW+NL2)                         
      D(IT+NSO2) = TFH(NL2,D(IP+NSI2),HOUT)                                     
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9095                                              
      WRITE (6,5981) D(IT+NSO2)                                                 
 5981 FORMAT(' ','5981D(IT+NSO2)=TFH(NL2,DIPNSI2,HOUT)=',E16.8)                 
 9095 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
   34 EFF2 = (D(IT+NSO2)-D(IT+NSI2))/(D(IT+NSI1)-D(IT+NSI2))                    
      GO TO 6                                                                   
    5 D(IGA+56) = CC/CH                                                         
      D(IGA+55) = ETAHA1*ETAHA2/(ETAHA1+ETAHA2)/CC                              
      EFF2 = TLUP(ID(IRCD+11))                                                  
      D(IT+NSO2) = D(IT+NSI2) + EFF2 * (D(IT+NSI1) - D(IT+NSI2))                
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9094                                              
      WRITE (6,4990) D(IT+NSI2),EFF2,D(IT+NSI1)                                 
 4990 FORMAT(' ','DIT+NSI2=',E16.8,'EFF2=',E16.8,'    DIT+NSI1=',E16.8)         
      WRITE (6,5980) D(IT+NSO2)                                                 
 5980 FORMAT(' ','5980D(IT+NSO2)=DIT+EFF2*DITNSI1-DITNSI2=',E16.8)              
 9094 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF (ID(IFL2+1).EQ.1) GO TO 35                                             
      Q = CC*(D(IT+NSI2)-D(IT+NSO2))                                            
      GO TO 36                                                                  
   35 Q = D(IW+NL2)*(HFT(NL2,D(IP+NSI2),D(IT+NSI2))-HFT(NL2,D(IP+NSI2),         
     * D(IT+NSO2)))                                                             
   36 IF (ID(IFL1+1).EQ.1) GO TO 37                                             
      D(IT+NSO1) = D(IT+NSI1)+Q/CH                                              
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9093                                              
      WRITE (6,4989) Q,CH                                                       
 4989 FORMAT(' ','Q=',E16.8,'CH=',E16.8)                                        
      WRITE (6,5979) D(IT+NSO1)                                                 
 5979 FORMAT(' ','5979D(IT+NSO1)=DITNSI+Q/CH=',E16.8)                           
 9093 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      GO TO 38                                                                  
   37 HOUT = HFT(NL1,D(IP+NSI1),D(IT+NSI1))+Q/D(IW+NL1)                         
      D(IT+NSO1) = TFH(NL1,D(IP+NSI1),HOUT)                                     
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=0                                                                   
      IF (IRITE .NE. 1) GO TO 9092                                              
      WRITE (6,5978) D(IT+NSO1)                                                 
 5978 FORMAT(' ','5978D(IT+NSO1)=TFH(NL1,DIPNSI1,HOUT)',E16.8)                  
 9092 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C$             DONT DIVIDE BY 0                                                 
   38 DT= ABS(D(IT+NSI1)-D(IT+NSI2))                                            
      IF( DT .LT. 0.0001) EFF1=0.99999                                          
      IF( DT .LT. 0.0001) GO TO 6                                               
      EFF1 = (D(IT+NSI1)-D(IT+NSO1))/(D(IT+NSI1)-D(IT+NSI2))                    
    6 TAN1 = 0.5*(D(IT+NSI1)+D(IT+NSO1))                                        
      TAN2 = 0.5*(D(IT+NSI2)+D(IT+NSO2))                                        
      IF (ABS(TAN1-TAVG1).LT.1.0 .AND. ABS(TAN2-TAVG2).LT.1.0) GO TO 28         
      IJ = IJ+1                                                                 
      IF (IJ.LT.4) GO TO 16                                                     
   28 CONTINUE                                                                  
      TAVG1 = TAN1                                                              
      TAVG2 = TAN2                                                              
      IJ = 0                                                                    
      D(IGA+41) = D(IW+NL1)                                                     
      IF (TR1.NE.0.0) D(IGA+41) = D(IGA+41)*MUR1/VIS(NL1,D(IP+NSI1),            
     *TAVG1)                                                                    
      IF (TR1.NE.0.0) D(IGA+41) = D(IGA+41)*MUR1/VIS(NL1,D(IP+NSI1),            
     *TAVG1)                                                                    
      PD1 = TLUP(ID(IRCD+9))                                                    
      IF (TR1.NE.0.0) PD1 = PD1*(D(IW+NL1)/D(IGA+41))**2                        
      D(IGA+42) = D(IW+NL2)                                                     
      IF (TR2.NE.0.0) D(IGA+42) = D(IGA+42)*MUR2/VIS(NL2,D(IP+NSI2),            
     *TAVG2)                                                                    
      PD2 = TLUP(ID(IRCD+10))                                                   
      IF (TR2.NE.0.0) PD2 = PD2*(D(IW+NL2)/D(IGA+42))**2                        
    7 CONTINUE                                                                  
      IF (IOP.NE.1 .AND. IOP.NE.3) GO TO 2                                      
      PD1 = PD1 / SIG(NL1,D(IP+NSI1),TAVG1)                                     
    2 IF (IOP.NE.2 .AND. IOP.NE.3) GO TO 1                                      
      PD2 = PD2 / SIG(NL2,D(IP+NSI2),TAVG2)                                     
    1 D(IP+NSO1) = D(IP+NSI1) - PD1                                             
      D(IP+NSO2) = D(IP+NSI2) - PD2                                             
      IF (II.EQ.-1) GO TO 8                                                     
C     CALL TDA(NL1,NSI1,NSO1,TDAH)                                              
C     CALL TDA(NL2,NSI2,NSO2,TDAC)                                              
      CALL TDAHS(NL1,NSI1,NSO1,TDAH)                                            
      CALL TDAHS(NL2,NSI2,NSO2,TDAC)                                            
      IF (D(IT+NSI1).EQ.D(IT+NSO1)) GO TO 12                                    
      SHRHN = (D(IT+NSI1)-TDAH) / (D(IT+NSI1)-D(IT+NSO1))                       
      GO TO 13                                                                  
   12 SHRHN = 1.0                                                               
   13 IF (D(IT+NSI2).EQ.D(IT+NSO2)) GO TO 14                                    
      SHRCN = (D(IT+NSI2)-TDAC) / (D(IT+NSI2)-D(IT+NSO2))                       
      GO TO 15                                                                  
   14 SHRCN = 1.0                                                               
   15 IF (SHRHN-SHRH) 41,42,43                                                  
   41 IDCHN = -1                                                                
      GO TO 44                                                                  
   42 IDCHN = 0                                                                 
      GO TO 44                                                                  
   43 IDCHN = 1                                                                 
   44 IF (SHRCN-SHRC) 45,46,47                                                  
   45 IDCCN = -1                                                                
      GO TO 48                                                                  
   46 IDCCN = 0                                                                 
      GO TO 48                                                                  
   47 IDCCN = 1                                                                 
   48 IDC = 0                                                                   
      IF (IDCHN.NE.IDCH .OR. IDCCN.NE.IDCC) IDC = 1                             
      IDCH = IDCHN                                                              
      IDCC = IDCCN                                                              
      IF (ABS(SHRHN-SHRH).LE.5.E-5 .AND. ABS(SHRCN-SHRC).LE.5.E-5)              
     * GO TO 23                                                                 
      ITER = ITER+1                                                             
      IF (ITER.GT.20) GO TO 22                                                  
      IF (IDC.NE.0) GO TO 30                                                    
      SHRH = SHRHN                                                              
      SHRC = SHRCN                                                              
      GO TO IS, (21,4)                                                          
   30 SHRH = 0.5*(SHRH+SHRHN)                                                   
      SHRC = 0.5*(SHRC+SHRCN)                                                   
      GO TO IS, (21,4)                                                          
   22 IF (IFP.EQ.0) GO TO 23                                                    
      CALL LINES(2)                                                             
      WRITE (OUT,1013) ID(ILN+NL1),ID(ILN+NL2)                                  
 1013 FORMAT(1H0,5X,35H***NON-CONVERGENCE SHR - HXA - LEGS,2I6)                 
   23 IF (II.NE.0) GO TO 8                                                      
      IF (IFPS.EQ.0) GO TO 8                                                    
      II = II+1                                                                 
      IFP = IFPS                                                                
      GO TO IS, (21,4)                                                          
    8 D(IRCD-6) = EFF1                                                          
      D(IRCD-7) = EFF2                                                          
      IF (IIOP.NE.0) GO TO 90                                                   
      D(IRCD-8) = SHRH                                                          
      D(IRCD-9) = SHRC                                                          
   90 IF (IFP.NE.1) GO TO 99                                                    
      IF (EFF1.GE.0.0 .AND. EFF1.LE.1.0) GO TO 11                               
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1011) CERR                                                     
 1011 FORMAT(6H0ERROR,I6,5X,21HINVALID EFFECTIVENESS)                           
      GO TO 97                                                                  
   11 IF (EFF2.GE.0.0 .AND. EFF2.LE.1.0) GO TO 98                               
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1011) CERR                                                     
      GO TO 97                                                                  
   98 CONTINUE                                                                  
      IF (ICPP.NE.0) GO TO 99                                                   
   97 CONTINUE                                                                  
      CALL PIOP(1,NL1,NSI1,NSO1)                                                
      IF (D(IH+NSI1).EQ.0.0) GO TO 96                                           
      CALL TDB1(NL1,NSO1,NSO1,HS1)                                              
      IF (D(IH+NSO1).GT.HS1) CALL HSOP(HS1)                                     
   96 CONTINUE                                                                  
      CALL PIOP(2,NL2,NSI2,NSO2)                                                
      IF (D(IH+NSI2).EQ.0.0) GO TO 95                                           
      CALL TDB1(NL2,NSO2,NSO2,HS2)                                              
      IF (D(IH+NSO2).GT.HS2) CALL HSOP(HS2)                                     
   95 CONTINUE                                                                  
      CALL LINES(2)                                                             
      WRITE (OUT,1001) EFF1,EFF2,SHRH,SHRC,Q                                    
 1001 FORMAT(1H0,5X,3HEF1,F7.4,3X,3HEF2,F7.4,3X,3HSR1,F7.4,3X,3HSR2,F7.4        
     *,3X,1HQ,F10.1)                                                             
   99 CONTINUE                                                                  
C$    WRITE(6,4890)                                                             
C4890 FORMAT(' ','LEAVING SUBROUTINE HXAPPP')                                   
      RETURN                                                                    
C$C     HXAPP                                                                   
C     HXAPPH                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A HEAT EXCHANGER WITH BOTH INLET AND OUTLET               
C      FLOW DEFINED                                                             
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE HXAPZ                                                          
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7)),                                                   
     *            (CERR,C(16)),                                                 
     *            (ICV(1),C(133)),                                              
     *            (SCR(1),C(151))                                               
      INTEGER OUT                                                               
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE  (SCR(1),NL1),                                                
     *             (SCR(2),NSI1),                                               
     *             (SCR(3),NSO1),                                               
     *             (SCR(4),NL2),                                                
     *             (SCR(5),NSI2),                                               
     *             (SCR(6),NSO2)                                                
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/011/                                                            
                                                                                
C   1 COMP CODE                                                                 
C   2 LEG 1                                                                     
C   3 STATION IN 1                                                              
C   4 STATION OUT 1                                                             
C   5 LEG 2                                                                     
C   6 STATION IN 2                                                              
C   7 STATION OUT 2                                                             
C   8 PRESS. DROP OPTION                                                        
C   9 PRESS. DROP TABLE NO. 1                                                   
C  10 PRESS. DROP TABLE NO. 2                                                   
C  11 EFFEC. TABLE NO.                                                          
C  12 ETAHA TABLE NO 1                                                          
C  13 REF TEMP-SIDE 1                                                           
C  14 ETAHA TABLE NO 2                                                          
C  15 REF TEMP-SIDE 2                                                           
C$ ADD MULT FACTORS FOR HA1 AND HA2                                             
C  16 MULTIPLIER FOR ETAHA TABLE NO 1                                           
C  17 MULTIPLIER FOR ETAHA TABLE NO 2                                           
      I = IACDB(17)                                                             
      ID(I+1) = ICV(1)                                                          
      NL1 = ILEGN(ICV(2))                                                       
      ID(I+2) = NL1                                                             
      CALL LEGRT(NL1)                                                           
      NSI1 = ISTAN(ICV(3))                                                      
      ID(I+3) = NSI1                                                            
      CALL START(NSI1)                                                          
      NSO1 = ISTAN(ICV(4))                                                      
      ID(I+4) = NSO1                                                            
      CALL STARS(NSO1)                                                          
      CALL FTL(NL1,IFTA)                                                        
      NL2  = ILEGN(ICV(5))                                                      
      ID(I+5) = NL2                                                             
      CALL LEGRT(NL2)                                                           
      NSI2 = ISTAN(ICV(6))                                                      
      ID(I+6) = NSI2                                                            
      CALL START(NSI2)                                                          
      NSO2 = ISTAN(ICV(7))                                                      
      ID(I+7) = NSO2                                                            
      CALL STARS(NSO2)                                                          
      CALL FTL(NL2,IFTA)                                                        
      ID(I+8) = ICV(8)                                                          
      ID(I+9) = ITIDN(ICV(9),1)                                                 
      ID(I+10) = ITIDN(ICV(10),1)                                               
      ID(I+11) = ITIDN(ICV(11),5)                                               
      IF (ICV(12).EQ.0 .AND. ICV(13).EQ.0 .AND. ICV(14).EQ.0 .AND.              
     * ICV(15).EQ.0 ) GO TO 99                                                  
      IF (ICV(12).NE.0 .AND. ICV(14).NE.0) GO TO 1                              
      CERR = CERR + 1                                                           
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR                                                     
 1000 FORMAT (6H0ERROR,I6,5X,20HETAHA DATA INCORRECT )                          
      GO TO 99                                                                  
    1 ID(I+12) = ITIDN(ICV(12),22)                                              
      ID(I+13) = IPARM(ICV(13))                                                 
      ID(I+14) = ITIDN(ICV(14),22)                                              
      ID(I+15) = IPARM(ICV(15))                                                 
C$ ADD ETHA MULT FACTORS                                                        
      ID(I+16) = IPARM(ICV(16))                                                 
      ID(I+17) = IPARM(ICV(17))                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     HXAPZ                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE HXB1PP                                                         
      ENTRY HXB2PP                                                              
      RETURN                                                                    
C     HXB1PP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE HXB1PZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (CERR,C(16)), (OUT,C(7))                                      
      INTEGER CERR, OUT                                                         
      ENTRY HXB2PZ                                                              
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR                                                     
 1000 FORMAT(6H0ERROR,I6,5X,28HHXB DELETED USE LOOP AND HXA)                    
      RETURN                                                                    
C     HXB1PZ                                                                    
      END                                                                       
C                                                                       00010001
C********************************************************************** 00020001
C                                                                       00030001
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                   00040001
C      A HEAT EXCHANGER                                                 00050001
C                                                                       00060001
C$    ASSUME MUCH MODIFIED... ADD WRITES FOR MORE SIZING DETAILS        00061001
C********************************************************************** 00070001
C                                                                       00080001
      SUBROUTINE HXSP                                                   03570000
      COMMON /CC/ C(600)                                                03580000
      EQUIVALENCE (IDS,C(5)), (PASS,C(17)), (IGA,C(35))                 03590000
     *, (ISV,C(47)), (IEV,C(49)), (NSV,C(50)), (NEV,C(51))              03600000
     *, (IMP,C(66)), (IME,C(67)), (NIT,C(76)), (PF,C(77)), (NWT,C(78))  03610000
     *, (ITAD,C(54)), (ICONV,C(79)), (OUT,C(7)), (CERR,C(16))           03620000
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (IFB,C(55)), (WTC,C(102))   03630000
     *, (CUC,C(104)), (RIC,C(106)), (DRC,C(108)), (WTIC,C(109))         03640000
     *, (WTDC,C(111)), (SCR(1),C(151)), (IRCD,C(45))                    03650000
     *, (ICV(1),C(133)), (PUN,C(91)), (ILN,C(37)), (IFP,C(22))          03660000
     *, (IH,C(30))                                                      03670000
      DIMENSION SCR(30), ICV(18)                                        03680000
      INTEGER OUT, CERR, PASS, PUN                                      03690000
      EQUIVALENCE         (ICV( 1),NLH   ), (ICV( 2),NLC   )            03700000
     *, (ICV( 3),NSIH  ), (ICV( 4),NSIC  ), (ICV( 5),NSOH  )            03710000
     *, (ICV( 6),NSOC  ), (ICV( 7),IHC   ), (ICV( 8),LH    )            03720000
     *, (ICV( 9),LC    ), (ICV(10),LN    ), (ICV(11),NPH   )            03730000
     *, (ICV(12),NPC   ), (ICV(13),IMH   ), (ICV(14),IMC   )            03740000
     *, (ICV(15),IFGH  ), (ICV(16),IFGC  ), (ICV(17),PSIH  )            03750000
     *, (ICV(18),PSIC  )                                                03760000
      EQUIVALENCE         (SCR( 1),REH   ), (SCR( 2),REC   )            03770000
     *, (SCR( 3),DENHI ), (SCR( 4),DENHO ), (SCR( 5),DENCI )            03780000
     *, (SCR( 6),DENCO ), (SCR( 7),TAH   ), (SCR( 8),TAC   )            03790000
     *, (SCR( 9),PAH   ), (SCR(10),PAC   ), (SCR(11),VISH  )            03800000
     *, (SCR(12),VISC  ), (SCR(13),CPH   ), (SCR(14),CPC   )            03810000
     *, (SCR(15),PRH   ), (SCR(16),PRC   ), (SCR(17),WCPH  )            03820000
     *, (SCR(18),WCPC  ), (SCR(19),WCP   ), (SCR(20),TSP   )            03830000
     *, (SCR(21),ISVS  ), (SCR(22),IEVS  ), (SCR(23),IEM   )            03840000
     *, (SCR(24),IDSS  ), (SCR(25),DSV   ), (SCR(26),PAR   )            03850000
     *, (SCR(27),REHP  ), (SCR(28),RECP  ), (SCR(29),EFFP  )            03860000
     *, (SCR(30),WT    )                                                03870000
      COMMON /DC/ DZ(2),D(128001)                                       03880002
      DIMENSION ID(128001)                                              03890002
      EQUIVALENCE (ID(1),D(1))                                          03900002
      REAL LH, LC, LN                                                   03910000
      REAL NH, NC                                                       03920000
c was                              ERL1/0.001/                          03930000
      DATA SVLL/0.1/, SVUL/400.0/, ERL1/0.0001/, ERL2/0.0001/            03930000
C$WAS*, SVIGL/6.0/, SVIGG/12.0/                                         03940000
CTHEN         0.6         1.2                                           03950000
     *, SVIGL/2.0/, SVIGG/4.0/                                          03960000
C   1 COMP CODE                                                         03970000
C   2 WTF                                                               03980000
C   3 CUF                                                               03990000
C   4 RI                                                                04000000
C   5 DRF                                                               04010000
C   6 LN                                                                04020000
C   7 IMSP                                                              04030000
C   8 ITSP                                                              04040000
C   9 IEF                                                               04050000
C  10 MAP                                                               04060000
C  11 IFLT                                                              04070000
C  12 IHC                                                               04080000
C  13 NL1                                                               04090000
C  14 NSI1                                                              04100000
C  15 NSO1                                                              04110000
C  16 L1                                                                04120000
C  17 NP1                                                               04130000
C  18 IM1                                                               04140000
C  19 IFG1                                                              04150000
C  20 IF1                                                               04160000
C  21 IJ1                                                               04170000
C  22 NL2                                                               04180000
C  23 NSI2                                                              04190000
C  24 NSO2                                                              04200000
C  25 L2                                                                04210000
C  26 NP2                                                               04220000
C  27 IM2                                                               04230000
C  28 IFG2                                                              04240000
C  29 IF2                                                               04250000
C  30 IJ2                                                               04260000
      IRCD = IRCDB(30)                                                  04270000
      IHC = ID(IRCD+12)                                                 04280000
      IF (IHC.EQ.2) GO TO 32                                            04290000
      IRDH = IRCD+12                                                    04300000
      IRDC = IRCD+21                                                    04310000
      GO TO 31                                                          04320000
   32 IRDH = IRCD+21                                                    04330000
      IRDC = IRCD+12                                                    04340000
   31 NLH = ID(IRDH+1)                                                  04350000
      NLC = ID(IRDC+1)                                                  04360000
      NSIH = ID(IRDH+2)                                                 04370000
      NSIC = ID(IRDC+2)                                                 04380000
      NSOH = ID(IRDH+3)                                                 04390000
      NSOC = ID(IRDC+3)                                                 04400000
      LH = D(ID(IRDH+4))                                                04410000
      LC = D(ID(IRDC+4))                                                04420000
      NPH = ID(IRDH+5)                                                  04430000
      NPC = ID(IRDC+5)                                                  04440000
      IMH = ITLUP(ID(IRDH+6))                                           04450000
      IMC = ITLUP(ID(IRDC+6))                                           04460000
      IFGH = ITLUP(ID(IRDH+7))                                          04470000
      IFGC = ITLUP(ID(IRDC+7))                                          04480000
      LN = D(ID(IRCD+6))                                                04490000
      SHRH = (D(IT+NSIH)-TDBI(NLH,NSIH,NSOH))/(D(IT+NSIH)-D(IT+NSOH))   04500000
      SHRC = (D(IT+NSIC)-TDBI(NLC,NSIC,NSOC))/(D(IT+NSIC)-D(IT+NSOC))   04510000
      TSP = TLUP(ID(IRCD+8))                                            04520000
      PAH = 0.5*(D(IP+NSIH)+D(IP+NSOH))                                 04530000
      PAC = 0.5*(D(IP+NSIC)+D(IP+NSOC))                                 04540000
      TAH = 0.5*(D(IT+NSIH)+D(IT+NSOH))                                 04550000
      TAC = 0.5*(D(IT+NSIC)+D(IT+NSOC))                                 04560000
C     WRITE (OUT,4000) D(IT+NSIH),D(IT+NSOH),D(IT+NSIC),D(IT+NSOC)      04570000
 4000 FORMAT (1H0, 4F10.2)                                              04580000
      DENHI = DEN(NLH,D(IP+NSIH),D(IT+NSIH))                            04590000
      DENHO = DEN(NLH,D(IP+NSOH),D(IT+NSOH))                            04600000
      DENCI = DEN(NLC,D(IP+NSIC),D(IT+NSIC))                            04610000
      DENCO = DEN(NLC,D(IP+NSOC),D(IT+NSOC))                            04620000
      CPH = SHP(NLH,PAH,TAH,D(IH+NSIH))                                 04630000
     * *SHRH                                                            04640000
      CPC = SHP(NLC,PAC,TAC,D(IH+NSIC))                                 04650000
     * *SHRC                                                            04660000
      VISH = VIS(NLH,PAH,TAH)                                           04670000
      VISC = VIS(NLC,PAC,TAC)                                           04680000
      PRH = (3600.0*VISH*CPH/COND(NLH,PAH,TAH))**.6666667               04690000
      PRC = (3600.0*VISC*CPC/COND(NLC,PAC,TAC))**.6666667               04700000
      EV1P1 = VISH**2/(1029.568*D(IFGH+4)**2*DENHI)                     04710000
      EV2P1 = VISC**2/(1029.568*D(IFGC+4)**2*DENCI)                     04720000
      EV1P2 = (DENHI/DENHO+1.0)*NPH/(2.0*D(IFGH+4))                     04730000
      EV2P2 = (DENCI/DENCO+1.0)*NPC/(2.0*D(IFGC+4))                     04740000
      EHP = CPH*VISH/(D(IFGH+4)*D(IFGH+5)*PRH*                          04750000
     *POLYI(2,D(IMH+5),TAH))*1800.0                                     04760000
      ECP = CPC*VISC/(D(IFGC+4)*D(IFGC+5)*PRC*                          04770000
     *POLYI(2,D(IMC+5),TAC))*1800.0                                     04780000
      IF (D(IFGH+8).NE.3.0) GO TO 301                                   04790000
      EFLH = SQRT(D(IFGH+3)**2+1.0/D(IFGH+2)**2)/2.0                    04800000
      GO TO 304                                                         04810000
  301 EFLH = D(IFGH+3)/2.0                                              04820000
  304 IF (D(IFGC+8).NE.3.0) GO TO 302                                   04830000
      EFLC = SQRT(D(IFGC+3)**2+1.0/D(IFGC+2)**2)/2.0                    04840000
      GO TO 303                                                         04850000
  302 EFLC = D(IFGC+3)/2.0                                              04860000
  303 CONTINUE                                                          04870000
      ICONV = 0                                                         04880000
      IDSS = IDS                                                        04890000
      IF (LN.NE.0.0) GO TO 200                                          04900000
      REHP = 0.8*D(IFGH+4)*D(IW+NLH)*NPH/VISH                           04910000
      RECP = 0.8*D(IFGC+4)*D(IW+NLC)*NPC/VISC                           04920000
      WCPH = D(IW+NLH)*CPH                                              04930000
      WCPC = D(IW+NLC)*CPC                                              04940000
      IF (WCPH.GT.WCPC) GO TO 13                                        04950000
   12 WCP = WCPH                                                        04960000
      EFFP = (D(IT+NSIH)-D(IT+NSOH))/(D(IT+NSIH)-D(IT+NSIC))            04970000
      GO TO 24                                                          04980000
   13 WCP = WCPC                                                        04990000
      EFFP = (D(IT+NSOC)-D(IT+NSIC))/(D(IT+NSIH)-D(IT+NSIC))            05000000
   24 IFLT = ID(IRCD+11)                                                05010000
      GO TO (41,41,42,43), IFLT                                         05020000
   41 IF (WCPH.GT.WCPC) GO TO 45                                        05030000
   44 D(IGA+56) = WCPH/WCPC                                             05040000
      GO TO 50                                                          05050000
   45 D(IGA+56) = WCPC/WCPH                                             05060000
      GO TO 50                                                          05070000
   42 IF (IHC.EQ.2) GO TO 45                                            05080000
      GO TO 44                                                          05090000
   43 IF (IHC.EQ.2) GO TO 44                                            05100000
      GO TO 45                                                          05110000
   50 PASS = 1                                                          05120000
      IFP = 0                                                           05130000
      NSV = 3                                                           05140000
      NEV = 3                                                           05150000
      NUM = 9                                                           05160000
      CALL GDCU(NSV,4,2,D,ISV)                                          05170000
      CALL GDCU(NEV,4,2,D,IEV)                                          05180000
      CALL GDCU(NSV,4,2,D,ISVS)                                         05190000
      CALL GDCU(NEV,4,2,D,IEVS)                                         05200000
      CALL GDCU(NUM,4,2,D,IMP)                                          05210000
      CALL GDCU(NSV,4,2,D,IME)                                          05220000
      CALL GDCU(NSV,4,2,D,IEM)                                          05230000
      SVIG = SVIGG                                                      05240000
      IF (ID(ID(IFB+NLH)+1).EQ.1) SVIG = SVIGL                          05250000
      IF (ID(ID(IFB+NLC)+1).EQ.1) SVIG = SVIGL                          05260000
      D(ISV+1) = SVIG                                                   05270000
      D(ISV+2) = SVIG                                                   05280000
      D(ISV+3) = SVIG                                                   05290000
      ASSIGN 4 TO JS                                                    05300000
      GO TO 300                                                         05310000
    4 IF (IDS.EQ.0) GO TO 1                                             05320000
      ASSIGN 1 TO IS                                                    05330000
      GO TO  101                                                        05340000
    1 CONTINUE                                                          05350000
      WT = 1.0                                                          05360000
      DO 10 II=1,NIT                                                    05370000
      IF (IDS.LT.0) IDS = 2                                             05380000
      IF (IDSS.LT.0 .AND. II.GT.IABS(IDSS)) IDS = 0                     05390000
      IF (II.EQ.NIT) IDS = 2                                            05400000
      D(IGA+1) = FLOAT(PASS)                                            05410000
      PASS = PASS+1                                                     05420000
       IF (ID(ITAD+NWT).NE.0) WT = TLUP(NWT)                            05430000
      DO 2 I=1,NSV                                                      05440000
      D(ISVS+I) = D(ISV+I)                                              05450000
    2 D(IEVS+I) = D(IEV+I)                                              05460000
      DO 11 J=1,NSV                                                     05470000
      D(ISV+J) = D(ISVS+J)*PF                                           05480000
      DSV = D(ISV+J)-D(ISVS+J)                                          05490000
      ASSIGN 5 TO JS                                                    05500000
      GO TO 300                                                         05510000
    5 DO 14 K=1,NEV                                                     05520000
      PAR = (D(IEV+K)-D(IEVS+K))/DSV                                    05530000
      CALL MSTO(K,J,PAR)                                                05540000
   14 CONTINUE                                                          05550000
      CALL MSTO(J,0,D(IEVS+J))                                          05560000
      ASSIGN 21 TO IS                                                   05570000
      IF (IDS.EQ.3) GO TO 101                                           05580000
   21 CONTINUE                                                          05590000
      D(ISV+J) = D(ISVS+J)                                              05600000
   11 CONTINUE                                                          05610000
      IF (IDS.GE.2) CALL MPRNT(D(IMP+1),NSV,D(IME+1))                   05620000
      CALL MCHK(D(IMP+1),NSV,D(IME+1))                                  05630000
      CALL MSOL(D(IMP+1),NSV,D(IME+1),D(IEM+1),IND)                     05640000
      IF (IND.NE.1) GO TO 40                                            05650000
      DO 16 J=1,NSV                                                     05660000
      D(ISV+J) = D(ISV+J)+WT*D(IMP+J)                                   05670000
      IF (D(ISV+J).GT.SVUL) D(ISV+J) = SVUL                             05680000
      IF (D(ISV+J).LT.SVLL) D(ISV+J) = SVLL                             05690000
   16 CONTINUE                                                          05700000
      ASSIGN 6 TO JS                                                    05710000
      GO TO 300                                                         05720000
    6 ASSIGN 20 TO IS                                                   05730000
      IF (IDS.GE.1) GO TO 101                                           05740000
   20 CONTINUE                                                          05750000
      ICONV = 0                                                         05760000
      IF (ABS(D(IEV+1)).GT.ERL1 .OR. ABS(D(IEV+2)).GT.ERL1              05770000
     * .OR. ABS(D(IEV+3)).GT.ERL2) ICONV = 1                            05780000
      IF (ICONV.EQ.0) GO TO 199                                         05790000
   10 CONTINUE                                                          05800000
   17 WTC = 0.0                                                         05810000
      RIC = 0.0                                                         05820000
      CUC = 0.0                                                         05830000
      DRC = 0.0                                                         05840000
      CALL FDC(0,4,2,D,0)                                               05850000
      GO TO 98                                                          05860000
   40 CERR = CERR+1                                                     05870000
      CALL LINES(2)                                                     05880000
      WRITE (OUT,1009) CERR                                             05890000
 1009 FORMAT(6H0ERROR,I6,5X,15HSINGULAR MATRIX)                         05900000
      ICONV = 1                                                         05910000
      GO TO 17                                                          05920000
  101 CONTINUE                                                          05930000
      JSV1 = ISV+1                                                      05940000
      JSV2 = ISV+NSV                                                    05950000
      JEV1 = IEV+1                                                      05960000
      JEV2 = IEV+NEV                                                    05970000
      CALL LINES(2)                                                     05980000
      WRITE (OUT,1003) PASS                                             05990000
 1003 FORMAT(6H0PASS ,I6)                                               06000000
      CALL LINES(3)                                                     06010000
      WRITE (OUT,1004) (D(IX),IX=JSV1,JSV2)                             06020000
 1004 FORMAT(5H0S.V./(1X,10E12.5))                                      06030000
      CALL LINES(3)                                                     06040000
      WRITE (OUT,1005) (D(IX),IX=JEV1,JEV2)                             06050000
 1005 FORMAT(5H0E.V./(1X,10E12.5))                                      06060000
      CALL LINES(2)                                                     06070000
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX06080000
      IRITE=1                                                           06090003
      IF (IRITE .NE. 1) GO TO 9098                                      06100000
      WRITE (OUT,9010)                                                  06110000
 9010 FORMAT('       REH         REC    NTUS(GA55)    EFF     ',        06120005
     *             'ETA HOT   ETA COLD')                                06130005
      WRITE (OUT,9020) REH,REC,D(IGA+55),EFF,ETAH,ETAC                  06140005
 9020 FORMAT(1H0,2F12.2, 4F10.5)                                        06150005
 9098 CONTINUE                                                          06160000
      GO TO IS, (1,20,21)                                               06180000
  300 CONTINUE                                                          06190000
      NH = (D(ISV+3)-D(IFGC+3)-2.0*TSP)/(D(IFGH+3)+D(IFGC+3)+2.0*TSP)   06200000
      NC = NH+1.0                                                       06210000
      PSIH = D(IFGH+3)*D(IFGH+4)*D(IFGH+6)*NH/((D(IFGH+3)+D(IFGC+3)     06220000
     *+2.0*TSP)*NH+D(IFGC+3)+2.0*TSP)                                   06230000
      PSIC = D(IFGC+3)*D(IFGC+4)*D(IFGC+6)*NC/((D(IFGH+3)+D(IFGC+3)     06240000
     *+2.0*TSP)*NH+D(IFGC+3)+2.0*TSP)                                   06250000
      REH = REHP/(D(ISV+2)*D(ISV+3)*PSIH)                               06260000
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX06270000
      IRITE=0                                                           06280000
      IF (IRITE .NE. 1) GO TO 9198                                      06290000
      WRITE (OUT,9110)                                                  06300000
 9110 FORMAT('        D(IFGH+4)      D(IW+NLH)       VISH          ',   06310000
     *               'NPH            REHP            RECP          ')   06320000
      WRITE (OUT,9120) D(IFGH+4),D(IW+NLH),VISH,NPH, REHP, RECP         06330000
 9120 FORMAT (1H0,3E15.4, I4,11X, 2E15.4 )                              06340000
      WRITE (OUT,9130)                                                  06350000
 9130 FORMAT('        PSIH           D(IFGH+3)      D(IFGH+6)      ',   06360000
     *               'NH             D(IFGC+3)      TSP            ')   06370000
      WRITE (OUT,9140) PSIH,D(IFGH+3),D(IFGH+6),NH, D(IFGC+3),TSP       06380000
 9140 FORMAT (1H0,3E15.4, I4,11X, 2E15.4)                               06390000
 9198 CONTINUE                                                          06400000
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX06410000
      REC = RECP/(D(ISV+1)*D(ISV+3)*PSIC)                               06420000
      D(IGA+21) = REH                                                   06430000
      FH = TLUP(ID(IRDH+8))                                             06440000
      AJH = TLUP(ID(IRDH+9))                                            06450000
      D(IGA+21) = REC                                                   06460000
      FC = TLUP(ID(IRDC+8))                                             06470000
      AJC = TLUP(ID(IRDC+9))                                            06480000
      D(IEV+1) = EV1P1*(1.0+PSIH**2)*REH**2*(D(ISV+1)*FH*EV1P2/         06490000
     * (1.0+PSIH**2)+DENHI/DENHO-1.0)                                   06500000
     * -(D(IP+NSIH)-D(IP+NSOH))                                         06510000
      D(IEV+2) = EV2P1*(1.0+PSIC**2)*REC**2*(D(ISV+2)*FC*EV2P2/         06520000
     * (1.0+PSIC**2)+DENCI/DENCO-1.0)                                   06530000
     * -(D(IP+NSIC)-D(IP+NSOC))                                         06540000
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX06550000
      IRITE=0                                                           06560000
      IF (IRITE .NE. 1) GO TO 9298                                      06570000
      WRITE (OUT,9210)                                                  06580000
 9210 FORMAT('        EHP            REH            AJH            ',   06590000
     *               'FH             D(IFGH+5)      D(IFGH+7)      ')   06600000
      WRITE (OUT,9220) EHP,REH,AJH,FH,D(IFGH+5),D(IFGH+7)               06610000
 9220 FORMAT (1H0,6E15.4)                                               06620000
      WRITE (OUT,9230)                                                  06630000
 9230 FORMAT('        ECP            REC            AJC            ',   06640000
     *               'FC             D(IFGC+5)      D(IFGC+7)      ')   06650000
      WRITE (OUT,9240) ECP,REC,AJC,FC,D(IFGC+5),D(IFGC+7)               06660000
 9240 FORMAT (1H0,6E15.4)                                               06670000
 9298 CONTINUE                                                          06680000
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX06690000
      ETAH = SQRT(EHP*REH*AJH*(1.0+D(IFGH+5)/D(IFGH+7)))                06700000
      ETAC = SQRT(ECP*REC*AJC*(1.0+D(IFGC+5)/D(IFGC+7)))                06710000
      ETAH = 1.0-D(IFGH+1)*(1.0-TANH(ETAH*EFLH)/(ETAH*EFLH))            06720000
      ETAC = 1.0-D(IFGC+1)*(1.0-TANH(ETAC*EFLC)/(ETAC*EFLC))            06730000
      D(IGA+55) = D(IFGH+4)*D(IW+NLH)*D(ISV+1)*NPH/                     06740000
     *(WCP*PSIH*REH*VISH*(D(IFGH+4)**2*PRH/                             06750000
     *(ETAH*CPH*VISH*PSIH*REH*AJH)+D(IFGC+4)**2*PRC/                    06760000
     *(ETAC*CPC*VISC*PSIC*REC*AJC)))                                    06770000
      EFF = TLUP(ID(IRCD+9))                                            06780000
      D(IEV+3) = EFF-EFFP                                               06790000
      GO TO JS, (4,5,6,7)                                               06800000
  199 CONTINUE                                                          06810000
      ASSIGN 7 TO JS                                                    06820000
      IFP = 1                                                           06830000
      GO TO 300                                                         06840000
    7 LH = D(ISV+1)                                                     06850000
      LC = D(ISV+2)                                                     06860000
      LN = D(ISV+3)                                                     06870000
      CALL FDC(0,4,2,D,0)                                               06880000
  200 NH = (LN-D(IFGC+3)-2.0*TSP)/(D(IFGH+3)+D(IFGC+3)+2.0*TSP)         06890000
      NC = NH+1.0                                                       06900000
      PSIH = D(IFGH+3)*D(IFGH+4)*D(IFGH+6)*NH/((D(IFGH+3)+D(IFGC+3)     06910000
     *+2.0*TSP)*NH+D(IFGC+3)+2.0*TSP)                                   06920000
      PSIC = D(IFGC+3)*D(IFGC+4)*D(IFGC+6)*NC/((D(IFGH+3)+D(IFGC+3)     06930000
     *+2.0*TSP)*NH+D(IFGC+3)+2.0*TSP)                                   06940000
      IFG = IFGH                                                        06950000
      IM = IMH                                                          06960000
      JJ = 0                                                            06970000
   70 IFS = D(IFG+8)+0.1                                                06980000
      BB = (D(IFG+3)-(D(IFG+9)-1.0)*TSP)/D(IFG+9)                       06990000
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX07000000
      IRITE=0                                                           07010000
      IF (IRITE .NE. 1) GO TO 9398                                      07020000
      WRITE (OUT,9310)                                                  07030000
 9310 FORMAT('        D(IFG+5)       D(IFG+2)       BB')                07040000
      WRITE (OUT,9320) D(IFG+5),D(IFG+2),BB                             07050000
 9320 FORMAT (1H0,3E15.4)                                               07060000
 9398 CONTINUE                                                          07070000
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX07080000
      IF (IFS-2) 71,72,73                                               07090000
   71 RHOF = D(IFG+5)*D(IFG+2)*D(IM+1)*(BB+1.0/D(IFG+2)                 07100000
     * -2.71682*D(IFG+5))/BB                                            07110000
      GO TO 74                                                          07120000
   72 RHOF = D(IFG+5)*D(IFG+2)*D(IM+1)*(BB+0.5708/D(IFG+2)-D(IFG+5))/BB 07130000
      GO TO 74                                                          07140000
   73 RHOF = (6.0*D(IFG+5)*D(IFG+2)**2*BB-1.0+SQRT(1.0+D(IFG+2)**2*BB*BB07150000
     * -12.0*D(IFG+5)*D(IFG+2)**2*BB))/(0.3333/D(IFG+5)-12.0*D(IFG+5)   07160000
     * *D(IFG+2)**2)                                                    07170000
      PHI = 2.0*ACOS(3.0*D(IFG+5)/(RHOF+3.0*D(IFG+5)))                  07180000
      RHOF = 1.0/(D(IFG+2)*COS(PHI/2.0))-6.0*D(IFG+5)*D(IFG+2)          07190000
     * *(BB+2.0*RHOF)                                                   07200000
      RHOF = D(IFG+2)*D(IFG+5)*D(IM+1)*(RHOF+3.0*D(IFG+5)*PHI)/BB       07210000
   74 IF (JJ.EQ.0) GO TO 75                                             07220000
      WTFC = RHOF*NC*(D(IFGC+3)-(D(IFGC+9)-1.0)*TSP)*LH*LC/1728.0       07230000
      GO TO 80                                                          07240000
   75 WTFH = RHOF*NH*(D(IFGH+3)-(D(IFGH+9)-1.0)*TSP)*LH*LC/1728.0       07250000
      IM = IMC                                                          07260000
      JJ = 1                                                            07270000
      IFG = IFGC                                                        07280000
      GO TO 70                                                          07290000
   80 IMSP = ITLUP(ID(IRCD+7))                                          07300000
      VOL = LH*LC*LN                                                    07310000
      WTCO = WTFH+WTFC+D(IMSP+1)*LH*LC*TSP*(NH*D(IFGH+9)+NC*D(IFGC+9)   07320000
     * +1.0)/1728.0                                                     07330000
      IF (D(IMSP+1).GT.460.0) WTCO = WTCO+6.44E-4*LH*LC                 07340000
     * *(NH*D(IFGH+9)+NC*D(IFGC+9)+1.0)                                 07350000
      WTC = 3.23126*WTCO/VOL**0.118                                     07360000
      WTF = D(ID(IRCD+2))                                               07370000
      IF (WTF.NE.0.0) WTC = WTC*WTF                                     07380000
      WTIC = 0.205*WTC                                                  07390000
      WTDC = 0.12*WTC                                                   07400000
      CUC = 2.08*WTC                                                    07410000
      CUF = D(ID(IRCD+3))                                               07420000
      IF (CUF.NE.0.0) CUC = CUC*CUF                                     07430000
      IFTH = ID(IFB+NLH)                                                07440000
      IFTH = ID(IFTH+1)                                                 07450000
      IFTC = ID(IFB+NLC)                                                07460000
      IFTC = ID(IFTC+1)                                                 07470000
C$        DO V'S ALL THE TIME                                           07480000
      VLH = PSIH*VOL                                                    07490000
      VLC = PSIC*VOL                                                    07500000
      IF (IFTC.EQ.2. .AND. IFTH.EQ.2) GO TO 91                          07510000
      IF (IFTH.NE.2 .AND. IFTC.NE.2) GO TO 95                           07520000
      IF (IFTH.NE.2 .AND. IFTC.EQ.2) GO TO 96                           07530000
      PRES = PAC                                                        07540000
      RIC = 0.00328                                                     07550000
C$    VLC = PSIC*VOL                                                    07560000
      GO TO 93                                                          07570000
   95 PRES = AMAX1(PAH,PAC)                                             07580000
      RIC = 0.00106                                                     07590000
C$    VLH = PSIH*VOL                                                    07600000
C$    VLC = PSIC*VOL                                                    07610000
      GO TO 93                                                          07620000
   96 PRES = PAH                                                        07630000
      RIC = 0.00328                                                     07640000
C$    VLH = PSIH*VOL                                                    07650000
   93 IF (PRES.LT.314.7) PRES = 314.7                                   07660000
      DRC = 1.0+((PRES-314.7)/283.0)**2                                 07670000
      GO TO 90                                                          07680000
   91 TEMP = D(IT+NSIH)                                                 07690000
      PRES = PAH                                                        07700000
      IF (TEMP.LT.1760.0) TEMP = 1760.0                                 07710000
      IF (PRES.LT.314.7)  PRES = 314.7                                  07720000
      DRC = 1.0+((TEMP-1760.0)/425.0)**2+((PRES-314.7)/283.0)**2        07730000
      RIC = 0.00256                                                     07740000
      IF (NPH.GT.1 .OR. NPC.GT.1) RIC = 0.01973                         07750000
   90 DRF = D(ID(IRCD+5))                                               07760000
      IF (DRF.NE.0.0) DRC = DRC*DRF                                     07770000
      IF (DRC.GT.10.0) DRC = 10.0                                       07780000
      RI = D(ID(IRCD+4))                                                07790000
      IF (RI.NE.0.0)  RIC = RI                                          07800000
      CALL SSA                                                          07810000
   98 CALL SCO                                                          07820000
      IDS = IDSS                                                        07830000
      CALL SCI(NLH,NSIH,NSOH)                                           07840000
      CALL SCI(NLC,NSIC,NSOC)                                           07850000
      IF (ICONV.EQ.0) GO TO 85                                          07860000
      CERR = CERR+1                                                     07870000
      CALL LINES(2)                                                     07880000
      WRITE (OUT,1010) CERR                                             07890000
 1010 FORMAT(6H0ERROR,I6,5X,14HNON CONVERGENT)                          07900000
      GO TO 99                                                          07910000
   85 CONTINUE
C 8/98                                                            07920000
      IFL1 = ID(IFB+NLH)                                                        
      IF (ID(IFL1+1).EQ.1) GO TO 18                                             
      Q = WCPH*(D(IT+NSOH)-D(IT+NSIH))*SHRH                                       
      GO TO 29                                                                  
   18 Q = D(IW+NLH)*(HFT(NLH,D(IP+NSOH),D(IT+NSOH))-HFT(NLH,D(IP+NSIH),         
     *D(IT+NSIH)))                                                              
C
   29 CALL LINES(2)                                                     07930000
      WRITE (OUT,1002) LH,LC,LN,VOL,WTCO                                07940000
 1002 FORMAT(1H0,5X,2HLH,F8.2,3X,2HLC,F8.2,3X,2HLN,F8.2,3X,3HVOL,F7.0,3X07950000
     *,3HWTC,F7.1)                                                      07960000
      IF (D(ID(IRCD+6)).NE.0.0) GO TO 86                                07970000
      WRITE (OUT,1008) VLH, VLC,   EFF,D(IGA+55), Q                        07980000
 1008 FORMAT(1H+, 5X, 5HVOLH=,F8.1,13X,   5HVOLC=,F8.1,13X              07990000
     *              , 4HEFF=,F7.4,2X,   4HNTU=,F7.3, 2X, 2HQ=,F10.1)       08000000
C$**********************************************************************08010000
C  86 IF (IFTH.EQ.1) WRITE (OUT,1006) VLH                               08020000
C1006 FORMAT(1H+,70X,4HVLH=,F8.1)                                       08030000
C     IF (IFTC.EQ.1) WRITE (OUT,1007) VLC                               08040000
C1007 FORMAT(1H+,70X,4HVLC=,F8.1)                                       08050000
C***********************************************************************08060000
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9333                                              
      Q1 = WCPH*(D(IT+NSOH)-D(IT+NSIH))*SHRH                                       
      Q2 = D(IW+NLH)*(HFT(NLH,D(IP+NSOH),D(IT+NSOH))-HFT(NLH,D(IP+NSIH),         
     *D(IT+NSIH)))                                                              
      WRITE (OUT,9311)                                                          
 9311 FORMAT(' *********WRITE 9311 FROM "HXSP" *********************'         
     *      ,'**************')                                                  
      WRITE (OUT,9312) CPH   , D(IT+NSOH) , D(IT+NSIH) , SHRH                               
     *               , D(IW+NLH), HFT(NLH,D(IP+NSOH),D(IT+NSOH)), 
     *  HFT(NLH,D(IP+NSIH), D(IT+NSIH)), IFL1, ID(IFL1+1), Q1, Q2, Q                             
 9312 FORMAT(' CPH   =', E12.5,  ' T2H   =', E12.5,  ' T1H   =', E12.5          
     *      ,' SHRH  =', E12.5,  ' WH    =', E12.5,  ' H2H   =', E12.5          
     *    ,/,' H1H   =', E12.5,  ' IFL1  =', I12  ,  'IDIFL+1=', I12
     *      ,' Q1    =', E12.5,  ' Q2    =', E12.5,  ' Q     =', E12.5)                                                 
 9333 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
   86 D(IGA+71) = LH                                                    08070000
      D(IGA+72) = LC                                                    08080000
      D(IGA+73) = LN                                                    08090000
      D(IGA+74) = VOL                                                   08100000
      D(IGA+75) = VLH                                                   08110000
      D(IGA+76) = VLC                                                   08120000
      MAP = ID(IRCD+10)                                                 08130000
      IF (MAP.EQ.0) GO TO 99                                            08140000
      IFP = 0                                                           08150000
      CALL GDCU(304,4,2,D,LOC)                                          08160000
      LOCZ = LOC+16                                                     08170000
      LOCY = LOCZ+16                                                    08180000
      LOC1 = LOC+288                                                    08190000
      LOC2 = LOC1+8                                                     08200000
      CPH = CPH/SHRH                                                    08210000
      CPC = CPC/SHRC                                                    08220000
      EHP = EHP/(SHRH**.3333333)                                        08230000
      ECP = ECP/(SHRC**.3333333)                                        08240000
      PRH = PRH/(SHRH**.6666667)                                        08250000
      PRC = PRC/(SHRC**.6666667)                                        08260000
      IF (IFTH.EQ.2) SIGH = SIG(NLH,D(IP+NSIH),TAH)                     08270000
      IF (IFTC.EQ.2) SIGC = SIG(NLC,D(IP+NSIC),TAC)                     08280000
      REHP = 0.8*D(IFGH+4)*NPH/(VISH*PSIH*LC*LN)                        08290000
      RECP = 0.8*D(IFGC+4)*NPC/(VISC*PSIC*LH*LN)                        08300000
      EV1P1 = EV1P1*(1.0+PSIH**2)                                       08310000
      EV2P1 = EV2P1*(1.0+PSIC**2)                                       08320000
      EV1P2 = EV1P2/(1.0+PSIH**2)                                       08330000
      EV2P2 = EV2P2/(1.0+PSIC**2)                                       08340000
      II = 0                                                            08350000
      DO 3059 I = 1,16                                                  08360000
      IF (I.EQ.1 .OR. I.EQ.5 .OR. I.EQ.15 .OR. I.EQ.16) II = II+1       08370000
      WH = I*0.2*D(IW+NLH)                                              08380000
      IF (I.EQ.16) WH = 5.0*D(IW+NLH)                                   08390000
      WCPH = WH*CPH                                                     08400000
      REH = REHP*WH                                                     08410000
      D(IGA+21) = REH                                                   08420000
      FH = TLUP(ID(IRDH+8))                                             08430000
      AJH = TLUP(ID(IRDH+9))                                            08440000
      PDH = EV1P1*REH**2*(LH*FH*EV1P2+DENHI/DENHO-1.0)                  08450000
      IF (IFTH.EQ.2) PDH = PDH*SIGH                                     08460000
      ETAH = SQRT(EHP*REH*AJH*(1.0+D(IFGH+5)/D(IFGH+7)))                08470000
      ETAH = 1.0-D(IFGH+1)*(1.0-TANH(ETAH*EFLH)/(ETAH*EFLH))            08480000
      EHAH = ETAH*CPH*AJH*WH*LH*NPH/(D(IFGH+4)*PRH)                     08490000
      JJ = 0                                                            08500000
      DO 3059 J=1,16                                                    08510000
      IF (J.EQ.1 .OR. J.EQ.5 .OR. J.EQ.15 .OR. J.EQ.16) JJ = JJ+1       08520000
      WC = J*0.2*D(IW+NLC)                                              08530000
      IF (J.EQ.16) WC = 5.0*D(IW+NLC)                                   08540000
      WCPC = WC*CPC                                                     08550000
      IF (WCPH.GT.WCPC) GO TO 3051                                      08560000
      WCP = WCPH                                                        08570000
      GO TO 3052                                                        08580000
 3051 WCP = WCPC                                                        08590000
 3052 GO TO (3053,3053,3054,3055), IFLT                                 08600000
 3053 IF (WCPH.GT.WCPC) GO TO 3056                                      08610000
 3057 D(IGA+56) = WCPH/WCPC                                             08620000
      GO TO 3060                                                        08630000
 3056 D(IGA+56) = WCPC/WCPH                                             08640000
      GO TO 3060                                                        08650000
 3054 IF (IHC.EQ.2) GO TO 3056                                          08660000
      GO TO 3057                                                        08670000
 3055 IF (IHC.EQ.2) GO TO 3057                                          08680000
      GO TO 3056                                                        08690000
 3060 REC = RECP*WC                                                     08700000
      D(IGA+21) = REC                                                   08710000
      FC = TLUP(ID(IRDC+8))                                             08720000
      AJC = TLUP(ID(IRDC+9))                                            08730000
      PDC = EV2P1*REC**2*(LC*FC*EV2P2+DENCI/DENCO-1.0)                  08740000
      IF (IFTC.EQ.2) PDC = PDC*SIGC                                     08750000
      ETAC = SQRT(ECP*REC*AJC*(1.0+D(IFGC+5)/D(IFGC+7)))                08760000
      ETAC = 1.0-D(IFGC+1)*(1.0-TANH(ETAC*EFLC)/(ETAC*EFLC))            08770000
      EHAC = ETAC*CPC*AJC*WC*LC*NPC/(D(IFGC+4)*PRC)                     08780000
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX08781005
      IRITE=1                                                           08782005
      IF (IRITE .NE. 1) GO TO 9498                                      08783005
      WRITE (OUT,9420)                                                  08786006
     *   WH    ,  EHAH  ,  WC    ,  EHAC !,  XXXX  ,  XXXX  ,  XXXX     08787006
 9420 FORMAT(' WH    =',F12.4  , ' ETAHAH=',F12.4  , ' WC    =',F10.4,  08787207
     *       ' ETAHAC=',F12.4)!, ' XXX   =',F12.4) ! ' XXX   =',F10.4)  08787306
 9498 CONTINUE                                                          08788005
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX08789005
      D(IGA+55) = 1.0/(WCP*(1.0/EHAH+1.0/EHAC))                         08790000
      EFF = TLUP(ID(IRCD+9))                                            08800000
      IF (EFF.GT.1.0) EFF = 1.0                                         08810000
      IF (IHC.EQ.2) GO TO 3061                                          08820000
      D(LOC+I) = WH                                                     08830000
      D(LOCZ+J) = WC                                                    08840000
      EFF = EFF*WCP/WCPH                                                08850000
      D(LOCY+I+16*(J-1)) = EFF                                          08860000
      IF (.NOT.(I.EQ.1 .OR. I.EQ.5 .OR. I.EQ.15 .OR. I.EQ.16))          08870000
     * GO TO 3050                                                       08880000
      D(LOC1+2*(II-1)+1) = WH                                           08890000
      D(LOC1+2*II) = PDH                                                08900000
      IF (.NOT.(J.EQ.1 .OR. J.EQ.5 .OR. J.EQ.15 .OR. J.EQ.16))          08910000
     * GO TO 3050                                                       08920000
      D(LOC2+2*(JJ-1)+1) = WC                                           08930000
      D(LOC2+2*JJ) = PDC                                                08940000
      GO TO 3050                                                        08950000
 3061 D(LOC+J) = WC                                                     08960000
      D(LOCZ+I) = WH                                                    08970000
      EFF = EFF*WCP/WCPC                                                08980000
      D(LOCY+J+16*(I-1)) = EFF                                          08990000
      IF (.NOT.(I.EQ.1 .OR. I.EQ.5 .OR. I.EQ.15 .OR. I.EQ.16))          09000000
     * GO TO 3050                                                       09010000
      D(LOC2+2*(II-1)+1) = WH                                           09020000
      D(LOC2+2*II) = PDH                                                09030000
      IF (.NOT.(J.EQ.1 .OR. J.EQ.5 .OR. J.EQ.15 .OR. J.EQ.16))          09040000
     * GO TO 3050                                                       09050000
      D(LOC1+2*(JJ-1)+1) = WC                                           09060000
      D(LOC1+2*JJ) = PDC                                                09070000
 3050 WRITE(12) WH, WC, EFF                                             09080000
      WRITE(13) WC, PDC                                                 09090000
      WRITE(17) WH, PDH                                                 09100000
 3059 CONTINUE                                                          09110000
      I1 = LOC+1                                                        09120000
      I2 = LOC+16                                                       09130000
      IF (MAP.EQ.2) GO TO 3001                                          09140000
      CALL LINES(100)                                                   09150000
      WRITE (OUT,2009)                                                  09160000
 2009 FORMAT(7H HX MAP)                                                 09170000
      WRITE (OUT,2011)                                                  09180000
 2011 FORMAT(6H0    *,56HTABID      1   5   3   0   0  41 0 1  16   0  409190000
     *2 0 1  16,24X,1H*)                                                09200000
      WRITE (OUT,2012)                                                  09210000
 2012 FORMAT(5X,1H*,12HTABT  HX EFF,68X,1H*)                            09220000
      WRITE (OUT,2010) (D(I),I=I1,I2)                                   09230000
 2010 FORMAT(5X,1H*,6HTABV  ,4X,4F10.4,30X,1H*)                         09240000
 3001 CONTINUE                                                          09250000
      IF (MAP.EQ.1) GO TO 3011                                          09260000
      WRITE (PUN,2001)                                                  09270000
 2001 FORMAT(56HTABID      1   5   3   0   0  41 0 1  16   0  42 0 1  1609280000
     *,24X)                                                             09290000
      WRITE (PUN,2002)                                                  09300000
 2002 FORMAT(12HTABT  HX EFF,68X)                                       09310000
      WRITE (PUN,2000) (D(I),I=I1,I2)                                   09320000
 2000 FORMAT(6HTABV  ,4X,4F10.4,30X)                                    09330000
 3011 CONTINUE                                                          09340000
      I1 = LOC+17                                                       09350000
      I2 = LOC+32                                                       09360000
      IF (MAP.EQ.2) GO TO 3002                                          09370000
      WRITE (OUT,2010) (D(I),I=I1,I2)                                   09380000
 3002 CONTINUE                                                          09390000
      IF (MAP.EQ.1) GO TO 3012                                          09400000
      WRITE (PUN,2000) (D(I),I=I1,I2)                                   09410000
 3012 CONTINUE                                                          09420000
      I1 = LOC+33                                                       09430000
      I2 = LOC+288                                                      09440000
      IF (MAP.EQ.2) GO TO 3003                                          09450000
      WRITE (OUT,2010) (D(I),I=I1,I2)                                   09460000
 3003 CONTINUE                                                          09470000
      IF (MAP.EQ.1) GO TO 3013                                          09480000
      WRITE (PUN,2000) (D(I),I=I1,I2)                                   09490000
 3013 CONTINUE                                                          09500000
      I1 = LOC1+1                                                       09510000
      I2 = LOC1+8                                                       09520000
      IF (MAP.EQ.2) GO TO 3004                                          09530000
      WRITE (OUT,2013)                                                  09540000
 2013 FORMAT(6H0    *,40HTABID      1   1   2   3   0  41 0 1   4,40X,1H09550000
     **)                                                                09560000
      WRITE (OUT,2014)                                                  09570000
 2014 FORMAT(5X,1H*,12HTABT  HX PD1,68X,1H*)                            09580000
      WRITE (OUT,2010) (D(I),I=I1,I2)                                   09590000
 3004 CONTINUE                                                          09600000
      IF (MAP.EQ.1) GO TO 3014                                          09610000
      WRITE (PUN,2003)                                                  09620000
 2003 FORMAT(40HTABID      1   1   2   3   0  41 0 1   4,40X)           09630000
      WRITE (PUN,2004)                                                  09640000
 2004 FORMAT(12HTABT  HX PD1,68X)                                       09650000
      WRITE (PUN,2000) (D(I),I=I1,I2)                                   09660000
 3014 CONTINUE                                                          09670000
      I1 = LOC2+1                                                       09680000
      I2 = LOC2+8                                                       09690000
      IF (MAP.EQ.2) GO TO 3005                                          09700000
      WRITE (OUT,2016)                                                  09710000
 2016 FORMAT(6H0    *,40HTABID      2   1   2   3   0  42 0 1   4,40X,1H09720000
     **)                                                                09730000
      WRITE (OUT,2015)                                                  09740000
 2015 FORMAT(5X,1H*,12HTABT  HX PD2,68X,1H*)                            09750000
      WRITE (OUT,2010) (D(I),I=I1,I2)                                   09760000
      CALL LINES(100)                                                   09770000
 3005 CONTINUE                                                          09780000
      IF (MAP.EQ.1) GO TO 3015                                          09790000
      WRITE (PUN,2006)                                                  09800000
 2006 FORMAT(40HTABID      2   1   2   3   0  42 0 1   4,40X)           09810000
      WRITE (PUN,2005)                                                  09820000
 2005 FORMAT(12HTABT  HX PD2,68X)                                       09830000
      WRITE (PUN,2000) (D(I),I=I1,I2)                                   09840000
 3015 CONTINUE                                                          09850000
      CALL FDC(0,4,2,D,0)                                               09860000
   99 CONTINUE                                                          09870000
      RETURN                                                            09880000
C     HXSP                                                              09890000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC09900000
      END                                                               09910000
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE HXSZ                                                           
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
     *, (IAUR,C(19)), (IT,C(29)), (SCR(1),C(151)), (MDPC,C(34))                 
     *, (ISP,C(42))                                                             
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER CERR, OUT                                                         
      EQUIVALENCE (SCR(1),NR1), (SCR(2),NR2), (SCR(3),LN), (SCR(4),L1)          
     *, (SCR(5),L2), (SCR(6),LOC), (SCR(7),CC), (SCR(8),NC), (SCR(9),NL)        
      DIMENSION CD(200)                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84))                                                   
      COMMON /CS/ SD(100)                                                       
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      real*8 cd, cn, sd
      DATA IFTA/011/                                                            
C   1 COMP CODE                                                                 
C   2 WTF                                                                       
C   3 CUF                                                                       
C   4 RI                                                                        
C   5 DRF                                                                       
C   6 LN                                                                        
C   7 IMSP                                                                      
C   8 ITSP                                                                      
C   9 IEF                                                                       
C  10 MAP                                                                       
C  11 IFLT                                                                      
C  12 IHC                                                                       
C  13 NL1                                                                       
C  14 NSI1                                                                      
C  15 NSO1                                                                      
C  16 L1                                                                        
C  17 NP1                                                                       
C  18 IM1                                                                       
C  19 IFG1                                                                      
C  20 IF1                                                                       
C  21 IJ1                                                                       
C  22 NL2                                                                       
C  23 NSI2                                                                      
C  24 NSO2                                                                      
C  25 L2                                                                        
C  26 NP2                                                                       
C  27 IM2                                                                       
C  28 IFG2                                                                      
C  29 IF2                                                                       
C  30 IJ2                                                                       
      IE = 1                                                                    
      IPRNT = 10*(MDPC/10)                                                      
      IPRNT = IPRNT-100*(IPRNT/100)                                             
      IF (MDPC.LT.0) IPRNT = -1                                                 
      I = IACDB(30)                                                             
      ID(I+1) = ICV(1)                                                          
      ID(I+2) = IPARM(ICV(2))                                                   
      ID(I+3) = IPARM(ICV(3))                                                   
      ID(I+4) = IPARM(ICV(4))                                                   
      ID(I+5) = IPARM(ICV(5))                                                   
      LN = ICV(6)                                                               
      ID(I+6) = IPARM(ICV(6))                                                   
      ID(I+7) = ITIDN(ICV(7),20)                                                
      ID(I+8) = ITIDN(ICV(8),34)                                                
      ID(I+9) = ITIDN(ICV(9),5)                                                 
      ID(I+10) = ICV(10)                                                        
      ID(I+11) = ICV(11)                                                        
      NR1 = 0                                                                   
      NR2 = 0                                                                   
    1 READ (IAUR,1200) CN,CC,NC,(ICV(II),II=2,18)                               
 1200 FORMAT(A6,A2,I4,17A4)                                                     
      BACKSPACE IAUR                                                            
      READ (IAUR,1000) CN,CC,NC,(ICV(II),II=2,18)                               
 1000 FORMAT(A6,A2,18I4)                                                        
      IF (CN.NE.SD(ICV(1)+1)) GO TO 2                                           
      IF (IPRNT.NE.0) GO TO 21                                                  
      CALL LINES(1)                                                             
      WRITE (OUT,1060) CN,CC,NC,(ICV(II),II = 2,18)                             
 1060 FORMAT(5X,A6,1X,A2,18(1X,I4))                                             
   21 IF (NR1.NE.0) GO TO 4                                                     
      NL = ILEGN(ICV(2))                                                        
      ID(I+13) = NL                                                             
      ID(I+14) = ISTAN(ICV(3))                                                  
      ID(I+15) = ISTAN(ICV(4))                                                  
      CALL FTL(NL,IFTA)                                                         
      L1 = ICV(5)                                                               
      ID(I+16) = IPARM(ICV(5))                                                  
      ID(I+18) = ITIDN(ICV(7),20)                                               
      ID(I+17) = ICV(6)                                                         
      ID(I+19) = ITIDN(ICV(8),31)                                               
      ID(I+20) = ITIDN(ICV(8),32)                                               
      ID(I+21) = ITIDN(ICV(8),33)                                               
      NR1=1                                                                     
      IF(NR2.EQ.1) GO TO 5                                                      
      GO TO 1                                                                   
    2 IF (CN.NE.SD(ICV(1)+2)) GO TO 3                                           
      IF (IPRNT.NE.0) GO TO 22                                                  
      CALL LINES(1)                                                             
      WRITE (OUT,1060) CN,CC,NC,(ICV(II),II = 2,18)                             
   22 IF (NR2.NE.0) GO TO 4                                                     
      NL = ILEGN(ICV(2))                                                        
      ID(I+22) = NL                                                             
      ID(I+23) = ISTAN(ICV(3))                                                  
      ID(I+24) = ISTAN(ICV(4))                                                  
      CALL FTL(NL,IFTA)                                                         
      L2 = ICV(5)                                                               
      ID(I+25) = IPARM(ICV(5))                                                  
      ID(I+26) = ICV(6)                                                         
      ID(I+27) = ITIDN(ICV(7),20)                                               
      ID(I+28) = ITIDN(ICV(8),31)                                               
      ID(I+29) = ITIDN(ICV(8),32)                                               
      ID(I+30) = ITIDN(ICV(8),33)                                               
      NR2 = 1                                                                   
      IF (NR1.EQ.1) GO TO 5                                                     
      GO TO 1                                                                   
    3 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1010) CERR                                                     
 1010 FORMAT(6H0ERROR,I6,5X,15HNO HX1/HX2 CARD)                                 
      BACKSPACE IAUR                                                            
      GO TO 99                                                                  
    4 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1020) CERR,CN                                                  
 1020 FORMAT(6H0ERROR,I6,5X,9HMULTIPLE ,A3,5H CARD)                             
      GO TO 99                                                                  
    5 IF (LN.EQ.0.AND.L1.EQ.0.AND.L2.EQ.0) GO TO 6                              
      IF (LN.NE.0.AND.L1.NE.0.AND.L2.NE.0) GO TO 6                              
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1040) CERR                                                     
 1040 FORMAT(6H0ERROR,I6,5X,20HMIXED SIZE SPECIFIED)                            
    6 IF (D(IT+ID(I+14))-D(IT+ID(I+23))) 11,12,13                               
   11 ID(I+12) = 2                                                              
      GO TO 99                                                                  
   12 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1030) CERR                                                     
 1030 FORMAT(6H0ERROR,I6,5X,24HEQUAL FLUID TEMPERATURES)                        
      GO TO 99                                                                  
   13 ID(I+12) = 1                                                              
   99 CONTINUE                                                                  
      RETURN                                                                    
      ENTRY HX1SZ                                                               
      IE = 2                                                                    
      GO TO 10                                                                  
      ENTRY HX2SZ                                                               
      IE = 3                                                                    
   10 CONTINUE                                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1050) CERR                                                     
 1050 FORMAT(6H0ERROR,I6,5X,15HHX CARD MISSING)                                 
      GO TO 99                                                                  
C     HXSZ                                                                      
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C$    SUBROUTINE INDTPZ IS NEW                                                  
C**********************************************************************         
C                                                                               
      SUBROUTINE INDTPZ                                                         
      COMMON /CC/C(600)                                                         
      EQUIVALENCE (OUT,C(7)),(CERR,C(16)),(NSTA,C(26)),(NLEG,C(25))             
     *,(IPAR,C(33)),(ILN,C(37)),(ISN,C(38)),(ILR,C(52)),(ISR,C(53))             
     *,(IW,C(27)),(IP,C(28)),(IT,C(29)),(IH,C(30)),(ICV(1),C(133))              
      DIMENSION ICV(18)                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      INTEGER OUT,CERR                                                          
C   1 COMP CODE                                                                 
C   2 LEG NUMBER                                                                
C   3 STATION NUMBER                                                            
C   4 W                                                                         
C   5 P                                                                         
C   6 T                                                                         
C   7 H                                                                         
      I=IACDB(7)                                                                
      ID(I+1)=ICV(1)                                                            
      NL=ILEGN(ICV(2))                                                          
      ID(I+2)=NL                                                                
      CALL LEGRS(NL)                                                            
      ID(ILR+NL)=999                                                            
      NS=ISTAN(ICV(3))                                                          
      ID(I+3)=NS                                                                
      CALL STARS(NS)                                                            
      ID(ISR+NS)=999                                                            
      D(IW+NL)=D(IPAR+ICV(4))                                                   
      D(IP+NS)=D(IPAR+ICV(5))                                                   
      D(IT+NS)=D(IPAR+ICV(6))                                                   
      D(IH+NS)=D(IPAR+ICV(7))                                                   
      RETURN                                                                    
C     INDTPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE INITSP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (WTS,C(101)), (CUS,C(103))          
     *, (RIS,C(105)), (DRS,C(107)), (WTDS,C(110)), (POWES,C(112))               
     *, (POWSS,C(113)), (POWHS,C(114)), (BAES,C(115)), (FC,C(121))              
     *, (SDRAG,C(122)), (EWT,C(128))                                            
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE (SCR(1),ITYP), (SCR(2),VALUE)                                 
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 TYPE                                                                      
C   3 VALUE                                                                     
      IRCD = IRCDB(3)                                                           
      ITYP = ID(IRCD+2)                                                         
      VALUE = D(ID(IRCD+3))                                                     
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12), ITYP                                  
    1 WTS = WTS+VALUE                                                           
      GO TO 99                                                                  
    2 CUS = CUS+VALUE                                                           
      GO TO 99                                                                  
    3 RIS = RIS+VALUE                                                           
      GO TO 99                                                                  
    4 DRS = DRS*VALUE                                                           
      GO TO 99                                                                  
    5 WTDS = WTDS+VALUE**2                                                      
      GO TO 99                                                                  
    6 POWES = POWES+VALUE                                                       
      GO TO 99                                                                  
    7 POWSS = POWSS+VALUE                                                       
      GO TO 99                                                                  
    8 POWHS = POWHS+VALUE                                                       
      GO TO 99                                                                  
    9 BAES = BAES+VALUE                                                         
      GO TO 99                                                                  
   10 FC = FC+VALUE                                                             
      GO TO 99                                                                  
   11 SDRAG = SDRAG+VALUE                                                       
      GO TO 99                                                                  
   12 EWT = EWT+VALUE                                                           
      WTS = WTS+VALUE                                                           
   99 CONTINUE                                                                  
      CALL SCOA                                                                 
      CALL LINES(2)                                                             
      WRITE (OUT,1000) ITYP,VALUE                                               
 1000 FORMAT(1H0,5X,4HTYPE,I4,5X,5HVALUE,E15.6)                                 
      RETURN                                                                    
C     INITSP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      INITIALIZATION                                                           
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE INITSZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
      DIMENSION ICV(18)                                                         
      INTEGER CERR, OUT                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 TYPE                                                                      
C   3 VALUE                                                                     
      I = IACDB(3)                                                              
      ID(I+1) = ICV(1)                                                          
      ID(I+2) = ICV(2)                                                          
      IF (ICV(2).GT.0 .AND. ICV(2).LE.12) GO TO 99                              
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,ICV(2)                                              
 1000 FORMAT(6H0ERROR,I6,5X,12HINVALID TYPE,I6)                                 
   99 CONTINUE                                                                  
      ID(I+3) = IPARM(ICV(3))                                                   
      RETURN                                                                    
C     INITSZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      AN INJECTOR                                                              
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE INJTPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)), (SCR(1),C(151)), (OUT,C(7)), (ICPP,C(88))                   
     *, (IFP,C(22))                                                             
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE  (SCR(1),NLI1), (SCR(2),NSI1), (SCR(3),NLI2)                  
     *, (SCR(4),NSI2), (SCR(5),NLO), (SCR(6),NSO), (SCR(7),HS)                  
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 INLET LEG NO                                                              
C   3 INLET STATION NO                                                          
C   4 WATER LEG NO                                                              
C   5 WATER STATION NO                                                          
C   6 OUTLET LEG NO                                                             
C   7 OUTLET STATION NO                                                         
      IRCD = IRCDB(7)                                                           
      NLI1 = ID(IRCD+2)                                                         
      NSI1 = ID(IRCD+3)                                                         
      NLI2 = ID(IRCD+4)                                                         
      NSI2 = ID(IRCD+5)                                                         
      NLO  = ID(IRCD+6)                                                         
      NSO  = ID(IRCD+7)                                                         
      IF(D(IW+NLI2).EQ.0.0) GO TO 1                                             
      D(IW+NLO) = D(IW+NLI1) + D(IW+NLI2)                                       
      D(IP+NSO) = D(IP+NSI1)                                                    
      D(IH+NSO) = D(IH+NSI1)+(1.0+D(IH+NSI1))*D(IW+NLI2)/D(IW+NLI1)             
C     WRITE(6,5998) D(IT+NSO)                                                   
C5998 FORMAT(' ','DITNSO PREVIOUS TO CHANGE=',E16.8)                            
      D(IT+NSO) = (D(IW+NLI1)*SHP(NLI1,D(IP+NSI1),D(IT+NSI1),D(IH+NSI1))        
     * *D(IT+NSI1)+D(IW+NLI2)                                                   
     * *D(IT+NSI2))/(D(IW+NLO)*SHP(NLO,D(IP+NSO),D(IT+NSI1),D(IH+NSO)))         
C     WRITE(6,5997) D(IT+NSO)                                                   
C5997 FORMAT(' ','D(IT+NSO) AFTER CHANGE=',E16.8)                               
      IF(D(IH+NSO).EQ.0.0) GO TO 2                                              
      CALL TDB2(NLI1,NSI1,NSI1,HS)                                              
      CALL TDB5(NLI2,NSI2,NSI2,HS)                                              
      CALL TDB4(NLO,NSO,NSO,HS)                                                 
      GO TO 2                                                                   
    1 D(IW+NLO) = D(IW+NLI1)                                                    
      D(IP+NSO) = D(IP+NSI1)                                                    
C     WRITE(6,5996) D(IT+NSI1)                                                  
C5996 FORMAT(' ','D(IT+NSO)=D(IT+NSI1) SKIP PREV CHANGE=',E16.8)                
      D(IT+NSO) = D(IT+NSI1)                                                    
      D(IH+NSO) = D(IH+NSI1)                                                    
      CALL TDB1(NLI1,NSI1,NSI1,HS)                                              
    2 IF(IFP.NE.1.OR.ICPP.NE.0) GO TO 99                                        
      CALL PIOP(-1,NLI1,NSI1,NSI1)                                              
      CALL PIOP(-2,NLI2,NSI2,NSI2)                                              
      CALL PIOP(-2,NLO,NSO,NSO)                                                 
      IF(D(IH+NSI1).NE.0.0.AND.D(IH+NSO).GT.HS) CALL HSOP(HS)                   
      IF(D(IP+NSI1).LE.D(IP+NSI2)) GO TO 99                                     
      CALL LINES(2)                                                             
      WRITE (OUT,1000)                                                          
 1000 FORMAT(1H0,5X,50H***WATER LINE PRESSURE LESS THAN AIR LINE PRESSUR        
     *E)                                                                        
   99 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF AN INJECTOR                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE INJTPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE  (SCR(1),NLI1), (SCR(2),NSI1), (SCR(3),NLI2)                  
     *, (SCR(4),NSI2), (SCR(5),NLO), (SCR(6),NSO)                               
     *, (SCR(7),NF)                                                             
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
      DATA IFTB/001/                                                            
C   1 COMP CODE                                                                 
C   2 INLET LEG NO                                                              
C   3 INLET STATION NO                                                          
C   4 WATER LEG NO                                                              
C   5 WATER STATION NO                                                          
C   6 OUTLET LEG NO                                                             
C   7 OUTLET STATION NO                                                         
      I = IACDB(7)                                                              
      ID(I+1) = ICV(1)                                                          
      NLI1 = ILEGN(ICV(2))                                                      
      ID(I+2) = NLI1                                                            
      CALL LEGRT(NLI1)                                                          
      NSI1 = ISTAN(ICV(3))                                                      
      ID(I+3) = NSI1                                                            
      CALL START(NSI1)                                                          
      NLI2 = ILEGN(ICV(4))                                                      
      ID(I+4) = NLI2                                                            
      CALL LEGRT(NLI2)                                                          
      NSI2 = ISTAN(ICV(5))                                                      
      ID(I+5) = NSI2                                                            
      CALL START(NSI2)                                                          
      NLO = ILEGN(ICV(6))                                                       
      ID(I+6) = NLO                                                             
      CALL LEGRS(NLO)                                                           
      NSO = ISTAN(ICV(7))                                                       
      ID(I+7) = NSO                                                             
      CALL STARS(NSO)                                                           
      CALL FTL(NLI1,IFTA)                                                       
      CALL FTL(NLI2,IFTB)                                                       
      CALL FRR(NLI1,NF)                                                         
      CALL FRS(NLO,NF)                                                          
      RETURN                                                                    
C     INJTPZ                                                                    
      END                                                                       
      SUBROUTINE INLTPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IP,C(28)), (IT,C(29)), (IH,C(30))              
     *, (IW,C(27)), (PASS,C(17)), (ISVT,C(46)), (ISV,C(47))                     
     *, (SCR(1),C(151)), (IFB,C(55))                                            
      INTEGER PASS                                                              
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (SCR(1),NL), (SCR(2),NS), (SCR(3),JSV)                        
     *, (SCR(4),NC), (SCR(5),LOCT), (SCR(6),P), (SCR(7),T)                      
     *, (SCR(8),TS), (SCR(9),V), (SCR(10),DSH)                                  
      COMMON /DC/ DZ(2),D(128001)                                                
      DIMENSION ID(2)                                                           
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION                                                                   
C   4 W                                                                         
C   5 P                                                                         
C   6 T                                                                         
C   7 H                                                                         
C   8 SV W                                                                      
C   9 SV P                                                                      
C  10 SV T                                                                      
C  11 SV H                                                                      
C  12 DSH                                                                       
      IENTRY = 0                                                                
      GO TO 1                                                                   
C*** ADD REFRIGERANT INLET AS ENTRY POINT                                       
C    TO PREVIOUS INLET SUBROUTINE                                               
      ENTRY RNLTPP                                                              
      IENTRY=1                                                                  
    1 IRCD = IRCDB(12)                                                          
      NL = ID(IRCD+2)                                                           
      NS = ID(IRCD+3)                                                           
      IF (PASS.EQ.1) CALL FLUIDP(NL)                                            
      LOCT = ID(IFB+NL)                                                         
      NC = ID(LOCT+1)                                                           
      LOCT = ID(LOCT+3)                                                         
      D(IW+NL) = D(ID(IRCD+4))                                                  
      D(IP+NS) = D(ID(IRCD+5))                                                  
      D(IT+NS) = D(ID(IRCD+6))                                                  
      D(IH+NS) = D(ID(IRCD+7))                                                  
    9 JSV = ID(IRCD+8)                                                          
      IF (JSV.EQ.0) GO TO 20                                                    
C*** FLOW RATE IS A STATE VARIABLE                                              
      IF (PASS.NE.1) GO TO 12                                                   
      ID(ISVT+JSV) = 1                                                          
      D(ISV+JSV) = D(IW+NL)                                                     
   12 D(IW+NL) = D(ISV+JSV)                                                     
   20 JSV = ID(IRCD+9)                                                          
C*** PRESSURE IS A STATE VARIABLE                                               
      IF (JSV.EQ.0) GO TO 30                                                    
      IF (PASS.NE.1) GO TO 22                                                   
      ID(ISVT+JSV) = 2                                                          
      D(ISV+JSV) = D(IP+NS)                                                     
   22 D(IP+NS) = D(ISV+JSV)                                                     
   30 JSV = ID(IRCD+10)                                                         
      IF (JSV.EQ.0) GO TO 40                                                    
C*** TEMPERATURE IS A STATE VARIABLE                                            
      IF (PASS.NE.1) GO TO 32                                                   
      ID(ISVT+JSV) = 3                                                          
      D(ISV+JSV) = D(IT+NS)                                                     
   32 D(IT+NS) = D(ISV+JSV)                                                     
   40 JSV = ID(IRCD+11)                                                         
      IF (JSV.EQ.0) GO TO 50                                                    
C*** ENTHALPY OR HUMIDITY IS A STATE VARIABLE                                   
      IF (PASS.NE.1) GO TO 42                                                   
      ID(ISVT+JSV) = 4                                                          
C*** IF FLUID TYPE IS 3 (REFRIGERANT), S.V. TYPE = 10 (ENTHALPY)                
      IF(IENTRY.EQ.1)ID(ISVT+JSV)=10                                            
      D(ISV+JSV) = D(IH+NS)                                                     
   42 D(IH+NS) = D(ISV+JSV)                                                     
   50 IF(NC.NE.3) GO TO 99                                                      
C*** RE-COMPUTE THE TEMPERATURE IF FLUID TYPE IS 3 (REFRIGERANT)                
C    TO AVOID ATTEMPTING TO DEFINE THE FLUID STATE BY PRESSURE                  
C    AND TEMPERATURE WHEN THE FLUID IS IN THE TWO-PHASE REGION                  
      QUAL=VQUALH('RNLTPP  ',LOCT,D(IP+NS),D(IH+NS))                            
C$** ADDED CALL TO VTLIQ (DYER FUNCTION)   CALL TO VTLIQ IS IN NEWZ ALSO        
      IF(QUAL.LT.0.0)D(IT+NS)=VTLIQ(LOCT,D(IP+NS),D(IH+NS))                     
      IF(QUAL.GE.0.0.AND.QUAL.LE.1.0)D(IT+NS)=VTS(LOCT,D(IP+NS))                
      IF(QUAL.GT.1.0)CALL VTAV2(LOCT,D(IT+NS),V,D(IP+NS),D(IH+NS))              
   99 CONTINUE                                                                  
      RETURN                                                                    
C     INLTPP, RNLTPP                                                            
      END                                                                       
C$** ADDED  "RNLTPZ"     REFRIGERANT INLET AS ENTRY POINT                       
      SUBROUTINE INLTPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL), (SCR(2),NS), (SCR(3),IOP)                        
     *, (SCR(4),NC)                                                             
      COMMON /DC/ DZ(2),D(128001)                                                
      DIMENSION ID(2)                                                           
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION                                                                   
C   4 W                                                                         
C   5 P                                                                         
C   6 T                                                                         
C   7 H                                                                         
C   8 SV W                                                                      
C   9 SV P                                                                      
C  10 SV T                                                                      
C  11 SV H                                                                      
C  12 DSH                                                                       
      IENTRY = 0                                                                
      GO TO 1                                                                   
      ENTRY RNLTPZ                                                              
      IENTRY = 1                                                                
    1 I = IACDB(12)                                                             
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      CALL LEGRS(NL)                                                            
      NS = ISTAN(ICV(3))                                                        
      ID(I+3) = NS                                                              
      CALL STARS(NS)                                                            
      CALL FLUIDZ(NL,ICV(9),ICV(10))                                            
      CALL FTR(NL,NC)                                                           
      ID(I+4) = IPARM(ICV(5))                                                   
      ID(I+5) = IPARM(ICV(6))                                                   
      ID(I+6) = IPARM(ICV(7))                                                   
      ID(I+7) = IPARM(ICV(8))                                                   
C$   AS IN NEWZ ADDED LINE BELOW                                                
      IF (IENTRY.EQ.1) GO TO 2                                                  
C$   AS IN NEWZ GOT RID OF 2 LINES BELOW                                        
C     IF (NC.EQ.3) ID(I+7) = 0                                                  
C     ID(I+12) = IPARM(ICV(11))                                                 
C                                                                               
      IF (NC.EQ.1) ID(I+7) = 0                                                  
    2 IF (ICV(4).EQ.0) GO TO 99                                                 
      IOP = ICV(4)/1000                                                         
      IF (IOP.NE.0) ID(I+8) = IASV(1)                                           
      IOP = 100*(ICV(4)/100)                                                    
      IOP = IOP-1000*(IOP/1000)                                                 
      IF (IOP.NE.0) ID(I+9) = IASV(2)                                           
      IOP = 10*(ICV(4)/10)                                                      
      IOP = IOP-100*(IOP/100)                                                   
      IF (IOP.NE.0) ID(I+10) = IASV(3)                                          
      IOP = ICV(4)-10*(ICV(4)/10)                                               
      IF(IOP.NE.0 .AND. NC.EQ.2) ID(I+11) = IASV(4)                             
C$   ADDED LINE BELOW                                                           
      IF(IOP.NE.0.AND.NC.EQ.3) ID(I+11) = IASV(10)                              
   99 CONTINUE                                                                  
      RETURN                                                                    
C     INLTPZ, RLNTPZ                                                            
      END                                                                       
C$** ADDED  "RTLTPP"     REFRIGERANT OUTLET AS ENTRY POINT                      
      SUBROUTINE INSLSP                                                 15437010
      COMMON /CC/ C(600)                                                15438010
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151)), (IW,C(27)) 15439010
     *, (IP,C(28)), (IT,C(29)), (WTC,C(102)), (CUC,C(104))              15440010
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))        15441010
     *, (IGA,C(35)), (IH,C(30))                                         15442010
      DIMENSION SCR(30)                                                 15443010
      INTEGER OUT                                                       15444010
      EQUIVALENCE         (SCR( 1),INT   ), (SCR( 2),INM   )            15445010
     *, (SCR( 3),IMT   ), (SCR( 4),THK   ), (SCR( 5),TD    )            15446010
     *, (SCR( 6),NL    ), (SCR( 7),NSI   ), (SCR( 8),NSO   )            15447010
     *, (SCR( 9),WTF   ), (SCR(10),CUF   ), (SCR(11),RI    )            15448010
     *, (SCR(12),DRF   )                                                15449010
      COMMON /DC/ DZ(2),D(128001)                                       15450023
      DIMENSION ID(128001)                                              15451023
      EQUIVALENCE (ID(1),D(1))                                          15452010
C   1 COMP CODE                                                         15453010
C   2 WTM                                                               15454010
C   3 CUM                                                               15455010
C   4 RI                                                                15456010
C   5 DRM                                                               15457010
C   6 INS TYPE                                                          15458010
C   7 INS METHOD                                                        15459010
C   8 THK,THK,THK                                                       15460010
C   9 DD,DD,A                                                           15461010
C  10 L,L,PA                                                            15462010
C  11 TD,TA,TA                                                          15463010
C  12 NSI,NL,TC                                                         15464010
C  13 TS,NSI,TS                                                         15465010
C  14    NSO,CF                                                         15466010
C  15 IMT,IMT,IMT                                                       15467010
      IRCD = IRCDB(15)                                                  15468010
      WTF = D(ID(IRCD+2))                                               15469010
      CUF = D(ID(IRCD+3))                                               15470010
      RI = D(ID(IRCD+4))                                                15471010
      DRF = D(ID(IRCD+5))                                               15472010
      INT = ID(IRCD+6)                                                  15473010
      INM = ID(IRCD+7)                                                  15474010
      IMT = ITLUP(ID(IRCD+15))                                          15475010
      THK = D(ID(IRCD+8))                                               15476010
      II = 0                                                            15477010
      IF (THK.NE.0.0) GO TO 200                                         15478010
      IF (INT.EQ.2) GO TO 100                                           15479010
      IF (INT.EQ.3) GO TO 4                                             15480010
      TD = D(ID(IRCD+11))                                               15481010
      IF (TD.EQ.0.0) TD = D(IT+ID(IRCD+12))                             15482010
      GO TO (11,11,13,14), INM                                          15483010
   11 THK = (0.045*TD-20.7)/(D(ID(IRCD+13))-0.2*TD-416.0)               15484010
      GO TO 10                                                          15485010
   13 THK = (0.064*TD-29.44)/(D(ID(IRCD+13))-0.2*TD-416.0)              15486010
      GO TO 10                                                          15487010
   14 THK = (0.06*TD-27.6)/(D(ID(IRCD+13))-0.2*TD-416.0)                15488010
   10 IF (THK.GT.0.0 .AND. THK.LE.1.0) GO TO 9                          15489010
      II = 1                                                            15490010
      THK = 1.0                                                         15491010
    9 GO TO (201,202,201,204), INM                                      15492010
    4 THK = 31.5*POLYI(2,D(IMT+5),0.5*(D(ID(IRCD+13))+D(ID(IRCD+12))))* 15493010
     *(D(ID(IRCD+13))-D(ID(IRCD+12)))/((0.9*1.714E-9*(D(ID(IRCD+12))+   15494010
     *D(ID(IRCD+11)))*(D(ID(IRCD+12))**2+D(ID(IRCD+11))**2)+            15495010
     *1.5*SQRT(D(ID(IRCD+10))/14.7))*(D(ID(IRCD+12))-D(ID(IRCD+11))))   15496010
      GO TO 212                                                         15497010
  100 NL = ID(IRCD+12)                                                  15498010
      NSI = ID(IRCD+13)                                                 15499010
      NSO = ID(IRCD+14)                                                 15500010
      TD = 0.5*(D(IT+NSI)+D(IT+NSO))                                    15501010
      THK = D(ID(IRCD+9))*(EXP(8.722E-3*POLYI(2,D(IMT+5),0.5*(TD+       15502010
     *D(ID(IRCD+11))))*(TD-D(ID(IRCD+11)))*D(ID(IRCD+10))/(D(IW+NL)*    15503010
     * SHP(NL,D(IP+NSI),D(IT+NSI),D(IH+NSI)) * (D(IT+NSI) - D(IT+NSO))))15504010
     * - 1.0) / 2.0                                                     15505010
  103 IF (INM.EQ.2) GO TO 101                                           15506010
      WTC = (D(ID(IRCD+9))+THK)*(6.1078E-4+1.0917E-3*THK)*D(ID(IRCD+10))15507010
      GO TO 102                                                         15508010
  101 WTC = (D(ID(IRCD+9))+THK)*(9.725E-3+1.275E-3*THK)*D(ID(IRCD+10))  15509010
     *-4.8625E-3*D(ID(IRCD+9))*D(ID(IRCD+10))                           15510010
  102 IF (WTF.NE.0.0) WTC = WTC*WTF                                     15511010
      CUC = 0.002378*D(ID(IRCD+9))*D(ID(IRCD+10))                       15512010
      IF (CUF.NE.0.0) CUC = CUC*CUF                                     15513010
      WTDC = 0.15*WTC                                                   15514010
      GO TO 250                                                         15515010
  200 IF (INT.EQ.3) GO TO 212                                           15516010
      IF (INT.EQ.2) GO TO 103                                           15517010
      GO TO (201,202,201,204), INM                                      15518010
  201 WTC = ((1.9667E-3*D(IMT+1)*D(ID(IRCD+9))+7.0E-4*D(IMT+1)+7.7E-2)  15519010
     **(THK-0.25)+4.25E-4*D(IMT+1)*D(ID(IRCD+9))+0.01*D(ID(IRCD+9))     15520010
     *+2.0833E-4*D(IMT+1)+1.59167E-2)*D(ID(IRCD+10))                    15521010
      IF (WTF.NE.0.0) WTC = WTC*WTF                                     15522010
      WTDC = 0.055*WTC                                                  15523010
      GO TO 205                                                         15524010
  202 WTC = ((5.5E-3*THK+5.8833E-3)*D(ID(IRCD+9))+0.015*THK-5.8333E-4)  15525010
     **D(ID(IRCD+10))                                                   15526010
      IF (WTF.NE.0.0) WTC = WTC*WTF                                     15527010
      WTDC = 0.149*WTC                                                  15528010
      GO TO 205                                                         15529010
  204 WTC = ((6.0667E-3*D(ID(IRCD+9))+8.6E-3)*(THK-0.25)+2.35E-3        15530010
     **D(ID(IRCD+9))+1.0667E-3)*D(ID(IRCD+10))                          15531010
      IF (WTF.NE.0.0) WTC = WTC*WTF                                     15532010
      WTDC = 0.167*WTC                                                  15533010
  205 CUC = D(ID(IRCD+10))*(9.583E-3*THK*(D(ID(IRCD+9))+THK)+0.04425)   15534010
      IF (CUF.NE.0.0) CUC = CUC*CUF                                     15535010
      GO TO 250                                                         15536010
  212 WTC = (3.646E-4*THK/D(ID(IRCD+14))+5.104E-4)*D(ID(IRCD+9))        15537010
      IF(WTF.NE.0.0) WTC = WTC*WTF                                      15538010
      CUC = (6.944E-4+7.9167E-5*THK)*D(ID(IRCD+9))                      15539010
      IF (CUF.NE.0.0) CUC = CUC*CUF                                     15540010
      WTDC = 0.15*WTC                                                   15541010
  250 WTIC = 0.205*WTC                                                  15542010
      RIC = 0.00001                                                     15543010
      IF (RI.NE.0.0) RIC = RI                                           15544010
      DRC = 1.0                                                         15545010
      IF (DRF.NE.0.0) DRC = DRC*DRF                                     15546010
      D(IGA+71) = THK                                                   15547010
      CALL SSA                                                          15548010
      CALL SCO                                                          15549010
      CALL LINES(2)                                                     15550010
      WRITE (OUT,1000) THK                                              15551010
 1000 FORMAT(1H0,5X,3HTHK,F7.2)                                         15552010
      IF (II.EQ.0) GO TO 99                                             15553010
      CALL LINES(2)                                                     15554010
      WRITE (OUT,1010)                                                  15555010
 1010 FORMAT(1H0,5X,17HTHICKNESS LIMITED)                               15556010
   99 CONTINUE                                                          15557010
      RETURN                                                            15558010
C     INSLSP                                                            15559010
      END                                                               15560010
*DECK,INSLSZ                                                            12110010
      SUBROUTINE INSLSZ                                                 12111010
      COMMON /CC/ C(600)                                                12112010
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))             12113010
     *, (SCR(1),C(151))                                                 12114010
      DIMENSION ICV(18), SCR(30)                                        12115010
      INTEGER CERR, OUT                                                 12116010
      EQUIVALENCE (SCR(1),NL)                                           12117010
      COMMON /DC/ DZ(2),D(128001)                                       12118023
      DIMENSION ID(128001)                                              12119023
      EQUIVALENCE (ID(1),D(1))                                          12120010
      DATA IFTA/010/                                                    12121010
C   1 COMP CODE                                                         12122010
C   2 WTM                                                               12123010
C   3 CUM                                                               12124010
C   4 RI                                                                12125010
C   5 DRM                                                               12126010
C   6 INS TYPE                                                          12127010
C   7 INS METHOD                                                        12128010
C   8 THK,THK,THK                                                       12129010
C   9 DD,DD,A                                                           12130010
C  10 L,L,PA                                                            12131010
C  11 TD,TA,TA                                                          12132010
C  12 NSI,NL,TC                                                         12133010
C  13 TS,NSI,TS                                                         12134010
C  14    NSO,CF                                                         12135010
C  15 IMT,IMT,IMT                                                       12136010
      I = IACDB(15)                                                     12137010
      ID(I+1) = ICV(1)                                                  12138010
      ID(I+2) = IPARM(ICV(2))                                           12139010
      ID(I+3) = IPARM(ICV(3))                                           12140010
      ID(I+4) = IPARM(ICV(4))                                           12141010
      ID(I+5) = IPARM(ICV(5))                                           12142010
      ID(I+6) = ICV(6)                                                  12143010
      ID(I+7) = ICV(7)                                                  12144010
      IF (ICV(6).GE.1 .AND. ICV(6).LE.3) GO TO 5                        12145010
      CERR = CERR+1                                                     12146010
      CALL LINES(2)                                                     12147010
      WRITE (OUT,1000) CERR,ICV(6)                                      12148010
 1000 FORMAT(6H0ERROR,I6,5X,23HINVALID INSULATION TYPE,I6)              12149010
      GO TO 99                                                          12150010
    5 IF (ICV(6).GT.1) GO TO 6                                          12151010
      IF (ICV(7).GE.1 .AND. ICV(7).LE.4) GO TO 35                       12152010
    7 CERR = CERR+1                                                     12153010
      CALL LINES(2)                                                     12154010
      WRITE (OUT,1010) CERR,ICV(7)                                      12155010
 1010 FORMAT(6H0ERROR,I6,5X,25HINVALID INSULATION METHOD,I6)            12156010
      GO TO 35                                                          12157010
    6 IF (ICV(7).LT.0 .OR. ICV(7).GT.2) GO TO 7                         12158010
   35 ID(I+8) = IPARM(ICV(8))                                           12159010
      ID(I+9) = IPARM(ICV(9))                                           12160010
      ID(I+10) = IPARM(ICV(10))                                         12161010
      ID(I+11) = IPARM(ICV(11))                                         12162010
      IF (ICV(6).GT.1) GO TO 10                                         12163010
      IF (ICV(12).NE.0) ID(I+12) = ISTAN(ICV(12))                       12164010
      ID(I+13) = IPARM(ICV(13))                                         12165010
      GO TO 16                                                          12166010
   10 IF (ICV(6).EQ.3) GO TO 15                                         12167010
      NL = ILEGN(ICV(12))                                               12168010
      CALL FTL(NL,IFTA)                                                 12169010
      ID(I+12) = NL                                                     12170010
      ID(I+13) = ISTAN(ICV(13))                                         12171010
      ID(I+14) = ISTAN(ICV(14))                                         12172010
      GO TO 16                                                          12173010
   15 ID(I+12) = IPARM(ICV(12))                                         12174010
      ID(I+13) = IPARM(ICV(13))                                         12175010
      ID(I+14) = IPARM(ICV(14))                                         12176010
   16 ID(I+15) = ITIDN(ICV(15),20)                                      12177010
   99 CONTINUE                                                          12178010
      RETURN                                                            12179010
C     INSLSZ                                                            12180010
      END                                                               12181010
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF LINE                                                      
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE LINEPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/011/                                                            
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
C$ 18 HO H OUTSIDE THE DUCT                                                     
C                                                                               
C  DETERMINE WHERE TO START STORING THE NEXT 18 VALUES FOR THE LINE             
C                                                                               
      I = IACDB(18)                                                             
C                                                                               
C  STORE THE TYPE OF COMPONENT, SEE "PD" ARRAY FOR TYPE NUMBERS                 
C                                                                               
      ID(I+1) = ICV(1)                                                          
C                                                                               
C  STORE THE LEG NUMBER?                                                        
C                                                                               
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      CALL LEGRT(NL)                                                            
C                                                                               
C  STORE THE INLET STATION NUMBER?                                              
C                                                                               
      NSI = ISTAN(ICV(3))                                                       
      ID(I+3) = NSI                                                             
      CALL START(NSI)                                                           
C                                                                               
C  STORE THE OUTLET STATION NUMBER?                                             
C                                                                               
      NSO = ISTAN(ICV(4))                                                       
      ID(I+4) = NSO                                                             
      CALL STARS(NSO)                                                           
      CALL FTL(NL,IFTA)                                                         
C                                                                               
C  STORE THE DELTA P OPTION INDICATOR                                           
C                                                                               
      ID(I+5) = ICV(5)                                                          
C                                                                               
C  STORE THE TABLE OR EQUATION OPTION FOR DELTA P                               
C                                                                               
      ID(I+6) = ICV(6)                                                          
      IF (ICV(13).EQ.0 .AND. ICV(14).EQ.0) GO TO 4                              
      ID(I+13) = ITIDN(ICV(13),11)                                              
      ID(I+14) = ITIDN(ICV(14),12)                                              
      ID(I+15) = IPARM(ICV(15))                                                 
      ID(I+18) = IPARM(ICV(18))                                                 
      GO TO 5                                                                   
    4 IF (ICV(16).EQ.0) GO TO 5                                                 
      ID(I+16) = ITIDN(ICV(16),13)                                              
      ID(I+17) = IPARM(ICV(17))                                                 
    5 IF (ICV(6).EQ.1) GO TO 1                                                  
      IF (ICV(6).EQ.2) GO TO 2                                                  
      ID(I+7) = ITIDN(ICV(7),1)                                                 
      GO TO 99                                                                  
    2 ID(I+7) = IPARM(ICV(9))                                                   
      GO TO 3                                                                   
    1 ID(I+7) = ITIDN(ICV(7),3)                                                 
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = ITIDN(ICV(9),2)                                                 
      ID(I+10) = IPARM(ICV(10))                                                 
    3 ID(I+11) = IPARM(ICV(11))                                                 
      ID(I+12) = IPARM(ICV(12))                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     LINEPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      A LINE                                                                   
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE LINESP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151)), (IW,C(27))         
     *, (IP,C(28)),  (IT,C(29)), (WTC,C(102)), (CUC,C(104)), (IGA,C(35))        
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))                
     *, (PASS,C(17)), (NIT,C(76)), (PF,C(77)), (NWT,C(78)), (ITAD,C(54))        
     *, (IDS,C(5)), (ICONV,C(79)), (IFP,C(22)), (CERR,C(16))                    
      DIMENSION SCR(30)                                                         
      INTEGER OUT, PASS, CERR                                                   
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO), (SCR(4),DIA)         
     *, (SCR(5),THK), (SCR(6),VOL), (SCR(7),ILT), (SCR(8),C1)                   
     *, (SCR(9),C2), (SCR(10),WTF,CUF,RI,DRF), (SCR(11),TA)                     
     *, (SCR(12),DENS), (SCR(13),VSC), (SCR(14),EV), (SCR(15),EVS)              
     *, (SCR(16),DIAS), (SCR(17),IS), (SCR(18),JS), (SCR(19),PA)                
     *, (SCR(20),WT), (SCR(21),IDSS), (SCR(22),F), (SCR(23),AKT)                
     *, (SCR(24),EVP), (SCR(25),II), (SCR(26),VEL), (SCR(27),AMN)               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA DUL/10.0/, DLL/0.1/, DIV/2.0/, ERL/0.01/                             
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 WTF                                                                       
C   6 CUF                                                                       
C   7 RI                                                                        
C   8 DRF                                                                       
C   9 DIA                                                                       
C  10 LENGTH                                                                    
C  11 THK                                                                       
C  12 Z                                                                         
C  13 IF                                                                        
C  14 C                                                                         
C  15 KT                                                                        
C  16 ILT                                                                       
      IRCD = IRCDB(16)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      DIA = D(ID(IRCD+9))                                                       
      IDSS = IDS                                                                
      ICONV = 0                                                                 
      TA = 0.5*(D(IT+NSI)+D(IT+NSO))                                            
      PA = 0.5*(D(IP+NSI)+D(IP+NSO))                                            
      DENS = DEN(NL,PA,TA)                                                      
      IF (DIA.NE.0.0) GO TO 200                                                 
      VSC = VIS(NL,PA,TA)                                                       
      DIA = DIV                                                                 
      F = 0.0                                                                   
      AKT = 0.0                                                                 
      PASS = 1                                                                  
      IFP = 0                                                                   
      EVP = -DENS*D(ID(IRCD+12))/1728.0-D(IP+NSI)+D(IP+NSO)                     
      ASSIGN 7 TO IS                                                            
      GO TO 300                                                                 
    7 IF (IDS.EQ.0) GO TO 71                                                    
      ASSIGN 71 TO JS                                                           
      GO TO 400                                                                 
   71 CONTINUE                                                                  
      WT = 1.0                                                                  
      DO 15 II = 1,NIT                                                          
      IF (IDS.LT.0) IDS = 2                                                     
      IF (IDSS.LT.0 .AND. II.GT.IABS(IDSS)) IDS = 0                             
      IF (II.EQ.NIT) IDS = 2                                                    
      D(IGA+1) = FLOAT(PASS)                                                    
      PASS = PASS+1                                                             
      IF (ID(ITAD+NWT).NE.0) WT = TLUP(NWT)                                     
      DIAS = DIA                                                                
      EVS = EV                                                                  
      DIA = DIAS*PF                                                             
      ASSIGN 8 TO IS                                                            
      GO TO 300                                                                 
    8 IF (IDS.LE.2) GO TO 81                                                    
      ASSIGN 81 TO JS                                                           
      GO TO 400                                                                 
   81 CONTINUE                                                                  
      IF ((EV-EVS).EQ.0.0) GO TO 16                                             
      DIA = DIAS-WT*EVS*(DIA-DIAS)/(EV-EVS)                                     
      IF (DIA.GT.DUL) DIA = DUL                                                 
      IF (DIA.LT.DLL) DIA = DLL                                                 
      ASSIGN 9 TO IS                                                            
      GO TO 300                                                                 
    9 IF (IDS.EQ.0) GO TO 91                                                    
      ASSIGN 91 TO JS                                                           
      GO TO 400                                                                 
   91 CONTINUE                                                                  
      ICONV = 0                                                                 
      IF (ABS(EV).GT.ERL) ICONV = 1                                             
      IF (ICONV.EQ.0) GO TO 199                                                 
   15 CONTINUE                                                                  
   17 WTC = 0.0                                                                 
      RIC = 0.0                                                                 
      CUC = 0.0                                                                 
      DRC = 0.0                                                                 
      GO TO 98                                                                  
   16 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1009) CERR                                                     
 1009 FORMAT(6H0ERROR,I6,5X,15HSINGULAR MATRIX)                                 
      ICONV = 1                                                                 
      GO TO 17                                                                  
  300 D(IGA+21) = 0.2546*D(IW+NL)/(VSC*DIA)                                     
      D(IGA+57) = DIA                                                           
      IF (ID(IRCD+13).NE.0) F = TLUP(ID(IRCD+13))                               
      IF (ID(IRCD+15).NE.0) AKT = TLUP(ID(IRCD+15))                             
      EV = 1.008E-3*(4.0*F*D(ID(IRCD+10))/DIA+D(ID(IRCD+14))*AKT)               
     * *D(IW+NL)**2/(DENS*DIA**4)+EVP                                           
      GO TO IS, (7,8,9,200)                                                     
  400 WRITE (OUT,1001) PASS                                                     
 1001 FORMAT(6H0PASS ,I6)                                                       
      WRITE (OUT,1002) DIA,EV,D(IGA+21)                                         
 1002 FORMAT(6H0S.V. ,E12.5,5X,5HE.V.7,E12.5,5X,3HRE ,E12.5)                    
C1002 FORMAT(6H0S.V. ,E12.5,5X,5HE.V. ,E12.5,5X,3HRE ,E12.5)                    
      GO TO JS, (71,81,91)                                                      
  199 ASSIGN 200 TO IS                                                          
      IFP = 1                                                                   
      GO TO 300                                                                 
  200 ILT = ID(IRCD+16)                                                         
      THK = D(ID(IRCD+11))                                                      
      GO TO (1,2,3,4,5), ILT                                                    
    1 IF (THK.NE.0.0)  GO TO 10                                                 
      THK = 0.035                                                               
      IF (DIA.GT.3.5) THK = 0.042                                               
   10 WTC = 0.34*DIA*THK*D(ID(IRCD+10))                                         
      IF (DIA.GT.3.5) GO TO 11                                                  
      C1 = 0.0097                                                               
      C2 = 0.0191                                                               
      GO TO 12                                                                  
   11 C1 = 0.029                                                                
      C2 = -0.0477                                                              
   12 RIC = 0.01164                                                             
      GO TO 100                                                                 
    2 IF (THK.NE.0.0) GO TO 20                                                  
      THK = 0.00677*DIA+0.00322                                                 
      IF (THK.LT.0.01) THK = 0.01                                               
   20 WTC = (2.4*DIA+1.25)*THK*D(ID(IRCD+10))                                   
      IF (DIA.GT.2.5) GO TO 21                                                  
      C1 = 0.103                                                                
      C2 = 0.026                                                                
      GO TO 22                                                                  
   21 C1 = 0.231                                                                
      C2 = -0.295                                                               
   22 RIC = 0.01164                                                             
      GO TO 100                                                                 
    3 IF (DIA.GT.3.5) GO TO 30                                                  
      WTC = 0.0137*DIA*D(ID(IRCD+10))                                           
      C1 = 0.011                                                                
      C2 = 0.0295                                                               
      GO TO 31                                                                  
   30 WTC = (0.0179*DIA-0.0147)*D(ID(IRCD+10))                                  
      C1 = 0.0254                                                               
      C2 = -0.0233                                                              
   31 RIC = 0.00291                                                             
      GO TO 100                                                                 
    4 IF (THK.NE.0.0) GO TO 40                                                  
      THK = 0.00677*DIA+0.00322                                                 
   40 WTC = 0.7*(2.4*DIA+1.28)*THK*D(ID(IRCD+10))                               
      IF (DIA.GT.2.5) GO TO 41                                                  
      C1 = 0.103                                                                
      C2 = 0.026                                                                
      GO TO 42                                                                  
   41 C1 = 0.231                                                                
      C2 = -0.295                                                               
   42 RIC = 0.01164                                                             
      GO TO 100                                                                 
    5 IF (THK.NE.0.0) GO TO 50                                                  
      THK = 0.028                                                               
   50 WTC = 0.3071*(DIA+THK)*THK*D(ID(IRCD+10))                                 
      IF (DIA.GT.3.5) GO TO 51                                                  
      C1 = 0.0097                                                               
      C2 = 0.0199                                                               
      GO TO 52                                                                  
   51 C1 = 0.029                                                                
      C2 = -0.0477                                                              
   52 RIC = 0.00285                                                             
  100 WTF = D(ID(IRCD+5))                                                       
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      IF (ILT.EQ.4 .OR. ILT.EQ.5) GO TO 101                                     
      WTIC = 3.45E-4*(DIA*D(ID(IRCD+10)))**1.58                                 
      GO TO 102                                                                 
  101 WTIC = 0.0                                                                
  102 WTDC = 0.15*WTC+0.158*WTIC                                                
      CUC  = (C1*DIA+C2)*D(ID(IRCD+10))                                         
      CUF = D(ID(IRCD+6))                                                       
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      RI = D(ID(IRCD+7))                                                        
      IF (RI.NE.0.0) RIC = RI                                                   
      DRC = 1.0                                                                 
      DRF = D(ID(IRCD+8))                                                       
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      CALL SSA                                                                  
   98 IDS = IDSS                                                                
      CALL SCO                                                                  
      CALL SCI(NL,NSI,NSO)                                                      
      IF (ICONV.EQ.0) GO TO 97                                                  
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1010) CERR                                                     
 1010 FORMAT(6H0ERROR,I6,5X,14HNON CONVERGENT)                                  
      GO TO 99                                                                  
   97 CALL LINES(2)                                                             
      WRITE (OUT,1000) DIA                                                      
 1000 FORMAT(1H0,5X,1HD,F9.2)                                                   
      D(IGA+71) = DIA                                                           
      VEL = 3.055774*D(IW+NL)/(DENS*DIA**2)                                     
      WRITE (OUT,1021) VEL                                                      
 1021 FORMAT(1H+,31X,1HV,F9.2)                                                  
      D(IGA+72) = VEL                                                           
      IF (ILT.EQ.3) GO TO 96                                                    
      WRITE (OUT,1020) THK                                                      
 1020 FORMAT(1H+,18X,3HTHK,F7.3)                                                
   96 IF (ILT.EQ.5) GO TO 95                                                    
      AMN = VEL/SOS(NL,D(IP+NSI),D(IT+NSI))                                     
      WRITE (OUT,1022) AMN                                                      
 1022 FORMAT(1H+,44X,1HM,F9.4)                                                  
      D(IGA+73) = AMN                                                           
      GO TO 99                                                                  
   95 VOL = 0.7854*DIA**2*D(ID(IRCD+10))                                        
      WRITE (OUT,1003) VOL                                                      
 1003 FORMAT(1H+,57X,2HVL,F8.1)                                                 
      D(IGA+74) = VOL                                                           
   99 CONTINUE                                                                  
      RETURN                                                                    
C     LINESP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE LINESZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
     *, (SCR(1),C(151))                                                         
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER OUT,CERR                                                          
      EQUIVALENCE (SCR(1),NL)                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/, IFTB/001/                                                 
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 WTF                                                                       
C   6 CUF                                                                       
C   7 RI                                                                        
C   8 DRF                                                                       
C   9 DIA                                                                       
C  10 LENGTH                                                                    
C  11 THK                                                                       
C  12 Z                                                                         
C  13 IF                                                                        
C  14 C                                                                         
C  15 KT                                                                        
C  16 ILT                                                                       
      I = IACDB(16)                                                             
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      ID(I+3) = ISTAN(ICV(3))                                                   
      ID(I+4) = ISTAN(ICV(4))                                                   
      ID(I+5) = IPARM(ICV(5))                                                   
      ID(I+6) = IPARM(ICV(6))                                                   
      ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      ID(I+10) = IPARM(ICV(10))                                                 
      ID(I+11) = IPARM(ICV(11))                                                 
      ID(I+12) = IPARM(ICV(12))                                                 
      IF (ICV(13).NE.0) ID(I+13) = ITIDN(ICV(13),3)                             
      ID(I+14) = IPARM(ICV(14))                                                 
      IF (ICV(15).NE.0) ID(I+15) = ITIDN(ICV(15),2)                             
      ID(I+16) = ICV(16)                                                        
      IF (ICV(16).GT.0 .AND. ICV(16).LE.5)  GO TO 98                            
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,ICV(16)                                             
 1000 FORMAT(6H0ERROR,I6,5X,17HINVALID LINE TYPE,I6)                            
      GO TO 99                                                                  
   98 IF (ICV(16).EQ.5) GO TO 97                                                
      CALL FTL(NL,IFTA)                                                         
      GO TO 99                                                                  
   97 CALL FTL(NL,IFTB)                                                         
   99 CONTINUE                                                                  
      RETURN                                                                    
C     LINESZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      A FLUID LOOP                                                             
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE LPSPP                                                          
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,  C(7)),                                                 
     *            (CERR, C(16)),                                                
     *            (PASS, C(17)),                                                
     *            (IW,   C(27)),                                                
     *            (IP,   C(28)),                                                
     *            (IT,   C(29)),                                                
     *            (IH,   C(30)),                                                
     *            (IRCD, C(45)),                                                
     *            (ISVT, C(46)),                                                
     *            (ISV,  C(47)),                                                
     *            (IEVT, C(48)),                                                
     *            (IEV,  C(49)),                                                
     *            (IFB,  C(55))                                                 
      DIMENSION SCR (30)                                                        
      INTEGER CERR, OUT, PASS                                                   
      EQUIVALENCE (SCR(1),NLI),                                                 
     *            (SCR(2),NSI),                                                 
     *            (SCR(3),NLO),                                                 
     *            (SCR(4),NSO),                                                 
     *            (SCR(5),ISV1,IEV1),                                           
     *            (SCR(6),ISV2,IEV2),                                           
     *            (SCR(7),ISV3,IEV3),                                           
     *            (SCR(8),ISV4,IEV4),                                           
     *            (SCR(9),IRCD2),                                               
     *            (SCR(10),NC),                                                 
     *            (SCR(11),LOCT),                                               
     *            (SCR(12),P),                                                  
     *            (SCR(13),T),                                                  
     *            (SCR(14),TS),                                                 
     *            (SCR(15),V),                                                  
     *            (SCR(16),DSH)                                                 
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
                                                                                
C   1 COMP CODE                                                                 
C   2 LEG NO.- LOOP START                                                       
C   3 STATION NO. - LOOP START                                                  
C   4 LEG NO. - LOOP END                                                        
C   5 STATION NO. - LOOP END                                                    
C   6 IRCD NO. 2                                                                
C   7 SV W / EV W                                                               
C   8 SV P / EV P                                                               
C   9 SV T / EV T                                                               
C  10 SV H / EV H                                                               
C  11 W (LOOPS ONLY)                                                            
C  12 P (LOOPS ONLY)                                                            
C  13 T (LOOPS ONLY)                                                            
C  14 H (LOOPS ONLY)                                                            
C  15 DSH                                                                       
                                                                                
      IE = 1                                                                    
      GO TO 1                                                                   
C                                                                               
C$** ENTRY POINT FOR LOOP END                                                   
C                                                                               
      ENTRY LPEPP                                                               
      IE = 2                                                                    
C                                                                               
C$** SET UP INTERNAL VARIABLES IN TERMS OF STORED ARRAY                         
C                                                                               
    1 IRCD = IRCDB(15)                                                          
      NLI = ID(IRCD+2)                                                          
      NSI = ID(IRCD+3)                                                          
      NLO = ID(IRCD+4)                                                          
      NSO = ID(IRCD+5)                                                          
      IRCD2 = ID(IRCD+6)                                                        
                                                                                
C$    WRITE(6,7777)IRCD,NLI,NSI                                                 
 7777 FORMAT(' LPSPP-1  IRCD = ',I5,' NLI = ',I5,' NSI = ',I5)                  
C$    WRITE(6,7776)NLO,NSO,IRCD2                                                
 7776 FORMAT(' LPSPP-2  NLO = ',I5,' NSO = ',I5,' IRCD2 = ',I5)                 
C                                                                               
C    PROCESS FOR LOOP START                                                     
C                                                                               
      IF (IE.EQ.1) THEN                                                         
         IF (PASS.EQ.1) CALL FLUIDP(NLI)                                        
         LOCT = ID(IFB+NLI)                                                     
         NC = ID(LOCT+1)                                                        
         LOCT = ID(LOCT+3)                                                      
C$    WRITE(6,7772)LOCT                                                         
 7772 FORMAT(' LPSPP-3  LOCT = ',I5)                                            
C                                                                               
C$** SET UP W,P,T,H PER INPUT DATA                                              
C                                                                               
         D(IW+NLI) = D(ID(IRCD+11))                                             
         D(IP+NSI) = D(ID(IRCD+12))                                             
         D(IT+NSI) = D(ID(IRCD+13))                                             
         D(IH+NSI) = D(ID(IRCD+14))                                             
C                                                                               
C$** SET UP THE STATE VARIABLES                                                 
C                                                                               
    9    ISV1 = ID(IRCD+7)                                                      
         ISV2 = ID(IRCD+8)                                                      
         ISV3 = ID(IRCD+9)                                                      
         ISV4 = ID(IRCD+10)                                                     
         IF (ISV1.EQ.0) GO TO 20                                                
         IF (PASS.NE.1) GO TO 12                                                
         ID(ISVT+ISV1) = 1                                                      
         D(ISV+ISV1) = D(IW+NLI)                                                
   12    D(IW+NLI) = D(ISV+ISV1)                                                
   20    IF(ISV2.EQ.0) GO TO 30                                                 
         IF (PASS.NE.1) GO TO 22                                                
         ID(ISVT+ISV2) = 2                                                      
         D(ISV+ISV2) = D(IP+NSI)                                                
   22    D(IP+NSI) = D(ISV+ISV2)                                                
   30    IF (ISV3.EQ.0) GO TO 40                                                
         IF (PASS.NE.1) GO TO 32                                                
         ID(ISVT+ISV3) = 3                                                      
         D(ISV+ISV3) = D(IT+NSI)                                                
   32    D(IT+NSI) = D(ISV+ISV3)                                                
   40    IF (ISV4.EQ.0) GO TO 45                                                
         IF (PASS.NE.1) GO TO 42                                                
         ID(ISVT+ISV4) = 4                                                      
         IF(NC.EQ.3)ID(ISVT+ISV4)=10                                            
         D(ISV+ISV4) = D(IH+NSI)                                                
   42    D(IH+NSI) = D(ISV+ISV4)                                                
   45    IF(NC.NE.3) GO TO 50                                                   
C$** IF FLUID IS A REFRIGERANT (TYPE=3) RECOMPUTE THE TEMP                      
         QUAL=VQUALH('LPSPP   ',LOCT,D(IP+NSI),D(IH+NSI))                       
         IF(QUAL.LT.0.0)D(IT+NSI)=VTLIQ(LOCT,D(IP+NSI),D(IH+NSI))               
         IF(QUAL.GE.0..AND.QUAL.LE.1.)D(IT+NSI)=VTS(LOCT,D(IP+NSI))             
         IF(QUAL.GT.1.)CALL VTAV2(LOCT,D(IT+NSI),V,D(IP+NSI),D(IH+NSI))         
   50    IF (IRCD2.NE.0) RETURN                                                 
         CERR = CERR+1                                                          
         CALL LINES(2)                                                          
         WRITE (OUT,1000) CERR                                                  
 1000    FORMAT(6H0ERROR,I6,5X,21HLOOPS/LOOPE UNMATCHED)                        
         RETURN                                                                 
                                                                                
      ENDIF                                                                     
C                                                                               
C    PROCESS FOR LOOP END                                                       
C                                                                               
C$    WRITE(6,7771)IEV,IEVT                                                     
 7771 FORMAT(' LPSPP-4  IEV = ',I5,' IEVT = ',I5)                               
      IEV1 = ID(IRCD+7)                                                         
      IEV2 = ID(IRCD+8)                                                         
      IEV3 = ID(IRCD+9)                                                         
      IEV4 = ID(IRCD+10)                                                        
                                                                                
C$    WRITE(6,7770)IEV1,IEV2,IEV3,IEV4                                          
 7770 FORMAT(' LPSPP-5  IEV1 = ',I8,' IEV2 = ',I5,' IEV3 = ',I5,                
     *' IEV4 = ',I8)                                                            
C$    WRITE(6,7769)PASS                                                         
 7769 FORMAT(' LPSPP-6  PASS = ',I5)                                            
                                                                                
      IF (PASS.EQ.1) THEN                                                       
         IF (IEV1.NE.0) ID(IEVT+IEV1) = 1                                       
         IF (IEV2.NE.0) ID(IEVT+IEV2) = 2                                       
         IF (IEV3.NE.0) ID(IEVT+IEV3) = 3                                       
         IF (IEV4.NE.0) ID(IEVT+IEV4) = 4                                       
      ENDIF                                                                     
                                                                                
C$    WRITE(6,7768)IW,IP,IT,IH                                                  
 7768 FORMAT(' LPSSP-9 IW = ',I5,' IP = ',I5,' IT = ',I5,' IH = ',I5)           
C$    WRITE(6,7767)D(IW+NLI)                                                    
 7767 FORMAT(' LPSSP-10 D(IW+NLI) = ',G12.5)                                    
                                                                                
    3 IF (IEV1.NE.0) D(IEV+IEV1) = D(IW+NLI)-D(IW+NLO)                          
      IF (IEV2.NE.0) D(IEV+IEV2) = D(IP+NSI)-D(IP+NSO)                          
      IF (IEV3.NE.0) D(IEV+IEV3) = D(IT+NSI)-D(IT+NSO)                          
      IF (IEV4.NE.0) D(IEV+IEV4) = D(IH+NSI)-D(IH+NSO)                          
                                                                                
      RETURN
c     LPSPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE LPSPZ                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151)), (CERR,C(16))                
     *, (OUT,C(7)), (IT,C(29))                                                  
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER CERR, OUT                                                         
      EQUIVALENCE (SCR(1),NLI), (SCR(2),NSI), (SCR(3),NLO), (SCR(4),NSO)        
     *, (SCR(5),NC), (SCR(6),IOP)                                               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG NO. - LOOP START                                                      
C   3 STATION NO. - LOOP START                                                  
C   4 LEG NO. - LOOP END                                                        
C   5 STATION NO. - LOOP END                                                    
C   6 IRCD NO. 2                                                                
C   7 SV W / EV W                                                               
C   8 SV P / EV P                                                               
C   9 SV T / EV T                                                               
C  10 SV H / EV H                                                               
C  11 W (LOOPS ONLY)                                                            
C  12 P (LOOPS ONLY)                                                            
C  13 T (LOOPS ONLY)                                                            
C  14 H (LOOPS ONLY)                                                            
C  15 DSH                                                                       
      IE = 1                                                                    
      GO TO 1                                                                   
      ENTRY LPEPZ                                                               
      IE = 2                                                                    
    1 I = IACDB(15)                                                             
      ID(I+1) = ICV(1)                                                          
      NLI = ILEGN(ICV(2))                                                       
      ID(I+2) = NLI                                                             
      IF (IE.EQ.2) GO TO 2                                                      
      CALL LEGRS(NLI)                                                           
      GO TO 3                                                                   
    2 CALL LEGRT(NLI)                                                           
    3 NSI = ISTAN(ICV(3))                                                       
      ID(I+3) = NSI                                                             
      IF (IE.EQ.2) GO TO 4                                                      
      CALL STARS(NSI)                                                           
      GO TO 5                                                                   
    4 CALL START(NSI)                                                           
    5 NLO = ILEGN(ICV(4))                                                       
      ID(I+4) = NLO                                                             
      IF (IE.EQ.2) CALL LEGRT(NLO)                                              
      NSO = ISTAN(ICV(5))                                                       
      ID(I+5) = NSO                                                             
      IF (IE.EQ.2) CALL START(NSO)                                              
      IF (IE.EQ.2) GO TO 10                                                     
      CALL FLUIDZ(NLI,ICV(6),ICV(7))                                            
      CALL FTR(NLI,NC)                                                          
      IF (ICV(8).EQ.0) GO TO 7                                                  
      IOP = ICV(8)/1000                                                         
      IF (IOP.NE.0) ID(I+7) = IASV(1)                                           
      IOP = 100*(ICV(8)/100)                                                    
      IOP = IOP-1000*(IOP/1000)                                                 
      IF (IOP.NE.0) ID(I+8) = IASV(2)                                           
      IOP = 10*(ICV(8)/10)                                                      
      IOP = IOP-100*(IOP/100)                                                   
C$ NEW STARTS HERE                                                              
      IF(IOP.EQ.0) GO TO 6                                                      
      IF(NC.NE.3) GO TO 8                                                       
      CERR=CERR + 1                                                             
      CALL LINES (2)                                                            
      WRITE(OUT,1001)CERR                                                       
 1001 FORMAT(' ERROR',I3,' TEMPERATURE CAN NOT BE STATE VARIABLE')              
    8 ID(I+9)=IASV(3)                                                           
    6 IOP =ICV(8)-10*(ICV(8)/10)                                                
      IF(IOP.NE.0 .AND. NC.EQ.2 ) ID(I+10) = IASV(4)                            
      IF(IOP.NE.0.AND.NC.EQ.3) ID(I+10) = IASV(10)                              
    7 ID(I+11) = IPARM(ICV(9))                                                  
      ID(I+12) = IPARM(ICV(10))                                                 
      ID(I+13) = IPARM(ICV(11))                                                 
      ID(I+14) = IPARM(ICV(12))                                                 
      IF (NC.EQ.1) ID(I+14) = 0                                                 
C$     CHANGE OVER HERE                                                         
      ID(I+15) = IPARM(ICV(13))                                                 
      IF (NSO.NE.0) ID(IT+NSO) = I                                              
      GO TO 99                                                                  
   10 IF (NSO.EQ.0) GO TO 99                                                    
      II = ID(IT+NSO)                                                           
      IF (II.NE.0) GO TO 11                                                     
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1002) CERR                                                     
 1002 FORMAT(6H0ERROR,I6,5X,21HLOOPS/LOOPE UNMATCHED)                           
      GO TO 99                                                                  
   11 ID(I+6) = II                                                              
      ID(II+6) = I                                                              
      ID(IT+NSO) = 0                                                            
      IF (NLI.EQ.ID(II+2) .AND. NSI.EQ.ID(II+3) .AND.NLO.EQ.ID(II+4)            
     * .AND.NSO.EQ.ID(II+5)) GO TO 12                                           
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1002) CERR                                                     
   12 IF (ICV(6).EQ.0) GO TO 99                                                 
      IOP = ICV(6)/1000                                                         
      IF (IOP.NE.0) ID(I+7) = IAEV(1)                                           
      IOP = 100*(ICV(6)/100)                                                    
      IOP = IOP-1000*(IOP/1000)                                                 
      IF (IOP.NE.0) ID(I+8) = IAEV(2)                                           
      IOP = 10*(ICV(6)/10)                                                      
      IOP = IOP-100*(IOP/100)                                                   
      IF (IOP.NE.0) ID(I+9) = IAEV(3)                                           
      IOP = ICV(6)-10*(ICV(6)/10)                                               
      IF (IOP.NE.0) ID(I+10) = IAEV(4)                                          
   99 CONTINUE                                                                  
      RETURN                                                                    
C     LPSPZ, LPEPZ                                                              
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      MISCELLANEOUS OPERATIONS                                                 
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MISCPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (SCR(1),C(151)), (ICPP,C(88))                   
     *, (IFP,C(22)), (OUT,C(7)), (ISV,C(47)), (ISVT,C(46)), (BIG,C(31))         
     *, (IEV,C(49)), (IEVT,C(48)), (NSV,C(50)), (NEV,C(51))                     
     *, (CERR,C(16)), (PASS,C(17)), (IPAR,C(33)), (IGA,C(35))                   
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (IH,C(30))                          
     *, (IIOP,C(90)), (ILN,C(37)), (ISN,C(38)), (ICDB,C(43))                    
     *, (ISL,C(131)), (ITN,C(39)), (ITAD,C(54)), (NCOMP,C(44))                  
      DIMENSION SCR(30)                                                         
      INTEGER PASS, OUT, CERR                                                   
      EQUIVALENCE (SCR(1),VX), (SCR(2),V1), (SCR(3),V2), (SCR(4),IVXT)          
     *, (SCR(5),IVXN), (SCR(6),IV1T), (SCR(7),IV1N), (SCR(8),IV2T)              
     *, (SCR(9),IV2N), (SCR(10),IOPR), (SCR(11),IOPT), (SCR(12),J)              
     *, (SCR(13),K), (SCR(14),IVT), (SCR(15),IVN), (SCR(16),IX)                 
     *, (SCR(17),IOPRA), (SCR(18),I), (SCR(19),II), (SCR(20),JJ)                
     *, (SCR(21),NOCS)                                                          
      COMMON /CCA/ CA(150)                                                      
      EQUIVALENCE (NOC,CA(1))                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 IVXT                                                                      
C   3 NVX                                                                       
C   4 IV1T                                                                      
C   5 NV1                                                                       
C   6 IOPR                                                                      
C   7 IV2T                                                                      
C   8 NV2                                                                       
C   9 IOPT                                                                      
C  10 ISP                                                                       
C  11 ISZ                                                                       
C  12 ISM                                                                       
      IRCD = IRCDB(12)                                                          
      NOCS = NOC                                                                
      IVXT = ID(IRCD+2)                                                         
      IVXN = ID(IRCD+3)                                                         
      IOPT = ID(IRCD+9)                                                         
      IF (IVXT.EQ.-5) GO TO 73                                                  
      IF (IVXT+3) 71,72,73                                                      
   73 IF (IOPT.EQ.1.AND.PASS.NE.1 .OR. IOPT.EQ.-1.AND.IIOP.NE.0)                
     * GO TO 99                                                                 
      IF (IOPT.EQ.2.AND.IFP.EQ.0) GO TO 99                                      
      GO TO 71                                                                  
   72 IF (IOPT.EQ.1 .AND. PASS.NE.1) GO TO 99                                   
      IF (IOPT.EQ.-1 .AND. IIOP.NE.0) GO TO 99                                  
   71 J = 1                                                                     
      K = 4                                                                     
   70 IVT = ID(IRCD+K)                                                          
      IVN = ID(IRCD+K+1)                                                        
      IF (IVT.EQ.-5) GO TO 74                                                   
      IF (IVT.GT.-2) GO TO 68                                                   
      IF (J .EQ. 0) GO TO 4                                                     
      II = IVT/10                                                               
      IF (II .EQ. -6) GO TO 75                                                  
    4 IF (IVT+3) 8,9,10                                                         
    8 IF (IVN.LT.1 .OR. IVN.GT.NEV) GO TO 69                                    
      IX = IEV                                                                  
      IF (PASS.EQ.1 .AND. IOPT.EQ.1 .AND. J.EQ.0) ID(IEVT+IVN) = 10             
      GO TO 1                                                                   
    9 IF (IVN.LT.1 .OR. IVN.GT.NSV) GO TO 69                                    
      IX = ISV                                                                  
      IF (PASS.EQ.1 .AND. IOPT.EQ.1 .AND. J.EQ.0) ID(ISVT+IVN) = 10             
      GO TO 1                                                                   
   10 IF (J.EQ.0) GO TO 22                                                      
      VX = TLUP(IVN)                                                            
      IVN = ID(ITN + IVN) / 100                                                 
      GO TO 2                                                                   
   22 IX = ID(ITAD+IVN)                                                         
      ID(IX+1) = 1                                                              
      D(IX+2) = VX                                                              
      IVXN = ID(ITN+IVN)/100                                                    
      GO TO 88                                                                  
   69 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1001) CERR,J                                                   
 1001 FORMAT(6H0ERROR,I6,5X,2HNV,I1,8H INVALID)                                 
      VX = BIG                                                                  
      GO TO 2                                                                   
   68 IF (IVT.LT.0) GO TO 21                                                    
      IF (IVT.EQ.0) GO TO 100                                                   
      GO TO (101,102,103,104), IVT                                              
   74 IX = ISL                                                                  
      GO TO 1                                                                   
   75 IX = ID(ICDB+IVN)                                                         
      JJ = -IVT + II*10 + 5                                                     
      VX = D(IX-JJ)                                                             
      GO TO 2                                                                   
   21 IX = IPAR                                                                 
      IF (IVN.EQ.0) IX = 0                                                      
      GO TO 1                                                                   
  100 IX = IGA                                                                  
      GO TO 1                                                                   
  101 IX = IW                                                                   
      GO TO 1                                                                   
  102 IX = IP                                                                   
      GO TO 1                                                                   
  103 IX = IT                                                                   
      GO TO 1                                                                   
  104 IX = IH                                                                   
    1 IF (J.EQ.0) GO TO 90                                                      
      VX = D(IX+IVN)                                                            
    2 GO TO (3,5), J                                                            
    3 V1 = VX                                                                   
      IV1T = IVT                                                                
      IV1N = IVN                                                                
      IOPR = ID(IRCD+6)                                                         
      IF (IOPR.LE.0) GO TO 7                                                    
      J = 2                                                                     
      K = 7                                                                     
      GO TO 70                                                                  
    5 V2 = VX                                                                   
      IV2T = IVT                                                                
      IV2N = IVN                                                                
      GO TO (11,12,13,14,15,16,17,18,19), IOPR                                  
    7 IF (IOPR.EQ.0) GO TO 40                                                   
      IOPRA = -IOPR                                                             
      GO TO (51,52,53,54,55,56), IOPRA                                          
   40 VX = V1                                                                   
      GO TO 6                                                                   
   11 VX = V1+V2                                                                
      GO TO 6                                                                   
   12 VX = V1-V2                                                                
      GO TO 6                                                                   
   13 VX = V1*V2                                                                
      GO TO 6                                                                   
   14 VX = V1/V2                                                                
      GO TO 6                                                                   
   15 VX = AMIN1(V1,V2)                                                         
      GO TO 6                                                                   
   16 VX = AMAX1(V1,V2)                                                         
      GO TO 6                                                                   
   17 VX = V1**V2                                                               
      GO TO 6                                                                   
   18 VX = 0.5*(V1+V2)                                                          
      GO TO 6                                                                   
   19 IF (V1-V2) 191,192,193                                                    
  191 VX = -1.0                                                                 
      I = ID(IRCD+12)                                                           
      IF (I.NE.0) GO TO 194                                                     
      GO TO 6                                                                   
  192 VX = 0.0                                                                  
      I = ID(IRCD+11)                                                           
      IF (I.NE.0) GO TO 194                                                     
      GO TO 6                                                                   
  193 VX = 1.0                                                                  
      I = ID(IRCD+10)                                                           
      IF (I.NE.0) GO TO 194                                                     
      GO TO 6                                                                   
  194 NOCS = NOCS+I                                                             
      IF (NOCS.LT.0) NOCS = 0                                                   
      IF (NOCS.GT.NCOMP) NOCS = NCOMP                                           
      GO TO 6                                                                   
   51 VX = ABS(V1)                                                              
      GO TO 6                                                                   
   52 VX = SQRT(V1)                                                             
      GO TO 6                                                                   
   53 VX = ALOG10(V1)                                                           
      GO TO 6                                                                   
   54 VX = ALOG(V1)                                                             
      GO TO 6                                                                   
   55 VX = 10.0**V1                                                             
      GO TO 6                                                                   
   56 VX = EXP(V1)                                                              
    6 J = 0                                                                     
      K = 2                                                                     
      GO TO 70                                                                  
   90 D(IX+IVN) = VX                                                            
   88 D(IRCD-6) = VX                                                            
      D(IRCD-7) = V1                                                            
      IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      IF (IVXT-1) 91,92,93                                                      
   93 IVXN = ID(ISN+IVXN)                                                       
      GO TO 91                                                                  
   92 IVXN = ID(ILN+IVXN)                                                       
   91 IF (IV1T-1) 94,95,96                                                      
   96 IV1N = ID(ISN+IV1N)                                                       
      GO TO 94                                                                  
   95 IV1N = ID(ILN+IV1N)                                                       
   94 CONTINUE                                                                  
      CALL PIOP(0,0,0,0)                                                        
      CALL LINES(3)                                                             
      WRITE (OUT,1000) VX,IVXT,IVXN,IOPR,IOPT,V1,IV1T,IV1N                      
 1000 FORMAT(                                                                   
     11H0,5X,2HVX,1PE15.6,3X,4HIVXT,I6,3X,4HIVXN,I6,3X,4HIOPR,I6,3X,            
     24HIOPT,I6/                                                                
     3    6X,2HV1,E15.6,3X,4HIV1T,I6,3X,4HIV1N,I6)                              
      IF (IOPR.LE.0) GO TO 99                                                   
      D(IRCD-8) = V2                                                            
      IF (IV2T-1) 89,97,98                                                      
   98 IV2N = ID(ISN+IV2N)                                                       
      GO TO 89                                                                  
   97 IV2N = ID(ILN+IV2N)                                                       
   89 CALL LINES(1)                                                             
      WRITE (OUT,1002) V2,IV2T,IV2N                                             
 1002 FORMAT(6X,2HV2,1PE15.6,3X,4HIV2T,I6,3X,4HIV2N,I6)                         
   99 CONTINUE                                                                  
      NOC = NOCS                                                                
      RETURN                                                                    
C     MISCPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF MISCELLANEOUS OPERATIONS                                  
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MISCPZ                                                         
                                                                                
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,    C(7)),                                               
     *            (CERR,   C(16)),                                              
     *            (NPV,    C(32)),                                              
     *            (NGA,    C(36)),                                              
     *            (ICDB,   C(43)),                                              
     *            (NCOMP,  C(44)),                                              
     *            (NSV,    C(50)),                                              
     *            (NEV,    C(51)),                                              
     *            (ISR,    C(53)),                                              
     *            (ILR,    C(52)),                                              
     *            (ISL,    C(131)),                                             
     *            (ICV(1), C(133)),                                             
     *            (SCR(1), C(151))                                              
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER CERR, OUT                                                         
      EQUIVALENCE (SCR(1),J),                                                   
     *            (SCR(2),K),                                                   
     *            (SCR(3),L),                                                   
     *            (SCR(4),IX),                                                  
     *            (SCR(5),II),                                                  
     *            (SCR(6),JJ),                                                  
     *            (SCR(7),KK)                                                   
                                                                                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
                                                                                
C   1 COMP CODE                                                                 
C   2 IVXT                                                                      
C   3 NVX                                                                       
C   4 IV1T                                                                      
C   5 NV1                                                                       
C   6 IOPR                                                                      
C   7 IV2T                                                                      
C   8 NV2                                                                       
C   9 IOPT                                                                      
C  10 ISP                                                                       
C  11 ISZ                                                                       
C  12 ISM                                                                       
                                                                                
      I = IACDB(12)                                                             
      ID(I+1) = ICV(1)                                                          
      ID(I+2) = ICV(2)                                                          
      ID(I+3) = ICV(3)                                                          
      IF (ICV(2).EQ.0) GO TO 2                                                  
      IF (ICV(2).EQ.-1) GO TO 1                                                 
      IF (ICV(2).EQ.-2) GO TO 34                                                
      IF (ICV(2).EQ.-3) GO TO 50                                                
      IF (ICV(2).EQ.-4) GO TO 60                                                
      IF (ICV(2).EQ.-5) GO TO 80                                                
      IF (ICV(2).GE.1 .OR. ICV(2).LE.4) GO TO 30                                
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1001) CERR                                                     
 1001 FORMAT(6H0ERROR,I6,5X,12HIVXT INVALID)                                    
      GO TO 3                                                                   
   50 IF (ICV(9).NE.1) GO TO 51                                                 
      ID(I+3) = IASV(10)                                                        
      IF (ICV(3).NE.0) GO TO 5                                                  
      GO TO 3                                                                   
   51 IF (ICV(9).NE.-1) GO TO 5                                                 
      IF (ICV(3).LE.0) ID(I+3) = NSV-ICV(3)                                     
      GO TO 3                                                                   
   60 IF (ICV(9).NE.1) GO TO 61                                                 
      ID(I+3) = IAEV(10)                                                        
      IF (ICV(3).NE.0) GO TO 5                                                  
      GO TO 3                                                                   
   61 IF (ICV(9).NE.-1) GO TO 5                                                 
      IF (ICV(3).EQ.0) ICV(3) = -1                                              
      IF (ICV(3).LE.0) ID(I+3) = NEV+ICV(3)+1                                   
      GO TO 3                                                                   
   30 IF (ICV(2).NE.1) GO TO 31                                                 
      IX = ILEGN(ICV(3))                                                        
      IF (ILR.EQ.0) GO TO 33                                                    
      IF (IX.EQ.0) GO TO 40                                                     
      IF (ID(ILR+IX).NE.0) GO TO 40                                             
   33 CALL LEGRS(IX)                                                            
      CALL FLUIDZ(IX,ICV(13),ICV(14))                                           
      GO TO 40                                                                  
   31 IX = ISTAN(ICV(3))                                                        
      IF (ISR.EQ.0) GO TO 32                                                    
      IF (IX.EQ.0) GO TO 40                                                     
      IF (ID(ISR+IX).NE.0) GO TO 40                                             
   32 CALL STARS(IX)                                                            
      GO TO 40                                                                  
   34 IX = ITIDN(ICV(3),99)                                                     
   40 ID(I+3) = IX                                                              
      GO TO 3                                                                   
    1 IF (ICV(3).LE.0 .OR. ICV(3).GT.NPV) GO TO 5                               
      IX = IPARM(ICV(3))                                                        
      GO TO 3                                                                   
   80 IF (ICV(3).LE.0 .OR. ICV(3).GT.100) GO TO 5                               
      GO TO 3                                                                   
    5 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1002) CERR                                                     
 1002 FORMAT(6H0ERROR,I6,5X,11HNVX INVALID)                                     
      GO TO 3                                                                   
    2 IF (ICV(3).LT.0 .AND. ICV(3).GE.-100) GO TO 4                             
      IF (ICV(3).LE.100 .OR. ICV(3).GT.NGA) GO TO 5                             
      GO TO 3                                                                   
    4 ICV(3) = IABS(ICV(3))                                                     
    3 J = 1                                                                     
      K = 4                                                                     
      L = 0                                                                     
   21 ID(I+4+L) = ICV(K)                                                        
      ID(I+5+L) = ICV(K+1)                                                      
      IF(ICV(K).GE.-5 .AND. ICV(K).LE.4) GO TO 6                                
      II = ICV(K)/10                                                            
      JJ = -ICV(K) + II*10                                                      
      IF (II.EQ.-6 .AND. JJ.GT.0 .AND. JJ.LE.4) GO TO 26                        
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1003) CERR,J                                                   
 1003 FORMAT(6H0ERROR,I6,5X,2HIV,I1,9HT INVALID)                                
      GO TO 7                                                                   
    6 IF (ICV(K).GT.0) GO TO 8                                                  
      IF (ICV(K).EQ.-5) GO TO 24                                                
      IF (ICV(K)+1) 22,10,11                                                    
   22 IF (ICV(K)+3) 25,23,9                                                     
   23 IF (ICV(K+1).LE.0) ID(I+5+L) = NSV-ICV(K+1)                               
      GO TO 7                                                                   
   25 IF (ICV(K+1).EQ.0) ICV(K+1) = -1                                          
      IF (ICV(K+1).LE.0) ID(I+5+L) = NEV+ICV(K+1)+1                             
      GO TO 7                                                                   
    9 ID(I+5+L) = ITIDN(ICV(K+1),99)                                            
      GO TO 7                                                                   
   10 IF (ICV(K+1).LT.0 .OR. ICV(K+1).GT.NPV) GO TO 13                          
      IX = IPARM(ICV(K+1))                                                      
      GO TO 7                                                                   
   24 IF (ICV(K+1).LE.0 .OR. ICV(K+1).GT.100) GO TO 13                          
      GO TO 7                                                                   
   26 IF (ICV(K+1).LE.0 .OR. ICV(K+1).GT.NCOMP) GO TO 13                        
      KK = ID(ICDB+ICV(K+1))                                                    
      IF (KK .EQ. 0) GO TO 13                                                   
      GO TO 7                                                                   
   13 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1004) CERR,J                                                   
 1004 FORMAT(6H0ERROR,I6,5X,2HNV,I1,8H INVALID)                                 
      GO TO 7                                                                   
   11 IF (ICV(K+1).LE.0 .OR. ICV(K+1).GT.NGA) GO TO 13                          
      GO TO 7                                                                   
    8 IF (ICV(K).NE.1) GO TO 14                                                 
      IX = ILEGN(ICV(K+1))                                                      
      CALL LEGRT(IX)                                                            
      GO TO 15                                                                  
   14 IX = ISTAN(ICV(K+1))                                                      
      CALL START(IX)                                                            
   15 ID(I+5+L) = IX                                                            
    7 IF (J.EQ.2) GO TO 70                                                      
      ID(I+6) = ICV(6)                                                          
      IF (ICV(6).GE.-6 .AND. ICV(6).LE.0) GO TO 42                              
      IF (ICV(6).LE.9) GO TO 20                                                 
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1005) CERR                                                     
 1005 FORMAT(6H0ERROR,I6,5X,12HIOPR INVALID)                                    
   20 J = 2                                                                     
      K = 7                                                                     
      L = 3                                                                     
      GO TO 21                                                                  
   42 IF (ICV(7).EQ.0 .AND. ICV(8).EQ.0) GO TO 70                               
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1007) CERR,ICV(6)                                              
 1007 FORMAT(6H0ERROR,I6,5X,35HOPERAND 2 NOT ALLOWED FOR OPERATION,I6)          
      GO TO 20                                                                  
   70 ID(I+9) = ICV(9)                                                          
      IF (ICV(9).GE.-1 .AND. ICV(9).LE.2) GO TO 71                              
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1008) CERR                                                     
 1008 FORMAT(6H0ERROR,I6,5X,12HIOPT INVALID)                                    
   71 IF (ICV(6).NE.9) GO TO 99                                                 
      ID(I+10) = ICV(10)                                                        
      ID(I+11) = ICV(11)                                                        
      ID(I+12) = ICV(12)                                                        
   99 CONTINUE                                                                  
      RETURN                                                                    
C     MISCPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      MISCELLANEOUS OPERATION                                                  
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MISCSP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (SCR(1),C(151)), (OUT,C(7)), (ILN,C(37))        
     *, (ISN,C(38)), (IPAR,C(33)), (IGA,C(35)), (IW,C(27)), (IP,C(28))          
     *, (IT,C(29)), (IH,C(30)), (ISL,C(131)), (ITN,C(39))                       
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE (SCR(1),VX), (SCR(2),V1), (SCR(3),V2), (SCR(4),IVXT)          
     *, (SCR(5),IVXN), (SCR(6),IV1T), (SCR(7),IV1N), (SCR(8),IV2T)              
     *, (SCR(9),IV2N), (SCR(10),IOPR), (SCR(11),J), (SCR(12),K)                 
     *, (SCR(13),IVT), (SCR(14),IVN), (SCR(15),IX), (SCR(16),IOPRA)             
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 IVXT                                                                      
C   3 NVX                                                                       
C   4 IV1T                                                                      
C   5 NV1                                                                       
C   6 IOPR                                                                      
C   7 IV2T                                                                      
C   8 NV2                                                                       
      IRCD = IRCDB(8)                                                           
      IVXT = ID(IRCD+2)                                                         
      IVXN = ID(IRCD+3)                                                         
      J = 1                                                                     
      K = 4                                                                     
   70 IVT = ID(IRCD+K)                                                          
      IVN = ID(IRCD+K+1)                                                        
      IF (IVT.EQ.-5) GO TO 74                                                   
      IF (IVT.GT.-2) GO TO 68                                                   
      VX = TLUP(IVN)                                                            
      IVN = ID(ITN + IVN) / 100                                                 
      GO TO 2                                                                   
   68 IF (IVT.LT.0) GO TO 21                                                    
      IF (IVT.EQ.0) GO TO 100                                                   
      GO TO (101,102,103,104), IVT                                              
   74 IX = ISL                                                                  
      GO TO 1                                                                   
   21 IX = IPAR                                                                 
      IF (IVN.EQ.0) IX = 0                                                      
      GO TO 1                                                                   
  100 IX = IGA                                                                  
      GO TO 1                                                                   
  101 IX = IW                                                                   
      GO TO 1                                                                   
  102 IX = IP                                                                   
      GO TO 1                                                                   
  103 IX = IT                                                                   
      GO TO 1                                                                   
  104 IX = IH                                                                   
    1 IF (J.EQ.0) GO TO 90                                                      
      VX = D(IX+IVN)                                                            
    2 GO TO (3,5), J                                                            
    3 V1 = VX                                                                   
      IV1T = IVT                                                                
      IV1N = IVN                                                                
      IOPR = ID(IRCD+6)                                                         
      IF (IOPR.LE.0) GO TO 7                                                    
      J = 2                                                                     
      K = 7                                                                     
      GO TO 70                                                                  
    5 V2 = VX                                                                   
      IV2T = IVT                                                                
      IV2N = IVN                                                                
      GO TO (11,12,13,14,15,16,17,18,19), IOPR                                  
    7 IF (IOPR.EQ.0) GO TO 40                                                   
      IOPRA = -IOPR                                                             
      GO TO (51,52,53,54,55,56), IOPRA                                          
   40 VX = V1                                                                   
      GO TO 6                                                                   
   11 VX = V1+V2                                                                
      GO TO 6                                                                   
   12 VX = V1-V2                                                                
      GO TO 6                                                                   
   13 VX = V1*V2                                                                
      GO TO 6                                                                   
   14 VX = V1/V2                                                                
      GO TO 6                                                                   
   15 VX = AMIN1(V1,V2)                                                         
      GO TO 6                                                                   
   16 VX = AMAX1(V1,V2)                                                         
      GO TO 6                                                                   
   17 VX = V1**V2                                                               
      GO TO 6                                                                   
   18 VX = 0.5*(V1+V2)                                                          
      GO TO 6                                                                   
   19 IF (V1-V2) 191,192,193                                                    
  191 VX = -1.0                                                                 
      GO TO 6                                                                   
  192 VX = 0.0                                                                  
      GO TO 6                                                                   
  193 VX = 1.0                                                                  
      GO TO 6                                                                   
   51 VX = ABS(V1)                                                              
      GO TO 6                                                                   
   52 VX = SQRT(V1)                                                             
      GO TO 6                                                                   
   53 VX = ALOG10(V1)                                                           
      GO TO 6                                                                   
   54 VX = ALOG(V1)                                                             
      GO TO 6                                                                   
   55 VX = 10.0**V1                                                             
      GO TO 6                                                                   
   56 VX = EXP(V1)                                                              
    6 J = 0                                                                     
      K = 2                                                                     
      GO TO 70                                                                  
   90 D(IX+IVN) = VX                                                            
      IF (IVXT-1) 91,92,93                                                      
   93 IVXN = ID(ISN+IVXN)                                                       
      GO TO 91                                                                  
   92 IVXN = ID(ILN+IVXN)                                                       
   91 IF (IV1T-1) 94,95,96                                                      
   96 IV1N = ID(ISN+IV1N)                                                       
      GO TO 94                                                                  
   95 IV1N = ID(ILN+IV1N)                                                       
   94 CONTINUE                                                                  
      CALL SCOA                                                                 
      CALL LINES(3)                                                             
C                                                                               
C                                                                               
      WRITE (OUT,1000) VX,IVXT,IVXN,IOPR,IOPT,V1,IV1T,IV1N                      
 1000 FORMAT(                                                                   
     11H0,5X,2HVX,1PE15.8,3X,4HIVXT,I6,3X,4HIVXN,I6,3X,4HIOPR,I6,3X,            
     24HIOPT,I6/                                                                
     3    6X,2HV1,E15.8,3X,4HIV1T,I6,3X,4HIV1N,I6)                              
C$WAS11H0,5X,2HVX,1PE15.6,3X,4HIVXT,I6,3X,4HIVXN,I6,3X,4HIOPR,I6/               
C$   3    6X,2HV1,E15.6,3X,4HIV1T,I6,3X,4HIV1N,I6)                              
      IF (IOPR.LE.0) GO TO 99                                                   
      IF (IV2T-1) 89,97,98                                                      
   98 IV2N = ID(ISN+IV2N)                                                       
      GO TO 89                                                                  
   97 IV2N = ID(ILN+IV2N)                                                       
   89 CALL LINES(1)                                                             
      WRITE (OUT,1002) V2,IV2T,IV2N                                             
 1002 FORMAT(6X,2HV2,1PE15.8,3X,4HIV2T,I6,3X,4HIV2N,I6)                         
   99 CONTINUE                                                                  
      RETURN                                                                    
C     MISCSP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MISCSZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151)), (CERR,C(16))                
     *, (OUT,C(7)), (NPV,C(32)), (NGA,C(36)), (ISL,C(131))                      
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER CERR, OUT                                                         
      EQUIVALENCE (SCR(1),J), (SCR(2),K), (SCR(3),L), (SCR(4),IX)               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 IVXT                                                                      
C   3 NVX                                                                       
C   4 IV1T                                                                      
C   5 NV1                                                                       
C   6 IOPR                                                                      
C   7 IV2T                                                                      
C   8 NV2                                                                       
      I = IACDB(8)                                                              
      ID(I+1) = ICV(1)                                                          
      ID(I+2) = ICV(2)                                                          
      ID(I+3) = ICV(3)                                                          
      IF (ICV(2).EQ.-1) GO TO 1                                                 
      IF (ICV(2).EQ.0) GO TO 2                                                  
      IF (ICV(2).EQ.-5) GO TO 80                                                
      IF (ICV(2).GE.1 .OR. ICV(2).LE.4) GO TO 30                                
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1001) CERR                                                     
 1001 FORMAT(6H0ERROR,I6,5X,12HIVXT INVALID)                                    
      GO TO 3                                                                   
   30 IF (ICV(2).NE.1) GO TO 31                                                 
      IX = ILEGN(ICV(3))                                                        
      GO TO 40                                                                  
   31 IX = ISTAN(ICV(3))                                                        
   40 ID(I+3) = IX                                                              
      GO TO 3                                                                   
    1 IF (ICV(3).LE.0 .OR. ICV(3).GT.NPV) GO TO 5                               
      IX = IPARM(ICV(3))                                                        
      GO TO 3                                                                   
   80 IF (ICV(3).LE.0 .OR. ICV(3).GT.100) GO TO 5                               
      GO TO 3                                                                   
    5 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1002) CERR                                                     
 1002 FORMAT(6H0ERROR,I6,5X,11HNVX INVALID)                                     
      GO TO 3                                                                   
    2 IF (ICV(3).LE.100 .OR. ICV(3).GT.NGA) GO TO 5                             
    3 J = 1                                                                     
      K = 4                                                                     
      L = 0                                                                     
   21 ID(I+4+L) = ICV(K)                                                        
      ID(I+5+L) = ICV(K+1)                                                      
      IF (ICV(K).GE.-2 .AND. ICV(K).LE.4) GO TO 6                               
      IF (ICV(K).EQ.-5) GO TO 24                                                
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1003) CERR,J                                                   
 1003 FORMAT(6H0ERROR,I6,5X,2HIV,I1,9HT INVALID)                                
      GO TO 7                                                                   
    6 IF (ICV(K).GT.0) GO TO 8                                                  
      IF (ICV(K)+1) 9,10,11                                                     
    9 ID(I+5+L) = ITIDN(ICV(K+1),99)                                            
      GO TO 7                                                                   
   10 IF (ICV(K+1).LT.0 .OR. ICV(K+1).GT.NPV) GO TO 13                          
      IX = IPARM(ICV(K+1))                                                      
      GO TO 7                                                                   
   24 IF (ICV(K+1).LE.0 .OR. ICV(K+1).GT.100) GO TO 13                          
      GO TO 7                                                                   
   13 CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1004) CERR,J                                                   
 1004 FORMAT(6H0ERROR,I6,5X,2HNV,I1,8H INVALID)                                 
      GO TO 7                                                                   
   11 IF (ICV(K+1).LE.0 .OR. ICV(K+1).GT.NGA) GO TO 13                          
      GO TO 7                                                                   
    8 IF (ICV(K).NE.1) GO TO 14                                                 
      IX = ILEGN(ICV(K+1))                                                      
      GO TO 15                                                                  
   14 IX = ISTAN(ICV(K+1))                                                      
   15 ID(I+5+L) = IX                                                            
    7 IF (J.EQ.2) GO TO 99                                                      
      ID(I+6) = ICV(6)                                                          
      IF (ICV(6).GE.-6 .AND. ICV(6).LE.0) GO TO 42                              
      IF (ICV(6).LE.9) GO TO 20                                                 
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1005) CERR                                                     
 1005 FORMAT(6H0ERROR,I6,5X,12HIOPR INVALID)                                    
   20 J = 2                                                                     
      K = 7                                                                     
      L = 3                                                                     
      GO TO 21                                                                  
   42 IF (ICV(7).EQ.0 .AND. ICV(8).EQ.0) GO TO 99                               
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1007) CERR,ICV(6)                                              
 1007 FORMAT(6H0ERROR,I6,5X,35HOPERAND 2 NOT ALLOWED FOR OPERATION,I6)          
      GO TO 20                                                                  
   99 CONTINUE                                                                  
      RETURN                                                                    
C     MISCSZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      THE MERGING OF TWO FLOWS                                                 
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MRGEPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)), (PASS,C(17)), (IEV,C(49)), (IEVT,C(48))                     
     *, (SCR(1),C(151)), (IFP,C(22)), (ICPP,C(88)), (IFB,C(55))                 
      INTEGER PASS                                                              
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (SCR(1),NLI1), (SCR(2),NSI1), (SCR(3),NLI2)                   
     *, (SCR(4),NSI2), (SCR(5),NLO), (SCR(6),NSO), (SCR(7),IOP)                 
     *, (SCR(8),WW), (SCR(9),HS), (SCR(10),IFL), (SCR(11),T)                    
     *, (SCR(12),V), (SCR(13),P), (SCR(14),H)                                   
     *, (SCR(15),HOUT)                                                          
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG IN 1                                                                  
C   3 STATION IN 1                                                              
C   4 LEG IN 2                                                                  
C   5 STATION IN 2                                                              
C   6 LEG OUT                                                                   
C   7 STATION OUT                                                               
C   8 IOP                                                                       
C$$ 9 ERROR VARIABLE INDEX                                                      
      IRCD = IRCDB(9)   ! WAS 8                                                 
      NLI1 = ID(IRCD+2)                                                         
      NSI1 = ID(IRCD+3)                                                         
      NLO = ID(IRCD+6)                                                          
      NSO = ID(IRCD+7)                                                          
      IOP = ID(IRCD+8)                                                          
      IF (IOP.GE.0) GO TO 1                                                     
      D(IW+NLO) = 2.0*D(IW+NLI1)                                                
      D(IP+NSO) = D(IP+NSI1)                                                    
      D(IT+NSO) = D(IT+NSI1)                                                    
      D(IH+NSO) = D(IH+NSI1)                                                    
      GO TO 99                                                                  
C$ NEWZ HAD A TYPO HERE AND LINE MISSING                                        
    1 NLI2 = ID(IRCD+4)                                                         
      NSI2 = ID(IRCD+5)                                                         
      IF (PASS.NE.1) GO TO 2                                                    
      IF (IOP.EQ.0) GO TO 2                                                     
      ID(IEVT+IOP) = 2                                                          
    2 D(IW+NLO) = D(IW+NLI1)+D(IW+NLI2)                                         
      IF (D(IW+NLO).NE.0.0) GO TO 7                                             
      D(IP+NSO) = D(IP+NSI1)                                                    
      D(IT+NSO) = D(IT+NSI1)                                                    
      D(IH+NSO) = D(IH+NSI1)                                                    
      GO TO 98                                                                  
    7 IF (IOP.EQ.0.OR.IOP.EQ.2) GO TO 5                                         
      D(IP+NSO) = (D(IW+NLI1)*D(IP+NSI1)+D(IW+NLI2)*D(IP+NSI2))/                
     *D(IW+NLO)                                                                 
      GO TO 6                                                                   
    5 D(IP+NSO) = D(IP+NSI1)                                                    
    6 IFL = ID(IFB+NLI1)                                                        
C$ NEWZ HAD A TYPO HERE ("IF" INSTEAD OF "IFL")                                 
      IF (ID(IFL+1).NE.3) GO TO 3                                               
      D(IH+NSO) = (D(IW+NLI1)*D(IH+NSI1)+D(IW+NLI2)*D(IH+NSI2))/                
C$  BELOW IS THE LINE THAT WAS FIXED (WAS "+NSO")                               
     * D(IW+NLO)                                                                
      IFL = ID(IFL + 3)                                                         
      QUAL=VQUALH('MRGEPP  ',IFL,D(IP+NSO),D(IH+NSO))                           
C$        VHLIQ WILL NOT GIVE THE TEMP. USE VTLIQ.                              
C$WAS IF (QUAL.LT.0.0) D(IT+NSO)=VHLIQ(IFL,D(IP+NSO),D(IT+NSO))                 
      IF (QUAL.LT.0.0) D(IT+NSO)=VTLIQ(IFL,D(IP+NSO),D(IH+NSO))                 
      IF (QUAL.GE.0.0.AND.QUAL.LE.1.) D(IT+NSO)=VTS(IFL,D(IP+NSO))              
      IF (QUAL.GT.1.) CALL VTAV2(IFL,D(IT+NSO),V,D(IP+NSO),D(IH+NSO))           
      GO TO 98                                                                  
    3 IF (ID(IFL+1).NE.1) GO TO 9                                               
      IF (ID(ID(IFL+3)-1).EQ.12) GO TO 9                                        
      HOUT = (D(IW+NLI1)*HFT(NLI1,D(IP+NSI1),D(IT+NSI1))+D(IW+NLI2) *           
     *HFT(NLI2,D(IP+NSI2),D(IT+NSI2)))/D(IW+NLO)                                
      D(IT+NSO) = TFH(NLO,D(IP+NSO),HOUT)                                       
      GO TO 8                                                                   
    9 D(IT+NSO) = (D(IW+NLI1)*D(IT+NSI1)+D(IW+NLI2)*D(IT+NSI2))/                
     *D(IW+NLO)                                                                 
    8 D(IH+NSO) = 0.0                                                           
      IF (ID(IFL+1).EQ.1) GO TO 98                                              
      WW = (D(IH+NSI1)/(1.0+D(IH+NSI1)))*D(IW+NLI1)                             
     *   + (D(IH+NSI2)/(1.0+D(IH+NSI2)))*D(IW+NLI2)                             
      D(IH+NSO) = WW/(D(IW+NLO)-WW)                                             
      IF (D(IH+NSI1).EQ.0.0.AND.D(IH+NSI2).EQ.0.0) GO TO 98                     
      CALL TDB2(NLI1,NSI1,NSI1,HS)                                              
      CALL TDB3(NLI2,NSI2,NSI2,HS)                                              
      CALL TDB4(NLO,NSO,NSO,HS)                                                 
      IF (IFP.NE.1.OR.ICPP.NE.0) GO TO 98                                       
      IF (D(IH+NSO).LE.HS) GO TO 98                                             
      CALL PIOP(-1,NLI1,NSI1,NSI1)                                              
      CALL PIOP(-2,NLI2,NSI2,NSI2)                                              
      CALL PIOP(-2,NLO,NSO,NSO)                                                 
      CALL HSOP(HS)                                                             
   98 CONTINUE                                                                  
      IF (IOP.EQ.0) GO TO 99                                                    
C$$$ WAS  D(IEV+IOP) = D(IP+NSI1)-D(IP+NSI2)                                    
C$$$      CHANGE 10/7/96 SFW  (STEAL FROM "SENPP")                              
      JEVI = ID(IRCD+9)                                                         
      D(IEV+JEVI) = D(IP+NSI1)-D(IP+NSI2)                                       
   99 CONTINUE                                                                  
      RETURN                                                                    
C     MRGEPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF MERGING OF TWO FLOWS                                      
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE MRGEPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151)), (CERR,C(16))                
     *, (OUT,C(7))                                                              
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER CERR, OUT                                                         
      EQUIVALENCE (SCR(1),NLI1), (SCR(2),NSI1), (SCR(3),NLI2)                   
     *, (SCR(4),NSI2), (SCR(5),NLO), (SCR(6),NSO), (SCR(7),IOP)                 
     *, (SCR(8),NF1), (SCR(9),NF2)                                              
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG IN 1                                                                  
C   3 STATION IN 1                                                              
C   4 LEG IN 2                                                                  
C   5 STATION IN 2                                                              
C   6 LEG OUT                                                                   
C   7 STATION OUT                                                               
C   8 IOP                                                                       
C   9 ERROR VARIABLE INDEX                                                      
      I = IACDB(9)                                                              
      ID(I+1) = ICV(1)                                                          
      NLI1 = ILEGN(ICV(2))                                                      
      ID(I+2) = NLI1                                                            
      CALL LEGRT(NLI1)                                                          
      NSI1 = ISTAN(ICV(3))                                                      
      ID(I+3) = NSI1                                                            
      CALL START(NSI1)                                                          
      CALL FRR(NLI1,NF1)                                                        
      IOP = ICV(8)                                                              
      IF (IOP.LT.0) GO TO 1                                                     
      NLI2 = ILEGN(ICV(4))                                                      
      ID(I+4) = NLI2                                                            
      CALL LEGRT(NLI2)                                                          
      NSI2 = ISTAN(ICV(5))                                                      
      ID(I+5) = NSI2                                                            
      CALL START(NSI2)                                                          
      CALL FRR(NLI2,NF2)                                                        
      IF (NF1.EQ.0.OR.NF2.EQ.0.OR.NF1.EQ.NF2) GO TO 1                           
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR                                                     
 1000 FORMAT(6H0ERROR,I6,5X,25HMERGE OF DIFFERENT FLUIDS)                       
    1 NLO = ILEGN(ICV(6))                                                       
      ID(I+6) = NLO                                                             
      CALL LEGRS(NLO)                                                           
      NSO = ISTAN(ICV(7))                                                       
      ID(I+7) = NSO                                                             
      CALL STARS(NSO)                                                           
      CALL FRS(NLO,NF1)                                                         
      ID(I+8) = IOP                                                             
      IF (IOP.LE.0) GO TO 99                                                    
C$$ WAS "I+8" IN "H017"                                                         
      ID(I+9) = IAEV(2)                                                         
   99 CONTINUE                                                                  
      RETURN                                                                    
C     MRGEPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE NRLRPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)),(IW,C(27)),(IP,C(28)),(IT,C(29))                 
     *, (IH,C(30)), (SCR(1),C(151)), (ICPP,C(88)), (OUT,C(7))                   
     *, (IFP,C(22)), (IGA,C(35)), (ILN,C(37)), (PASS,C(17))                     
     *,(CERR,C(16)),(IIOP,C(90)),(DP(1),C(171)),(ITAD,C(54))                    
     *,(IFB,C(55)),(GVTY,C(370)),(CJ,C(371))                                    
     *,(VRC,C(378)),(VRH,C(379)),(PCOLD,C(380)),(PHOT,C(381))                   
      DIMENSION SCR(30),DP(8)                                                   
      INTEGER PASS,OUT,CERR                                                     
      REAL MUR1,MUR2,MUAVG,MH,NC,LC,LH,MC                                       
      EQUIVALENCE  (SCR(1),NLC), (SCR(2),NSIC), (SCR(3),NSOC)                   
     *, (SCR(4),NLH), (SCR(5),NSIH), (SCR(6),NSOH), (SCR(7),AC)                 
     *,(SCR(8),AH),(SCR(9),ITBL1),(SCR(10),ITBL2),(SCR(11),HTCL)                
     *,(SCR(12),HTC2),(SCR(13),HTCV),(SCR(14),HTHL),(SCR(15),HTH2)              
     *, (SCR(16),HTHV)                                                          
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
     *,(D(41),VFC),(D(42),CFMC),(D(43),VGH),(D(44),VFH)                         
     *,(D(49),SC),(D(50),VSATC),(D(51),HSATC),(D(52),HSATH)                     
     *,(D(53),LC),(D(54),SH),(D(55),VSATH),(D(56),LH)                           
     *,(D(57),QC),(D(58),QH),(D(59),HN),(D(60),HH)                              
     *,(D(61),CQ),(D(62),HQ),(D(63),PINH),(D(64),PINC)                          
     *,(D(65),TTH),(D(67),HLOW),(D(68),V)                                       
     *,(D(69),HIGH),(D(70),HTEST),(D(71),HOUTH),(D(72),TH)                      
     *,(D(73),KEY),(D(74),TVTC),(D(75),TVTH),(D(76),XCX)                        
C   1 COMP CODE                                                                 
C   2 LEG COLD NLC                                                              
C   3 STATION IN COLD NSIC                                                      
C   4 STATION OUT COLD NSOC                                                     
C   5 LEG HOT NLH                                                               
C   6 STATION IN HOT NSIH                                                       
C   7 STATION OUT HOT NSOH                                                      
C   8 FLOW AREA COLD AC                                                         
C   9 FLOW AREA HOT AH                                                          
C  10 PRESSURE DROP TABLE RELATIVE NUMBER FOR TYPES 61 AND 62                   
C     (COLD AND HOT SIDES, RESPECTIVELY), FT OF VAPOR VS CFM WITH               
C     VAPOR ONLY                                                                
C  11 HEAT TRANSFER COEFFICIENT, LIQUID HTCL                                    
C  12 HEAT TRANSFER COEFICEIENT, 2 PHASE HTC2                                   
C  13 HEAT TRANSFER COEFICEIENT, VAPOR HTCV                                     
C  14 HEAT TRANSFER AREA ON COLD SIDE (SQUARE FEET)                             
C  15 HEAT TRANSFER AREA ON HOT SIDE (SQUARE FEET)                              
C*** PRESSURE-DROP FUNCTION FOR TWO-PHASE FLOW BASED ON                         
C    FAC. INC., CORRELATION OF BAROCZY'S METHOD                                 
      FNQ(X,PF)=-PF*X**1.65/2.65+(1.+PF)*(X**.7/1.7-(1.-X)**2.3                 
     */2.3+(1.-(1.-X)**3.3)/2.3/3.3/X)                                          
C*** VOID-FRACTION CORRELATION                                                  
      FNV(X,VR)=X/(X+SL*(1.-X)/VR)                                              
C*** RATIO OF MOMENTUM FLUX TO MOMENTUM OF LIQUID-ONLY FLOW                     
      FNM(X,VR)=((1.-X)/(1.-VE))**2*(1.-VE*(1.-SL*SL/VR))                       
C*** DERIVATIVE OF THE PRESSURE-DROP FUNCTION                                   
      FNP(X,PF)=-PF*X**1.65+(1.+PF)*(X**.7+X*(1.-X)**1.3)                       
      IRCD = IRCDB(16)                                                          
      NLC  = ID(IRCD+2)                                                         
      NSIC = ID(IRCD+3)                                                         
      NSOC = ID(IRCD+4)                                                         
      NLH  = ID(IRCD+5)                                                         
      NSIH = ID(IRCD+6)                                                         
      NSOH = ID(IRCD+7)                                                         
      AC = D(ID(IRCD+8))                                                        
      AH = D(ID(IRCD+9))                                                        
      LCOLD = ID(IFB+NLC)                                                       
      LCOLD=ID(LCOLD+3)                                                         
      HTCL=D(ID(IRCD+12))                                                       
      HTC2=D(ID(IRCD+13))                                                       
      HTCV=D(ID(IRCD+14))                                                       
      HTHL=HTCL                                                                 
      HTH2=HTC2                                                                 
      HTHV=HTCV                                                                 
      AHTC=D(ID(IRCD+15))                                                       
      AHTH=D(ID(IRCD+16))                                                       
C$WAS LHOT=LCOLD                                                                
C$   - (ORIGINAL NRLRPP ASSUMED THE HOT REFRIG WAS EXACT SAME # AS COLD)        
C$           (THIS WAS PROBABLY TO MAKE THE "INDATA" CARD WORK WITHOUT          
C$           (DEFINING THE FLUID WITH THE INDATA CARD)                          
C$   -   COPY FORMAT OF LCOLD FOR LHOT AND ADD 2 LINES BELOW TO                 
C$       ALLOW DIFFERENT FLUID NUMBERS ON EACH SIDE                             
      LHOT = ID(IFB+NLH)                                                        
      LHOT=ID(LHOT+3)                                                           
      MC=D(IW+NLC)*60.0                                                         
      MH=D(IW+NLH)*60.0                                                         
      ITRTTN=0                                                                  
      SC=VTS(LCOLD,D(IP+NSIC))                                                  
      VSATC=VSV('NRLRPP 1',LCOLD,D(IP+NSIC),SC)                                 
      HSATC=VH(LCOLD,D(IP+NSIC),SC,VSATC)                                       
      LC=VHFG(LCOLD,D(IP+NSIC),SC,VSATC)                                        
      HLIQC=HSATC-LC                                                            
      SH=VTS(LHOT,D(IP+NSIH))                                                   
      VSATH=VSV('NRLRPP 2',LHOT,D(IP+NSIH),SH)                                  
      LH=VHFG(LHOT,D(IP+NSIH),SH,VSATH)                                         
      HSATH=VH(LHOT,D(IP+NSIH),SH,VSATH)                                        
      HLIQH=HSATH-LH                                                            
      TC=D(IT+NSIC)                                                             
      TH=D(IT+NSIH)                                                             
      QC=1.-(HSATC-D(IH+NSIC))/LC                                               
      IF(QC.LT.0.0)QC=0.0                                                       
      IF(QC.GT.1.0)QC=1.0                                                       
      QH=1.-(HSATH-D(IH+NSIH))/LH                                               
      IF(QH.LT.0.0)QH=0.0                                                       
      IF(QH.GT.1.0)QH=1.0                                                       
   80 HN=D(IH+NSIH)                                                             
      HH=HN                                                                     
      CN=D(IH+NSIC)                                                             
      PINH=D(IP+NSIH)                                                           
      PINC=D(IP+NSIC)                                                           
      TTH=D(IT+NSIH)                                                            
      CQ=QC                                                                     
      HQ=QH                                                                     
      HLOW=VHLIQ(LHOT,D(IP+NSIH),D(IT+NSIC))                                    
      IF(TC.GT.SH) HLOW=VH(LHOT,D(IP+NSIH),TC,                                  
C$WAS:   "NSO" IS MEANINGLESS (=0.),USE IP+NSIH AS IN "HLOW" LINE ABOVE         
C$   *                    VSV('NRLRPP 3',LHOT,D(IP+NSO),TC))                    
C$ IS                                                                           
     *                    VSV('NRLRPP 3',LHOT,D(IP+NSIH),TC))                   
      HIGH=HH                                                                   
      HTEST=HIGH-HLOW                                                           
      TVTC=VTS(LCOLD,PINC)                                                      
      TVTH=VTS(LHOT,PINH)                                                       
      FC=VCPF(LCOLD,PINC,TVTC)                                                  
      FH=VCPF(LHOT,PINH,TVTH)                                                   
      GC=VCPV(LCOLD,PINC,TVTC)                                                  
      GH=VCPV(LHOT,PINH,TVTH)                                                   
  200 HOUTH=0.5*(HLOW+HIGH)                                                     
      HH=HOUTH                                                                  
      QH=0.0                                                                    
      IF(HH.LT.HLIQH)TH=VTLIQ(LHOT,PINH,HH)                                     
      IF(HH.GE.HLIQH)TH=SH                                                      
      QH=1.-(HSATH-HH)/LH                                                       
      IF(QH.GT.1.0)CALL VTAV2(LHOT,TH,VHOT,PINH,HH)                             
      T0=TH                                                                     
      TC=D(IT+NSIC)                                                             
      HC=CN                                                                     
      QC=CQ                                                                     
      X0=0.0                                                                    
      KEY=0                                                                     
  280 IF(HH.LT.HLIQH)GO TO 290                                                  
      IF(HH.GE.HLIQH.AND.HH.LT.HSATH) GO TO 300                                 
      GO TO 310                                                                 
  290 IF(HC.LT.HLIQC)GO TO 320                                                  
      IF(HC.GE.HLIQC.AND.HC.LT.HSATC)GO TO 610                                  
      GO TO 770                                                                 
  300 IF(HC.LT.HLIQC)GO TO 970                                                  
      IF(HC.GE.HLIQC.AND.HC.LT.HSATC)GO TO 1150                                 
      GO TO 1300                                                                
  310 IF(HC.LT.HLIQC)GO TO 1460                                                 
      IF(HC.GE.HLIQC.AND.HC.LT.HSATC)GO TO 1680                                 
      GO TO 1840                                                                
  320 CHTCL=HTCL                                                                
      CH=HTHL                                                                   
      XCX=MC*FC/(MH*FH)                                                         
      NC=1.0/(MC*FC*(1.0/(AHTC*CHTCL)+1.0/(AHTH*CH)))                           
      IF((NC*(1.0-XCX)).GT.(14./0.4343))GO TO 2070                              
      IF(XCX.EQ.1.)GO TO 440                                                    
      XH=(XCX*(TH-TC)/(TH-XCX*TC-(1.-XCX)*SH))                                  
      IF(XH) 380,380,390                                                        
  380 XH=1.1                                                                    
      GO TO 400                                                                 
  390 XH=ALOG(XH)/(NC*(1.-XCX))                                                 
  400 XC=((TH-TC)/(TH-XCX*TC-(1.-XCX)*SC))                                      
      IF(XC) 410,410,420                                                        
  410 XC=1.1                                                                    
      GO TO 460                                                                 
  420 XC=ALOG(XC)/(NC*(1.-XCX))                                                 
      GO TO 460                                                                 
  440 XH=(SH-TH)/(NC*(TH-TC))                                                   
      XC=(SC-TC)/(NC*(TH-TC))                                                   
  460 X=XC                                                                      
      IF(XC.GT.XH)X=XH                                                          
      IF((X+X0)-1.0)480,480,470                                                 
  470 X=1.-X0                                                                   
      KEY=1                                                                     
  480 IF(XCX.EQ.1.0) GO TO 520                                                  
      LINE=480                                                                  
      QTY=-NC*(1.0-XCX)*X                                                       
C     WRITE(OUT,1717) LINE,QTY                                                  
 1717 FORMAT(5X,'*****JUST PAST LINE',I4,'*****      QTY=',E17.7)               
      DH=(TH-XCX*TC+XCX*(TC-TH)*EXP(-NC*(1.0-XCX)*X))/(1.0-XCX)                 
      DC=(TH-XCX*TC+(TC-TH)*EXP(-NC*(1.-XCX)*X))/(1.-XCX)                       
      GO TO 560                                                                 
  520 DH=TH+NC*(TH-TC)*X                                                        
      DC=TC+NC*(TH-TC)*X                                                        
  560 HC=VHLIQ(LCOLD,PINC,DC)                                                   
      HH=VHLIQ(LHOT,PINH,DH)                                                    
      IF(X.NE.XH)GO TO 565                                                      
      DH=SH                                                                     
      HH=HLIQH                                                                  
  565 IF(X.NE.XC)GO TO 570                                                      
      DC=SC                                                                     
      HC=HLIQC                                                                  
  570 TH=DH                                                                     
      TC=DC                                                                     
      QH=1.0-(HSATH-HH) /LH                                                     
      QC=1.0-(HSATC-HC) / LC                                                    
C*** DETERMINE WHERE THE COMPUTATIONS ARE TO PROCEED                            
C*** BASED ON THE NEXT FLOW REGION                                              
      GO TO 1990                                                                
C*** COLD SIDE TWO-PHASE; HOT SIDE LIQUID                                       
C*** COMPUTE USING THE SAME LOGIC AS ABOVE FOR LIQUID-LIQUID FLOWS              
  610 CHTCL=HTC2                                                                
      CH=HTHL                                                                   
      XCX=MC*LC/(MH*FH*(TH-TC))                                                 
      NC=1.0/(MH*FH*XCX*(1./(AHTC*CHTCL)+1./(AHTH*CH)))                         
      XH=(SH-TC)/(TH-TC)                                                        
      IF(XH) 651,651,652                                                        
  651 XH=1.1                                                                    
      GO TO 660                                                                 
  652 XH=ALOG(XH)/(XCX*NC)                                                      
  660 XC=(1.+XCX*(1.-QC))                                                       
      IF (XC) 661,661,662                                                       
  661 XC=1.1                                                                    
      GO TO 670                                                                 
  662 XC=ALOG(XC)/(XCX*NC)                                                      
  670 X=XC                                                                      
      IF(XC.GT.XH)X=XH                                                          
      IF((X+X0)-1.) 690,690,680                                                 
  680 X=1.-X0                                                                   
      KEY=1                                                                     
      LINE=680                                                                  
      QTY=XCX*NC*X                                                              
C     WRITE(OUT,1717) LINE,QTY                                                  
  690 DH=TC+(TH-TC)*EXP(XCX*NC*X)                                               
      HH=VHLIQ(LHOT,PINH,DH)                                                    
      IF(X.NE.XH)GO TO 695                                                      
      DH=SH                                                                     
      HH=HLIQH                                                                  
  695 TH=DH                                                                     
      QH=1.0-(HSATH-HH) / LH                                                    
      QC=QC-(1.-EXP(XCX*NC*X))/XCX                                              
      IF(X.NE.XC) GO TO 750                                                     
      QC=1.0                                                                    
      TC=SC                                                                     
      HC=HSATC                                                                  
      GO TO 1990                                                                
  750 HC=HSATC-(1.-QC)*LC                                                       
      GO TO 1990                                                                
C*** COLD SIDE VAPOR, HOT SIDE LIQUID                                           
  770 CHTCL=HTCV                                                                
      CH=HTHL                                                                   
      XCX=MC*GC/(MH*FH)                                                         
      NC=1./(MC*GC*(1./(AHTC*CHTCL)+1./(AHTH*CH)))                              
      IF(XCX.EQ.1.) GO TO 840                                                   
      XH=(XCX*(TH-TC)/(TH-XCX*TC-(1.-XCX)*SH))                                  
      IF(XH) 821,821,822                                                        
  821 XH=1.1                                                                    
      GO TO 850                                                                 
  822 XH=ALOG(XH)/(NC*(1.-XCX))                                                 
      GO TO 850                                                                 
  840 XH=(SH-TH)/(NC*(TH-TC))                                                   
  850 X=XH                                                                      
      IF((X+X0)-1.) 870,870,860                                                 
  860 X=1.0-X0                                                                  
      KEY=1                                                                     
  870 IF(XCX.EQ.1.0) GO TO 910                                                  
      LINE=870                                                                  
      QTY= -NC*(1.0-XCX)*X                                                      
C     WRITE(OUT,1717) LINE,QTY                                                  
  880 DH=(TH-XCX*TC+XCX*(TC-TH)*EXP(-NC*(1.-XCX)*X))/(1.-XCX)                   
      DC=(TH-XCX*TC+(TC-TH)*EXP(-NC*(1.-XCX)*X))/(1.-XCX)                       
      GO TO 930                                                                 
  910 DH=TH+NC*(TH-TC)*X                                                        
      DC=TC+NC*(TH-TC)*X                                                        
  930 HH=VHLIQ(LHOT,PINH,DH)                                                    
      IF(X.NE.XH)GO TO 940                                                      
      DH=SH                                                                     
      HH=HLIQH                                                                  
  940 TH=DH                                                                     
      V=VSV('NRLRPP 4',LCOLD,PINC,DC)                                           
      HC=VH(LCOLD,PINC,DC,V)                                                    
      QH=1.0-(HSATH-HH)/LH                                                      
      TC=DC                                                                     
      GO TO 1990                                                                
  970 CH=HTH2                                                                   
      CHTCL=HTCL                                                                
      XCX=MC*FC*(SH-TC)/(MH*LH)                                                 
      NC=1./(MC*FC*(1./(AHTC*CHTCL)+1./(AHTH*CH)))                              
      XH=(XCX/(XCX-1.+QH))                                                      
      IF(XH)1020,1020,1030                                                      
 1020 XH=1.1                                                                    
      GO TO 1040                                                                
 1030 XH=ALOG(XH)/NC                                                            
 1040 XC=(TC-TH)/(SC-TH)                                                        
      IF(XC) 1041,1041,1042                                                     
 1041 XC=1.1                                                                    
      GO TO 1050                                                                
 1042 XC=ALOG(XC)/NC                                                            
 1050 X=XC                                                                      
      IF(XC.GT.XH) X=XH                                                         
      IF((X+X0)-1.) 1070,1070,1060                                              
 1060 X=1.0-X0                                                                  
      KEY=1                                                                     
      LINE=1060                                                                 
      QTY= -NC*X                                                                
C     WRITE(OUT,1717) LINE,QTY                                                  
 1070 QH=QH+XCX*(1.0-EXP(-NC*X))                                                
      HH=HSATH - (1.0-QH)*LH                                                    
      IF(X.EQ.XH)HH=HSATH                                                       
      DC=TH+(TC-TH)*EXP(-NC*X)                                                  
      HC=VHLIQ(LCOLD,PINC,DC)                                                   
      IF(X.NE.XC)GO TO 1090                                                     
      DC=SC                                                                     
      HC=HLIQC                                                                  
 1090 TH=SH                                                                     
      TC=DC                                                                     
      QC=1.0-(HSATC-HC) / LC                                                    
      GO TO 1990                                                                
C*** BOTH SIDES TWO-PHASE                                                       
 1150 CH=HTH2                                                                   
      CHTCL=HTC2                                                                
      XCX=MC*LC/(MH*LH)                                                         
      NC=(SH-SC)/(MC*LC*(1./(AHTC*CHTCL)+1./(AHTH*CH)))                         
      XC=(1.-QC)/NC                                                             
      XH=(1.-QH)/(XCX*NC)                                                       
      X=XC                                                                      
      IF(XC.GT.XH)X=XH                                                          
      IF((X+X0).LE.1.0) GO TO 1230                                              
      X=1.-X0                                                                   
      KEY=1                                                                     
 1230 QC=QC+NC*X                                                                
      QH=QH+NC*XCX*X                                                            
      HC=HSATC-(1.0-QC)*LC                                                      
      HH=HSATH-(1.0-QH)*LH                                                      
      IF(X.EQ.XH)HH=HSATH                                                       
      IF(X.EQ.XC)HC=HSATC                                                       
      TC=SC                                                                     
      TH=SH                                                                     
      GO TO 1990                                                                
C*** HOT SIDE IS TWO-PHASE, COLD SIDE IS A VAPOR                                
 1300 CH=HTH2                                                                   
      CHTCL=HTCV                                                                
      XCX=MC*GC/(MH*LH/(TH-TC))                                                 
      NC=1./(MC*GC*(1./(AHTC*CHTCL)+1./(AHTH*CH)))                              
      XH=(XCX/(XCX-1.+QH))                                                      
      IF(XH) 1350,1350,1360                                                     
 1350 XH=1.1                                                                    
      GO TO 1370                                                                
 1360 XH=ALOG(XH)/NC                                                            
 1370 X=XH                                                                      
      IF((X+X0)-1.) 1390,1390,1380                                              
 1380 X=1.-X0                                                                   
      KEY=1                                                                     
      LINE=1380                                                                 
      QTY=-NC*X                                                                 
C     WRITE(OUT,1717) LINE,QTY                                                  
      IF(NC*X.GT.14./0.4343) GO TO 2070                                         
 1390 QH=QH+XCX*(1.-EXP(-NC*X))                                                 
      DC=TH+(TC-TH)*EXP(-NC*X)                                                  
      HH=HSATH-(1.0-QH)*LH                                                      
      IF(X.EQ.XH)HH=HSATH                                                       
      V=VSV('NRLRPP 5',LCOLD,PINC,DC)                                           
      HC=VH(LCOLD,PINC,DC,V)                                                    
      TC=DC                                                                     
      TH=SH                                                                     
      GO TO 1990                                                                
C*** HOT SIDE VAPOR, COLD SIDE LIQUID                                           
 1460 CH=HTHV                                                                   
      CHTCL=HTCL                                                                
      XCX=MC*FC/(MH*GH)                                                         
      HH=HSATH-(1.0-QH)*LH                                                      
      NC=1./(MC*FC*(1./(AHTC*CHTCL)+1./(AHTH*CH)))                              
      IF(XCX.EQ.1.0) GO TO 1530                                                 
      XC=((TH-TC)/(TH-XCX*TC-(1.-XCX)*SC))                                      
      IF(XC) 1511,1511,1512                                                     
 1511 XC=1.1                                                                    
      GO TO 1540                                                                
 1512 XC=ALOG(XC)/(NC*(1.-XCX))                                                 
      GO TO 1540                                                                
 1530 XC=(SC-TC)/(NC*(TH-TC))                                                   
 1540 X=XC                                                                      
      IF((X+X0)-1.) 1560,1560,1550                                              
 1550 X=1.0-X0                                                                  
      KEY=1                                                                     
 1560 IF(XCX.EQ.1.0) GO TO 1600                                                 
      DC=(TH-XCX*TC+(TC-TH)*EXP(-NC*(1.-XCX)*X))/(1.-XCX)                       
      DH=(TH-XCX*TC+XCX*(TC-TH)*EXP(-NC*(1.-XCX)*X))/                           
     * (1.-XCX)                                                                 
      GO TO 1620                                                                
 1600 DC=TC+(TH-TC)*NC*X                                                        
      DH=TH+(TH-TC)*NC*X                                                        
 1620 V=VSV('NRLRPP 6',LHOT,PINH,DH)                                            
      HH=VH(LHOT,PINH,DH,V)                                                     
      HC=VHLIQ(LCOLD,PINC,DC)                                                   
      IF(X.NE.XC)GO TO 1630                                                     
      DC=SC                                                                     
      HC=HSATC                                                                  
 1630 TH=DH                                                                     
      QC=1.0-(HSATC-HC) / LC                                                    
      TC=DC                                                                     
      GO TO 1990                                                                
C*** HOT SIDE VAPOR, COLD SIDE TWO-PHASE                                        
 1680 CH=HTHV                                                                   
      CHTCL=HTC2                                                                
      XCX=MC*LC/(MH*GH*(TH-SC))                                                 
      NC=1./(MH*GH*XCX*(1./(AHTC*CHTCL)+1./(AHTH*CH)))                          
      XC=(1.+XCX*(1.-QC))                                                       
      IF(XC)1730,1730,1740                                                      
 1730 XC=1.1                                                                    
      GO TO 1750                                                                
 1740 XC=ALOG(XC)/(XCX*NC)                                                      
 1750 X=XC                                                                      
      IF((X+X0)-1.0) 1770,1770,1760                                             
 1760 X=1.0-X0                                                                  
      KEY=1                                                                     
      LINE=1760                                                                 
      QTY=XCX*NC*X                                                              
C     WRITE(OUT,1717) LINE,QTY                                                  
 1770 DH=TC+(TH-TC)*EXP(XCX*NC*X)                                               
      QC=QC-(1.0-EXP(XCX*NC*X))/XCX                                             
      V=VSV('NRLRPP 7',LHOT,PINH,DH)                                            
      HH=VH(LHOT,PINH,DH,V)                                                     
      TH=DH                                                                     
      HC=HSATC-(1.-QC)*LC                                                       
      IF(X.NE.XC)GO TO 1990                                                     
      TC=SC                                                                     
      HC=HSATC                                                                  
      GO TO 1990                                                                
C*** BOTH SIDES VAPOR                                                           
 1840 CH=HTHV                                                                   
      CHTCL=HTCV                                                                
      XCX=MC*GC/(MH*GH)                                                         
      NC=1./(MC*GC*(1./(AHTC*CHTCL)+1./(AHTH*CH)))                              
      X=1.-X0                                                                   
      IF(XCX.EQ.1.0) GO TO 1930                                                 
      LINE=1840                                                                 
      QTY= -NC*(1.0-XCX)*X                                                      
C     WRITE(OUT,1717) LINE,QTY                                                  
      IF (NC*(1.-XCX)*X.GT.14./0.4343) GO TO 2070                               
      DH=(TH-XCX*TC+XCX*(TC-TH)*EXP(-NC*(1.-XCX)*X))/(1.-XCX)                   
      DC=(TH-XCX*TC+(TC-TH)*EXP(-NC*(1.-XCX)*X))/(1.-XCX)                       
      GO TO 1950                                                                
 1930 DH=TH+NC*(TH-TC)*X                                                        
      DC=TC+NC*(TH-TC)*X                                                        
 1950 V=VSV('NRLRPP 8',LHOT,PINH,DH)                                            
      HH=VH(LHOT,PINH,DH,V)                                                     
      V=VSV('NRLRPP 9',LCOLD,PINC,DC)                                           
      HC=VH(LCOLD,PINC,DC,V)                                                    
      TH=DH                                                                     
      TC=DC                                                                     
      KEY=1                                                                     
 1990 X0=X0+X                                                                   
      ITRTTN=ITRTTN+1                                                           
      IF(ITRTTN.GT.100)GO TO 2055                                               
      IF(KEY) 280,280,2025                                                      
 2025 IF(ABS(HH-HN).LT.(.0001*HTEST)) GO TO 2060                                
      IF(HH-HN) 2040,2040,2050                                                  
 2040 HLOW=HOUTH                                                                
      GO TO 200                                                                 
 2050 HIGH=HOUTH                                                                
      GO TO 200                                                                 
 2055 CONTINUE                                                                  
C     WRITE(OUT,2056) HH,HN                                                     
 2056 FORMAT(' ITERATION LIMIT IN INTERCOOLER HH/HN =',2F10.2)                  
      GO TO 2072                                                                
 2060 CONTINUE                                                                  
C     WRITE(OUT,2061)                                                           
 2061 FORMAT(' THE INTERCOOLER HEAT TRANSFER ANALYSED COMPLETELY'               
     * )                                                                        
C     WRITE(OUT,2062)                                                           
 2062 FORMAT('         TIN         HIN      TOUT     HOUT')                     
C     WRITE(OUT,2063)D(IT+NSIC),CN,TC,HC                                        
 2063 FORMAT(' COLD',4F10.3)                                                    
C     WRITE(OUT,2064)TTH,HN,T0,HOUTH                                            
 2064 FORMAT(' HOT',4F10.3)                                                     
      DO 2100 I=1,8                                                             
 2100 DP(I)=0.0                                                                 
      MC=MC/3600.                                                               
      MH=MH/3600.                                                               
      VGC=VSATC                                                                 
      VFC=1./VDL(LCOLD,SC)                                                      
      VRC=VGC/VFC                                                               
      VGH=VSATH                                                                 
      VFH=1./VDL(LHOT,SH)                                                       
      VRH=VGH/VFH                                                               
      IF(ABS(QH-1.0).LT..0001)QH=1.0                                            
      IF(ABS(QC-1.0).LT..0001)QC=1.0                                            
      IF(QC.LT.0.0)QC=0.0                                                       
      IF(QC.GT.1.)QC=1.0                                                        
      IF(QH.LT.0.0)QH=0.0                                                       
      IF(QH.GT.1.)QH=1.0                                                        
      CFMC=D(IW+NLC)*VGC                                                        
      CFMH=D(IW+NLH)*VGH                                                        
      D(IGA+101)=CFMC                                                           
      D(IGA+102)=CFMH                                                           
      PCOLD=TLUP(ID(IRCD+10))*VGC                                               
      PHOT=TLUP(ID(IRCD+11))*VGH                                                
      SL=(VRC-1.)**.25                                                          
      PB=-0.25*ALOG10(VRC)                                                      
      IF(PB.LT.-1.)PB=-1.                                                       
      IF(CQ.EQ.0.0)GO TO 2290                                                   
      DP(1)=CQ*FNQ(CQ,PB)                                                       
      IF(CQ.EQ.1.0)GO TO 2282                                                   
      VE=FNV(CQ,VRC)                                                            
      DP(3)=FNM(CQ,VRC)                                                         
      GO TO 2290                                                                
 2282 DP(3)=VRC                                                                 
 2290 IF(QC)2330,2330,2295                                                      
 2295 DP(2)=QC*FNQ(QC,PB)                                                       
      IF(QC.EQ.1.)GO TO 2298                                                    
      VE=FNV(QC,VRC)                                                            
      DP(4)=FNM(QC,VRC)                                                         
      GO TO 2310                                                                
 2298 DP(4)=VRC                                                                 
 2310 IF((QC-CQ).LT.0.01)GO TO 2315                                             
      DP(1)=(DP(2)-DP(1))/(QC-CQ)                                               
      GO TO 2330                                                                
 2315 DP(1)=FNP(QC,PB)                                                          
 2330 DP(1)=PCOLD*((1./VRC)**2+(1.-(1./VRC)**2)*DP(1))                          
C$                                                                              
C$ IS THE LINE BELOW MISSING A DIVISOR OF 2,I.E. "ONE-HALF-RHO-VEE**2"          
C$        NEEDS FIXING                                                          
C$WAS DP(1)=DP(1)+(DP(4)-DP(3))*(MC/AC)**2*VFC/GVTY/144.                        
      DP(1)=DP(1)+(DP(4)-DP(3))*(MC/AC)**2*VFC/(2.*GVTY)/144.                   
      SL=(VRH-1.)**.25                                                          
      PB=-0.25*ALOG10(VRH)                                                      
      IF(PB.LT.-1.)PB=-1.                                                       
      IF(HQ.NE.0)GO TO 2370                                                     
      DP(2)=0.0                                                                 
      DP(4)=0.0                                                                 
      GO TO 2420                                                                
 2370 DP(2)=HQ*FNQ(HQ,PB)                                                       
      IF(HQ.NE.1.)GO TO 2371                                                    
      DP(4)=VRH                                                                 
      GO TO 2380                                                                
 2371 VE=FNV(HQ,VRH)                                                            
      DP(4)=FNM(HQ,VRH)                                                         
 2380 IF(QH.NE.0.0)GO TO 2390                                                   
      DP(3)=0.0                                                                 
      DP(5)=0.0                                                                 
      GO TO 2400                                                                
 2390 DP(3)=QH*FNQ(QH,PB)                                                       
      IF(QH.NE.1.0)GO TO 2395                                                   
 2392 DP(5)=VRH                                                                 
      GO TO 2400                                                                
 2395 VE=FNV(QH,VRH)                                                            
      DP(5)=FNM(QH,VRH)                                                         
 2400 IF((HQ-QH).GE..01)GO TO 2405                                              
      DP(2)=FNP(HQ,PB)                                                          
      GO TO 2420                                                                
 2405 DP(2)=(DP(3)-DP(2))/(QH-HQ)                                               
 2420 DP(2)=PHOT*((1./VRH)**2+(1.-(1./VRH)**2)*DP(2))                           
C$                                                                              
C$ IS THE LINE BELOW MISSING A DIVISOR OF 2,I.E. "ONE-HALF-RHO-VEE**2"          
C$        NEEDS FIXING                                                          
C$WAS DP(2)=DP(2)+(DP(5)-DP(4))*(MH/AH)**2*VFH/GVTY/144.                        
      DP(2)=DP(2)+(DP(5)-DP(4))*(MH/AH)**2*VFH/(2.*GVTY)/144.                   
C     WRITE(OUT,2500)DP(1)                                                      
 2500 FORMAT(' COLD SIDE PRESSURE DROP(PSI) = ',E12.5)                          
C     WRITE(OUT,2600)DP(2)                                                      
 2600 FORMAT(' HOT SIDE PRESSURE DROP(PSI) = ',E12.5)                           
      D(IP+NSOH)=PINH-DP(2)                                                     
      IF(D(IP+NSOH).GT.0.0)GO TO 2610                                           
C     WRITE(OUT,7655)D(IP+NSOH),D(IP+NSIH)                                      
 7655 FORMAT(' *+* OUTLET PRESSURE IN NRCLR HOT SIDE WAS ',E14.7,               
     * ' RESET PRESSURE TO INLET VALUE ',E14.7)                                 
      D(IP+NSOH)=D(IP+NSIH)                                                     
 2610 D(IP+NSOC)=PINC-DP(1)                                                     
      IF(D(IP+NSOC).GT.0.0) GO TO 2620                                          
C     WRITE(OUT,7654)ID(IP+NSOC),D(IP+NSIC)                                     
 7654 FORMAT(' *+*OUTLET PRESSURE IN NRCLR COLD SIDE WAS',E14.7,                
     * 'RESET PRESSURE TO INLET VALUE',E14.7)                                   
      D(IP+NSOC)=D(IP+NSIC)                                                     
 2620 D(IT+NSOH)=T0                                                             
      D(IT+NSOC)=TC                                                             
      D(IH+NSOH)=HOUTH                                                          
      D(IH+NSOC)=HC                                                             
      GO TO 2835                                                                
 2070 DPRC=.4343*NC*(1.-XCX)                                                    
C     WRITE(OUT,2071) DPRC                                                      
 2071 FORMAT(' INTERCOOLER TOO LARGE FOR FLOW RATE, PRECISION=',                
     * F10.3)                                                                   
 2072 D(IP+NSOH)=D(IP+NSIH)                                                     
      D(IP+NSOC)=D(IP+NSIC)                                                     
      TOUTH=D(IT+NSIC)                                                          
      TOUTC=D(IT+NSIH)                                                          
      IF(TOUTH-SH) 2710,2720,2720                                               
 2710 HOUTH=VHLIQ(LHOT,D(IP+NSOH),TOUTH)                                        
      GO TO 2730                                                                
 2720 VHOT=VSV('NRLRPP10',LHOT,D(IP+NSOH),TOUTH)                                
      HOUTH=VH(LHOT,D(IP+NSOH),TOUTH,VHOT)                                      
 2730 IF(TOUTC-SC)2740,2750,2750                                                
 2740 HOUTC=VHLIQ(LCOLD,D(IP+NSOC),TOUTC)                                       
      GO TO 2760                                                                
 2750 VCOLD=VSV('NRLRPP11',LCOLD,D(IP+NSOC),TOUTC)                              
      HOUTC=VH(LCOLD,D(IP+NSOC),TOUTC,VCOLD)                                    
 2760 QHOT=D(IW+NLH)*(D(IH+NSIH)-HOUTH)                                         
      QCOLD=D(IW+NLC)*(HOUTC-D(IH+NSIC))                                        
      IF(QCOLD-QHOT)2770,2770,2810                                              
 2770 D(IT+NSOC)=TOUTC                                                          
      D(IH+NSOC)=HOUTC                                                          
      D(IH+NSOH)=D(IH+NSIH)-QCOLD/D(IW+NLH)                                     
      QHOT=VQUALH('NRLRPP 1',LHOT,D(IP+NSOH),D(IH+NSOH))                        
      IF(QHOT)2780,2785,2785                                                    
 2780 D(IT+NSOH)=VTLIQ(LHOT,D(IP+NSOH),D(IH+NSOH))                              
      GO TO 2835                                                                
 2785 IF (QHOT.GT.1.0)GO TO 2790                                                
      D(IT+NSOH)=SH                                                             
      GO TO 2835                                                                
 2790 CALL VTAV2(LHOT,D(IT+NSOH),V,D(IP+NSOH),D(IH+NSOH))                       
 2800 RETURN                                                                    
 2810 D(IT+NSOH)=TOUTH                                                          
      D(IH+NSOH)=HOUTH                                                          
      D(IH+NSOC)=D(IH+NSIC)+QHOT/D(IW+NLC)                                      
      QCOLD=VQUALH('NRLRPP 2',LCOLD,D(IP+NSOC),D(IH+NSOC))                      
      IF (QCOLD)2820,2825,2825                                                  
 2820 D(IT+NSOC)=VTLIQ(LCOLD,D(IP+NSOC),D(IH+NSOC))                             
      GO TO 2835                                                                
 2825 IF(QCOLD.GT.1.0) GO TO 2830                                               
      D(IT+NSOC)=SC                                                             
      GO TO 2835                                                                
 2830 CALL VTAV2(LCOLD,D(IT+NSOC),V,D(IP+NSOC),D(IH+NSOC))                      
 2835 IF(IFP.NE.1.OR. ICPP.NE.0) GO TO 99                                       
      CALL PIOP(1,NLC,NSIC,NSOC)                                                
      CALL PIOP(2,NLH,NSIH,NSOH)                                                
      CALL LINES(2)                                                             
      Q1=D(IW+NLH)*(D(IH+NSIH)-D(IH+NSOH))                                      
      Q2=D(IW+NLC)*(D(IH+NSOC)-D(IH+NSIC))                                      
      Q=AMIN1 (Q1,Q2)                                                           
C     WRITE(OUT,*) Q1,Q2                                                        
      WRITE(OUT,1718) Q                                                         
 1718 FORMAT(5X,'INTERCOOLER HEAT TRANSFER = ',E17.7,'BTU/MIN')                 
   99 CONTINUE                                                                  
 2090 RETURN                                                                    
C     NRLRPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C                                                                               
C$    SUBROUTINE NRLRPZ IS NEW                                                  
C**********************************************************************         
C                                                                               
      SUBROUTINE NRLRPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
     *,(CERR,C(16)), (OUT,C(7))                                                 
      INTEGER OUT                                                               
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NLC), (SCR(2),NSIC),(SCR(3),NSOC)                     
     *, (SCR(4),NLH), (SCR(5),NSIH),(SCR(6),NSOH)                               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/100/                                                            
C   1 COMP CODE                                                                 
C   2 LEG COLD SIDE                                                             
C   3 STATION IN COLD SIDE                                                      
C   4 STATION OUT COLD SIDE                                                     
C   5 LEG HOT SIDE                                                              
C   6 STATION IN HOT SIDE                                                       
C   7 STATION OUT HOT SIDE                                                      
C   8 FLOW AREA COLD (AFC) , SQUARE FEET                                        
C   9 FLOW AREA HOT (AFH) , SQUARE FEET                                         
C  10 PRESSURE DROP TABLE RELATIVE NUMBER FOR TYPES 61 AND 62                   
C  11 HEAT TRANSFER COEFFICIENT, LIQUID FLOW {BTU/(HR*SQ FT*DEG)}               
C  12 HEAT TRANSFER COEFFICIENT, 2 PHASE {BTU/(HR*SQ FT*DEG)}                   
C  13 HEAT TRANSFER COEFFICIENT, VAPOR FLOW {BTU/(HR*SQ FT*DEG)}                
C  14 HEAT TRANSFER AREA ON COLD SIDE (SQUARE FEET)                             
C  15 HEAT TRANSFER AREA ON HOT SIDE (SQUARE FEET)                              
      I = IACDB(16)                                                             
      ID(I+1) = ICV(1)                                                          
      NLC = ILEGN(ICV(2))                                                       
      ID(I+2) = NLC                                                             
      CALL LEGRT(NLC)                                                           
      NSIC = ISTAN(ICV(3))                                                      
      ID(I+3) = NSIC                                                            
      CALL START(NSIC)                                                          
      NSOC = ISTAN(ICV(4))                                                      
      ID(I+4) = NSOC                                                            
      CALL STARS(NSOC)                                                          
      CALL FTL(NLC,IFTA)                                                        
      NLH =ILEGN(ICV(5))                                                        
      ID(I+5) =NLH                                                              
      CALL LEGRT(NLH)                                                           
      NSIH = ISTAN(ICV(6))                                                      
      ID(I+6) = NSIH                                                            
      CALL START(NSIH)                                                          
      NSOH = ISTAN(ICV(7))                                                      
      ID(I+7) = NSOH                                                            
      CALL STARS(NSOH)                                                          
      CALL FTL(NLH,IFTA)                                                        
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      ID(I+10) = ITIDN(ICV(10),61)                                              
      ID(I+11) = ITIDN(ICV(10),62)                                              
      ID(I+12) = IPARM(ICV(11))                                                 
      ID(I+13) = IPARM(ICV(12))                                                 
      ID(I+14) = IPARM(ICV(13))                                                 
      ID(I+15) = IPARM(ICV(14))                                                 
      ID(I+16)= IPARM(ICV(15))                                                  
      RETURN                                                                    
C     NRLRPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      A CHOKED AND UNCHOKED NOZZLE AND RAM AIR EXITS                           
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE NZZLPP                                                         
      COMMON /CC/ C(600)                                                        
      COMMON /DT/ DTES(64001)                                                   
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)),(SCR(1),C(151)), (PASS,C(17)), (IEV,C(49))                   
     *, (IEVT,C(48)), (IGA,C(35)), (BIG,C(31)), (OUT,C(7))                      
     *, (ICPP,C(88)), (IFP,C(22))                                               
     *, (ILN,C(37)), (ISN,C(38))     !C$ADDED                                   
      DIMENSION SCR(30)                                                         
      INTEGER PASS,OUT                                                          
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO), (SCR(4),JEVI)        
     *, (SCR(5),GAMMA), (SCR(6),PD), (SCR(7),PR), (SCR(8),PRCR)                 
     *, (SCR(9),A), (SCR(10),WCALC), (SCR(11),EFF)                              
     *, (SCR(12),HS), (SCR(13),SIGN), (SCR(14),PRS), (SCR(15),DP)               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG N0                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 DISCHARGE PRESSURE                                                        
C   6 OPTION                                                                    
C   7 PRESS RATIO TABLE OR CD TABLE NO                                          
C   8 EFF TABLE NO                                                              
C   9 DIA RATIO                                                                 
C  10 THROAT DIA                                                                
C  11 AREA                                                                      
C  12 ERROR VARIABLE INDEX                                                      
      IRCD = IRCDB(12)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      D(IGA+41) = D(IW+NL)                                                      
      JEVI = ID(IRCD+12)                                                        
      PD = D(ID(IRCD+5))                                                        
      GAMMA = GAM(NL,D(IP+NSI),D(IT+NSI))                                       
      EFF = TLUP(ID(IRCD+8))                                                    
      D(IRCD-7) = EFF                                                           
      SIGN = 1.0                                                                
      IF (ID(IRCD+6).EQ.1) GO TO 10  !OPTION 1 USES CD TABLE                                            
      PR = TLUP(ID(IRCD+7))                                                     
      PRS = PR                                                                  
      D(IP+NSO) = D(IP+NSI)*PR                                                  
      IF (PASS.NE.1) GO TO 1                                                    
      ID(IEVT+JEVI) = 2                                                         
    1 IF (PD.NE.0) DTES(IEV+JEVI) = D(IP+NSO)/PD                                
      D(IEV+JEVI) = D(IP+NSO)-PD                                                
      GO TO 20                                                                  
   10 PR = PD/D(IP+NSI)                                                         
      D(IP+NSO) = PD                                                            
      PRCR = ( 2. /(GAMMA +1.))** (GAMMA /(GAMMA - 1.))                         
      IF (PR.LE.PRCR.AND.PR.GE.0.0) PR = PRCR                                   
      PRS = PR                                                                  
      IF (ID(IRCD+11).EQ.0) GO TO 11                                            
      A = D(ID(IRCD+11))                                                        
      GO TO 12                                                                  
   11 A = 0.7854*D(ID(IRCD+10))**2                                              
   12 IF(PR.GT.0.0 .AND. PR.LE.1.0) GO TO 15                                    
      SIGN = -1.0                                                               
       DP = PD - D(IP + NSI)                                                    
       PR =  PD / (DP + PD )                                                    
       IF(PR.LE.PRCR .AND. PR.GE.0.0) PR = PRCR                                 
   15 IF(D(IGA+4).NE.BIG .AND. D(IGA+4).NE.0.0) GO TO 16                        
      D(IGA+27) = -1.0                                                          
      GO TO 17                                                                  
   16 D(IGA+27) = 2.4*D(IW+NL)/(A*DEN(0,D(IGA+5),D(IGA+6))*D(IGA+4))            
   17 IF (ID(IRCD+7).NE.0) A = A*TLUP(ID(IRCD+7))                               
C$NEW                                                                           
C***********************************************************************        
      IRITE=0                                                                   
      IF(D(ID(IRCD+9)) .GT. 1.0) ISTOP=1                                        
      IF(D(ID(IRCD+9)) .GT. 1.0) GO TO 666                                      
      IF (IRITE.LE.0) GO TO 918                                                 
C     IF (THING.GT.0.) GO TO 918                                                
  666 IRCD2=IRCD+2                                                              
      IRCD3=IRCD+3                                                              
      IRCD4=IRCD+4                                                              
 9888 WRITE (OUT,888) GAMMA,                                                    
     *         ID(ILN+NL),ID(ISN+NSI),ID(ISN+NSO)                               
C     IF (THING.LE.0.) WCALC=999.                                               
C     IF (THING.LE.0.) GO TO 18                                                 
  888 FORMAT(' NOZZLE TRYING TO TAKE SQRT(THING)='                              
     *     ,'  GAMMA =',E12.4                                                   
     *   ,/,'      NL=',I6   ,  '     NSI=',I6   ,  '     NSO=',I6 )            
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
 2001 DT    = D(ID(IRCD+10))                                                    
      DR    = D(ID(IRCD+9))                                                     
      DLINE = DT/DR                                                             
      FLOW  = D(IW+NL)                                                          
      D(IGA+21) = 0.25465*D(IW+NL)*DR/(VIS(NL,D(IP+NSI),D(IT+NSI))*DT)          
      RELINE= D(IGA+21)                                                         
      D(IGA+33) = D(IW+NL) * SQRT(D(IT+NSI)) / (D(IP+NSI)*.78539*DT**2)         
      FLOFNC= D(IGA+33)                                                         
      WRITE (OUT,8000) PR    , DT    , DR    , DLINE , FLOW                     
     *               , RELINE, EFF                                              
     *               , WCALC                                                    
 8000 FORMAT(' PRATIO=',F10.6,  '  DTHROT=',F10.4,  '  DRATIO=',F10.6           
     *     ,'  DLINE =',F10.4,  '  FLOW  =',E10.4,  '  RELINE=',E10.4           
     *     ,'  EFF   =',F10.4,/                                                 
     *, 77X,  'WCALC =',F10.4)                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C***********************************************************************        
  918 THING = (GAMMA/(GAMMA-1.0)                                                
     * *DEN(NL,D(IP+NSI),D(IT+NSI))* ABS(D(IP+NSI))*(PR**(2.0/GAMMA)            
     * -PR**((GAMMA+1.0)/GAMMA)))/SQRT(1.0-D(ID(IRCD+9))**4                     
     * *PR**(2.0/GAMMA))                                                        
      WCALC = SIGN*40.124*A*SQRT(THING)                                         
   18 IF (PASS.NE.1) GO TO 19                                                   
      ID(IEVT+JEVI) = 1                                                         
   19 IF (WCALC.NE.0) DTES(IEV+JEVI) = D(IW+NL)/WCALC                           
      D(IEV+JEVI) = D(IW+NL)-WCALC                                              
   20 D(IT+NSO) = D(IT+NSI)*(1.0-EFF*(1.0-ABS(PR)**((GAMMA-1.0)/GAMMA)))        
      D(IH+NSO) = D(IH+NSI)                                                     
      IF (D(IH+NSI).NE.0.0) CALL TDB(NL,NSI,NSO,HS)                             
      D(IRCD-6) = PRS                                                           
      IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      CALL PIOP(1,NL,NSI,NSO)                                                   
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HS) CALL HSOP(HS)                 
      CALL LINES(2)                                                             
      WRITE (OUT,1000) PRS,EFF                                                  
 1000 FORMAT(1H0,5X,2HPR,F8.4,3X,3HEFF,F7.4)                                    
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
 1001 DT    = D(ID(IRCD+10))                                                    
      DR    = D(ID(IRCD+9))                                                     
      DLINE = DT/DR                                                             
      FLOW  = D(IW+NL)                                                          
      D(IGA+21) = 0.25465*D(IW+NL)*DR/(VIS(NL,D(IP+NSI),D(IT+NSI))*DT)          
      RELINE= D(IGA+21)                                                         
      D(IGA+33) = D(IW+NL) * SQRT(D(IT+NSI)) / (D(IP+NSI)*.78539*DT**2)         
      FLOFNC= D(IGA+33)                                                         
      WRITE (OUT,9000) PR    , DT    , DR    , DLINE , FLOW                     
     *               , RELINE, EFF                                              
     *               , WCALC                                                    
 9000 FORMAT(' PRATIO=',F10.6,  '  DTHROT=',F10.4,  '  DRATIO=',F10.6           
     *     ,'  DLINE =',F10.4,  '  FLOW  =',E10.4,  '  RELINE=',E10.4           
     *     ,'  EFF   =',F10.4,/                                                 
     *, 77X,  'WCALC =',F10.4)                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF(ISTOP.NE.1) GO TO 99                                                   
      STOP                                                                      
   99 CONTINUE                                                                  
      RETURN                                                                    
C     NZZLPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A NOZZLE                                                  
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE NZZLPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                       
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 LEG N0                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 DISCHARGE PRESSURE                                                        
C   6 OPTION                                                                    
C   7 PRESS RATIO TABLE OR CD TABLE NO                                          
C   8 EFF TABLE NO                                                              
C   9 DIA RATIO                                                                 
C  10 THROAT DIA                                                                
C  11 THROAT AREA                                                               
C  12 ERROR VARIABLE INDEX                                                      
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
      ID(I+5) = IPARM(ICV(5))                                                   
      ID(I+6) = ICV(6)                                                          
      ID(I+8) = ITIDN(ICV(8),17)                                                
      IF (ICV(6).EQ.1) GO TO 1                                                  
      ID(I+7) = ITIDN(ICV(7),4)                                                 
      ID(I+12)  = IAEV(2)                                                       
      GO TO 99                                                                  
    1 IF (ICV(7).NE.0) ID(I+7) = ITIDN(ICV(7),9)                                
      ID(I+9) = IPARM(ICV(9))                                                   
      ID(I+10) = IPARM(ICV(10))                                                 
      ID(I+11) = IPARM(ICV(11))                                                 
      ID(I+12)  = IAEV(1)                                                       
   99 CONTINUE                                                                  
      RETURN                                                                    
C     NZZLPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      AN ORFICE                                                                
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE ORIFPP                                                         
      COMMON /CC/ C(600)                                                        
      COMMON /DT/ DTES(64001)                                                   
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)), (SCR(1),C(151)),(PASS,C(17)), (IEV,C(49))                   
     *, (IEVT,C(48)), (ISV,C(47)), (ISVT,C(46)), (IGA,C(35))                    
     *, (ICPP,C(88)), (IFP,C(22)), (OUT,C(7))                                   
     *, (IFB,C(55))                                                             
     *, (IIOP,C(90))                                                            
      DIMENSION SCR(30)                                                         
      INTEGER PASS,OUT                                                          
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
     *, (SCR(4),JEVI), (SCR(5),JSVI), (SCR(6),GAMMA), (SCR(7),PRCR)             
     *, (SCR(8),PR), (SCR(9),WCALC), (SCR(10),PD), (SCR(11),IOP)                
     *, (SCR(12),NC), (SCR(13),DR), (SCR(14),DT)                                
     *, (SCR(15),HS), (SCR(16),B2)                                              
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 TABLE / EQUATION OPTIONS                                                  
C   6 PRESS RATIO TABLE NO / ORIF COEFF TABLE NO                                
C   7 THROAT DIA                                                                
C   8 DIA RATIO                                                                 
C   9 STATE VARIABLE INDEX                                                      
C  10 ERROR VARIABLE INDEX                                                      
C  11 FLOW COEF MULT                                                            
C  12 PRESSURE RATIO INITIAL GUESS (OPTIONAL)                                   
      IRCD = IRCDB(12)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      D(IGA+41) = D(IW+NL)                                                      
      IOP = ID(IRCD+5)                                                          
      NC = ID(IFB+NL)                                                           
      NC = ID(NC+1)                                                             
      B2 = D(ID(IRCD+11))                                                       
C *** 3 STATEMENTS ADDED BY PFM 11/25/85                                        
      IF (ID(IRCD+11).EQ.0) B2 = 0.0                                            
      IF (B2.EQ.0.0) B2 = 1.0                                                   
      DT = D(ID(IRCD+7))                                                        
      IF (ID(IRCD+7).EQ.0) DT = 0.0                                             
      DR = D(ID(IRCD+8))                                                        
      IF (ID(IRCD+8).EQ.0) DR = 0.0                                             
      IF(DR.EQ.0.0 .OR. DT.EQ.0.0) GO TO 10                                     
      D(IGA+20) = DR                                                            
      D(IGA+21) = 0.25465*D(IW+NL)*DR/(VIS(NL,D(IP+NSI),D(IT+NSI))*DT)          
      D(IGA+33) = D(IW+NL) * SQRT(D(IT+NSI)) / (D(IP+NSI)*.78539*DT**2)         
      GO TO 9                                                                   
   10 D(IGA+20) = -1.0                                                          
      D(IGA+21) = -1.0                                                          
      D(IGA+33) = -1.0                                                          
    9 IF (NC.EQ.2) GO TO 3                                                      
      IF (IOP.NE.0) GO TO 6                                                     
      PD = TLUP(ID(IRCD+6))                                                     
      GO TO 7                                                                   
   13 PD = (D(IW+NL) /( TLUP(ID(IRCD+6)) * B2 * DT ** 2                         
     * * 31.513 )) ** 2 / DEN(NL,D(IP+NSI),D(IT+NSI))                           
      GO TO 7                                                                   
    6 IF (IOP.EQ.2) GO TO 13                                                    
      PD = (D(IW+NL)* SQRT(1.0 - DR** 4) /(TLUP(ID(IRCD+6)) * DT ** 2           
     *  * 31.513))** 2  / DEN(NL,D(IP+NSI),D(IT+NSI))                           
    7 D(IP+NSO) = D(IP+NSI) - PD                                                
      D(IRCD-6) = PD                                                            
      GO TO 99                                                                  
    3 IF (IOP.NE.0) GO TO 4                                                     
      PR = TLUP(ID(IRCD+6))                                                     
      D(IRCD-6) = PR                                                            
      D(IP+NSO) = D(IP+NSI) * PR                                                
      GO TO 99                                                                  
    4 JSVI = ID(IRCD+9)                                                         
      JEVI = ID(IRCD+10)                                                        
      IF (PASS.NE.1) GO TO 1                                                    
      ID(ISVT+JSVI) = 9                                                         
      D(ISV+JSVI) = 0.9                                                        
      IF (ID(IRCD+12).EQ.0) GO TO 1                                             
      PR = D(ID(IRCD+12))                                                       
      IF (PR.LE.0.0 .OR. PR.GE.1.0) GO TO 1                                     
      D(ISV+JSVI) = PR                                                          
    1 GAMMA = GAM(NL,D(IP+NSI),D(IT+NSI))                                       
      PR = D(ISV+JSVI)                                                          
      D(IRCD-6) = PR                                                            
      D(IGA+24) = PR                                                            
      PRCR = ( 2. /(GAMMA+1.))**(GAMMA/(GAMMA-1.))                              
      IF (IIOP.NE.0) GO TO 14                                                   
      IF (PR.LE.PRCR.AND.PR.GE.0.0) PR = PRCR                                   
   14 IF (IOP.EQ.2) GO TO 12                                                    
      WCALC = 31.513 * TLUP(ID(IRCD+6)) * DT ** 2 * SQRT(                       
     * GAMMA/(GAMMA-1.0)*DEN(NL,D(IP+NSI),D(IT+NSI))*ABS(D(IP+NSI))             
     *  * (PR ** (2.0/GAMMA) - PR ** ((GAMMA +1.0)/GAMMA))) /SQRT(1.0           
     * - DR ** 4 * PR ** (2.0/ GAMMA) )                                         
      GO TO 11                                                                  
   12 WCALC = 31.513 * B2 * TLUP(ID(IRCD+6)) * DT ** 2 * SQRT( GAMMA /          
     * (GAMMA - 1.0) * DEN(NL,D(IP+NSI),D(IT+NSI)) * ABS(D(IP+NSI))             
     *  * (PR ** (2.0/GAMMA) - PR ** ((GAMMA+1.0)/ GAMMA)))                     
   11 IF(D(IP+NSI).LT.0.0) WCALC = -WCALC                                       
C$ ADD CD                                                                       
      CD=TLUP(ID(IRCD+6))                                                       
      IF (PASS.NE.1) GO TO 2                                                    
      ID(IEVT+JEVI) = 1                                                         
    2 D(IEV+JEVI) = D(IW+NL) - WCALC                                            
      IF(WCALC.NE.0) DTES(IEV+JEVI) = D(IW+NL)/WCALC                            
      D(IP+NSO) = D(IP+NSI) * D(ISV+JSVI)                                       
   99 CONTINUE                                                                  
      D(IT+NSO) = D(IT+NSI)                                                     
      D(IH+NSO) = D(IH+NSI)                                                     
      IF (D(IH+NSI).NE.0.0) CALL TDB(NL,NSI,NSO,HS)                             
      IF (IFP.NE.1.OR.ICPP.NE.0 ) GO TO 98                                      
      CALL PIOP(1,NL,NSI,NSO)                                                   
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HS) CALL HSOP(HS)                 
      IF (NC.EQ.1) GO TO 8                                                      
C$      WRITE (OUT,1000) PR                                                     
C$ 1000 FORMAT (1H0,5X,2HPR,F8.4    )                                           
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      DLINE = DT/DR                                                             
      FLOW  = D(IW+NL)                                                          
      RELINE= D(IGA+21)                                                         
      WRITE (OUT,9000) PR    , DT    , DR    , DLINE , FLOW                     
     *               , RELINE, CD                                               
     *               , WCALC                                                    
 9000 FORMAT(' PRATIO=',F10.6,  '  DTHROT=',F10.4,  '  DRATIO=',F10.6           
     *     ,'  DLINE =',F10.4,  '  FLOW  =',E10.4,  '  RELINE=',E10.4           
     *     ,'  CD    =',F10.4,/                                                 
     *, 77X,  'WCALC =',F10.4)                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      GO TO 98                                                                  
    8 WRITE (OUT,1001) PD                                                       
 1001 FORMAT (1H0,5X,2HPD,F8.4    )                                             
   98 CONTINUE                                                                  
      RETURN                                                                    
C     ORIFPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF AN ORFICE                                                 
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE ORIFPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
     *, (SCR(4),NC)                                                             
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/011/                                                            
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 TABLE / EQUATION OPTIONS                                                  
C   6 PRESS RATIO TABLE NO / ORIF COEFF TABLE NO                                
C   7 THROAT DIA                                                                
C   8 DIA RATIO                                                                 
C   9 STATE VARIABLE INDEX                                                      
C  10 ERROR VARIABLE INDEX                                                      
C  11 FLOW COEF MULT                                                            
C  12 PRESSURE RATIO INITIAL GUESS (OPTIONAL)                                   
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
      CALL FTR(NL,NC)                                                           
      ID(I+5) = ICV(5)                                                          
      IF (ICV(5).NE.0) GO TO 1                                                  
      IF (NC.EQ.0) GO TO 99                                                     
      IF (NC.EQ.1) GO TO 4                                                      
      IF (NC.EQ.2) GO TO 2                                                      
      CALL FTL(NL,IFTA)                                                         
      GO TO 99                                                                  
    4 ID(I+6) = ITIDN(ICV(6),1)                                                 
      GO TO 5                                                                   
    2 ID(I+6) = ITIDN(ICV(6),4)                                                 
      GO TO 5                                                                   
    1 ID(I+6) = ITIDN(ICV(6),9)                                                 
    5 ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+11) = IPARM(ICV(11))   !WAS ICV9                                               
      ID(I+12) = IPARM(ICV(12))   !$C WAS ICV9                                              
      CALL FTL(NL,IFTA)                                                         
      IF (NC.NE.2 .OR. ICV(5).EQ.0) GO TO 99                                    
      ID(I+9) = IASV(9)                                                         
      ID(I+10) = IAEV(1)                                                        
   99 CONTINUE                                                                  
      RETURN                                                                    
C     ORIFPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      THE OUTLET FROM THE ECS                                                  
C                                                                               
C$** ADDED  "RTLTPP"     REFRIGERANT OUTLET AS ENTRY POINT                      
C**********************************************************************         
C                                                                               
      SUBROUTINE OTLTPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IP,C(28)), (IW,C(27)), (PASS,C(17))            
C$   ADDED  OUT-C(7)                                                            
     *, (IEVT,C(48)), (IEV,C(49)), (IT,C(29)), (IH,C(30)), (OUT,C(7))           
     *, (SCR(1),C(151))                                                         
      INTEGER OUT, PASS                                                         
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (SCR(1),NL), (SCR(2),NS), (SCR(3),JEV)                        
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION                                                                   
C   4 W                                                                         
C   5 P                                                                         
C   6 T                                                                         
C   7 H                                                                         
C   8 EV W                                                                      
C   9 EV P                                                                      
C  10 EV T                                                                      
C  11 EV H                                                                      
      IENTRY = 0                                                                
      GO TO 1                                                                   
      ENTRY RTLTPP                                                              
      IENTRY = 1                                                                
    1 IRCD = IRCDB(11)                                                          
      NL = ID(IRCD+2)                                                           
      NS = ID(IRCD+3)                                                           
      JEV = ID(IRCD+8)                                                          
      W = D(IW+NL)                                                              
      P = D(IP+NS)                                                              
      T = D(IT+NS)                                                              
      H = D(IH+NS)                                                              
C*** ON FIRST PASS SET UP THE ERROR VARBLES                                     
      IF (JEV.EQ.0) GO TO 20                                                    
      IF (PASS.EQ.1) ID(IEVT+JEV) = 1                                           
      D(IEV+JEV) = D(IW+NL)-D(ID(IRCD+4))                                       
   20 JEV = ID(IRCD+9)                                                          
      IF (JEV.EQ.0) GO TO 30                                                    
      IF (PASS.EQ.1) ID(IEVT+JEV) = 2                                           
      D(IEV+JEV) = D(IP+NS)-D(ID(IRCD+5))                                       
   30 JEV = ID(IRCD+10)                                                         
      IF (JEV.EQ.0) GO TO 40                                                    
      IF (PASS.EQ.1) ID(IEVT+JEV) = 3                                           
      D(IEV+JEV) = D(IT+NS)-D(ID(IRCD+6))                                       
   40 JEV = ID(IRCD+11)                                                         
      IF (JEV.EQ.0) GO TO 99                                                    
      IF (PASS.EQ.1) ID(IEVT+JEV) = 4                                           
      D(IEV+JEV) = D(IH+NS)-D(ID(IRCD+7))                                       
   99 CONTINUE                                                                  
C$ LINES BELOW ARE NEW PER COSTELLO                                             
      IF(IENTRY.EQ.0) RETURN                                                    
C*** IF THE FLUID IS A REFRIGERANT, WRITE W,P,T,H                               
      WRITE(OUT,1000)W,P,T,H                                                    
 1000 FORMAT(' *** W = ',F8.3,  ' P = ',F8.3,   ' T = ',F8.3,                   
     *           ' H = ',F8.3)                                                  
      RETURN                                                                    
C     OTLTPP, RTLTPP                                                            
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF THE OUTLET FROM THE ECS                                   
C                                                                               
C$** ADDED  "RTLTPZ"     REFRIGERANT OUTLET AS ENTRY POINT                      
C**********************************************************************         
C                                                                               
      SUBROUTINE OTLTPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL), (SCR(2),NS), (SCR(3),IOP)                        
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION                                                                   
C   4 W                                                                         
C   5 P                                                                         
C   6 T                                                                         
C   7 H                                                                         
C   8 EV W                                                                      
C   9 EV P                                                                      
C  10 EV T                                                                      
C  11 EV H                                                                      
      ENTRY RTLTPZ                                                              
      I = IACDB(11)                                                             
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      CALL LEGRT(NL)                                                            
      NS = ISTAN(ICV(3))                                                        
      ID(I+3) = NS                                                              
      CALL START(NS)                                                            
      ID(I+4) = IPARM(ICV(5))                                                   
      ID(I+5) = IPARM(ICV(6))                                                   
      ID(I+6) = IPARM(ICV(7))                                                   
      ID(I+7) = IPARM(ICV(8))                                                   
      IF (ICV(4).EQ.0) GO TO 99                                                 
      IOP = ICV(4)/1000                                                         
      IF (IOP.NE.0) ID(I+8) = IAEV(1)                                           
      IOP = 100*(ICV(4)/100)                                                    
      IOP = IOP-1000*(IOP/1000)                                                 
      IF (IOP.NE.0) ID(I+9) = IAEV(2)                                           
      IOP = 10*(ICV(4)/10)                                                      
      IOP = IOP-100*(IOP/100)                                                   
      IF (IOP.NE.0) ID(I+10) = IAEV(3)                                          
      IOP = ICV(4)-10*(ICV(4)/10)                                               
      IF (IOP.NE.0) ID(I+11) = IAEV(4)                                          
   99 CONTINUE                                                                  
      RETURN                                                                    
C     OTLTPZ, RTLTPZ                                                            
      END                                                                       
C
C
C**********************************************************************         
C     SUBROUTINE VLVPP WITH ENTRY PREGPP IS NOT HERE.. IT'S IN ABVAPOR.FORT  
C**********************************************************************         
C
C
C
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      A PUMP                                                                   
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE PUMPPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30))     ,(SCR(1),C(151))                                         
     *, (ICPP,C(88)), (IFP,C(22)), (OUT,C(7)), (IGA,C(35))                      
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
     *, (SCR(4),DP), (SCR(5),NST), (SCR(6),FPWR), (SCR(7),SPWR)                 
     *, (SCR(8),EFFM), (SCR(9),QADD), (SCR(10),PR), (SCR(11),EFF)               
     *, (SCR(12),GAMMA), (SCR(13),TOUT), (SCR(14),HS), (SCR(15),TA)             
     *, (SCR(16),CP)                                                            
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 SHAFT NO                                                                  
C   6 PRESSURE RISE TABLE NO                                                    
C   7 STATIC EFFICIENCY TABLE NO                                                
C   8 MECH EFF                                                                  
      IE = 1                                                                    
      GO TO 3                                                                   
      ENTRY FANPP                                                               
      IE = 2                                                                    
    3 IRCD = IRCDB(8)                                                           
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      NST = ID(IRCD+5)                                                          
      D(IGA+41) = D(IW+NL)                                                      
      IF(IE.EQ.2) GO TO 1                                                       
C	 PUMPP 
      D(IGA+29) = D(IW+NL) * 7.4805  / DEN(NL,D(IP+NSI),D(IT+NSI))              
      D(IRCD-6) = D(IGA+29)   !IGA29=GPM                                                    
      DP = TLUP(ID(IRCD+6))                                                     
      D(IGA+31) = DP                                                            
      D(IP+NSO) = D(IP+NSI) + DP                                                
      FPWR = 4.3636E-3 * DP * D(IW+NL) / DEN(NL,D(IP+NSI),D(IT+NSI))            
      EFF = TLUP(ID(IRCD+7))                                                    
      SPWR = FPWR / EFF                                                         
      EFFM = D(ID(IRCD+8))                                                      
      QADD = (SPWR-FPWR) * 42.416                                               
      SPWR = SPWR/EFFM                                                          
      D(IGA+90+NST) = D(IGA+90+NST)-SPWR                                        
      CP = SHP(NL,D(IP+NSI),D(IT+NSI),D(IH+NSI))                                
      D(IT+NSO) = D(IT+NSI)+QADD/(D(IW+NL)*CP)                                  
      TA = 0.5*(D(IT+NSI)+D(IT+NSO))                                            
      CP = SHP(NL,D(IP+NSI),TA,D(IH+NSI))                                       
      D(IT+NSO) = D(IT+NSI)+QADD/(D(IW+NL)*CP)                                  
      GO TO 99
C      FANPP CONTINUES                                             
    1 D(IGA+28) = D(IW+NL) / DEN(NL,D(IP+NSI),D(IT+NSI))  !IGA28=CFM                      
      D(IRCD-6) = D(IGA+28)                                                     
      D(IP+NSO) = D(IP+NSI) + TLUP(ID(IRCD+6))                                  
      PR = D(IP+NSO) / D(IP+NSI)                                                
      EFF = TLUP(ID(IRCD+7))                                                    
      GAMMA = GAM(NL,D(IP+NSI),D(IT+NSI))                                       
      TOUT = D(IT+NSI) +  (D(IT+NSI)*PR**((GAMMA-1.0)/GAMMA) - D(IT+NSI)        
     *  )  / EFF                                                                
      EFFM = D(ID(IRCD+8))                                                      
      TA = 0.5*(D(IT+NSI)+D(IT+NSO))                                            
      CP = SHP(NL,D(IP+NSI),TA,D(IH+NSI))                                       
      SPWR = 0.02356*D(IW+NL)*CP*(TOUT-D(IT+NSI))/EFFM                          
      D(IGA+90+NST) = D(IGA+90+NST) - SPWR                                      
      D(IT+NSO) = TOUT                                                          
   99 CONTINUE
C      BOTH PUMPP AND FANPP CONTINUE                                                                     
      D(IH+NSO) = D(IH+NSI)                                                     
      IF (D(IH+NSI).NE.0.0) CALL TDB(NL,NSI,NSO,HS)                             
      D(IRCD-7) = EFF                                                           
      D(IRCD-8) = SPWR                                                          
      IF (IFP.NE.1.OR.ICPP.NE.0) GO TO 98                                       
      CALL PIOP(1,NL,NSI,NSO)                                                   
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HS) CALL HSOP(HS)                 
      CALL LINES(2)                                                             
      IF (IE.EQ.1) GO TO 4                                                      
      IF (D(IGA+28) .GT. 9999.) GO TO 222                                                      
      WRITE (OUT,1001) NST,D(IGA+28),EFF,EFFM,SPWR                                   
 1001 FORMAT(1H0,5X,6HSHAFT ,I4,3X,3HCFM,F7.2,3X,3HEFF,F7.4,
     * 3X,4HEFFM,F6.3,3X,7HHPSHAFT,F8.2)                                                                         
      GO TO 98
  222 WRITE (OUT,1222) NST,D(IGA+28),EFF,EFFM,SPWR                                   
 1222 FORMAT(1H0,5X,6HSHAFT ,I4,3X,3HCFM,F8.0,2X,3HEFF,F7.4,
     *3X,4HEFFM,F6.3,3X,7HHPSHAFT,F8.2)        
      GO TO 98
    4 CONTINUE
      IF (D(IGA+29) .GT. 9999.) GO TO 444                                                     
      WRITE (OUT,1002) NST,D(IGA+29),EFF,EFFM,SPWR                                   
 1002 FORMAT(1H0,5X,6HSHAFT ,I4,3X,3HGPM,F7.2,3X,3HEFF,F7.4,
     *3X,4HEFFM,F6.3,3X,7HHPSHAFT,F8.2)                                                                         
      GO TO 98      
  444 WRITE (OUT,1444) NST,D(IGA+29),EFF,EFFM,SPWR                                   
 1444 FORMAT(1H0,5X,6HSHAFT ,I4,3X,3HGPM,F8.0,2X,3HEFF,F7.4,
     *3X,4HEFFM,F6.3,3X,7HHPSHAFT,F8.2)                                                                         
   98 CONTINUE                                                                  
      RETURN                                                                    
C     PUMPPP,FANPP                                                              
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A PUMP                                                    
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE PUMPPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DIMENSION IFTA(2)                                                         
      DATA IFTA/001,010/                                                        
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 SHAFT NO                                                                  
C   6 PRESSURE RISE TABLE NO                                                    
C   7 STATIC EFFICIENCY TABLE NO                                                
C   8 MECH EFF                                                                  
      IE = 1                                                                    
      GO TO 4                                                                   
      ENTRY FANPZ                                                               
      IE = 2                                                                    
    4 I = IACDB(8)                                                              
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
      CALL FTL(NL,IFTA(IE))                                                     
      ID(I+5) = ICV(5)                                                          
      CALL SRT(ICV(5))                                                          
      ID(I+6) = ITIDN(ICV(6),16)                                                
      ID(I+7) = ITIDN(ICV(6),17)                                                
      ID(I+8) = IPARM(ICV(7))                                                   
      RETURN                                                                    
C     PUMPPZ,FANPZ                                                              
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      A PUMP                                                                   
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE PUMPSP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151)), (IW,C(27))         
     *, (IP,C(28)), (IT,C(29)), (WTC,C(102)), (CUC,C(104)), (RIC,C(106))        
     *, (DRC,C(108)),(WTIC,C(109)), (WTDC,C(111)), (IGA,C(35))                  
     *, (HPE,C(112)), (IH,C(30))                                                
      DIMENSION SCR(30)                                                         
      EQUIVALENCE         (SCR( 1),NL    ), (SCR( 2),NSI   )                    
     *, (SCR( 3),NSO   ), (SCR( 4),NST   ), (SCR( 5),DPR   )                    
     *, (SCR( 6),WTP   ), (SCR( 7),WTR   ), (SCR( 8),WTL   )                    
     *, (SCR( 9),VOL   ), (SCR(10),EP    ), (SCR(11),EM    )                    
     *, (SCR(12),HPP   ), (SCR(13),IPT   ), (SCR(14),DENO  )                    
     *, (SCR(15),DENI  ), (SCR(16),DP    ), (SCR(17),DV    )                    
     *, (SCR(18),WTF   ), (SCR(19),CUF   ), (SCR(20),RI    )                    
     *, (SCR(21),DRF   ), (SCR(22),VF    ), (SCR(23),N     )                    
     *, (SCR(24),HPM   ), (SCR(25),FHP   ), (SCR(26),IND   )                    
     *, (SCR(27),WTPK  )                                                        
      INTEGER OUT                                                               
      REAL N                                                                    
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DIMENSION TABE(4), TABN(4)                                                
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
C  10 VL                                                                        
C  11 IPT                                                                       
      IRCD = IRCDB(11)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      NST = ID(IRCD+5)                                                          
      DENI = DEN(NL,D(IP+NSI),D(IT+NSI))                                        
      DENO = DEN(NL,D(IP+NSO),D(IT+NSO))                                        
      N = D(IGA+80+NST)                                                         
      DPR = 1728.0*D(IW+NL)/(DENO*N)                                            
      IPT = ID(IRCD+11)                                                         
      IF (IPT.EQ.2) GO TO 10                                                    
      IF (DPR.LE.0.028) GO TO 5                                                 
      WTP = 10.63*DPR**.6666667                                                 
      GO TO 15                                                                  
   10 IF (DPR.LE.0.088) GO TO 5                                                 
      WTP = 22.92*DPR**.6666667                                                 
      GO TO 15                                                                  
    5 WTP = 2.1                                                                 
   15 VF = D(ID(IRCD+10))*DENI*(1.0/DEN(NL,D(IP+NSI),710.0)                     
     * -1.0/DEN(NL,D(IP+NSI),395.0))                                            
      WTR = 0.131*VF**.6666667                                                  
      WTL = 0.55+0.9575*D(IW+NL)/DENO                                           
      WTPK = WTP+WTR+WTL                                                        
      WTF = D(ID(IRCD+6))                                                       
      IF (WTF.NE.0.0) WTPK = WTPK*WTF                                           
      WTC = 1.125*WTPK+D(ID(IRCD+10))*DENI/1728.0                               
      VOL = 33.04*WTPK                                                          
      CUC = 81.2+1.05*WTPK                                                      
      CUF = D(ID(IRCD+7))                                                       
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      RIC = 0.04016                                                             
      RI = D(ID(IRCD+8))                                                        
      IF (RI.NE.0.0) RIC = RI                                                   
      DRC = 1.0                                                                 
      DRF = D(ID(IRCD+9))                                                       
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      WTIC = 0.205*WTPK                                                         
      WTDC = SQRT(0.0361*WTP**2+0.0551*WTR**2+0.0441*WTL**2)                    
      DP = D(IP+NSO)-D(IP+NSI)                                                  
      FHP = 4.3636E-3*DP*D(IW+NL)/DENI                                          
      HPP = D(IW+NL) * SHP(NL,D(IP+NSI),D(IT+NSI),D(IH+NSI)) *                  
     * (D(IT+NSO) - D(IT+NSI)) / 42.416 + FHP                                   
      EP = FHP/HPP                                                              
c@      CALL MDSCT(2,IND,EM,TABE,N,TABN,4,1,DV,DV,DV)                             
      CALL MDSCT(2,IND,EM,TABE,N,TABN,4,1,DV,DV,IDV,Idummy)                             
      EM = EM*(1.0-0.281/(HPP**0.169))                                          
      HPM = HPP/EM                                                              
      HPE = HPE+HPM                                                             
      CALL SSA                                                                  
      CALL SCO                                                                  
      CALL SCI(NL,NSI,NSO)                                                      
      CALL LINES(2)                                                             
      WRITE (OUT,1000) WTP,WTR,WTL,VOL,N,EP,HPP                                 
 1000 FORMAT(1H0,5X,3HWTP,F7.2,3X,3HWTR,F7.2,3X,3HWTL,F7.2,3X,3HVOL,F7.0        
     *,1HN,F9.0,3X,3HEFF,F7.4,3X,2HHP,F8.2)                                     
      CALL LINES(2)                                                             
      WRITE (OUT,1001) EM,HPM                                                   
 1001 FORMAT(1H0,5X,10HDRIVE  2/3,3X,3HEFF,F7.4,3X,2HHP,F8.2)                   
      D(IGA+71) = VOL                                                           
      RETURN                                                                    
C     PUMPSP                                                                    
      END                                                                       
*DECK,PUMPSZ                                                                    
      SUBROUTINE PUMPSZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
     *, (SCR(1),C(151)), (IGA,C(35))                                            
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL)                                                   
      INTEGER OUT,CERR                                                          
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA /001/                                                           
C   1 COMP CODE                                                                 
C   2 NL                                                                        
C   3 NSI                                                                       
C   4 NSO                                                                       
C   5 NST                                                                       
C   6 WTM                                                                       
C   7 CUM                                                                       
C   8 RI                                                                        
C   9 DRM                                                                       
C  10 VL                                                                        
C  11 IPT                                                                       
      I = IACDB(11)                                                             
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
      ID(I+11) = ICV(11)                                                        
      IF (ICV(11).GT.0 .AND. ICV(11).LE.2) GO TO 10                             
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1010) CERR,ICV(11)                                             
 1010 FORMAT(6H0ERROR,I6,5X,17HINVALID PUMP TYPE,I6)                            
   10 IF (D(IGA+80+ICV(5)).NE.0.0) GO TO 99                                     
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR                                                     
 1000 FORMAT(6H0ERROR,I6,5X,25HSHAFT SPEED NOT SPECIFIED)                       
   99 CONTINUE                                                                  
      RETURN                                                                    
C     PUMPSZ                                                                    
      END                                                                       
      SUBROUTINE QBLRPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)),(IW,C(27)),(IP,C(28)),(IT,C(29))                 
     *,(IH,C(30)),(SCR(1),C(151)),(GC,C(370)),(JC,C(371))                       
     *,(ICPP,C(88)),(IFP,C(22)),(OUT,C(7)),(IGA,C(35))                          
     *,(PASS,C(17)),(IEVT,C(48)),(IEV,C(49)),(ITAD,C(54))                       
     *,(IFB,C(55)),(ISN,C(38))                                                  
      DIMENSION SCR(30)                                                         
      INTEGER OUT,PASS                                                          
      REAL L,MU,K,JC,ME,MI,MP                                                   
      EQUIVALENCE  (SCR(1),NL),(SCR(2),NSI),(SCR(3),NSO)                        
     *,(SCR(4),IOP),(SCR(5),FFT),(SCR(6),COT),(SCR(7),AF)                       
     *,(SCR(8),Q),(SCR(9),L),(SCR(10),AH),(SCR(11),DH)                          
      DIMENSION MU(2),CPY(2),V(2),PR(2),T(5),TW(5),HT(5),DP(2)                  
     *,K(2)                                                                     
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
     *,(D(51),T(1)),(D(56),TW(1)),(D(61),HT(1))                                 
     *,(D(71),HFG),(D(72),ST),(D(73),PT)                                        
     *,(D(75),TIN),(D(76),W),(D(77),IRET),(D(78),CO)                            
     *,(D(79),FF),(D(81),VR),(D(82),DV)                                         
     *,(D(83),SL),(D(84),HSAT),(D(86),TSAT)                                     
     *,(D(87),X1),(D(88),X2),(D(89),TNB),(D(90),DD)                             
     *,(D(91),FD),(D(92),B),(D(93),EM),(D(94),RS)                               
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 OPTION NO                                                                 
C   6 FRICTION FACTOR TABLE                                                     
C   7 COLBURN FACTOR TABLE                                                      
C   8 HEAT EXCHANGER FLOW AREA                                                  
C   9 HEAT INPUT RATE IN (BTU/MIN)                                              
C  10 HEAT EXCHANGER FLOW PATH LENGTH (FEET)                                    
C  11 HT EXCHG/HT TRANSFER AREA INCLUDING FINS (SQUARE FEET)                    
C  12 HEAT EXCHANGER HYDRAULIC DIAMETER                                         
C  13 ERROR VARIABLE OPTION                                                     
C*** ERROR-VARIABLE OPTION: 0 IF NO ERROR VARIABLE; 1 IF SUPERHEAT              
C  14 ERROR VARIABLE INDEX                                                      
C  15 MAXIMUM BOILER WALL TEMPERATURE                                           
C*** SET UP INTERNAL VARIABLES IN TERMS OF STORED ARRAY                         
      IRCD = IRCDB(15)                                                          
      NL=ID(IRCD+2)                                                             
      NSI=ID(IRCD+3)                                                            
      NSO=ID(IRCD+4)                                                            
      IOP=ID(IRCD+5)                                                            
      IF (ID(IRCD+13).EQ.0) GO TO 107                                           
      TMAX=D(ID(IRCD+15))                                                       
      JEVI=ID(IRCD+14)                                                          
  107 CONTINUE                                                                  
      LOCT=ID(IFB+NL)                                                           
      LOCT=ID(LOCT+3)                                                           
      AF=D(ID(IRCD+8))                                                          
      Q=D(ID(IRCD+9))                                                           
      L=D(ID(IRCD+10))                                                          
      AH=D(ID(IRCD+11))                                                         
      DH=D(ID(IRCD+12))                                                         
C*** COMPUTE THE SATURATION PROPERTIES                                          
      TSAT=VTS(LOCT,D(IP+NSI))                                                  
C*** COMPUTE THE TRANSPORT PROPERTIES BASED ON INLET CONDITIONS                 
C$ MOVE "TIN" DEFINITION UP AND USE "TIN" (WAS "TSAT") FOR FLUID PROPS          
C$      LEAVE TSAT FOR VAPOR PROPERTIES                                         
      TIN=D(IT+NSI)                                                             
      CPY(1)=VCPF(LOCT,D(IP+NSI),TIN)                                           
      CPY(2)=VCPV(LOCT,D(IP+NSI),TSAT)                                          
      MU(1)=VVISCF(LOCT,D(IP+NSI),TIN)                                          
      MU(2)=VVISCV(LOCT,D(IP+NSI),TSAT)                                         
      K(1)=VCONDF(LOCT,D(IP+NSI),TIN)                                           
      K(2)=VCONDV(LOCT,D(IP+NSI),TSAT)                                          
      V(1)=1.0/VDL(LOCT,TIN)                                                    
      V(2)=VSV('QBLRPP 1',LOCT,D(IP+NSI),TSAT)                                  
      HFG=VHFG(LOCT,D(IP+NSI),TSAT,V(2))                                        
      ST=VST(LOCT,TSAT)                                                         
      PT=VDPDT(LOCT,D(IP+NSI))                                                  
      W=D(IW+NL)                                                                
C$      ADD TO INITIALIZE (WAS SOMETIMES BEING PASSED                           
C$                                          FROM PREVIOUS COMPONENT)            
      XE=0.0                                                                    
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9010                                              
      VF=(W/60.) / (1./V(1)*AF)                                                 
      WRITE (OUT,9911)                                                          
 9911 FORMAT(' ',/)                                                             
      WRITE (OUT,9011)                                                          
 9011 FORMAT(' *********WRITE 9011 FROM "QBLRPP" *********************'         
     *      ,'**************')                                                  
      WRITE (OUT,9012) AF    , Q     , L     , AH                               
     *               , DH    , TSAT  , CPY(1), CPY(2)                           
     *               , MU(1) , MU(2) , K(1)  , K(2)                             
     *               , V(1)  , V(2)  , HFG   , ST                               
     *               , PT    , TIN   , W     , D(IP+NSI)                        
     *               , D(IH+NSI),VF                                             
 9012 FORMAT(' AFLOW =', E12.5,  ' QRATE =', E12.5,  ' LFLOW =', E12.5          
     *      ,' AHTRAN=', E12.5,/,' DHYD  =', E12.5,  ' TSAT  =', E12.5          
     *      ,' CPY(1)=', E12.5,  ' CPY(2)=', E12.5,/,' MU(1) =', E12.5          
     *      ,' MU(2) =', E12.5,  ' K(1)  =', E12.5,  ' K(2)  =', E12.5          
     *   ,/ ,' V(1)  =', E12.5,  ' V(2)  =', E12.5,  ' HFG V2=', E12.5          
     *      ,' ST    =', E12.5,/,' PTDPDT=', E12.5,  ' TIN   =', E12.5          
     *      ,' W     =', E12.5,  ' P IN  =', E12.5,/,' H IN  =', E12.5          
     *      ,' VF F/S=', E12.5)                                                 
 9010 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      DO 1 I = 1,2                                                              
    1 PR(I)=3600.*CPY(I)*MU(I)/K(I)                                             
      IRET=-1                                                                   
C*** COMPUTE THE REYNOLDS NUMBER AS THE INDEPENDENT VARIABLE                    
      G=W/AF/60.0                                                               
C*** BASED ON 100% LIQUID FLOW                                                  
      D(IGA+21)=G*DH/MU(1)                                                      
C*** USE TULP FOR COLBURN FACTOR, THEN COMPUTE HEAT TRANSFER COEFF.             
      CO=TLUP(ID(IRCD+7))                                                       
      HT(1)=CO*60.*G*CPY(1)/PR(1)**.67                                          
C*** USE TULP FOR FRICTION FACTOR                                               
      FF=TLUP(ID(IRCD+6))                                                       
C*** COMPUTE VOLUME RATIO AND SLIP FOR LATER TWO-PHASE COMPUTATIONS             
      VR=V(2)/V(1)                                                              
      DV=1.0-1.0/VR                                                             
      SL=SQRT(SQRT(VR-1.))                                                      
C*** COMPUTE PRESSURE DROP AS IF 100% LIQUID FLOW                               
      DP(1)=4.*FF*L*G*G*V(1)/(2.*DH*GC)                                         
C*** REPEAT THE ABOVE COMPUTATIONS AS IF 100% VAPOR FLOW                        
      D(IGA+21)=G*DH/MU(2)                                                      
      CO=TLUP(ID(IRCD+7))                                                       
      HT(5)=CO*60.*G*CPY(2)/PR(2)**.67                                          
      FF=TLUP(ID(IRCD+6))                                                       
      DP(2)=4.*FF*L*G*G*V(2)/(2.*DH*GC)                                         
C*** DETERMINE WHICH FLUID-PHASE IS FLOWING AT THE INLET                        
      HSAT=VH(LOCT,D(IP+NSI),TSAT,V(2))-HFG                                     
      TIN=D(IT+NSI)                                                             
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9020                                              
      REF=G*DH/MU(1)                                                            
      D(IGA+21)=REF                                                             
      COLBF=TLUP(ID(IRCD+7))                                                    
      FFF=TLUP(ID(IRCD+6))                                                      
      REV=G*DH/MU(2)                                                            
      D(IGA+21)=REV                                                             
      COLBV=TLUP(ID(IRCD+7))                                                    
      FFV=TLUP(ID(IRCD+6))                                                      
      VHPTV2=VH(LOCT,D(IP+NSI),TSAT,V(2))                                       
      QPSIF=DP(1)*DH/(4.*FFF*L)/144.                                            
      QPSIV=DP(2)*DH/(4.*FFV*L)/144.                                            
      WRITE (OUT,9021)                                                          
 9021 FORMAT(' WRITE 9021 FROM "QBLRPP"    ')                                   
      WRITE (OUT,9022) PR(1) , PR(2) , G     , REF                              
     *               , COLBF , HT(1) , FFF   , VR                               
     *               , DV    , SL    , DP(1) , REV                              
     *               , COLBV , HT(5) , FFV   , DP(2)                            
     *               , HSAT  , TIN   , VHPTV2,QPSIF                             
     *               , QPSIV                                                    
 9022 FORMAT(' PR(1) =', E12.5,  ' PR(2) =', E12.5,  ' G     =', E12.5          
     *      ,' REF   =', E12.5,/,' COLBF =', E12.5,  ' HT(1) =', E12.5          
     *      ,' FFF   =', E12.5,  ' VR    =', E12.5,/,' DV    =', E12.5          
     *      ,' SL    =', E12.5,  ' DP(1) =', E12.5,  ' REV   =', E12.5          
     *   ,/ ,' COLBV =', E12.5,  ' HT(5) =', E12.5,  ' FFV   =', E12.5          
     *      ,' DP(2) =', E12.5,/,' HSAT L=', E12.5,  ' TIN   =', E12.5          
     *      ,' VHPTV2=', E12.5,  ' QPSIF =', E12.5,/,' QPSIV =', E12.5)         
 9020 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C*** IN LIQUID-ONLY REGION AT INLET OR NOT?                                     
      IF(D(IH+NSI)-HSAT)360,332,332                                             
C*** IN TWO-PHASE REGION AT INLET OR ALL VAPOR?                                 
  332 IF(D(IH+NSI)-HSAT-HFG)333,1272,1272                                       
C*** IN TWO-PHASE REGION AT INLET                                               
  333 XI=(D(IH+NSI)-HSAT)/HFG                                                   
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9023                                              
C         APPROX FOR Q HERE IS GOOD ONLY FOR TWO-PHASE                          
      XILIM1=AMIN1(1.,XI)                                                       
      QVAP=1/64.4 * V(2) * (XILIM1*G)**2/144.                                   
      WRITE (OUT,9024)                                                          
 9024 FORMAT(' WRITE 9024 FROM "QBLRPP" ... TWO PHASE AT INLET   ')             
      WRITE (OUT,9025) XI    , QVAP                                             
 9025 FORMAT(' XI    =', E12.5,  ' QVAP  =', E12.5)                             
 9023 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      TIN=TSAT                                                                  
      X1=0.0                                                                    
      X2=0.0                                                                    
      GO TO 750                                                                 
C*** IN LIQUID-ONLY REGION AT INLET                                             
  360 XI=0.0                                                                    
C$$       (OLD CODE FAILED TO CONSIDER THE POSSIBILITY OF ALL-LIQUID            
C$$        FORCED CONVECTION HEATING UP TO TSAT AND THEN SATURATED              
C$$        BOILING STARTING THERE.  IF THERE WAS NO NUCLEATE (SUBCOOLED)        
C$$        BOILING, IT ASSUMED THERE WAS NO TWO-PHASE FLOW AT ALL.)             
                                                                                
C*** COMPUTE FLUID TEMPERATURE (T) AND WALL TEMPERATURE (TW) AT OUTLET          
C***   ASSUMING LIQUID-ONLY FORCED CONVECTION HEATING ALL THE WAY               
      T5CNV=TIN+Q/(W*CPY(1))                                                    
      DT5CNV=Q/(AH*HT(1))                                                       
      TW5CNV=T5CNV+DT5CNV                                                       
C$ THIS COMMENT USED TO SAY SEE IF FLUID REACHED THE "TNB" BEFORE EXIT.         
C$                                 ^^^^^                                        
C$    SINCE TNB IS GREATER THAN TSAT (OBVIOUS FROM EQUATION), HOW CAN           
C$    TFLUID BE GREATER THAN TNB (AND ALSO TSAT) AND STILL BE SUBCOOLED?        
C$    OBVIOUSLY THEY MEANT "TWALL", NOT "TFLUID", BUT THAT WASN'T WHAT          
C$    WAS CODED TO COMPUTE "X1".                                                
C$                                                                              
C*** DETERMINE TEMPERATURE AT WHICH NUCLEATE BOILING BEGINS (TNB)               
C    AND DETERMINE IF WALL REACHES THE TEMPERATURE BEFORE OUTLET                
      TW1CNV=TIN+DT5CNV                                                         
      IF (Q) 420,425,425                                                        
  420 TNB=9999.                                                                 
      GO TO 428                                                                 
  425 TNB=TSAT+SQRT(8.*60.*ST*Q/(K(1)*AH*PT))                                   
  428 CONTINUE                                                                  
C$WAS X1=(TNB-TIN)*W*CPY(1)*L/Q-W*CPY(1)*L/(HT(1)*AH)                           
C*** FIND X AT WHICH NUCLEATE BOILING WOULD BEGIN                               
      X1NUC=(1. - (TW5CNV-TNB)/(TW5CNV-TW1CNV))  * L                            
      IF (Q.LT.0.) X1NUC=999999.                                                
C*** FIND X AT WHICH BULK BOILING WOULD BEGIN (TSAT IS REACHED)                 
C***   IF FORCED-CONVECTION HEATED ALL THE WAY.                                 
      X1SAT=(TSAT-TIN)/(T5CNV-TIN)    * L                                       
C     WRITE(OUT,*) T(1),TW(1),TNB,X1                                            
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9030                                              
      WRITE (OUT,9031)                                                          
 9031 FORMAT(' WRITE 9031 FROM "QBLRPP"    ')                                   
      WRITE (OUT,9032) T5CNV , TW1CNV, TNB     , X1                             
     *                ,X1NUC , X1SAT , DT5CNV  , TW5CNV                         
 9032 FORMAT(' T5CNV =', E12.5,  ' TW1CNV=', E12.5,  ' TNB   =', E12.5          
     *      ,' X1    =', E12.5,/,' X1NUC =', E12.5,  ' X1SAT =', E12.5          
     *      ,' DT5CNV=', E12.5,  ' TW5CNV=', E12.5)                             
 9030 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF(TNB-TWALIN)430,450,450                                                 
C*** NUCLEATE BOILING BEGINS IMMEDIATELY AT THE INLET                           
  430 X1=0.0                                                                    
      GO TO 485                                                                 
C*** NO NUKE BOILING BEFORE TSAT REACHED (455);OR NOT                           
  450 IF(X1SAT-X1NUC)455,455,451                                                
C*** X1NUC IS LESS THAN X1SAT, BUT IS L LESS THAN X1NUC?                        
  451 IF(L-X1NUC)455,480,480                                                    
C*** NO NUCLEATE BOILING, BUT IS LIQUID EXIT TEMP BELOW TSAT OR ABOVE?          
C$ WAS 455 IF(T1CNV-TSAT)460,460,477  (IN INTERMEDIATE VERSION)                 
  455 IF(T5CNV-TSAT)460,460,477                                                 
C*** TEXIT BELOW TSAT, THERE IS NO BOILING WITHIN THE BOILER                    
  460 X1=L                                                                      
      X2=0.                                                                     
      X3=0.                                                                     
      XE=0.                                                                     
      T(5)=T5CNV                                                                
      TW(5)=TW5CNV                                                              
      GO TO 1310                                                                
C*** FORCED CONV. HEATS TEXIT TO ABOVE TSAT; THEN BULK BOIL W/O NUC BOIL        
  477 X1=X1SAT                                                                  
      X2=0.                                                                     
      T(1)=TSAT                                                                 
      TW(1)=TSAT+DT5CNV                                                         
      T(2)=TSAT                                                                 
      TW(2)=TW(1)                                                               
      GO TO 750                                                                 
C*** THERE IS FORCED-CONVECTION HEATING FOLLOWED BY NUCLEATE BOILING            
  480 X1=X1NUC                                                                  
      T(1)=TIN+Q*X1/(L*W*CPY(1))                                                
      TW(1)=T(1)+Q/(HT(1)*AH)                                                   
C*** COMPUTE THE LENGTH OF THE NUCLEATE-BOILING REGION                          
C***       ALSO ENTRY FOR NUCLEATE BOILING IMMED AT INLET                       
C$   WAS 485 X2=XSAT-X1                                                         
  485 X2=X1SAT-X1                                                               
      IF(X2.GE.(L-X1))X2=L-X1                                                   
C*** COMPUTE THE BUBBLE DEPARTURE DIAMETER (DD) IN FEET                         
  490 DD=4.65E-4*SQRT(GC/GC*ST*V(1)/DV)*(VR*CPY(1)*TSAT/HFG)                    
     ***1.25                                                                    
C*** COMPUTE THE FREQUENCY OF DEPARTURE OF BUBBLES, 1/SEC.                      
      FD=.6*SQRT(SQRT(ST*GC*GC*V(1)*DV))/DD                                     
C*** ASSUME THE SURFACE FACTORS: B(SURFACE COFFICIENT); EM (EXPONENT            
C    RELATING THE NUMBER OF NUCLEATION SITES GO THE TEMPERATURE                 
C    DIFFERENCE); AND RS(RADIUS, FT., OF SMALLEST SITE)                         
      B=400.                                                                    
      EM=2.0                                                                    
      RS=0.0001                                                                 
C*** COMPUTE THE NUMBER OF NUCLEATION SITES PER SQ. FT.                         
C*** JC=778.28 PER LINE 265 OF FORT(FTB1S)                                      
      XN=B*(JC*RS*HFG*(TNB-TSAT)/(2.*TSAT*ST*V(2)))**EM                         
C*** COMPUTE THE POOL BOILING HEAT-TRANSFER COEFFICIENT                         
      HP=3.5*(K(1)/DD/60.)*XN*DD*DD                                             
      HP=HP*SQRT(PR(1))*SQRT(FD*DD*DD/(V(1)*MU(1)))                             
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9040                                              
      WRITE (OUT,9041)                                                          
 9041 FORMAT(' WRITE 9041 FROM "QBLRPP"    ')                                   
      WRITE (OUT,9042) X1    , T(1)  , TW(1) , X2                               
     *               , DD    , FD    , B     , EM                               
     *               , RS    , XN    , HP                                       
 9042 FORMAT(' X1    =', E12.5,  ' T(1)  =', E12.5,  ' TW(1) =', E12.5          
     *      ,' X2    =', E12.5,/,' DD    =', E12.5,  ' FD    =', E12.5          
     *      ,' B     =', E12.5,  ' EM    =', E12.5,/,' RS    =', E12.5          
     *      ,' XN    =', E12.5,/,' HP    =', E12.5)                             
 9040 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF(IRET)500,500,980                                                       
C*** COMPUTE THE COMBINED CONVECTION/BOILING COEFFICIENT                        
  500 HT(2)=(HP*HP+HT(1)*HT(1))/(2.*HP)                                         
C*** ITERATIVELY COMPUTE THE WALL TEMPERATURE IN THE NUCLEATE REGION            
      A1=HP/(TNB-TSAT)**2                                                       
      A0=2.0*(Q/(A1*AH))**2                                                     
      A1=4.0*((HT(1)/A1)**2)/3.0                                                
      S0=SQRT(A0*A0-A1**3)                                                      
C$WAS XX=ABS((A0-S0)/(A0+S0))                                                   
C$WAS IF(XX-.0001)650,670,670                                                   
C$     SIMPLIFY AND MAKE SURE NO NEGATIVE # IS RAISED TO A POWER                
      IF(A0-S0)650,670,670                                                      
  650 U=(A0+S0)**(1./3.)                                                        
      GO TO 680                                                                 
  670 U=(A0+S0)**(1./3.)+(A0-S0)**(1./3.)                                       
  680 TC=(SQRT(U)+SQRT(U-4.*(U/2.-SQRT((U/2.)**2-.75*A1))))/2.                  
      T(2)=TSAT                                                                 
      TW(2)=TSAT+TC                                                             
      TW(5)=TW(2)                                                               
      X3=0.0                                                                    
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9050                                              
      WRITE (OUT,9051)                                                          
 9051 FORMAT(' WRITE 9051 FROM "QBLRPP"    ')                                   
      WRITE (OUT,9052) IRET  , HT(2) , A0    , A1                               
     *                ,S0    , XX    , U     , TC                               
     *                ,T(2)  , TW(2) , TW(5) , XE                               
 9052 FORMAT(' IRET  =', I4,8X,  ' HT(2) =', E12.5,  ' A0    =', E12.5          
     *      ,' A1    =', E12.5,/,' S0    =', E12.5,  ' XX    =', E12.5          
     *      ,' U     =', E12.5,  ' TC    =', E12.5,/,' T(2)  =', E12.5          
     *      ,' TW(2) =', E12.5,  ' TW(5) =', E12.5,  ' XE CHK=', E12.5)         
 9050 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C*** GO TO 1280 IF THE EXIT IS 100% LIQUID (NO NET BOILING)                     
C$ WAS   IF(X2.GE.(L-X1))GO TO 1280                                             
      IF(X2.GE.(L-X1-.0001))GO TO 1280                                          
C*** STARTING REGION 3: BULK BOILING                                            
  750 XC=1.0                                                                    
C*** COMPUTE THE DRYOUT POSITION                                                
      X3=W*(CPY(1)*(TSAT-TIN)+HFG*(1.0-XI))*L/Q-X1-X2                           
C*** COMPUTE THE EXIT QUALITY                                                   
      XE=(Q/W-CPY(1)*(TSAT-TIN))/HFG+XI                                         
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9053                                              
      WRITE (OUT,9054)                                                          
 9054 FORMAT(' WRITE 9054 BULK BOILING-DRYOUT POSITION  FROM "QBLRPP"')         
      WRITE (OUT,9055) X3    , XE                                               
 9055 FORMAT(' X3DRYO=', E12.5,  ' XE    =', E12.5)                             
 9053 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF(XE-1.)830,820,820                                                      
C*** FILM-BOILING OR VAPOR-ONLY HEATING OCCUR                                   
  820 XE=1.0                                                                    
      GO TO 1140                                                                
C*** COMPUTE THE CRITICAL EXIT QUALITY ABOVE WHICH FILM BOILING OCCURS          
C$WAS COMPUTE THE CRITICAL EXIT QUALITY ABOVE WHICH VAPORIZATION OCCURS         
  830 XC=JC*HFG*(XE-XI)/L                                                       
C$            NEED TO USE "REF" HERE AS ON P 97 COSTELLO                        
C$            CODE USED OLD "REV" VALUE... LINE BELOW SHOULD FIX                
      D(IGA+21)=G*DH/MU(1)                                                      
      XC=1.04-1.14E-5*(D(IGA+21)*D(IGA+21)*XC)**.375                            
C*** COMPUTE LENGTH OF THE BULK-BOILING REGION (X3)                             
      X3=X3*(XC-XI)/(1.0-XI)                                                    
      IF (X3.LT.0.0) X3=0.0                                                     
      IF(X3.GT.(L-X1-X2))X3=L-X1-X2                                             
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9060                                              
      WRITE (OUT,9061)                                                          
 9061 FORMAT(' WRITE 9061  FROM "QBLRPP"    ')                                  
      WRITE (OUT,9062) X3    , XE    , XC                                       
 9062 FORMAT(' X3    =', E12.5,  ' XE    =', E12.5,  ' XC    =', E12.5)         
 9060 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF(XE.GE.XC)GO TO 1140                                                    
C*** COMPUTE THE TWO-PHASE PARAMETER                                            
      TT=(1.0/XE-1.0)**0.9*SQRT(DP(1)/DP(2))                                    
      X3=L-X1-X2                                                                
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9070                                              
      WRITE (OUT,9071)                                                          
 9071 FORMAT(' WRITE 9071    XE LESS THAN XC    FROM "QBLRPP"    ')             
      WRITE (OUT,9072) TT    , X3                                               
 9072 FORMAT(' TT    =', E12.5,  ' X3    =', E12.5)                             
 9070 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C*** GO TO 1090 IF XE AND TT INDICATE THE RANGE FOR ELLERBROOK'S FORMULA        
      IF(XE.GT.0.7.OR.TT.LT.0.01)GO TO 1090                                     
C*** USE CHEN'S FORMULA                                                         
      F=1.0+1.88/TT**0.8                                                        
      Y=F*(G*DH/MU(1)*(1.0-XE))**0.8/10000.                                     
      S=EXP(-Y)                                                                 
C*** COMPUTE TNB AS A FIRST APPROXIMATION TO THE WALL TEMPERATURE               
C    THEN GO TO 490 TO COMPUTE THE POOL-BOILING HEAT-TRANSFER COEFF.            
      TNB=TSAT+SQRT(.8*60.*ST*Q/(K(1)*AH*PT))                                   
      IRET=1                                                                    
      GO TO 490                                                                 
C*** COMPUTE THE BULK-BOILING COEFFICIENT WITH CHEN'S FACTORS                   
  980 HT(3)=F*HT(1)+S*HP                                                        
C*** COMPUTE THE WALL TEMPERATURE AT THE END OF THE TWO-PHASE REGION            
C    UNDER THE ASSUMPTION THAT EM IS APPROXIMATELY 2.0                          
      A1=HP/(TNB-TSAT)**2                                                       
      A0=Q/(AH*S*A1*2.0)                                                        
      A1=F*HT(1)/(3.0*S*A1)                                                     
      S0=SQRT(A0*A0+A1**3)                                                      
      T(3)=TSAT                                                                 
      TW(3)=TSAT+(A0+S0)**(1./3.)-(S0-A0)**(1./3.)                              
      TW(5)=TW(3)                                                               
      T(5)=TSAT                                                                 
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9080                                              
      WRITE (OUT,9081)                                                          
 9081 FORMAT(' WRITE 9081     CHEN''S FORMULA       FROM "QBLRPP"    ')         
      WRITE (OUT,9082) F     , Y     , S     , TNB                              
     *                ,IRET  , A0    , A1    , S0                               
     *                ,T(3)  , TW(3) , TW(5) , T(5)                             
     *                ,HT(3)                                                    
 9082 FORMAT(' F     =', E12.5,  ' Y     =', E12.5,  ' S     =', E12.5          
     *      ,' TNB   =', E12.5,/,' IRET  =', I4,8X,  ' A0    =', E12.5          
     *      ,' A1    =', E12.5,  ' S0    =', E12.5,/,' T(3)  =', E12.5          
     *      ,' TW(3) =', E12.5,  ' TW(5) =', E12.5,  ' T(5)  =', E12.5,/        
     *      ,' HT(3) =', E12.5)                                                 
 9080 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C*** GO TO 1320 TO COMPUTE THE TWO-PHASE PRESSURE DROP                          
      GO TO 1320                                                                
C$     THE ELLERBROOK FORMULA GIVES AN H LOWER THAN FOR FILM BOILING            
C$     AND LOWER THAN "HT(5)" (ALL VAPOR).  IS SOMETHING WRONG OR IS            
C$     ELLERBROOK JUST A DAMN FLAKE??                                           
C*** ELLERBROOK'S FORMULA FOR THE HEAT-TRANSFER COEFFICIENT                     
 1090 Y=2.35-ALOG(TT)*(.266+.0255*ALOG(TT))                                     
      HT(3)=HT(5)*(Q/(AH*60.*G*HFG))**.4*EXP(Y)                                 
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9090                                              
      WRITE (OUT,9091)                                                          
 9091 FORMAT(' WRITE 9091   ELLERBROOK''S HT(3) FORMULA FROM "QBLRPP"')         
      WRITE (OUT,9092) Y     , HT(3)                                            
 9092 FORMAT(' Y     =', E12.5,  ' HT(3) =', E12.5)                             
 9090 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      GO TO 1180                                                                
C*** XE>XCRIT OR XE>1.  SO FILM BOILING OR VAPOR-ONLY HEATING OCCURS            
C$WAS SO FILM BOILING OR VAPOR-ONLY HEATING OCCURS                              
 1140 HT(3)=HT(5)*(XE+SL/VR*(1.0-XE))**0.8                                      
C*** FLUID AND WALL TEMPERATURES AT THE END OF THE TWO-PHASE REGION             
C$WAS 1180 T(3)=TSAT+(Q*(X3+X2+X1)/W/L-CPY(1)*(TSAT-TIN)-HFG*(XE-XI))           
C$         /CPY(2)                                                              
C$                                                                              
C$   CERTAINLY WHILE TWO-PHASE, T=TSAT!!!                                       
 1180 T(3)=TSAT                                                                 
      TW(3)=T(3)+Q/(AH*HT(3))                                                   
      T(5)=T(3)                                                                 
      TW(5)=TW(3)                                                               
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9100                                              
      WRITE (OUT,9101)                                                          
 9101 FORMAT(' WRITE 9101   FILM BOIL OR VAPOR ONLY HEATING  "QBLRPP"')         
      WRITE (OUT,9102) T(3)  , HT(3) , TW(3) , T(5)                             
     *                ,TW(5) , X1    , X2    , X3                               
     *                ,XI    , XE                                               
 9102 FORMAT(' T(3)  =', E12.5,  ' HT(3) =', E12.5,  ' TW(3) =', E12.5          
     *      ,' T(5)  =', E12.5,/,' TW(5) =', E12.5,  ' X1    =', E12.5          
     *      ,' X2    =', E12.5,  ' X3    =', E12.5,/,' XI    =', E12.5          
     *      ,' XE    =', E12.5)                                                 
 9100 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
C*** IF VAPOR-ONLY FLOW DOES NOT OCCUR, GO TO 1320 TO COMPUTE DELTA P           
      IF(XE.LT.1.0)GO TO 1320                                                   
C*** COMPUTE OUTLET CONDITIONS FOR VAPOR-ONLY FLOW                              
      T(5)=(Q/W-CPY(1)*(TSAT-TIN)-HFG*(1.0-XI))/CPY(2)+TSAT                     
      XE=1.0                                                                    
      TW(5)=T(5)+Q/(AH*HT(5))                                                   
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9110                                              
      WRITE (OUT,9111)                                                          
 9111 FORMAT(' WRITE 9111   VAPOR ONLY HEATING  "QBLRPP"')                      
      WRITE (OUT,9112) T(5)  , XE    , TW(5)                                    
 9112 FORMAT(' T(5)  =', E12.5,  ' XE    =', E12.5,  ' TW(5) =', E12.5)         
 9110 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      GO TO 1320                                                                
C*** COMPUTE OUTLET CONDITIONS FOR VAPOR-ONLY FLOW FROM INLET TO OUTLET         
 1272 T(5)=TIN+Q/W/CPY(2)                                                       
      TW(5)=T(5)+Q/AH/HT(5)                                                     
      XI=1.000                                                                  
C$   ADD 144 FACTOR; DO NOT COMPUTE "D(IP+NSO)" HERE AND LINE 1475              
      XDP=DP(2)/144.                                                            
C$    D(IP+NSO)=D(IP+NSI)-DP(2)/144.                                            
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9120                                              
      WRITE (OUT,9121)                                                          
 9121 FORMAT(' WRITE 9121   VAPOR ONLY INLET TO OUTLET   "QBLRPP"')             
      WRITE (OUT,9122) T(5)  , TW(5)                                            
 9122 FORMAT(' T(5)  =', E12.5,  ' TW(5) =', E12.5)                             
 9120 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      GO TO 1475                                                                
C*** COMPUTE THE OUTLET TEMPERATURE FOR NO NET VAPOR GENERATION                 
C$ WAS  1280 T(5)= TIN+Q/(AH*CPY(1))                                            
 1280 T(5)= TIN+Q/(W*CPY(1))                                                    
 1310 PB=0.0                                                                    
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9130                                              
      WRITE (OUT,9131)                                                          
 9131 FORMAT(' WRITE 9131 TOUT FOR NO NET VAPOR GENERATED  "QBLRPP"')           
      WRITE (OUT,9132) T(5)  , PB                                               
 9132 FORMAT(' T(5)  =', E12.5,  ' PB    =', E12.5)                             
 9130 CONTINUE                                                                  
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      GO TO 1350                                                                
C*** COMPUTE THE TWO-PHASE PRESSURE DROP BY FAC. INC. CORRELATION               
C    OF BAROCZY'S GRAPHICAL CORRELATION                                         
 1320 PF=0.25*ALOG10(DP(1)/DP(2))                                               
      PB=XE**.7/1.7-(1-XE)**2.3/2.3+(1.-(1.-XE)**3.3)/(3.3*2.3*XE)              
C*** COMPUTE THE FRICTION PRESSURE DROP                                         
      PB=-XE**1.65*PF/2.65+(1.0+PF)*PB                                          
 1350 FP=DP(1)*(X1+X2)/L+(DP(1)+PB*(DP(2)-DP(1)))*X3/L                          
      FP=FP+DP(2)*(1.-(X1+X2+X3)/L)                                             
      IF(XE) 1382,1382,1385                                                     
C*** ADD THE MOMENTUM PRESSURE DROP                                             
 1382 MP=0.0                                                                    
      GO TO 1470                                                                
 1385 VE=XE/(XE+SL*(1.0-XE)/VR)                                                 
      IF(VE-1.0)1388,1386,1386                                                  
 1386 ME=VR                                                                     
      GO TO 1390                                                                
 1388 ME=((1.-XE)/(1.-VE))**2*(1.-VE*(1.-SL*SL/VR))                             
 1390 IF(XI)1395,1395,1400                                                      
 1395 MI=1.0                                                                    
      GO TO 1440                                                                
 1400 VI=XI/(XI+SL*(1.0-XI)/VR)                                                 
      IF(VI-1.0)1430,1425,1425                                                  
 1425 MI=VR                                                                     
      GO TO 1440                                                                
 1430 MI=((1.-XI)/(1.-VI))**2*(1.-VI*(1.-SL*SL/VR))                             
 1440 MP=G*G*V(1)*(ME-MI)/GC                                                    
 1470 XDP=(FP+MP)/144.                                                          
 1475 D(IP+NSO)=D(IP+NSI)-XDP                                                   
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9140                                              
      XFPPSI= FP/144.                                                           
      XMPPSI= MP/144.                                                           
      WRITE (OUT,9141)                                                          
 9141 FORMAT(' WRITE 9141     DELTA P CALC          FROM "QBLRPP"    ')         
      WRITE (OUT,9142) PF    , PB    , FP    , XE                               
     *                ,VE    , ME    , MI    , VI                               
     *                ,MP    , XMPPSI, XDP   , XFPPSI                           
     *                ,X1    , X2    , X3                                       
 9142 FORMAT(' PF    =', E12.5,  ' PB    =', E12.5,  ' FP    =', E12.5          
     *      ,' XE    =', E12.5,/,' VE    =', E12.5,  ' ME    =', E12.5          
     *      ,' MI    =', E12.5,  ' VI    =', E12.5,/,' MP    =', E12.5          
     *      ,' XMPPSI=', E12.5,  ' XDP   =', E12.5,  ' XFPPSI=', E12.5,/        
     *      ,' X1    =', E12.5,  ' X2    =', E12.5,  ' X3    =', E12.5)         
 9140 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF (D(IP+NSO).GT.0.0)GO TO 1476                                           
      WRITE(OUT,7655) D(IP+NSO),D(IP+NSI)                                       
 7655 FORMAT(' *+* OUTLET PRESSURE IN QBOILER WAS',E14.7,                       
     *  ' RESET PRESSURE TO INLET VALUE',E14.7)                                 
      XDP=0.0                                                                   
      D(IP+NSO)=D(IP+NSI)                                                       
 1476 CONTINUE                                                                  
C$  IS                                                                          
      D(IH+NSO)=D(IH+NSI)+Q/W                                                   
C$  WAS:                                                                        
C$    C$WAS IF(T(5)-TSAT)1478,1479,1479                                         
C$          IF(T(5)-TSAT)1478,14785,1479                                        
C$     1478 D(IH+NSO)=VHLIQ(LOCT,D(IP+NSI),T(5))                                
C$          GO TO 1480                                                          
C$    C$      NEW LINE BELOW CALCULATES EXIT ENTHALPY WHEN TWO-PHASE AT         
C$    14785 D(IH+NSO)=HSAT+XE*HFG                                               
C$          GO TO 1480                                                          
C$     1479 VV=VSV('QBLRPP 2',LOCT,D(IP+NSO),T(5))                              
C$          D(IH+NSO)=VH(LOCT,D(IP+NSO),T(5),VV)                                
C$     1480 CONTINUE                                                            
C$                                                                              
      D(IT+NSO)=T(5)                                                            
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IRITE=1                                                                   
      IF (IRITE .NE. 1) GO TO 9150                                              
      WRITE (OUT,9151)                                                          
 9151 FORMAT(' WRITE 9151   ASSIGN HOUT AND TOUT    FROM "QBLRPP"    ')         
      WRITE (OUT,9152) T(5)  , VV    , TSAT  , HSAT                             
     *                ,XE    , HFG                                              
 9152 FORMAT(' T(5)  =', E12.5,  ' VV    =', E12.5,  ' TSAT  =', E12.5          
     *      ,' HSAT  =', E12.5,/,' XE    =', E12.5,  ' HFG   =', E12.5)         
 9150 CONTINUE                                                                  
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      IF (ID(IRCD+13).EQ.0) GO TO 1579                                          
      IF(PASS.NE.1) GO TO 1577                                                  
      ID(IEVT+JEVI)=3                                                           
 1577 IF (TMAX.GE.TSAT) GO TO 1576                                              
      WRITE(OUT,1580) TMAX,TSAT,TW(5)                                           
 1580 FORMAT(5X,'WARNING FROM QBOILER: ERROR VARIABLE CANNNOT BE '/             
     *' CONTROLLED BECAUSE THE CONTROLLED WALL TEMPERATURE OF',E17.7/           
     *' IS LESS THAN THE SATURATION TEMPERATURE OF',E17.7/                      
     *' THE WALL TEMPERATURE IS',E17.7)                                         
 1576 IF(T(5).GT.TSAT)GO TO 1578                                                
      WRITE(OUT,1590)XE                                                         
 1590 FORMAT(5X,'WARNING FROM QBOILER: TWO PHASE FLUID AT OUTLET',              
     *' PROBABLY CANNOT CONTROL WALL TEMP. EXIT QUALITY IS',E12.4)              
 1578 D(IEV+JEVI) = (TMAX-TW(5))/XE                                             
 1579 CONTINUE                                                                  
 1485 FORMAT(' OVERALL: DP,TW(5),T(5) ',3F10.3)                                 
      IF (IFP.NE.1.OR. ICPP.NE.0) GO TO 99                                      
      CALL PIOP(1,NL,NSI,NSO)                                                   
      CALL LINES(2)                                                             
      WRITE(OUT,1581) ID(ISN+NSO),TW(5)                                         
 1581 FORMAT(5X,'BOILER AT STATION NO.',I4,' HAS A MAXIMUM WALL ',              
     * 'TEMPERATURE OF',E15.5,' DEGREES R')                                     
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
      WRITE(OUT,9581) XI, XE                                                    
 9581 FORMAT('       INLET QUAL=', F8.3, '    EXIT QUAL=', F8.3)                
C$XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        
   99 CONTINUE                                                                  
      RETURN                                                                    
C     QBLRPP                                                                    
      END                                                                       
C$    SUBROUTINE QBLRPZ IS NEW                                                  
      SUBROUTINE QBLRPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
     *,(CERR,C(6)), (OUT,C(7))                                                  
      INTEGER OUT                                                               
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                       
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/100/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 OPTION: 0 = DELTA P, 1 = SIGMA * DELTA P                                  
C   6 FRICTION FACTOR TABLE                                                     
C   7 COLBURN-FACTOR TABLE                                                      
C   8 HEAT EXCHANGER FLOW AREA                                                  
C   9 HEAT INPUT RATE IN (BUT/MIN)                                              
C  10 HEAT EXCHANGER FLOW-PATH LENGTH (FEET)                                    
C  11 HEAT EXCHANGER/HEAT TRANSFER AREA INCLUDING FINS (SQUARE FEET)            
C  12 HEAT EXCHANGER HYDRALIC DIAMETER                                          
C  13 ERROR VAR OPTION                                                          
C  14 ERROR VAR INDEX                                                           
C  15 OUTLET WALL TEMPERATURE                                                   
      I = IACDB(15)                                                             
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
      ID(I+6) = ITIDN(ICV(6),32)                                                
      ID(I+7) = ITIDN(ICV(7),33)                                                
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      ID(I+10) = IPARM(ICV(10))                                                 
      ID(I+11) = IPARM(ICV(11))                                                 
      ID(I+12) = IPARM(ICV(12))                                                 
      ID(I+13) = ICV(13)                                                        
      IF (ICV(13).EQ.0) RETURN                                                  
      ID(I+14) = IAEV(3)                                                        
      ID(I+15) = IPARM(ICV(14))                                                 
      RETURN                                                                    
C     QBOILPZ                                                                   
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      A COMPARTMENT HEAT LOAD                                                  
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE QLDPP                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)), (SCR(1),C(151)), (ICPP,C(88)), (OUT,C(7))                   
     *, (IFP,C(22)), (IGA,C(35)), (ILN,C(37)), (PASS,C(17))                     
     *, (CERR,C(16)), (IIOP,C(90)), (IFB,C(55))                                 
      DIMENSION SCR(30)                                                         
      INTEGER CERR, PASS, OUT                                                   
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO), (SCR(4),UA)          
     *, (SCR(5),TS), (SCR(6),Q), (SCR(7),CP), (SCR(8),TA), (SCR(9),PD)          
     *, (SCR(10),HS), (SCR(11),TDAR), (SCR(12),II), (SCR(13),SHR)               
     *, (SCR(14),SHRN), (SCR(15),IFPS), (SCR(16),ITER), (SCR(17),IDC)           
     *, (SCR(18),IDCN), (SCR(19),IS), (SCR(20),IFL), (SCR(21),IJ)               
     *, (SCR(22),TAN), (SCR(23),HOUT)                                           
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 SIGMA OPTION                                                              
C   6 PD TABLE NO.                                                              
C   7 UA TABLE NO.                                                              
C   8 TS TABLE NO.                                                              
C   9 B1                                                                        
C  10 Q TABLE NO.                                                               
C  11 B2                                                                        
      IRCD = IRCDB(11)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      IFL = ID(IFB+NL)                                                          
      D(IGA+41) = D(IW+NL)                                                      
      D(IH+NSO) = D(IH+NSI)                                                     
      IF (PASS.NE.1) GO TO 6                                                    
      SHR = 1.0                                                                 
      GO TO 7                                                                   
    6 SHR = D(IRCD-9)                                                           
    7 IF (ID(IRCD+7).NE.0 .AND. ID(IRCD+8).NE.0) GO TO 11                       
      IF (ID(IRCD+10).NE.0) GO TO 21                                            
      Q = 0.0                                                                   
      D(IT+NSO) = D(IT+NSI)                                                     
      TA = D(IT+NSI)                                                            
      ASSIGN 22 TO IS                                                           
      GO TO 31                                                                  
   11 UA = TLUP(ID(IRCD+7))*D(ID(IRCD+9))                                       
      TS = TLUP(ID(IRCD+8))                                                     
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
      IF (ABS(TAN-TA).LT.1.0) GO TO 43                                          
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
      IF (ABS(TAN-TA).LT.1.0) GO TO 45                                          
   44 CONTINUE                                                                  
   45 TA = TAN                                                                  
   46 ASSIGN 12 TO IS                                                           
      GO TO 31                                                                  
   12 Q = UA*(TS-TA)                                                            
      CALL TDA(NL,NSI,NSO,TDAR)                                                 
      SHRN = (D(IT+NSI)-TDAR)/(D(IT+NSI)-D(IT+NSO))                             
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
 1013 FORMAT(1H0,5X,36H***NON-CONVERGENCE SHR - QLOAD - LEG,I6)                 
   16 IF (II.NE.0) GO TO 90                                                     
      IF (IFPS.EQ.0) GO TO 90                                                   
      II = II+1                                                                 
      IFP = IFPS                                                                
      GO TO 13                                                                  
   21 Q = TLUP(ID(IRCD+10))*D(ID(IRCD+11))                                      
      IF (ID(IFL+1).EQ.1) GO TO 47                                              
   48 TAN = D(IT+NSI)                                                           
      DO 49 IJ=1,4                                                              
      TA = TAN                                                                  
      D(IT+NSO) = D(IT+NSI)+Q/(D(IW+NL)*SHP(NL,D(IP+NSI),TA,D(IH+NSI)))         
      TAN = 0.5*(D(IT+NSI)+D(IT+NSO))                                           
      IF (ABS(TAN-TA).LT.1.0) GO TO 50                                          
   49 CONTINUE                                                                  
   50 TA = TAN                                                                  
      GO TO 51                                                                  
   47 IF (ID(ID(IFL+3)-1).EQ.12) GO TO 48                                       
      HOUT = HFT(NL,D(IP+NSI),D(IT+NSI))+Q/D(IW+NL)                             
      D(IT+NSO) = TFH(NL,D(IP+NSI),HOUT)                                        
      TA = 0.5*(D(IT+NSI)+D(IT+NSO))                                            
   51 ASSIGN 22 TO IS                                                           
      GO TO 31                                                                  
   22 TDAR = D(IT+NSO)                                                          
      IF (D(IH+NSI).NE.0.0) CALL TDB(NL,NSI,NSO,HS)                             
      IF (D(IT+NSI).EQ.D(IT+NSO)) GO TO 23                                      
      SHR = (D(IT+NSI)-TDAR)/(D(IT+NSI)-D(IT+NSO))                              
      GO TO 90                                                                  
   23 SHR = 1.0                                                                 
      GO TO 90                                                                  
   31 PD = TLUP(ID(IRCD+6))                                                     
      IF (ID(IRCD+5).NE.0) PD = PD/SIG(NL,D(IP+NSI),TA)                         
      D(IP+NSO) = D(IP+NSI)-PD                                                  
      GO TO IS, (12,22)                                                         
   90 CONTINUE                                                                  
      IF (IIOP.EQ.0) D(IRCD-9) = SHR                                            
      D(IRCD-6) = Q                                                             
      IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      CALL PIOP(1,NL,NSI,NSO)                                                   
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HS) CALL HSOP(HS)                 
      CALL LINES(2)                                                             
      WRITE (OUT,1001) Q,SHR                                                    
 1001 FORMAT(1H0,5X,2HQT,F8.2,3X,3HSHR,F7.4)                                    
   99 CONTINUE                                                                  
      RETURN                                                                    
C     QLDPP                                                                     
      END                                                                       
      SUBROUTINE QLDPZ                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                       
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/011/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 SIGMA OPTION                                                              
C   6 PD TABLE NO.                                                              
C   7 UA TABLE NO.                                                              
C   8 TS TABLE NO.                                                              
C   9 B1                                                                        
C  10 Q TABLE NO.                                                               
C  11 B2                                                                        
      I = IACDB(11)                                                             
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
      ID(I+6) = ITIDN(ICV(6),1)                                                 
      IF (ICV(7).EQ.0 .AND. ICV(8).EQ.0) GO TO 1                                
      ID(I+7) = ITIDN(ICV(7),11)                                                
      ID(I+8) = ITIDN(ICV(8),12)                                                
      ID(I+9) = IPARM(ICV(9))                                                   
      GO TO 99                                                                  
    1 IF (ICV(10).EQ.0) GO TO 99                                                
      ID(I+10) = ITIDN(ICV(10),13)                                              
      ID(I+11) = IPARM(ICV(11))                                                 
   99 CONTINUE                                                                  
      RETURN                                                                    
C     QLDPZ                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      A RAM AIR INLET                                                          
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE RINSP                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151))                     
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (WTC,C(102)), (CUC,C(104))          
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))                
     *, (IGA,C(35)), (SDR,C(122)), (IH,C(30)), (IFB,C(55))                      
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE (SCR(1),NL), (SCR(2),ARIN), (SCR(3),INLT)                     
     *, (SCR(4),RHO), (SCR(5),VEL), (SCR(6),IRN), (SCR(7),NSO)                  
     *, (SCR(8),WTF), (SCR(9),CUF), (SCR(10),RI), (SCR(11),DRF)                 
     *, (SCR(12),ARDF), (SCR(13),SAR), (SCR(14),WTDF), (SCR(15),AXL)            
     *, (SCR(16),VOL), (SCR(17),DENS), (SCR(18),THK), (SCR(19),DRAG)            
     *, (SCR(20),TEMP), (SCR(21),PRES), (SCR(22),IFBL), (SCR(23),IFT)           
     *, (SCR(24),IM), (SCR(25),GMA)                                             
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION OUT                                                               
C   4 WTF                                                                       
C   5 CUF                                                                       
C   6 RI                                                                        
C   7 DRF                                                                       
C   8 ARIN                                                                      
C   9 THK                                                                       
C  10 IM                                                                        
C  11 INLT                                                                      
      IRCD = IRCDB(11)                                                          
      NL = ID(IRCD+2)                                                           
      NSO = ID(IRCD+3)                                                          
      WTF = D(ID(IRCD+4))                                                       
      CUF = D(ID(IRCD+5))                                                       
      RI = D(ID(IRCD+6))                                                        
      DRF = D(ID(IRCD+7))                                                       
      ARIN = D(ID(IRCD+8))                                                      
      THK = D(ID(IRCD+9))                                                       
      IM = ITLUP(ID(IRCD+10))                                                   
      DENS = D(IM+1)/1728.0                                                     
      INLT = ID(IRCD+11)                                                        
      IF (ARIN.NE.0.0) GO TO 10                                                 
      RHO = DEN(0,D(IGA+5),D(IGA+6))                                            
      VEL = D(IGA+4)                                                            
      IF (INLT.EQ.4) GO TO 4                                                    
      IF (VEL.NE.0.0) GO TO 11                                                  
      ARIN = D(IW+NL)*SQRT(D(IGA+6))/(8.9376*D(IGA+5))                          
      GO TO 10                                                                  
   11 GO TO (1,2,3), INLT                                                       
    1 ARIN = 3.6923*D(IW+NL)/(RHO*VEL)                                          
      GO TO 10                                                                  
    2 IF (D(IGA+3).GE.1.0) GO TO 3                                              
      ARIN = 3.4286*D(IW+NL)/(RHO*VEL)                                          
      GO TO 10                                                                  
    3 ARIN = 3.0*D(IW+NL)/(RHO*VEL)                                             
      GO TO 10                                                                  
    4 ARIN = 2.4*D(IW+NL)/ (DEN(NL,D(IP+NSO),D(IT+NSO))*0.3*                    
     * SOS(NL,D(IP+NSO),D(IT+NSO)))                                             
   10 WTC = 8.0*DENS*THK*ARIN                                                   
      GMA = GAM(0,D(IGA+5),D(IGA+6))                                            
      TEMP = D(IGA+8)/(1.0+0.01125*(GMA-1.0))                                   
      PRES = (D(IP+NSO)-1.242E-4*D(IW+NL)**2/(DEN(NL,D(IP+NSO),D(IT+NSO)        
     *)*ARIN**2))/(1.0+0.01125*(GMA-1.0))**(GMA/(GMA-1.0))                      
      ARDF = 2.4*D(IW+NL)/(DEN(NL,PRES,TEMP)*0.15*SOS(NL,PRES,TEMP))            
      SAR = (ARDF-ARIN)/0.10453                                                 
      IF (SAR.GT.0.0) GO TO 13                                                  
      ARDF = 0.0                                                                
      WTDF = 0.0                                                                
      AXL = 0.0                                                                 
      GO TO 15                                                                  
   13 WTDF = DENS*THK*SAR                                                       
      AXL = (SQRT(ARDF)-SQRT(ARIN))/0.1863                                      
   15 WTC = WTC+WTDF                                                            
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      WTIC = 0.205*WTC                                                          
      WTDC = 0.25*WTC                                                           
      VOL = 1.89*ARIN**1.5+AXL*(ARIN+ARDF+SQRT(ARIN*ARDF))/3.0                  
      DRAG = D(IW+NL)*VEL/1932.0                                                
      SDR = SDR+DRAG                                                            
      CUC = 13.0+0.139*(WTC)**1.65                                              
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      RIC = 0.00010                                                             
      IF (RI.NE.0.0) RIC = RI                                                   
      DRC = 1.0                                                                 
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      CALL SSA                                                                  
      CALL SCO                                                                  
      IFBL = ID(IFB+NL)                                                         
      IFT = ID(IFBL+1)                                                          
      IRN = ID(IFBL+2)                                                          
      CALL LINES(2)                                                             
      WRITE (OUT,1010) D(IW+NL),D(IGA+5),D(IP+NSO),D(IGA+6),D(IT+NSO)           
     *, D(IH+NSO),D(IH+NSO),IFT,IRN                                             
 1010 FORMAT(1H0,5X,1HW,F9.2,3X,2HPI,F8.2,3X,2HPO,F8.2,3X,2HTI,F8.2,3X,         
     *2HTO,F8.2,3X,2HHI,F8.4,3X,2HHO,F8.4,3X,2HFT,I8,3X,2HFN,I8)                
      CALL LINES(2)                                                             
      WRITE (OUT,1000) ARIN,ARDF,VOL,DRAG                                       
 1000 FORMAT(1H0,5X,2HAI,F8.1,3X,2HAD,F8.1,3X,3HVOL,F7.0,3X,4HDRAG,F6.1)        
      D(IGA+71) = VOL                                                           
      D(IGA+72) = ARIN                                                          
      IF (ARDF.NE.0.0) GO TO 99                                                 
      CALL LINES(2)                                                             
      WRITE (OUT,1020)                                                          
 1020 FORMAT(1H0,5X,21HDIFFUSER NOT REQUIRED)                                   
   99 CONTINUE                                                                  
      RETURN                                                                    
C     RINSP                                                                     
      END                                                                       
*DECK,RINSZ                                                                     
      SUBROUTINE RINSZ                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
     *, (SCR(1),C(151)), (IFB,C(55))                                            
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER CERR,OUT                                                          
      EQUIVALENCE (SCR(1),NL), (SCR(2),IFSF)                                    
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION OUT                                                               
C   4 WTF                                                                       
C   5 CUF                                                                       
C   6 RI                                                                        
C   7 DRF                                                                       
C   8 ARIN                                                                      
C   9 THK                                                                       
C  10 IM                                                                        
C  11 INLT                                                                      
      I = IACDB(11)                                                             
      ID(I+1) = ICV(1)                                                          
      NL = ILEGN(ICV(2))                                                        
      ID(I+2) = NL                                                              
      ID(I+3) = ISTAN(ICV(3))                                                   
      CALL FTL(NL,IFTA)                                                         
      ID(I+4) = IPARM(ICV(4))                                                   
      ID(I+5) = IPARM(ICV(5))                                                   
      ID(I+6) = IPARM(ICV(6))                                                   
      ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8) = IPARM(ICV(8))                                                   
      ID(I+9) = IPARM(ICV(9))                                                   
      ID(I+10) = ITIDN(ICV(10),20)                                              
      ID(I+11) = ICV(11)                                                        
      IFSF = ID(IFB)                                                            
      IFSF = ID(IFSF+1)                                                         
      IF (IFSF.EQ.2) GO TO 1                                                    
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1001) CERR                                                     
 1001 FORMAT(6H0ERROR,I6,5X,25HFREE STREAM FLUID INVALID)                       
    1 CONTINUE                                                                  
      IF (ICV(11).GT.0 .AND. ICV(11).LE.4) GO TO 99                             
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,ICV(11)                                             
 1000 FORMAT(6H0ERROR,I6,5X,18HINVALID INLET TYPE,I6)                           
   99 CONTINUE                                                                  
      RETURN                                                                    
C     RINSZ                                                                     
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      RAM AIR OUTLET                                                           
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE ROUTSP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (OUT,C(7)), (SCR(1),C(151))                     
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (WTC,C(102)), (CUC,C(104))          
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))                
     *, (IGA,C(35))                                                             
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO), (SCR(4),WTF)         
     *, (SCR(5),CUF), (SCR(6),RI), (SCR(7),DRF), (SCR(8),DENS)                  
     *, (SCR(9),THK), (SCR(10),AROUT), (SCR(11),VOL)                            
     *, (SCR(12),IM), (SCR(13),PR), (SCR(14),GMA)                               
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
C   9 AROUT                                                                     
C  10 THK                                                                       
C  11 IM                                                                        
C  12 DISCH COEFF                                                               
      IRCD = IRCDB(12)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      WTF = D(ID(IRCD+5))                                                       
      CUF = D(ID(IRCD+6))                                                       
      RI = D(ID(IRCD+7))                                                        
      DRF = D(ID(IRCD+8))                                                       
      AROUT = D(ID(IRCD+9))                                                     
      THK = D(ID(IRCD+10))                                                      
      IM = ITLUP(ID(IRCD+11))                                                   
      DENS = D(IM+1)/1728.0                                                     
      IF (AROUT.NE.0.0) GO TO 1                                                 
      PR = D(IP+NSO)/D(IP+NSI)                                                  
      IF (PR.LT.0.5283) PR = 0.5283                                             
      GMA = GAM(NL,D(IP+NSI),D(IT+NSI))                                         
      AROUT = D(IW+NL)/(40.1248*D(ID(IRCD+12))*SQRT(DEN(NL,D(IP+NSI),           
     *D(IT+NSI))*D(IP+NSI)*GMA/(GMA-1.0)*(PR**(2.0/GMA)-PR**((GMA+1.0)/         
     *GMA))))                                                                   
    1 WTC = 4.5*DENS*THK*(1.0+1.69*SQRT(AROUT)+1.2*AROUT)                       
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      VOL = 0.85*AROUT**1.5                                                     
      WTDC = 0.25*WTC                                                           
      WTIC = 0.205*WTC                                                          
      DRC = 1.0                                                                 
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC =10.0                                                
      CUC = 13.0+0.139*WTC**1.65                                                
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      RIC = 0.00010                                                             
      IF (RI.NE.0.0) RIC = RI                                                   
      CALL SSA                                                                  
      CALL SCO                                                                  
      CALL SCI(NL,NSI,NSO)                                                      
      CALL LINES(2)                                                             
      WRITE (OUT,1000) AROUT,VOL                                                
 1000 FORMAT(1H0,5X,2HAO,F8.1,3X,3HVOL,F7.0)                                    
      D(IGA+71) = VOL                                                           
      D(IGA+72) = AROUT                                                         
      RETURN                                                                    
C     ROUTSP                                                                    
      END                                                                       
*DECK,ROUTSZ                                                                    
      SUBROUTINE ROUTSZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (CERR,C(16)), (OUT,C(7))                     
     *, (SCR(1),C(151))                                                         
      DIMENSION ICV(18),SCR(30)                                                 
      INTEGER CERR,OUT                                                          
      EQUIVALENCE (SCR(1),NL)                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 WTF                                                                       
C   6 CUF                                                                       
C   7 RI                                                                        
C   8 DRF                                                                       
C   9 AROUT                                                                     
C  10 THK                                                                       
C  11 IM                                                                        
C  12 DISCH COEFF                                                               
      I = IACDB(12)                                                             
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
      ID(I+10) = IPARM(ICV(10))                                                 
      ID(I+11) = ITIDN(ICV(11),20)                                              
      ID(I+12) = IPARM(ICV(12))                                                 
      RETURN                                                                    
C     ROUTSZ                                                                    
      END                                                                       
      SUBROUTINE RPMPPP                                                         
      INTEGER OUT                                                               
      REAL MR,NR,EFFM,MPWR                                                      
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)),(IW,C(27)),(IP,C(28)),(IT,C(29))                 
     *,(IH,C(30)),(SCR(1),C(151)),(IFB,C(55)),(ITD,C(54))                       
     *,(IEV,C(49)),(IEVT,C(48))                                                 
     *, (ICPP,C(88)), (IFP,C(22)), (OUT,C(7)), (IGA,C(35))                      
      DIMENSION SCR(30)                                                         
      EQUIVALENCE  (SCR(1),NL),(SCR(2),NSI),(SCR(3),NSO)                        
     *,(SCR(4),NST),(SCR(5),TBL1),(SCR(6),TBL2),(SCR(7),TBL3)                   
     *, (SCR(8),QR), (SCR(9),PR), (SCR(10),RPM), (SCR(11),HP)                   
     *, (SCR(12),ERRV)                                                          
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 INLET STATION NO                                                          
C   4 OUTLET STATION NO                                                         
C   5 SHAFT NO                                                                  
C   6 PRESSURE RISE TABLE NO                                                    
C   7 PUMP EFFICIENCY TABLE NO                                                  
C   8 MOTOR EFFICIENCY TABLE                                                    
C   9 FLOW RATE AT ZERO PRESSURE DIFFERENCE (QR)                                
C  10 PRESSURE DIFFERENCE AT ZERO FLOW RATE, PSIA (PR)                          
C  11 SHAFT SPEED CORRESPONDING TO QR AND PR (RPM)                              
C  12 RATED OUTPUT OF MOTOR (HP)                                                
C  13 ERROR VARIABLE INDEX                                                      
C*** SET UP INTERNAL VARIABLES IN TERMS OF STORED ARRAY                         
      IRCD = IRCDB(13)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      NST = ID(IRCD+5)                                                          
C*** ENTER MASS FLOW RATE AS INDEPENDENT VARIABLE FOR TLUP                      
      D(IGA+41) = D(IW+NL)                                                      
      QR = D(ID(IRCD+9))                                                        
      PR = D(ID(IRCD+10))                                                       
      NR = D(ID(IRCD+11))                                                       
      MR = D(ID(IRCD+12))                                                       
      LOCT=ID(IFB+NL)                                                           
      LOCT=ID(LOCT+3)                                                           
      Q=D(IW+NL)                                                                
      D(IP+NSO)=D(IP+NSI)                                                       
      D(IT+NSO)=D(IT+NSI)                                                       
      D(IH+NSO)=D(IH+NSI)                                                       
C*** TEST TEMPERATURE TO SEE IF IN LIQUID STATE                                 
      TEMP=D(IT+NSI)                                                            
      TSAT=VTS(LOCT,D(IP+NSI))                                                  
      IF(TSAT.GT.TEMP) GO TO 390                                                
      WRITE(OUT,380) TEMP,TSAT,D(IP+NSI)                                        
  380 FORMAT (' *+* WARNING FROM RPMPPP:  INLET TWO-PHASE, WITH T=',            
     *E14.7,' (TSAT=',E14.7,') AT P=',E14.7,/,                                  
     *' USING SPECIFIC VOLUME AT SAT AT P')                                     
      TEMP=TSAT                                                                 
  390 GPM=7.4805*D(IW+NL)/VDL(LOCT,D(IT+NSI))                                   
C*** SET UP THE SHAFT SPEED AS AN INDEPENDENT VARIABLE FOR TULP                 
      D(IGA+101)=D(IGA+NST+80)                                                  
C*** SET UP THE MASS FLOW RATE AS THE SECOND INDEPENDENT VARIABLE FOR           
C    TULP                                                                       
      D(IGA+102)=Q                                                              
      RPM=D(IGA+NST+80)                                                         
C*** OBTAIN THE PRESSURE RISE FROM THE TABLE                                    
      DP=TLUP(ID(IRCD+6))                                                       
      IF(DP)570,410,410                                                         
  410 D(IP+NSO)=D(IP+NSI)+DP                                                    
      PWR=144.*DP*GPM/(7.4805*33000)                                            
C*** OBTAIN THE EFFICIENCY FROM THE TABLE                                       
      EFF=TLUP(ID(IRCD+7))                                                      
  412 CONTINUE                                                                  
C*** WATCH FOR UNREALISTIC EFFICIENCIES. REVISE EFFICIENCY                      
C    AND KEEP GOING IF EFFICIENCY IS UNREALISTIC.                               
      IF(EFF.LE.0.OR.EFF.GE.1.0)GO TO 570                                       
      SPWR=PWR/EFF                                                              
      HEAT=(SPWR-PWR)*42.416                                                    
      D(IGA+101)=SPWR                                                           
      EFFM=TLUP(ID(IRCD+8))                                                     
  414 CONTINUE                                                                  
C*** WATCH FOR UNREALISTIC MECHANICAL EFFICIENCY. REVISE EFFICIENCY             
C    AND KEEP GOING IF EFFICIENCY IS UNREALISTIC.                               
      IF(EFFM.LE.0.0.OR.EFFM.GE.1.0)GO TO 580                                   
      MPWR=SPWR/EFFM                                                            
      D(IGA+NST+90)=D(IGA+NST+90)-MPWR                                          
C*** TAKE THREE ITERATIONS ON THE SPECIFIC HEAT                                 
      DO 500 I=1,3                                                              
      D(IT+NSO)=D(IT+NSI)+HEAT/D(IW+NL)/                                        
     * VCPF(LOCT,D(IP+NSI),(D(IT+NSO)+D(IT+NSI))/2.)                            
  500 CONTINUE                                                                  
      D(IH+NSO)=D(IH+NSI)+SPWR*42.416/D(IW+NL)                                  
  560 IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      CALL PIOP(I,NL,NSI,NSO)                                                   
      CALL LINES(2)                                                             
      WRITE(OUT,1401) NSI,MPWR,RPM                                              
 1401 FORMAT(5X,'REFRIGERANT PUMP WITH INLET STATION NO.',                      
     *I10/'    HAS A SHAFT-POWER INPUT OF',E17.7,' HP AND A SHAFT',             
     * ' SPEED OF',E17.7,' RPM')                                                
   99 CONTINUE                                                                  
      RETURN                                                                    
  570 WRITE(OUT,575) D(IW+NL),RPM,EFF                                           
  575 FORMAT(' *+* UNREALISTIC EFFICIENCY IN REFRIGERANT PUMP FOR'/             
     *' *+* FLOW OF',F8.4,' AND',F8.1,' RPM (EFF =',F8.4,')'/                   
     *' *+* EFFICIENCY RESET TO BE BETWEEN 0. AND 1.')                          
      IF (EFF.LE.0.) EFF=0.01+EFF/1000.                                         
      IF (EFF.GE.1.) EFF=0.99+EFF/1000.                                         
      GO TO 412                                                                 
  580 WRITE(OUT,585) SPWR,EFFM                                                  
  585 FORMAT(' *+* UNREALISTIC MECHANICAL EFFICIENCY IN REFRIGERANT ',          
     *'PUMP'/' *+* WITH SHAFT POWER OF ',F10.3,'HP (EFFM=',F8.4,')'/            
     *' *+* MECHANICAL EFFICIENCY RESET TO BE BETWEEN 0. AND 1.')               
      IF (EFFM.LE.0) EFFM=0.01+EFFM/1000.                                       
      IF (EFFM.GE.0) EFFM=0.99+EFFM/1000.                                       
      GO TO 414                                                                 
C     RPMPPP                                                                    
      END                                                                       
C$    SUBROUTINE RPMPPZ IS NEW                                                  
      SUBROUTINE RPMPPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7)),(CERR,C(16)),(ICV(1),C(133)),                      
     * (SCR(1),C(151))                                                          
      INTEGER CERR,OUT                                                          
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                       
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/100/                                                            
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 SHAFT NO                                                                  
C   6 PRESSURE RISE TABLE                                                       
C   7 STATIC-EFFICIENCU TABLE                                                   
C   8 MOTOR-EFFICIENCY TABLE                                                    
C   9 FLOW RATE AT ZERO PRESSURE DIFFERENCE                                     
C  10 PRESSURE DIFFERENCE AT ZERO FLOW RATE, PSIA                               
C  11 SHAFT SPEED CORRESPONDING TO FLOW RATE AND PRESSURE DROP                  
C  12 RATED OUTPUT OF MOTOR, HP                                                 
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
      CALL SRT(ICV(5))                                                          
      ID(I+6)=ITIDN(ICV(6),8)                                                   
      ID(I+7)=ITIDN(ICV(7),9)                                                   
      ID(I+8)=ITIDN(ICV(8),10)                                                  
      ID(I+9)=IPARM(ICV(9))                                                     
      ID(I+10)=IPARM(ICV(10))                                                   
      ID(I+11)=IPARM(ICV(11))                                                   
      ID(I+12)=IPARM(ICV(12))                                                   
   69 RETURN                                                                    
C     RPMPPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      FOR A FLOW RATE, PRESSURE, TEMPERATURE OR HUMIDITY/ENTHALPY              
C      SENSOR                                                                   
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SENPP                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)),(SCR(1),C(151)),(IFB,C(55))                      
     *,(PASS,C(17)),(IEV,C(49)),(IEVT,C(48))                                    
     *,(IW,C(27)),(IP,C(28)),(IT,C(29)),(IH,C(30))                              
      INTEGER PASS                                                              
      DIMENSION SCR(30)                                                         
      EQUIVALENCE  (SCR(1),ICT),(SCR(5),NL),(SCR(6),NS)                         
     *,(SCR(4),JEVI),(SCR(8),LOCT),(SCR(9),VALUE)                               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG NUMBER                                                                
C   3 STATION NUMBER                                                            
C   4 SENSOR CODE(1=FLOW,2=PRESSURE,3=TEMPERATURE,4=ENTHALPY,                   
C                 5=SUPERHEAT,6=SUBCOOLING,7=QUALITY)                           
C   5 CONTROL VALUE                                                             
C   6 ERROR VARIABLE TYPE                                                       
C   7 ERROR VARIABLE INDEX                                                      
C$         JUST USE THE NEW ONE   5/5/92  SFW                                   
C$     IF NO CONTROL VALUE IN COL 28, ASSUME OLD STYLE SENSOR                   
C$    IF (ID(IRCD+5).EQ.0) GO TO 98                                             
C$    GO TO 02                                                                  
C$ 98 CALL SENPPO                                                               
C$    RETURN                                                                    
   02 IRCD=IRCDB(7)                                                             
      NL=ID(IRCD+2)                                                             
      NS=ID(IRCD+3)                                                             
      LOCT=ID(IFB+NL)                                                           
      LOCT=ID(LOCT+3)                                                           
      ICT=ID(IRCD+4)                                                            
      GO TO (10,20,30,40,50,60,70), ICT                                         
   10 VALUE=D(IW+NL)                                                            
C*** FLOW RATE IS BEING CONTROLLED                                              
      GO TO 100                                                                 
   20 VALUE=D(IP+NS)                                                            
C*** PRESSURE IS BEING CONTROLLED                                               
      GO TO 100                                                                 
   30 VALUE=D(IT+NS)                                                            
C*** TEMPERATURE IS BEING CONTROLLED                                            
      GO TO 100                                                                 
   40 VALUE=D(IH+NS)                                                            
C*** ENTHALPY IS BEING CONTROLLED                                               
      GO TO 100                                                                 
   50 P = D(IP+NS)                                                              
      TSAT = VTS(LOCT,P)                                                        
      TSH = D(ID(IRCD+5))                                                       
      V = VSV('SENPP   ',LOCT,P,TSAT)                                           
      HSAT = VH(LOCT,P,TSAT,V)                                                  
C*** COMPUTE APPROXIMATE SUPERHEAT (CORRECT AT CORRECT SUPERHEAT)               
C    BY DIVIDING THE ENTHALPY DIFFERENCE THE SPECIFIC HEAT OF THE               
C    VAPOR, THEREBY AVOIDING ZERO-SENSITIVITY ERRORS WHEN THE ITERATION         
C    RESULTS IN SATURATED OR TWO-PHASE FLUID AT THE SENSED POINT                
      VALUE = (D(IH+NS)-HSAT)/VCPV(LOCT,P,TSAT+0.5*TSH)                         
C*** SUPERHEAT IS BEING CONTROLLED                                              
      GO TO 100                                                                 
   60 P = D(IP+NS)                                                              
      TSAT = VTS(LOCT,P)                                                        
      TSC = D(ID(IRCD+3))                                                       
      HLIQ = VHLIQ(LOCT,P,TSAT)                                                 
C*** COMPUTE THE APPROXIMATE SUBCOOLING VIA THE ENTHALPY DIFFERENCE,            
C    AS WAS DONE FOR THE SUPERHEAT CASE                                         
      VALUE= (HLIQ-D(IH+NS))/VCPF(LOCT,P,TSAT-0.5*TSC)                          
C*** SUBCOOLING IS BEING CONTROLLED                                             
      GO TO 100                                                                 
   70 VALUE=VQUALH('SENPP   ',LOCT,D(IP+NS),D(IH+NS))                           
C*** QUALITY IS BEING CONTROLLED                                                
  100 JEVI=ID(IRCD+7)                                                           
      IF(PASS.NE.1) GO TO 110                                                   
      ID(IEVT+JEVI)=ID(IRCD+6)                                                  
  110 D(IEV+JEVI)=VALUE-D(ID(IRCD+5))                                           
C*** D(IEV+JEVI) IS THE VALUE OF THE CONTROL ERROR (ERROR VARIABLE)             
C    THIS ERROR WILL BECOME ZERO AS THE SYSTEM SOLUTION IS OBTAINED             
      RETURN                                                                    
C     SENPP                                                                     
      END                                                                       
c      SUBROUTINE SENPPO                                                         
c      COMMON /CC/ C(600)                                                        
c      EQUIVALENCE  (IRCD,C(45)), (SCR(1),C(151))                                
c     *, (PASS,C(17)), (IEV,C(49)), (IEVT,C(48))                                 
c      INTEGER PASS                                                              
c      DIMENSION SCR(30)                                                         
c      EQUIVALENCE  (SCR(1),JEVI)                                                
c      COMMON /DC/ DZ(2),D(128001)                                               
c      DIMENSION ID(128001)                                                      
c      EQUIVALENCE (ID(1),D(1))                                                  
cC   1 COMP CODE                                                                 
cC   2 SENSOR CODE                                                               
cC   3 CONTROL VALUE                                                             
cC   4 ERROR VARIABLE TYPE                                                       
cC   5 ERROR VARIABLE INDEX                                                      
c      IRCD = IRCDB(5)                                                           
c      JEVI = ID(IRCD+5)                                                         
c      IF(PASS.NE.1) GO TO 1                                                     
c      ID(IEVT+JEVI) = ID(IRCD+4)                                                
c    1 D(IEV+JEVI) = D(ID(IRCD+2))-D(ID(IRCD+3))                                 
c      RETURN                                                                    
cC     SENPPO                                                                    
cC$*************                                                                 
c      END                                                                       
      SUBROUTINE SENPZ                                                          
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)),(SCR(1),C(151))                               
      EQUIVALENCE (IW,C(27)), (IP,C(28)), (IT,C(29)), (IH,C(30))                
     *, (CERR,C(16)), (OUT,C(7))                                                
      INTEGER CERR,OUT                                                          
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),ICT), (SCR(2),NL),(SCR(3),NS), (SCR(4),IX)            
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C$       2 AND 3 INSERTED,  ICV(2) CHANGED TO 4 BELOW ETC.                      
C   2 LEG NUMBER                                                                
C   3 STATION NUMBER                                                            
C   4 SENSOR CODE                                                               
C   5 CONTROL VALUE                                                             
C   6 ERROR VARIABLE TYPE                                                       
C   7 ERROR VARIABLE INDEX                                                      
      I = IACDB(7)                                                              
      ID(I+1) = ICV(1)                                                          
      IF (ICV(4).GT.0.AND.ICV(4).LT.8) GO TO 5                                  
    9 CERR = CERR+1                                                             
      CALL LINES (2)                                                            
      WRITE (OUT,1000) CERR,ICV(4)                                              
 1000 FORMAT(6H0ERROR,I6,5X,22HINCORRECT CONTROL TYPE,I6)                       
      GO TO 6                                                                   
    5 ID(I+4) = ICV(4)                                                          
      ID(I+6)=ICV(4)                                                            
      NL = ILEGN(ICV(2))                                                        
      CALL LEGRT(NL)                                                            
      ID(I+2) = NL                                                              
      CALL FTR(NL,NF)                                                           
      IF(NF.LT.3.AND.ICV(4).GT.4)GO TO 9                                        
      NS=ISTAN(ICV(3))                                                          
      CALL START(NS)                                                            
      ID(I+3) = NS                                                              
    6 ID(I+5) = IPARM(ICV(5))                                                   
      ID(I+7) = IAEV(ICV(4))                                                    
      RETURN                                                                    
C     SENPZ                                                                     
      END                                                                       
C$*************                                                                 
      SUBROUTINE SEPRPP                                                         
C*** THIS SUBROUTINE SIMULATES THE PERFORMANCE OF A LIQUID/VAPOR                
C    SEPARATOR WITH INLET FLUID IN ANY STATE; PURE VAPOR LEAVING                
C    LEG OUT 1  AND PURE SATURATED LIQUID LEAVING LEG OUT 2                     
      COMMON /CC/C(600)                                                         
      EQUIVALENCE (IRCD,C(45)),(IW,C(27)),(IP,C(28)),(IT,C(29)),                
     *(IH,C(30)),(ISV,C(47)),(PASS,C(17)),(ISVT,C(46)),                         
     * (SCR(1),C(151)),(IFB,C(55)),(OUT,C(7))                                   
      INTEGER PASS,OUT                                                          
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (SCR(1),NLI),(SCR(2),NSI),(SCR(3),NLOG),                      
     *  (SCR(4),NSOG),(SCR(5),NLOF),(SCR(6),NSOF),(SCR(7),TS),                  
     *  (SCR(8),VG),(SCR(9),HG),(SCR(10),HFG)                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG IN WITH ANY QUALITY                                                   
C   3 STATION IN                                                                
C   4 LEG OUT 1 WITH SATURATED VAPOR INLY                                       
C   5 STATION OUT 1                                                             
C   6 LEG OUT 2 WITH SATURATED LIQUID ONLY                                      
C   7 STATION OUT 2                                                             
      IRCD=IRCDB(7)                                                             
      NLI=ID(IRCD+2)                                                            
      NSI=ID(IRCD+3)                                                            
      NLOG=ID(IRCD+4)                                                           
      NSOG=ID(IRCD+5)                                                           
      NLOF=ID(IRCD+6)                                                           
      NSOF=ID(IRCD+7)                                                           
      LOCT=ID(IFB+NLI)                                                          
      LOCT=ID(LOCT+3)                                                           
C*** G REPRESENTS THE GASEOUS STATE; F, THE FLUID STATE                         
C    COMPUTE THE SATURATION PROPERTIES                                          
      TS=VTS(LOCT,D(IP+NSI))                                                    
      VG=VSV('SEPRPP  ',LOCT,D(IP+NSI),TS)                                      
      HG=VH(LOCT,D(IP+NSI),TS,VG)                                               
      HFG=VHFG(LOCT,D(IP+NSI),TS,VG)                                            
C*** SET THE OUTLET PROPERTIES BASED ON PERFECT SEPERATION INTO                 
C    SATURATED LIQUID AND SATURATED VAPOR                                       
      D(IP+NSOG)=D(IP+NSI)                                                      
      D(IP+NSOF)=D(IP+NSI)                                                      
      D(IT+NSOG)=TS                                                             
      D(IT+NSOF)=TS                                                             
      D(IH+NSOG)=HG                                                             
      D(IH+NSOF)=HG-HFG                                                         
C***THE TOTAL ENERGY CONVECTED OUT EQUALS THAT CONVECTED IN                     
      D(IW+NLOG)=D(IW+NLI)*(D(IH+NSI)-HG+HFG)/HFG                               
      D(IW+NLOF)=D(IW+NLI)-D(IW+NLOG)                                           
C***CHECK FOR UNREASONABLE FLOW SPLIT                                           
      IF (D(IW+NLOF).LT.0.0.OR.D(IW+NLOG).LT.0.0) GO TO 50                      
   40 CONTINUE                                                                  
      IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      CALL PIOP(1,NLI,NSI,NSOG)                                                 
      CALL LINES(2)                                                             
      WRITE (OUT,1001) D(IW+NLOF),D(IW+NLOG)                                    
 1001 FORMAT (1H0,5X,12HIN SEPARATOR,18H   LIQUID FLOW OUT,E14.5,               
     * 17H   VAPOR FLOW OUT,E14.5)                                              
   99 CONTINUE                                                                  
      RETURN                                                                    
   50 QUAL=(D(IH+NSI)-HG+HFG)/HFG                                               
      WRITE (OUT,1004) QUAL                                                     
 1004 FORMAT (1H0,5X,30HINLET QUALITY TO SEPARATOR WAS,E14.5,                   
     *  20H  POSSIBLY AN ERROR )                                                
      WRITE (OUT,1002) D(IW+NLOF),D(IW+NLOG)                                    
 1002 FORMAT(1H0,5X,36HUNREASONABLE FLOW SPLIT IN SEPARATOR                     
     * ,  16H LIQUID FLOW OUT,E14.5,19H AND VAPOR FLOW OUT,E14.5)               
      WRITE (OUT,1003)                                                          
 1003 FORMAT (1H0,5X,25HNEGATIVE FLOW SET TO ZERO)                              
      IF (D(IW+NLOF).GT.0.0) GO TO 60                                           
      D(IW+NLOG)=D(IW+NLI)                                                      
      D(IW+NLOF)=0.                                                             
      GO TO 40                                                                  
   60 D(IW+NLOF)=D(IW+NLI)                                                      
      D(IW+NLOG)=0.                                                             
      GO TO 40                                                                  
C     SEPRPP                                                                    
      END                                                                       
      SUBROUTINE SEPRPZ                                                         
C$    SUBROUTINE SEPRPZ IS NEW                                                  
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)),(SCR(1),C(151)),(CERR,C(16))                  
     *, (OUT,C(7))                                                              
      DIMENSION ICV(18), SCR(30)                                                
      INTEGER CERR, OUT                                                         
      EQUIVALENCE (SCR(1),NLI), (SCR(2),NSI), (SCR(3),NLOG)                     
     *, (SCR(4),NSOG), (SCR(5),NLOF), (SCR(6),NSOF), (SCR(7),IOP)               
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C$   DATA IFTA NOT USED IN SEPRPZ                                               
C$    DATA IFTA/100/                                                            
C   1 COMP CODE                                                                 
C   2 LEG IN WITH ANY QUALITY                                                   
C   3 STATION IN                                                                
C   4 LEG OUT 1 WITH SATURATED VAPOR ONLY                                       
C   5 STATION OUT 1                                                             
C   6 LEG OUT 2 WITH LIQUID ONLY                                                
C   7 STATION OUT 2                                                             
      I = IACDB(7)                                                              
      ID(I+1) = ICV(1)                                                          
      NLI = ILEGN(ICV(2))                                                       
      ID(I+2) = NLI                                                             
      CALL LEGRT(NLI)                                                           
      CALL FRR(NLI,NF)                                                          
      NSI = ISTAN(ICV(3))                                                       
      ID(I+3) = NSI                                                             
      CALL START(NSI)                                                           
      NLOG = ILEGN(ICV(4))                                                      
      CALL FRS(NLOG,NF)                                                         
      ID(I+4) = NLOG                                                            
      CALL LEGRS(NLOG)                                                          
      NSOG = ISTAN(ICV(5))                                                      
      ID(I+5) = NSOG                                                            
      CALL STARS(NSOG)                                                          
      NLOF = ILEGN(ICV(6))                                                      
      ID(I+6) = NLOF                                                            
      CALL LEGRS(NLOF)                                                          
      CALL FRS(NLOF,NF)                                                         
      NSOF = ISTAN(ICV(7))                                                      
      ID(I+7) = NSOF                                                            
      CALL STARS(NSOF)                                                          
      CALL FRS(NLOF,NF1)                                                        
C$  NOT NEEDED                                                                  
C$    CONTINUE                                                                  
      RETURN                                                                    
C     SEPRPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      A SHAFT                                                                  
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SHFTPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (PASS,C(17)), (ISVT,C(46)), (ISV,C(47))         
     *, (ICPP,C(88)), (IFP,C(22)), (OUT,C(7))                                   
     *, (IGA,C(35)), (SCR(1),C(151))                                            
      DIMENSION SCR(30)                                                         
      INTEGER PASS, OUT                                                         
      EQUIVALENCE (SCR(1),NST), (SCR(2),IOP), (SCR(3),SPEED)                    
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 SHAFT                                                                     
C   3 SHAFT TYPE                                                                
C   4 SHAFT SPEED                                                               
      IRCD = IRCDB(4)                                                           
      NST = ID(IRCD+2)                                                          
      IOP = ID(IRCD+3)                                                          
      IF (IOP.NE.0) GO TO 1                                                     
      SPEED = D(ID(IRCD+4))                                                     
      GO TO 3                                                                   
    1 IF (PASS.NE.1) GO TO 2                                                    
      ID(ISVT+IOP) = 7                                                          
      D(ISV+IOP) = D(ID(IRCD+4))                                                
    2 SPEED = D(ISV+IOP)                                                        
    3 D(IGA+80+NST) = SPEED                                                     
      D(IRCD-6) = SPEED                                                         
      IF (IFP.NE.1 .OR. ICPP.NE.0 .OR. D(IGA+80+NST).EQ.0.0) GO TO 99           
      CALL PIOP(0,0,0,0)                                                        
      CALL LINES(2)                                                             
      WRITE (OUT,1000) NST,SPEED                                                
 1000 FORMAT(1H0,5X,6HSHAFT ,I4,3X,1HN,F9.0)                                    
   99 CONTINUE                                                                  
      D(IGA+90+NST) = 0.0                                                       
      RETURN                                                                    
C     SHFTPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A SHAFT                                                   
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SHFTPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133))                                               
      DIMENSION ICV(18)                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 SHAFT                                                                     
C   3 SHAFT TYPE                                                                
C   4 SHAFT SPEED                                                               
      I = IACDB(4)                                                              
      ID(I+1) = ICV(1)                                                          
      ID(I+2) = ICV(2)                                                          
      CALL SRS(ICV(2))                                                          
      IF (ICV(3).EQ.0) GO TO 1                                                  
      ID(I+3) = IASV(7)                                                         
    1 ID(I+4) = IPARM(ICV(4))                                                   
      RETURN                                                                    
C     SHFTPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      FLOW SPLITTER                                                            
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SPLTPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)), (ISV,C(47)), (PASS,C(17)), (ISVT,C(46))                     
     *, (SCR(1),C(151))                                                         
     *, (IIOP,C(90))                                                            
      INTEGER PASS                                                              
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (SCR(1),NLI), (SCR(2),NSI), (SCR(3),NLO1)                     
     *, (SCR(4),NSO1), (SCR(5),NLO2), (SCR(6),NSO2), (SCR(7),RS)                
     *, (SCR(8),IOP)                                                            
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG IN                                                                    
C   3 STATION IN                                                                
C   4 LEG OUT 1                                                                 
C   5 STATION OUT 1                                                             
C   6 LEG OUT 2                                                                 
C   7 STATION OUT 2                                                             
C   8 ISR                                                                       
C   9 IOP                                                                       
      IRCD = IRCDB(9)                                                           
      NLI = ID(IRCD+2)                                                          
      NSI = ID(IRCD+3)                                                          
      NLO1 = ID(IRCD+4)                                                         
      NSO1 = ID(IRCD+5)                                                         
      D(IP+NSO1) = D(IP+NSI)                                                    
      D(IT+NSO1) = D(IT+NSI)                                                    
      D(IH+NSO1) = D(IH+NSI)                                                    
      IOP = ID(IRCD+9)                                                          
      IF (IOP.GE.0) GO TO 1                                                     
      D(IW+NLO1) = 0.5*D(IW+NLI)                                                
      GO TO 99                                                                  
    1 NLO2 = ID(IRCD+6)                                                         
      NSO2 = ID(IRCD+7)                                                         
      D(IP+NSO2) = D(IP+NSI)                                                    
      D(IT+NSO2) = D(IT+NSI)                                                    
      D(IH+NSO2) = D(IH+NSI)                                                    
      IF (IOP.GT.0) GO TO 2                                                     
      RS = D(ID(IRCD+8))                                                        
      GO TO 4                                                                   
    2 IF (PASS.NE.1) GO TO 3                                                    
      ID(ISVT+IOP) = 5                                                          
      D(ISV+IOP) = D(ID(IRCD+8))                                                
    3 RS = D(ISV+IOP)                                                           
    4 D(IW+NLO1) = RS*D(IW+NLI)                                                 
      D(IW+NLO2) = (1.0-RS)*D(IW+NLI)                                           
      IF (IOP.EQ.0) GO TO 99                                                    
      IF (IIOP.NE.IOP) GO TO 99                                                 
      ITER = 0                                                                  
      IF (D(IW+NLI).LE.0.01) GO TO 99                                           
    7 IF (RS.GE.0.5) GO TO 5                                                    
      IF (D(IW+NLO1).GE.0.01 .AND. RS.GE.0.001) GO TO 99                        
      RS = 1.1*RS                                                               
      D(ISV+IOP) = 2.0*D(ISV+IOP)                                               
      GO TO 6                                                                   
    5 IF (D(IW+NLO2).GE.0.01 .AND. RS.LE.0.999) GO TO 99                        
      RS = 0.9*RS                                                               
      D(ISV+IOP) = 0.5*D(ISV+IOP)                                               
      GO TO 6                                                                   
    6 D(IW+NLO1) = RS*D(IW+NLI)                                                 
      D(IW+NLO2) = (1.0-RS)*D(IW+NLI)                                           
      ITER = ITER+1                                                             
      IF (ITER.EQ.100) GO TO 99                                                 
      GO TO 7                                                                   
   99 CONTINUE                                                                  
      RETURN                                                                    
C     SPLTPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A FLOW SPLITTER                                           
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SPLTPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NLI), (SCR(2),NSI), (SCR(3),NLO1)                     
     *, (SCR(4),NSO1), (SCR(5),NLO2), (SCR(6),NSO2), (SCR(7),IOP)               
     *, (SCR(8),NF)                                                             
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG IN                                                                    
C   3 STATION IN                                                                
C   4 LEG OUT 1                                                                 
C   5 STATION OUT 1                                                             
C   6 LEG OUT 2                                                                 
C   7 STATION OUT 2                                                             
C   8 ISR                                                                       
C   9 IOP                                                                       
      I = IACDB(9)                                                              
      ID(I+1) = ICV(1)                                                          
      NLI = ILEGN(ICV(2))                                                       
      ID(I+2) = NLI                                                             
      CALL LEGRT(NLI)                                                           
      NSI = ISTAN(ICV(3))                                                       
      ID(I+3) = NSI                                                             
      CALL START(NSI)                                                           
      CALL FRR(NLI,NF)                                                          
      NLO1 = ILEGN(ICV(4))                                                      
      ID(I+4) = NLO1                                                            
      CALL LEGRS(NLO1)                                                          
      NSO1 = ISTAN(ICV(5))                                                      
      ID(I+5) = NSO1                                                            
      CALL STARS(NSO1)                                                          
      CALL FRS(NLO1,NF)                                                         
      IOP = ICV(8)                                                              
      IF (IOP.LT.0) GO TO 1                                                     
      NLO2 = ILEGN(ICV(6))                                                      
      ID(I+6) = NLO2                                                            
      CALL LEGRS(NLO2)                                                          
      NSO2 = ISTAN(ICV(7))                                                      
      ID(I+7) = NSO2                                                            
      CALL STARS(NSO2)                                                          
      CALL FRS(NLO2,NF)                                                         
      ID(I+8) = IPARM(ICV(9))                                                   
    1 ID(I+9) = IOP                                                             
      IF (IOP.LE.0) GO TO 99                                                    
      ID(I+9) = IASV(5)                                                         
   99 CONTINUE                                                                  
      RETURN                                                                    
C     SPLTPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      A SHAFT POWER BALANCE                                                    
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SPOWPP                                                         
      COMMON /CC/ C(600)                                                        
      COMMON /DT/ DTES(64001)                                                   
      EQUIVALENCE (IRCD,C(45)), (IGA,C(35)), (PASS,C(17)), (IEVT,C(48))         
     *, (ICPP,C(88)), (IFP,C(22)), (OUT,C(7))                                   
     *, (IEV,C(49)), (SCR(1),C(151))                                            
      DIMENSION SCR(30)                                                         
      INTEGER PASS, OUT                                                         
      EQUIVALENCE (SCR(1),NST), (SCR(2),IERR), (SCR(3),HP)                      
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 SHAFT                                                                     
C   3 POWER BAL.                                                                
C   4 POWER ERR.                                                                
      IRCD = IRCDB(4)                                                           
      NST = ID(IRCD+2)                                                          
      IERR = ID(IRCD+4)                                                         
      IF (PASS.NE.1) GO TO 1                                                    
      ID(IEVT+IERR) = 5                                                         
    1 HP = D(IGA+90+NST)                                                        
      D(IRCD-6) = HP                                                            
      IF (IFP.NE.1) D(IEV+IERR) = HP+D(ID(IRCD+3))                              
      IF (IFP.NE.1) DTES(IEV+IERR) = D(ID(IRCD+3))                              
      IF (IFP.NE.1 .OR. ICPP.NE.0 .OR. D(ID(IRCD+3)).EQ.0.0) GO TO 99           
      CALL PIOP(0,0,0,0)                                                        
      CALL LINES(2)                                                             
      WRITE (OUT,1000) NST,HP                                                   
 1000 FORMAT(1H0,5X,6HSHAFT ,I4,3X,2HHP,F8.2)                                   
   99 CONTINUE                                                                  
      RETURN                                                                    
C     SPOWPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A SHAFT POWER BALANCE                                     
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE SPOWPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133))                                               
      DIMENSION ICV(18)                                                         
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 SHAFT                                                                     
C   3 POWER BAL.                                                                
C   4 POWER ERR.                                                                
      I = IACDB(4)                                                              
      ID(I+1) = ICV(1)                                                          
      ID(I+2) = ICV(2)                                                          
      CALL SRT(ICV(2))                                                          
      ID(I+3) = IPARM(ICV(3))                                                   
      ID(I+4) = IAEV(5)                                                         
      CALL SRE(ICV(2))                                                          
      RETURN                                                                    
C     SPOWPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      A TURBINE                                                                
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE TURBPP                                                         
      COMMON /CC/ C(600)                                                        
      COMMON /DT/ DTES(64001)                                                   
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)), (SCR(1),C(151)), (ISV,C(47)), (ISVT,C(46))                  
     *, (IEV,C(49)), (IEVT,C(48)), (T0,C(65)), (PASS,C(17))                     
     *, (IGA,C(35)), (ICPP,C(88)), (IFP,C(22)), (OUT,C(7))                      
     *, (IIOP,C(90))                                                            
      DIMENSION SCR(30)                                                         
      INTEGER PASS, OUT                                                         
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
     *, (SCR(4),NSA), (SCR(5),NPA), (SCR(6),GAMMA), (SCR(7),FFC)                
     *, (SCR(8),DTI), (SCR(9),EFF), (SCR(10),NST), (SCR(11),PR)                 
     *, (SCR(12),IOP), (SCR(13),JSVI), (SCR(14),JEVI), (SCR(15),HP)             
     *, (SCR(16),TAV), (SCR(17),HS), (SCR(18),IPRT), (SCR(19),AM)               
     *, (SCR(20),PRTS)                                                          
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 SHAFT NO                                                                  
C   6 OPTION                                                                    
C   7 PRESSURE RATIO                                                            
C   8 FLOW FACTOR TABLE NO                                                      
C   9 EFFICIENCY TABLE NO                                                       
C  10 MECHANICAL EFFICIENCY                                                     
C  11 STATE VARIABLE INDEX                                                      
C  12 ERROR VARIABLE INDEX                                                      
C  13 FLOW FACTOR B                                                             
C  14 EXIT AREA                                                                 
      IRCD = IRCDB(14)                                                          
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      D(IGA+41) = D(IW+NL)                                                      
      NST = ID(IRCD+5)                                                          
      NSA= NST + 80                                                             
      NPA = NST + 90                                                            
      IOP = ID(IRCD+6)                                                          
      IPRT = IOP/10                                                             
      IOP = IOP-IPRT*10                                                         
      JSVI = ID(IRCD+11)                                                        
      JEVI = ID(IRCD+12)                                                        
      IF (PASS.NE.1 .OR. IOP.EQ.0) GO TO 1                                      
      ID(ISVT+JSVI) = 8                                                         
      D(ISV+JSVI) = D(ID(IRCD+7))                                               
    1 IF (IOP.EQ.0) GO TO 2                                                     
      IF (D(ISV+JSVI).LT.1.0 .AND. IIOP.NE.JSVI) D(ISV+JSVI) = 1.0              
      IF (IOP.EQ.1) GO TO 3                                                     
    4 D(IGA+24) = D(ISV+JSVI)                                                   
      D(IGA+25) = D(IGA+NSA) / SQRT(D(IT+NSI) / T0 )                            
      FFC = TLUP(ID(IRCD+8))                                                    
      IF (ID(IRCD+13).NE.0) FFC = FFC*D(ID(IRCD+13))                            
      IF(PASS.NE.1) GO TO 5                                                     
      ID(IEVT+JEVI) = 1                                                         
    5 D(IEV+JEVI) = D(IW+NL)-FFC*D(IP+NSI)/SQRT(D(IT+NSI))                      
C     WRITE(6,3999) D(IW+NL),FFC,D(IP+NSI),D(IT+NSI)                            
C3999 FORMAT(' ','TURB=',4E15.7)                                                
      DTES(IEV+JEVI) = D(IW+NL)/(FFC*D(IP+NSI)/SQRT(D(IT+NSI)))                 
    3 PR = D(ISV+JSVI)                                                          
      GO TO 6                                                                   
    2 PR = D(ID(IRCD+7))                                                        
    6 GAMMA =  GAM(NL,D(IP+NSI),D(IT+NSI))                                      
      DTI = D(IT+NSI)*(1.0-PR**((1.0-GAMMA)/GAMMA))                             
      IF (DTI.LE.0.0) DTI = 1.E-6                                               
      D(IRCD-6) = PR                                                            
      D(IGA+23) = D(IGA+NSA) / SQRT(DTI)                                        
      D(IGA+24) = PR                                                            
      EFF = TLUP(ID(IRCD+9))                                                    
      D(IRCD-7) = EFF                                                           
      D(IT+NSO) = D(IT+NSI) - EFF * DTI                                         
      D(IP+NSO) = D(IP+NSI)/PR                                                  
      IF (IPRT.EQ.0) GO TO 8                                                    
      AM = 2.4*D(IW+NL)/(DEN(NL,D(IP+NSO),D(IT+NSO))*D(ID(IRCD+14)))/           
     *SOS(NL,D(IP+NSO),D(IT+NSO))                                               
      GAMMA = GAM(NL,D(IP+NSO),D(IT+NSO))                                       
      PRTS = (1.0+0.5*(GAMMA-1.0)*AM**2)**(GAMMA/(GAMMA-1.0))                   
      D(IP+NSO) = D(IP+NSI)/PR*PRTS                                             
    8 D(IH+NSO) = D(IH+NSI)                                                     
      TAV = 0.5*(D(IT+NSI)+D(IT+NSO))                                           
      HP = 0.02356 * D(IW+NL) * SHP(NL,D(IP+NSI),TAV,D(IH+NSI))                 
     * *(D(IT+NSI)-D(IT+NSO))*D(ID(IRCD+10))                                    
      D(IRCD-8) = HP                                                            
      D(IGA+NPA) = D(IGA+NPA)+HP                                                
      IF (D(IH+NSI).NE.0.0) CALL TDB(NL,NSI,NSO,HS)                             
      IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      CALL PIOP(1,NL,NSI,NSO)                                                   
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSO).GT.HS) CALL HSOP(HS)                 
      CALL LINES(2)                                                             
      WRITE (OUT,1001) NST,PR,EFF,HP                                            
 1001 FORMAT(1H0,5X,6HSHAFT ,I4,3X,2HPR,F8.4,3X,3HEFF,F7.4,3X,2HHP,F8.2)        
   99 CONTINUE                                                                  
      RETURN                                                                    
C     TURBPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A TURBINE                                                 
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE TURBPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE  (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                      
     *, (SCR(4),IPRO), (SCR(5),IPRT)                                            
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 SHAFT NO                                                                  
C   6 OPTION                                                                    
C   7 PRESSURE RATIO                                                            
C   8 FLOW FACTOR TABLE NO                                                      
C   9 EFFICIENCY TABLE NO                                                       
C  10 MECHANICAL EFFICIENCY                                                     
C  11 STATE VARIABLE INDEX                                                      
C  12 ERROR VARIABLE INDEX                                                      
C  13 FLOW FACTOR B                                                             
C  14 EXIT AREA                                                                 
      I = IACDB(14)                                                             
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
      CALL SRT(ICV(5))                                                          
      ID(I+6) = ICV(6)                                                          
      IPRT = ICV(6)/10                                                          
      IPRO = ICV(6)-IPRT*10                                                     
      ID(I+7) = IPARM(ICV(7))                                                   
      IF (IPRO.EQ.2) ID(I+8) = ITIDN(ICV(8),8)                                  
      ID(I+9) = ITIDN(ICV(8),7)                                                 
      ID(I+10) = IPARM(ICV(9))                                                  
      IF (IPRO-1) 2,3,4                                                         
    4 ID(I+12) = IAEV(1)                                                        
      ID(I+13) = IPARM(ICV(10))                                                 
    3 ID(I+11) = IASV(8)                                                        
    2 IF (IPRT.NE.0) ID(I+14) = IPARM(ICV(11))                                  
      RETURN                                                                    
C     TURBPZ                                                                    
      END                                                                       
*DECK,USERPP                                                                    
      SUBROUTINE USERPP                                                         
      RETURN                                                                    
C     USERPP                                                                    
      END                                                                       
*DECK,USERPZ                                                                    
      SUBROUTINE USERPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (CERR,C(16)), (OUT,C(7))                                      
      INTEGER CERR, OUT                                                         
      DIMENSION CD (200)                                                        
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84))
      REAL*8 cd, cn                                                     
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,CN                                                  
 1000 FORMAT(6H0ERROR,I6,5X,10HCOMPONENT ,A6,13H NOT PROVIDED)                  
      RETURN                                                                    
C     USERPZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR CODE THAT                 
C     THE USER WANTS TO INCLUDE                                                 
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE USERSP                                                         
      RETURN                                                                    
C     USERSP                                                                    
      END                                                                       
*DECK,USERSZ                                                                    
      SUBROUTINE USERSZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (CERR,C(16)), (OUT,C(7))                                      
      INTEGER CERR, OUT                                                         
      DIMENSION CD(200)                                                         
      EQUIVALENCE (CD(1),C(201))                                                
      EQUIVALENCE (CN,CD(84))
      REAL*8 cd, cn                                                     
      CERR = CERR+1                                                             
      CALL LINES(2)                                                             
      WRITE (OUT,1000) CERR,CN                                                  
 1000 FORMAT(6H0ERROR,I6,5X,10HCOMPONENT ,A6,13H NOT PROVIDED)                  
      RETURN                                                                    
C     USERSZ                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE CALCULATES THE COMPONENT PERFORMANCE FOR                   
C      A WATER SEPERATOR                                                        
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE WSEPPP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (IW,C(27)), (IP,C(28)), (IT,C(29))              
     *, (IH,C(30)), (SCR(1),C(151)), (ICPP,C(88)), (OUT,C(7))                   
     *, (IFP,C(22)), (IGA,C(35)), (PASS,C(17))                                  
      DIMENSION SCR(30)                                                         
      INTEGER PASS, OUT                                                         
      EQUIVALENCE (SCR(1),NLI), (SCR(2),NSI), (SCR(3),NLO1)                     
     *, (SCR(4),NSO1), (SCR(5),NLO2), (SCR(6),NSO2), (SCR(7),DW)                
     *, (SCR(8),PD), (SCR(9),HS), (SCR(10),DH), (SCR(11),EFF)                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 INLET LEG NO                                                              
C   3 INLET STATION NO                                                          
C   4 OUTLET LEG NO                                                             
C   5 OUTLET STATION NO                                                         
C   6 DRAIN LEG NO                                                              
C   7 DRAIN STATION NO                                                          
C   8 OPTION                                                                    
C   9 PRESSURE DROP TABLE NO                                                    
C  10 EFF TABLE NO                                                              
      IE = 1                                                                    
      GO TO 1                                                                   
      ENTRY DRNPP                                                               
      IE = 2                                                                    
    1 IRCD = IRCDB(10)                                                          
      NLI = ID(IRCD+2)                                                          
      NSI = ID(IRCD+3)                                                          
      NLO1 = ID(IRCD+4)                                                         
      NSO1 = ID(IRCD+5)                                                         
      NLO2 = ID(IRCD+6)                                                         
      NSO2 = ID(IRCD+7)                                                         
      IF (PASS.EQ.1) CALL FLUIDP(NLO2)                                          
      D(IGA+41) = D(IW+NLI)                                                     
      PD = 0.                                                                   
      DH = 0.                                                                   
      DW = 0.                                                                   
      IF (D(IH+NSI).NE.0.0) CALL TDB1(NLI,NSI,NSI,HS)                           
      IF (IE.EQ.2) GO TO 3                                                      
      D(IGA+30) = 0.                                                            
      IF (D(IH+NSI).NE.0.0 .AND. D(IH+NSI).GE.HS) D(IGA+30) = 1.0               
      PD = TLUP(ID(IRCD+9))                                                     
      IF (ID(IRCD+8).EQ.0) GO TO 3                                              
      PD = PD / SIG(NLI,D(IP+NSI) ,D(IT+NSI) )                                  
    3 D(IP+NSO1) = D(IP+NSI) - PD                                               
      D(IP+NSO2) = D(IP+NSO1)                                                   
C     WRITE (6,5999) D(IT+NSO1),D(IT+NSI)                                       
C5999 FORMAT(' ','D(IT+NSO1)=',E16.8,'D(IT+NSI)',E16.8)                         
      D(IT+NSO1) = D(IT+NSI)                                                    
C *** ******************************************************************        
C ***                                                                           
C *** TRY CHANGE D(IT+NOS1) = D(IT+NSI) PFM 7/26/85                             
C *** CHANGED BACK D(IT+NOS1) = D(IT+NSI) PFM 8/09/85                           
C ***                                                                           
C *** ******************************************************************        
C     WRITE (6,5998) D(IT+NSO2),D(IT+NSO1)                                      
C5998 FORMAT(' ','D(IT+NSO2)=',E16.8,'D(IT+NSO1)',E16.8)                        
      D(IT+NSO2) = D(IT+NSI)                                                    
C     D(IT+NSO2) = D(IT+NSO1)                                                   
      D(IH+NSO1) = D(IH+NSI)                                                    
      D(IH+NSO2) = 0.                                                           
      EFF = TLUP(ID(IRCD+10))                                                   
      D(IRCD-6) = EFF                                                           
      IF(D(IH+NSI).LE.HS) GO TO 4                                               
      DH = EFF * (D(IH+NSI)-HS)                                                 
      DW = DH * D(IW+NLI) / (1. + D(IH+NSI))                                    
    4 D(IW+NLO1) = D(IW+NLI) - DW                                               
      D(IW+NLO2) = DW                                                           
      D(IH+NSO1) = D(IH+NSI) - DH                                               
      IF (IE.EQ.2) GO TO 5                                                      
      IF (D(IH+NSI).EQ.0.0) GO TO 5                                             
      CALL TDB2(NLI,NSI,NSI,HS)                                                 
      CALL TDB5(NLO2,NSO2,NSO2,HS)                                              
      CALL TDB6(NLO1,NSO1,NSO1,HS)                                              
    5 IF (IFP.NE.1 .OR. ICPP.NE.0) GO TO 99                                     
      CALL PIOP(-1,NLI,NSI,NSI)                                                 
      CALL PIOP(-2,NLO1,NSO1,NSO1)                                              
      IF (D(IH+NSI).NE.0.0.AND.D(IH+NSO1).GT.HS) CALL HSOP(HS)                  
      CALL PIOP(-2,NLO2,NSO2,NSO2)                                              
      CALL LINES(2)                                                             
      WRITE (OUT,1000) EFF                                                      
 1000 FORMAT (1H0,5X,3HEFF,F7.4  )                                              
   99 CONTINUE                                                                  
      RETURN
c     WSEPPP                                                                    
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE READS, CHECKS AND SORTS THE DATA FOR THE                   
C      PERFORMANCE OF A WATER SEPERATOR                                         
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE WSEPPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151)), (IWPT,C(89))                
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NLI), (SCR(2),NSI), (SCR(3),NLO1)                     
     *, (SCR(4),NSO1), (SCR(5),NLO2), (SCR(6),NSO2), (SCR(7),NF)                
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 INLET LEG NO                                                              
C   3 INLET STATION NO                                                          
C   4 OUTLET LEG NO                                                             
C   5 OUTLET STATION NO                                                         
C   6 DRAIN LEG NO                                                              
C   7 DRAIN STATION NO                                                          
C   8 OPTION                                                                    
C   9 PRESSURE DROP TABLE NO                                                    
C  10 EFF TABLE NO                                                              
      IE = 1                                                                    
      GO TO 1                                                                   
      ENTRY DRNPZ                                                               
      IE = 2                                                                    
    1 I = IACDB(10)                                                             
      ID(I+1) = ICV(1)                                                          
      NLI = ILEGN(ICV(2))                                                       
      ID(I+2) = NLI                                                             
      CALL LEGRT(NLI)                                                           
      NSI = ISTAN(ICV(3))                                                       
      ID(I+3) = NSI                                                             
      CALL START(NSI)                                                           
      NLO1 = ILEGN(ICV(4))                                                      
      ID(I+4) = NLO1                                                            
      CALL LEGRS(NLO1)                                                          
      NSO1 = ISTAN(ICV(5))                                                      
      ID(I+5) = NSO1                                                            
      CALL STARS(NSO1)                                                          
      NLO2 = ILEGN(ICV(6))                                                      
      ID(I+6) = NLO2                                                            
      CALL LEGRS(NLO2)                                                          
      NSO2 = ISTAN(ICV(7))                                                      
      ID(I+7) = NSO2                                                            
      CALL STARS(NSO2)                                                          
      CALL FTL(NLI,IFTA)                                                        
      CALL FRR(NLI,NF)                                                          
      CALL FRS(NLO1,NF)                                                         
      CALL FLUIDZ(NLO2,1,IWPT)                                                  
      IF (IE.EQ.2) GO TO 2                                                      
      ID(I+8) = ICV(8)                                                          
      ID(I+9) = ITIDN(ICV(9),1)                                                 
      ID(I+10) = ITIDN(ICV(10),17)                                              
      GO TO 99                                                                  
    2 ID(I+10) = ITIDN(ICV(8),17)                                               
   99 CONTINUE                                                                  
      RETURN                                                                    
C     WSEPPZ,DRNPZ                                                              
      END                                                                       
C                                                                               
C**********************************************************************         
C                                                                               
C    THIS SUBROUTINE PERFORMS THE SIZING ANALYSIS FOR                           
C      A WATER SEPERATOR                                                        
C                                                                               
C**********************************************************************         
C                                                                               
      SUBROUTINE WSEPSP                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (IRCD,C(45)), (SCR(1),C(151)), (IW,C(27))                     
     *, (IP,C(28)), (IT,C(29)),  (WTC,C(102)), (CUC,C(104))                     
     *, (RIC,C(106)), (DRC,C(108)), (WTIC,C(109)), (WTDC,C(111))                
     *, (OUT,C(7))                                                              
     *, (IGA,C(35))                                                             
      DIMENSION SCR(30)                                                         
      INTEGER OUT                                                               
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO), (SCR(4),WTF)         
     *, (SCR(5),CUF), (SCR(6),RI), (SCR(7),DRF), (SCR(8),VOL)                   
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
      IRCD = IRCDB(8)                                                           
      NL = ID(IRCD+2)                                                           
      NSI = ID(IRCD+3)                                                          
      NSO = ID(IRCD+4)                                                          
      WTF = D(ID(IRCD+5))                                                       
      CUF = D(ID(IRCD+6))                                                       
      RI = D(ID(IRCD+7))                                                        
      DRF = D(ID(IRCD+8))                                                       
      WTC = 0.0936*D(IW+NL)                                                     
      IF (WTF.NE.0.0) WTC = WTC*WTF                                             
      CUC = 18.82+2.08*WTC                                                      
      IF (CUF.NE.0.0) CUC = CUC*CUF                                             
      RIC = 0.00285                                                             
      IF (RI.NE.0.0) RIC = RI                                                   
      DRC = 1.0                                                                 
      IF (DRF.NE.0.0) DRC = DRC*DRF                                             
      IF (DRC.GT.10.0) DRC = 10.0                                               
      WTIC = 0.205*WTC                                                          
      WTDC = 0.173*WTC                                                          
      VOL = 2.11*D(IW+NL)**1.5                                                  
      CALL SSA                                                                  
      CALL SCO                                                                  
      CALL SCI(NL,NSI,NSO)                                                      
      CALL LINES(2)                                                             
      WRITE (OUT,1000) VOL                                                      
 1000 FORMAT (1H0,5X,3HVOL,F7.0)                                                
      D(IGA+71) = VOL                                                           
      RETURN                                                                    
C     WSEPSP                                                                    
      END                                                                       
*DECK,WSEPSZ                                                                    
      SUBROUTINE WSEPSZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (ICV(1),C(133)), (SCR(1),C(151))                              
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL)                                                   
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/010/                                                            
C   1 COMP CODE                                                                 
C   2 LEG                                                                       
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 WTF                                                                       
C   6 CUF                                                                       
C   7 RI                                                                        
C   8 DRF                                                                       
      I =  IACDB(8)                                                             
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
      RETURN                                                                    
C     WSEPSZ                                                                    
      END                                                                       
C$    NEW PER COSTELLO...    DID NOT EXIST IN ORIGINAL FORTRAN                  
      SUBROUTINE XPNDPP                                                         
      REAL K,N,N1,J,EFFM,N0                                                     
      INTEGER OUT,PASS                                                          
      COMMON/CC/C(600)                                                          
      COMMON/DC/DZ(2),D(128001)                                                 
      DIMENSION ID(2)                                                           
      DIMENSION SCR(30)                                                         
      EQUIVALENCE (IRCD,C(45)),(IW,C(27)),(IP,C(28)),(IT,C(29)),                
     *(IH,C(30)),(SCR(1),C(151)),(ICPP,C(88)),(OUT,C(7)),(IEVT,C(48)),          
     *(IFP,C(22)),(IGA,C(35)),(ILN,C(37)),(PASS,C(17)),(IFB,C(55)),             
     *(CERR,C(16)),(IIOP,C(90)),(GC,C(370)),(J,C(371)),(IEV,C(49)),             
     *(ITAD,C(54)),(ITN,C(39))                                                  
      EQUIVALENCE (NL,SCR(2)),(NSI,SCR(3)),(NSO,SCR(4)),(NST,SCR(5)),           
     *(IOPT,SCR(6)),(PR,SCR(7)),(FLO,SCR(8)),(EF,SCR(9)),(ME,SCR(10)),          
     *(D3,SCR(11)),(AO,SCR(12)),(Z,SCR(13)),(KN,SCR(14)),(ITER,SCR(30))         
      EQUIVALENCE (ID(1),D(1))                                                  
C   1 COMP CODE                                                                 
C   2 LEG NUMBER                                                                
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 SHAFT NUMBER                                                              
C   6 OPTION                                                                    
C   7 PRESSURE RATIO                                                            
C   8 FLOW FACTOR TABLE                                                         
C   9 EFFICIENCY TABLE                                                          
C  10 MECHANICAL EFFICIENCY                                                     
C  11 DISPLACEMENT PER REVOLUTION                                               
C  12 OUTLET AREA RATIO                                                         
C  13 OPTION 0-DIMENSIONED 1-DIMENSIONLESS TABLES                               
C  14 EXPONENT IN PRESSURE NORMALIZING EQUATION                                 
      IRCD=IRCDB(15)                                                            
C  15 ERROR VARIABLE INDEX                                                      
      NL=ID(IRCD+2)                                                             
      NSI=ID(IRCD+3)                                                            
      NSO=ID(IRCD+4)                                                            
      NST=ID(IRCD+5)                                                            
      IOPT=ID(IRCD+6)                                                           
      PR=D(ID(IRCD+7))                                                          
      EFFM=D(ID(IRCD+10))                                                       
      D3=D(ID(IRCD+11))                                                         
      AO=D(ID(IRCD+12))                                                         
      Z=ID(IRCD+13)                                                             
      KN=D(ID(IRCD+14))                                                         
      JEVI=ID(IRCD+15)                                                          
      LOCT=ID(IFB+NL)                                                           
      LOCT=ID(LOCT+3)                                                           
      QUAL=VQUALH('XPNDPP  ',LOCT,D(IP+NSI),D(IH+NSI))                          
      ITER=0                                                                    
      N=D(IGA+NST+80)                                                           
      FR=D(IW+NL)                                                               
      IF(QUAL)55,55,60                                                          
C*** IF PURE LIQUID ENTERS THE EXPANDER, SET OUTLET = INLET                     
   55 WRITE(OUT,111)                                                            
  111 FORMAT(' PURE LIQUID IS ENTERING THE EXPANDER')                           
      D(IP+NSO)=D(IP+NSI)                                                       
      D(IT+NSO)=D(IT+NSI)                                                       
      D(IH+NSO)=D(IH+NSI)                                                       
      RETURN                                                                    
   60 IF(QUAL-1.0)65,100,100                                                    
C*** TWO-PHASE FLOW ENTERING THE EXPANDER                                       
   65 VI=1.0/VDL(LOCT,D(IT+NSI))+QUAL*                                          
     *                     (VSV('XPNDPP 1',LOCT,D(IP+NSI),D(IT+NSI))            
     * -1.0/VDL(LOCT,D(IT+NSI)))                                                
      HFG=VHFG(LOCT,D(IP+NSI),D(IT+NSI),VI)                                     
      SSAT=VS(LOCT,D(IT+NSI),VI)                                                
      SI=SSAT-(1.-QUAL)*HFG/D(IT+NSI)                                           
      GO TO 200                                                                 
C*** PURE VAPOR ENTERING THE EXPANDER                                           
  100 VI=VSV('XPNDPP 2',LOCT,D(IP+NSI),D(IT+NSI))                               
      SI=VS(LOCT,D(IT+NSI),VI)                                                  
C*** COMPUTE THE SPEED OF SOUND BASED ON VAPOR                                  
  200 AI=VSOS(LOCT,D(IP+NSI),D(IT+NSI))*60.                                     
  310 DM=D3**(1./3.)                                                            
      DN=N                                                                      
  320 IF(IOPT-1)550,770,960                                                     
C*** PRESSURE RATIO AND FLOW RATE ARE FIXED; SHAFT SPEED IS KNOWN               
  550 D(IP+NSO)=PR*(D(IP+NSI))                                                  
      QX=FR*VI                                                                  
      GO TO 620                                                                 
  610 FR=QX/VI                                                                  
C*** USE TLUP FOR EFFICIENCY AS A FUNCTION OF CFM AND RPM                       
  620 D(IGA+101)=QX                                                             
      D(IGA+102)=DN                                                             
      EF=TLUP(ID(IRCD+9))                                                       
C*** DETERMINE THE OUTLET CONDITIONS BASED ON ISENTROPIC EXPANSION              
      TSAT=VTS(LOCT,D(IP+NSO))                                                  
      VSAT=VSV('XPNDPP 3',LOCT,D(IP+NSO),TSAT)                                  
      SSAT=VS(LOCT,TSAT,VSAT)                                                   
      HSAT=VH(LOCT,D(IP+NSO),TSAT,VSAT)                                         
      HFG=VHFG(LOCT,D(IP+NSO),TSAT,VSAT)                                        
      SFG=HFG/TSAT                                                              
      QUAL=((SI-SSAT)/SFG)+1.                                                   
      IF(QUAL.GE.1.0) GO TO 650                                                 
      VO=(1.0/VDL(LOCT,TSAT))+QUAL*(VSAT-1.0/VDL(LOCT,TSAT))                    
      HX=HSAT-(1.0-QUAL)*HFG                                                    
      GO TO 660                                                                 
  650 CALL VTAV1(LOCT,D(IT+NSO),VO,D(IP+NSO),SI)                                
      HX=VH(LOCT,D(IP+NSO),D(IT+NSO),VO)                                        
C*** COMPUTE THE OUTLET CONDITIONS BASED ON THE EXPANDER EFFICIENCY             
  660 HT=HX                                                                     
  670 D(IH+NSO)=D(IH+NSI)-EF*(D(IH+NSI)-HX)-(FR*VO/(DM*DM*AO*60.))**2/          
     * (2.*GC*J)                                                                
      HSAT=VH(LOCT,D(IP+NSO),TSAT,VSAT)                                         
      QUAL=1.0+(D(IH+NSO)-HSAT)/HFG                                             
      IF(QUAL.GE.1.0) GO TO 500                                                 
      D(IT+NSO)=TSAT                                                            
      VO=1.0/VDL(LOCT,D(IT+NSO))+QUAL*((VSAT)-1./VDL(LOCT,D(IT+NSO)))           
      GO TO 600                                                                 
  500 CALL VTAV2(LOCT,D(IT+NSO),VO,D(IP+NSO),D(IH+NSO))                         
  600 CONTINUE                                                                  
C*** ITERATE ON THE OUTLET-VELOCITY CORRECTION                                  
  700 IF(ABS((D(IH+NSO)-HT)/(D(IH+NSO)-D(IH+NSI))).LT..0001)GO TO 710           
      HT=D(IH+NSO)                                                              
      GO TO 670                                                                 
  710 FR=QX/VI                                                                  
C*** COMPUTE THE POWER OUTPUT, W                                                
      W=FR*EF*(D(IH+NSI)-HX)                                                    
      D(IGA+NST+90)=D(IGA+NST+90)+W*778./33000.                                 
      IF(IOPT.EQ.2) GO TO 750                                                   
      GO TO 1130                                                                
  750 CONTINUE                                                                  
      IF(PASS.NE.1) GO TO 752                                                   
      ID(IEVT+JEVI)=7                                                           
  752 D(IEV+JEVI)=(DN-D(NST+80+IGA))/(DN+D(NST+80+IGA))                         
  760 GO TO 1130                                                                
C*** RPM AND CFM ARE KNOWN. USE STRADDLING PROCEDURE TO GET PRESSURE            
C    RATO FROM TLUP AS A FUNCTION OF RPM AND PRESSURE RATIO                     
  770 QX=FR*VI                                                                  
      I=1                                                                       
      JD=1                                                                      
      KY=0                                                                      
      JN=ID(IRCD+8)                                                             
      IAD=ID(ITAD+JN)                                                           
      NX=ID(IAD+4)                                                              
      IX=ID(IAD+8)                                                              
      IND=0                                                                     
      D(IGA+102)=DN                                                             
      NZ=ID(IAD+7)                                                              
      IZ=ID(IAD+9)                                                              
C*** TESTTO SEE IF THE HIGHEST FLOW RATE IS HIGHER THAN THE                     
C    LEG FLOW RATE (AT ZERO PRESSURE DROP)                                      
      D(IGA+101)=0.0                                                            
      Q9=TLUP(JN)                                                               
      IF(Q9.GT.QX) GO TO 780                                                    
      WRITE(OUT,3130)                                                           
      GO TO 1140                                                                
  780 IF(ID(IAD+1).EQ.3) GO TO 785                                              
      IF(ID(IAD+1).EQ.33) GO TO 781                                             
      WRITE(OUT,3133)                                                           
 3133 FORMAT(' *+*WRONG TABLE FOR XPANDR. MUST BE 3 DIMENSIONAL')               
      CERR=CERR+1                                                               
      RETURN                                                                    
C*** START THE SCAN FOR THE STRADDLING POINTS.  FIND THE INDEX (IX)             
C    OF THE LOWEST PRESSURE RATIO IN THE TABLE                                  
  781 CALL MDISSR(N,D(IZ),I,NZ,JD,NPX,KY,IND)                                   
      IX=IX+(NX*NPX)                                                            
  785 IENDX=IX+NX-1                                                             
C*** SCAN THE VALUES OF THE PRESSURE RATIOS FOR THE ONE THAT GIVES A            
C    FLOW JUST BELOW THE LEG FLOW (QX)                                          
      DO 800 I=IX,IENDX                                                         
      D(IGA+101)=D(I)                                                           
      YVAL=TLUP(JN)                                                             
      IF(YVAL.GT.QX)GO TO 790                                                   
C*** SET UP THE STRADDLE POINT BETWEEN (P1,Q1) AND (P2,Q2).  NOTE THAT          
C    Q IS ASSUMED TO DECREASE AS THE PR (P) INCREASES.                          
      Q1=YVAL                                                                   
      P1=D(I)                                                                   
      P9=D(I-1)                                                                 
      IF(I.GT.IX) GO TO 810                                                     
      P9=0.0                                                                    
      GO TO 810                                                                 
  790 Q9=YVAL                                                                   
  800 CONTINUE                                                                  
      WRITE(OUT,3130)                                                           
 3130 FORMAT(' *+* NO BRACKET IN TABLE FOR QX')                                 
      GO TO 1140                                                                
  810 P9MP1=P9-P1                                                               
      P1HLD=P1                                                                  
  830 IF(Q9-Q1)832,1140,832                                                     
C*** INTERPOLATE LINEARLY BETWEEN THE STRADDLE POINTS TO DETERMINE BY           
C    ITERATION THE CORRECT PRESSURE RATIO.                                      
  832 PX=P1+(QX-Q1)*(P9-P1)/(Q9-Q1)                                             
      IF(PX.GT.1.0) GO TO 1140                                                  
  840 D(IGA+101)=PX                                                             
      Q0=TLUP(ID(IRCD+8))                                                       
  855 IF(ABS((QX-Q0)/QX).LT..0001) GO TO 920                                    
  860 IF(QX-Q0) 870,870,885                                                     
  870 P9=PX                                                                     
      Q9=Q0                                                                     
      GO TO 900                                                                 
  885 P1=PX                                                                     
  887 Q1=Q0                                                                     
  900 ITER=ITER+1                                                               
      IF(ITER-100) 832,832,1140                                                 
C*** AT 920 THE SOLUTION HAS BEEN FOUND, SO RETURN TO 620 TO                    
C    DETERMINE THE OUTLET CONDITIONS.                                           
  920 D(IP+NSO)=D(IP+NSI)*PX                                                    
  930 GO TO 610                                                                 
C*** PRESSURE RATIO AND LEG FLOW ARE KNOWN.  DETERMINE THE SHAFT SPEED          
C    REQUIRED TO GIVE THESE.  USE NEWTON'S METHOD WITH A PERTURBATION           
C    INSTEAD OF DIFFERENTIATION.                                                
  960 PX=PR                                                                     
  990 QX=FR*VI                                                                  
 1000 D(IGA+101)=PX                                                             
      D(IGA+102)=DN                                                             
      Q0=TLUP(ID(IRCD+8))                                                       
 1010 XX=ABS((Q0-QX)/QX)                                                        
 1015 IF(XX-.0001)1090,1090,1020                                                
 1020 DN1=1.001*DN                                                              
      D(IGA+102)=DN1                                                            
      Q1=TLUP(ID(IRCD+8))                                                       
 1050 IF(Q1-Q0) 1052,1140,1052                                                  
C$      THE FIRST "+" IN THE EQN BELOW WAS A "*" IN NEWZ                        
 1052 DN=DN+(DN1-DN)*(QX-Q0)/(Q1-Q0)                                            
      IF(DN.LE.0.0.OR.DN.GT.1.E+20) GO TO 1140                                  
      ITER=ITER+1                                                               
      IF(ITER-100)1000,1000,1140                                                
C$ THE STATEMENT BELOW WAS UNREACHABLE ANYWAY                                   
C$ 1070 GO TO 1000                                                              
C*** ITERATION IS COMPLETE, SO GO TO 610 TO DETERMINE THE REMAINDER             
C    OF THE OUTLET CONDITIONS.                                                  
 1090 D(IP+NSO)=D(IP+NSI)*PX                                                    
 1120 GO TO 610                                                                 
 1140 WRITE(OUT,1142)                                                           
 1142 FORMAT(' SOLUTION IS PHYSICALLY IMPOSSIBLE')                              
 1150 WRITE(OUT,1152)QX                                                         
 1152 FORMAT(' VOLUME FLOW RATE, IN CFM, WAS',F9.2)                             
 1160 WRITE(OUT,1162)PX                                                         
 1162 FORMAT(' PRESSURE RATIO WAS',F9.2)                                        
 1170 WRITE(OUT,1172) N                                                         
 1172 FORMAT(' RPM WAS',F9.2)                                                   
      D(IP+NSO)=D(IP+NSI)                                                       
      D(IT+NSO)=D(IT+NSI)                                                       
      D(IH+NSO)=D(IH+NSI)                                                       
 1130 IF(IFP.NE.1 .OR. ICPP.NE.0 ) GO TO 99                                     
      CALL PIOP(1,NL,NSI,NSO)                                                   
      CALL LINES(2)                                                             
      WRITE(OUT,1173) W                                                         
 1173 FORMAT(5X,'EXPANDER OUTPUT =',E17.7,'BTU/MIN')                            
   99 CONTINUE                                                                  
 2000 RETURN                                                                    
C     XPNDPP                                                                    
      END                                                                       
      SUBROUTINE XPNDPZ                                                         
      COMMON /CC/ C(600)                                                        
      EQUIVALENCE (OUT,C(7)),(CERR,C(16)),(ICV(1),C(133)),                      
     * (SCR(1),C(151))                                                          
      INTEGER CERR,OUT                                                          
      DIMENSION ICV(18), SCR(30)                                                
      EQUIVALENCE (SCR(1),NL), (SCR(2),NSI), (SCR(3),NSO)                       
     *, (SCR(4),IPRO), (SCR(5),IPRT)                                            
      COMMON /DC/ DZ(2),D(128001)                                               
      DIMENSION ID(128001)                                                      
      EQUIVALENCE (ID(1),D(1))                                                  
      DATA IFTA/111/                                                            
C   1 COMP CODE                                                                 
C   2 LEG NO                                                                    
C   3 STATION IN                                                                
C   4 STATION OUT                                                               
C   5 SHAFT NO                                                                  
C   6 OPTION                                                                    
C   7 PRESSURE RATIO                                                            
C   8 FLOW FACTOR TABLE NO                                                      
C   9 EFFICIENCY TABLE NO                                                       
C  10 MECHANICAL EFFICIENCY                                                     
C  11 DISPLACEMENT PER REVOLUTION (CUBIC FEET)                                  
C  12 OUTLET AREA RATIO                                                         
C  13 OPTION - 0 = DIMESIONED TABLES: 1 = DIMENSIONLESS                         
C  14 EXPONENT IN PRESSURE NORMALIZING EQUATION                                 
C  15 ERROR VARIABLE FOR OPTION 2                                               
      I = IACDB(15)                                                             
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
      CALL SRT(ICV(5))                                                          
      ID(I+6) = ICV(6)                                                          
      IF(ICV(6).NE.2)GO TO 5                                                    
      ID(I+15)=IAEV(7)                                                          
    5 ID(I+7) = IPARM(ICV(7))                                                   
      ID(I+8)=ITIDN(ICV(8),8)                                                   
      ID(I+9) = ITIDN(ICV(8),7)                                                 
      ID(I+10) =IPARM(ICV(9))                                                   
      ID(I+11)=IPARM(ICV(10))                                                   
      ID(I+12)=IPARM(ICV(11))                                                   
      ID(I+13)=ICV(12)                                                          
      ID(I+14)=IPARM(ICV(13))                                                   
      IF(ID(I+13)-1) 69,7,12                                                    
    7 IF(ID(I+14).NE.0) GO TO 69                                                
   12 CERR = CERR + 1                                                           
      CALL LINES(2)                                                             
      WRITE(OUT,1000)CERR                                                       
 1000 FORMAT(' ERROR',I6,' TABLE DIMENSIONLESS KN MUST NOT BE 0')               
   69 RETURN                                                                    
C     XPNDPZ                                                                    
      END                                                                       

C*   NORTHROP GRUMMAN PROPRIETARY