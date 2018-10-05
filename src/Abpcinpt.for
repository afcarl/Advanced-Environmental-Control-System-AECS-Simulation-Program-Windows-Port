C*   NORTHROP GRUMMAN PROPRIETARY
C@ AT FIRST OPP, SUB TABR FROM CRAY (ALREADY HAVE PTTL FROM CRAY) 
C*    BLOCK DATA
      BLOCK DATA BD1
      COMMON /CC/ C(600)
      DIMENSION CD(200)
      REAL*8  CD
      EQUIVALENCE (CD(1),C(201))
      EQUIVALENCE (IPB,C(2)), (ISB,C(3)), (IDD,C(4)), (IDS,C(5))
     *, (IN,C(6)), (OUT,C(7)), (SEQCN,C(11)), (LCL,C(12)), (LC,C(13))
     *, (IENDJ,C(14)), (IDIM,C(24)), (IW,C(27)), (IFB,C(55))
     *, (IP,C(28)), (IT,C(29)), (IH,C(30)), (BIG,C(31)), (IGA,C(35))
     *, (NGA,C(36)), (ILN,C(37)), (ISN,C(38)), (MNTAB,C(41))
     *, (ISP,C(42)), (IPAU1,C(56)), (IPAU2,C(57)), (IPAU3,C(58))
     *, (NOCP,C(59)), (ISAU1,C(60)), (ISAU2,C(61)), (ISAU3,C(62))
     *, (NOCS,C(63)), (P0,C(64)), (T0,C(65)), (INP,C(68)), (PL,C(80))
C    *, (TL,C(81)), (NIT,C(76)), (PF,C(77)), (TLM,C(120))
C    *, (ERRL,C(83)), (PUN,C(91)), (CPV,C(132))
C    *, (SVLL(1),C(181)), (SVUL(1),C(191))
C    *, (ALRS,C(125)), (ALCTS,C(126)), (ALTTS,C(127))
     *,(TL,C(81)),(NIT,C(76)),(PF,C(77)),(TLM,C(120)),(MRDNIT,C(373))
     *,(ERRL,C(83)),(PUN,C(91)),(CPV,C(132)),(GC,C(370))
     *,(SVLL(1),C(181)),(SVUL(1),C(191)),(XJOULE,C(371))
     *,(ALRS,C(125)),(ALCTS,C(126)),(ALTTS,C(127)),(MRD,C(372))
      DIMENSION SVLL(10), SVUL(10)
      INTEGER OUT, PUN, SEQCN
C
       REAL*8  PRFM,   SIZE,   ENDJ,   PCNG,   SCNG,   NOGO,
     1           PDUM,   QCNG,   TITEND, CASEND, PAREND, COMEND,
     2           TABEND, ENDCA,  COMMT,  CHANGE, BLANKS, GRPH,
     3           TITLE,  CASEA,  CASEB,  CASEC,  CASED,  PARAM,
     4           VALUE,  VALUES, CMPNNT, TABID,  TABT,   TABV,
     5           PTAB,   STAB,   ENDCAS, GTITL
C
      EQUIVALENCE (PRFM,CD(1)), (SIZE,CD(2)), (ENDJ,CD(3)), (PCNG,CD(4))
     *, (SCNG,CD(5)), (NOGO,CD(6)), (PDUM,CD(7))
     *, (QCNG,CD(8))
     *, (TITEND,CD(11)), (CASEND,CD(12)), (PAREND,CD(13))
     *, (COMEND,CD(14)), (TABEND,CD(15)), (ENDCA,CD(16))
     *, (COMMT,CD(21)), (CHANGE,CD(22)), (BLANKS,CD(23))
     *, (GRPH,CD(24))
     *, (TITLE,CD(31)), (CASEA,CD(32)), (CASEB,CD(33)), (CASEC,CD(34))
     *, (CASED,CD(35)), (PARAM,CD(36)), (VALUE,CD(37)), (VALUES,CD(38))
     *, (CMPNNT,CD(39)), (TABID,CD(40)), (TABT,CD(41)), (TABV,CD(42))
     *, (PTAB,CD(43)), (STAB,CD(44)), (ENDCAS,CD(45))
     *, (GTITL(1),CD(72))
      DIMENSION GTITL(10)
      COMMON /DC/ DZ(2), D(128001)
      EQUIVALENCE (ISP0,C(69)), (NDC(1),C(70)), (LLU(1),C(73))
      DIMENSION NDC(3), LLU(3)
      COMMON /CCA/ CA(150)
      EQUIVALENCE (IOMSET,CA(7))
C*     LCL WAS 55,NOW 90 
      DATA IPB/0/, ISB/0/, IDD/0/, IDS/0/, IN/5/, OUT/6/, SEQCN/0/
     *, LCL/90/, LC/0/, IENDJ/0/, IDIM/0/, IW/0/, IP/0/, IT/0/
C*     *, IH/0/, BIG/1.E75/, IGA/0/, NGA/120/, ILN/0/, ISN/0/
     *, IH/0/, BIG/3.40E+38/, IGA/0/, NGA/120/, ILN/0/, ISN/0/
     *, MNTAB/100/, ISP/0/, IPAU1/8/, IPAU2/9/, IPAU3/10/, NOCP/100/
     *, ISAU1/8/, ISAU2/11/, ISAU3/10/, NOCS/100/, P0/14.7/, T0/519.0/
     *, INP/1/, PL/0.0001/, TL/20.0/, NIT/ 25/, PF/1.001 /, TLM/960.0/
C$ TL WAS 300.
C$   *, IFB/0/, ERRL/0.01/, PUN/7/, CPV/0.5/
C$   *, ALRS/60000.0/, ALCTS/1500.0/, ALTTS/2000.0/
     *, IFB/0/, ERRL/0.01/, PUN/7/, CPV/0.5/,GC/32.174/,XJOULE/778.28/
     *, ALRS/60000.0/, ALCTS/1500.0/, ALTTS/2000.0/,MRD/0/,MRDNIT/3/
C$	SVUL IN 1ST POSITION IS TYPE 1 FLOW WAS 1.E-3 NOW 1.E-4
C$	SVUL IN 9TH POSITION IS TYPE 9 ORIF PRESS RATIO WAS .999 NOW .9999
C$	SVUL RESTORED TO .999
      DATA SVLL/1.E-4,1.E-6,1.E-3,1.E-12,1.E-3,1.E-12,1.E-3,1.E-3,1.E-6
     *,1.E-12/
      DATA SVUL/1.E6,1.E6,1.E6,0.999,0.999,1.E12,1.E6,1.E6,0.999,1.E12/
      DATA PRFM/6HPERFOR/, SIZE/4HSIZE/, ENDJ/6HENDJOB/, PCNG/6HPCHANG/
     *, SCNG/6HSCHANG/, NOGO/4HNOGO/, PDUM/6HPDUMMY/
     *, QCNG/6HQCHANG/
     *, TITEND/6HTITEND/, CASEND/6HCASEND/, PAREND/6HPAREND/
     *, COMEND/6HCOMEND/, TABEND/6HTABEND/, ENDCA/6HENDCAS/
     *, COMMT/6H******/, CHANGE/6HCHANGE/, BLANKS/6H      /
     *, GRPH/6HGRAPHI/
     *, TITLE/5HTITLE/, CASEA/5HCASEA/, CASEB/5HCASEB/, CASEC/5HCASEC/
     *, CASED/5HCASED/, PARAM/5HPARAM/, VALUE/5HVALUE/, VALUES/6HVALUES/
     *, CMPNNT/6HCMPNNT/, TABID/5HTABID/, TABT/4HTABT/, TABV/4HTABV/
     *, PTAB/4HPTAB/, STAB/4HSTAB/, ENDCAS/6HENDCAS/
     *, GTITL/8HGENERAL ,8HECS PROG,8HRAM     ,7*8H        /
C$$$   CHANGED BACK TO 32001 SINCE D ABOVE IS STILL DIMENSIONED TO 32001
C*    DATA DZ/2*0.0/, D/32001*0.0/
C***  COMMENTED OUT TO RUN ON PC   NOT NEEDED FOR PC
C@      DATA DZ/2*0.0/, D/16383*0.0,15618*0.0/
      DATA DZ/2*0.0/, D/128001*0.0/
C$$$      DATA DZ/2*0.0/, D/3201*0.0/
      DATA ISP0/101/, NDC/5001,30001,32001/, LLU/3*0/
      DATA IOMSET/0/
C     BLOCK DATA CC + DC
      END
C*    BLOCK DATA
      BLOCK DATA BD2
      COMMON /CP/ PD(100)
      REAL*8    PD
      DATA PD/5HUSERP,5HINLET,6HOUTLET,5HSPLIT,5HMERGE,3HHXA,5HVALVE
     1, 6HCVALVE, 6HSENSOR, 4HLINE, 4HCOMP, 4HTURB, 5HSHAFT, 6HSPOWER
     2, 5HQLOAD, 4HPREG, 6HNOZZLE, 4HORIF, 4HHXB1, 4HHXB2, 4HMISC
     3, 4HPUMP, 3HFAN, 3HAPU, 5HEJECT, 4HDSEP, 6HBOILER, 4HWSEP
     4, 6HINJECT, 5HLOOPS, 5HLOOPE, 5HCNNCT, 5HDRAIN, 5HVLINE, 5HVCOMP
C$   5, 4HCOND, 4HEVAP, 3HCOS, 3HCOE, 4HCLSV, 4HCLEV, 3HFMS, 3HCCS, 2HUA
C$   6, 3HCAW, 3HCTS, 6HPRINTA, 4HFUNC, 6HPRINTB
C$   *, 50*8H$$$$$$$$, 6H$$$$$$/
     5, 4HCOND, 4HEVAP,  5HXPAND, 6HQBOILR, 5HNRCLR, 4HRPMP, 6HRINLET
     6, 3HCCS, 6HROUTLT, 5HSEPAR, 3HCTS, 4HSOLN, 4HFUNC,6HPRINTB
C$ ROUTINES ADDED
     *, 6HINDATA, 3HCOP, 4HREFP, 4HFLUP, 46*8H$$$$$$$$, 6H$$$$$$/
C     BLOCK DATA CP
      END
C*    BLOCK DATA
      BLOCK DATA BD3
      COMMON /CS/ SD(100)
      REAL*8    SD
      DATA SD/5HUSERS, 5HVALVE, 4HDSEP, 4HWSEP, 5HEJECT, 3HAPU, 3HRIN
     1, 4HROUT, 2HHX, 3HHX1, 3HHX2, 4HLINE, 5HCOND1, 5HCOND2, 5HEVAP1
     2, 5HEVAP2, 5HBOIL1, 5HBOIL2, 6HHEATER, 4HINIT, 4HMISC, 4HCOMP
     3, 4HTURB, 4HPUMP, 3HFAN, 5HVCOMP, 5HCNTRL, 6HINSLTN, 4HORIF
     *, 70*8H$$$$$$$$, 6H$$$$$$/
C     BLOCK DATA CS
      END
C     BLOCK DATA CP
C     END
      SUBROUTINE GDCC (NUM,LEN,ISP,DCV,LOC,NMIN,NRET)                   00042000
      COMMON /CC/ C(600)                                                00042010
      EQUIVALENCE (NDC(1),C(70)), (LLU(1),C(73)), (OUT,C(7))            00042020
      DIMENSION NDC(3), LLU(3)                                          00042030
      INTEGER OUT                                                       00042040
      COMMON /DC/ DZ(2),D(128001)                                            00042050
      DIMENSION ID(2)                                                   00042060
      EQUIVALENCE (ID(1),D(1))                                          00042070
      NA = NDC(ISP+1)-NDC(ISP)-LLU(ISP+1)                               00042080
      IF (NA.LT.NMIN) GO TO 97                                          00042090
      NRET = NUM                                                        00042100
      IF (NRET.GT.NA) NRET = NA                                         00042110
      LOC = LLU(ISP+1)+NDC(ISP)                                         00042120
      LLU(ISP+1) = LLU(ISP+1)+NRET                                      00042130
      DO 2 I=1,NRET                                                     00042140
    2 ID(LOC+I) = 0                                                     00042150
      GO TO 99                                                          00042160
   97 CONTINUE                                                          00042170
      CALL LINES(3)                                                     00042180
      WRITE (OUT,1000) ISP,NUM,NA                                       00042190
 1000 FORMAT(33H0DYNAMIC CORE OVERFLOW - SUBPOOL ,I4/11H REQUESTED ,I6, 00042200
     *10X,10HAVAILABLE ,I6)                                             00042210
c$*      CALL EXIT                                                         00042220
   99 RETURN                                                            00042230
C     GDCC                                                              00042240
      END                                                               00042250
      SUBROUTINE GDCU (NUM,LEN,ISP,DCV,LOC)                             00042260
      COMMON /CC/ C(600)                                                00042270
      EQUIVALENCE (NDC(1),C(70)), (LLU(1),C(73)), (OUT,C(7))            00042280
      DIMENSION NDC(3), LLU(3)                                          00042290
      INTEGER OUT                                                       00042300
      COMMON /DC/ DZ(2),D(128001)                                            00042310
      DIMENSION ID(2)                                                   00042320
      EQUIVALENCE (ID(1),D(1))                                          00042330
      IL = LLU(ISP+1)+NUM+NDC(ISP)                                      00042340
      IF (IL.GT.NDC(ISP+1)) GO TO 98                                    00042350
      LOC = LLU(ISP+1)+NDC(ISP)                                         00042360
      LLU(ISP+1) = LLU(ISP+1)+NUM                                       00042370
      DO 1 I=1,NUM                                                      00042380
    1 ID(LOC+I) = 0                                                     00042390
      GO TO 99                                                          00042400
      ENTRY FDC (NUM,LEN,ISP,DCV,LOC)                                   00042410
      LOC = 0                                                           00042420
      IF (NUM.EQ.0) GO TO 3                                             00042430
      LLU(ISP+1) = LLU(ISP+1)-NUM                                       00042440
      IF (ISP.EQ.0) LLU(1) = 0                                          00042450
      GO TO 99                                                          00042460
    3 LLU(ISP+1) = 0                                                    00042470
      GO TO 99                                                          00042480
      ENTRY LDC (NUM,LEN,ISP,DCV,LOC)                                   00042490
      NUM = NDC(ISP+1)-NDC(ISP)-LLU(ISP+1)                              00042500
      LOC = LLU(ISP+1)                                                  00042510
      GO TO 99                                                          00042520
   98 CONTINUE                                                          00042530
      NA = NDC(ISP+1)-NDC(ISP)-LLU(ISP+1)                               00042540
      CALL LINES(3)                                                     00042550
      WRITE (OUT,1000) ISP,NUM,NA                                       00042560
 1000 FORMAT(33H0DYNAMIC CORE OVERFLOW - SUBPOOL ,I4/11H REQUESTED ,I6, 00042570
     *10X,10HAVAILABLE ,I6)                                             00042580
c$*      CALL EXIT                                                         00042590
   99 RETURN                                                            00042600
C     gdcu  DC SUB                                                            00042610
      END                                                               00042620
      FUNCTION IASV (N)                                                 00054210
      COMMON /CC/ C(600)                                                00054220
      EQUIVALENCE (NSV,C(50)), (NEV,C(51)), (OUT,C(7)), (MDPC,C(34))    00054230
      INTEGER OUT                                                       00054240
      NSV = NSV+1                                                       00054250
      K = NSV                                                           00054260
      IF (MDPC.LT.0) GO TO 99                                           00054270
      CALL LINES(1)                                                     00054280
      WRITE (OUT,1001) NSV,N                                            00054290
 1001 FORMAT(104X,4HSV N,I4,3H  T,I4)                                   00054300
   99 CONTINUE                                                          00054310
      IASV = K                                                          00054320
      RETURN                                                            00054330
      ENTRY IAEV (N)                                                    00054340
      NEV = NEV+1                                                       00054350
      K = NEV                                                           00054360
      IF (MDPC.LT.0) GO TO 99                                           00054370
      CALL LINES(1)                                                     00054380
      WRITE (OUT,1002) NEV,N                                            00054390
 1002 FORMAT(104X,4HEV N,I4,3H  T,I4)                                   00054400
      IAEV = K                                                          00054410
      RETURN                                                            00054420
C     IASV, IAEV                                                        00054430
      END                                                               00054440
      FUNCTION ILEGN (N)                                                00054450
      COMMON /CC/ C(600)                                                00054460
      EQUIVALENCE (OUT,C(7)), (CERR,C(16)), (ILN,C(37)), (NUMLEG,C(25)) 00054470
     *, (ISN,C(38)), (NUMSTA,C(26)), (TYPE,C(1))                        00054480
      INTEGER OUT, CERR, TYPE                                           00054490
      COMMON /DC/ DZ(2),D(128001)                                            00054500
      DIMENSION ID(2)                                                   00054510
      EQUIVALENCE (ID(1),D(1))                                          00054520
      IF (N.GT.0) GO TO 1                                               00054530
    5 CERR = CERR+1                                                     00054540
      CALL LINES(2)                                                     00054550
      WRITE (OUT,1001) CERR,N                                           00054560
 1001 FORMAT(6H0ERROR,I6,5X,18HINVALID LEG NUMBER,I6)                   00054570
      K = 0                                                             00054580
      GO TO 99                                                          00054590
    1 IF (ILN.NE.0) GO TO 2                                             00054600
      CALL GDCU(NUMLEG,4,0,D,ILN)                                       00054610
      NLU = 0                                                           00054620
    2 IF (IABS(TYPE).EQ.2) NLU = NUMLEG                                 00054630
      IF (NLU.EQ.0) GO TO 4                                             00054640
      DO 3 I=1,NLU                                                      00054650
      K = I                                                             00054660
      IF (N.EQ.ID(ILN+I)) GO TO 99                                      00054670
    3 CONTINUE                                                          00054680
      IF (IABS(TYPE).EQ.2) GO TO 5                                      00054690
      IF (NLU.LT.NUMLEG) GO TO 4                                        00054700
      CERR = CERR+1                                                     00054710
      CALL LINES(2)                                                     00054720
      WRITE (OUT,1000) CERR,N                                           00054730
 1000 FORMAT(6H0ERROR,I6,5X,26HTOO MANY LEG NUMBERS - LEG,I6)           00054740
      K = 0                                                             00054750
      GO TO 99                                                          00054760
    4 NLU = NLU+1                                                       00054770
      ID(ILN+NLU) = N                                                   00054780
      K = NLU                                                           00054790
   99 ILEGN = K                                                         00054800
      RETURN                                                            00054810
      ENTRY ISTAN (N)                                                   00054820
      IF (N.GT.0) GO TO 11                                              00054830
   15 CERR = CERR+1                                                     00054840
      CALL LINES(2)                                                     00054850
      WRITE (OUT,1011) CERR,N                                           00054860
 1011 FORMAT(6H0ERROR,I6,5X,22HINVALID STATION NUMBER,I6)               00054870
      K = 0                                                             00054880
      GO TO 199                                                         00054890
   11 IF (ISN.NE.0) GO TO 12                                            00054900
      CALL GDCU(NUMSTA,4,0,D,ISN)                                       00054910
      NSU = 0                                                           00054920
   12 IF (IABS(TYPE).EQ.2) NSU = NUMSTA                                 00054930
      IF (NSU.EQ.0) GO TO 14                                            00054940
      DO 13 I=1,NSU                                                     00054950
      K = I                                                             00054960
      IF (N.EQ.ID(ISN+I)) GO TO 199                                     00054970
   13 CONTINUE                                                          00054980
      IF (IABS(TYPE).EQ.2) GO TO 15                                     00054990
      IF (NSU.LT.NUMSTA) GO TO 14                                       00055000
      CERR = CERR+1                                                     00055010
      CALL LINES(2)                                                     00055020
      WRITE (OUT,1010) CERR,N                                           00055030
 1010 FORMAT(6H0ERROR,I6,5X,34HTOO MANY STATION NUMBERS - STATION,I6)   00055040
      K = 0                                                             00055050
      GO TO 199                                                         00055060
   14 NSU = NSU+1                                                       00055070
      ID(ISN+NSU) = N                                                   00055080
      K = NSU                                                           00055090
  199 ISTAN = K                                                         00055100
      RETURN                                                            00055110
C     ILEGN, ISTAN                                                      00055120
      END                                                               00055130
      SUBROUTINE INPUTA                                                 00058110
      COMMON /CC/ C(600)                                                00058120
      EQUIVALENCE (CERR,C(16)), (OUT,C(7)), (TYPE,C(1)), (IRS,C(18))    00058130
     *, (IAUI,C(20)), (IFP,C(22)), (NCOMP,C(44))                        00058140
      INTEGER CERR, OUT, TYPE                                           00058150
      DIMENSION CD(200)                                                 00058160
      REAL*8    CD                                                      00058170
      EQUIVALENCE (CD(1),C(201))                                        00058180
      EQUIVALENCE (BLANKS,CD(23)), (V(1),CD(51)), (CMPNNT,CD(39))       00058190
       REAL*8    V, BLANKS, CMPNNT, STC, TCE, SCAC, SCBC, SCCC, SCDC,   00058200
     1           CCE, SPC, PEC, CEC, STBC, TEC, CARD                    00058210
      DIMENSION V(11)                                                   00058220
      DIMENSION STC(11), TCE(11), SCAC(11), SCBC(11), SCCC(11), SCDC(11)00058230
     *, CCE(11), SPC(11), PEC(11), CEC(11), STBC(11), TEC(11)           00058240
      DIMENSION CARD(11)                                                00058250
      INTEGER READA                                                     00058260
      DATA STC/8HTITLE   ,8HGENERAL ,8HECS PROG,8HRAM     ,7*8H        /00058270
      DATA TCE/8HTITEND  ,10*8H        /                                00058280
      DATA SCAC/8HCASEA   ,10*8H        /                               00058290
      DATA SCBC/8HCASEB   ,10*8H        /                               00058300
      DATA SCCC/8HCASEC   ,10*8H        /                               00058310
      DATA SCDC/8HCASED   ,10*8H        /                               00058320
      DATA CCE/8HCASEND  ,10*8H        /                                00058330
      DATA SPC/8HPARAM   ,10*8H        /                                00058340
      DATA PEC/8HPAREND  ,10*8H        /                                00058350
      DATA CEC/8HCOMEND  ,10*8H        /                                00058360
      DATA STBC/8HTABT    ,10*8H        /                               00058370
      DATA TEC/8HTABEND  ,10*8H        /                                00058380
      IRS = 1                                                           00058390
      IFP = 0                                                           00058400
      IF(V(3).NE.BLANKS) IFP = 1                                        00058410
      NCOMP = 0                                                         00058420
      KNT = 0                                                           00058430
    1 ID = READA(CARD)                                                  00058440
      IF (ID.GT.1) GO TO 2                                              00058450
      KNT = KNT+1                                                       00058460
      CALL WRTEA(CARD)                                                  00058470
      GO TO 1                                                           00058480
    2 IF (KNT.EQ.0.AND.TYPE.GT.0) CALL WRTEA(STC)                       00058490
      CALL WRTEA(TCE)                                                   00058500
      IRS= 2                                                            00058510
      IF (ID.GT.2) GO TO 3                                              00058520
      CALL WRTEA(CARD)                                                  00058530
      IRS = 3                                                           00058540
      ID = READA(CARD)                                                  00058550
      GO TO 4                                                           00058560
    3 IF (TYPE.LT.0) GO TO 33                                           00058570
      CALL WRTEA(SCAC)                                                  00058580
   33 IRS = 3                                                           00058590
    4 IF (ID.GT.3) GO TO 5                                              00058600
      CALL WRTEA(CARD)                                                  00058610
      IRS = 4                                                           00058620
      ID = READA(CARD)                                                  00058630
      GO TO 6                                                           00058640
    5 IF (TYPE.LT.0) GO TO 55                                           00058650
      CALL WRTEA(SCBC)                                                  00058660
   55 IRS = 4                                                           00058670
    6 IF (ID.GT.4) GO TO 7                                              00058680
      CALL WRTEA(CARD)                                                  00058690
      IRS = 5                                                           00058700
      ID = READA(CARD)                                                  00058710
      GO TO 8                                                           00058720
    7 IF (TYPE.LT.0) GO TO 77                                           00058730
      CALL WRTEA(SCCC)                                                  00058740
   77 IRS = 5                                                           00058750
    8 IF (ID.GT.5) GO TO 9                                              00058760
      CALL WRTEA(CARD)                                                  00058770
      IRS = 6                                                           00058780
      ID = READA(CARD)                                                  00058790
      GO TO 10                                                          00058800
    9 IF (TYPE.LT.0) GO TO 99                                           00058810
      CALL WRTEA(SCDC)                                                  00058820
   99 IRS = 6                                                           00058830
   10 CALL WRTEA(CCE)                                                   00058840
      IF (ID.GT.6) GO TO 11                                             00058850
      CALL WRTEA(CARD)                                                  00058860
      IRS = 7                                                           00058870
      ID = READA(CARD)                                                  00058880
      GO TO 12                                                          00058890
   11 IF (TYPE.LT.0) GO TO 1111                                         00058900
      CALL WRTEA(SPC)                                                   00058910
 1111 IRS = 7                                                           00058920
      GO TO 12                                                          00058930
   13 ID = READA(CARD)                                                  00058940
   12 IF (ID.GT.8) GO TO 14                                             00058950
      CALL WRTEA(CARD)                                                  00058960
      GO TO 13                                                          00058970
   14 CALL WRTEA(PEC)                                                   00058980
      IRS = 9                                                           00058990
      GO TO 15                                                          00059000
   16 ID = READA(CARD)                                                  00059010
   15 IF (ID.GT.9) GO TO 17                                             00059020
      CALL WRTEA(CARD)                                                  00059030
      IF (CARD(1).NE.CMPNNT) NCOMP = NCOMP+1                            00059040
      GO TO 16                                                          00059050
   17 CALL WRTEA(CEC)                                                   00059060
      IRS = 10                                                          00059070
   18 IF (ID.EQ.15) GO TO 30                                            00059080
      IF (ID.EQ.13.OR.ID.EQ.14) GO TO 23                                00059090
      IF (ID.GT.10) GO TO 21                                            00059100
   22 CALL WRTEA(CARD)                                                  00059110
      ID = READA(CARD)                                                  00059120
      IF (ID.EQ.11) GO TO 26                                            00059130
      CALL WRTEA(STBC)                                                  00059140
      GO TO 27                                                          00059150
   26 CALL WRTEA(CARD)                                                  00059160
   29 ID = READA(CARD)                                                  00059170
   27 IF (ID.EQ.12) GO TO 28                                            00059180
      CALL WRTEA(TEC)                                                   00059190
      GO TO 18                                                          00059200
   28 CALL WRTEA(CARD)                                                  00059210
      GO TO 29                                                          00059220
   21 CERR = CERR+1                                                     00059230
      CALL LINES(2)                                                     00059240
      WRITE (OUT,1006) CERR,CARD                                        00059250
 1006 FORMAT(6H0ERROR,I6,5X,15HNO TABID CARD *,A6,9A8,A2,1H*)           00059260
   25 ID = READA(CARD)                                                  00059270
      IF (ID.EQ.15) GO TO 30                                            00059280
      IF (ID.EQ.10) GO TO 22                                            00059290
      IF (ID.EQ.13.OR.ID.EQ.14) GO TO 23                                00059300
      CALL LINES(1)                                                     00059310
      WRITE (OUT,1007) CARD                                             00059320
 1007 FORMAT(31X,1H*,A6,9A8,A2,1H*)                                     00059330
      GO TO 25                                                          00059340
   23 CALL WRTEA(CARD)                                                  00059350
      ID = READA(CARD)                                                  00059360
      GO TO 18                                                          00059370
   30 CALL WRTEA(CARD)                                                  00059380
      REWIND IAUI                                                       00059390
      RETURN                                                            00059400
C     INPUTA                                                            00059410
      END                                                               00059420
      SUBROUTINE LINES (N)                                              00065880
      COMMON /CC/ C(600)                                                00065890
      EQUIVALENCE (OUT,C(7)), (PAGE,C(10)), (SEQCN,C(11)), (LCL,C(12))  00065900
     *, (LC,C(13))                                                      00065910
      INTEGER OUT, PAGE, SEQCN                                          00065920
      INTEGER*2 iyr, imon, iday                                          00065920
      DIMENSION CD(200)                                                 00065930
      EQUIVALENCE (CD(1),C(201))                                        00065960
      EQUIVALENCE (TITLE(1),CD(62)), (CASN,CD(82)), (DATE,CD(83))       00065970
       REAL*8    CD                                                     00065940
       REAL*8    TITLE, CASN, DATE                                      00065950
      DIMENSION TITLE(10)                                               00065980
      LC = LC+N                                                         00065990
      IF (LC.LE.LCL) RETURN                                             00066000
      LC = 0                                                            00066010
      PAGE = PAGE+1                                                     00066020
c
      CALL GETDAT(IYR,IMON,IDAY)                                                             
      CALL GETTIM(IHR,IMIN,ISEC,I100TH)                                                      
C     WRITE (OUT,1000) TITLE,PAGE,SEQCN,CASN,DATE                               
C1000 FORMAT(8H0*AECS* ,9A8,A2,26X,4HPAGE,I9/1H ,I5,2X,5HCASE ,A8,87X,          
CCCCC    *5HDATE ,F9/1H )                                                           
c
      WRITE (OUT,1000) TITLE,PAGE,SEQCN,CASN,imon,iday,iyr      
     * , IHR,IMIN,ISEC,I100TH
 1000 FORMAT(8H1*AECS* ,9A8,A2,26X,4HPAGE,I9/1H ,I5,2X,5HCASE ,A8,72X,  
     *5HDATE , i2, '/', i2, '/', i4,'  ', I2,':',I2,':',I2,'.',I2)                               
      RETURN                                                            00066060
C     LINES                                                             00066070
      END                                                               00066080
      SUBROUTINE MPDUM                                                  00078890
C     OVERLAY (AECS,3,0)                                                00078900
C     PROGRAM O30                                                       00078910
      COMMON /CC/ C(600)                                                00078920
      EQUIVALENCE (IN,C(6)), (OUT,C(7)), (PERR,C(8)), (NLEG,C(25))      00078930
     *, (NSTA,C(26)), (IW,C(27)), (IP,C(28)), (IT,C(29)), (IH,C(30))    00078940
     *, (BIG,C(31)), (NGO,C(15)), (PAGE,C(10)), (SEQCN,C(11))           00078950
     *, (LC,C(13)), (IGA,C(35)), (NGA,C(36)), (ISP,C(42)), (ILN,C(37))  00078960
     *, (ISN,C(38)), (CERR,C(16)), (IFB,C(55)), (SCR(1),C(151))         00078970
     *, (IPAU1,C(56))                                                   00078980
      DIMENSION SCR(30)                                                 00078990
      INTEGER OUT, PERR, CERR, PAGE, SEQCN                              00079000
      EQUIVALENCE (SCR(1),I), (SCR(2),J), (SCR(3),K), (SCR(4),IV)       00079010
     *, (SCR(5),IX), (SCR(6),IS), (SCR(7),NLC), (SCR(8),NSC)            00079020
     *, (SCR(9),NC), (SCR(10),IFT), (SCR(11),IRN), (SCR(12),VAL1)       00079030
     *, (SCR(13),VAL2), (SCR(14),VAL3), (SCR(15),TIME), (SCR(16),LOC)   00079040
     *, (SCR(17),L)                                                     00079050
      DIMENSION CD(200)                                                 00079060
       REAL*8   CD, CN !8/98                                                      00079070
      EQUIVALENCE (CD(1),C(201))                                        00079080
       REAL*8    CTRL, NOGO, COMMT, ENDCAS, V, PDUM, DATE, TITL,        00079090
     1           GTITL, CASN                                            00079100
      EQUIVALENCE (CTRL(1),CD(1)), (NOGO,CD(6)), (COMMT,CD(21))         00079110
     *, (ENDCAS,CD(16)), (V(1),CD(51)), (PDUM,CD(7)), (DATE,CD(83))     00079120
     *, (TITL(1),CD(62)), (GTITL(1),CD(72)), (CASN,CD(82))              00079130
      DIMENSION CTRL(1), V(11), TITL(10), GTITL(10)                     00079140
      COMMON /DC/ DZ(2),D(128001)                                        00079160
      DIMENSION ID(2)                                                   00079170
      EQUIVALENCE (ID(1),D(1))                                          00079180
      DIMENSION CARD(11)                                                00079190
       REAL*8    CLEG, CSTA, CGEN, CNL, CNS                             00079200
      EQUIVALENCE (CN,CARD(1))                                          00079210
      DATA CLEG/3HLEG/, CSTA/3HSTA/, CGEN/6HGENARG/, CNL/4HNLEG/        00079220
     *, CNS/4HNSTA/                                                     00079230
c@      CALL SECOND(TIME)                                                 00079240
      PERR = 0                                                          00079250
      NGO = 0                                                           00079260
      PAGE = 0                                                          00079270
      SEQCN = SEQCN+1                                                   00079280
      IF (IFB.EQ.0) GO TO 5                                             00079290
      NC = 0                                                            00079300
      DO 6 I=NC,NLEG                                                    00079310
      J = ID(IFB+I)                                                     00079320
      IF (J.EQ.0) GO TO 6                                               00079330
      DO 7 K=I,NLEG                                                     00079340
      L = ID(IFB+K)                                                     00079350
      IF (J.NE.L) GO TO 7                                               00079360
      ID(IFB+K) = 0                                                     00079370
    7 CONTINUE                                                          00079380
      CALL FDC(4,4,0,D,J)                                               00079390
    6 CONTINUE                                                          00079400
      IFB = IFB-1                                                       00079410
      CALL FDC(NLEG+1,4,0,D,IFB)                                        00079420
    5 CONTINUE                                                          00079430
      IF (ISN.NE.0) CALL FDC(NSTA,4,0,D,ISN)                            00079440
      IF (ILN.NE.0) CALL FDC(NLEG,4,0,D,ILN)                            00079450
      IF (IH.NE.0) CALL FDC(NSTA,4,0,D,IH)                              00079460
      IF (IT.NE.0) CALL FDC(NSTA,4,0,D,IT)                              00079470
      IF (IP.NE.0) CALL FDC(NSTA,4,0,D,IP)                              00079480
      IF (IW.NE.0) CALL FDC(NLEG,4,0,D,IW)                              00079490
      IF (IGA.NE.0) CALL FDC(NGA,4,0,D,IGA)                             00079500
      IF (ISP.NE.0) CALL FDC(0,4,ISP,D,0)                               00079510
      ISP = 0                                                           00079520
      CALL GDCU(NGA,4,0,D,IGA)                                          00079530
      DO 4 I=1,10                                                       00079540
    4 TITL(I) = GTITL(I)                                                00079550
      CASN = PDUM                                                       00079560
      CALL LINES(100)                                                   00079570
      LC = LC+1                                                         00079580
      WRITE (OUT,1020) TIME                                             00079590
 1020 FORMAT(8H TIME D ,F10.2,5H SEC.)                                  00079600
      NLC = 0                                                           00079610
      NLEG = 1                                                          00079620
      NSC = 0                                                           00079630
      NSTA = 2                                                          00079640
      NC = 0                                                            00079650
  100 READ (IN,1002) CARD                                               00079660
 1002 FORMAT(A6,9A8,A2)                                                 00079670
      CD(84) = CN                                                       00079680
      IF (CN.EQ.COMMT) GO TO 100                                        00079690
      IF (CN.EQ.CLEG) GO TO 101                                         00079700
      IF (CN.EQ.CSTA) GO TO 102                                         00079710
      IF (CN.EQ.CGEN) GO TO 103                                         00079720
      IF (CN.EQ.CNL) GO TO 104                                          00079730
      IF (CN.EQ.CNS) GO TO 105                                          00079740
      IF (CN.EQ.ENDCAS) GO TO 302                                       00079750
      DO 3 I=1,7                                                        00079760
      K = I                                                             00079770
      IF (CN.EQ.CTRL(I)) GO TO 300                                      00079780
    3 CONTINUE                                                          00079790
  118 CONTINUE                                                          00079800
      CERR = CERR+1                                                     00079810
      CALL LINES(2)                                                     00079820
      WRITE (OUT,1004) CERR,CARD                                        00079830
 1004 FORMAT(6H0ERROR,I6,5X,10HBAD CARD *,A6,9A8,A2,1H*)                00079840
      GO TO 100                                                         00079850
  101 CONTINUE                                                          00079860
      IF (NLC.EQ.0) GO TO 111                                           00079870
  114 CONTINUE                                                          00079880
      BACKSPACE IN                                                      00079890
      READ (IN,1005) CN,IV,IFT,IRN,VAL1                                 00079900
 1005 FORMAT(A6,I4,2I5,E10.0)                                           00079910
      IX = 0                                                            00079920
      IF (IV.EQ.0) GO TO 131                                            00079930
      IX = ILEGN(IV)                                                    00079940
      IF (IX.EQ.0) GO TO 100                                            00079950
      D(IW+IX) = VAL1                                                   00079960
  131 IF (IFT.GT.0 .AND. IFT.LE.3) GO TO 130                            00079970
      CERR = CERR+1                                                     00079980
      CALL LINES(2)                                                     00079990
      WRITE (OUT,1021) CERR,IFT                                         00080000
 1021 FORMAT(6H0ERROR,I6,5X,18HINVALID FLUID TYPE,I6)                   00080010
  130 CALL GDCU(4,4,0,D,LOC)                                            00080020
      ID(LOC+1) = IFT                                                   00080030
      ID(LOC+2)= IRN                                                    00080040
      ID(IFB+IX) = LOC                                                  00080050
      NC = 1                                                            00080060
      GO TO 100                                                         00080070
  102 CONTINUE                                                          00080080
      IF (NSC.EQ.0) GO TO 112                                           00080090
  115 CONTINUE                                                          00080100
      BACKSPACE IN                                                      00080110
      READ (IN,1003) CN,IV,VAL1,VAL2,VAL3                               00080120
 1003 FORMAT(A6,I4,3E10.0)                                              00080130
      IX = ISTAN(IV)                                                    00080140
      IF (IX.EQ.0) GO TO 100                                            00080150
      D(IP+IX) = VAL1                                                   00080160
      D(IT+IX) = VAL2                                                   00080170
      D(IH+IX) = VAL3                                                   00080180
      NC = 1                                                            00080190
      GO TO 100                                                         00080200
  103 CONTINUE                                                          00080210
      BACKSPACE IN                                                      00080220
      READ (IN,1003) CN,IV,VAL1                                         00080230
      IF (IV.LT.1.OR.IV.GT.NGA) GO TO 113                               00080240
      D(IGA+IV) = VAL1                                                  00080250
      NC = 1                                                            00080260
      GO TO 100                                                         00080270
  113 CERR = CERR+1                                                     00080280
      CALL LINES(2)                                                     00080290
      WRITE (OUT,1007) CERR,IV                                          00080300
 1007 FORMAT(6H0ERROR,I6,5X,18HBAD ARGUMENT INDEX,I6)                   00080310
      GO TO 100                                                         00080320
  111 CONTINUE                                                          00080330
      IS = 0                                                            00080340
  119 CONTINUE                                                          00080350
      CERR = CERR+1                                                     00080360
      CALL LINES(2)                                                     00080370
      WRITE (OUT,1031) CERR                                             00080380
 1031 FORMAT(6H0ERROR,I6,5X,12HNO NLEG CARD)                            00080390
      NLEG = 1                                                          00080400
      GO TO 116                                                         00080410
  104 CONTINUE                                                          00080420
      IF (NLC.EQ.1) GO TO 118                                           00080430
      BACKSPACE IN                                                      00080440
      READ (IN,1003) CN,NLEG                                            00080450
      IF (NLEG.LE.0) NLEG = 1                                           00080460
      IS = 1                                                            00080470
  116 CONTINUE                                                          00080480
      CALL GDCU(NLEG,4,0,D,IW)                                          00080490
      CALL GDCU(NLEG+1,4,0,D,IFB)                                       00080500
      IFB = IFB+1                                                       00080510
      DO 1 I=1,NLEG                                                     00080520
    1 D(IW+I) = BIG                                                     00080530
      NLC = 1                                                           00080540
      IF (IS) 121,114,100                                               00080550
  112 CONTINUE                                                          00080560
      IS = 0                                                            00080570
  120 CONTINUE                                                          00080580
      CERR = CERR+1                                                     00080590
      CALL LINES(2)                                                     00080600
      WRITE (OUT,1032) CERR                                             00080610
 1032 FORMAT(6H0ERROR,I6,5X,12HNO NSTA CARD)                            00080620
      NSTA = 1                                                          00080630
      GO TO 117                                                         00080640
  105 CONTINUE                                                          00080650
      IF (NSC.EQ.1) GO TO 118                                           00080660
      BACKSPACE IN                                                      00080670
      READ (IN,1003) CN,NSTA                                            00080680
      IF (NSTA.LE.0) NSTA = 1                                           00080690
      IS = 1                                                            00080700
  117 CONTINUE                                                          00080710
      CALL GDCU(NSTA,4,0,D,IP)                                          00080720
      CALL GDCU(NSTA,4,0,D,IT)                                          00080730
      CALL GDCU(NSTA,4,0,D,IH)                                          00080740
      DO 2 I=1,NSTA                                                     00080750
      D(IP+I) = BIG                                                     00080760
      D(IT+I) = BIG                                                     00080770
    2 D(IH+I) = BIG                                                     00080780
      NSC = 1                                                           00080790
      IF (IS) 122,115,100                                               00080800
  300 IF (CN.EQ.NOGO) GO TO 100                                         00080810
      DO 310 I=1,11                                                     00080820
  310 V(I) = CARD(I)                                                    00080830
      GO TO 312                                                         00080840
C 301 CALL EXIT                                                         00080850
  302 READ (IN,1002) V                                                  00080860
  312 CONTINUE                                                          00080870
      IS = -1                                                           00080880
      IF (NLC.EQ.0) GO TO 119                                           00080890
  121 CONTINUE                                                          00080900
      IS = -1                                                           00080910
      IF (NSC.EQ.0) GO TO 120                                           00080920
  122 CONTINUE                                                          00080930
      CALL LINES(1)                                                     00080940
      WRITE (OUT,1001) NLEG,NSTA                                        00080950
 1001 FORMAT(24H DUMMY PERFORMANCE CASE.,I6,7H LEG(S),I6,11H STATION(S))00080960
      CALL LINES(2)                                                     00080970
      WRITE (OUT,1010)                                                  00080980
 1010 FORMAT(2H0W)                                                      00080990
      IF (ILN.EQ.0) GO TO 401                                           00081000
      CALL PARNF2(NLEG,D(ILN+1),D(IW+1))                                00081010
  401 CONTINUE                                                          00081020
      CALL LINES(2)                                                     00081030
      WRITE (OUT,1011)                                                  00081040
 1011 FORMAT(2H0P)                                                      00081050
      IF (ISN.EQ.0) GO TO 402                                           00081060
      CALL PARNF2(NSTA,D(ISN+1),D(IP+1))                                00081070
  402 CONTINUE                                                          00081080
      CALL LINES(2)                                                     00081090
      WRITE (OUT,1012)                                                  00081100
 1012 FORMAT(2H0T)                                                      00081110
      IF (ISN.EQ.0) GO TO 403                                           00081120
      CALL PARNF2(NSTA,D(ISN+1),D(IT+1))                                00081130
  403 CONTINUE                                                          00081140
      CALL LINES(2)                                                     00081150
      WRITE (OUT,1013)                                                  00081160
 1013 FORMAT(2H0H)                                                      00081170
      IF (ISN.EQ.0) GO TO 404                                           00081180
      CALL PARNF5(NSTA,D(ISN+1),D(IH+1))                                00081190
  404 CONTINUE                                                          00081200
      CALL LINES(2)                                                     00081210
      WRITE (OUT,1014)                                                  00081220
 1014 FORMAT(20H0GENERAL ARGUMENT(S))                                   00081230
      CALL PARNTH(NGA,D(IGA+1))                                         00081240
      CALL GDCU(NLEG,4,1,D,J)                                           00081250
      CALL LINES(2)                                                     00081260
      WRITE (OUT,1016)                                                  00081270
 1016 FORMAT(14H0FLUID TYPE(S))                                         00081280
      DO 405 I=1,NLEG                                                   00081290
      K = ID(IFB+I)                                                     00081300
      IF (K.EQ.0) GO TO 405                                             00081310
      ID(J+I) = ID(K+1)                                                 00081320
  405 CONTINUE                                                          00081330
      CALL IPAREN(NLEG,D(ILN+1),D(J+1))                                 00081340
      CALL LINES(2)                                                     00081350
      WRITE (OUT,1017)                                                  00081360
 1017 FORMAT(25H0FLUID RELATIVE NUMBER(S))                              00081370
      DO 406 I=1,NLEG                                                   00081380
      K = ID(IFB+I)                                                     00081390
      IF (K.EQ.0) GO TO 406                                             00081400
      ID(J+I) = ID(K+2)                                                 00081410
  406 CONTINUE                                                          00081420
      CALL IPAREN(NLEG,D(ILN+1),D(J+1))                                 00081430
      I = ID(IFB)                                                       00081440
      L = ID(I+1)                                                       00081450
      K = ID(I+2)                                                       00081460
      CALL LINES(2)                                                     00081470
      WRITE (OUT,1018) L,K                                              00081480
 1018 FORMAT(25H0FREE STREAM FLUID - TYPE,I4,3X,15HRELATIVE NUMBER,I6)  00081490
      CALL FDC(NLEG,4,1,D,J)                                            00081500
   99 CONTINUE                                                          00081510
      IF (NC.EQ.0) CERR = CERR+1                                        00081520
      PERR = CERR                                                       00081530
      CALL LINES(2)                                                     00081540
      WRITE (OUT,1015)                                                  00081550
 1015 FORMAT(9H0CASE END)                                               00081560
      WRITE (OUT,1022)                                                  00081570
 1022 FORMAT(1H1)                                                       00081580
      RETURN                                                            00081590
C     MPDUM                                                             00081600
      END                                                               00081610
      SUBROUTINE MRGE                                                   00082150
      CHARACTER*8 CHARAY                                                00082150
      COMMON /CC/ C(600)                                                00082160
      EQUIVALENCE (OUT,C(7)), (CERR,C(16)), (IAUR,C(19))                00082170
     *, (IAUB,C(21)), (IAUI,C(20)), (NCOMP,C(44))                       00082180
      INTEGER OUT, CERR                                                 00082190
      DIMENSION CD(200)                                                 00082200
      REAL*8    CD                                                      00082210
      EQUIVALENCE (CD(1),C(201))                                        00082220
      EQUIVALENCE (PARAM,CD(36)), (CMPNNT,CD(39))                       00082230
       REAL*8    CARDC, CARDB, CHANGC, PARAM, CMPNNT                    00082240
C*    DIMENSION CARDC(11), CARDB(11), CRDC(2), CRDB(2)                  00082250
      DIMENSION CARDC(11), CARDB(11)                                    00082250
      DIMENSION CHANGC(11)                                              00082260
      INTEGER READC, READB, RDC, RDB                                    00082270
      DATA CHANGC/8HCHANGE  ,10*8H        /                             00082280
      DATA X/2H X/, DEL/2H D/, REP/2H R/, ADD/2H A/                     00082290
     *, BLNK/2H  /                                                      00082300
      NCOMP = 0                                                         00082310
      IC = READC(CARDC)                                                 00082320
      CALL WRTEB(CARDC)                                                 00082330
      IB = READB(CARDB)                                                 00082340
      KNT = 0                                                           00082350
    1 IC = READC(CARDC)                                                 00082360
      IF (IC.EQ.1) GO TO 2                                              00082370
      KNT = KNT+1                                                       00082380
      CALL WRTEB(CARDC)                                                 00082390
      GO TO 1                                                           00082400
    2 IB = READB(CARDB)                                                 00082410
      IF (IB.EQ.1) GO TO 3                                              00082420
      IF (KNT.GT.0) GO TO 2                                             00082430
      CALL WRTEB(CARDB)                                                 00082440
      GO TO 2                                                           00082450
    3 CALL WRTEB(CARDC)                                                 00082460
    4 IC = READC(CARDC)                                                 00082470
    5 IB = READB(CARDB)                                                 00082480
      IF (CARDC(1).EQ.CARDB(1)) GO TO 6                                 00082490
      CALL WRTEB(CARDB)                                                 00082500
      GO TO 5                                                           00082510
    6 CALL WRTEB(CARDC)                                                 00082520
      IF (IC.NE.2) GO TO 4                                              00082530
      IC = READC(CARDC)                                                 00082540
      IF (CARDC(1).EQ.PARAM) GO TO 8                                    00082550
    7 IB = READB(CARDB)                                                 00082560
      IF (IB.EQ.3) GO TO 9                                              00082570
      CALL WRTEB(CARDB)                                                 00082580
      GO TO 7                                                           00082590
    8 CALL WRTEB(CARDC)                                                 00082600
      IC = READC(CARDC)                                                 00082610
      IB = READB(CARDB)                                                 00082620
      GO TO 7                                                           00082630
    9 CALL WRTEB(CARDC)                                                 00082640
      IF (IC.EQ.3) GO TO 10                                             00082650
      IC = READC(CARDC)                                                 00082660
      GO TO 9                                                           00082670
   10 LCM = 0                                                           00082680
   28 IB = RDB(CARDB)                                                   00082690
CWAS
C     CALL CORE (CARDB(2), 8)                                           00082700
C     READ (6,1100)  CCB, NCB                                           00082710
CIS
      WRITE(CHARAY,'(A8)')CARDB(2)                                      00082710
      READ (CHARAY,1100)  CCB, NCB                                      00082710
 1100 FORMAT(A2,I4)                                                     00082720
   11 IC = RDC(CARDC)                                                   00082730
CWAS
C     CALL CORE(CARDC(2),8)                                             00082740
C     READ (6,1100)  CCC,NCC                                            00082750
CIS
C
      WRITE(CHARAY,'(A8)')CARDC(2)                                      00082710
      READ (CHARAY,1100)  CCC,NCC                                       00082750
      IF (IC.EQ.4) GO TO 22                                             00082760
      IF (CCC.EQ.X.OR.CCC.EQ.DEL.OR.CCC.EQ.REP.OR.CCC.EQ.ADD) GO TO 13  00082770
      IF (CCC.EQ.BLNK) GO TO 13                                         00082780
   12 CERR = CERR+1                                                     00082790
      CALL LINES(2)                                                     00082800
      WRITE (OUT,1000) CERR,CARDC(1),CCC,NCC,(CARDC(I),I=3,11)          00082810
 1000 FORMAT(6H0ERROR,I6,5X,16HINVALID CHANGE *,A6,A2,I4,8A8,A4,1H*)    00082820
      GO TO 11                                                          00082830
   13 IF (NCC.NE.0) GO TO 16                                            00082840
      IF (CCC.EQ.ADD .OR. CCC.EQ.BLNK) GO TO 14                         00082850
      IF (CCC.EQ.X) GO TO 15                                            00082860
      GO TO 12                                                          00082870
   14 CALL WRTEC(CARDC)                                                 00082880
      GO TO 11                                                          00082890
   15 IF (IB.EQ.4) GO TO 11                                             00082900
      IB = RDB(CARDB)                                                   00082910
CWAS
C      CALL CORE(CARDB(2), 8)                                           00082920
C      READ (6, 1100)  CCB,NCB                                          00082930
CIS
       WRITE(CHARAY,'(A8)')CARDB(2)                                     00082930
       READ (CHARAY, 1100)  CCB,NCB                                     00082930
      LCM = 10000                                                       00082940
      GO TO 15                                                          00082950
   16 IF (NCC.LT.LCM) GO TO 12                                          00082960
      IF (NCC.GT.LCM) GO TO 17                                          00082970
      IF (CCC.EQ.ADD .OR. CCC.EQ.BLNK) GO TO 14                         00082980
      GO TO 12                                                          00082990
   17 IF (IB.EQ.4) GO TO 12                                             00083000
      IF (NCB-NCC) 18,19,12                                             00083010
   18 IF (NCB.NE.0) LCM = NCB                                           00083020
      CALL WRTEC(CARDB)                                                 00083030
   27 IB = RDB(CARDB)                                                   00083040
CWAS
C      CALL CORE(CARDB(2), 8)                                           00083050
C      READ (6, 1100)  CCB,NCB                                          00083060
CIS
       WRITE(CHARAY,'(A8)')CARDB(2)                                     00082930
       READ (CHARAY, 1100)  CCB,NCB                                     00083060
      GO TO 17                                                          00083070
   19 IF (NCB.NE.0) LCM = NCB                                           00083080
      IF (CCC.EQ.X) GO TO 15                                            00083090
      IF (CCC.EQ.DEL) GO TO 28                                          00083100
      IF (CCC.EQ.REP) GO TO 20                                          00083110
      IF (CCC.EQ.ADD .OR. CCC.EQ.BLNK) GO TO 21                         00083120
      GO TO 12                                                          00083130
   20 CALL WRTEC(CARDC)                                                 00083140
      GO TO 28                                                          00083150
   21 CALL WRTEC(CARDB)                                                 00083160
      CALL WRTEC(CARDC)                                                 00083170
      GO TO 28                                                          00083180
   22 IF (IB.EQ.4) GO TO 29                                             00083190
      CALL WRTEC(CARDB)                                                 00083200
      IB = RDB(CARDB)                                                   00083210
CWAS
C      CALL CORE(CARDB(2), 8)                                           00083220
C      READ (6, 1100)  CCB,NCB                                          00083230
CIS
       WRITE(CHARAY,'(A8)')CARDB(2)                                     00082930
       READ (CHARAY, 1100)  CCB,NCB                                     00083230
      GO TO 22                                                          00083240
   29 CALL WRTEC(CARDC)                                                 00083250
      NCOMP = NCOMP-1                                                   00083260
   23 IC = READC(CARDC)                                                 00083270
      IF (IC.EQ.6) GO TO 24                                             00083280
      CALL WRTEB(CARDC)                                                 00083290
      GO TO 23                                                          00083300
   24 CALL WRTEB(CHANGC)                                                00083310
   25 IB = READB(CARDB)                                                 00083320
      IF (IB.EQ.6) GO TO 26                                             00083330
      CALL WRTEB(CARDB)                                                 00083340
      GO TO 25                                                          00083350
   26 CALL WRTEB(CARDC)                                                 00083360
      REWIND IAUR                                                       00083370
      REWIND IAUI                                                       00083380
      REWIND IAUB                                                       00083390
      RETURN                                                            00083400
C     MRGE                                                              00083410
      END                                                               00083420
      SUBROUTINE MSIZE                                                  00084930
C     OVERLAY (AECS,2,0)                                                00084940
C     PROGRAM O20                                                       00084950
      COMMON /CC/ C(600)                                                00084960
      EQUIVALENCE (OUT,C(7)), (CERR,C(16)), (SERR,C(9))                 00084970
      INTEGER OUT, CERR, SERR                                           00084980
C*    IQC = 0                                                           00084990
C   1 CALL OVERLAY(4HAECS,2,1,0)                                        00085000
    1 CALL SMM                                                          00085010
C*    IQC = CERR                                                        00085020
C   2 CALL OVERLAY(4HAECS,2,2,0)                                        00085030
    2 CALL SZM                                                          00085040
C*    IQC = CERR                                                        00085050
C   3 CALL OVERLAY(4HAECS,2,3,0)                                        00085060
    3 CALL SPM                                                          00085070
   99 CONTINUE                                                          00085080
      SERR = CERR                                                       00085090
      WRITE (OUT,1001)                                                  00085100
 1001 FORMAT(1H1)                                                       00085110
      RETURN                                                            00085120
C     MSIZE                                                             00085130
      END                                                               00085140
      SUBROUTINE PMM                                                    00099630
C     OVERLAY (AECS,1,1)                                                00099640
C     PROGRAM O11                                                       00099650
      COMMON /CC/ C(600)                                                00099660
      EQUIVALENCE (PERR,C(8)), (TYPE,C(1)), (SEQCN,C(11)), (PAGE,C(10)) 00099670
     *, (LC,C(13)), (NGO,C(15)), (OUT,C(7)), (PASS,C(17)), (IAUR,C(19)) 00099680
     *, (ILN,C(37)), (ISN,C(38)), (NSV,C(50)), (NEV,C(51))              00099690
     *, (ITN,C(39)), (ISP,C(42)), (ISVT,C(46)), (ISV,C(47))             00099700
     *, (IEVT,C(48)), (IEV,C(49)), (ILR,C(52)), (ISR,C(53))             00099710
     *, (IGA,C(35)), (NLEG,C(25)), (NSTA,C(26)), (NGA,C(36))            00099720
     *, (IAUB,C(21)), (IAUI,C(20)), (IPAU1,C(56)), (IPAU2,C(57))        00099730
     *, (IPAU3,C(58)), (ICDB,C(43)), (IFB,C(55)), (ISTR,C(82))          00099740
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (IH,C(30))                  00099750
      INTEGER TYPE, PAGE, SEQCN, OUT, PERR, PASS                        00099760
      DIMENSION CD(200)                                                 00099770
      REAL*8    CD                                                      00099780
      REAL*8    DATE, CASN, PRFM, PCNG, V, TITL, GTITL                  00099790
      EQUIVALENCE (CD(1),C(201))                                        00099800
      EQUIVALENCE (DATE,CD(83)), (CASN,CD(82)), (PRFM,CD(1))            00099810
     *, (PCNG,CD(4)), (V(1),CD(51)), (TITL(1),CD(62)), (GTITL(1),CD(72))00099820
      DIMENSION V(11), TITL(10), GTITL(10)                              00099830
      COMMON /DC/ DZ(2),D(128001)                                            00099840
      DIMENSION ID(2)                                                   00099850
      EQUIVALENCE (ID(1),D(1))                                          00099860
      PASS = -1                                                         00099870
      PERR = 0                                                          00099880
      NGO = 0                                                           00099890
      PAGE = 0                                                          00099900
      SEQCN = SEQCN+1                                                   00099910
c@      CALL SECOND(TIME)                                                 00099920
      DO 1 I=1,10                                                       00099930
    1 TITL(I) = GTITL(I)                                                00099940
      IAUR = IPAU2                                                      00099950
      IAUI = IPAU2                                                      00099960
      IF (TYPE) 2,3,4                                                   00099970
    4 CASN = PRFM                                                       00099980
      GO TO 20                                                          00099990
    2 CASN = PCNG                                                       00100000
      IAUR = IPAU1                                                      00100010
      IAUB = IPAU2                                                      00100020
      IAUI = IPAU3                                                      00100030
      GO TO 20                                                          00100040
    3 CASN = PCNG                                                       00100050
   20 CALL LINES(100)                                                   00100060
      LC = LC+1                                                         00100070
      WRITE (OUT,1000) TIME                                             00100080
 1000 FORMAT(8H TIME M ,F10.2,5H SEC.)                                  00100090
      IF (IFB.EQ.0) GO TO 5                                             00100100
      II = 0                                                            00100110
      DO 6 I=II,NLEG                                                    00100120
      J = ID(IFB+I)                                                     00100130
      IF (J.EQ.0) GO TO 6                                               00100140
      DO 7 K=I,NLEG                                                     00100150
      L = ID(IFB+K)                                                     00100160
      IF (J.NE.L) GO TO 7                                               00100170
      ID(IFB+K) = 0                                                     00100180
    7 CONTINUE                                                          00100190
      CALL FDC(4,4,0,D,J)                                               00100200
    6 CONTINUE                                                          00100210
      IFB = IFB-1                                                       00100220
      CALL FDC(NLEG+1,4,0,D,IFB)                                        00100230
    5 CONTINUE                         
      loc=0                                                             00100240
      IF (ISN.NE.0) CALL FDC(NSTA,4,0,D,ISN)                            00100250
      IF (ILN.NE.0) CALL FDC(NLEG,4,0,D,ILN)                            00100260
      IF (IH.NE.0) CALL FDC(NSTA,4,0,D,IH)                              00100270
      IF (IT.NE.0) CALL FDC(NSTA,4,0,D,IT)                              00100280
      IF (IP.NE.0) CALL FDC(NSTA,4,0,D,IP)                              00100290
      IF (IW.NE.0) CALL FDC(NLEG,4,0,D,IW)                              00100300
      IF (IGA.NE.0) CALL FDC(NGA,4,0,D,IGA)                             00100310
      IF (ISP.NE.0) CALL FDC(0,4,ISP,D,loc)  !sfw "loc" was "0"              00100320
      ISP = 0                                                           00100330
      CALL GDCU(NGA,4,0,D,IGA)                                          00100340
      ICDB = 0                                                          00100350
      ILR = 0                                                           00100360
      ISR = 0                                                           00100370
      ISTR = 0                                                          00100380
      ITN = 0                                                           00100390
      NSV = 0                                                           00100400
      ISVT = 0                                                          00100410
      ISV = 0                                                           00100420
      NEV = 0                                                           00100430
      IEVT = 0                                                          00100440
      IEV = 0                                                           00100450
      ISP = 1                                                           00100460
      CALL WRTEA(V)                                                     00100470
      CALL INPUTA                                                       00100480
      IF (TYPE.LT.0) CALL MRGE                                          00100490
      RETURN                                                            00100500
C     PMM                                                               00100510
      END                                                               00100520
      SUBROUTINE PPM                                                    00100620
C     OVERLAY (AECS,1,3)                                                00100630
C     PROGRAM O13                                                       00100640
      COMMON /CC/ C(600)                                                00100650
      EQUIVALENCE (OUT,C(7)), (NGO,C(15)), (CERR,C(16)), (IDS,C(5))     00100660
     *, (NSV,C(50)), (IFP,C(22)), (PASS,C(17))                          00100670
     *, (ICONV,C(79)), (ICPP,C(88)), (IIOP,C(90))                       00100680
     *, (ITAD,C(54)), (NWT,C(78))                                       00100690
      INTEGER OUT, CERR, PASS                                           00100700
      DIMENSION CD(200)                                                 00100710
      REAL*8    CD                                                      00100720
      EQUIVALENCE (CD(1),C(201))                                        00100730
      REAL*8    DATE                                                    00100740
      EQUIVALENCE (DATE,CD(83))                                         00100750
      COMMON /DC/ DZ(2),D(128001)                                            00100760
      DIMENSION ID(2)                                                   00100770
      EQUIVALENCE (ID(1),D(1))                                          00100780
      PASS = 1                                                          00100790
      IF (NGO.EQ.0) GO TO 2                                             00100800
      CALL LINES(1)                                                     00100810
      WRITE (OUT,1002)                                                  00100820
 1002 FORMAT(19H *** NO GO CASE ***)                                    00100830
      CERR = CERR+1                                                     00100840
      GO TO 99                                                          00100850
    2 IF (CERR.EQ.0) GO TO 3                                            00100860
      CALL LINES(4)                                                     00100870
      WRITE (OUT,1003) CERR                                             00100880
 1003 FORMAT(14H0CASE REJECTED/1H0,I6,18H ERROR(S) DETECTED)            00100890
      GO TO 99                                                          00100900
    3 CONTINUE                                                          00100910
      CALL LINES(100)                                                   00100920
C@      CALL SECOND(TIME)                                                 00100930
      WRITE (OUT,1000) TIME                                             00100940
 1000 FORMAT(8H TIME P ,F10.2)                                          00100950
      ICONV = 0                                                         00100960
      IF (NSV.EQ.0) GO TO 1                                             00100970
      IF(ID(ITAD+NWT).NE.0) GO TO 4                                     00100980
      CALL LINES(2)                                                     00100990
      WRITE(OUT,1016)                                                   00101000
 1016 FORMAT(35H0CASE USED NO WEIGHING FACTOR TABLE   )                 00101010
    4 CALL SOLN1                                                        00101020
    1 CONTINUE                                                          00101030
      IFP= 1                                                            00101040
      IIOP = 0                                                          00101050
      IF (IDS.NE.0) CALL LINES(100)                                     00101060
      CALL PCOMPP                                                       00101070
      CALL OUTPTA                                                       00101080
      CALL LINES(2)                                                     00101090
      WRITE (OUT,1015)                                                  00101100
 1015 FORMAT(9H0CASE END)                                               00101110
   99 CONTINUE                                                          00101120
      RETURN                                                            00101130
C     PPM                                                               00101140
      END                                                               00101150
      SUBROUTINE PQC (IQC)                                              00101160
      COMMON /CC/ C(600)                                                00101170
      EQUIVALENCE (IATMP,C(84)), (IATMT,C(85)), (NGO,C(15))             00101180
     *, (IENDJ,C(14)), (NGA,C(36)), (IGA,C(35)), (IVM,C(86))            00101190
     *, (NPV,C(32)), (IPAR,C(33)), (IN,C(6))                            00101200
     *, (OUT,C(7)), (CERR,C(16))                                        00101210
      INTEGER OUT, CERR                                                 00101220
      DIMENSION IC(100)                                                 00101230
      EQUIVALENCE (IC(1),C(1))                                          00101240
      DIMENSION CD(200)                                                 00101250
      REAL*8    CD                                                      00101260
      EQUIVALENCE (CD(1),C(201))                                        00101270
      EQUIVALENCE (V(1),CD(51)), (ENDC,CD(16)), (ENDJ,CD(3))            00101280
     *, (QCNG,CD(8)), (CASN,CD(82))                                     00101290
     *, (VALUE,CD(37))                                                  00101300
      DIMENSION V(11)                                                   00101310
       REAL*8    V, ENDC, ENDJ, QCNG, CASN, VALUE, ALTN, VOMN, PTVN,    00101320
     1           PAN, TAN, HAN, CNCN, RSTN, DMPN                        00101330
      COMMON /DC/ DZ(2),D(128001)                                            00101340
      DIMENSION ID(2)                                                   00101350
      EQUIVALENCE (ID(1),D(1))                                          00101360
      DATA ALTN/3HALT/, VOMN/3HVOM/, PTVN/3HPTV/, PAN/3HPAM/, TAN/3HTAM/00101370
     *, HAN/3HHAM/, CNCN/4HCASN/, RSTN/5HRESET/, DMPN/4HDUMP/           00101380
      IF (NGO.NE.0) GO TO 96                                            00101390
    1 CONTINUE                                                          00101400
      D(IGA+1) = 0.0                                                    00101410
      IF (IVM.NE.1) D(IGA+3) = 0.0                                      00101420
      IF (IVM.NE.-1) D(IGA+4) = 0.0                                     00101430
      IF (IATMP.NE.0) D(IGA+5) = 0.0                                    00101440
      IF (IATMT.NE.0) D(IGA+6) = 0.0                                    00101450
      D(IGA+7) = 0.0                                                    00101460
      D(IGA+8) = 0.0                                                    00101470
      DO 67 I=10,NGA                                                    00101480
   67 D(IGA+I) = 0.0                                                    00101490
      CALL LINES(100)                                                   00101500
      WRITE (OUT,1004) V                                                00101510
 1004 FORMAT(2H *,A6,9A8,A2,1H*)                                        00101520
   68 READ (IN,1005) V                                                  00101530
 1005 FORMAT(A6,9A8,A2)                                                 00101540
      IF (ENDC.EQ.V(1)) GO TO 70                                        00101550
      IF (RSTN.EQ.V(1)) GO TO 65                                        00101560
      IF (DMPN.EQ.V(1)) GO TO 64                                        00101570
      IF (CNCN.EQ.V(1)) GO TO 60                                        00101580
      IF (ALTN.EQ.V(1)) GO TO 71                                        00101590
      IF (VOMN.EQ.V(1)) GO TO 72                                        00101600
      IF (PTVN.EQ.V(1) .OR. VALUE.EQ.V(1)) GO TO 73                     00101610
      IF (PAN.EQ.V(1)) GO TO 74                                         00101620
      IF (TAN.EQ.V(1)) GO TO 75                                         00101630
      IF (HAN.EQ.V(1)) GO TO 76                                         00101640
   69 CERR = CERR+1                                                     00101650
      CALL LINES(1)                                                     00101660
      WRITE (OUT,1006) V                                                00101670
 1006 FORMAT(2H  ,A6,9A8,A2,9H*BAD CARD)                                 00101680
      GO TO 68                                                          00101690
   64 BACKSPACE IN                                                      00101700
      READ (IN,1009) IC(5)                                              00101710
 1009 FORMAT(10X,I5)                                                    00101720
      GO TO 66                                                          00101730
   65 CERR = IQC                                                        00101740
      GO TO 66                                                          00101750
   70 CALL LINES(1)                                                     00101760
      WRITE (OUT,1001) V                                                00101770
 1001 FORMAT(2H *,A6,9A8,A2,1H*)                                        00101780
      IF (CERR.EQ.0) GO TO 51                                           00101790
      CALL LINES(2)                                                     00101800
      WRITE (OUT,1003)                                                  00101810
 1003 FORMAT(30H0CASE REJECTED DUE TO ERROR(S))                         00101820
   51 READ (IN,1000) V                                                  00101830
 1000 FORMAT(A6,9A8,A2)                                                 00101840
      GO TO 98                                                          00101850
   60 BACKSPACE IN                                                      00101860
      READ (IN,1002) CASN                                               00101870
 1002 FORMAT(10X,A8)                                                    00101880
      GO TO 66                                                          00101890
   71 BACKSPACE IN                                                      00101900
      READ (IN,1007) A                                                  00101910
 1007 FORMAT(10X,E10.0)                                                 00101920
      D(IGA+2) = A                                                      00101930
      GO TO 66                                                          00101940
   72 BACKSPACE IN                                                      00101950
      READ (IN,1007) A                                                  00101960
      IF (IVM) 721,66,722                                               00101970
  721 D(IGA+4) = A                                                      00101980
      GO TO 66                                                          00101990
  722 D(IGA+3) = A                                                      00102000
      GO TO 66                                                          00102010
   73 BACKSPACE IN                                                      00102020
      READ (IN,1008) I,A                                                00102030
 1008 FORMAT(6X,I4,E10.0)                                               00102040
      IF (I.LT.1 .OR. I.GT.NPV) GO TO 69                                00102050
      D(IPAR+I) = A                                                     00102060
      GO TO 66                                                          00102070
   74 BACKSPACE IN                                                      00102080
      READ (IN,1007) A                                                  00102090
      D(IGA+5) = A                                                      00102100
      GO TO 66                                                          00102110
   75 BACKSPACE IN                                                      00102120
      READ (IN,1007) A                                                  00102130
      D(IGA+6) = A                                                      00102140
      GO TO 66                                                          00102150
   76 BACKSPACE IN                                                      00102160
      READ (IN,1007) A                                                  00102170
      D(IGA+9) = A                                                      00102180
   66 CALL LINES(1)                                                     00102190
      WRITE (OUT,1001) V                                                00102200
      GO TO 68                                                          00102210
   98 CONTINUE                                                          00102220
      IF (CERR.EQ.0) GO TO 99                                           00102230
      IF (QCNG.EQ.V(1)) GO TO 1                                         00102240
      IQC = -1                                                          00102250
   99 CONTINUE                                                          00102260
      IF (ENDJ.EQ.V(1)) IENDJ = 1                                       00102270
      GO TO 199                                                         00102280
   96 READ (IN,1000) V                                                  00102290
      IF (ENDC.NE.V(1)) GO TO 96                                        00102300
      READ (IN,1000) V                                                  00102310
      IF (QCNG.EQ.V(1)) GO TO 96                                        00102320
  199 CONTINUE                                                          00102330
C     PQC                                                               00102340
      RETURN                                                            00102350
      END                                                               00102360
      SUBROUTINE PTABR                                                  00102370
      COMMON /CC/ C(600)                                                00102380
      EQUIVALENCE (OUT,C(7)), (NTID,C(40)), (ITAD,C(54)), (INP,C(68))   00102390
     *, (ISP,C(42)), (IGA,C(35)), (IW,C(27)), (IP,C(28)), (IT,C(29))    00102400
     *, (IH,C(30))                                                      00102410
      INTEGER OUT                                                       00102420
      COMMON /DC/ DZ(2),D(128001)                                            00102430
      DIMENSION ID(2)                                                   00102440
      EQUIVALENCE (ID(1),D(1))                                          00102450
      DIMENSION TD(3)                                                   00102460
      EQUIVALENCE (TN,TD(1)), (ITD,TD(2)), (NLB,TD(3))                  00102470
      DATA TE/4HTEND/                                                   00102480
      DO 11 I=1,NTID                                                    00102490
      ITM = ID(ITAD+I)                                                  00102500
      IF (ITM.GE.0 .OR. ITM.LE.-1000000) GO TO 11                       00102510
      ITM = -ITM                                                        00102520
      IT1 = ITM/100                                                     00102530
      IT2 = ITM-IT1*100                                                 00102540
      ID(ITAD+I) = -(IT2*10000+IT1)                                     00102550
   11 CONTINUE                                                          00102560
      ITE = 0                                                           00102570
    1 ILTN = 0                                                          00102580
      LTN = 1000000                                                     00102590
      DO 2 I=1,NTID                                                     00102600
      ITM = ID(ITAD+I)                                                  00102610
      IF (ITM.GE.0.OR.ITM.LE.-1000000) GO TO 2                          00102620
      ITA = -ITM                                                        00102630
      IF (ITA.GE.LTN) GO TO 2                                           00102640
      ITMS = ITM                                                        00102650
      ILTN = I                                                          00102660
      LTN = ITA                                                         00102670
    2 CONTINUE                                                          00102680
      IF (ILTN.EQ.0) GO TO 99                                           00102690
    3 CONTINUE                                                          00102700
      IF (ITE.NE.0) GO TO 97                                            00102710
    7 CONTINUE                                                          00102720
      READ (INP) TD                                                     00102730
      IF (TN.EQ.TE) GO TO 98                                            00102740
      IF (ITD.EQ.LTN) GO TO 6                                           00102750
      IF (ITD.GT.LTN) GO TO 96                                          00102760
      READ (INP)                                                        00102770
      READ (INP)                                                        00102780
      GO TO 7                                                           00102790
    6 READ (INP)                                                        00102800
      CALL GDCU(NLB,4,ISP,D,LOC)                                        00102810
      I1 = LOC+1                                                        00102820
      I2 = LOC+NLB                                                      00102830
      READ (INP) TN,(D(I),I=I1,I2)                                      00102840
      NDIM = ID(LOC+1)                                                  00102850
      IF (NDIM.EQ.1) GO TO 100                                          00102860
      IF (NDIM.EQ.2) GO TO 200                                          00102870
      IF (NDIM.EQ.3) GO TO 300                                          00102880
      IF (NDIM.EQ.33) GO TO 300                                         00102890
      IF (NDIM.EQ.4) GO TO 400                                          00102900
      IF (NDIM.EQ.44) GO TO 400                                         00102910
      IF (NDIM.EQ.-1) GO TO 10                                          00102920
  100 CONTINUE                                                          00102930
      GO TO 4                                                           00102940
  200 CONTINUE                                                          00102950
      L = ID(LOC+2)                                                     00102960
      ASSIGN 201 TO IS                                                  00102970
      GO TO 20                                                          00102980
  201 CONTINUE                                                          00102990
      ID(LOC+2) = L                                                     00103000
      ID(LOC+5) = ID(LOC+5)+LOC                                         00103010
      ID(LOC+6) = ID(LOC+6)+LOC                                         00103020
      GO TO 4                                                           00103030
  300 CONTINUE                                                          00103040
      L = ID(LOC+2)                                                     00103050
      ASSIGN 301 TO IS                                                  00103060
      GO TO 20                                                          00103070
  301 CONTINUE                                                          00103080
      ID(LOC+2) = L                                                     00103090
      L = ID(LOC+5)                                                     00103100
      ASSIGN 302 TO IS                                                  00103110
      GO TO 20                                                          00103120
  302 CONTINUE                                                          00103130
      ID(LOC+5) = L                                                     00103140
      ID(LOC+8) = ID(LOC+8)+LOC                                         00103150
      ID(LOC+9) = ID(LOC+9)+LOC                                         00103160
      ID(LOC+10) = ID(LOC+10)+LOC                                       00103170
      GO TO 4                                                           00103180
  400 CONTINUE                                                          00103190
      L = ID(LOC+2)                                                     00103200
      ASSIGN 401 TO IS                                                  00103210
      GO TO 20                                                          00103220
  401 CONTINUE                                                          00103230
      ID(LOC+2) = L                                                     00103240
      L = ID(LOC+5)                                                     00103250
      ASSIGN 402 TO IS                                                  00103260
      GO TO 20                                                          00103270
  402 CONTINUE                                                          00103280
      ID(LOC+5) = L                                                     00103290
      L = ID(LOC+8)                                                     00103300
      ASSIGN 403 TO IS                                                  00103310
      GO TO 20                                                          00103320
  403 CONTINUE                                                          00103330
      ID(LOC+8) = L                                                     00103340
      ID(LOC+11) = ID(LOC+11)+LOC                                       00103350
      ID(LOC+12) = ID(LOC+12)+LOC                                       00103360
      ID(LOC+13) = ID(LOC+13)+LOC                                       00103370
      ID(LOC+14) = ID(LOC+14)+LOC                                       00103380
      GO TO 4                                                           00103390
   10 CONTINUE                                                          00103400
      ID(LOC+3) = ID(LOC+3)+LOC                                         00103410
      GO TO 4                                                           00103420
   20 CONTINUE                                                          00103430
      I1 = L/10000                                                      00103440
      I2 = L-I1*10000                                                   00103450
      IF (I1.EQ.0) GO TO 21                                             00103460
      GO TO (22,23,24,25), I1                                           00103470
   21 L = IGA+I2                                                        00103480
      GO TO 26                                                          00103490
   22 L = IW+ILEGN(I2)                                                  00103500
      GO TO 26                                                          00103510
   23 L = IP+ISTAN(I2)                                                  00103520
      GO TO 26                                                          00103530
   24 L = IT+ISTAN(I2)                                                  00103540
      GO TO 26                                                          00103550
   25 L = IH+ISTAN(I2)                                                  00103560
   26 GO TO IS                                                          00103570
    4 CONTINUE                                                          00103580
      DO 5 I=1,NTID                                                     00103590
      IF (ID(ITAD+I).EQ.ITMS) ID(ITAD+I) = LOC                          00103600
    5 CONTINUE                                                          00103610
      GO TO 1                                                           00103620
   96 CONTINUE                                                          00103630
      BACKSPACE INP                                                     00103640
      GO TO 97                                                          00103650
   98 CONTINUE                                                          00103660
      ITE = 1                                                           00103670
   97 CONTINUE                                                          00103680
      IT2 = LTN/10000                                                   00103690
      IT1 = LTN-IT2*10000                                               00103700
      CALL LINES(2)                                                     00103710
      WRITE (OUT,1001) IT1,IT2                                          00103720
 1001 FORMAT(27H0ERROR - PTAB TABLE MISSING,2I6)                        00103730
      LOC = 0                                                           00103740
      GO TO 4                                                           00103750
   99 CONTINUE                                                          00103760
      REWIND INP                                                        00103770
      RETURN                                                            00103780
C     PTABR                                                             00103790
      END                                                               00103800
C$99 DELETED PTTL FROM HERE 10/14/99
      SUBROUTINE PZM                                                    00110450
C     OVERLAY (AECS,1,2)                                                00110460
C     PROGRAM O12                                                       00110470
      COMMON /CC/ C(600)                                                00110480
      EQUIVALENCE (OUT,C(7)), (NGO,C(15)), (IAUR,C(19)), (PASS,C(17))   00110490
     *, (NWT,C(78))                                                     00110500
      INTEGER OUT, PASS                                                 00110510
      DIMENSION CD(200)                                                 00110520
      REAL*8    CD                                                      00110530
      REAL*8    TITLE, DATE, TITEND, CN, TITLC                          00110540
      EQUIVALENCE (CD(1),C(201))                                        00110550
      EQUIVALENCE (TITLE(1),CD(62)), (DATE,CD(83)), (TITEND,CD(11))     00110560
     *, (CN,CD(84))                                                     00110570
      DIMENSION TITLE(1)                                                00110580
      DIMENSION TITLC(10)                                               00110590
      PASS = 0                                                          00110600
      IF (NGO.GT.0) GO TO 99                                            00110610
C@      CALL SECOND(TIME)                                                 00110620
      READ (IAUR,1000)                                                  00110630
 1000 FORMAT(A6)                                                        00110640
      IT = 0                                                            00110650
    1 READ (IAUR,1001) CN,TITLC                                         00110660
 1001 FORMAT(A6,9A8,A2)                                                 00110670
      IF (CN.EQ.TITEND) GO TO 4                                         00110680
      IF (IT.GT.0) GO TO 3                                              00110690
      IT = 1                                                            00110700
      DO 2 I=1,10                                                       00110710
    2 TITLE(I) = TITLC(I)                                               00110720
      CALL LINES(100)                                                   00110730
      WRITE (OUT,1002) TIME                                             00110740
 1002 FORMAT(8H TIME Z ,F10.2)                                          00110750
    3 CALL LINES(2)                                                     00110760
      WRITE (OUT,1003) TITLC                                            00110770
 1003 FORMAT(1H0,5X,9A8,A2)                                             00110780
      GO TO 1                                                           00110790
    4 CONTINUE                                                          00110800
      CALL PCASER                                                       00110810
      CALL PARAMR                                                       00110820
      CALL PCOMPR                                                       00110830
      NWT = ITIDN(1,0)                                                  00110840
      CALL TABR                                                         00110850
      REWIND IAUR                                                       00110860
   99 CONTINUE                                                          00110870
      RETURN                                                            00110880
C     PZM                                                               00110890
      END                                                               00110900
      INTEGER FUNCTION READA (CARD)                                     00112780
      COMMON /CC/ C(600)                                                00112790
      EQUIVALENCE (IAUI,C(20)), (IN,C(6)), (IRS,C(18)), (TYPE,C(1))     00112800
     *, (CERR,C(16)), (OUT,C(7)), (IENDJ,C(14)), (NGO,C(15))            00112810
     *, (NOCS,C(63)), (IDIM,C(24)), (NOCP,C(59))                        00112820
     *, (IFP,C(22))                                                     00112830
      INTEGER TYPE, OUT, CERR                                           00112840
      DIMENSION CD(200)                                                 00112850
      REAL*8    CD                                                      00112860
      EQUIVALENCE (CD(1),C(201))                                        00112870
       REAL*8    COMMT, ENDC, ENDJ, V, NOGO, BLANKS, CRDN, CTRLN        00112880
      EQUIVALENCE (COMMT,CD(21)), (ENDC,CD(16)), (ENDJ,CD(3))           00112890
     *, (V(1),CD(51)), (NOGO,CD(6)), (BLANKS,CD(23))                    00112900
      DIMENSION V(11)                                                   00112910
      DIMENSION CRDN(1)                                                 00112920
      EQUIVALENCE (CRDN(1),CD(31))                                      00112930
      DIMENSION CTRLN(1)                                                00112940
      EQUIVALENCE (CTRLN(1),CD(1))                                      00112950
      COMMON /CP/ PD(100)                                               00112960
       REAL*8    PD, CMNP                                               00112970
      DIMENSION CMNP(1)                                                 00112980
      EQUIVALENCE (CMNP(1),PD(1))                                       00112990
      COMMON /CS/ SD(100)                                               00113000
       REAL*8    SD, CMNS                                               00113010
      DIMENSION CMNS(1)                                                 00113020
      EQUIVALENCE (CMNS(1),SD(1))                                       00113030
       REAL*8    CARD                                                   00113040
      DIMENSION CARD(11)                                                00113050
    1 READ (IN,1000) CARD                                               00113060
 1000 FORMAT(A6,9A8,A2)                                                 00113070
      IF (CARD(1).EQ.COMMT) GO TO 1                                     00113080
      DO 2 I=IRS,14                                                     00113090
      K = I                                                             00113100
      IF (CARD(1).EQ.CRDN(I)) GO TO 100                                 00113110
    2 CONTINUE                                                          00113120
      K = 15                                                            00113130
      IF (CARD(1).EQ.ENDC) GO TO 101                                    00113140
      IF (TYPE.EQ.2.OR.TYPE.EQ.-2) GO TO 33                             00113150
      DO 4 I=1,NOCP                                                     00113160
      K = I                                                             00113170
      IF (CARD(1).EQ.CMNP(I)) GO TO 102                                 00113180
    4 CONTINUE                                                          00113190
      GO TO 55                                                          00113200
   33 DO 3 I=1,NOCS                                                     00113210
      K = I                                                             00113220
      IF (CARD(1).EQ.CMNS(I)) GO TO 102                                 00113230
    3 CONTINUE                                                          00113240
   55 DO 5 I=1,8                                                        00113250
      K = I                                                             00113260
      IF (CARD(1).EQ.CTRLN(I)) GO TO 103                                00113270
    5 CONTINUE                                                          00113280
      CERR = CERR+1                                                     00113290
      CALL LINES(2)                                                     00113300
      WRITE (OUT,1001) CERR,CARD                                        00113310
 1001 FORMAT(6H0ERROR,I6,5X,12HCARD ERROR *,A6,9A8,A2,1H*)              00113320
      GO TO 1                                                           00113330
  100 READA = K                                                         00113340
      RETURN                                                            00113350
  101 READ (IN,1000) V                                                  00113360
      IF (V(1).EQ.COMMT) GO TO 101                                      00113370
      IF (V(1).NE.NOGO) GO TO 100                                       00113380
      NGO = 1                                                           00113390
      GO TO 101                                                         00113400
  102 K = 9                                                             00113410
      GO TO 100                                                         00113420
  103 IF (CARD(1).EQ.ENDJ) GO TO 200                                    00113430
      IF (CARD(1).EQ.NOGO) GO TO 201                                    00113440
  105 V(1) = CARD(1)                                                    00113450
      CARD(1) = ENDC                                                    00113460
      DO 104 I=2,11                                                     00113470
      V(I) = CARD(I)                                                    00113480
  104 CARD(I) = BLANKS                                                  00113490
      K = 15                                                            00113500
      GO TO 100                                                         00113510
  200 IENDJ = 1                                                         00113520
      GO TO 105                                                         00113530
  201 NGO = 1                                                           00113540
      GO TO 1                                                           00113550
C 202 IENDJ = 1                                                         00113560
C     CARD(1) = ENDC                                                    00113570
C     V(1) = ENDJ                                                       00113580
C     DO 203 I=2,11                                                     00113590
C     V(I) = BLANKS                                                     00113600
C 203 CARD(I) = BLANKS                                                  00113610
C     K = 15                                                            00113620
C     GO TO 100                                                         00113630
C     READA                                                             00113640
      END                                                               00113650
      INTEGER FUNCTION READC (CARD)                                     00113660
      COMMON /CC/ C(600)                                                00113670
      EQUIVALENCE (IAUR,C(19)), (IAUI,C(20)), (IAUB,C(21))              00113680
      DIMENSION CD(200)                                                 00113690
      REAL*8    CD, EC, CMPNNT, CARD                                    00113700
      EQUIVALENCE (CD(1),C(201))                                        00113710
      EQUIVALENCE (EC(1),CD(11)), (CMPNNT,CD(39))                       00113720
      DIMENSION EC(6)                                                   00113730
      DIMENSION CARD(11)                                                00113740
      INTEGER READB, RDB, RDC                                           00113750
      READ (IAUI,1000) CARD                                             00113760
 1000 FORMAT(A6,9A8,A2)                                                 00113770
      DO 1 I=1,6                                                        00113780
      J = I                                                             00113790
      IF (CARD(1).EQ.EC(I)) GO TO 2                                     00113800
    1 CONTINUE                                                          00113810
      J = 0                                                             00113820
    2 CONTINUE                                                          00113830
      GO TO 99                                                          00113840
      ENTRY READB (CARD)                                                00113850
      READ (IAUB,1000) CARD                                             00113860
      DO 3 I=1,6                                                        00113870
      J = I                                                             00113880
      IF (CARD(1).EQ.EC(I)) GO TO 4                                     00113890
    3 CONTINUE                                                          00113900
      J = 0                                                             00113910
    4 READB = J                                                         00113920
      RETURN                                                            00113930
      ENTRY RDC (CARD)                                                  00113940
      READ (IAUI,1001) CARD                                             00113950
 1001 FORMAT(A6,9A8,A2)                                                 00113960
      DO 5 I=1,6                                                        00113970
      J = I                                                             00113980
      IF (CARD(1).EQ.EC(I)) GO TO 6                                     00113990
    5 CONTINUE                                                          00114000
      J = 0                                                             00114010
    6 RDC = J                                                           00114020
      RETURN                                                            00114030
      ENTRY RDB (CARD)                                                  00114040
      READ (IAUB,1001) CARD                                             00114050
      DO 7 I=1,6                                                        00114060
      J = I                                                             00114070
      IF (CARD(1).EQ.EC(I)) GO TO 8                                     00114080
    7 CONTINUE                                                          00114090
      J = 0                                                             00114100
    8 RDB = J                                                           00114110
      RETURN                                                            00114120
   99 CONTINUE                                                          00114130
      READC = J                                                         00114140
      RETURN                                                            00114150
C     READB, READC, RDB, RDC                                            00114160
      END                                                               00114170
      SUBROUTINE SCASER                                                 00116900
      COMMON /CC/ C(600)                                                00116910
      EQUIVALENCE (IAUR,C(19)), (MDPC,C(34)), (OUT,C(7)), (LC,C(13))    00116920
     *, (WTS,C(101)), (CUS,C(103)), (RIS,C(105)), (DRS,C(107))          00116930
     *, (WTDS,C(110)), (POWES,C(112)), (POWSS,C(113)), (POWHS,C(114))   00116940
     *, (BAES,C(115)), (FC,C(121)), (SDRAG,C(122)), (EWT,C(128))        00116950
     *, (ISSP,C(123)), (ISPP,C(124)), (IFCW,C(119)), (IFCP,C(129))      00116960
     *, (TIME,C(23)), (THR,C(130)), (DRAG,C(94)), (LDR,C(93))           00116970
     *, (WTE,C(97)), (WTF,C(96)), (WTG,C(95)), (ELL,C(116))             00116980
     *, (HLL,C(117)), (GWHP,C(118)), (HWHP,C(100)), (OSHP,C(99))        00116990
     *, (OBAE,C(98))                                                    00117000
      REAL LDR                                                          00117010
      INTEGER OUT                                                       00117020
      DIMENSION CD(200)                                                 00117030
       REAL*8    CD, CASN  !C$8/98  ADD CASN                                                    00117040
      EQUIVALENCE (CD(1),C(201))                                        00117050
      EQUIVALENCE (CASN,CD(82)), (CN,CD(84))                            00117060
      COMMON /DC/ DZ(2),D(128001)                                            00117070
      DIMENSION ID(1)                                                   00117080
      EQUIVALENCE (ID(1),D(1))                                          00117090
      READ (IAUR,1001) CN,CASN,MDPC,ISSP,ISPP,IFCW,IFCP                 00117100
 1001 FORMAT(A6,2X,A8,16I4)                                             00117110
      READ (IAUR,1002) TIME,THR,DRAG,LDR                                00117120
 1002 FORMAT(10X,7E10.0)                                                00117130
      READ (IAUR,1002) WTE,WTF,WTG                                      00117140
      READ (IAUR,1002) ELL,HLL,GWHP,HWHP,OSHP,OBAE                      00117150
      READ (IAUR,1000)                                                  00117160
 1000 FORMAT(20A4)                                                      00117170
      IPRNT = MDPC/1000                                                 00117180
      IF (MDPC.LT.0) IPRNT = -1                                         00117190
      IF (IPRNT.NE.0) GO TO 99                                          00117200
      CALL LINES(100)                                                   00117210
      LC = LC+2                                                         00117220
      WRITE (OUT,1011) CASN,ISSP,ISPP                                   00117230
 1011 FORMAT(1H ,10X,5HCASE ,A8,7X,5HISSP ,I5,10X,5HISPP ,I5)           00117240
      IF (ISPP.NE.0) GO TO 99                                           00117250
      LC = LC+2                                                         00117260
      WRITE (OUT,1012) IFCW,IFCP                                        00117270
 1012 FORMAT(1H ,10X,5HIFCW ,I5,10X,5HIFCP ,I5)                         00117280
      LC = LC+2                                                         00117290
      WRITE (OUT,1013) TIME,THR,DRAG,LDR                                00117300
 1013 FORMAT(1H ,10X,5HTIME ,F10.2,5X,5HTHR  ,F10.0,5X,5HDRAG ,F10.0,5X,00117310
     *5HLDR  ,F10.2)                                                    00117320
      LC = LC+2                                                         00117330
      WRITE (OUT,1014) WTE,WTF,WTG                                      00117340
 1014 FORMAT(1H ,10X,5HWTE  ,F10.0,5X,5HWTF  ,F10.0,5X,5HWTG  ,F10.0)   00117350
      LC = LC+2                                                         00117360
      WRITE (OUT,1015) ELL,HLL,GWHP,HWHP,OSHP,OBAE                      00117370
 1015 FORMAT(1H ,10X,5HELL  ,F10.1,5X,5HHLL  ,F10.1,5X,5HGWHP ,F10.4,5X,00117380
     *5HHWHP ,F10.4,5X,5HOSHP ,F10.1,5X,5HOBAE ,F10.1)                  00117390
   99 CONTINUE                                                          00117400
      IF (ISPP.EQ.0) IFCW = ITIDN(IFCW,41)                              00117410
      IF (ISPP.EQ.0) IFCP = ITIDN(IFCP,42)                              00117420
      WTS = 0.0                                                         00117430
      CUS = 0.0                                                         00117440
      RIS = 0.0                                                         00117450
      DRS = 1.0                                                         00117460
      WTDS = 0.0                                                        00117470
      POWES = 0.0                                                       00117480
      POWSS = 0.0                                                       00117490
      POWHS = 0.0                                                       00117500
      BAES = 0.0                                                        00117510
      FC = 0.0                                                          00117520
      SDRAG = 0.0                                                       00117530
      EWT = 0.0                                                         00117540
      RETURN                                                            00117550
C     SCASER                                                            00117560
      END                                                               00117570
      SUBROUTINE SCOMPP                                                 00118040
      COMMON /CC/ C(600)                                                00118050
      EQUIVALENCE (ICDB,C(43)), (NCOMP,C(44)), (IFB,C(55)), (NLEG,C(25))00118060
     *, (IFP,C(22)), (IGA,C(35))                                        00118070
      DIMENSION CD(200)                                                 00118080
       REAL*8   CD, CN !8/98                                                      00118090
      EQUIVALENCE (CD(1),C(201))                                        00118100
      EQUIVALENCE (CN,CD(84))                                           00118110
      COMMON /CCA/ CA(150)                                              00118120
      EQUIVALENCE (NOC,CA(1))                                           00118130
      COMMON /CS/ SD(100)                                               00118140
       REAL*8 SD    
      COMMON /DC/ DZ(2),D(128001)                                        00118150
      DIMENSION ID(2)                                                   00118160
      EQUIVALENCE (ID(1),D(1))                                          00118170
      IF (IFB.EQ.0) GO TO 2                                             00118180
      NC = 0                                                            00118190
      DO 3 I=NC,NLEG                                                    00118200
      J = ID(IFB+I)                                                     00118210
      IF (J.EQ.0) GO TO 3                                               00118220
      CALL FLUIDP(I)                                                    00118230
    3 CONTINUE                                                          00118240
    2 CONTINUE                                                          00118250
      CALL GDCU(10,4,1,D,ISSS)                                          00118260
      DO 4 I=1,10                                                       00118270
    4 D(ISSS+I) = D(IGA+80+I)                                           00118280
      NOC = 0                                                           00118290
    1 NOC = NOC+1                                                       00118300
      IF (NOC.GT.NCOMP) GO TO 99                                        00118310
      LOC = ID(ICDB+NOC)                                                00118320
      IF (LOC.EQ.0) GO TO 1                                             00118330
      I = ID(LOC+1)                                                     00118340
      CN = SD(I)                                                        00118350
      IFP = 1                                                           00118360
      GO TO (101,102,103,104,105,106,107,108,109,110,111,112,113,114,11500118370
     1,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131  00118380
     *),I                                                               00118390
  101 CONTINUE                                                          00118400
      CALL USERSP                                                       00118410
      GO TO 98                                                          00118420
  102 CONTINUE                                                          00118430
      CALL VLVSP                                                        00118440
      GO TO 98                                                          00118450
  103 CONTINUE                                                          00118460
      CALL DSEPSP                                                       00118470
      GO TO 98                                                          00118480
  104 CONTINUE                                                          00118490
      CALL WSEPSP                                                       00118500
      GO TO 98                                                          00118510
  105 CONTINUE                                                          00118520
      CALL EJCTSP                                                       00118530
      GO TO 98                                                          00118540
  106 CONTINUE                                                          00118550
      CALL APUSP                                                        00118560
      GO TO 98                                                          00118570
  107 CONTINUE                                                          00118580
      CALL RINSP                                                        00118590
      GO TO 98                                                          00118600
  108 CONTINUE                                                          00118610
      CALL ROUTSP                                                       00118620
      GO TO 98                                                          00118630
  109 CONTINUE                                                          00118640
      CALL HXSP                                                         00118650
      GO TO 98                                                          00118660
  110 CONTINUE                                                          00118670
      GO TO 98                                                          00118680
  111 CONTINUE                                                          00118690
      GO TO 98                                                          00118700
  112 CONTINUE                                                          00118710
      CALL LINESP                                                       00118720
      GO TO 98                                                          00118730
  113 CONTINUE                                                          00118740
      CALL CONDSP                                                       00118750
      GO TO 98                                                          00118760
  114 CONTINUE                                                          00118770
      GO TO 98                                                          00118780
  115 CONTINUE                                                          00118790
      CALL EVAPSP                                                       00118800
      GO TO 98                                                          00118810
  116 CONTINUE                                                          00118820
      GO TO 98                                                          00118830
  117 CONTINUE                                                          00118840
      CALL BOILSP                                                       00118850
      GO TO 98                                                          00118860
  118 CONTINUE                                                          00118870
      GO TO 98                                                          00118880
  119 CONTINUE                                                          00118890
      CALL EHTRSP                                                       00118900
      GO TO 98                                                          00118910
  120 CONTINUE                                                          00118920
      CALL INITSP                                                       00118930
      GO TO 98                                                          00118940
  121 CONTINUE                                                          00118950
      CALL MISCSP                                                       00118960
      GO TO 98                                                          00118970
  122 CONTINUE                                                          00118980
      CALL COMPSP                                                       00118990
      GO TO 98                                                          00119000
  123 CONTINUE                                                          00119010
      CALL TURBSP                                                       00119020
      GO TO 98                                                          00119030
  124 CONTINUE                                                          00119040
      CALL PUMPSP                                                       00119050
      GO TO 98                                                          00119060
  125 CONTINUE                                                          00119070
      CALL FANSP                                                        00119080
      GO TO 98                                                          00119090
  126 CONTINUE                                                          00119100
      CALL VCMPSP                                                       00119110
      GO TO 98                                                          00119120
  127 CONTINUE                                                          00119130
      CALL CNTRSP                                                       00119140
      GO TO 98                                                          00119150
  128 CONTINUE                                                          00119160
      CALL INSLSP                                                       00119170
      GO TO 98                                                          00119180
  129 CONTINUE                                                          00119190
      GO TO 98                                                          00119200
  130 CONTINUE                                                          00119210
      GO TO 98                                                          00119220
  131 CONTINUE                                                          00119230
   98 CONTINUE                                                          00119240
      GO TO 1                                                           00119250
   99 CONTINUE                                                          00119260
      DO 5 I=1,10                                                       00119270
    5 D(IGA+80+I) = D(ISSS+I)                                           00119280
      RETURN                                                            00119290
C     SCOMPP                                                            00119300
      END                                                               00119310
      SUBROUTINE SCOMPR                                                 00119320
      COMMON /CC/ C(600)                                                00119330
      EQUIVALENCE (IAUR,C(19)), (OUT,C(7)), (LC,C(13)), (MDPC,C(34))    00119340
     *, (ICV(1),C(133)), (NOCS,C(63)), (IDD,C(4)), (ICDB,C(43))         00119350
     *, (NCOMP,C(44)), (IFB,C(55)), (NLEG,C(25)), (NSTA,C(26))          00119360
     *, (ILN,C(37)), (ISN,C(38)), (IPAR,C(33))                          00119370
     *, (ISP,C(42))                                                     00119380
      EQUIVALENCE (ICV(1),K)                                            00119390
      DIMENSION ICV(18)                                                 00119400
      INTEGER OUT                                                       00119410
      DIMENSION CD(200)                                                 00119420
       REAL*8    CD                                                         00119430
       REAL*8   COMEND         !8/98
       REAL*8   CN,CC        !8/98
       REAL*8   CMPN      !8/98
      EQUIVALENCE (CD(1),C(201))                                        00119440
      EQUIVALENCE (COMEND,CD(14)), (CN,CD(84))                          00119450
      COMMON /CCA/ CA(150)                                              00119460
      EQUIVALENCE (NOC,CA(1))                                           00119470
      COMMON /CS/ SD(100)                                               00119480
       REAL*8     SD
      EQUIVALENCE (CMPN(1),SD(1))                                       00119490
      DIMENSION CMPN(1)                                                 00119500
      COMMON /DC/ DZ(2),D(128001)                                            00119510
      DIMENSION ID(2)                                                   00119520
      EQUIVALENCE (ID(1),D(1))                                          00119530
      REAL*8     N109, N113, N115, N117                                 00119540
      DATA N109/2HHX/, N113/4HCOND/, N115/4HEVAP/, N117/6HBOILER/       00119550
      IF (IFB.EQ.0) GO TO 7                                             00119560
      NC = 0                                                            00119570
      DO 8 I=NC,NLEG                                                    00119580
      J = ID(IFB+I)                                                     00119590
      IF (J.EQ.0) GO TO 8                                               00119600
      CALL FLUIDS(I,ID(J+1),ID(J+2))                                    00119610
    8 CONTINUE                                                          00119620
    7 CONTINUE                                                          00119630
      IPRNT = 10*(MDPC/10)                                              00119640
      IPRNT = IPRNT-100*(IPRNT/100)                                     00119650
      IF (MDPC.LT.0) IPRNT = -1                                         00119660
      ICNE = 0                                                          00119670
      LCR = 0                                                           00119680
      IF(NCOMP.NE.0) CALL GDCU(NCOMP,4,ISP,D,ICDB)                      00119690
      NOC = 0                                                           00119700
      IF (IPRNT.LT.0) GO TO 1                                           00119710
      CALL LINES(100)                                                   00119720
      LC = LC+1                                                         00119730
      WRITE (OUT,1000)                                                  00119740
 1000 FORMAT(13H COMPONENT(S))                                          00119750
      IF (IPRNT.NE.0) GO TO 1                                           00119760
      LC = LC+2                                                         00119770
      WRITE (OUT,1003)                                                  00119780
 1003 FORMAT(104H     1    6  8   12   16   20   24   28   32   36   40 00119790
     *  44   48   52   56   60   64   68   72   76   80/1H )            00119800
    1 READ (IAUR,1200) CN,CC,NC,(ICV(I),I=2,18)                         00119810
 1200 FORMAT(A6,A2,I4,17A4)                                             00119820
      IF (CN.EQ.COMEND) GO TO 99                                        00119830
      IF (NC.EQ.0) GO TO 5                                              00119840
      IF (NC.LT.LCR) ICNE = 1                                           00119850
      LCR = NC                                                          00119860
    5 CONTINUE                                                          00119870
      BACKSPACE IAUR                                                    00119880
      READ (IAUR,1001) CN,CC,NC,(ICV(I),I=2,18)                         00119890
 1001 FORMAT(A6,A2,18I4)                                                00119900
      IF (IPRNT.NE.0) GO TO 2                                           00119910
      CALL LINES(1)                                                     00119920
      WRITE (OUT,1002) CN,CC,NC,(ICV(I),I=2,18)                         00119930
 1002 FORMAT(5X,A6,1X,A2,18(1X,I4))                                     00119940
    2 DO 3 I=1,NOCS                                                     00119950
      K = I                                                             00119960
      IF (CN.EQ.CMPN(I)) GO TO 4                                        00119970
    3 CONTINUE                                                          00119980
      GO TO 1                                                           00119990
    4 CONTINUE                                                          00120000
      NOC = NOC+1                                                       00120010
      GO TO (101,102,103,104,105,106,107,108,109,110,111,112,113,114,11500120020
     1,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131  00120030
     *),K                                                               00120040
  101 CONTINUE                                                          00120050
      CALL USERSZ                                                       00120060
      GO TO 98                                                          00120070
  102 CONTINUE                                                          00120080
      CALL VLVSZ                                                        00120090
      GO TO 98                                                          00120100
  103 CONTINUE                                                          00120110
      CALL DSEPSZ                                                       00120120
      GO TO 98                                                          00120130
  104 CONTINUE                                                          00120140
      CALL WSEPSZ                                                       00120150
      GO TO 98                                                          00120160
  105 CONTINUE                                                          00120170
      CALL EJCTSZ                                                       00120180
      GO TO 98                                                          00120190
  106 CONTINUE                                                          00120200
      CALL APUSZ                                                        00120210
      GO TO 98                                                          00120220
  107 CONTINUE                                                          00120230
      CALL RINSZ                                                        00120240
      GO TO 98                                                          00120250
  108 CONTINUE                                                          00120260
      CALL ROUTSZ                                                       00120270
      GO TO 98                                                          00120280
  109 CONTINUE                                                          00120290
      CALL HXSZ                                                         00120300
      CN = N109                                                         00120310
      GO TO 98                                                          00120320
  110 CONTINUE                                                          00120330
      CALL HX1SZ                                                        00120340
      GO TO 1                                                           00120350
  111 CONTINUE                                                          00120360
      CALL HX2SZ                                                        00120370
      GO TO 1                                                           00120380
  112 CONTINUE                                                          00120390
      CALL LINESZ                                                       00120400
      GO TO 98                                                          00120410
  113 CONTINUE                                                          00120420
      CALL CON1SZ                                                       00120430
      CN = N113                                                         00120440
      GO TO 98                                                          00120450
  114 CONTINUE                                                          00120460
      CALL CON2SZ                                                       00120470
      GO TO 98                                                          00120480
  115 CONTINUE                                                          00120490
      CALL EVA1SZ                                                       00120500
      CN = N115                                                         00120510
      GO TO 98                                                          00120520
  116 CONTINUE                                                          00120530
      CALL EVA2SZ                                                       00120540
      GO TO 98                                                          00120550
  117 CONTINUE                                                          00120560
      CALL BOL1SZ                                                       00120570
      CN = N117                                                         00120580
      GO TO 98                                                          00120590
  118 CONTINUE                                                          00120600
      CALL BOL2SZ                                                       00120610
      GO TO 98                                                          00120620
  119 CONTINUE                                                          00120630
      CALL EHTRSZ                                                       00120640
      GO TO 98                                                          00120650
  120 CONTINUE                                                          00120660
      CALL INITSZ                                                       00120670
      GO TO 98                                                          00120680
  121 CONTINUE                                                          00120690
      CALL MISCSZ                                                       00120700
      GO TO 98                                                          00120710
  122 CONTINUE                                                          00120720
      CALL COMPSZ                                                       00120730
      GO TO 98                                                          00120740
  123 CONTINUE                                                          00120750
      CALL TURBSZ                                                       00120760
      GO TO 98                                                          00120770
  124 CONTINUE                                                          00120780
      CALL PUMPSZ                                                       00120790
      GO TO 98                                                          00120800
  125 CONTINUE                                                          00120810
      CALL FANSZ                                                        00120820
      GO TO 98                                                          00120830
  126 CONTINUE                                                          00120840
      CALL VCMPSZ                                                       00120850
      GO TO 98                                                          00120860
  127 CONTINUE                                                          00120870
      CALL CNTRSZ                                                       00120880
      GO TO 98                                                          00120890
  128 CONTINUE                                                          00120900
      CALL INSLSZ                                                       00120910
      GO TO 98                                                          00120920
  129 CONTINUE                                                          00120930
      GO TO 98                                                          00120940
  130 CONTINUE                                                          00120950
      GO TO 1                                                           00120960
  131 CONTINUE                                                          00120970
      GO TO 1                                                           00120980
   98 CONTINUE                                                          00120990
      LOC = ID(ICDB+NOC)                                                00121000
      D(LOC-2) = CN                                                     00121010
      GO TO 1                                                           00121020
   99 CONTINUE                                                          00121030
      IF (IPRNT.LT.0) GO TO 6                                           00121040
      IF (ICNE.EQ.0) GO TO 6                                            00121050
      CALL LINES(3)                                                     00121060
      WRITE (OUT,1004)                                                  00121070
 1004 FORMAT(45H0* NOTE - CARD NUMBERS NOT IN ASCENDING ORDER/10X,31HMAY00121080
     * PRODUCE INVALID CHANGE CASE)                                     00121090
    6 CONTINUE                                                          00121100
      IF (IDD.EQ.0) GO TO 17                                            00121110
      CALL LINES(NLEG/10+3)                                             00121120
      I1 = ILN+1                                                        00121130
      I2 = ILN+NLEG                                                     00121140
      WRITE (OUT,1013) (ID(I),I=I1,I2)                                  00121150
 1013 FORMAT(4H0ILN/(1X,10I10))                                         00121160
      CALL LINES(NSTA/10+3)                                             00121170
      I1 = ISN+1                                                        00121180
      I2 = ISN+NSTA                                                     00121190
      WRITE (OUT,1014) (ID(I),I=I1,I2)                                  00121200
 1014 FORMAT(4H0ISN/(1X,10I10))                                         00121210
      CALL LINES(1)                                                     00121220
      WRITE (OUT,1010) IPAR                                             00121230
 1010 FORMAT(5H IPAR,I10)                                               00121240
      IF (ICDB.EQ.0) GO TO 17                                           00121250
      DO 15 J=1,NCOMP                                                   00121260
      LOC = ID(ICDB+J)                                                  00121270
      IF (LOC.EQ.0) GO TO 15                                            00121280
      NUM = ID(LOC)-(ID(LOC)/10000)*10000                               00121290
      I1 = LOC+1                                                        00121300
      I2 = LOC+NUM                                                      00121310
      CALL LINES(NUM/10+3)                                              00121320
      WRITE (OUT,1009) J,LOC,(ID(I),I=I1,I2)                            00121330
 1009 FORMAT(5H0COMP,I6,5X,I10/(1X,10I10))                              00121340
   15 CONTINUE                                                          00121350
   17 CONTINUE                                                          00121360
      IF (IDD.EQ.0) GO TO 18                                            00121370
      IF (IFB.EQ.0) GO TO 18                                            00121380
      I1 = IFB                                                          00121390
      I2 = IFB+NLEG                                                     00121400
      CALL LINES(NLEG/10+3)                                             00121410
      WRITE (OUT,1011) (ID(I),I=I1,I2)                                  00121420
 1011 FORMAT(4H0IFB/(1X,10I10))                                         00121430
      NC = 0                                                            00121440
      DO 19 J=NC,NLEG                                                   00121450
      I1 = ID(IFB+J)                                                    00121460
      IF (I1.EQ.0) GO TO 19                                             00121470
      IF (J.EQ.0) GO TO 22                                              00121480
      I2 = J-1                                                          00121490
      DO 21 I=NC,I2                                                     00121500
      IF (I1.EQ.ID(IFB+I)) GO TO 19                                     00121510
   21 CONTINUE                                                          00121520
   22 I1 = I1+1                                                         00121530
      I2 = I1+1                                                         00121540
      CALL LINES(1)                                                     00121550
      WRITE (OUT,1012) (ID(I),I=I1,I2)                                  00121560
 1012 FORMAT(1H ,2I10)                                                  00121570
   19 CONTINUE                                                          00121580
   18 CONTINUE                                                          00121590
      RETURN                                                            00121600
C     SCOMPR                                                            00121610
      END                                                               00121620
      SUBROUTINE SMM                                                    00123460
C     OVERLAY (AECS,2,1)                                                00123470
C     PROGRAM O21                                                       00123480
      COMMON /CC/ C(600)                                                00123490
      EQUIVALENCE (SERR,C(9)), (TYPE,C(1)), (SEQCN,C(11)), (PAGE,C(10)) 00123500
     *, (LC,C(13)), (NGO,C(15)), (OUT,C(7)), (PASS,C(17)), (IAUR,C(19)) 00123510
     *, (ITN,C(39)), (IAUB,C(21)), (IAUI,C(20)), (ISAU1,C(60))          00123520
     *, (ISAU2,C(61)), (ISAU3,C(62)), (ICDB,C(43))                      00123530
     *, (ISP,C(42))                                                     00123540
      INTEGER TYPE, PAGE, SEQCN, OUT, SERR, PASS                        00123550
      DIMENSION CD(200)                                                 00123560
       REAL*8    CD                                                     00123570
C$8/98    REAL*8    V   !C@                                                00123570
      REAL*8    DATE, CASN, PRFM, PCNG, V, TITL, GTITL, size, scng!10/99   00099790
      EQUIVALENCE (CD(1),C(201))                                        00123580
      EQUIVALENCE (DATE,CD(83)), (CASN,CD(82)), (SIZE,CD(2))            00123590
     *, (SCNG,CD(5)), (V(1),CD(51))                                     00123600
     *, (TITL(1),CD(62)), (GTITL(1),CD(72))                             00123610
      DIMENSION V(11), TITL(10), GTITL(10)                              00123620
      COMMON /DC/ DZ(2),D(128001)                                            00123630
      PASS = -1                                                         00123640
      SERR = 0                                                          00123650
      NGO = 0                                                           00123660
      PAGE = 0                                                          00123670
      SEQCN = SEQCN+1                                                   00123680
C@      CALL SECOND(TIME)                                                 00123690
      DO 1 I=1,10                                                       00123700
    1 TITL(I) = GTITL(I)                                                00123710
      IAUR = ISAU2                                                      00123720
      IAUI = ISAU2                                                      00123730
      IF (TYPE) 2,3,4                                                   00123740
    4 CASN = SIZE                                                       00123750
      GO TO 20                                                          00123760
    2 CASN = SCNG                                                       00123770
      IAUR = ISAU1                                                      00123780
      IAUB = ISAU2                                                      00123790
      IAUI = ISAU3                                                      00123800
      GO TO 20                                                          00123810
    3 CASN = SCNG                                                       00123820
   20 CALL LINES(100)                                                   00123830
      LC = LC+1                                                         00123840
      WRITE (OUT,1000) TIME                                             00123850
 1000 FORMAT(8H TIME M ,F10.2,5H SEC.)                                  00123860
      IF (ISP.NE.0) CALL FDC(0,4,ISP,D,0)                               00123870
      ISP = 0                                                           00123880
      ICDB = 0                                                          00123890
      ITN = 0                                                           00123900
      ISP= 1                                                            00123910
      CALL WRTEA(V)                                                     00123920
      CALL INPUTA                                                       00123930
      IF (TYPE.LT.0) CALL MRGE                                          00123940
      RETURN                                                            00123950
C     SMM                                                               00123960
      END                                                               00123970
      SUBROUTINE SPM                                                    00127370
C     OVERLAY (AECS,2,3)                                                00127380
C     PROGRAM O23                                                       00127390
      COMMON /CC/ C(600)                                                00127400
      EQUIVALENCE (OUT,C(7)), (NGO,C(15)), (CERR,C(16)), (PERR,C(8))    00127410
     *, (PASS,C(17)), (ITAD,C(54)), (NWT,C(78))                         00127420
      INTEGER PASS, OUT, PERR, CERR                                     00127430
      DIMENSION CD(200)                                                 00127440
       REAL*8    CD                                                     00127450
      EQUIVALENCE (CD(1),C(201))                                        00127460
      EQUIVALENCE (DATE,CD(83))                                         00127470
      COMMON /DC/ DZ(2),D(128001)                                            00127480
      DIMENSION ID(2)                                                   00127490
      EQUIVALENCE (ID(1),D(1))                                          00127500
      PASS = 1                                                          00127510
      IF (NGO.EQ.0) GO TO 2                                             00127520
      CALL LINES(1)                                                     00127530
      WRITE (OUT,1002)                                                  00127540
 1002 FORMAT(19H *** NO GO CASE ***)                                    00127550
      CERR = CERR+1                                                     00127560
      GO TO 99                                                          00127570
    2 IF (CERR.EQ.0) GO TO 3                                            00127580
      CALL LINES(4)                                                     00127590
      WRITE (OUT,1003) CERR                                             00127600
 1003 FORMAT(14H0CASE REJECTED/1H0,I6,18H ERROR(S) DETECTED)            00127610
      GO TO 99                                                          00127620
    3 CONTINUE                                                          00127630
      IF (PERR.EQ.0) GO TO 1                                            00127640
      CALL LINES(2)                                                     00127650
      WRITE (OUT,1004)                                                  00127660
 1004 FORMAT(42H0CASE REJECTED DUE TO PERFORMANCE ERROR(S))             00127670
      GO TO 99                                                          00127680
    1 CONTINUE                                                          00127690
C@      CALL SECOND(TIME)                                                 00127700
      CALL LINES(100)                                                   00127710
      WRITE (OUT,1000) TIME                                             00127720
 1000 FORMAT(8H TIME P ,F10.2)                                          00127730
      IF(ID(ITAD+NWT).NE.0) GO TO 4                                     00127740
      CALL LINES(2)                                                     00127750
      WRITE(OUT,1005)                                                   00127760
 1005 FORMAT(35H0CASE USED NO WEIGHING FACTOR TABLE  )                  00127770
    4 CALL SCOMPP                                                       00127780
      CALL OUTPTB                                                       00127790
      CALL LINES(2)                                                     00127800
      WRITE (OUT,1015)                                                  00127810
 1015 FORMAT(9H0CASE END)                                               00127820
   99 CONTINUE                                                          00127830
      RETURN                                                            00127840
C     SPM                                                               00127850
      END                                                               00127860
      SUBROUTINE STABR                                                  00129220
      COMMON /CC/ C(600)                                                00129230
      EQUIVALENCE (NTID,C(40)), (ITAD,C(54)), (ITN,C(39)), (OUT,C(7))   00129240
     *, (BIG,C(31))                                                     00129250
      INTEGER OUT                                                       00129260
      COMMON /DC/ DZ(2),D(128001)                                            00129270
      DIMENSION ID(2)                                                   00129280
      EQUIVALENCE (ID(1),D(1))                                          00129290
      N = 1                                                             00129300
    4 CONTINUE                                                          00129310
      NUM = 0                                                           00129320
      DO 1 I=1,NTID                                                     00129330
      IDT = ID(ITAD+I)                                                  00129340
      IF (IDT.GE.-1000000) GO TO 1                                      00129350
      IDT = -IDT-1000000                                                00129360
      DO 2 J=1,NTID                                                     00129370
      K = J                                                             00129380
      IF (IDT.EQ.ID(ITN+J)) GO TO 3                                     00129390
    2 CONTINUE                                                          00129400
    5 CONTINUE                                                          00129410
      NUM = NUM+1                                                       00129420
      IF (N.LT.10) GO TO 1                                              00129430
      I1 = IDT/100                                                      00129440
      I2 = IDT-I1*100                                                   00129450
      CALL LINES(2)                                                     00129460
      WRITE (OUT,1001) I1,I2                                            00129470
 1001 FORMAT(31H0ERROR - UNABLE TO SATISFY STAB,2I6)                    00129480
      ID(ITAD+I) = BIG                                                  00129490
      GO TO 1                                                           00129500
    3 CONTINUE                                                          00129510
      IF (ID(ITAD+K).LE.-1000000) GO TO 5                               00129520
      ID(ITAD+I) = ID(ITAD+K)                                           00129530
    1 CONTINUE                                                          00129540
      N = N+1                                                           00129550
      IF (NUM.GT.0.AND.N.LE.10) GO TO 4                                 00129560
      RETURN                                                            00129570
C     STABR                                                             00129580
      END                                                               00129590
      SUBROUTINE SZM                                                    00129600
C     OVERLAY (AECS,2,2)                                                00129610
C     PROGRAM O22                                                       00129620
      COMMON /CC/ C(600)                                                00129630
      EQUIVALENCE (OUT,C(7)), (NGO,C(15)), (IAUR,C(19)), (PASS,C(17))   00129640
     *, (NWT,C(78))                                                     00129650
      INTEGER OUT, PASS                                                 00129660
      DIMENSION CD(200)                                                 00129670
       REAL*8    CD        
C$$8/98                                                                 00129680
      REAL*8    TITLE, DATE, TITEND, CN, TITLC                          00110540
      EQUIVALENCE (CD(1),C(201))                                        00129690
      EQUIVALENCE (TITLE(1),CD(62)), (DATE,CD(83)), (TITEND,CD(11))     00129700
     *, (CN,CD(84))                                                     00129710
      DIMENSION TITLE(1)                                                00129720
      DIMENSION TITLC(10)                                               00129730
      PASS = 0                                                          00129740
      IF (NGO.GT.0) GO TO 99                                            00129750
C@      CALL SECOND(TIME)                                                 00129760
      READ (IAUR,1000)                                                  00129770
 1000 FORMAT(A6)                                                        00129780
      IT = 0                                                            00129790
    1 READ (IAUR,1001) CN,TITLC                                         00129800
 1001 FORMAT(A6,9A8,A2)                                                 00129810
      IF (CN.EQ.TITEND) GO TO 4                                         00129820
      IF (IT.GT.0) GO TO 3                                              00129830
      IT = 1                                                            00129840
      DO 2 I=1,10                                                       00129850
    2 TITLE(I) = TITLC(I)                                               00129860
      CALL LINES(100)                                                   00129870
      WRITE (OUT,1002) TIME                                             00129880
 1002 FORMAT(8H TIME Z ,F10.2)                                          00129890
    3 CALL LINES(1)                                                     00129900
      WRITE (OUT,1003) TITLC                                            00129910
 1003 FORMAT(1H ,5X,9A8,A2)                                             00129920
      GO TO 1                                                           00129930
    4 CONTINUE                                                          00129940
      CALL SCASER                                                       00129950
      CALL PARAMR                                                       00129960
      CALL SCOMPR                                                       00129970
      NWT = ITIDN(1,0)                                                  00129980
      CALL TABR                                                         00129990
      REWIND IAUR                                                       00130000
   99 CONTINUE                                                          00130010
      RETURN                                                            00130020
C     SZM                                                               00130030
      END                                                               00130040
      SUBROUTINE TABR
      COMMON /CC/ C(600)                                                00131440
      EQUIVALENCE (NTID,C(40)), (ISP,C(42)), (ITAD,C(54)), (IAUR,C(19)) 00131450
     *, (ITN,C(39)), (OUT,C(7)), (CERR,C(16))                           00131460
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (IH,C(30)), (IGA,C(35))     00131470
     *, (NLEG,C(25)), (NSTA,C(26)), (NGA,C(36)), (ILN,C(37))            00131480
     *, (ISN,C(38)), (MDPC,C(34)), (BIG,C(31))                          00131490
     *, (LC,C(13))                                                      00131500
      INTEGER OUT, CERR                                                 00131510
      DIMENSION CD(200)                                                 00131520
      REAL*8    CD                                                      00131530
      EQUIVALENCE (CD(1),C(201))                                        00131540
       REAL*8    CN, ENDCAS, CHANG, TABEND, PTAB, STAB, CARD            00131550
      EQUIVALENCE (CN,CD(84)), (ENDCAS,CD(16)), (CHANG,CD(22))          00131560
     *, (TABEND,CD(15)), (PTAB,CD(43)), (STAB,CD(44))                   00131570
      COMMON /DC/ DZ(2),D(128001)                                            00131580
      DIMENSION ID(2)                                                   00131590
      EQUIVALENCE (ID(1),D(1))                                          00131600
      DIMENSION ICV(18), CARD(11), VALS(4), ISI(3)                      00131610
      EQUIVALENCE (NREL,ICV(1)), (NTYP,ICV(2)), (NDIM,ICV(3))           00131620
     *, (IST,ICV(4)), (NAT1,ICV(5)), (NAR1,ICV(6)), (IEI1,ICV(7))       00131630
     *, (NPT1,ICV(8)), (NAT2,ICV(9)), (NAR2,ICV(10)), (IEI2,ICV(11))    00131640
     *, (NPT2,ICV(12)), (NAT3,ICV(13)), (NAR3,ICV(14)), (IEI3,ICV(15))  00131650
     *, (NPT3,ICV(16))                                                  00131660
      EQUIVALENCE (VAL,VALS(1))                                         00131670
      IPRNT = MDPC-10*(MDPC/10)                                         00131680
      IF (MDPC.LT.0) IPRNT = -1                                         00131690
      IF (IPRNT.LT.0) GO TO 25                                          00131700
      CALL LINES(100)                                                   00131710
      LC = LC+1                                                         00131720
      WRITE (OUT,1100) NTID                                             00131730
 1100 FORMAT(7H TABLES,10X,I4,9H REQUIRED)                              00131740
   25 IC = 0                                                            00131750
      NPT = 0                                                           00131760
      NST = 0                                                           00131770
      IF (NTID.EQ.0) GO TO 1                                            00131780
      CALL GDCU(NTID,4,ISP,D,ITAD)                                      00131790
      DO 8 I=1,NTID                                                     00131800
      IF (ID(ITN+I).GE.0) GO TO 8                                       00131810
      NPT = NPT+1                                                       00131820
      ID(ITAD+I) = ID(ITN+I)                                            00131830
      IF (IPRNT.NE.0) GO TO 8                                           00131840
      I1 = IABS(ID(ITN+I))/100                                          00131850
      I2 = -ID(ITN+I)-I1*100                                            00131860
      CALL LINES(1)                                                     00131870
      WRITE (OUT,1025) I1,I2                                            00131880
 1025 FORMAT(16H PERMANENT TABLE,2I6)                                   00131890
    8 CONTINUE                                                          00131900
    1 IERR = 0                                                          00131910
    2 READ (IAUR,1000) CN,ICV                                           00131920
 1000 FORMAT(A6,2X,18I4)                                                00131930
      IF (CN.EQ.ENDCAS) GO TO 90                                        00131940
      IF (CN.EQ.CHANG) GO TO 60                                         00131950
      IF (CN.EQ.PTAB) GO TO 70                                          00131960
      IF (CN.EQ.STAB) GO TO 80                                          00131970
      IF (NTID.EQ.0) GO TO 7                                            00131980
      IDT = 100*NREL+NTYP                                               00131990
      DO 3 I=1,NTID                                                     00132000
      J = I                                                             00132010
      IF (ID(ITN+I).EQ.IDT) GO TO 10                                    00132020
    3 CONTINUE                                                          00132030
    7 CONTINUE                                                          00132040
      IF (IPRNT.NE.0) GO TO 4                                           00132050
      CALL LINES(1)                                                     00132060
      WRITE (OUT,1001)                                                  00132070
 1001 FORMAT(35H TABLE LISTED BELOW IS NOT REQUIRED)                    00132080
    5 CALL LINES(1)                                                     00132090
      WRITE (OUT,1002) CN,ICV                                           00132100
 1002 FORMAT(5X,1H*,A6,2X,18I4,1H*)                                     00132110
    4 READ (IAUR,1003) CARD                                             00132120
 1003 FORMAT(A6,9A8,A2)                                                 00132130
      IF (CARD(1).EQ.TABEND) GO TO 1                                    00132140
    6 IF (IPRNT.NE.0) GO TO 4                                           00132150
      CALL LINES(1)                                                     00132160
      WRITE (OUT,1004) CARD                                             00132170
 1004 FORMAT(5X,1H*,A6,9A8,A2,1H*)                                      00132180
      GO TO 4                                                           00132190
   10 IF (ID(ITAD+J).EQ.0) GO TO 11                                     00132200
      IF (IPRNT.NE.0) GO TO 9                                           00132210
      IF (IC.NE.0) GO TO 9                                              00132220
      CALL LINES(2)                                                     00132230
      WRITE (OUT,1005)                                                  00132240
 1005 FORMAT(40H0TABLE LISTED BELOW PREVIOUSLY SPECIFIED)               00132250
      GO TO 5                                                           00132260
    9 CONTINUE                                                          00132270
      READ (IAUR,1003) CARD                                             00132280
      IF (CARD(1).EQ.TABEND) GO TO 2                                    00132290
      GO TO 9                                                           00132300
   11 D(ITAD+J) = BIG                                                   00132310
      IF (IPRNT.NE.0) GO TO 12                                          00132320
      CALL LINES(1)                                                     00132330
      WRITE(OUT,1006) NREL,NTYP                                         00132340
 1006 FORMAT(7H TABLE ,2I6)                                             00132350
   12 CONTINUE                                                          00132360
      IF (NDIM.EQ.0.OR.NDIM.EQ.1) GO TO 100                             00132370
      IF (NDIM.EQ.2) GO TO 200                                          00132380
      IF (NDIM.EQ.3.OR.NDIM.EQ.33) GO TO 300                            00132390
      IF (NDIM.EQ.4.OR.NDIM.EQ.44) GO TO 400                            00132400
      IF (NDIM.EQ.-1) GO TO 15                                          00132410
      CALL LINES(2)                                                     00132420
      WRITE (OUT,1007)                                                  00132430
 1007 FORMAT(86H0ERROR - TABLE LISTED BELOW HAS INCORRECT NUMBER OF DIME00132440
     *NSIONS - CHECKING DISCONTINUED)                                   00132450
      GO TO 5                                                           00132460
C     ONE DIMENSIONAL TABLE                                             00132470
  100 CONTINUE                                                          00132480
      NDIM = 1                                                          00132490
      READ (IAUR,1003) CARD                                             00132500
      IF (IPRNT.NE.0) GO TO 102                                         00132510
      CALL LINES(1)                                                     00132520
      WRITE (OUT,1008) (CARD(I),I=2,11)                                 00132530
 1008 FORMAT(5X,6HTITLE/,9A8,A2)                                        00132540
      CALL LINES(1)                                                     00132550
      WRITE (OUT,1012) NDIM                                             00132560
 1012 FORMAT(1H ,4X,4HNDIM,I6)                                          00132570
  102 READ (IAUR,1009) CN,VAL                                           00132580
 1009 FORMAT(A6,4X,4E10.0)                                              00132590
      NLOC = 2                                                          00132600
      CALL GDCU(NLOC,4,ISP,D,LOC)                                       00132610
      IF (CN.EQ.TABEND) GO TO 50                                        00132620
      ID(ITAD+J) = LOC                                                  00132630
      ID(LOC+1) = 1                                                     00132640
      D(LOC+2) = VAL                                                    00132650
      IF (IPRNT.NE.0) GO TO 103                                         00132660
      CALL LINES(1)                                                     00132670
      WRITE (OUT,1013) VAL                                              00132680
 1013 FORMAT(5X,8HVALUE = ,1PE12.5)                                     00132690
  103 READ (IAUR,1003) CARD                                             00132700
      IF (CARD(1).NE.TABEND) GO TO 40                                   00132710
      GO TO 1                                                           00132720
C     TWO DIMENSIONAL TABLE                                             00132730
  200 CONTINUE                                                          00132740
      IE = IEI1/100                                                     00132750
      II = IEI1-IE*100                                                  00132760
      READ (IAUR,1003) CARD                                             00132770
      IF (IPRNT.NE.0) GO TO 212                                         00132780
      CALL LINES(1)                                                     00132790
      WRITE (OUT,1008) (CARD(I),I=2,11)                                 00132800
      CALL LINES(1)                                                     00132810
      WRITE (OUT,1022) NDIM,IST                                         00132820
 1022 FORMAT(1H ,4X,4HNDIM,I6,5X,3HIST,I6)                              00132830
  212 IF (IST.GE.0.AND.IST.LE.3) GO TO 213                              00132840
      CALL LINES(2)                                                     00132850
      WRITE (OUT,1010)                                                  00132860
 1010 FORMAT (18H0ERROR - IST WRONG)                                    00132870
      IERR = 1                                                          00132880
  213 IF (IPRNT.NE.0) GO TO 202                                         00132890
      I = 1                                                             00132900
      CALL LINES(1)                                                     00132910
      WRITE (OUT,1020) I,NAT1,NAR1,IE,II,NPT1                           00132920
 1020 FORMAT(5X,8HARGUMENT,I2,5X,5HTYPE ,I4,5X,5HRELN ,I4,5X,7HEXTRAP ,I00132930
     *4,5X,7HINTERP ,I4,5X,5HNPTS ,I4)                                  00132940
  202 ASSIGN 201 TO IS                                                  00132950
      JERR = 0                                                          00132960
      I = 5                                                             00132970
      GO TO 30                                                          00132980
  201 IERR = IERR+JERR                                                  00132990
      IF (JERR.EQ.0) GO TO 203                                          00133000
      CALL LINES(2)                                                     00133010
      WRITE (OUT,1021)                                                  00133020
 1021 FORMAT(1H0,4X,21HCHECKING DISCONTINUED)                           00133030
      GO TO 4                                                           00133040
  203 NLOC = 7+2*NPT1                                                   00133050
      CALL GDCU(NLOC,4,ISP,D,LOC)                                       00133060
      ID(LOC+1) = NDIM                                                  00133070
      ID(LOC+2) = L                                                     00133080
      ID(LOC+3) = IEI1                                                  00133090
      ID(LOC+4) = NPT1                                                  00133100
      IP1 = LOC+7                                                       00133110
      IP2 = IP1+NPT1                                                    00133120
      ID(LOC+5) = IP1+1                                                 00133130
      ID(LOC+6) = IP2+1                                                 00133140
      ID(LOC+7) = IST                                                   00133150
C$      IF (IPRNT.NE.0) GO TO 204                                         00133160
C$      CALL LINES(1)                                                     00133170
C$      WRITE (OUT,1060)                                                  00133180
C$ 1060 FORMAT(1H )                                                       00133190
C$  204 CONTINUE                                                          00133200
      JERR = 0                                                          00133210
      K = 0                                                             00133220
      VL = -BIG                                                         00133230
      ISD = 0                                                           00133240
      IF (IST.EQ.1.OR.IST.EQ.3) ISD = 1                                 00133250
      ISI(1) = 0                                                        00133260
      IF (IST.EQ.2.OR.IST.EQ.3) ISI(1) = 1                              00133270
      DO 205 I=1,NPT1,2                                                 00133280
      READ (IAUR,1009) CN,VALS                                          00133290
      IF (CN.EQ.TABEND) GO TO 206                                       00133300
      K = K+1                                                           00133310
      D(IP1+K) = VALS(1)                                                00133320
      IF (ISI(1).NE.0) D(IP1+K) = ALOG10(D(IP1+K))                      00133330
      D(IP2+K) = VALS(2)                                                00133340
      IF (ISD.NE.0) D(IP2+K) = ALOG10(D(IP2+K))                         00133350
      L = 2                                                             00133360
      IF (I.EQ.NPT1) GO TO 207                                          00133370
      K = K+1                                                           00133380
      D(IP1+K) = VALS(3)                                                00133390
      IF (ISI(1).NE.0) D(IP1+K) = ALOG10(D(IP1+K))                      00133400
      D(IP2+K) = VALS(4)                                                00133410
      IF (ISD.NE.0) D(IP2+K) = ALOG10(D(IP2+K))                         00133420
      L = 4                                                             00133430
  207 IF (IPRNT.NE.0) GO TO 211                                         00133440
      CALL LINES(1)                                                     00133450
      WRITE (OUT,1061) (VALS(M),M=1,L)                                  00133460
 1061 FORMAT(10X,1P4E15.5)                                              00133470
  211 IF (VALS(1).GE.VL) GO TO 208                                      00133480
      JERR = JERR+1                                                     00133490
  208 VL = VALS(1)                                                      00133500
      IF (I.EQ.NPT1) GO TO 205                                          00133510
      IF (VALS(3).GE.VL) GO TO 209                                      00133520
      JERR = JERR+1                                                     00133530
  209 VL = VALS(3)                                                      00133540
  205 CONTINUE                                                          00133550
  206 CONTINUE                                                          00133560
      IF (JERR.EQ.0) GO TO 210                                          00133570
      CALL LINES(2)                                                     00133580
      WRITE (OUT,1062)                                                  00133590
 1062 FORMAT(38H0ERROR - VALUES NOT IN ASCENDING ORDER)                 00133600
      IERR = IERR+JERR                                                  00133610
  210 IF (K.LT.NPT1) GO TO 50                                           00133620
      READ (IAUR,1003) CARD                                             00133630
      IF (CARD(1).NE.TABEND) GO TO 40                                   00133640
      IF (IERR.GT.0) GO TO 51                                           00133650
      ID(ITAD+J) = LOC                                                  00133660
      GO TO 1                                                           00133670
C     THREE DIMENSIONAL TABLE                                           00133680
  300 CONTINUE                                                          00133690
      IE = IEI1/100                                                     00133700
      II = IEI1-IE*100                                                  00133710
      IO = 0                                                            00133720
      INT = II/10                                                       00133730
      II = II-10*INT                                                    00133740
      IO = IO+INT                                                       00133750
      READ (IAUR,1003) CARD                                             00133760
      IF (IPRNT.NE.0) GO TO 329                                         00133770
      CALL LINES(1)                                                     00133780
      WRITE (OUT,1008) (CARD(I),I=2,11)                                 00133790
      CALL LINES(1)                                                     00133800
      WRITE (OUT,1022) NDIM,IST                                         00133810
  329 IF (IST.GE.0.AND.IST.LE.7) GO TO 328                              00133820
      CALL LINES(1)                                                     00133830
      WRITE (OUT,1010)                                                  00133840
      IERR = 1                                                          00133850
  328 IF (IPRNT.NE.0) GO TO 303                                         00133860
      I = 1                                                             00133870
      CALL LINES(1)                                                     00133880
      WRITE (OUT,1020) I,NAT1,NAR1,IE,II,NPT1                           00133890
  303 ASSIGN 301 TO IS                                                  00133900
      JERR = 0                                                          00133910
      I = 5                                                             00133920
      GO TO 30                                                          00133930
  301 IERR = IERR+JERR                                                  00133940
      IP1 = L                                                           00133950
      IE = IEI2/100                                                     00133960
      II = IEI2-IE*100                                                  00133970
      INT = II/10                                                       00133980
      II = II-10*INT                                                    00133990
      IO = IO+INT                                                       00134000
      IF (IPRNT.NE.0) GO TO 304                                         00134010
      CALL LINES(1)                                                     00134020
      I = 2                                                             00134030
      WRITE (OUT,1020) I,NAT2,NAR2,IE,II,NPT2                           00134040
      IF (NDIM.EQ.3) GO TO 304                                          00134050
      IF (IO.EQ.0) GO TO 304                                            00134060
      CALL LINES(1)                                                     00134070
      WRITE (OUT,1023)                                                  00134080
 1023 FORMAT (1H ,4X,15HORTHOGONAL GRID)                                00134090
  304 ASSIGN 302 TO IS                                                  00134100
      JERR = 0                                                          00134110
      I = 9                                                             00134120
      GO TO 30                                                          00134130
  302 IERR = IERR+JERR                                                  00134140
      IP2 = L                                                           00134150
      IF (JERR.EQ.0) GO TO 305                                          00134160
      CALL LINES(2)                                                     00134170
      WRITE (OUT,1021)                                                  00134180
      GO TO 4                                                           00134190
  305 IF (NDIM.EQ.33) GO TO 309                                         00134200
      NLOC = 11+NPT1+NPT2+NPT1*NPT2                                     00134210
      GO TO 310                                                         00134220
  309 NLOC = 11+NPT2+2*NPT1*NPT2                                        00134230
  310 CALL GDCU(NLOC,4,ISP,D,LOC)                                       00134240
      ID(LOC+1) = NDIM                                                  00134250
      ID(LOC+2) = IP1                                                   00134260
      ID(LOC+3) = IEI1                                                  00134270
      ID(LOC+4) = NPT1                                                  00134280
      ID(LOC+5) = IP2                                                   00134290
      ID(LOC+6) = IEI2                                                  00134300
      ID(LOC+7) = NPT2                                                  00134310
      IP1 = LOC+11                                                      00134320
      ID(LOC+8) = IP1+1                                                 00134330
      IF (NDIM.EQ.33) GO TO 311                                         00134340
      IP2 = IP1+NPT1                                                    00134350
      IP3 = IP2+NPT2                                                    00134360
      GO TO 312                                                         00134370
  311 IP2 = IP1+NPT1*NPT2                                               00134380
      IP3 = IP2+NPT2                                                    00134390
  312 ID(LOC+9) = IP2+1                                                 00134400
      ID(LOC+10) = IP3+1                                                00134410
      ID(LOC+11) = IST                                                  00134420
  450 CONTINUE                                                          00134430
      N = 1                                                             00134440
      IF (NDIM.EQ.33 .OR. NDIM.EQ.44) N = NPT2                          00134450
      ISI(1) = 0                                                        00134460
      IF (IST.EQ.2.OR.IST.EQ.3.OR.IST.EQ.6.OR.IST.EQ.7                  00134470
     *.OR.IST.EQ.10.OR.IST.EQ.11.OR.IST.EQ.14.OR.IST.EQ.15)             00134480
     *ISI(1) = 1                                                        00134490
      DO 313 I=1,N                                                      00134500
      JERR = 0                                                          00134510
C$      IF (IPRNT.NE.0) GO TO 306                                         00134520
C$      CALL LINES(1)                                                     00134530
C$      WRITE (OUT,1060)                                                  00134540
C$  306 CONTINUE                                                          00134550
      VL = -BIG                                                         00134560
      K = 0                                                             00134570
      IP4 = IP1+(I-1)*NPT1                                              00134580
      DO 314 II=1,NPT1,4                                                00134590
      READ (IAUR,1009) CN,VALS                                          00134600
      IF (CN.EQ.TABEND) GO TO 50                                        00134610
      IV = 0                                                            00134620
      DO 316 III=1,4                                                    00134630
      K = K+1                                                           00134640
      IV = IV+1                                                         00134650
      D(IP4+K) = VALS(IV)                                               00134660
      IF (ISI(1).NE.0) D(IP4+K) = ALOG10(D(IP4+K))                      00134670
      IF (VALS(IV).GE.VL) GO TO 319                                     00134680
      JERR = JERR+1                                                     00134690
  319 VL= VALS(IV)                                                      00134700
      IF (K.EQ.NPT1) GO TO 318                                          00134710
  316 CONTINUE                                                          00134720
  318 IF (IPRNT.NE.0) GO TO 317                                         00134730
      CALL LINES(1)                                                     00134740
      WRITE (OUT,1061) (VALS(III),III=1,IV)                             00134750
  317 IF (JERR.EQ.0) GO TO 314                                          00134760
      CALL LINES(1)                                                     00134770
      WRITE (OUT,1062)                                                  00134780
      IERR = IERR+JERR                                                  00134790
  314 CONTINUE                                                          00134800
  313 CONTINUE                                                          00134810
      JERR = 0                                                          00134820
C$      IF (IPRNT.NE.0) GO TO 307                                         00134830
C$      CALL LINES(1)                                                     00134840
C$      WRITE (OUT,1060)                                                  00134850
C$  307 CONTINUE                                                          00134860
      VL = -BIG                                                         00134870
      K = 0                                                             00134880
      ISI(2) = 0                                                        00134890
      IF (IST.EQ.4.OR.IST.EQ.5.OR.IST.EQ.6.OR.IST.EQ.7                  00134900
     *.OR.IST.EQ.12.OR.IST.EQ.13.OR.IST.EQ.14.OR.IST.EQ.15)             00134910
     *ISI(2) = 1                                                        00134920
      DO 320 I=1,NPT2,4                                                 00134930
      READ (IAUR,1009) CN,VALS                                          00134940
      IF (CN.EQ.TABEND) GO TO 50                                        00134950
      IV = 0                                                            00134960
      DO 321 II=1,4                                                     00134970
      K = K+1                                                           00134980
      IV = IV+1                                                         00134990
      D(IP2+K) = VALS(IV)                                               00135000
      IF (ISI(2).NE.0) D(IP2+K) = ALOG10(D(IP2+K))                      00135010
      IF (VALS(IV).GE.VL) GO TO 322                                     00135020
      JERR = JERR+1                                                     00135030
  322 VL = VALS(IV)                                                     00135040
      IF (K.EQ.NPT2) GO TO 323                                          00135050
  321 CONTINUE                                                          00135060
  323 IF (IPRNT.NE.0) GO TO 324                                         00135070
      CALL LINES(1)                                                     00135080
      WRITE (OUT,1061) (VALS(III),III=1,IV)                             00135090
  324 IF (JERR.EQ.0) GO TO 320                                          00135100
      CALL LINES(1)                                                     00135110
      WRITE (OUT,1062)                                                  00135120
      IERR = IERR+JERR                                                  00135130
  320 CONTINUE                                                          00135140
      N = NPT1*NPT2                                                     00135150
      K = 0                                                             00135160
C$      IF (IPRNT.NE.0) GO TO 308                                         00135170
C$      CALL LINES(1)                                                     00135180
C$      WRITE (OUT,1060)                                                  00135190
C$  308 CONTINUE                                                          00135200
      ISD = 0                                                           00135210
      IF (IST.EQ.1.OR.IST.EQ.3.OR.IST.EQ.5.OR.IST.EQ.7                  00135220
     *.OR.IST.EQ.9.OR.IST.EQ.11.OR.IST.EQ.13.OR.IST.EQ.15)              00135230
     *ISD = 1                                                           00135240
      DO 325 I=1,N,4                                                    00135250
      READ (IAUR,1009) CN,VALS                                          00135260
      IF (CN.EQ.TABEND) GO TO 50                                        00135270
      IV = 0                                                            00135280
      DO 326 II=1,4                                                     00135290
      K = K+1                                                           00135300
      IV = IV+1                                                         00135310
      D(IP3+K) = VALS(IV)                                               00135320
      IF (ISD.NE.0) D(IP3+K) = ALOG10(D(IP3+K))                         00135330
      IF (K.EQ.N) GO TO 327                                             00135340
  326 CONTINUE                                                          00135350
  327 IF (IPRNT.NE.0) GO TO 325                                         00135360
      CALL LINES(1)                                                     00135370
      WRITE (OUT,1061) (VALS(III),III=1,IV)                             00135380
  325 CONTINUE                                                          00135390
      IF (NDIM.EQ.4 .OR. NDIM.EQ.44) GO TO 454                          00135400
      READ (IAUR,1003) CARD                                             00135410
      IF (CARD(1).NE.TABEND) GO TO 40                                   00135420
      IF (IERR.GT.0) GO TO 51                                           00135430
      ID(ITAD+J) = LOC                                                  00135440
      GO TO 1                                                           00135450
C     FOUR DIMENSIONAL TABLE                                            00135460
  400 CONTINUE                                                          00135470
      IE = IEI1/100                                                     00135480
      II = IEI1-IE*100                                                  00135490
      IO = 0                                                            00135500
      INT = II/10                                                       00135510
      II = II-10*INT                                                    00135520
      IO = IO+INT                                                       00135530
      READ (IAUR,1003) CARD                                             00135540
      IF (IPRNT.NE.0) GO TO 429                                         00135550
      CALL LINES(1)                                                     00135560
      WRITE (OUT,1008) (CARD(I),I=2,11)                                 00135570
      CALL LINES(1)                                                     00135580
      WRITE (OUT,1022) NDIM,IST                                         00135590
  429 IF (IST.GE.0.AND.IST.LE.15) GO TO 428                             00135600
      CALL LINES(1)                                                     00135610
      WRITE (OUT,1010)                                                  00135620
      IERR = 1                                                          00135630
  428 IF (IPRNT.NE.0) GO TO 403                                         00135640
      I = 1                                                             00135650
      CALL LINES(1)                                                     00135660
      WRITE (OUT,1020) I,NAT1,NAR1,IE,II,NPT1                           00135670
  403 ASSIGN 401 TO IS                                                  00135680
      JERR = 0                                                          00135690
      I = 5                                                             00135700
      GO TO 30                                                          00135710
  401 IERR = IERR+JERR                                                  00135720
      IP1 = L                                                           00135730
      IE = IEI2/100                                                     00135740
      II = IEI2-IE*100                                                  00135750
      INT = II/10                                                       00135760
      II = II-10*INT                                                    00135770
      IO = IO+INT                                                       00135780
      IF (IPRNT.NE.0) GO TO 404                                         00135790
      CALL LINES(1)                                                     00135800
      I = 2                                                             00135810
      WRITE (OUT,1020) I,NAT2,NAR2,IE,II,NPT2                           00135820
  404 ASSIGN 402 TO IS                                                  00135830
      JERR = 0                                                          00135840
      I = 9                                                             00135850
      GO TO 30                                                          00135860
  402 IERR = IERR+JERR                                                  00135870
      IP2 = L                                                           00135880
      IE = IEI3/100                                                     00135890
      II = IEI3-IE*100                                                  00135900
      INT = II/10                                                       00135910
      II = II-10*INT                                                    00135920
      IO = IO+INT                                                       00135930
      IF (IPRNT.NE.0) GO TO 430                                         00135940
      CALL LINES(1)                                                     00135950
      I = 3                                                             00135960
      WRITE (OUT,1020) I,NAT3,NAR3,IE,II,NPT3                           00135970
      IF (NDIM.EQ.4) GO TO 430                                          00135980
      IF (IO.EQ.0) GO TO 430                                            00135990
      WRITE (OUT,1023)                                                  00136000
  430 ASSIGN 431 TO IS                                                  00136010
      JERR = 0                                                          00136020
      I = 13                                                            00136030
      GO TO 30                                                          00136040
  431 IERR = IERR+JERR                                                  00136050
      IP3 = L                                                           00136060
      IF (JERR.EQ.0) GO TO 405                                          00136070
      CALL LINES(2)                                                     00136080
      WRITE (OUT,1021)                                                  00136090
      GO TO 4                                                           00136100
  405 IF (NDIM.EQ.44) GO TO 409                                         00136110
      NLOC = 15+NPT3+NPT3*(NPT1+NPT2+NPT1*NPT2)                         00136120
      GO TO 410                                                         00136130
  409 NLOC = 15+NPT3+NPT3*(NPT2+2*NPT1*NPT2)                            00136140
  410 CALL GDCU(NLOC,4,ISP,D,LOC)                                       00136150
      ID(LOC+1) = NDIM                                                  00136160
      ID(LOC+2) = IP1                                                   00136170
      ID(LOC+3) = IEI1                                                  00136180
      ID(LOC+4) = NPT1                                                  00136190
      ID(LOC+5) = IP2                                                   00136200
      ID(LOC+6) = IEI2                                                  00136210
      ID(LOC+7) = NPT2                                                  00136220
      ID(LOC+8) = IP3                                                   00136230
      ID(LOC+9) = IEI3                                                  00136240
      ID(LOC+10) = NPT3                                                 00136250
      IP4 = LOC+15                                                      00136260
      ID(LOC+11) = IP4+1                                                00136270
      IP1 = IP4+NPT3                                                    00136280
      ID(LOC+12) = IP1+1                                                00136290
      IF (NDIM.EQ.44) GO TO 411                                         00136300
      IP2 = IP1+NPT1                                                    00136310
      IP3 = IP2+NPT2                                                    00136320
      GO TO 412                                                         00136330
  411 IP2 = IP1+NPT1*NPT2                                               00136340
      IP3 = IP2+NPT2                                                    00136350
  412 ID(LOC+13) = IP2+1                                                00136360
      ID(LOC+14) = IP3+1                                                00136370
      ID(LOC+15) = IST                                                  00136380
      ISI(3) = 0                                                        00136390
      IF (IST.GE.8.AND.IST.LE.15) ISI(3) = 1                            00136400
      JERR = 0                                                          00136410
C$      IF (IPRNT.NE.0) GO TO 406                                         00136420
C$      CALL LINES(1)                                                     00136430
C$      WRITE (OUT,1060)                                                  00136440
C$  406 CONTINUE                                                          00136450
      VL = -BIG                                                         00136460
      K = 0                                                             00136470
      DO 432 I=1,NPT3,4                                                 00136480
      READ (IAUR,1009) CN,VALS                                          00136490
      IF (CN.EQ.TABEND) GO TO 50                                        00136500
      IV = 0                                                            00136510
      DO 433 II=1,4                                                     00136520
      K = K+1                                                           00136530
      IV = IV+1                                                         00136540
      D(IP4+K) = VALS(IV)                                               00136550
      IF (ISI(3).NE.0) D(IP4+K) = ALOG10(D(IP4+K))                      00136560
      IF (VALS(IV).GE.VL) GO TO 434                                     00136570
      JERR = JERR+1                                                     00136580
  434 VL = VALS(IV)                                                     00136590
      IF (K.EQ.NPT3) GO TO 435                                          00136600
  433 CONTINUE                                                          00136610
  435 IF (IPRNT.NE.0) GO TO 436                                         00136620
      CALL LINES(1)                                                     00136630
      WRITE (OUT,1061) (VALS(III),III=1,IV)                             00136640
  436 IF (JERR.EQ.0) GO TO 432                                          00136650
      CALL LINES(1)                                                     00136660
      WRITE (OUT,1062)                                                  00136670
      IERR = IERR+JERR                                                  00136680
  432 CONTINUE                                                          00136690
      IF (NDIM.EQ.44) GO TO 452                                         00136700
      NI = NPT1+NPT2+NPT1*NPT2                                          00136710
      GO TO 453                                                         00136720
  452 NI = NPT2+2*NPT1*NPT2                                             00136730
  453 IP1 = IP1-NI                                                      00136740
      IP2 = IP2-NI                                                      00136750
      IP3 = IP3-NI                                                      00136760
c$was DO 451 IIII=1,NPT3                                                00136770
      IIII=0
 4444 IIII=IIII+1             
      IP1 = IP1+NI                                                      00136780
      IP2 = IP2+NI                                                      00136790
      IP3 = IP3+NI                                                      00136800
      GO TO 450
  454 CONTINUE
      IF (IIII.EQ.NPT3) GO TO 451
      GO TO 4444
  451 CONTINUE                                                          00136820
      READ (IAUR,1003) CARD                                             00136830
      IF (CARD(1).NE.TABEND) GO TO 40                                   00136840
      IF (IERR.GT.0) GO TO 51                                           00136850
      ID(ITAD+J) = LOC                                                  00136860
      GO TO 1                                                           00136870
C     ARRAY TABLE                                                       00136880
   15 CONTINUE                                                          00136890
      READ (IAUR,1003) CARD                                             00136900
      IF (IPRNT.NE.0) GO TO 16                                          00136910
      CALL LINES(1)                                                     00136920
      WRITE (OUT,1008) (CARD(I),I=2,11)                                 00136930
      CALL LINES(1)                                                     00136940
      WRITE (OUT,1015) NDIM,NPT1                                        00136950
 1015 FORMAT(1H ,4X,4HNDIM,I6,5X,4HNPTS,I5)                             00136960
   16 ASSIGN 17 TO IS                                                   00136970
      I = 5                                                             00136980
      JERR = 0                                                          00136990
      GO TO 38                                                          00137000
   17 IERR = IERR+JERR                                                  00137010
      IF (JERR.EQ.0) GO TO 18                                           00137020
      CALL LINES(2)                                                     00137030
      WRITE (OUT,1021)                                                  00137040
      GO TO 4                                                           00137050
   18 NLOC = 3+NPT1                                                     00137060
      CALL GDCU(NLOC,4,ISP,D,LOC)                                       00137070
      ID(LOC+1) = NDIM                                                  00137080
      ID(LOC+2) = NPT1                                                  00137090
      IP1 = LOC+3                                                       00137100
      ID(LOC+3) = IP1                                                   00137110
      K = 0                                                             00137120
C$      IF (IPRNT.NE.0) GO TO 22                                          00137130
C$      CALL LINES(1)                                                     00137140
C$      WRITE (OUT,1060)                                                  00137150
C$   22 CONTINUE                                                          00137160
      DO 19 I=1,NPT1,4                                                  00137170
      READ (IAUR,1009) CN,VALS                                          00137180
      IF (CN.EQ.TABEND) GO TO 50                                        00137190
      IV = 0                                                            00137200
      DO 20 II=1,4                                                      00137210
      K = K+1                                                           00137220
      IV = IV+1                                                         00137230
      D(IP1+K) = VALS(IV)                                               00137240
      IF (K.EQ.NPT1) GO TO 21                                           00137250
   20 CONTINUE                                                          00137260
   21 IF (IPRNT.NE.0) GO TO 19                                          00137270
      CALL LINES(1)                                                     00137280
      WRITE (OUT,1061) (VALS(III),III=1,IV)                             00137290
   19 CONTINUE                                                          00137300
      READ (IAUR,1003) CARD                                             00137310
      IF (CARD(1).NE.TABEND) GO TO 40                                   00137320
      IF (IERR.GT.0) GO TO 51                                           00137330
      ID(ITAD+J) = LOC                                                  00137340
      GO TO 1                                                           00137350
C     CHECK ARGUMENT, EXTRAP0LATION, INTERP0LATION CODES                00137360
   30 CONTINUE                                                          00137370
      K = ICV(I)                                                        00137380
      L = ICV(I+1)                                                      00137390
      IF (K.GE.0.AND.K.LE.4) GO TO 31                                   00137400
      CALL LINES(2)                                                     00137410
      WRITE (OUT,1050)                                                  00137420
 1050 FORMAT(28H0ERROR - ARGUMENT TYPE WRONG)                           00137430
      IERR = IERR+1                                                     00137440
   31 K = K+1                                                           00137450
      IF (L.LE.0) GO TO 35                                              00137460
      GO TO (32,33,34,43,44), K                                         00137470
   32 IF (L.GT.NGA) GO TO 35                                            00137480
      L = IGA+L                                                         00137490
      GO TO 39                                                          00137500
   33 M = IW                                                            00137510
      DO 36 K=1,NLEG                                                    00137520
      N = K                                                             00137530
      IF (L.EQ.ID(ILN+K)) GO TO 41                                      00137540
   36 CONTINUE                                                          00137550
      GO TO 35                                                          00137560
   34 M = IP                                                            00137570
      GO TO 45                                                          00137580
   43 M = IT                                                            00137590
      GO TO 45                                                          00137600
   44 M = IH                                                            00137610
   45 DO 42 K=1,NSTA                                                    00137620
      N = K                                                             00137630
      IF (L.EQ.ID(ISN+K)) GO TO 41                                      00137640
   42 CONTINUE                                                          00137650
      GO TO 35                                                          00137660
   41 L = M+N                                                           00137670
      GO TO 39                                                          00137680
   35 CALL LINES(2)                                                     00137690
      WRITE (OUT,1051)                                                  00137700
 1051 FORMAT(28H0ERROR - ARGUMENT RELN WRONG)                           00137710
      IERR = IERR+1                                                     00137720
      L = 0                                                             00137730
   39 IF (IE.GE.0 .AND. IE.LE.3) GO TO 46                               00137740
      CALL LINES(2)                                                     00137750
      WRITE (OUT,1056)                                                  00137760
 1056 FORMAT(30H0ERROR - ARGUMENT EXTRAP WRONG)                         00137770
      IERR = IERR+1                                                     00137780
   46 IF (II.LE.7) GO TO 38                                             00137790
      CALL LINES(2)                                                     00137800
      WRITE (OUT,1052)                                                  00137810
 1052 FORMAT(30H0ERROR - ARGUMENT INTERP WRONG)                         00137820
      IERR = IERR+1                                                     00137830
   38 IF (ICV(I+3).GT.1) GO TO 37                                       00137840
      CALL LINES(2)                                                     00137850
      WRITE (OUT,1053)                                                  00137860
 1053 FORMAT(28H0ERROR - ARGUMENT NPTS WRONG)                           00137870
      JERR = JERR+1                                                     00137880
   37 GO TO IS                                                          00137890
   40 CONTINUE                                                          00137900
      CALL LINES(2)                                                     00137910
      WRITE (OUT,1014)                                                  00137920
 1014 FORMAT(38H0ERROR - TABLE HAS TOO MANY TABV CARDS)                 00137930
      CALL FDC(NLOC,4,ISP,D,LOC)                                        00137940
      GO TO 6                                                           00137950
   50 CONTINUE                                                          00137960
      CALL LINES(2)                                                     00137970
      WRITE (OUT,1054)                                                  00137980
 1054 FORMAT(37H0ERROR - TABLE HAS TOO FEW TABV CARDS)                  00137990
   51 CONTINUE                                                          00138000
      CALL FDC(NLOC,4,ISP,D,LOC)                                        00138010
      GO TO 1                                                           00138020
   60 IC = 1                                                            00138030
      GO TO 2                                                           00138040
C     PTAB CARD                                                         00138050
   70 CONTINUE                                                          00138060
      IF (NTID.EQ.0) GO TO 73                                           00138070
      IDT = 100*NREL+NTYP                                               00138080
      DO 71 I=1,NTID                                                    00138090
      J = I                                                             00138100
      IF (ID(ITN+I).EQ.IDT) GO TO 72                                    00138110
   71 CONTINUE                                                          00138120
   73 IF (IPRNT.NE.0) GO TO 2                                           00138130
      CALL LINES(1)                                                     00138140
      WRITE (OUT,1001)                                                  00138150
      CALL LINES(1)                                                     00138160
      WRITE (OUT,1002) CN,ICV                                           00138170
      GO TO 2                                                           00138180
   72 IF (ID(ITAD+J).EQ.0) GO TO 74                                     00138190
   75 IF (IPRNT.NE.0) GO TO 2                                           00138200
      IF (IC.NE.0) GO TO 2                                              00138210
      CALL LINES(2)                                                     00138220
      WRITE (OUT,1005)                                                  00138230
      CALL LINES(1)                                                     00138240
      WRITE (OUT,1002) CN,ICV                                           00138250
      GO TO 2                                                           00138260
   74 IDT = 100*ICV(3)+ICV(4)                                           00138270
      ID(ITAD+J) = -IDT                                                 00138280
      NPT = NPT+1                                                       00138290
      IF (IPRNT.NE.0) GO TO 2                                           00138300
      CALL LINES(2)                                                     00138310
      WRITE (OUT,1018) NREL,NTYP,ICV(3),ICV(4)                          00138320
 1018 FORMAT(7H TABLE ,2I6/5X,5HPTAB ,2I6)                              00138330
      GO TO 2                                                           00138340
C     STAB CARD                                                         00138350
   80 CONTINUE                                                          00138360
      IF (NTID.EQ.0) GO TO 83                                           00138370
      IDT = 100*NREL+NTYP                                               00138380
      DO 81 I=1,NTID                                                    00138390
      J = I                                                             00138400
      IF (ID(ITN+I).EQ.IDT) GO TO 82                                    00138410
   81 CONTINUE                                                          00138420
   83 IF (IPRNT.NE.0) GO TO 2                                           00138430
      CALL LINES(1)                                                     00138440
      WRITE (OUT,1001)                                                  00138450
      CALL LINES(1)                                                     00138460
      WRITE (OUT,1002) CN,ICV                                           00138470
      GO TO 2                                                           00138480
   82 IF (ID(ITAD+J).NE.0) GO TO 75                                     00138490
      IDT = 100*ICV(3)+ICV(4)                                           00138500
      IF (IPRNT.NE.0) GO TO 86                                          00138510
      CALL LINES(2)                                                     00138520
      WRITE (OUT,1017) NREL,NTYP,ICV(3),ICV(4)                          00138530
 1017 FORMAT(7H TABLE ,2I6/5X,5HSTAB ,2I6)                              00138540
   86 CONTINUE                                                          00138550
      DO 84 I=1,NTID                                                    00138560
      K = I                                                             00138570
      IF (ID(ITN+I).EQ.IDT) GO TO 85                                    00138580
   84 CONTINUE                                                          00138590
      D(ITAD+J) = BIG                                                   00138600
      CALL LINES(2)                                                     00138610
      WRITE (OUT,1016)                                                  00138620
 1016 FORMAT(27H0ERROR - SAME TABLE MISSING)                            00138630
      GO TO 2                                                           00138640
   85 ID(ITAD+J) = -ID(ITN+K)-1000000                                   00138650
      NST = NST+1                                                       00138660
      GO TO 2                                                           00138670
   90 CONTINUE                                                          00138680
      IF (NTID.EQ.0) GO TO 99                                           00138690
      IDT = 100                                                         00138700
      DO 91 I=1,NTID                                                    00138710
      J = I                                                             00138720
      IF (ID(ITN+I).EQ.IDT) GO TO 92                                    00138730
   91 CONTINUE                                                          00138740
      GO TO 93                                                          00138750
   92 IF (ID(ITAD+J).NE.0) GO TO 93                                     00138760
      NPT = NPT + 1                                                     00138770
      ID(ITAD+J) = -IDT                                                 00138780
   93 CONTINUE                                                          00138790
C     RESOLVE PERMANENT TABLES                                          00138800
      IF (NPT.GT.0) CALL PTABR                                          00138810
C     RESOLVE SAME TABLES                                               00138820
      IF (NST.GT.0) CALL STABR                                          00138830
C     TABLE CHECK                                                       00138840
      CALL TABC                                                         00138850
   99 CONTINUE                                                          00138860
      RETURN                                                            00138870
C     TABR                                                              00138880
      END                                                               00138890
      FUNCTION TLUP(N)                                                  00144020
      COMMON /CC/ C(600)                                                00144030
      EQUIVALENCE (ITAD,C(54)), (IFP,C(22)), (OUT,C(7)), (ITN,C(39))    00144040
     *, (CERR,C(16)), (IDS,C(5))                                        00144050
      INTEGER OUT, CERR                                                 00144060
      DIMENSION CD(200)                                                 00144070
       REAL*8    CD,  CN                                                 00144080
      EQUIVALENCE (CD(1),C(201))                                        00144090
      EQUIVALENCE (CN,CD(84))                                           00144100
      COMMON /DC/ DZ(2),D(128001)                                            00144110
      DIMENSION ID(2)                                                   00144120
      EQUIVALENCE (ID(1),D(1))                                          00144130
      DIMENSION A(3), YY(8)                                             00144140
      IAD = ID(ITAD+N)                                                  00144150
      NDIM = ID(IAD+1)                                                  00144160
      IF (NDIM.LE.0) GO TO 100                                          00144170
      IF (NDIM.NE.1) GO TO 10                                           00144180
      VAL = D(IAD+2)                                                    00144190
      GO TO 99                                                          00144200
   10 IXA = ID(IAD+2)                                                   00144210
      IEX = ID(IAD+3)                                                   00144220
      NX = ID(IAD+4)                                                    00144230
      IF (NDIM.NE.2) GO TO 20                                           00144240
      IX = ID(IAD+5)                                                    00144250
      IY = ID(IAD+6)                                                    00144260
      IST = ID(IAD+7)                                                   00144270
      VX = D(IXA)                                                       00144280
      IF (IST.EQ.0 .OR. IST.EQ.1) GO TO 11                              00144290
      IF (VX.LT.1.18E-38) VX = 1.18E-38                                 00144300
C*    IF (VX.LT.1.0E-75) VX = 1.0E-75                                   00144300
      VX = ALOG10(VX)                                                   00144310
   11 CALL MDSCT(NDIM,IND,VAL,D(IY),VX,D(IX),NX,IEX,DV,DV,DV,DV)        00144320
      IF (IST.EQ.0.OR.IST.LE.2) GO TO 98                                00144330
      IF (VAL.LT.-75.0) VAL = -75.0                                     00144340
      VAL = 10.0**VAL                                                   00144350
      GO TO 98                                                          00144360
   20 IZA = ID(IAD+5)                                                   00144370
      IEZ = ID(IAD+6)                                                   00144380
      NZ = ID(IAD+7)                                                    00144390
      IF (NDIM.NE.3 .AND. NDIM.NE.33) GO TO 30                          00144400
      IX = ID(IAD+8)                                                    00144410
      IZ = ID(IAD+9)                                                    00144420
      IY = ID(IAD+10)                                                   00144430
      IST = ID(IAD+11)                                                  00144440
      VX = D(IXA)                                                       00144450
      IF (IST.EQ.0.OR.IST.EQ.1.OR.IST.EQ.4.OR.IST.EQ.5) GO TO 21        00144460
      IF (VX.LT.1.18E-38) VX = 1.18E-38                                 00144470
C*    IF (VX.LT.1.0E-75) VX = 1.0E-75                                   00144470
      VX = ALOG10(VX)                                                   00144480
   21 VZ = D(IZA)                                                       00144490
      IF (IST.EQ.0.OR.IST.EQ.1.OR.IST.EQ.2.OR.IST.EQ.3) GO TO 22        00144500
      IF (VZ.LT.1.18E-38) VZ = 1.18E-38                                 00144510
C*    IF (VZ.LT.1.0E-75) VZ = 1.0E-75                                   00144510
      VZ = ALOG10(VZ)                                                   00144520
   22 CALL MDSCT(NDIM,IND,VAL,D(IY),VX,D(IX),NX,IEX,VZ,D(IZ),NZ,IEZ)    00144530
      IF (IST.EQ.0.OR.IST.EQ.2.OR.IST.EQ.4.OR.IST.EQ.6) GO TO 98        00144540
      IF (VAL.LT.-75.0) VAL = -75.0                                     00144550
      VAL = 10.0**VAL                                                   00144560
      GO TO 98                                                          00144570
   30 IWA = ID(IAD+8)                                                   00144580
      IEW = ID(IAD+9)                                                   00144590
      NW = ID(IAD+10)                                                   00144600
      IW = ID(IAD+11)                                                   00144610
      IX = ID(IAD+12)                                                   00144620
      IZ = ID(IAD+13)                                                   00144630
      IY = ID(IAD+14)                                                   00144640
      IST = ID(IAD+15)                                                  00144650
      VX = D(IXA)                                                       00144660
      IF (IST.EQ.0.OR.IST.EQ.1.OR.IST.EQ.4.OR.IST.EQ.5                  00144670
     *.OR.IST.EQ.8.OR.IST.EQ.9.OR.IST.EQ.12.OR.IST.EQ.13) GO TO 31      00144680
      IF (VX.LT.1.18E-38) VX = 1.18E-38                                 00144690
C*    IF (VX.LT.1.0E-75) VX = 1.0E-75                                   00144690
      VX = ALOG10(VX)                                                   00144700
   31 VZ = D(IZA)                                                       00144710
      IF (IST.EQ.0.OR.IST.EQ.1.OR.IST.EQ.2.OR.IST.EQ.3                  00144720
     *.OR.IST.EQ.8.OR.IST.EQ.9.OR.IST.EQ.10.OR.IST.EQ.11) GO TO 32      00144730
      IF (VZ.LT.1.18E-38) VZ = 1.18E-38                                 00144740
C*    IF (VZ.LT.1.0E-75) VZ = 1.0E-75                                   00144740
      VZ = ALOG10(VZ)                                                   00144750
   32 VW = D(IWA)                                                       00144760
      IF (IST.EQ.0.OR.IST.EQ.1.OR.IST.EQ.2.OR.IST.EQ.3                  00144770
     *.OR.IST.EQ.4.OR.IST.EQ.5.OR.IST.EQ.6.OR.IST.EQ.7) GO TO 33        00144780
      IF (VW.LT.1.18E-38) VX = 1.18E-38                                 00144790
C*    IF (VW.LT.1.0E-75) VX = 1.0E-75                                   00144790
      VW = ALOG10(VW)                                                   00144800
   33 IE = IEW/100                                                      00144810
      II = IEW-IE*100                                                   00144820
      II = II-10*(II/10)                                                00144830
      IND = 0                                                           00144840
      VWA = VW                                                          00144850
      CALL MDISSR(VWA,D(IW),1,NW,II,NPW,IE,INDX)                        00144860
      IF (INDX.NE.0) IND = 100                                          00144870
      J = NPW-1                                                         00144880
      NLW = II+1                                                        00144890
      IF (NDIM.EQ.44) GO TO 36                                          00144900
      NI = NX+NZ+NX*NZ                                                  00144910
      GO TO 37                                                          00144920
   36 NI = NZ+2*NX*NZ                                                   00144930
   37 DO 38 I=1,NLW                                                     00144940
      J = J+1                                                           00144950
      NII = (J-1)*NI                                                    00144960
      IXI = IX+NII                                                      00144970
      IZI = IZ+NII                                                      00144980
      IYI = IY+NII                                                      00144990
      CALL MDSCT(NDIM,INDX,YY(I),D(IYI),VX,D(IXI),NX,IEX,VZ,D(IZI),NZ,  00145000
     *IEZ)                                                              00145010
      IND = IND+INDX                                                    00145020
   38 CONTINUE                                                          00145030
      VAL = FLAGRA(VWA,D(IW+NPW-1),YY,NLW)                              00145040
      IF (IST.EQ.0.OR.IST.EQ.2.OR.IST.EQ.4.OR.IST.EQ.6                  00145050
     *.OR.IST.EQ.8.OR.IST.EQ.10.OR.IST.EQ.12.OR.IST.EQ.14) GO TO 98     00145060
      IF (VAL.LT.-75.0) VAL = -75.0                                     00145070
      VAL = 10.0**VAL                                                   00145080
   98 CONTINUE                                                          00145090
      IF (IFP.EQ.0) GO TO 99                                            00145100
      IDR = ID(ITN+N)/100                                               00145110
      IDT = ID(ITN+N)-IDR*100                                           00145120
      NDM1 = NDIM-1                                                     00145130
      IF (NDIM.EQ.33) NDM1 = 2                                          00145140
      IF (NDIM.EQ.44) NDM1 = 3                                          00145150
      A(1) = D(IXA)                                                     00145160
      IF (NDIM.NE.2) A(2) = D(IZA)                                      00145170
      IF (NDIM.EQ.4 .OR. NDIM.EQ.44) A(3) = D(IWA)                      00145180
      IF (IND.EQ.0) GO TO 96                                            00145190
      CALL LINES(2)                                                     00145200
      WRITE (OUT,1001) CN,IDR,IDT,VAL,(A(I),I=1,NDM1)                   00145210
 1001 FORMAT(11H0COMPONENT ,A6,20H EXTRAPOLATED TABLE ,2I6,9H VALUE = , !8/98  00145220
     *1PE12.5,13H ARGUMENT(S) ,3E12.5)                                  00145230
      GO TO 99                                                          00145240
   96 CONTINUE                                                          00145250
      IF (IDS.EQ.0) GO TO 99                                            00145260
      CALL LINES(1)                                                     00145270
      WRITE (OUT,1003) CN,IDR,IDT,VAL,(A(I),I=1,NDM1)                   00145280
 1003 FORMAT(11H COMPONENT ,A6,20H              TABLE ,2I6,9H VALUE = , 00145290
     *1PE12.5,13H ARGUMENT(S) ,3E12.5)                                  00145300
   99 CONTINUE                                                          00145310
      TLUP = VAL                                                        00145320
      RETURN                                                            00145330
  100 VAL = 1.0                                                         00145340
      CERR = CERR+1                                                     00145350
      IDR = ID(ITN+N)/100                                               00145360
      IDT = ID(ITN+N)-IDR*100                                           00145370
      CALL LINES(2)                                                     00145380
      WRITE (OUT,1002) CERR,CN,IDR,IDT                                  00145390
 1002 FORMAT(6H0ERROR,I6,5X,10HCOMPONENT ,A6,14H INVALID TABLE,2I6)     00145400
      GO TO 99                                                          00145410
C     TLUP                                                              00145420
      END                                                               00145430
      SUBROUTINE WRTEA (CARD)                                           00157040
      COMMON /CC/ C(600)                                                00157050
      EQUIVALENCE (IAUI,C(20)), (TYPE,C(1)), (OUT,C(7)), (IDIM,C(24))   00157060
     *, (IFP,C(22))                                                     00157070
      INTEGER TYPE, OUT                                                 00157080
       REAL*8    CARD                                                   00157090
      DIMENSION CARD(11)                                                00157100
      WRITE (IAUI,1000) CARD                                            00157110
 1000 FORMAT(A6,9A8,A2)                                                 00157120
      IF (IDIM.EQ.0) GO TO 98                                           00157130
      CALL LINES(1)                                                     00157140
      WRITE (OUT,900) CARD                                              00157150
  900 FORMAT(2H *,A6,9A8,A2,1H*)                                        00157160
   98 IF (TYPE.GE.0) GO TO 99                                           00157170
      IF (IFP.NE.0) GO TO 99                                            00157180
      CALL LINES(1)                                                     00157190
      WRITE (OUT,1002) CARD                                             00157200
 1002 FORMAT(8H CHANGE*,A6,9A8,A2,1H*)                                  00157210
   99 CONTINUE                                                          00157220
      RETURN                                                            00157230
C     WRTEA                                                             00157240
      END                                                               00157250
      SUBROUTINE WRTEB (CARD)                                           00157260
      COMMON /CC/ C(600)                                                00157270
      EQUIVALENCE (IAUR,C(19)), (IAUI,C(20)), (IAUB,C(21))              00157280
     *, (IDIM,C(24)), (OUT,C(7)), (NCOMP,C(44))                         00157290
      INTEGER OUT                                                       00157300
      DIMENSION CD(200)                                                 00157310
      REAL*8    CD, CMPNNT, CARD                                        00157320
      EQUIVALENCE (CD(1),C(201))                                        00157330
      EQUIVALENCE (CMPNNT,CD(39))                                       00157340
      DIMENSION CARD(11)                                                00157350
      WRITE (IAUR,1000) CARD                                            00157360
 1000 FORMAT(A6,9A8,A2)                                                 00157370
      IF (IDIM.EQ.0) GO TO 99                                           00157380
      CALL LINES(1)                                                     00157390
      WRITE (OUT,900) CARD                                              00157400
  900 FORMAT(2H *,A6,9A8,A2,1H*)                                        00157410
   99 CONTINUE                                                          00157420
      RETURN                                                            00157430
      ENTRY WRTEC (CARD)                                                00157440
      WRITE (IAUR,1001) CARD                                            00157450
 1001 FORMAT(A6,9A8,A2)                                                 00157460
      IF (CARD(1).NE.CMPNNT) NCOMP = NCOMP+1                            00157470
      IF (IDIM.EQ.0) GO TO 99                                           00157480
      CALL LINES(1)                                                     00157490
      WRITE (OUT,901) CARD                                              00157500
  901 FORMAT(2H *,A6,9A8,A2,1H*)                                        00157510
      GO TO 99                                                          00157520
C     WRTEB, WRTEC                                                      00157530
      END                                                               00157540
C$    SUBROUTINE MPERF MODIFIED FOR OTHER PROGRAM CHANGES
      SUBROUTINE MPERF
C     OVERLAY (AECS,1,0)
C     PROGRAM O10
      COMMON /CC/ C(600)
      EQUIVALENCE (OUT,C(7)), (CERR,C(16)), (PERR,C(8))
      INTEGER OUT, CERR, PERR
      DIMENSION CD(200)
      REAL * 8  CD
      REAL * 8  V, QCNG
      EQUIVALENCE (CD(1),C(201))
      EQUIVALENCE (V(1),CD(51)), (QCNG,CD(8))
      DIMENSION V(11)
C$    COMMON /CCA/ CA(150)
      COMMON /CCA/ CA(150)
      EQUIVALENCE (IPSP,CA(2))
      IQC = 0
C   1 CALL OVERLAY(4HAECS,1,1,0)
    1 CALL PMM
      IQC = CERR
C   2 CALL OVERLAY(4HAECS,1,2,0)
    2 CALL PZM
      IQC = CERR
C   3 CALL OVERLAY(4HAECS,1,3,6HRECALL)
    3 CALL PPM
      IF (QCNG.NE.V(1)) GO TO 99
      CALL PQC(IQC)
      IF (IQC.LT.0) GO TO 99
      GO TO 3
   99 CONTINUE
      PERR = CERR
      WRITE (OUT,1001)
 1001 FORMAT(1H1)
      RETURN
C     MPERF
      END
      FUNCTION IACDB (NUM)
      COMMON /CC/ C(600)
      EQUIVALENCE (ICDB,C(43)), (ISP,C(42)), (TYPE,C(1))
      INTEGER TYPE
C$ WAS  COMMON /CCA/ CA(150)
      COMMON /CCA/ CA(150)
      EQUIVALENCE (NOC,CA(1))
      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(2)
      EQUIVALENCE (ID(1),D(1))
      IF (IABS(TYPE).EQ.1) NADD = 11
      IF (IABS(TYPE).EQ.2) NADD = 4
      NUMT = NUM+NADD
      CALL GDCU(NUMT,4,ISP,D,LOC)
      LOC = LOC+NADD
      ID(ICDB+NOC) = LOC
      ID(LOC) = NOC*10000+NUM
      IACDB = LOC
      RETURN
C     IACDB
      END
      SUBROUTINE PCOMPP
      COMMON /CC/ C(600)
      EQUIVALENCE (ICDB,C(43)), (NCOMP,C(44)), (IATMP,C(84))
     *, (IATMT,C(85)), (IVM,C(86)), (IAP,C(87)), (SCR(1),C(151))
     *, (IGA,C(35)), (ICPP,C(88)), (IFP,C(22))
      DIMENSION SCR(30)
      EQUIVALENCE (SCR(1),GAMMA), (SCR(2),TR)
      DIMENSION CD(200)
      REAL * 8  CD, CN, ATMN
      EQUIVALENCE (CD(1),C(201))
      EQUIVALENCE (CN,CD(84))
C$    COMMON /CCA/ CA(150)
      COMMON /CCA/ CA(150)
      EQUIVALENCE (NOC,CA(1))
      COMMON /CP/ PD(100)
      REAL * 8  PD
      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(2)
      EQUIVALENCE (ID(1),D(1))
      DATA ATMN/3HATM/
      CN = ATMN
      IF (IATMP.NE.0) D(IGA+5) = TLUP(IATMP)
      IF (IATMT.NE.0) D(IGA+6) = TLUP(IATMT)
      IF (IAP.EQ.0) GO TO 5
      CALL FLUIDP(0)
      IF (IVM) 2,5,4
    2 D(IGA+3) = D(IGA+4)/SOS(0,D(IGA+5),D(IGA+6))
      GO TO 3
    4 D(IGA+4) = D(IGA+3)*SOS(0,D(IGA+5),D(IGA+6))
    3 CONTINUE
      GAMMA = GAM(0,D(IGA+5),D(IGA+6))
      TR = 1.0+0.5*(GAMMA-1.0)*D(IGA+3)**2
      D(IGA+8) = D(IGA+6)*TR
      D(IGA+7) = D(IGA+5)*TR**(GAMMA/(GAMMA-1.0))
    5 NOC = 0
    1 NOC = NOC+1
      IF (NOC.GT.NCOMP) GO TO 90
      LOC = ID(ICDB+NOC)
      IF (LOC.EQ.0) GO TO 1
      I = ID(LOC+1)
      CN = PD(I)
      ICPP = ID(LOC-3)
      GO TO (101,102,103,104,105,106,107,108,109,110,111,112,113,114,115
     1,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131
     2,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147
C$    ADDED 151-153
     3,148,149,150,151,152,153
     *),I
  101 CONTINUE
      CALL USERPP
      GO TO 98
  102 CONTINUE
      CALL INLTPP
      GO TO 98
  103 CONTINUE
      CALL OTLTPP
      GO TO 98
  104 CONTINUE
      CALL SPLTPP
      GO TO 98
  105 CONTINUE
      CALL MRGEPP
      GO TO 98
  106 CONTINUE
      CALL HXAPPH
      GO TO 98
  107 CONTINUE
      CALL VLVPP
      GO TO 98
  108 CONTINUE
      CALL CVLVPP
      GO TO 98
  109 CONTINUE
      CALL SENPP
      GO TO 98
  110 CONTINUE
      CALL LINEPP
      GO TO 98
  111 CONTINUE
      CALL COMPPP
      GO TO 98
  112 CONTINUE
      CALL TURBPP
      GO TO 98
  113 CONTINUE
      CALL SHFTPP
      GO TO 98
  114 CONTINUE
      CALL SPOWPP
      GO TO 98
  115 CONTINUE
      CALL QLDPP
      GO TO 98
  116 CONTINUE
      CALL PREGPP
      GO TO 98
  117 CONTINUE
      CALL NZZLPP
      GO TO 98
  118 CONTINUE
      CALL ORIFPP
      GO TO 98
  119 CONTINUE
      CALL HXB1PP
      GO TO 98
  120 CONTINUE
      CALL HXB2PP
      GO TO 98
  121 CONTINUE
      CALL MISCPP
      GO TO 98
  122 CONTINUE
      CALL PUMPPP
      GO TO 98
  123 CONTINUE
      CALL FANPP
      GO TO 98
  124 CONTINUE
      CALL APUPP
      GO TO 98
  125 CONTINUE
      CALL EJCTPP
      GO TO 98
  126 CONTINUE
      CALL VLVPP
      GO TO 98
  127 CONTINUE
      CALL BOILPP
      GO TO 98
  128 CONTINUE
      CALL WSEPPP
      GO TO 98
  129 CONTINUE
      CALL INJTPP
      GO TO 98
  130 CONTINUE
      CALL LPSPP
      GO TO 98
  131 CONTINUE
      CALL LPEPP
      GO TO 98
  132 CONTINUE
      CALL CNNTPP
      GO TO 98
  133 CONTINUE
      CALL DRNPP
      GO TO 98
  134 CONTINUE
      CALL VLNEPP
      GO TO 98
  135 CONTINUE
      CALL VCMPPP
      GO TO 98
  136 CONTINUE
      CALL CONDPP
      GO TO 98
  137 CONTINUE
      CALL EVAPPP
      GO TO 98
  138 CONTINUE
      CALL XPNDPP
      GO TO 98
  139 CONTINUE
      CALL QBLRPP
      GO TO 98
  140 CONTINUE
      CALL NRLRPP
      GO TO 98
  141 CONTINUE
      CALL RPMPPP
      GO TO 98
  142 CONTINUE
      CALL RNLTPP
      GO TO 98
  143 CONTINUE
      GO TO 98
  144 CONTINUE
      CALL RTLTPP
      GO TO 98
  145 CONTINUE
      CALL SEPRPP
      GO TO 98
  146 CONTINUE
      GO TO 98
  147 CONTINUE
      GO TO 98
  148 CONTINUE
      CALL FUNCPP
      GO TO 98
  149 CONTINUE
      GO TO 98
  150 CONTINUE
      GO TO 98
C$    ADDED 151-153
  151 CONTINUE
      CALL COPPP
      GO TO 98
  152 CONTINUE
      CALL REFPPP
      GO TO 98
  153 CONTINUE
      CALL FLUPPP
   98 CONTINUE
      GO TO 1
   90 CONTINUE
   99 CONTINUE
      RETURN
C     PCOMPP
      END
      SUBROUTINE PCOMPR
      COMMON /CC/ C(600)
      EQUIVALENCE (IAUR,C(19)), (OUT,C(7)), (LC,C(13)), (MDPC,C(34))
     *, (NLEG,C(25)), (ILN,C(37)), (NSTA,C(26)), (ISN,C(38))
     *, (CERR,C(16)), (IDD,C(4)), (NSV,C(50)), (IFB,C(55))
     *, (NEV,C(51)), (ICDB,C(43)), (NCOMP,C(44)), (ISP,C(42))
     *, (ICV(1),C(133)), (NOCP,C(59)), (IPAR,C(33))
     *, (ICPP,C(88)), (SCR(1),C(151))
      DIMENSION ICV(18), SCR(30)
      EQUIVALENCE (ICV(1),K)
      INTEGER OUT, CERR
      DIMENSION CD(200)
      REAL * 8  CD
      EQUIVALENCE (CD(1),C(201))
      REAL * 8  COMEND, CN
      EQUIVALENCE (COMEND,CD(14)), (CN,CD(84))
C$    COMMON /CCA/ CA(150)
      COMMON /CCA/ CA(150)
      EQUIVALENCE (NOC,CA(1))
      COMMON /CP/ PD(100)
      REAL * 8  PD, CMPN
      EQUIVALENCE (CMPN(1),PD(1))
      DIMENSION CMPN(1)
      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(2)
      EQUIVALENCE (ID(1),D(1))
      REAL N130
      DATA N130/4HLOOP/
      IPRNT = 10*(MDPC/10)
      IPRNT = IPRNT-100*(IPRNT/100)
      IF (MDPC.LT.0) IPRNT = -1
      ICNE = 0
      LCR = 0
      ICPPO = 0
      IF (ICPP.EQ.0) ICPPO = 1
      IF (NCOMP.NE.0) CALL GDCU(NCOMP,4,ISP,D,ICDB)
      NOC = 0
      IF (IPRNT.LT.0) GO TO 1
      CALL LINES(100)
      LC = LC+1
      WRITE (OUT,1000)
 1000 FORMAT(13H COMPONENT(S))
      IF (IPRNT.NE.0) GO TO 1
      LC = LC+3
      WRITE (OUT,1003)
 1003 FORMAT(104H0    1    6  8   12   16   20   24   28   32   36   40
     *  44   48   52   56   60   64   68   72   76   80/1H )
    1 READ (IAUR,1200) CN,CC,NC,(ICV(I),I=2,18)
 1200 FORMAT(A6,A2,I4,17A4)
      IF (CN.EQ.COMEND) GO TO 99
      IF (NC.EQ.0) GO TO 5
      IF (NC.LT.LCR) ICNE = 1
      LCR = NC
    5 CONTINUE
      BACKSPACE IAUR
      READ (IAUR,1001) CN,CC,NC,(ICV(I),I=2,18)
 1001 FORMAT(A6,A2,18I4)
      IF (IPRNT.NE.0) GO TO 2
      CALL LINES(1)
      WRITE (OUT,1002) CN,CC,NC,(ICV(I),I=2,18)
 1002 FORMAT(5X,A6,1X,A2,18(1X,I4))
    2 DO 3 I=1,NOCP
      K = I
      IF (CN.EQ.CMPN(I)) GO TO 4
    3 CONTINUE
      IF (NOC.EQ.0) GO TO 1
      LOC = ID(ICDB+NOC)
      IF (ICV(2).GT.0) ID(LOC-3) = ICPPO
      IF (ICV(2).LT.0) ID(LOC-3) = 0
      GO TO 1
    4 CONTINUE
      NOC = NOC+1
      GO TO (101,102,103,104,105,106,107,108,109,110,111,112,113,114,115
     1,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131
     2,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147
C$    ADDED 151-153
     3,148,149,150,151,152,153
     *),K
  101 CONTINUE
      CALL USERPZ
      GO TO 98
  102 CONTINUE
      CALL INLTPZ
      GO TO 98
  103 CONTINUE
      CALL OTLTPZ
      GO TO 98
  104 CONTINUE
      CALL SPLTPZ
      GO TO 98
  105 CONTINUE
      CALL MRGEPZ
      GO TO 98
  106 CONTINUE
      CALL HXAPZ
      GO TO 98
  107 CONTINUE
      CALL VLVPZ
      GO TO 98
  108 CONTINUE
      CALL CVLVPZ
      GO TO 98
  109 CONTINUE
      CALL SENPZ
      GO TO 98
  110 CONTINUE
      CALL LINEPZ
      GO TO 98
  111 CONTINUE
      CALL COMPPZ
      GO TO 98
  112 CONTINUE
      CALL TURBPZ
      GO TO 98
  113 CONTINUE
      CALL SHFTPZ
      GO TO 98
  114 CONTINUE
      CALL SPOWPZ
      GO TO 98
  115 CONTINUE
      CALL QLDPZ
      GO TO 98
  116 CONTINUE
      CALL PREGPZ
      GO TO 98
  117 CONTINUE
      CALL NZZLPZ
      GO TO 98
  118 CONTINUE
      CALL ORIFPZ
      GO TO 98
  119 CONTINUE
      CALL HXB1PZ
      GO TO 98
  120 CONTINUE
      CALL HXB2PZ
      GO TO 98
  121 CONTINUE
      CALL MISCPZ
      GO TO 98
  122 CONTINUE
      CALL PUMPPZ
      GO TO 98
  123 CONTINUE
      CALL FANPZ
      GO TO 98
  124 CONTINUE
      CALL APUPZ
      GO TO 98
  125 CONTINUE
      CALL EJCTPZ
      GO TO 98
  126 CONTINUE
      CALL VLVPZ
      GO TO 98
  127 CONTINUE
      CALL BOILPZ
      GO TO 98
  128 CONTINUE
      CALL WSEPPZ
      GO TO 98
  129 CONTINUE
      CALL INJTPZ
      GO TO 98
  130 CONTINUE
      CALL LPSPZ
      CN = N130
      GO TO 98
  131 CONTINUE
      CALL LPEPZ
      CN = N130
      GO TO 98
  132 CONTINUE
      CALL CNNTPZ
      GO TO 98
  133 CONTINUE
      CALL DRNPZ
      GO TO 98
  134 CONTINUE
      CALL VLNEPZ
      GO TO 98
  135 CONTINUE
      CALL VCMPPZ
      GO TO 98
  136 CONTINUE
      CALL CONDPZ
      GO TO 98
  137 CONTINUE
      CALL EVAPPZ
      GO TO 98
C$ ADDED 138-142(THESE WERE "CONTINUE" AND "GO TO" WITH NO CALL BETWEEN
  138 CONTINUE
      CALL XPNDPZ
      GO TO 98
  139 CONTINUE
      CALL QBLRPZ
      GO TO 98
  140 CONTINUE
      CALL NRLRPZ
      GO TO 1
  141 CONTINUE
      CALL RPMPPZ
      GO TO 1
  142 CONTINUE
      CALL RNLTPZ
      GO TO 98
  143 CONTINUE
C$ CCSPZ AND FUNCPZ ARE IN OLD FORTRAN, THE REST OF THESE ARE NEW
C$ THE REST OF PCOMPR BELOW THE CALLS IS THE SAME
      CALL CCSPZ
      GO TO 1
  144 CONTINUE
      CALL RTLTPZ
      GO TO 98
  145 CONTINUE
      CALL SEPRPZ
      GO TO 98
  146 CONTINUE
      GO TO 98
  147 CONTINUE
      CALL SOLNPZ
      GO TO 98
  148 CONTINUE
      CALL FUNCPZ
      GO TO 98
  149 CONTINUE
      GO TO 98
  150 CONTINUE
      CALL INDTPZ
      GO TO 98
  151 CONTINUE
      CALL COPPZ
      GO TO 98
  152 CONTINUE
      CALL REFPPZ
      GO TO 98
  153 CONTINUE
      CALL FLUPPZ
   98 CONTINUE
      LOC = ID(ICDB+NOC)
      D(LOC-2) = CN
      ID(LOC-3) = ICPP
      GO TO 1
   99 CONTINUE
      IF (IPRNT.LT.0) GO TO 6
      IF (ICNE.EQ.0) GO TO 6
      CALL LINES(3)
      WRITE (OUT,1004)
 1004 FORMAT(45H0* NOTE - CARD NUMBERS NOT IN ASCENDING ORDER/10X,31HMAY
     * PRODUCE INVALID CHANGE CASE)
    6 CONTINUE
      IF (IDD.EQ.0) GO TO 17
      CALL LINES(NLEG/10+3)
      I1 = ILN+1
      I2 = ILN+NLEG
      WRITE (OUT,1013) (ID(I),I=I1,I2)
 1013 FORMAT(4H0ILN/(1X,10I10))
      CALL LINES(NSTA/10+3)
      I1 = ISN+1
      I2 = ISN+NSTA
      WRITE (OUT,1014) (ID(I),I=I1,I2)
 1014 FORMAT(4H0ISN/(1X,10I10))
      CALL LINES(1)
      WRITE (OUT,1010) IPAR
 1010 FORMAT(5H IPAR,I10)
      IF (ICDB.EQ.0) GO TO 17
      DO 15 J=1,NCOMP
      LOC = ID(ICDB+J)
      IF (LOC.EQ.0) GO TO 15
      NUM = ID(LOC)-(ID(LOC)/10000)*10000
      I1 = LOC+1
      I2 = LOC+NUM
      CALL LINES(NUM/10+3)
      WRITE (OUT,1009) J,LOC,(ID(I),I=I1,I2)
 1009 FORMAT(5H0COMP,I6,5X,I10/(1X,10I10))
   15 CONTINUE
   17 CONTINUE
      IF (IDD.EQ.0) GO TO 18
      IF (IFB.EQ.0) GO TO 18
      I1 = IFB
      I2 = IFB+NLEG
      CALL LINES(NLEG/10+3)
      WRITE (OUT,1011) (ID(I),I=I1,I2)
 1011 FORMAT(4H0IFB/(1X,10I10))
      K = 0
      DO 19 J=K,NLEG
      I1 = ID(IFB+J)
      IF (I1.EQ.0) GO TO 19
      IF (J.EQ.0) GO TO 22
      I2 = J-1
      DO 21 I=K,I2
      IF (I1.EQ.ID(IFB+I)) GO TO 19
   21 CONTINUE
   22 I1 = I1+1
      I2 = I1+1
      CALL LINES(1)
      WRITE (OUT,1012) (ID(I),I=I1,I2)
 1012 FORMAT(1X,2I10)
   19 CONTINUE
   18 CONTINUE
      IF (NSV.EQ.NEV) GO TO 13
      CERR = CERR+1
      CALL LINES(2)
      WRITE (OUT,1007) CERR,NSV,NEV
 1007 FORMAT(6H0ERROR,I6,5X,17HSTATE VARIABLE(S),I4,5X,17HERROR VARIABLE
     *(S),I4,5X,10HUNBALANCED)
      I = MAX0(NSV,NEV)
      NSV = I
      NEV = I
   13 IF (IPRNT.NE.0) GO TO 16
      CALL LINES(2)
      WRITE (OUT,1008) NSV
 1008 FORMAT(28H0    STATE/ERROR VARIABLE(S),I6)
   16 CONTINUE
      RETURN
C     PCOMPR
      END
C$    ASSUME MUCH MODIFIED
C$  MODIFIED TO ALLOW FOR 54 VALUE REFRIGERANT PERM TABLES
      SUBROUTINE TABC
      COMMON /CC/ C(600)
      EQUIVALENCE (NTID,C(40)), (ITN,C(39)), (ITAD,C(54)), (BIG,C(31))
     *, (CERR,C(16)), (OUT,C(7)), (IFB,C(55)), (NLEG,C(25)), (IDD,C(4))
     *, (IW,C(27)), (IP,C(28)), (IT,C(29)), (IH,C(30)), (IGA,C(35))
      INTEGER CERR, OUT
      COMMON /DC/ DZ(2),D(128001)
      DIMENSION ID(2)
      EQUIVALENCE (ID(1),D(1))
      DO 91 I=1,NTID
      IDT = ID(ITN+I)
      NREL = IDT/100
      NTYP = IDT-NREL*100
      IF (D(ITAD+I).EQ.BIG) GO TO 92
      IF (ID(ITAD+I).EQ.0) GO TO 93
      GO TO 91
   92 CALL LINES(2)
      CERR = CERR+1
      WRITE (OUT,1055) CERR,NREL,NTYP
 1055 FORMAT(6H0ERROR,I6,5X,6HTABLE ,2I5,9H REJECTED)
      ID(ITAD+I) = 0
      GO TO 91
   93 IF (NTYP.EQ.0) GO TO 91
      CALL LINES(2)
      CERR = CERR+1
      WRITE (OUT,1101) CERR,NREL,NTYP
 1101 FORMAT(6H0ERROR,I6,5X,6HTABLE ,2I5,8H MISSING)
   91 CONTINUE
      IF (IFB.EQ.0) GO TO 2012
      II = 0
      DO 94 I=II,NLEG
      LOC = ID(IFB+I)
      IF (LOC.EQ.0) GO TO 94
      IF (I.EQ.0) GO TO 95
      K = I-1
      DO 96 J=II,K
      IF (LOC.EQ.ID(IFB+J)) GO TO 94
   96 CONTINUE
   95 J = ID(LOC+1)
      IF (J.EQ.0) GO TO 94
      K = ID(LOC+2)
      IF (K.EQ.0) GO TO 94
      K = ID(LOC+3)
      IF (K.EQ.0) GO TO 94
      L = ID(ITAD+K)
      IF (L.EQ.0) GO TO 94
      M = ID(L+1)
      IF (M.EQ.-1) GO TO 97
      CERR = CERR+1
      J = ID(ITN+K)
      K = J/100
      J = IABS(J-K*100)
      CALL LINES(2)
      WRITE (OUT,1023) CERR,K,J
 1023 FORMAT(6H0ERROR,I6,5X,20HFLUID PROPERTY TABLE,I6,I3,8H NOT -1D)
      GO TO 94
   97 M = ID(L+2)
      GO TO (901,902,903), J
  901 N = 12
      IF (M.EQ.20) GO TO 94
      GO TO 904
  902 N = 25
      IF (M.EQ.37) GO TO 94
      GO TO 904
  903 N = 38
  904 IF (N.EQ.M) GO TO 94
C$  TWO LINES BELOW ADDED TO ALLOW FOR 54 VALUE REFRIGERANT PERM TABLES
C$      NUMBER IS 52 IN NEWZ
      N = 54
      IF(N.EQ.M) GO TO 94
      CERR = CERR+1
      J = ID(ITN+K)
      K = J/100
      J = IABS(J-K*100)
      CALL LINES(2)
      WRITE (OUT,1024) CERR,K,J
 1024 FORMAT(6H0ERROR,I6,5X,20HFLUID PROPERTY TABLE,I6,I3,11H SIZE WRONG
     *)
   94 CONTINUE
 2012 CONTINUE
      IF (IDD.EQ.0) GO TO 99
      CALL LINES(1)
      WRITE (OUT,2020) IW,IP,IT,IH,IGA
 2020 FORMAT(3H IW,I10,3X,2HIP,I10,3X,2HIT,I10,3X,2HIH,I10,3X,3HIGA,I9)
      I1 = ITN+1
      I2 = ITN+NTID
      CALL LINES(NTID/10+3)
      WRITE (OUT,2001) (ID(I),I=I1,I2)
 2001 FORMAT(4H0ITN/(1X,10I10))
      I1 = ITAD+1
      I2 = ITAD+NTID
      CALL LINES(NTID/10+3)
      WRITE (OUT,2002) (ID(I),I=I1,I2)
 2002 FORMAT(5H0ITAD/(1X,10I10))
      DO 2000 I=1,NTID
      J = ID(ITAD+I)
      IF (J.EQ.0) GO TO 2000
      IF (I.EQ.1) GO TO 2013
      I2 = I-1
      DO 2014 K=1,I2
      IF (J.EQ.ID(ITAD+K)) GO TO 2000
 2014 CONTINUE
 2013 NDIM = ID(J+1)
      I1 = J+1
      IF (NDIM.EQ.1) GO TO 2003
      IF (NDIM.EQ.2) GO TO 2004
      IF (NDIM.EQ.3) GO TO 2005
      IF (NDIM.EQ.33) GO TO 2006
      IF (NDIM.EQ.4) GO TO 2015
      IF (NDIM.EQ.44) GO TO 2016
      IF (NDIM.EQ.-1) GO TO 2011
      GO TO 2000
 2011 I2 = J+3+ID(J+2)
      I3 = I1+2
      GO TO 2007
 2003 I2 = J+2
      I3 = I1
      GO TO 2007
 2004 I2 = J+7+2*ID(J+4)
      I3 = I1+6
      GO TO 2007
 2005 I2 = J+11+ID(J+4)+ID(J+7)+ID(J+4)*ID(J+7)
      GO TO 2008
 2006 I2 = J+11+ID(J+7)+2*ID(J+4)*ID(J+7)
 2008 I3 = I1+10
      GO TO 2007
 2015 I2 = J+15+ID(J+10)+ID(J+10)*(ID(J+4)+ID(J+7)+ID(J+4)*ID(J+7))
      GO TO 2017
 2016 I2 = J+15+ID(J+10)+ID(J+10)*(ID(J+7)+2*ID(J+4)*ID(J+7))
 2017 I3 = I1+14
 2007 I4 = I3+1
      CALL LINES((I3-I1)/10+2)
      WRITE (OUT,2009) (ID(K),K=I1,I3)
 2009 FORMAT(1H ,10I10/1X,10I10)
      CALL LINES((I2-I4)/10+2)
      WRITE (OUT,2010) (D(K),K=I4,I2)
 2010 FORMAT(1X,10E12.5)
 2000 CONTINUE
   99 CONTINUE
      RETURN
C     TABC
      END

C*   NORTHROP GRUMMAN PROPRIETARY