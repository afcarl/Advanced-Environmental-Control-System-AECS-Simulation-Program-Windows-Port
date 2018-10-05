C*   NORTHROP GRUMMAN PROPRIETARY
      PROGRAM AECS
C                                                                       00000010
C     IECS GENERAL ECS COMPUTER PROGRAM                                 00000020
C     AECS VERSION                                                      00000030
C                                                                       00000040
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000050
C                                                                       00000060
C     DEVELOPED BY                                                      00000070
C     MCDONNELL AIRCRAFT COMPANY / MCDONNELL DOUGLAS CORPORATION        00000080
C     UNDER AIR FORCE CONTRACT F33615-70-C-1235                         00000090
C                                                                       00000100
C     SPONSORED BY AIR FORCE FLIGHT DYNAMICS LABORATORY, PROJECT 6146   00000110
C     PROJECT ENGINEER  E. A. ZARA (FEE)                                00000120
C                                                                       00000130
C     WRITTEN BY                                                        00000140
C     A. E. WHITNEY,  C. E. WHITMAN,  K. C. LI                          00000150
C                                                                       00000160
C     WRITTEN FOR IBM 360 / CONVERTED TO CDC 6600                       00000170
C                                                                       00000180
C     CONVERTED BACK TO THE IBM/370 BY                                  00000190
C     GENE WILMOT, HAMILTON STANDARD, 23 JAN 1979                       00000200
C                                                                       00000210
C*    CONVERTED TO MS-DOS  BY                                           00000210
C*    DAVE MARTIN, PAT O'ROURKE, STEVE ATMUR, STEVE WEST                00000210
C*    NORTHROP AIRCRAFT, xx JUN 1991                                    00000210
C*    INDICATES CHANGE MADE FOR COMPATIBILITY WITH F77L FOR PC
C
C     MODS FOR COMPATIBILITY WITH DIGITAL VISUAL FORTRAN STEVE WEST 1998
C
C	UPGRADES TO MANY COMPONENTS BY STEVE WEST 1998-2003 
C                                                                       00000210
C     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00000220
C                                                                       00000210
C  the following section was from ftb1sq (SDA)                          00000210
C                                                                       00000210
      COMMON /AA/ IIIIIIII(600)
      COMMON /CC/ C(600)
      EQUIVALENCE (TYPE,C(1)), (IPB,C(2)), (ISB,C(3)), (IN,C(6))
     *, (OUT,C(7)), (CERR,C(16))
     *, (IPAU1,C(56)), (IPAU2,C(57)), (IPAU3,C(58))
     *, (ISAU1,C(60)), (ISAU2,C(61)), (ISAU3,C(62))
     *, (ISL,C(131))
      INTEGER OUT, TYPE, CERR
      DIMENSION IC(200)
      EQUIVALENCE (IC(1),C(1))
      DIMENSION CD(200)
      REAL*8  CD
      EQUIVALENCE (CD(1),C(201))
      EQUIVALENCE (PRFM,CD(1)), (SIZE,CD(2)), (ENDJ,CD(3)), (PCNG,CD(4))
     *, (SCNG,CD(5)), (PDUM,CD(7)), (COMMT,CD(21)), (V(1),CD(51))
     *, (GRPH,CD(24)), (ADATE,CD(83))
      DIMENSION V(11)
      CHARACTER*88 VSAD
C$                 CA(100)
      COMMON /CCA/ CA(150)
      DIMENSION ICA(2)
      EQUIVALENCE (ICA(1),CA(1))
      COMMON /CP/ PD(100)
      REAL*8 PD
      COMMON /CS/ SD(100)
      REAL*8 SD
      COMMON /DC/ DZ(2), D(128001)
C*    INTEGER TIMB
      REAL*8  PRFM,   SIZE,   ENDJ,   PCNG,   SCNG,   PDUM,   COMMT,
     1          V,      COM,    DMPN,   PTTLN,  GRPH,   ADATE,  CIN,
     2          CEN,    SDN,    CDN,    PDN,    CCN
C*   2          CI,     CEN,    SDN,    CDN,    PDN,    CCN
      REAL*8  V2
      DATA COM/6HCOMMON/, CIN/4H  CI/, CEN/4H  CE/, CDN/4H  CD/
     *, PDN/4H  PD/, SDN/4H  SD/
     *, DMPN/4HDUMP/, PTTLN/4HPTTL/
C
C*  OPEN FILES FOR PC USE USE TEMP NAMES IN THIS VERSION ... MORE
C*  FLEXIBILITY CAN BE ADDED LATER
       OPEN (UNIT=1,FILE='c:\data\AECSPROG\AECS01.DAT',
     1   FORM='UNFORMATTED', STATUS='OLD', RECL=27998)
C$ WAS4/98     1   FORM='UNFORMATTED', STATUS='UNKNOWN')
       OPEN (UNIT=5,FILE='c:\data\AECSMODEL\aaAecsIN.txt', 
     1     FORM='FORMATTED', STATUS='OLD')
       OPEN (UNIT=6,FILE='c:\data\AECSMODEL\AECSOUT.DOC',
     1     FORM='FORMATTED', STATUS='OLD')
       OPEN (UNIT=7,FILE='c:\data\AECSPROG\AECS07.DAT', 
     1     FORM='FORMATTED', STATUS='UNKNOWN')
       OPEN (UNIT=8,FILE='c:\data\AECSPROG\AECS08.DAT',
     1     FORM='FORMATTED', STATUS='UNKNOWN')
       OPEN (UNIT=9,FILE='c:\data\AECSPROG\AECS09.DAT',
     1     FORM='FORMATTED', STATUS='UNKNOWN')
       OPEN (UNIT=10,FILE='c:\data\AECSPROG\AECS10.DAT',
     1   FORM='FORMATTED',   STATUS='UNKNOWN')
       OPEN (UNIT=11,FILE='c:\data\AECSPROG\AECS11.DAT',
     1   FORM='FORMATTED',   STATUS='UNKNOWN')
       OPEN (UNIT=12,FILE='c:\data\AECSPROG\AECS12.DAT',
     1   FORM='UNFORMATTED', STATUS='UNKNOWN')
       OPEN (UNIT=13,FILE='c:\data\AECSPROG\AECS13.DAT',
     1   FORM='UNFORMATTED', STATUS='UNKNOWN')
       OPEN (UNIT=14,FILE='c:\data\AECSPROG\AECS14.DAT',
     1  FORM='FORMATTED',    STATUS='UNKNOWN')
       OPEN (UNIT=15,FILE='c:\data\AECSPROG\AECS15.DAT',
     1   FORM='FORMATTED',   STATUS='UNKNOWN')
       OPEN (UNIT=16,FILE='c:\data\AECSPROG\AECS16.DAT', 
     1  FORM='FORMATTED',    STATUS='UNKNOWN')
       OPEN (UNIT=17,FILE='c:\data\AECSPROG\AECS17.DAT',
     1  FORM='UNFORMATTED',  STATUS='UNKNOWN')
       OPEN (UNIT=18,FILE='c:\data\AECSPROG\AECS18.DAT',
     1  FORM='FORMATTED',    STATUS='UNKNOWN')
       OPEN (UNIT=19,FILE='c:\data\AECSPROG\AECS19.DAT',
     1  FORM='FORMATTED',    STATUS='UNKNOWN')
C
       WRITE (IPAU1, 1000) ENDJ
       REWIND IPAU1
       REWIND IPAU2
       WRITE (IPAU3, 1000) ENDJ
       REWIND IPAU3
       WRITE (ISAU1, 1000) ENDJ
       REWIND ISAU1
       REWIND ISAU2
       WRITE (ISAU3, 1000) ENDJ
       REWIND ISAU3
       CALL GDCU (100, 4, 0, D, ISL)
C
C*      WRITE (OUT,1200)
C* 1200 FORMAT(1H0)
c@      CALL DATE(ADATE)
      ISL = 1
    1 READ (IN,1000,END=10) V
      WRITE (VSAD,'(11A8)') V
 1000 FORMAT (A6,9A8,A2)
    2 CERR = 0
      IF (COMMT.EQ.V(1)) GO TO 1
      IF (PRFM.EQ.V(1)) GO TO 3
      IF (ENDJ.EQ.V(1)) GO TO 10
      IF (PCNG.EQ.V(1)) GO TO 5
      IF (SIZE.EQ.V(1)) GO TO 7
      IF (SCNG.EQ.V(1)) GO TO 8
      IF (PDUM.EQ.V(1)) GO TO 11
      IF (COM.EQ.V(1)) GO TO 999
      IF (DMPN.EQ.V(1)) GO TO 9999
      IF (PTTLN.EQ.V(1)) GO TO 9899
    6 CONTINUE
      WRITE (OUT,1001) V
 1001 FORMAT (2H *,A6,9A8,A2,1H*,5X,28HINVALID PROGRAM CONTROL CARD)
      GO TO 1
    3 IF (IPB.EQ.2) GO TO 6
      TYPE = 1
      IPB = 1
      GO TO 4
    5 TYPE = -1
      IF (IPB.GT.0) GO TO 4
      CERR = CERR+1
      WRITE (OUT,1002) CERR
 1002 FORMAT(6H0ERROR,I6,5X,36HPCHANGE NOT PRECEDED BY PERFORM CASE)
      TYPE = 0
C   4 CALL OVERLAY(4HAECS,1,0,0)
    4 CALL MPERF
      GO TO 2
    7 IF (ISB.EQ.2) GO TO 6
      TYPE = 2
      IF (IPB.NE.0) GO TO 12
      CERR = CERR+1
      WRITE (OUT,1003) CERR
 1003 FORMAT(6H0ERROR,I6,5X,33HSIZE NOT PRECEDED BY PERFORM CASE)
   12 ISB = 1
      GO TO 9
    8 TYPE = -2
      IF (ISB.GT.0) GO TO 9
      CERR = CERR+1
      WRITE (OUT,1004) CERR
 1004 FORMAT(6H0ERROR,I6,5X,33HSCHANGE NOT PRECEDED BY SIZE CASE)
      TYPE = 0
C   9 CALL OVERLAY(4HAECS,2,0,0)
    9 CALL MSIZE
      GO TO 2
   11 TYPE = 0
      IPB = -1
C     CALL OVERLAY(4HAECS,3,0,0)
      CALL MPDUM
      GO TO 2
   10 CONTINUE
      WRITE (OUT,1015)
 1015 FORMAT(1H0,20X,10HEND OF JOB)
      STOP 0
  999 CONTINUE
CWAS  CALL CORE (V, 16)
C IS
      READ(VSAD,1100)  CCN
C$    WAS
C$    READ (14, 1100)  CCN
C$     IS
C$     BACKSPACE IN
CWAS  READ (IN, 1100)  CCN
 1099 FORMAT(6X,A4)
C$
 1100 FORMAT(8X,A4)
      WRITE (6, 6666)  CCN,V
 6666 FORMAT (1H , 'CCN = ', A8, /, 1H , 'V = ', 11A8)
      IF (CCN.EQ.CIN) GO TO 801
      IF (CCN.EQ.CEN) GO TO 802
      IF (CCN.EQ.CDN) GO TO 803
      IF (CCN.EQ.PDN) GO TO 804
      IF (CCN.EQ.SDN) GO TO 805
      GO TO 6
CWAS
C 801 CALL CORE (V, 24)
CWAS
C     READ (IN,1101) IV1,IV2
CIS
  801 READ (VSAD,1101) IV1,IV2 ! common ci reads
 1101 FORMAT(12X,2I5)
      IF (IV1.LT.1 .OR. IV1.GT.200) GO TO 997
      IC(IV1) = IV2
      GO TO 998
CWAS
C 802 CALL CORE (V, 28)
CWAS
C     READ (IN,1102) IV1,V1
CIS
  802 READ (VSAD,1102) IV1,V1 ! common ce reads 
 1102 FORMAT(12X,I5,E10.0)
      IF (IV1.LT.1 .OR. IV1.GT.200) GO TO 997
      C(IV1) = V1
      GO TO 998
CWAS
C 803 CALL CORE (V, 28)
C     READ (IN,1103) IV1,V2
CIS
  803 READ (VSAD,1103) IV1,V2
 1103 FORMAT(12X,I5,A8)
      IF (IV1.LT.1 .OR. IV1.GT.100) GO TO 997
      CD(IV1) = V2
      GO TO 998
CWAS
C 804 CALL CORE (V, 28)
C     READ (IN,1103) IV1,V2
CIS
  804 READ (VSAD,1103) IV1,V2
      IF (IV1.LT.1 .OR. IV1.GT.100) GO TO 997
      PD(IV1) = V2
      GO TO 998
CWAS
C 805 CALL CORE (V, 28)
C     READ (IN,1103) IV1,V2
CIS
  805 READ (VSAD,1103) IV1,V2
      IF (IV1.LT.1 .OR. IV1.GT.100) GO TO 997
      SD(IV1) = V2
  998 CONTINUE
      WRITE (OUT,1005) V
 1005 FORMAT(2H *,A6,9A8,A2,1H*)
      GO TO 1
  997 CONTINUE
      GO TO 6
 9999 CONTINUE
      BACKSPACE IN
      READ (IN,1101) IV1
      IC(5) = IV1
      GO TO 998
 9899 CONTINUE
C     CALL OVERLAY(4HAECS,4,0,0)
C$99      CALL PTTL
c$*      CALL EXIT
      STOP
C     THIS CARD IS INCLUDED TO OVERRIDE A SYSTEM BUG
C     READ (1)
C     MECS
      END
C*   NORTHROP GRUMMAN PROPRIETARY