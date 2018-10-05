
Advanced Environmental Control System (AECS) Simulation Program (Windows Port).

The Advanced Environmental Control System (AECS) Simulation Program, is a WINDOWS PORT of the Aircraft ECS design and analysis simulation program, covering each of the following:

1 Aircraft Equipment,
2 Air Conditioning Equipment,
3 Refrigeration,
4 Refrigerant Compressors,
5 Cooling Systems,
6 Heat Exchangers,
7 Turbomachinery,
8 Expanders,
9 Intercoolers,
10. Roots Compressors,
11. Refregerant Boilers,
12. Refrigerant Evaporators,
13. Refrigerant Control Valves,
14. Coefficient of Performance,
15. Refrigerant Condensers,
16. Solution Procedures,
17. Refrigerant Pumps,
18. Sensors,
19. Refrigerant Separators,
20. Refrigerant Lines,
21. Maximum Rate of Descent.


User Manuals are included in (7) PDF scanned files located in: "./AECS_Manuals".


Operation: "./aecs_orig.exe"


*****************************************************
START: Example of "aaAecsIN.txt" INPUT:
*****************************************************
PERFORM                                                                         
TITLE EXAMPLE CASE FOR COMPRESSOR WITH ROOTS POCKET OPTION, 01/04/05            
TITEND                                                                          
CASEA   VCOMP   0000   3   4   0                                                
CASEB      0   0   0   0   0                                                    
CASEC      0.0          0.0        0.0                                          
CASED      0.0                                                                  
CASEND                                                                          
PARAM   15                                                                      
VALUES   1  1.0       158.5674    595.45   122.026    1140.                     
VALUES   6  1.74         .85     .000369      .9                                
VALUES  11  0.04      200.0000    600.00     121.100                            
PAREND                                                                          
RINLET    20   1   30100   1   2   3   4   3 -22                                
RINLET    22   3   50000  11  12  13  14   3 -22                                
SHAFT     30   1   0   5                                                        
VCOMP     40   1   3   9   1   0   6  71  72   7   2   8   5   3   2   9        
ROUTLT    70   2   90000   1   2   3   4                                        
COMEND                                                                          
TABID     71   4  33   0   0  280101   2   0 101 101   2                        
TABT      VCOMP: PRESSURE RATIO = F(FLOW RATE,SHAFT SPEED)                      
TABV      0.0       1.0                                                         
TABV      0.0        11.0                                                       
TABV      1000.     11000.                                                      
TABV      2.0       1.0       20.0       1.0                                    
TABEND                                                                          
TABID     72   6  33   0   0  280101   2   0 101 101   2                        
TABT      VCOMP: EFFICIENCY = F(FLOW RATE,SHAFT SPEED)                          
TABV      0.0       1.0                                                         
TABV      0.0        11.0                                                       
TABV      1000.      11000.                                                     
TABV       0.85      0.85       0.85       0.85                                 
TABEND                                                                          
PTAB       1   0   1   0                                                        
ENDCASE                                                                         
*****************************************************
START: Example of "aaAecsIN.txt" INPUT:
*****************************************************



*****************************************************
START: Example of "AECSOUT.DOC" OUTPUT:
*****************************************************
1*AECS* GENERAL ECS PROGRAM                                                                                 PAGE        1
     1  CASE PERFOR                                                                          DATE 10/ 5/2018  12: 4:10.61
 TIME M       0.00 SEC.
1*AECS* EXAMPLE CASE FOR COMPRESSOR WITH ROOTS POCKET OPTION, 01/04/05                                      PAGE        2
     1  CASE PERFOR                                                                          DATE 10/ 5/2018  12: 4:10.61
 TIME Z       0.00
0     EXAMPLE CASE FOR COMPRESSOR WITH ROOTS POCKET OPTION, 01/04/05            
... (content removed) ...
0SOLUTION CONVERGED IN      6 TRY(S)
0     0 ERROR(S) DETECTED
0CASE END
1
0                    END OF JOB
*****************************************************
START: Example of "AECSOUT.DOC" OUTPUT:
*****************************************************

