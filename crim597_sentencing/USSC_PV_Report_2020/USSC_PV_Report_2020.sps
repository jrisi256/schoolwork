* Encoding: UTF-8.
/*********************************************************************************************************/
/*                                                                                                                                 */
/* This SPSS program will extract the United States Sentencing Commission's 2020          */
/* Federal Probation and Supervised Release Violations  report file and create a SAS          */
/* dataset.                                                                                                                    */
/*                                                                                                                                 */
/* Prior to running the program you must update the DAT file location in the                       */
/* FILE HANDLE DATA / NAME line to correspond to the location of the file on your           */
/* computer.                                                                                                                 */
/*                                                                                                                                 */
/* You must also update the file output location in the SAVE OUTFILE statement at the     */
/* end of the program to correspond to the folder you want the output file to be located.       */
/*                                                                                                                                 */
/*********************************************************************************************************/

/* The following line should contain the complete path and name of your raw data file */
/* The last line of this file contains the path to your output '.sav' file */

FILE HANDLE DATA / NAME="./ussc_pv_report_2020.dat" LRECL=135 .

DATA LIST FILE=DATA/
   USSCIDN 1-7              ACCAP 8                  AGE 9-10              
   AGGROLHI 11              CAROFFAP 12              CIRCDIST 13-14        
   COMBDRG2 15-16           FISCALYR 17-20           MITROLHI 21-22        
   MONSEX 23                NEWCIT 24                NEWRACE 25            
   OFFTYPE2 26-27           PROBATN 28-30            PV_AGE 31-32          
   PV_CIRCDIST 33-34        PV_DISP 35               PV_DSHEAR 36          
   PV_DSJANDC 37            PV_DSSUMMON 38           PV_DSVIOL 39          
   PV_DSWAIVER 40           PV_DSWORK 41             PV_FY 42-45           
   PV_GRADVIOL 46           PV_GRADVIOL_TABLE 47-48   PV_REVTABLE 49        
   PV_REVTABLE_CELL 50-59 (A)   PV_SENSPLT0 60-65        PV_SENTIMP 66         
   PV_SENTTOT 67-72         PV_VIOLTYP 73            SAFETY 74             
   SENSPCAP 75-92           SENSPLT0 93-110          SENTIMP 111           
   SOURCES 112              STATMAX 113-117          STATMIN 118-121       
   SUPREL 122-124           SUPRVISN 125-127         SUPSERVED 128-133     
   WEAPON 134               XCRHISSR 135 .        


FORMATS
  PV_SENSPLT0 (F7.2) /     PV_SENTTOT (F7.2) /      SENSPCAP (F19.16) /    
  SENSPLT0 (F19.16) /      SUPSERVED (F7.2) / .
  

SAVE OUTFILE='./ussc_pv_report_2020.sav'.
