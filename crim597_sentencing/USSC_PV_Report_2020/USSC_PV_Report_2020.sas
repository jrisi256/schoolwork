/*****************************************************************************************
*                                                                                        *
* This SAS program will extract the United States Sentencing Commission's 2020 Federal   *
* Probation and Supervised Release Violations report file and create a SAS dataset.      *
*                                                                                        *
* Prior to running the program you must update the DAT file location in the FILNAME line *
* to correspond to the location of the file on your computer.                            *
*                                                                                        *
* You must also update the library location in the LIBNAME statement to correspond to    *
* the folder you want the output file to be located.                                     *
*                                                                                        *
*****************************************************************************************/

/* The following line should contain the complete path and name of your raw data file */
FILENAME datafile './ussc_pv_report_2020.dat' ;

/* The following line should contain the directory in which you wish your SAS file to be stored */
libname library './' ;

/* The following line contains the name (without the extension) for your SAS dataset */
%LET dataset = USSC_PV_Report_2020 ;

DATA library.&dataset ;
INFILE datafile LRECL=135;
INPUT
   USSCIDN  1-7             ACCAP  8                 AGE  9-10             
   AGGROLHI  11             CAROFFAP  12             CIRCDIST  13-14       
   COMBDRG2  15-16          FISCALYR  17-20          MITROLHI  21-22       
   MONSEX  23               NEWCIT  24               NEWRACE  25           
   OFFTYPE2  26-27          PROBATN  28-30           PV_AGE  31-32         
   PV_CIRCDIST  33-34       PV_DISP  35              PV_DSHEAR  36         
   PV_DSJANDC  37           PV_DSSUMMON  38          PV_DSVIOL  39         
   PV_DSWAIVER  40          PV_DSWORK  41            PV_FY  42-45          
   PV_GRADVIOL  46          PV_GRADVIOL_TABLE  47-48   PV_REVTABLE  49       
   PV_REVTABLE_CELL $ 50-59   PV_SENSPLT0  60-65       PV_SENTIMP  66        
   PV_SENTTOT  67-72        PV_VIOLTYP  73           SAFETY  74            
   SENSPCAP  75-92          SENSPLT0  93-110         SENTIMP  111          
   SOURCES  112             STATMAX  113-117         STATMIN  118-121      
   SUPREL  122-124          SUPRVISN  125-127        SUPSERVED  128-133    
   WEAPON  134              XCRHISSR  135          ;                       

LENGTH
   USSCIDN 6                ACCAP 3                  AGE 3                 
   AGGROLHI 3               CAROFFAP 3               CIRCDIST 3            
   COMBDRG2 3               FISCALYR 4               MITROLHI 3            
   MONSEX 3                 NEWCIT 3                 NEWRACE 3             
   OFFTYPE2 3               PROBATN 4                PV_AGE 3              
   PV_CIRCDIST 3            PV_DISP 3                PV_DSHEAR 3           
   PV_DSJANDC 3             PV_DSSUMMON 3            PV_DSVIOL 3           
   PV_DSWAIVER 3            PV_DSWORK 3              PV_FY 4               
   PV_GRADVIOL 3            PV_GRADVIOL_TABLE 3      PV_REVTABLE 3         
   PV_SENTIMP 3             PV_VIOLTYP 3             SAFETY 3              
   SENTIMP 3                SOURCES 3                STATMAX 4             
   STATMIN 4                SUPREL 4                 SUPRVISN 4            
   WEAPON 3                 XCRHISSR 3 ;

        

RUN ;
