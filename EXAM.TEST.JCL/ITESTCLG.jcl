//RTPOT49T JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//*
// SET COBPGM='TESTOUTP'
//**** Compile JCL ******
//STP0002 EXEC PROC=ELAXFCOC,
// CICS=,
// DB2=,
// COMP=
//COBOL.SYSPRINT DD SYSOUT=*
//SYSLIN DD DISP=SHR,
//        DSN=&SYSUID..COBOBJS.OBJ(&COBPGM.)
//COBOL.SYSLIB DD DISP=SHR,
//        DSN=RTPOT49.EXAM.DEV.COPYLIB
//COBOL.SYSXMLSD DD DUMMY
//COBOL.SYSIN DD DISP=SHR,
//        DSN=RTPOT49.EXAM.TEST.COBOL(&COBPGM.)
//****Link/Edit Step ******
//LKED EXEC PROC=ELAXFLNK
//LINK.SYSLIB DD DSN=CEE.SCEELKED,
//        DISP=SHR
//        DD DSN=&SYSUID..LEARN.LOAD,
//        DISP=SHR
//LINK.OBJ0000 DD DISP=SHR,
//        DSN=&SYSUID..COBOBJS.OBJ(&COBPGM.)
//LINK.SYSLIN DD *
     INCLUDE OBJ0000
/*
//LINK.SYSLMOD   DD  DISP=SHR,
//        DSN=&SYSUID..LEARN.LOAD(&COBPGM.)
//** Go (Run) Step. Add //DD cards when needed ******
//GO    EXEC   PROC=ELAXFGO,GO=&COBPGM.,
//        LOADDSN=&SYSUID..LEARN.LOAD
//******* ADDITIONAL RUNTIME JCL HERE ******
//RPTPRINT DD SYSOUT=*
//UTSTCASE DD DSN=RTPOT49.EXAM.UNITESTS.SORTED,DISP=SHR
//STATEZIP DD DSN=DDS0001.LEARN.STATE.ADDRESS.ZIP,DISP=SHR
//ITSTRPT DD SYSOUT=*
//RPTINPUT DD DSN=&SYSUID..REPORT.OUT,DISP=SHR
//DPARTSUP DD DSN=&SYSUID..EXAM.PARTSUPP.IN,DISP=SHR
//DOUTPUT  DD DSN=&SYSUID..PARTMAIN.GOOD.OUT,DISP=SHR
//DERROR   DD DSN=&SYSUID..PARTMAIN.ERROR.OUT,DISP=SHR
//DPARTS   DD DSN=&SYSUID..PARTMAIN.PARTS.OUT,DISP=SHR
//DSUPPS   DD DSN=&SYSUID..PARTMAIN.SUPLIERS.OUT,DISP=SHR
//DADDRS DD DSN=&SYSUID..PARTMAIN.ADRESSES.OUT,DISP=SHR
//DPO    DD DSN=&SYSUID..PARTMAIN.PURCHRDS.OUT,DISP=SHR
//PARTSUPP DD DSN=&SYSUID..EXAM.DEV.PARTSUPP,DISP=SHR
//RPTDEBUG DD SYSOUT=*
//RPTFILE  DD SYSOUT=*
//ERRFILE  DD SYSOUT=*
//PRTLINE  DD SYSOUT=*


