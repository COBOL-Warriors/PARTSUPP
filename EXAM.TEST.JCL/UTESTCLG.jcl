//RTPOT49T JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//*
//SORTSTEP EXEC PGM=SORT
//SYSPRINT DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//SORTIN   DD  DSN=&SYSUID..EXAM.UNITESTS,DISP=SHR
//SORTOUT DD DSN=&SYSUID..EXAM.UNITESTS.SORTED,
//             DISP=(NEW,CATLG,DELETE),
//            STORCLAS=USRBASE,SPACE=(TRK,(5,5),RLSE),
//            DCB=(LRECL=1006,BLKSIZE=0,RECFM=FB,DSORG=PS)
//SYSIN DD *
  SORT FIELDS=(1,23,CH,A)
// SET COBPGM='PARTEDIT'
//**** Compile JCL ******
//STP0000 EXEC PROC=ELAXFCOC,
// CICS=,
// DB2=,
// COMP=
//COBOL.SYSPRINT DD SYSOUT=*
//SYSLIN DD DISP=SHR,
//        DSN=&SYSUID..COBOBJS.OBJ(&COBPGM.)
//COBOL.SYSLIB DD DISP=SHR,
//        DSN=&SYSUID..EXAM.DEV.COPYLIB
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
// SET COBPGM='SUPPEDIT'
//**** Compile JCL ******
//STP0001 EXEC PROC=ELAXFCOC,
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
// SET COBPGM='ADDREDIT'
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

// SET COBPGM='POEDIT'
//**** Compile JCL ******
//STP0004 EXEC PROC=ELAXFCOC,
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
//*
// SET COBPGM='UNITEST'
//**** Compile JCL ******
//STP0005 EXEC PROC=ELAXFCOC,
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
//UTSTCASE DD DSN=RTPOT49.EXAM.UNITESTS.SORTED,DISP=SHR
//UTSTRPT DD SYSOUT=*
//STATEZIP DD DSN=DDS0001.LEARN.STATE.ADDRESS.ZIP,DISP=SHR
//RPTDEBUG DD SYSOUT=*
//RPTFILE  DD SYSOUT=*
//ERRFILE  DD SYSOUT=*
//PRTLINE  DD SYSOUT=*


