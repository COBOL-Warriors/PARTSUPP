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
//SORTSTEP EXEC PGM=SORT
//SYSPRINT DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//SORTIN   DD  DSN=&SYSUID..EXAM.UNITESTS.SORTED,DISP=SHR
//SORTOUT DD DSN=&SYSUID..EXAM.PARTSUPP.IN,
//             DISP=(NEW,CATLG,DELETE),
//            STORCLAS=USRBASE,SPACE=(TRK,(5,5),RLSE),
//            DCB=(LRECL=473,BLKSIZE=0,RECFM=FB,DSORG=PS)
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
//*
// SET COBPGM='RPTPRINT'
//**** Compile JCL ******
//STP0006 EXEC PROC=ELAXFCOC,
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
// SET COBPGM='PARTMAIN'
//**** Compile JCL ******
//STP0006 EXEC PROC=ELAXFCOC,
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
//DPARTSUP DD DSN=RTPOT49.EXAM.PARTSUPP.IN,DISP=SHR
//DOUTPUT  DD DSN=&SYSUID..PARTMAIN.GOOD.OUT,DISP=(NEW,CATLG,KEEP),
//            STORCLAS=USRBASE,SPACE=(TRK,(1,1),RLSE),
//            DCB=(LRECL=473,BLKSIZE=0,RECFM=FB,DSORG=PS)
//DERROR   DD DSN=&SYSUID..PARTMAIN.ERROR.OUT,DISP=(NEW,CATLG,DELETE),
//            STORCLAS=USRBASE,SPACE=(TRK,(1,1),RLSE),
//            DCB=(LRECL=563,BLKSIZE=0,RECFM=FB,DSORG=PS)
//DPARTS   DD DSN=&SYSUID..PARTMAIN.PARTS.OUT,DISP=(NEW,CATLG,DELETE),
//            STORCLAS=USRBASE,SPACE=(TRK,(1,1),RLSE),
//            DCB=(LRECL=91,BLKSIZE=0,RECFM=FB,DSORG=PS)
//DSUPPS   DD DSN=&SYSUID..PARTMAIN.SUPLIERS.OUT,
//            DISP=(NEW,CATLG,DELETE),
//            STORCLAS=USRBASE,SPACE=(TRK,(1,1),RLSE),
//            DCB=(LRECL=38,BLKSIZE=0,RECFM=FB,DSORG=PS)
//DADDRS DD DSN=&SYSUID..PARTMAIN.ADRESSES.OUT,DISP=(NEW,CATLG,DELETE),
//            STORCLAS=USRBASE,SPACE=(TRK,(1,1),RLSE),
//            DCB=(LRECL=219,BLKSIZE=0,RECFM=FB,DSORG=PS)
//DPO    DD DSN=&SYSUID..PARTMAIN.PURCHRDS.OUT,DISP=(NEW,CATLG,DELETE),
//            STORCLAS=USRBASE,SPACE=(TRK,(1,1),RLSE),
//            DCB=(LRECL=123,BLKSIZE=0,RECFM=FB,DSORG=PS)
//PARTSUPP DD DSN=&SYSUID..EXAM.DEV.PARTSUPP,DISP=SHR
//STATEZIP DD DSN=DDS0001.LEARN.STATE.ADDRESS.ZIP,DISP=SHR
//RPTDEBUG DD SYSOUT=*
//RPTPRINT DD SYSOUT=*
//RPTFILE  DD SYSOUT=*
//ERRFILE  DD SYSOUT=*
//PRTLINE  DD SYSOUT=*
//*
// SET COBPGM='RPTPRINT'
//**** Compile JCL ******
//STP0006 EXEC PROC=ELAXFCOC,
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
//RPTINPUT DD DSN=&SYSUID..PARTMAIN.GOOD.OUT,DISP=SHR
//RPTDEBUG DD SYSOUT=*
//RPTPRINT DD DSN=&SYSUID..REPORT.OUT,DISP=(NEW,CATLG,DELETE),
//            STORCLAS=USRBASE,SPACE=(TRK,(12,5),RLSE),
//            DCB=(LRECL=132,BLKSIZE=0,RECFM=FB,DSORG=PS)
//RPTFILE  DD SYSOUT=*
//ERRFILE  DD SYSOUT=*
//PRTLINE  DD SYSOUT=*

//******* ADDITIONAL RUNTIME JCL HERE ******



