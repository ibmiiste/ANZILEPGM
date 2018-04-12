/* ======================================================== */
/* Command: ANZILEPGM - Analyze ILE programs                */
/* ======================================================== */
 ANZILEPGM:  CMD        PROMPT('Analyze ILE programs' )
             PARM       KWD(LIB ) TYPE(*NAME) LEN(10) SPCVAL((*ALLUSR) +
                          (*CURLIB) (*USRLIBL)) MIN(1) PROMPT('Library to +
                          analyze')
