       //  ===============================================================
       //  Module: ANZILEPGM2 - Analyze ILE programs - Build database
       //  ===============================================================

     FQadspobj  IPE  E             Disk
     FAnzilemod O    E             Disk    Prefix(Mod_)
     F                                     Rename(Anzilemod:Modrec)
     FAnzilesvc O    E             Disk    Prefix(Svc_)
     F                                     Rename(Anzilesvc:Svcrec)

       // ------------------------------------------- Procedure prototypes
     D Bldmodlst       PR
     D                               20    Value
     D                                8    Value

     D Bldsvclst       PR
     D                               20    Value
     D                                8    Value

     D Crtusrspc       PR
     D                               20    Value

     D Getlstinf       PR
     D                               20    Value
     D                               10U 0
     D                               10U 0
     D                               10U 0

     D Pgmisile        PR              N
     D                               20    Value

       // ------------------------------------------------- API prototypes
     D Qbnlpgmi        PR                  Extpgm('QBNLPGMI')
     D   Apiusrspc                   20    Const
     D   Apilstfmt                    8    Const
     D   Qpgmname                    20    Const
     D   Apierr                      16

     D Qbnlspgm        PR                  Extpgm('QBNLSPGM')
     D   Apiusrspc                   20    Const
     D   Apilstfmt                    8    Const
     D   Qpgmname                    20    Const
     D   Apierr                      16

     D Qclrpgmi        PR                  Extpgm('QCLRPGMI')
     D  Pgmi0100                    161
     D  Pgmilen                      10U 0 Const
     D  Pgmifmt                       8    Const
     D  Qpgmname                     20    Const
     D  Apierr                       16

     D Quscrtus        PR                  Extpgm('QUSCRTUS')
     D  Uspcname                     20    Const
     D  Uspcextatr                   10    Const
     D  Uspcsiz                      10U 0 Const
     D  Uspcinzval                    1    Const
     D  Uspcpubaut                   10    Const
     D  Uspctext                     50    Const
     D  Uspcrpl                      10    Const
     D  Apierr                       16

     D Qgetlstmod      PR                  Extpgm('QUSRTVUS')
     D  Apiusrspc                    20    Const
     D  Apilstpos                    10U 0 Const
     D  Apisizent                    10U 0 Const
     D  Apirtnvar                   508

     D Qgetlstspg      PR                  Extpgm('QUSRTVUS')
     D  Apiusrspc                    20    Const
     D  Apilstpos                    10U 0 Const
     D  Apisizent                    10U 0 Const
     D  Apirtnvar                    56

     D Qgetlstinf      PR                  Extpgm('QUSRTVUS')
     D  Apiusrspc                    20    Const
     D  Apilstpos                    10U 0 Const
     D  Apisizent                    10U 0 Const
     D  Apirtnvar                    16

       // ----------------------------------------------- Global variables
     D Modfmt        E DS                  Extname('ANZILEMOD')
     D                                     Based(Modfmtptr)
     D                                     Prefix(Mod_)

     D Svcfmt        E DS                  Extname('ANZILESVC')
     D                                     Based(Svcfmtptr)
     D                                     Prefix(Svc_)

     D Modfmtptr       S               *   Inz(*NULL)
     D Svcfmtptr       S               *   Inz(*NULL)

       //  ---------------------------------------------------------------
       //  - Main procedure
       //  ---------------------------------------------------------------
      /free
       If Odobtp = '*SRVPGM' Or Pgmisile(Odobnm + Odlbnm);
         Bldmodlst(Odobnm + Odlbnm:Odobtp);
         Bldsvclst(Odobnm + Odlbnm:Odobtp);
       Endif;
      /end-free

       //  ---------------------------------------------------------------
       //  Procedure: BldModLst - Populate database with modules used
       //  ---------------------------------------------------------------
     P Bldmodlst       B
     D                 PI
     D Qpgmname                      20    Value
     D Objtype                        8    Value

     D Apiusrspc       C                   'PGML0100  QTEMP     '

     D Apierr          S             16
     D Apilstpos       S             10U 0
     D Apinbrent       S             10U 0
     D Apirtnvar       S            508
     D Apisizent       S             10U 0
     D X               S             10U 0

      /free
       Crtusrspc(Apiusrspc);      // Create User Space To Hold Module List

       If Objtype = '*SRVPGM';                        // Build Module List
         Qbnlspgm(Apiusrspc:'SPGL0100':Qpgmname:Apierr);
       Else;
         Qbnlpgmi(Apiusrspc:'PGML0100':Qpgmname:Apierr);
       Endif;

       Getlstinf(Apiusrspc:Apilstpos:Apinbrent:Apisizent); // Get List Hdr

       If Apinbrent < 1;                                   // Process List
         Return;
       Endif;

       Modfmtptr = %ADDR(Apirtnvar);

       For X = 1 To Apinbrent;
         Qgetlstmod(Apiusrspc:Apilstpos:Apisizent:Apirtnvar);
         Write Modrec;
         Apilstpos = Apilstpos + Apisizent;
       Endfor;

       Return;
      /end-free
     P Bldmodlst       E

       //  ---------------------------------------------------------------
       //  Procedure: BldSvcLst - Populate database with service
       //  ---------------------------------------------------------------
     P Bldsvclst       B
     D                 PI
     D Qpgmname                      20    Value
     D Objtype                        8    Value

     D Apiusrspc       C                   'PGML0200  QTEMP     '

     D Apierr          S             16
     D Apilstfmt       S              8
     D Apilstpos       S             10U 0
     D Apinbrent       S             10U 0
     D Apirtnvar       S             56
     D Apisizent       S             10U 0
     D X               S             10U 0

      /free
       Crtusrspc(Apiusrspc); // Create User Space For Service Program List

       If Objtype   = '*SRVPGM';             // Build Service Program List
         Qbnlspgm(Apiusrspc:'SPGL0200':Qpgmname:Apierr);
       Else;
         Qbnlpgmi(Apiusrspc:'PGML0200':Qpgmname:Apierr);
       Endif;

       Getlstinf(Apiusrspc:Apilstpos:Apinbrent:Apisizent); // Get List Hdr

       If Apinbrent < 1;                                   // Process List
         Return;
       Endif;

       Svcfmtptr = %ADDR(Apirtnvar);

       For X = 1 To Apinbrent;
         Qgetlstspg(Apiusrspc:Apilstpos:Apisizent:Apirtnvar);
         If %SUBST(Apirtnvar:31:10) <> 'QSYS      ';
           Write Svcrec;
         Endif;
         Apilstpos = Apilstpos + Apisizent;
       Endfor;

       Return;
      /end-free
     P Bldsvclst       E

       //  ---------------------------------------------------------------
       //  Procedure: CrtUsrSpc - Creates a user space
       //  ---------------------------------------------------------------
     P Crtusrspc       B
     D                 PI
     D Uspcname                      20    Value

     D Apierr          S             16

      /free
       Quscrtus(Uspcname:
                'ANZILEPGM':4096:X'00':'*ALL':*BLANKS:'*YES':Apierr);
       Return;
      /end-free
     P Crtusrspc       E

       //  ---------------------------------------------------------------
       //  Procedure: GetLstInf - Retrieves generic header list format
       //  ---------------------------------------------------------------
     P Getlstinf       B
     D                 PI
     D Uspcname                      20    Value
     D Lstpos                        10U 0
     D Lstnbrent                     10U 0
     D Lstsizent                     10U 0

     D Uspcrtnvar      DS
     D Uspclstpos                    10U 0
     D                               10U 0
     D Uspcnbrent                    10U 0
     D Uspcsizent                    10U 0

      /free
       Qgetlstinf(Uspcname:125:16:Uspcrtnvar);        // Get list position
       Lstpos    = Uspclstpos + 1;
       Lstnbrent = Uspcnbrent;
       Lstsizent = Uspcsizent;
       Return;
      /end-free
     P Getlstinf       E

       //  ---------------------------------------------------------------
       //  Procedure: PgmIsILE - Returns indicator for program type
       //                       *ON = ILE program
       //                       *OFF = Not ILE program
       //  ---------------------------------------------------------------
     P Pgmisile        B
     D                 PI              N
     D Qpgmname                      20    Value

     D Apierr          S             16

     D Pgmi0100        DS
     D Pgmipgmtyp            161    161

      /free
       Qclrpgmi(Pgmi0100:%SIZE(Pgmi0100):'PGMI0100':Qpgmname:Apierr);
       Return (Pgmipgmtyp = 'B');        // *ON=Ile Pgm, *OFF=Not Ile Pgm
      /end-free
     P Pgmisile        E
