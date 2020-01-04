Program Scanner;

USES Crt;

TYPE
    TokenType = ( tNone,        { tipe-tipe token }
                  tIdentifier,
                   tInteger,
                  tReal,
                  tCharConstant,
                  tString,
                  tPlus,
                  tMin,
                  tMult,
                  tDiv,
                  tAssigment,
                  tTitikDua,
                  tRange,
                  tKoma,
                  tTitik,
                  tTitikKoma,
                  tEqual,
                  tLess,
                  tLessEqu,
                  tGreater,
                  tGreaterEqu,
                  tKurungSikuBuka,
                  tKurungSikuTutup,
                  tKurungBuka,
                  tKurungTutup,
                  tKeyAND,              { Keyword }
                  tKeyARRAY,
                  tKeyBEGIN,
                  tKeyCASE,
                  tKeyCONST,
                  tKeyDIV,
                  tKeyDO,
                  tKeyDOWNTO,
                  tKeyELSE,
                  tKeyEND,
                  tKeyFOR,
                  tKeyFILE,
                  tKeyFUNCITION,
                  tKeyGOTO,
                  tKeyIF,
                  tKeyIN,
                  tKeyLABEL,
                  tKeyMOD,
                  tKeyNIL,
                  tKeyNOT,
                  tKeyOF,
                  tKeyOR,
                  tKeyPACKED,
                  tKeyPROCEDURE,
                  tKeyPROGRAM,
                  tKeyRECORD,
                  tKeySET,
                  tKeyTHEM,
                  tKeyTO,
                  tKeyTYPE,
                  tKeyUNTIL,
                  tKeyVAR,
                  tKeyWHILE,
                  tKeyWITH,
                );
    ScanErrorType = ( errNone,          { tipe-tipe error }
                      errScanUnexpChar,
                      errScanFloat,
                      errScanInt,
                      errScanApostExp,
                      errScanUnexpEOF
                    );
    CONST
        EndFile = #26
        JmlKeyWord = 40;
        Keyword : ARRAY [1..JmlKeyWord] OFSTRING[9]=(
                'AND',      'ARRAY',    'BEGIN',       'BOOLEAN',      'CASE',
                'CONST',    'CHAR',     'DIV',         'DO',           'DOWNTO',
                'ELSE',     'END',      'FILE',        'FOR',
                'FUNCITION',    'GOTO', 'IF',          'IN',
                'INTEGER',      'LABEL',        'MOD', 'NIL',           'NOT',
                'OF',   'OR',   'PACKED',       'PROCEDURE',
                'PROGRAM',      'REAL', 'RECORD',       'REPEAT',       'SET',
                'STRING',       'THEN', 'TO',   'TYPE', 'UNTIL',
                'VAR',  'WHILE',        'WITH',
                                );
    eMax       =    38;         { Eksponen Maksimal }
    eMin       =   -38;         { Eksponen Minimal }
    JmlSigDgt  =     5;         { Jumlah digit yang signifikan }
    ValMax     = 16383;         { 2**14 - 1 }
    MaxInfo    =   128;


VAR
        ScanStr   : STRING;
        Token     : TokenType;
        inum      : LONGINT;
        rnum      : REAL;
        CC        : CHAR;

        { Nama file program sumber, dan file keterangan hasil kompilasi }
        FileSource, FileRes : STRING;
        { File Program sumber, dan file keterangan hasil kompilasi }
        InFile, ResFile : TEXT;
        ErrCounter      : Integer;
        LineCounter     : Integer;      { Baris yang sedang diproses }

        lokasi : ARRAY {1..MaxInfo} of STRING;
        jinfo  : integer;

     { ****************** PROSEDUR - PROSEDUR PENDUKUNG ****************** }


     FUNCTION OpenFile:BOOLEAN;
     ( funggis operasi file )
     VAR i:INTEGER;
        Open1,Open2: BOOLEAN;
     BEGIN
        WRITELN; WRITE('File will be parend (.PAS) : ');
        READLN(FileSource);
        i:=POS('.',FileSource);
        IF i<>0 THEN FileSource:=COPY(FileSource,1,i-1);

        WRITE('Output file (.TXT): ');
        READLN(FileRes); WRTELN;
        i:POS('.',FilRes);
        IF i<>0 THEN FilRes:=COPY(FileRes,1,i-1);

        OpenFile:=TRUE;
        ($I-)
        ASSIGN(InFile,FileSource+'.PAS');
        RESET(InFile);
        ($I+)
        IF IOResult<>0 THEN
        BEGIN
                WRITELN('File ',FileSource,'.PAS doesn''t exist...');
                OpenFile:=FALSE;
        END
        ELSE
        BEGIN
                ($I-)
                ASSIGN(ResFile,FileRes+'.TXT');
                REWRITE(ResFile);
                ($I+)
                IF IOResult<>0 THEN
                BEGIN
                        WRUTWLN(FileRes,'.TXT csn''t be made...');
                        OpenFile:=FALSE;
                END
        END;
     END;

     FUNCTION UpperCase(Str:STRING):STRING;
     {  Fungsi untuk mengubah ke huruf kapital  }
     VAR
        i:INTEGER;
     BGEIN
        FOR i:=1 TU LENGTH(Str) DO
                Str[i]:=Upcase(Str[i]);
        UpperCase:=Str;
     END;

     PROCEDURE IncLine(var LineCounter : Integer);
     {  Menampilkan nomor baris yang sedang diproses  }
     BEGIN
        GOTOXY(1,WHEREY);
        LineCounter:=LineCounter+1;
        WRITE('Processing line : ',LineCounter);
     END;

     PROCEDURE ReadChar;
     {  Maju membaca satu karakter pada file source  }
     BEGIN
        READ(InFile,CC);
     END;

     PROCEDURE ProsErr(err:ScaErrorType);
     {  Prosedur penanganan kesalahan\error  }
     BEGIN
        GotoXY(25,WHEREY);
        WRITE('*Error : ');
        Case err OF
        errScanUnexpChar  : BEGIN WRITE('Unexpected character',CC);
                ReadChar;
        END;
        errScanApostEXP  :      WRITE(#39'  expected ');
        errScanUnexpEOF  :      BEGIN WRITE('Unexpected End of File   ');
                                        READLN; HALT;
                                END;
        errScanFloat     :      WRITE('Floating point format error')
        errScanInt       :      WRITW('Integer value error')
     END;
        READLN;
     END;

PROCEDURE WriteFile;
(  Penukisan hasil scanning ke file keterangan  )
var
        i       : integer;
        adainfo : boolean;
BEGIN
        WRITE   (ResFile,'      ');
        WRITE   (ResFile,Ord(Token):2);
        WRITE   (ResFile,'      ');
        CASE Token of
                tIdentifier :
                        begin
                                adainfo := false;
                                i       := 0;
                                repeat
                                        i:= i+1;
                                        if lokasi[i]=ScanStr then adainfo := true;
                                until (adainfo) or (i>jinfo);
                                if (not adainfo) and (jinfo<maxinfo) then
                                begin
                                        jinfo:=jinfo+1
                                        lokasi(jinfo):=ScanStr;
                                        WRITE(ResFile,jinfo:4);
                                end
                                else
                                        WRITE(ResFile,i:4);
                                WRITE(ResFile,'         '+ScanStr);
                        end;
                        Integer :
                                begin
                                        jinfo:=+1;
                                        lokasi[jinfo]:=ScanStr;
                                        WRITE(ResFile,jinfo:4);
                                end;
                                WRITE(ResFile',         ');
                                WRITE(ResFile.inum);
                        end;
                Real;
                        begin
                                if (jinfo<maxinfo) then
                                 begin
                                        jinfo:=jinfo+1;
                                        lokasi[jinfo]:=ScanStr;
                                        WRITE(ResFile,jinfo:4);
                                 end;
                                 WRITE(ResFile,'        ');
                                 WRITE(ResFile,rnum);
                        end;
                tChartConstan, tString ;
                        begin
                                if (jinfo<maxinfo) then
                                 begin
                                        jinfo:=jinfo+1;
                                        lokasi[jinfo]:=ScanStr;
                                        WRITE(ResFile,jinfo:4);
                                        WRITE(ResFile,'         ');
                                        WRITE(ResFile,ScanStr);
                                 end;
                                end;
                        else
                        begin
                                WRITE(ResFile,0:4);
                                WRITE(ResFile,'         ');
                                WRITE(ResFilr,ScanStr);
                        end;
                        end;
                        WRITELN(ResFile,'');
                END;

                (*********************** PROSEDUR SCAN ***************************

                PROCEDURE SCAN;
                CONST
                        SPACE = #32;
                        TAB     = #9;
                        CR      = #13;
                        LF      = #10;
                VAR Adatoken    : BOOLEAN;
                        i,j,k,e : INTEGER;
                        KeyToken        : TokenTypeABSOLUTE k;
                        TampStr         : STRING;
                        Error           : ScanErrorType;
                        Ex, Comment     : BOOLEAN;

                        PROCEDURE GetExp;
                        {  prosedur yang memberikan eksponen / bilangan di belakang 'e' | 'E'  }
                        VAR pangkat,sign: INTEGER;
                        BEGIN
                                sign:=1;  pangkat:=0;
                        ReadChar;  { Get char after E|e  }
                        IF CC IN ['+','-'] THEN
                        BEGIN
                                IF CC='-' THEN sign:=-1;
                                ReadChar;
                        END;
                        IF NOT (CC IN ['0'..'9']) THEN Error:=errScanFloat
                        ELSE  {  there is a numeric char after E|e  }
                        BEGIN
                                REPEAT
                                        pangkat:=10*pangkat+ORD(CCO-ORD('0');
                                        ReadChar;
                                UNTIL NOT (CC IN ['0'..'9']);
                                e:=e+pangkat*sign; { pangkat desimal total suatu bilangan real }
                                        END;
                                END;

                                PROCEDURE KonverToReal;
                                { Prosedur untuk memperoleh bil. real dalam format desimal berkoma }
                                VAR s:INTEGER; d,t:REAL;
                                BEGIN
                                        IF k+e>eMax THEN Eror:=errScanFloat
                                        ELSE
                                                IF k+e<eMin THEN rnum:=0 { bilangan terlalu kecil, jadikan 0 }
                                                ELSE
                                                BEGIN
                                                        s:=abs(e); t:=1.0; d:=10.0;
                                                        REPEAT
                                                                WHILE NOT ODD(s) DO
                                                                BEGIN s:=s DIV 2; d:=sqr(d); END;
                                                                s:=s-1; t:=d*t;
                                                        UNTIL s=0;
                                                        IF e>=0 THEN rnum:=rnum*t ELSE rnum:=rnum/t;
                                                END;
                                        END;

                                BEGIN { SCAN }
                                        AdaToken := FALSE;
                                        WHILE (NOT AdaToken) DO
                                        BEGIN
                                                Error:=errNone; Comment:=FALSE; Token:=tNone;
                                                WHILE CC IN [CR,LF,TAB,SPACE] DO
                                                BEGIN
                                                        IF CC=LF THEN IncLine(LineCounter);
                                                        ReadChar; { Skip WhiteSpace }
                                                END;
                                                { CC NOT IN WhiteSpace }
                                                IF CC=EndFile THEN EXIT; { Ketemu EOF }
                                                { CC bukan EOF }
                                                CASE CC OF
                                                        'a'..'z','A'..'Z': { Identifier }
                                        BEGIN
                                                ScanStr:='';
                                                REPEAT
                                                        ScanStr:=ScanStr+CC
                                                        ReadChar;
                                                UNTIL NOT (CC IN ['a'..'z'..'A'..'Z','0'..'9','_']);
                                                { Test keyword }
                                                TampStr:=UpperCase(ScanStr);
                                                i:=1; j:=JmlKeyword; { index untuk pencarian keyword dalam tabel }
                                                        REPEAT
                                                                k:=(i+j) DIV 2;
                                                                IF TampStr<=KeyWord[k] THEN j:k-1;
                                                                IF TampStr>=KeyWord[k] THEN i:=k+1;
                                                        UNTIL i>j;

                                                        IF i-j>;
                                                        BEGIN k:=k+ORD(tKurungTutup); Token := KeyToken; END
                                                        ELSE
                                                        BEGIN Token := tIdentifier;
                                                ScanStr:=Copy(ScanStr,1,10); END;
                                                        END; { end identifier }

                                                        '0'..'9': konstanta numerik }
                                                        BEGIN
                                                                k:=0: rnum:=0; Token:=tInteger;
                                                                REPEAT
                                                                        inum:=inum*10 + ORD(CC) - ORD('0');
                                                                        k:=k+1;
                                                                        ReadChar;
                                                                UNTIL NOT (CC IN ['0'..'9']);
                                                                { Uji error numerik }
                                                                IF (k>JmlSigDgt) OR (inum>ValMax) THEN { bilangan melebihi batas }
                                                                IF CC= ',' THEN { real atau range }
                                                                BEGIN
                                                                        ReadChar;
                                                                        IF CC=',' THEN { range }
                                                                        BEGIN ScanStr:='..'; { CC:=':'; } END
                                                                        ELSE { Real }
                                                                        BEGIN
                                                                                IF NOT (CC IN ['0'..'9']) THEN { setelah ',' bukan angka }
                                                                                BEGIN
                                                                                        Error:=errScanFloat;
                                                                                        IF CC IN ['e','E'] THEN GetExp;
                                                                                END
                                                                                ELSE
                                                                                BEGIN
                                                                                        Token:=tReal; rnum:=inum; e:=0;
                                                                                        REPEAT
                                                                                        e:=e-1; ( untuk menentukan posisi titik bilangan real )

                                                                                        rnum:=rnum*10 + ORD(CC) - ORD('0');
                                                                                        ReadChar;
                                                                                UNTIL NOT (CC IN ['0'..'9']);
                                                                                IF CC IN ['e','E'] THEN GetExp;
                                                                                IF (Error<>errScanFloat) AND (e<>0) THEN
                                                                                        KonverToReal;
                                                                                END;
                                                                        END; { Real }
                                                                END
                                                                ELSE ( CC<>',')
                                                                        IF CC IN ['e','E'] THEN {untuk bentuk spt 1E+2}
                                                                        BEGIN
                                                                                Token:=tReal; rnum:=inum; e:=0; GetExp;
                                                                                IF ((Error<>errScanFloat) AND (e<>0)) THEN
                                                                                KonverToReal;
                                                                        END;
                                                                END; { end konstanta numerik }

                                                        '(: {komentar1 atau kurung buka }
                                                        BEGIN
                                                                ScanStr:=CC;
                                                                ReadChar;
                                                                IF CC<>'*' THEN Token:=tKurungBukan
                                                                ELSE { '(*', komentar, harus dibuang }
                                                                BEGIN
                                                                        Comment:=TRUE;
                                                                        ReadChar;
                                                                        IF CC<>EndFile THEN
                                                                        BEGIN
                                                                                REPEAT
                                                                                        WHILE NOT (CC IN ['*',ENDFile]) DO
                                                                                        BEGIN
                                                                                                IF CC=LF THEN IncLine(LineCounter);
                                                                                                ReadChar; ( skip )
                                                                                        END;
                                                                                        { CC='*' atau CC=EOF }
                                                                                        IF CC='*' THEN IncLine(LineCounter)
                                                                                UNTIL CC IN [')',EndFile];

                                                                                IF CC=')' THEN ReadChar ELSE
                                                Error:=errScanUnexEOF;
                                                        END
                                                        ELSE Error:=errScanUnexpEOF;
                                                END;
                                                END; { end komentar 1 }

                                                '('; { komentar 2 }
                                                        BEGIN
                                                                Comment:=TRUE;
                                                                REPEAT
                                                                        ReadChar;
                                                                IF CC=LF THEN IncLine(LineCounter);
                                                        UNTIL CC IN ['}', EndFile];
                                                        IF CC='}' THEN ReadChar ELSE Error:=errScanUnexEOF;
                                                END; { komentar 2 }

                                        ''''; { string literal atau konstanta karakter }
                                        BEGIN
                                                ScanStr:=''; Ex:=FALSE;
                                                WHILE NOT Ex DO
                                                BEGIN
                                                        REPEAT
                                                                ScanStr:=ScanStr+CC;
                                                                ReadChar;
                                                        UNTIL (CC IN ["",LF,EndFile]);
                                                        IF CC IN [LF,EndFile] THEN
                                                        BEGIN
                                                                Ex:=TRUE;
                                                                IF CC=LF THEN
                                                                BEGIN
                                                                        IncLine(LineCounter);
                                        Error:=errScanApostExp;
                                                        END
                                                        ELSE Error:=errScanUnexpEOF;
                                        END
                                        ELSE {'}
                                        BEGIN
                                                ScanStr:=ScanStr+CC;
                                                ReadChar;
                                                IF CC<>'''' THEN Ex:=TRUE:
                                        END;
                                END;
                                IF NOT (Error IN [errScanApostExp,errScanUnexpEOF])
                        THEN
                                BEGIN { konstanta string atau karakter }
                                        DELETE(ScanStr,LENGTH(ScanStr),1);
                                        DELETE(ScanStr,1,1);
                                        IF LENGTH(ScanStr)>1 THEN Token:=tString
                                        ELSE
                                        BEGIN inum:=ORD(ScanStr[1]);
                        Token:=tCharConstant; END;
                                END;
                        END; { string literal atau konstanta karakter }

                        ':' : { titik dua
                                BEGIN
                                        ScanStr:=CC;
                                        ReadChar;
                                        IF CC='=' THEN { assignment }
                                        BEGIN
                                                Token:=tAssignment; ScanStr:=ScanStr+CC;
                        ReadChar;
                                        END
                                        ELSE
                                                IF ScanStr='..' THEN Token:=tRange
                                                ELSE Token;=tTitikDua
                                        END;    { titik dua }
                        '<' : { lebih kecil, kecil sama, tidak sama }
                                BEGIN
                                        ScanStr:=CC;
                                        ReadChar;
                                        IF CC='=' THEN
                                        BEGIN Token:=tLessEqu; ScanStr:=ScanStr+CC;
                        ReadChar; END
                                        ELSE
                                                IF CC='>' THEN
                                                BEGIN
                                                        Token:=tInequal; ScanStr:=ScanStr+CC;
                        ReadChar;
                                END
                                ELSE Token:=tLess;
                        END; { lebih kecil, kecil sama, tidak sama }

                                '>' : { lebih besar, besar sama }
                                BEGIN
                                        ScanStr:=CC;
                                        ReadChar;
                                        IF CC='=' THEN
                                        BEGIN
                                                Token:=tGreater; ScanStr:=ScanStr+CC;
                                ReadChar;
                                        END
                                        ELSE Token:=tGreater;
                                END; { lebih besar, besar sama }

                                '.' : { titik atau range }
                                        BEGIN
                                                ReadChar;
                                                IF ScanStr='..' THEN
                                                        Token := tRange
                                                ELSE
                                                BEGIN
                                                        Token := tTitik;
                                                        ScanStr:='.';
                                                END;
                                        END; {titik atau range}
                                        '+', '-', '*', '/', ',', ';', '=', ')', '[', ']';
                                        BEGIN
                                                ScanStr:=CC;
                                                CASE CC OF
                                                '+': Token:=tPlus;
                                                '-': Token:=tMin;
                                                '*': Token:=tMult;
                                                '/': Token:=tDIV;
                                                ',': Token:=tTitikKoma;
                                                '=': Token:=tEqual;
                                                ')': Token:=tKurungTutup;
                                                '[': Toekn:=tKurungSikuBuka;
                                                ']': Token:=tKurungSikuTutup;
                                        END;
                                        ReadChar;
                                END;

                                ELSE    Error:=errScanUnexpChar;
                                END; { EndCase }
                                IF (((NOT Comment) AND (Error<>errScanUnexpChar))
                                        OR (CC=EndFile)) THEN
                                AdaToken:=TRUE;
                                IF Errir<>errNone THEN ProsERR(Error);
                        END; {ENDWhilw}
                END; { SCAN }
                { *********************** PROGRAM UTAMA ***************** }
                BEGIN
                        if OpenFile then
                        begin
                                readchar;
                                jinfo := 0;
                                WRITELN(ResFilr,'Iternal number Lokasi Token');
                                WTITELN(ResFile,'--------------------');
                                while (CC<>EndFile) do
                                begin
                                        SCAN;
                                        WriteFile;
                                end;
                        end;

                        Close(ResFile);
                        END.