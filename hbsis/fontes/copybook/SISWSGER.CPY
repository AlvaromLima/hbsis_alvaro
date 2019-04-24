      **************************************
      * DEFINICAO DE VARIAVEIS UTILIZADAS  *
      **************************************
       77 WOPCAO               PIC X(05) VALUE SPACES.
       77 WFIM                 PIC X(02) VALUE SPACES.

       77 W-SEL                PIC 9(01) VALUE ZEROS.
       77 W-CONT               PIC 9(08) VALUE ZEROS.
       77 W-OPCAO              PIC X(01) VALUE SPACES.
	   77 W-ENTER              PIC X(01) VALUE SPACES.

       77 WS-RESULTADO-ACESSO  PIC X(02) VALUE "00".
	   77 WS-FS                PIC X(02) VALUE "00".
	   77 WS-FS-ERRO           PIC X(02) VALUE "00".
       77 W-SN                 PIC X(01) VALUE SPACES.
       77 W-ACT                PIC 9(02) VALUE ZEROS.
       77 LIMPA                PIC X(50) VALUE SPACES.
       77 DOPCAO               PIC X(10) VALUE SPACES.
	   77 TRACO                PIC X(80) VALUE SPACES.

       77 CONLIN               PIC 9(03) VALUE ZEROS.
       77 CONPAG               PIC 9(03) VALUE ZEROS.
	   77 LINPAG               PIC 9(03) VALUE 45.

	   77 WS-CONT              PIC 9(09) VALUE ZEROS.
       77 WS-GRAVACAO          PIC X(01) VALUE SPACES.
       77 WS-GRAVACAO-ERRO     PIC 9(01) VALUE ZEROS.

       77 WS-FLAG-CSV          PIC 9(01) VALUE ZEROS.
	   77 WS-FLAG              PIC 9(01) VALUE ZEROS.
	   
       01 MENS                 PIC X(50) VALUE SPACES.
       01 F REDEFINES MENS.
           03 ERRO             PIC X(002).
		   03 MENS-P           PIC X(048).

       01 WS-EOF-SW            PIC X(01) VALUE "N".
		   88 EOF-SW           VALUE "Y".
		   88 NOT-EOF-SW       VALUE "N".

	   01 PARAMETRO-CGCCPF.
           05 COMPRIMENTO            PIC S9(04) COMP.
           05 LKS-NUMERO-I           PIC 9(015).
           05 FILLER                 PIC X(001).
           05 LKS-NUMERO-F           PIC 9(015).
           05 FILLER                 PIC X(001).
           05 LKS-TIPO-CALCULO       PIC X(003).
           05 FILLER                 PIC X(001).
           05 LKS-ACAO               PIC X(001).
           05 LKS-RETORNO            PIC 9(001).

	   01 WTD-ENTRADA-ARQUIVO.
	       03 FILLER            PIC X(11) VALUE
	           "importacao\".
		   03 WTD-ENTRA-ARQUIVO PIC X(89) VALUE SPACES.

       01 WTD-ARQ-ERRO. 
	       03 FILLER            PIC X(11) VALUE
	           "importacao\".
           03 FILLER            PIC X(05) VALUE
               "ERRO_".		   
		   03 WTD-ARQERRO       PIC X(84) VALUE SPACES.
