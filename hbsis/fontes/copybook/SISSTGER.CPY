      **************************************
      * DEFINICAO DAS TELAS UTILIZADAS     *
      **************************************
       01 MENU-PRINCIPAL.
		02 BLANK SCREEN.
		02 LINE 01 COL 1  VALUE 
		  "**********************************************".
		02 LINE 01 COL 47 VALUE 
		  "**********************************".
		02 LINE 02 COL 28 VALUE "Sistema de Franquia AMBEV ".
		02 LINE 03 COL 1  VALUE 
		  "**********************************************".
		02 LINE 03 COL 47 VALUE 
		  "**********************************".
		02 LINE 07 COL 33 VALUE "Menu Principal".
		02 LINE 09 COL 10 VALUE "01.00 - Cadastros".
        02 LINE 10 COL 10 VALUE "01.01 - Cadastro de Cliente".
        02 LINE 11 COL 10 VALUE "01.02 - Cadastro de Vendedor".
        02 LINE 13 COL 10 VALUE "02.00 - Relatorios".
        02 LINE 14 COL 10 VALUE "02.01 - Relatorio de Clientes".
        02 LINE 15 COL 10 VALUE "02.02 - Relatorio de Vendedores".
        02 LINE 17 COL 10 VALUE "03.00 - Executar".
        02 LINE 18 COL 10 VALUE
         "03.01 - Executar Distribuicao de Clientes".
        02 LINE 20 COL 10 VALUE "00.00 - Sair do sistema".
        02 LINE 22 COL 10 "Digite a opcao desejada[  .  ]".
        02 OPCAO LINE 22 COL 34 pic X(05) USING WOPCAO AUTO.
      *
       01 TELA-MENU-CLIENTE.
		02 BLANK SCREEN.
		02 LINE 01 COL 1  VALUE 
		  "**********************************************".
		02 LINE 01 COL 47 VALUE 
		  "**********************************".
		02 LINE 02 COL 28 VALUE "Sistema de Franquia AMBEV".
		02 LINE 03 COL 1  VALUE 
		  "**********************************************".
		02 LINE 03 COL 47 VALUE 
		  "**********************************".
        02 LINE 07 COL 33 VALUE "CADASTRO DE CLIENTES".
        02 LINE 09 COL 10 VALUE "1 - Inclusao".
        02 LINE 10 COL 10 VALUE "2 - Alteracao".
        02 LINE 11 COL 10 VALUE "3 - Exclusao".
        02 LINE 12 COL 10 VALUE "4 - Importacao".
		02 LINE 13 COL 10 VALUE "5 - Retornar".
        02 LINE 20 COL 10 "Digite a opcao desejada[.]".
        02 OPCAO LINE 20 COL 34 PIC  X(01) USING W-OPCAO AUTO.
      *
	   01 TELA-CLIENTE.	
		02 BLANK SCREEN.
		02 LINE 01 COL 1  VALUE 
		  "**********************************************".
		02 LINE 01 COL 47 VALUE 
		  "**********************************".
		02 LINE 02 COL 28 VALUE "Sistema de Franquia AMBEV".
		02 LINE 03 COL 1  VALUE 
		  "**********************************************".
		02 LINE 03 COL 47 VALUE 
		  "**********************************".
		02 LINE 07 COL 33 VALUE	"CADASTRO DE CLIENTES".
		02 LINE 07 COL 66 VALUE	"[".
	    02 LINE 07 COL 67 PIC X(10) USING DOPCAO.
		02 LINE 07 COL 77 VALUE	"]".
		02 LINE 09 COL 10 VALUE	"CODIGO       : [       ]".
		02 LINE 09 COL 40 VALUE	"DIGITE [0000000] PARA SAIR". 
		02 LINE 11 COL 10 VALUE "CNPJ         : [".
		02 LINE 11 COL 40 VALUE	"]".
		02 LINE 13 COL 10 VALUE "RAZAO SOCIAL : [".
		02 LINE 13 COL 66 VALUE	"]".
        02 LINE 15 COL 10 VALUE "LATITUDE     : [ ] [           ]".
        02 LINE 17 COL 10 VALUE "LONGITUDE    : [ ] [           ]".
      *
       01 TELA-MENU-VENDEDOR.
		02 BLANK SCREEN.
		02 LINE 01 COL 1  VALUE 
		  "**********************************************".
		02 LINE 01 COL 47 VALUE 
		  "**********************************".
		02 LINE 02 COL 28 VALUE "Sistema de Franquia AMBEV".
		02 LINE 03 COL 1  VALUE 
		  "**********************************************".
		02 LINE 03 COL 47 VALUE 
		  "**********************************".
        02 LINE 07 COL 33 VALUE "CADASTRO DE VENDEDORES".
        02 LINE 09 COL 10 VALUE "1 - Inclusao".
        02 LINE 10 COL 10 VALUE "2 - Alteracao".
        02 LINE 11 COL 10 VALUE "3 - Exclusao".
        02 LINE 12 COL 10 VALUE "4 - Importacao".
		02 LINE 13 COL 10 VALUE "5 - Retornar".
        02 LINE 20 COL 10 "Digite a opcao desejada[.]".
        02 OPCAO LINE 20 COL 34 PIC  X(01) USING W-OPCAO AUTO.
      *
	   01 TELA-VENDEDOR.	
		02 BLANK SCREEN.
		02 LINE 01 COL 1  VALUE 
		  "**********************************************".
		02 LINE 01 COL 47 VALUE 
		  "**********************************".
		02 LINE 02 COL 28 VALUE "Sistema de Franquia AMBEV".
		02 LINE 03 COL 1  VALUE 
		  "**********************************************".
		02 LINE 03 COL 47 VALUE 
		  "**********************************".
		02 LINE 07 COL 33 VALUE	"CADASTRO DE VENDEDORES".
		02 LINE 07 COL 66 VALUE	"[".
	    02 LINE 07 COL 67 PIC X(10) USING DOPCAO.
		02 LINE 07 COL 77 VALUE	"]".
		02 LINE 09 COL 10 VALUE	"CODIGO       : [   ]".
		02 LINE 09 COL 40 VALUE	"DIGITE [000] PARA SAIR". 
		02 LINE 11 COL 10 VALUE "CPF          : [".
		02 LINE 11 COL 37 VALUE	"]".
		02 LINE 13 COL 10 VALUE "NOME VENDEDOR: [".
		02 LINE 13 COL 66 VALUE	"]".
        02 LINE 15 COL 10 VALUE "LATITUDE     : [ ] [           ]".
        02 LINE 17 COL 10 VALUE "LONGITUDE    : [ ] [           ]".
      *
	   01 TELA-ENTRADA-ARQUIVO.
		02 BLANK SCREEN.
		02 LINE 01 COL 1  VALUE 
		  "**********************************************".
		02 LINE 01 COL 47 VALUE 
		  "**********************************".
		02 LINE 02 COL 28 VALUE "Sistema de Franquia AMBEV".
		02 LINE 03 COL 1  VALUE 
		  "**********************************************".
		02 LINE 03 COL 47 VALUE 
		  "**********************************".
		02 LINE 07 COL 33 VALUE	"IMPORTACAO".
		02 LINE 12 COL 01 VALUE "ARQUIVO:".
		02 ENTRADA LINE 12 COL 15 PIC X(50) 
			USING WTD-ENTRA-ARQUIVO AUTO.
        02 LINE 25 COL 12 VALUE "TECLE 'X' PARA SAIR".
      *
	   01 TELA-CLIENTE-REL.	
		02 BLANK SCREEN.
		02 LINE 01 COL 1  VALUE 
		  "**********************************************".
		02 LINE 01 COL 47 VALUE 
		  "**********************************".
		02 LINE 02 COL 28 VALUE "Sistema de Franquia AMBEV".
		02 LINE 03 COL 1  VALUE 
		  "**********************************************".
		02 LINE 03 COL 47 VALUE 
		  "**********************************".
		02 LINE 07 COL 33 VALUE	"RELATORIO DE CLIENTES".
		02 LINE 09 COL 10 VALUE	"TIPO DE ORDENACAO    : [A]".
		02 LINE 09 COL 38 VALUE "[A]SCENDENTE".
		02 LINE 09 COL 51 VALUE "OU [D]ESCENDENTE".
		02 LINE 11 COL 10 VALUE	"TIPO DE CLASSIFICACAO: [C]".
		02 LINE 11 COL 38 VALUE "[C]ODIGO CLIENTE".
		02 LINE 11 COL 55 VALUE "OU [R]AZAO SOCIAL".
		02 LINE 13 COL 10 VALUE	"CODIGO               :".
		02 LINE 13 COL 33 VALUE	"[0000001] A [9999999]".
		02 LINE 15 COL 10 VALUE	"RAZAO SOCIAL         :".
		02 LINE 15 COL 33 VALUE	"[".
		02 LINE 15 COL 75 VALUE	"] A ".
		02 LINE 17 COL 33 VALUE	"[".
		02 LINE 17 COL 75 VALUE	"]".
      *
	   01 TELA-VENDEDOR-REL.	
		02 BLANK SCREEN.
		02 LINE 01 COL 1  VALUE 
		  "**********************************************".
		02 LINE 01 COL 47 VALUE 
		  "**********************************".
		02 LINE 02 COL 28 VALUE "Sistema de Franquia AMBEV".
		02 LINE 03 COL 1  VALUE 
		  "**********************************************".
		02 LINE 03 COL 47 VALUE 
		  "**********************************".
		02 LINE 07 COL 33 VALUE	"RELATORIO DE VENDEDORES".
		02 LINE 09 COL 10 VALUE	"TIPO DE ORDENACAO    : [A]".
		02 LINE 09 COL 38 VALUE "[A]SCENDENTE".
		02 LINE 09 COL 51 VALUE "OU [D]ESCENDENTE".
		02 LINE 11 COL 10 VALUE	"TIPO DE CLASSIFICACAO: [C]".
		02 LINE 11 COL 38 VALUE "[C]ODIGO".
		02 LINE 11 COL 47 VALUE "OU [N]OME DO VENDEDOR".
		02 LINE 13 COL 10 VALUE	"CODIGO               :".
		02 LINE 13 COL 33 VALUE	"[001] A [999]".
		02 LINE 15 COL 10 VALUE	"NOME VENDEDOR        :".
		02 LINE 15 COL 33 VALUE	"[".
		02 LINE 15 COL 75 VALUE	"] A ".
		02 LINE 17 COL 33 VALUE	"[".
		02 LINE 17 COL 75 VALUE	"]".
      *
	   01 TELA-MENS.
		02 LINE 25 COL 12 VALUE "MENSAGEM:".
		02 LINE 25 COL 22 PIC X(50) USING MENS.
		02 X LINE 25 COL 75 PIC X(01) USING W-ENTER AUTO.
      *
