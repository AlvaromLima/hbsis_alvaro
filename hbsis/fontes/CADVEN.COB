      **************************************
      * MANUTENCAO DO CADASTRO DE VENDEDORES *
      **************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADVEN.
	   AUTHOR. ALVARO LIMA.
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   COPY "copybook\SISSLGER.CPY".
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
	   COPY "copybook\SISFDGER.CPY".
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
	   COPY "copybook\SISWSGER.CPY".
	   77 WID-ARQ-VENDEDOR     PIC X(100) VALUE 
	           "dados\VENDEDOR.DAT".
	   77 WRL-ARQ-IMPRESSAO    PIC X(100) VALUE 
	           "relatorios\VENDEDOR.txt".

       77 W-EXISTE-CPFCGC      PIC X(001) VALUE SPACES.
       77 WCODIGO-VENDEDOR     PIC 9(003) VALUE ZEROS.
       77 WCPF                 PIC 9(011) VALUE ZEROS.

       77 W-SINAL1             PIC X(001) VALUE SPACES.
       77 W-SINAL2             PIC X(001) VALUE SPACES.

	   LINKAGE SECTION.
	   01 LS-ID PIC X(5).

       SCREEN SECTION.
	   COPY "copybook\SISSTGER.CPY".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION  USING LS-ID.
       INICIO.
           PERFORM OPEN-VENDEDOR.
	       MOVE "F" TO WFIM.
		   PERFORM TELA-VENDEDOR-0 UNTIL WFIM = "T". 
	   TELA-VENDEDOR-0.
	       MOVE SPACES TO W-OPCAO
           DISPLAY TELA-MENU-VENDEDOR.
           ACCEPT TELA-MENU-VENDEDOR.
           EVALUATE W-OPCAO
             WHEN "1"
			    PERFORM INCLUSAO-000 THRU INCLUSAO-EXIT
             WHEN "2"
			    PERFORM ALTERACAO-000 THRU ALTERACAO-EXIT
		     WHEN "3"
			    PERFORM EXCLUSAO-000 THRU EXCLUSAO-EXIT
             WHEN "4"
			    PERFORM IMPORTACAO-000 THRU IMPORTACAO-EXIT
             WHEN "5"
                PERFORM CLOSE-VENDEDOR
                EXIT PROGRAM
           END-EVALUATE.
		   
      **********************
      * ROTINA DE INCLUSAO *
      **********************
	   INCLUSAO-000.
	       MOVE "INCLUSAO" TO DOPCAO.
           MOVE 0          TO W-SEL.
       INC-001.
		   MOVE ZEROS  TO VEN_CODIGO_VENDEDOR
                          WCODIGO-VENDEDOR
			              VEN_CPF
						  WCPF
			              VEN_LATITUDE
			              VEN_LONGITUDE
						  .
           MOVE SPACES TO VEN_NOME_VENDEDOR
		                  W-EXISTE-CPFCGC
                          WS-GRAVACAO
                          W-SINAL1
                          W-SINAL2
						  .
       INC-010.
		   DISPLAY TELA-VENDEDOR.
		   PERFORM ENTRA-CODIGO UNTIL W-SEL > 0
		   IF W-SEL = 2
              MOVE 0 TO W-SEL
              PERFORM ENTRA-DADOS UNTIL W-SEL > 0
              MOVE 0 TO W-SEL
			  PERFORM CONFIRMA-DADOS UNTIL W-SEL > 0
              PERFORM GRAVACAO 
			  .	
			MOVE ZEROS TO W-SEL
			.
	   INCLUSAO-EXIT.
		   EXIT.

      ***********************
      * ROTINA DE ALTERACAO *
      ***********************
	   ALTERACAO-000.
	       MOVE "ALTERACAO" TO DOPCAO.
           MOVE 0           TO W-SEL.
       ALT-001.
	       PERFORM INC-001.
		   DISPLAY TELA-VENDEDOR.
		   PERFORM ENTRA-CODIGO UNTIL W-SEL > 0
		   IF W-SEL = 2
              MOVE 0 TO W-SEL
              PERFORM ENTRA-DADOS UNTIL W-SEL > 0
              MOVE 0 TO W-SEL
			  PERFORM ALTERA-DADOS UNTIL W-SEL > 0
   		      PERFORM REGRAVACAO
			  .	
			MOVE ZEROS TO W-SEL
			.
	   ALTERACAO-EXIT.
		   EXIT.

      ***********************
      * ROTINA DE EXCLUSAO  *
      ***********************
	   EXCLUSAO-000.
	       MOVE "EXCLUSAO"  TO DOPCAO.
           MOVE 0           TO W-SEL.
       EXC-001.
	       PERFORM INC-001.
		   DISPLAY TELA-VENDEDOR.
		   PERFORM ENTRA-CODIGO UNTIL W-SEL > 0
		   IF W-SEL = 2
              MOVE 0 TO W-SEL
			  PERFORM EXCLUI-DADOS UNTIL W-SEL > 0
   		      PERFORM EXCLUSAO.
			  .	
			MOVE ZEROS TO W-SEL
			.
	   EXCLUSAO-EXIT.
		   EXIT.

      ************************
      * ROTINA DE IMPORTACAO *
      ************************
	   IMPORTACAO-000.
	       MOVE "IMPORTACAO" TO DOPCAO.
           MOVE 0            TO W-SEL.
		   MOVE SPACES       TO WTD-ENTRA-ARQUIVO.
       IMP-001.
	       PERFORM INC-001.
           DISPLAY TELA-ENTRADA-ARQUIVO
		   PERFORM UNTIL W-SEL > 0	
             ACCEPT TELA-ENTRADA-ARQUIVO
		     IF WTD-ENTRA-ARQUIVO = SPACES
			  MOVE 
               	"*** NOME DO ARQUIVO NAO INFORMADO ***" 
			           TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
			 ELSE
			  MOVE 1 TO W-SEL
			 END-IF 
		   END-PERFORM
           IF WTD-ENTRA-ARQUIVO NOT = "X"
		    MOVE 0 TO W-SEL
		    PERFORM CONFIRMA-DADOS UNTIL W-SEL > 0
            MOVE 0            TO W-SEL
            IF W-SN	          = "S"	   
			   MOVE WTD-ENTRA-ARQUIVO TO WTD-ARQERRO
               MOVE WTD-ENTRADA-ARQUIVO TO WTD-ARQ-VENDEDOR
			   PERFORM OPEN-TXT
			   MOVE ZEROS  TO WS-CONT
               PERFORM READ-TXT UNTIL WS-CONT > 1
            .

		   IF WS-GRAVACAO-ERRO > 0
			  MOVE 
			  " IMPORTACAO C/ ERRO - VERIFIQUE ARQUIVO " 
			            TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
		   ELSE
		   IF W-SN = "S"
              MOVE "*** DADOS GRAVADOS COM SUCESSO *** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
			.   

		   MOVE ZEROS TO W-SEL
           CLOSE VENDEDOR-TXT
		         ARQ-ERR
           .
		   
	   IMPORTACAO-EXIT.
		   EXIT.
			
       READ-TXT.
           MOVE "N" TO WS-EOF-SW
		   IF WS-CONT   = 1
              CLOSE VENDEDOR-TXT
              PERFORM OPEN-TXT
			  .
		   PERFORM UNTIL EOF-SW
				READ VENDEDOR-TXT AT END
					MOVE "Y" TO WS-EOF-SW
				END-READ 
               
                IF WS-EOF-SW = "N" AND WS-CONT = 0
					PERFORM VERIFICA-ERROS
				END-IF   
           
				IF WS-EOF-SW = "N"       AND 
				   WS-CONT   = 1         AND
				   WS-GRAVACAO-ERRO > 0
					MOVE "Y" TO WS-EOF-SW
				END-IF   
		   
				IF WS-EOF-SW = "N" AND WS-CONT = 1
					PERFORM PROCESSA-REGISTROS
				END-IF   
		   END-PERFORM
		   ADD 1 TO WS-CONT
           .

	   VERIFICA-ERROS.
           IF R_VEN_CODIGO_VENDEDOR = ZEROS
			  MOVE "CODIGO INVALIDO" TO MENS
			  PERFORM GRAVACAO-ERRO
			  .
		   
           MOVE R_VEN_CODIGO_VENDEDOR TO VEN_CODIGO_VENDEDOR
			                             WCODIGO-VENDEDOR
		   READ ARQ-VENDEDOR
		   
		   MOVE R_VEN_CPF TO VEN_CPF
		   MOVE SPACES TO MENS
           PERFORM ENTRA-DADOS
           IF MENS NOT = SPACES
			  PERFORM GRAVACAO-ERRO
			  .
  		   
		   MOVE R_VEN_NOME_VENDEDOR TO VEN_NOME_VENDEDOR
		   MOVE SPACES TO MENS
           PERFORM ENTRA-NOME-VENDEDOR
           IF MENS NOT = SPACES
			  PERFORM GRAVACAO-ERRO
			  .
		   
	   PROCESSA-REGISTROS.
           MOVE "S" TO WS-GRAVACAO
		   MOVE "-" TO W-SINAL1 W-SINAL2
				 
           MOVE R_VEN_CODIGO_VENDEDOR TO VEN_CODIGO_VENDEDOR

		   READ ARQ-VENDEDOR INVALID KEY
              MOVE "S" TO WS-GRAVACAO
           END-READ
				   
		   MOVE R_VEN_CPF TO VEN_CPF
		   MOVE R_VEN_NOME_VENDEDOR TO VEN_NOME_VENDEDOR
		   MOVE R_VEN_LATITUDE      TO VEN_LATITUDE
		   MOVE R_VEN_LONGITUDE     TO VEN_LONGITUDE
       
           IF W-SINAL1 = "-"
		      MULTIPLY -1 BY VEN_LATITUDE
			     GIVING VEN_LATITUDE
				 .
           IF W-SINAL2 = "-"
		      MULTIPLY -1 BY VEN_LONGITUDE
			     GIVING VEN_LONGITUDE
				 .

		   MOVE "S" TO W-SN
		   PERFORM GRAVACAO
		   IF WS-RESULTADO-ACESSO = "22"
			PERFORM REGRAVACAO
           .
        			
	   GRAVACAO-ERRO.
           IF WS-GRAVACAO-ERRO = 0
		      MOVE 1 TO WS-GRAVACAO-ERRO
		      OPEN OUTPUT ARQ-ERR
			  .
		   MOVE MENS        TO ERRO-MSG
	       MOVE REG-VEN-TXT TO ERRO-REG
		   WRITE REGERR
          .	   

	   GRAVACAO.
		   IF W-SN = "S"
			  WRITE REGVEN
              IF WS-RESULTADO-ACESSO = "00" OR "02"
                 MOVE "*** DADOS GRAVADOS *** " TO MENS
				 IF W-OPCAO NOT = 4	
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
				 END-IF  
              ELSE              
			  IF WS-RESULTADO-ACESSO = "22"
                 MOVE "*** VENDEDOR JA EXISTE *** " TO MENS
				 IF W-OPCAO NOT = 4	
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
				 END-IF  
              ELSE
                 MOVE "ERRO NA GRAVACAO DO ARQUIVO DE VENDEDOR"
                                                   TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
           .	   

	   REGRAVACAO.
		   IF W-SN = "S"
			  REWRITE REGVEN
              IF WS-RESULTADO-ACESSO = "00" OR "02"
                 MOVE "*** REGISTRO ALTERADO ***" TO MENS
				 IF W-OPCAO NOT = 4	
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
				 END-IF  
              ELSE              
                 MOVE "ERRO NA ALTERACAO DE VENDEDOR"
                                                   TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
           .	   

	   EXCLUSAO.
		   IF W-SN = "S"
              DELETE ARQ-VENDEDOR RECORD
              IF WS-RESULTADO-ACESSO = "00"
                 MOVE "*** REGISTRO EXCLUIDO ***" TO MENS
				 IF W-OPCAO NOT = 4	
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
				 END-IF  
              ELSE
                 MOVE "ERRO NA EXCLUSAO DO REGISTRO"
                               				   TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
              .

	   ENTRA-CODIGO.
		   ACCEPT VEN_CODIGO_VENDEDOR AT 0926
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02 OR VEN_CODIGO_VENDEDOR = 0
	          MOVE 1 TO W-SEL
           ELSE
		      MOVE VEN_CODIGO_VENDEDOR TO WCODIGO-VENDEDOR
              READ ARQ-VENDEDOR
              IF WS-RESULTADO-ACESSO NOT = "23"
                 IF WS-RESULTADO-ACESSO = "00"
		          PERFORM MOSTRA-DADOS
                  IF W-OPCAO = 1
		             MOVE "*** VENDEDOR JA CADASTRADO ***" 
				                   TO MENS
	                 PERFORM ROT-MENS THRU 
				                      ROT-MENS-FIM
				     PERFORM INC-001
					 DISPLAY TELA-VENDEDOR
                  ELSE
                     IF W-OPCAO = 2
					    MOVE VEN_CPF TO WCPF
 				     END-IF		
                     MOVE 2 TO W-SEL				  
                 ELSE
		          MOVE "ERRO NA LEITURA ARQUIVO VENDEDOR" 
                                   TO MENS
	              PERFORM ROT-MENS THRU 
				                      ROT-MENS-FIM
     	          MOVE 1 TO W-SEL
 			  ELSE
                IF W-OPCAO = 1			  
		          MOVE 2 TO W-SEL
				ELSE  
		          MOVE "*** VENDEDOR NAO CADASTRADO ***" 
				                   TO MENS
	              PERFORM ROT-MENS THRU 
				                      ROT-MENS-FIM
				.				   
				   
  	   ENTRA-DADOS.
		   IF W-OPCAO NOT = 4	
              ACCEPT VEN_CPF AT 1126
              ACCEPT W-ACT FROM ESCAPE KEY
		      .  
           IF W-ACT = 02
	          MOVE 1 TO W-SEL
		   ELSE  
				MOVE 11      TO  COMPRIMENTO
				MOVE VEN_CPF TO  LKS-NUMERO-I
				MOVE ZEROS TO  LKS-NUMERO-F
				MOVE "CPF" TO  LKS-TIPO-CALCULO
				MOVE "V"   TO  LKS-ACAO
				MOVE ZEROS TO  LKS-RETORNO
				CALL "CADDIGIT" USING PARAMETRO-CGCCPF
				IF LKS-RETORNO NOT = 0 OR 
				   VEN_CPF = ZEROS
				   MOVE "CPF INVALIDO" TO MENS
				   IF W-OPCAO NOT = 4	
				    PERFORM ROT-MENS THRU ROT-MENS-FIM
				   END-IF	
				ELSE
			       MOVE "S"  TO W-EXISTE-CPFCGC
			       READ ARQ-VENDEDOR
				        KEY IS VEN_CPF INVALID KEY
					  MOVE "N" TO W-EXISTE-CPFCGC
                   END-READ
 			   .

           IF W-OPCAO    = 2 OR
		      W-OPCAO    = 4
        		IF VEN_CPF   NOT = WCPF
			      MOVE VEN_CPF TO WCPF
				ELSE	  
				  MOVE "N" TO W-EXISTE-CPFCGC
                .

           IF VEN_CPF        > ZEROS AND
			  LKS-RETORNO    = 0
			   
			  IF W-EXISTE-CPFCGC = "S"
				MOVE "CPF JA CADASTRADO" TO MENS
				IF W-OPCAO NOT = 4	
				   PERFORM ROT-MENS THRU ROT-MENS-FIM
				END-IF   
			  ELSE
			    MOVE WCODIGO-VENDEDOR TO VEN_CODIGO_VENDEDOR
			    READ ARQ-VENDEDOR
     		      KEY IS VEN_CODIGO_VENDEDOR INVALID KEY
				    IF W-OPCAO NOT = 4	
	                   MOVE SPACES TO VEN_NOME_VENDEDOR
					   MOVE ZEROS  TO VEN_LATITUDE
			                          VEN_LONGITUDE
					END-IF				  
			    END-READ

                IF W-OPCAO    = 2 OR
				   W-OPCAO    = 4
 				   MOVE WCPF TO VEN_CPF
				END-IF   
				
                IF W-OPCAO    = 1 OR 
				   W-OPCAO    = 2
					PERFORM ENTRA-NOME-VENDEDOR 
						UNTIL W-SEL > 0
					MOVE 0 TO W-SEL	  
					PERFORM ENTRA-LOCALIZACAO						  
						UNTIL W-SEL > 0
					MOVE 1 TO W-SEL
			.
	
	   ENTRA-NOME-VENDEDOR.
           IF W-OPCAO    = 1 OR 
			  W-OPCAO    = 2
			  ACCEPT VEN_NOME_VENDEDOR AT 1326
              ACCEPT W-ACT FROM ESCAPE KEY
			  .
           IF W-ACT = 02
	          MOVE 1 TO W-SEL
		   ELSE  
				IF VEN_NOME_VENDEDOR = SPACES
				   MOVE "NOME VENDEDOR INVALIDO" TO MENS
				   IF W-OPCAO NOT = 4	
				    PERFORM ROT-MENS THRU ROT-MENS-FIM
				   END-IF   
			    ELSE
			       MOVE 1 TO W-SEL
			.
			   
       ENTRA-LOCALIZACAO.
		   ACCEPT W-SINAL1 AT 1526
		   ACCEPT VEN_LATITUDE AT 1530
		   IF VEN_LATITUDE > ZEROS
		      MULTIPLY 1 BY VEN_LATITUDE 
				  GIVING VEN_LATITUDE
		      IF W-SINAL1 = "-"
		        MULTIPLY -1 BY VEN_LATITUDE 
				  GIVING VEN_LATITUDE
		   .
		   
  		   ACCEPT W-SINAL2 AT 1726
  		   ACCEPT VEN_LONGITUDE AT 1730
		   IF VEN_LONGITUDE > ZEROS
		      MULTIPLY 1 BY VEN_LONGITUDE 
				  GIVING VEN_LONGITUDE
		      IF W-SINAL2 = "-"
		        MULTIPLY -1 BY VEN_LONGITUDE 
				  GIVING VEN_LONGITUDE
		   .
		   MOVE 1 TO W-SEL
		   .
			
	   CONFIRMA-DADOS.
           MOVE "S" TO W-SN
           DISPLAY "DADOS OK (S/N) : " AT 2340
           ACCEPT W-SN WITH UPDATE AT 2357
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
	          MOVE 1 TO W-SEL
		   ELSE  
                IF W-SN = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE 1 TO W-SEL
                ELSE
				IF W-SN NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                ELSE
		           MOVE 2 TO W-SEL
                .
				
	   ALTERA-DADOS.
           MOVE "N" TO W-SN
           DISPLAY "ALTERAR (S/N) : " AT 2340
           ACCEPT W-SN WITH UPDATE AT 2357
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
	          MOVE 1 TO W-SEL
		   ELSE  
                IF W-SN = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE 1 TO W-SEL
                ELSE
				IF W-SN NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                ELSE
		           MOVE 2 TO W-SEL
                .

       EXCLUI-DADOS.
           MOVE "N" TO W-SN
           DISPLAY "EXCLUIR  (S/N) : " AT 2340
           ACCEPT W-SN WITH UPDATE AT 2357
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
	          MOVE 1 TO W-SEL
		   ELSE  
                IF W-SN = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE 1 TO W-SEL
                ELSE
                IF W-SN NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                ELSE
		           MOVE 2 TO W-SEL
                .
				
       MOSTRA-DADOS.
		   DISPLAY VEN_CODIGO_VENDEDOR AT 0926
		   DISPLAY VEN_CPF AT 1126
		   DISPLAY VEN_NOME_VENDEDOR AT 1326
		   DISPLAY VEN_LATITUDE AT 1530
		   DISPLAY VEN_LONGITUDE AT 1730
		   IF VEN_LATITUDE < ZEROS
		      MOVE "-" TO W-SINAL1
			  DISPLAY W-SINAL1 AT 1526
		   ELSE	  
		      MOVE "+" TO W-SINAL1
			  DISPLAY W-SINAL1 AT 1526
		   .
		   
		   IF VEN_LONGITUDE < ZEROS
		      MOVE "-" TO W-SINAL2
		      DISPLAY W-SINAL2 AT 1726
		   ELSE	  
		      MOVE "+" TO W-SINAL2
			  DISPLAY W-SINAL2 AT 1726
		   .
		   .

      *******************************
      * ROTINA DE ABERTURA ARQUIVOS *
      *******************************
	   OPEN-VENDEDOR.	   
           OPEN I-O ARQ-VENDEDOR
           IF WS-RESULTADO-ACESSO NOT = "00"
               IF WS-RESULTADO-ACESSO = "05"
                 OPEN OUTPUT ARQ-VENDEDOR
                 CLOSE ARQ-VENDEDOR
				 OPEN I-O ARQ-VENDEDOR
               ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO VENDEDOR" 
				        TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
    			 PERFORM CLOSE-VENDEDOR

                 EXIT PROGRAM
				 .

       OPEN-TXT.
	       OPEN INPUT VENDEDOR-TXT
           IF WS-FS NOT = "00"
			IF WS-FS = "30"
			   MOVE "*** ARQUIVO NAO ENCONTRADO ***" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
			ELSE
               MOVE WS-FS       TO ERRO
			   MOVE 
			   " ERRO NA ABERTURA DO ARQUIVO VENDEDOR.TXT" 
			                    TO MENS-P
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               PERFORM CLOSE-VENDEDOR
               EXIT PROGRAM
            .

		CLOSE-VENDEDOR.
		   CLOSE ARQ-VENDEDOR
		   .
		   
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY TELA-MENS.
           ACCEPT TELA-MENS.
       ROT-MENS2.
		   MOVE SPACES TO MENS
           DISPLAY TELA-MENS.
       ROT-MENS-FIM.
           EXIT.
