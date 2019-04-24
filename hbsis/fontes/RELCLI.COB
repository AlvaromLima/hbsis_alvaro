      **************************************
      * RELATORIO DO CADASTRO DE CLIENTES  *
      **************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELCLI.
      *------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   COPY "copybook\SISSLGER.CPY".

      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
	   COPY "copybook\SISFDGER.CPY".
      *
      *------------------------------------------------------
       WORKING-STORAGE SECTION.
	   COPY "copybook\SISWSGER.CPY".
	   77 WID-ARQ-CLIENTE      PIC X(100) VALUE 
	           "dados\CLIENTE.DAT".
	   77 WRL-ARQ-IMPRESSAO    PIC X(100) VALUE 
	           "relatorios\CLIENTE.txt".

	   77 W-CODIGO1    PIC 9(07) VALUE 0000001.
	   77 W-CODIGO2    PIC 9(07) VALUE 9999999.
	   77 W-RAZAO1     PIC X(40) VALUE SPACES.
	   77 W-RAZAO2     PIC X(40) VALUE SPACES.

       77 W-ORDEM              PIC X(001) VALUE "A".
       77 W-CLASSIFICACAO      PIC X(001) VALUE "C".

       01 CABEC.
          03 FILLER PIC X(08) VALUE "CODIGO".
          03 FILLER PIC X(15) VALUE "CNPJ".
          03 FILLER PIC X(43) VALUE "NOME".
          03 FILLER PIC X(15) VALUE "LATITUDE".
          03 FILLER PIC X(51) VALUE "LONGITUDE".

       01 DETAL.
          03 IMPCOD       PIC X(07).
          03 FILLER       PIC X(01) VALUE SPACES.
          03 IMPCNPJ      PIC X(14).
          03 FILLER       PIC X(01) VALUE SPACES.
          03 IMPNOME      PIC X(40).
          03 FILLER       PIC X(01) VALUE SPACES.
          03 IMPSINAL1    PIC X(01).
          03 IMPLATITUDE  PIC ZZ9,99999999B(02).
          03 IMPSINAL2    PIC X(01).
          03 IMPLONGITUDE PIC ZZ9,99999999B(38).
      *-------------------------------------------------------
       SCREEN SECTION.
	   COPY "copybook\SISSTGER.CPY".

      *-------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE SPACES TO W-RAZAO1
		                  W-RAZAO2
		   MOVE "A"    TO W-ORDEM
           MOVE "C"    TO W-CLASSIFICACAO		   
	       MOVE 0000001 TO W-CODIGO1
	       MOVE 9999999 TO W-CODIGO2
		   .
      *
       INC-OP0.
           OPEN INPUT ARQ-CLIENTE.
           IF WS-RESULTADO-ACESSO NOT = "00"
              MOVE "ERRO ABERTURA DO ARQ. CLIENTES" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              PERFORM ROT-FIM
              EXIT PROGRAM
			  .
           OPEN OUTPUT ARQIMP.

       MONTA-TELA.
	       DISPLAY TELA-CLIENTE-REL
		   
           MOVE 0 TO W-SEL
		   PERFORM UNTIL W-SEL > 0 
             ACCEPT W-ORDEM AT 0934
             IF W-ORDEM NOT = "A" AND
		        W-ORDEM NOT = "D"
                MOVE "ORDEM INVALIDA"  TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
             ELSE
			    MOVE 1 TO W-SEL
		     END-IF		
           END-PERFORM
		   
           MOVE 0 TO W-SEL
		   PERFORM UNTIL W-SEL > 0 
    		 ACCEPT W-CLASSIFICACAO AT 1134
             IF W-CLASSIFICACAO NOT = "C" AND
		        W-CLASSIFICACAO NOT = "R"
                MOVE "CLASSIFICACAO INVALIDA"  TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
             ELSE
			    MOVE 1 TO W-SEL
		     END-IF		
           END-PERFORM

           MOVE 0 TO W-SEL
		   PERFORM UNTIL W-SEL > 0 
			 ACCEPT W-CODIGO1 AT 1334
			 IF W-CODIGO1 = "0000000" 	
                MOVE "CODIGO INICIAL INVALIDO"  TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
             ELSE
			    MOVE 1 TO W-SEL
		     END-IF		
	       END-PERFORM
		   
           MOVE 0 TO W-SEL
		   PERFORM UNTIL W-SEL > 0 
			 ACCEPT W-CODIGO2 AT 1346
			 IF W-CODIGO2 < W-CODIGO1
                MOVE "CODIGO FINAL MENOR QUE INICIAL" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
             ELSE
			    MOVE 1 TO W-SEL
		     END-IF		
	       END-PERFORM

		   ACCEPT W-RAZAO1 AT 1534
           MOVE 0 TO W-SEL
		   PERFORM UNTIL W-SEL > 0 
			 ACCEPT W-RAZAO2 AT 1734
			 IF W-RAZAO2 < W-RAZAO1
                MOVE 
	            "RAZAO SOCIAL FINAL E MENOR QUE RAZAO INICIAL" 
				                 TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
             ELSE
			    MOVE 1 TO W-SEL
		     END-IF		
	       END-PERFORM
		   
		   IF W-RAZAO1 = SPACES    AND
		      W-RAZAO2 = SPACES
			  MOVE "A" TO W-RAZAO1
			  MOVE "Z" TO W-RAZAO2
			  .
			  				
           MOVE 0 TO W-SEL
		   PERFORM CONFIRMA-TELA UNTIL W-SEL > 0
 		   .
		   
       LER-CLIENTE.
	   	   IF W-ORDEM = "A"
    		  IF W-CLASSIFICACAO = "C"
	             MOVE ZEROS TO CLI_CODIGO_CLIENTE
                 START ARQ-CLIENTE 
                    KEY IS GREATER THAN CLI_CODIGO_CLIENTE
					   INVALID KEY 
         	            MOVE "Y" TO WS-EOF-SW
	             END-START
		      ELSE
	             MOVE SPACES TO CLI_RAZAO_SOCIAL
                 START ARQ-CLIENTE 
	                KEY IS GREATER THAN CLI_RAZAO_SOCIAL
					   INVALID KEY 
         	            MOVE "Y" TO WS-EOF-SW
	             END-START
		     END-IF
           ELSE
    		  IF W-CLASSIFICACAO = "C"
	             MOVE 9999999 TO CLI_CODIGO_CLIENTE
                 START ARQ-CLIENTE 
	                KEY IS LESS THAN CLI_CODIGO_CLIENTE
					   INVALID KEY 
         	            MOVE "Y" TO WS-EOF-SW
	             END-START
		      ELSE
	             MOVE HIGH-VALUES TO CLI_RAZAO_SOCIAL
                 START ARQ-CLIENTE 
	                KEY IS LESS THAN CLI_RAZAO_SOCIAL
					   INVALID KEY 
         	            MOVE "Y" TO WS-EOF-SW
	             END-START
              .		   

	       MOVE "N" TO WS-EOF-SW
	       
		   IF W-SN = "S"
		     PERFORM ROT-CABEC

			 IF W-ORDEM = "A"
	          READ ARQ-CLIENTE NEXT AT END
				  MOVE "Y" TO WS-EOF-SW
			  END-READ 
			  PERFORM UNTIL EOF-SW  
			   IF WS-EOF-SW = "N"
	               PERFORM CLASSIFICACAO-DADOS 	
				   READ ARQ-CLIENTE NEXT AT END
				     MOVE "Y" TO WS-EOF-SW
			       END-READ 
			   END-IF
		      END-PERFORM
             ELSE
	          READ ARQ-CLIENTE PREVIOUS AT END
				  MOVE "Y" TO WS-EOF-SW
			  END-READ 
			  PERFORM UNTIL EOF-SW  
			   IF WS-EOF-SW = "N"
	               PERFORM CLASSIFICACAO-DADOS 	
				   READ ARQ-CLIENTE PREVIOUS AT END
				     MOVE "Y" TO WS-EOF-SW
			       END-READ 
			   END-IF
		      END-PERFORM
			.
			 
           IF WS-EOF-SW = "Y"
		      MOVE "RELATORIO DE CLIENTES OK" 
					    TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              .
  
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           CLOSE ARQ-CLIENTE.
	   ROT-EXIT.
           CLOSE ARQIMP.
           EXIT PROGRAM.

       ROT-FIMS.
           STOP RUN.
      *

      ***************************
      * ROTINA DE IMPRIME DADOS *
      ***************************
       ROT-CABEC.
           ADD 1 TO CONPAG
           WRITE REGIMP FROM CABEC.
           MOVE SPACES TO REGIMP.
           WRITE REGIMP.

       IMPRIME-DADOS.
           MOVE CLI_CODIGO_CLIENTE TO IMPCOD.
		   MOVE CLI_CNPJ           TO IMPCNPJ 
           MOVE CLI_RAZAO_SOCIAL   TO IMPNOME.
           MOVE CLI_LATITUDE       TO IMPLATITUDE.
           MOVE CLI_LONGITUDE      TO IMPLONGITUDE.
		   MOVE "+" TO IMPSINAL1
           IF CLI_LATITUDE < ZEROS 
		      MOVE "-" TO IMPSINAL1
			  .
		   MOVE "+" TO IMPSINAL2
           IF CLI_LONGITUDE < ZEROS 
		      MOVE "-" TO IMPSINAL2
              .
			  
           WRITE REGIMP FROM DETAL.
           ADD 1 TO CONLIN
           IF CONLIN > LINPAG
              MOVE ZEROS TO CONLIN
              MOVE SPACES TO REGIMP
              WRITE REGIMP
              WRITE REGIMP
              WRITE REGIMP
              WRITE REGIMP
              PERFORM ROT-CABEC		   
           .

	   CONFIRMA-TELA.
           MOVE "S" TO W-SN
           DISPLAY "CONFIRMA (S/N) : " AT 2340
           ACCEPT W-SN WITH UPDATE AT 2357

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

	   CLASSIFICACAO-DADOS. 	
           IF W-CLASSIFICACAO = "C"
	          IF CLI_CODIGO_CLIENTE NOT < W-CODIGO1 AND 
		         CLI_CODIGO_CLIENTE NOT > W-CODIGO2
			     PERFORM IMPRIME-DADOS
              .
			  
		   IF W-CLASSIFICACAO = "R"
		      IF CLI_RAZAO_SOCIAL NOT < W-RAZAO1 AND 
		         CLI_RAZAO_SOCIAL NOT > W-RAZAO2
				 PERFORM IMPRIME-DADOS
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
