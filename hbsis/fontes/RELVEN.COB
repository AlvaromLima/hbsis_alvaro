      ***************************************
      * RELATORIO DO CADASTRO DE VENDEDORES *
      ***************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELVEN.
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
	   77 WID-ARQ-VENDEDOR      PIC X(100) VALUE 
	           "dados\VENDEDOR.DAT".
	   77 WRL-ARQ-IMPRESSAO    PIC X(100) VALUE 
	           "relatorios\VENDEDOR.txt".

	   77 W-CODIGO1    PIC 9(03) VALUE 001.
	   77 W-CODIGO2    PIC 9(03) VALUE 999.
	   77 W-NOME1      PIC X(40) VALUE SPACES.
	   77 W-NOME2      PIC X(40) VALUE SPACES.

       77 W-ORDEM              PIC X(001) VALUE "A".
       77 W-CLASSIFICACAO      PIC X(001) VALUE "C".

       01 CABEC.
          03 FILLER PIC X(08) VALUE "CODIGO".
          03 FILLER PIC X(15) VALUE "CPF".
          03 FILLER PIC X(43) VALUE "NOME".
          03 FILLER PIC X(15) VALUE "LATITUDE".
          03 FILLER PIC X(51) VALUE "LONGITUDE".

       01 DETAL.
          03 IMPCOD       PIC X(07).
          03 FILLER       PIC X(01) VALUE SPACES.
          03 IMPCPF       PIC X(14).
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
           MOVE SPACES TO W-NOME1
		                  W-NOME2
		   MOVE "A"    TO W-ORDEM
           MOVE "C"    TO W-CLASSIFICACAO		   
	       MOVE 001    TO W-CODIGO1
	       MOVE 999    TO W-CODIGO2
		   .
      *
       INC-OP0.
           OPEN INPUT ARQ-VENDEDOR.
           IF WS-RESULTADO-ACESSO NOT = "00"
              MOVE "ERRO ABERTURA DO ARQ. VENDEDORES" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              PERFORM ROT-FIM
              EXIT PROGRAM
              .
           OPEN OUTPUT ARQIMP.

       MONTA-TELA.
	       DISPLAY TELA-VENDEDOR-REL
		   
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
		        W-CLASSIFICACAO NOT = "N"
                MOVE "CLASSIFICACAO INVALIDA"  TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
             ELSE
			    MOVE 1 TO W-SEL
		     END-IF		
           END-PERFORM

           MOVE 0 TO W-SEL
		   PERFORM UNTIL W-SEL > 0 
			 ACCEPT W-CODIGO1 AT 1334
			 IF W-CODIGO1 = "000" 	
                MOVE "CODIGO INICIAL INVALIDO"  TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
             ELSE
			    MOVE 1 TO W-SEL
		     END-IF		
	       END-PERFORM
		   
           MOVE 0 TO W-SEL
		   PERFORM UNTIL W-SEL > 0 
			 ACCEPT W-CODIGO2 AT 1342
			 IF W-CODIGO2 < W-CODIGO1
                MOVE "CODIGO FINAL MENOR QUE INICIAL" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
             ELSE
			    MOVE 1 TO W-SEL
		     END-IF		
	       END-PERFORM

		   ACCEPT W-NOME1 AT 1534
           MOVE 0 TO W-SEL
		   PERFORM UNTIL W-SEL > 0 
    	     ACCEPT W-NOME2 AT 1734
			 IF W-NOME2 < W-NOME1
                MOVE 
	            "NOME FINAL E MENOR QUE O NOME INICIAL" 
				                 TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
             ELSE
			    MOVE 1 TO W-SEL
		     END-IF		
	       END-PERFORM
           .

  		   IF W-NOME1 = SPACES    AND
		      W-NOME2 = SPACES
			  MOVE "A" TO W-NOME1
			  MOVE "Z" TO W-NOME2
			  .

           MOVE 0 TO W-SEL
		   PERFORM CONFIRMA-TELA UNTIL W-SEL > 0
 		   .

       LER-VENDEDOR.
		   IF W-ORDEM = "A"
		      IF W-CLASSIFICACAO = "C"
	            MOVE ZEROS TO VEN_CODIGO_VENDEDOR
                START ARQ-VENDEDOR 
	               KEY IS GREATER THAN VEN_CODIGO_VENDEDOR
						INVALID KEY 
         	              MOVE "Y" TO WS-EOF-SW
	            END-START
		      ELSE
	            MOVE SPACES TO VEN_NOME_VENDEDOR
                START ARQ-VENDEDOR 
			       KEY IS GREATER THAN VEN_NOME_VENDEDOR
						INVALID KEY 
         	              MOVE "Y" TO WS-EOF-SW
	            END-START
			  END-IF	
		   ELSE		
		      IF W-CLASSIFICACAO = "C"
	            MOVE 999 TO VEN_CODIGO_VENDEDOR
                START ARQ-VENDEDOR
                   KEY IS LESS THAN VEN_CODIGO_VENDEDOR
						INVALID KEY 
         	              MOVE "Y" TO WS-EOF-SW
	            END-START
		      ELSE
	            MOVE HIGH-VALUES TO VEN_NOME_VENDEDOR
                START ARQ-VENDEDOR 
	               KEY IS LESS THAN VEN_NOME_VENDEDOR
						INVALID KEY 
         	              MOVE "Y" TO WS-EOF-SW
	            END-START
	            .
              
	       MOVE "N" TO WS-EOF-SW
		   
	       IF W-SN = "S"
		     PERFORM ROT-CABEC

			 IF W-ORDEM = "A"
				READ ARQ-VENDEDOR NEXT AT END
					MOVE "Y" TO WS-EOF-SW
				END-READ 

			    PERFORM UNTIL EOF-SW
				  IF WS-EOF-SW = "N"
				   PERFORM CLASSIFICACAO-DADOS
				   READ ARQ-VENDEDOR NEXT AT END
					 MOVE "Y" TO WS-EOF-SW
				   END-READ 
                  END-IF
		        END-PERFORM
             ELSE
				READ ARQ-VENDEDOR PREVIOUS AT END
					MOVE "Y" TO WS-EOF-SW
				END-READ 

			    PERFORM UNTIL EOF-SW
				  IF WS-EOF-SW = "N"
				   PERFORM CLASSIFICACAO-DADOS
				   READ ARQ-VENDEDOR PREVIOUS AT END
					 MOVE "Y" TO WS-EOF-SW
				   END-READ 
                  END-IF
		        END-PERFORM
			 .
			 
           IF WS-EOF-SW = "Y"
		      MOVE "RELATORIO DE VENDEDORES OK" 
					    TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              .
  
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           CLOSE ARQ-VENDEDOR.
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
           MOVE VEN_CODIGO_VENDEDOR TO IMPCOD.
		   MOVE VEN_CPF             TO IMPCPF 
           MOVE VEN_NOME_VENDEDOR   TO IMPNOME.
           MOVE VEN_LATITUDE        TO IMPLATITUDE.
           MOVE VEN_LONGITUDE       TO IMPLONGITUDE.
		   MOVE "+" TO IMPSINAL1
           IF VEN_LATITUDE < ZEROS 
		      MOVE "-" TO IMPSINAL1
			  .
		   MOVE "+" TO IMPSINAL2
           IF VEN_LONGITUDE < ZEROS 
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
	          IF VEN_CODIGO_VENDEDOR NOT < W-CODIGO1 AND 
		         VEN_CODIGO_VENDEDOR NOT > W-CODIGO2
				 PERFORM IMPRIME-DADOS
			  .

		   IF W-CLASSIFICACAO = "N"
		      IF VEN_NOME_VENDEDOR NOT < W-NOME1 AND 
		         VEN_NOME_VENDEDOR NOT > W-NOME2
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
