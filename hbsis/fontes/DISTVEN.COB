      *******************************************
      * DISTRIBUICAO DE CLIENTES NOS VENDEDORES *
      *******************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISTVEN.
	   AUTHOR. ALVARO LIMA.
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   COPY "copybook\SISSLGER.CPY".
		   SELECT ARQ-WRK ASSIGN TO WID-ARQ-WRK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS WRK-CODIGO-CLIENTE
				  LOCK MODE    IS MANUAL
                  FILE STATUS  IS WS-RESULTADO-ACESSO.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
	   COPY "copybook\SISFDGER.CPY".
       FD ARQ-WRK
           LABEL RECORD IS STANDARD.
       01 REGWRK.
          03 WRK-CODIGO-CLIENTE      PIC 9(007).
          03 FILLER                  PIC X(013).
 
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
	   COPY "copybook\SISWSGER.CPY".
       77 WID-ARQ-CLIENTE      PIC X(100) VALUE 
	           "dados\CLIENTE.DAT".
	   77 WID-ARQ-VENDEDOR     PIC X(100) VALUE 
	           "dados\VENDEDOR.DAT".
	   77 WID-ARQ-WRK          PIC X(100) VALUE 
	           "temp\ARQ-WRK.DAT".
	   77 WRL-ARQ-CSV          PIC X(100) VALUE        		   
	           "exportacao\vencli.csv".

	   77 WS-EOF               PIC X(001) VALUE "N".
       77 WS-METROS-AUX	       PIC 9(015) VALUE ZEROS.
       77 WS-METROS            PIC 9(15)       VALUE ZEROS.

	   77 WS-CODIGO-CLIENTE    PIC 9(007) VALUE ZEROS.
	   77 WS-RAZAO-SOCIAL      PIC X(040) VALUE SPACES.
	   
       01 WR-WRK-C-CSV.
	     03 F PIC X(016) VALUE '"CODIGO CLIENTE"'.
         03 F PIC X(001) VALUE ";".
	     03 F PIC X(014) VALUE '"RAZAO SOCIAL"'.
         03 F PIC X(001) VALUE ";".
	     03 F PIC X(017) VALUE '"CODIGO VENDEDOR"'.
         03 F PIC X(001) VALUE ";".
	     03 F PIC X(018) VALUE '"NOME DO VENDEDOR"'.
         03 F PIC X(001) VALUE ";".
	     03 F PIC X(011) VALUE '"DISTANCIA"'.

       01 WR-WRK-CSV.
	     03 WRK-CSV-CODIGOCLIENTE  PIC 9(007) VALUE ZEROS.
         03 F                      PIC X(001) VALUE ";".
         03 F                      PIC X(001) VALUE '"'.
		 03 WRK-CSV-RAZAOSOCIAL    PIC X(040) VALUE SPACES.
         03 F                      PIC X(001) VALUE '"'.
         03 F                      PIC X(001) VALUE ";".
         03 WRK-CSV-CODIGOVENDEDOR PIC 9(003) VALUE ZEROS.
         03 F                      PIC X(001) VALUE ";".
         03 F                      PIC X(001) VALUE '"'.
		 03 WRK-CSV-NOMEVENDEDOR   PIC X(040) VALUE SPACES.
         03 F                      PIC X(001) VALUE '"'.
         03 F                      PIC X(001) VALUE ";".
         03 WRK-CSV-DISTANCIA      PIC 9(010) VALUE ZEROS.
         03 F                      PIC X(001) VALUE ";".
 		 
       01 WDIST-2PONTOS.	   
	     03 P1LA    PIC S9(3)V99999999 VALUE ZEROS.
	     03 P1LO    PIC S9(3)V99999999 VALUE ZEROS.
	     03 P2LA    PIC S9(3)V99999999 VALUE ZEROS.
	     03 P2LO    PIC S9(3)V99999999 VALUE ZEROS.
         03 DLAT    PIC S9(3)V99999999 VALUE ZEROS.
         03 DLONG   PIC S9(3)V99999999 VALUE ZEROS.
	   
	     03 RAIOS   PIC 9(09)V9(03) VALUE ZEROS.
	     03 PI      PIC 9(01)V9(20) VALUE ZEROS.

         03 SIN_ADLAT      PIC 9(05)V9(18) VALUE ZEROS.
         03 COS_P1LA       PIC 9(05)V9(18) VALUE ZEROS.
         03 COS_P2LA       PIC 9(05)V9(18) VALUE ZEROS.
         03 SIN_ADLONG     PIC 9(05)V9(18) VALUE ZEROS.
         03 SQRT_A1        PIC 9(05)V9(18) VALUE ZEROS.
         03 SQRT_A1_X      PIC 9(05)V9(18) VALUE ZEROS.
         03 ATAN2_SQRT_A1  PIC 9(05)V9(18) VALUE ZEROS.
         03 A1             PIC 9(05)V9(18) VALUE ZEROS.
         03 C1             PIC 9(05)V9(18) VALUE ZEROS.

	   LINKAGE SECTION.
	   01 LS-ID PIC X(5).

       SCREEN SECTION.
	   COPY "copybook\SISSTGER.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION  USING LS-ID.
       INICIO.
       INC-OP0.
           MOVE ZEROS TO WS-FLAG-CSV
	       MOVE "N" TO WS-EOF W-SN
	       MOVE "N" TO WS-EOF-SW
		   PERFORM OPEN-ARQUIVOS.
	       
		   MOVE "N" TO WS-EOF-SW
           PERFORM READ-VENDEDOR UNTIL WS-EOF-SW = "Y".
		   MOVE "N" TO WS-EOF-SW
           PERFORM READ-WRK UNTIL WS-EOF-SW = "Y".
           PERFORM CLOSE-ARQUIVOS.
		   
		   IF W-SN = "Y"
              MOVE "*** PROCESSAMENTO COM SUCESSO *** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
           ELSE
              MOVE "*** PROCESSAMENTO COM ERRO. VERIFIQUE *** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
		   .   

           EXIT PROGRAM
		   .
		   
	   READ-VENDEDOR.
	       MOVE ZEROS TO VEN_CODIGO_VENDEDOR
           START ARQ-VENDEDOR 
	           KEY IS GREATER THAN VEN_CODIGO_VENDEDOR
				INVALID KEY 
         	       MOVE "Y" TO WS-EOF-SW
	       END-START
		   READ ARQ-VENDEDOR NEXT AT END
                   MOVE "Y" TO WS-EOF-SW
		   END-READ 
		   
		   PERFORM UNTIL WS-EOF-SW = "Y"
               IF WS-EOF-SW = "N"
                   MOVE VEN_LATITUDE   TO P1LA
		           MOVE VEN_LONGITUDE  TO P1LO

				   MOVE "N"    TO WS-EOF
				   MOVE ZEROS  TO WS-METROS-AUX
	                              WS-CODIGO-CLIENTE
	               MOVE SPACES TO WS-RAZAO-SOCIAL

                   PERFORM READ-CLIENTE UNTIL WS-EOF = "Y"
		           PERFORM MONTA-CSV
			   END-IF

			   READ ARQ-VENDEDOR NEXT AT END
				    MOVE "Y" TO WS-EOF-SW
					            W-SN
			   END-READ 
		   END-PERFORM
		   .  

	   READ-CLIENTE.
	       MOVE ZEROS TO CLI_CODIGO_CLIENTE
           START ARQ-CLIENTE
	           KEY IS GREATER THAN CLI_CODIGO_CLIENTE
					INVALID KEY 
         	          MOVE "Y" TO WS-EOF
	       END-START
	       READ ARQ-CLIENTE NEXT AT END
			   MOVE "Y" TO WS-EOF
		   END-READ 

           MOVE CLI_CODIGO_CLIENTE TO WS-CODIGO-CLIENTE
           MOVE CLI_RAZAO_SOCIAL   TO WS-RAZAO-SOCIAL

		   PERFORM UNTIL WS-EOF = "Y"
				IF WS-EOF = "N"

                   MOVE CLI_LATITUDE       TO P2LA
                   MOVE CLI_LONGITUDE      TO P2LO

      * CALCULA O METRO PELA COORDENADAS LATITUDE E LONGITUDE  
                   MOVE ZEROS TO WS-METROS
                   PERFORM CALCULO-DISTANCIA-ENTRE-2PONTOS
                   IF WS-METROS-AUX = ZEROS
				      MOVE WS-METROS  TO WS-METROS-AUX
				   END-IF

				   IF WS-METROS < WS-METROS-AUX
                      MOVE CLI_CODIGO_CLIENTE    TO WRK-CODIGO-CLIENTE
			          READ ARQ-WRK INVALID KEY
                         MOVE CLI_CODIGO_CLIENTE TO WS-CODIGO-CLIENTE
                         MOVE CLI_RAZAO_SOCIAL   TO WS-RAZAO-SOCIAL
                         MOVE WS-METROS TO WS-METROS-AUX
				      END-READ
                   END-IF

			    END-IF

 	            READ ARQ-CLIENTE NEXT AT END
				  MOVE "Y" TO WS-EOF
			    END-READ 

		   END-PERFORM
           .

	   MONTA-CSV.
	   
            IF WS-FLAG-CSV = 0
			   MOVE SPACES TO REGCSV
			   MOVE WR-WRK-C-CSV TO REGCSV
			   PERFORM GRAVA-CSV
			   .

  		    MOVE SPACES TO REGCSV

	        MOVE WS-CODIGO-CLIENTE   TO WRK-CSV-CODIGOCLIENTE
		    MOVE WS-RAZAO-SOCIAL     TO WRK-CSV-RAZAOSOCIAL

            MOVE VEN_CODIGO_VENDEDOR TO WRK-CSV-CODIGOVENDEDOR
		    MOVE VEN_NOME_VENDEDOR   TO WRK-CSV-NOMEVENDEDOR
            MOVE WS-METROS-AUX       TO WRK-CSV-DISTANCIA

			MOVE WR-WRK-CSV TO REGCSV
			PERFORM GRAVA-CSV
            .

			MOVE 1 TO WS-FLAG-CSV 

            MOVE SPACES TO REGWRK
            MOVE WS-CODIGO-CLIENTE   TO WRK-CODIGO-CLIENTE
			PERFORM GRAVA-WRK
			.

	   GRAVA-CSV.
           WRITE REGCSV.
		   
	   GRAVA-WRK.
           WRITE REGWRK.
           IF WS-RESULTADO-ACESSO = "00" OR "02"
              MOVE "*** DADOS GRAVADOS *** "   TO MENS
           ELSE              
			  IF WS-RESULTADO-ACESSO = "22"
                 MOVE "*** WRK JA EXISTE *** " TO MENS
              ELSE
                 MOVE "ERRO NA GRAVACAO DO ARQUIVO DE ARQ-WRK"
                                               TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
           .	   

       OPEN-ARQUIVOS.
           OPEN INPUT ARQ-CLIENTE.
           IF WS-RESULTADO-ACESSO NOT = "00"
              MOVE "ERRO ABERTURA DO ARQ. CLIENTES" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              PERFORM ROT-FIM
              EXIT PROGRAM
			  .

           OPEN INPUT ARQ-VENDEDOR.
           IF WS-RESULTADO-ACESSO NOT = "00"
              MOVE "ERRO ABERTURA DO ARQ. VENDEDORES" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              PERFORM ROT-FIM
              EXIT PROGRAM
              .
			  
           OPEN I-O ARQ-WRK
           IF WS-RESULTADO-ACESSO NOT = "00"
               IF WS-RESULTADO-ACESSO = "05"
                 OPEN OUTPUT ARQ-WRK 
                 CLOSE ARQ-WRK
				 OPEN I-O ARQ-WRK
               ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQ-WRK" 
				        TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
    			 PERFORM CLOSE-ARQUIVOS
                 EXIT PROGRAM
				 .

           OPEN OUTPUT ARQ-CSV
		   .

       CLOSE-ARQUIVOS.
           CLOSE ARQ-CLIENTE
                 ARQ-VENDEDOR
				 ARQ-WRK
                 ARQ-CSV
           .				 

	   READ-WRK.
	       MOVE ZEROS TO WRK-CODIGO-CLIENTE
           START ARQ-WRK 
	           KEY IS GREATER THAN WRK-CODIGO-CLIENTE
				INVALID KEY 
         	       MOVE "Y" TO WS-EOF-SW
	       END-START
		   READ ARQ-WRK NEXT AT END
                   MOVE "Y" TO WS-EOF-SW
		   END-READ 
		   
		   PERFORM UNTIL WS-EOF-SW = "Y"

  			    DELETE ARQ-WRK RECORD

 	            READ ARQ-WRK NEXT AT END
				  MOVE "Y" TO WS-EOF-SW
			    END-READ 

		   END-PERFORM
           .
		   
	   CALCULO-DISTANCIA-ENTRE-2PONTOS.
	       MOVE 3,14159265358979323846 TO PI
		   MOVE 6378,137  TO RAIOS

      * No Brasil são todas as latitudes e longitudes negativas 
		   IF P1LA > 0
		      MULTIPLY -1 by P1LA giving P1LA
			  .
		   IF P1LO > 0
		      MULTIPLY -1 by P1LO giving P1LO
			  .
		   IF P2LA > 0	  
		      MULTIPLY -1 by P2LA giving P2LA
			  .
		   IF P2LO > 0
			  MULTIPLY -1 by P2LO giving P2LO
			  .
			  
           COMPUTE P1LA  = P1LA * PI / 180
           COMPUTE P1LO  = P1LO * PI / 180
           COMPUTE P2LA  = P2LA * PI / 180
           COMPUTE P2LO  = P2LO * PI / 180
						
           COMPUTE DLAT  = P2LA + (P1LA * -1)
		   COMPUTE DLONG = P2LO + (P1LO * -1)

      * SIN_ADLAT
           COMPUTE SIN_ADLAT = FUNCTION SIN( DLAT / 2 )
			         
      * COS_P1LA
           COMPUTE COS_P1LA = FUNCTION COS( P1LA )

      * COS_P2LA
           COMPUTE COS_P2LA = FUNCTION COS( P2LA )

      * SIN_ADLONG 
		   COMPUTE SIN_ADLONG = FUNCTION SIN( DLONG / 2 ) 
           COMPUTE A1 = SIN_ADLAT * SIN_ADLAT + COS_P1LA  * 
			            COS_P2LA  *
			            SIN_ADLONG * SIN_ADLONG
			
      * SQRT_A1
		   COMPUTE SQRT_A1 = FUNCTION SQRT( A1 )

      * SQRT_A1_X 
           COMPUTE A1 = 1 + (A1 * -1)
		   COMPUTE SQRT_A1_X = FUNCTION SQRT( A1 )
  
      * ATAN2_SQRT_A1
		   COMPUTE ATAN2_SQRT_A1 = 2 *
			        FUNCTION ATAN( SQRT_A1 / SQRT_A1_X )

           COMPUTE WS-METROS ROUNDED = RAIOS * ATAN2_SQRT_A1 * 1000 		
           .

      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           PERFORM CLOSE-ARQUIVOS.
	   ROT-EXIT.
           EXIT PROGRAM.

       ROT-FIMS.
           STOP RUN.

      **********************
      * ROTINA DE MENSAGEM *
      **********************
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
