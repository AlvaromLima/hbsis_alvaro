       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.
       AUTHOR. AlvaroLima.
       
	   DATA DIVISION.
       WORKING-STORAGE SECTION.
	   COPY "copybook\SISWSGER.CPY".
	   
       SCREEN SECTION.
	   COPY "copybook\SISSTGER.CPY".
	   
       PROCEDURE DIVISION.
       INICIO.
	       MOVE "00" TO WFIM.
		   PERFORM MENU-PRINCIPAL-000 UNTIL WFIM = "99". 
		   STOP RUN.
	   MENU-PRINCIPAL-000.
           MOVE "00.00" TO WOPCAO
		   DISPLAY MENU-PRINCIPAL
           ACCEPT MENU-PRINCIPAL
           EVALUATE WOPCAO
             WHEN "01.01"
                CALL "CADCLI" USING WOPCAO
             WHEN "01.02"
                CALL "CADVEN" USING WOPCAO
		     WHEN "02.01"
                CALL "RELCLI" USING WOPCAO
             WHEN "02.02"
                CALL "RELVEN" USING WOPCAO
             WHEN "03.01"
                CALL "DISTVEN" USING WOPCAO
             WHEN "00.00"
                MOVE "99" TO WFIM
           END-EVALUATE.
		   