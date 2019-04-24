      **************************************
      * DEFINICAO DAS FDS UTILIZADAS       *
      **************************************
       FD ARQ-CLIENTE
           RECORD CONTAINS 84 CHARACTERS.
       01 REGCLI.
           05 CLI_CODIGO_CLIENTE      PIC 9(007).
           05 CLI_CNPJ                PIC 9(014).
		   05 CLI_RAZAO_SOCIAL        PIC X(040).
           05 CLI_LATITUDE            PIC S9(003)V9(008).
           05 CLI_LONGITUDE           PIC S9(003)V9(008).
      *
       FD ARQ-VENDEDOR
           RECORD CONTAINS 76 CHARACTERS.
       01 REGVEN.
           05 VEN_CODIGO_VENDEDOR     PIC 9(003).
           05 VEN_CPF                 PIC 9(011).
		   05 VEN_NOME_VENDEDOR       PIC X(040).
           05 VEN_LATITUDE            PIC S9(003)V9(008).
           05 VEN_LONGITUDE           PIC S9(003)V9(008).
      *
       FD CLIENTE-TXT
           RECORD CONTAINS 84 CHARACTERS.
       01 REG-CLI-TXT.
           05 R_CLI_CODIGO_CLIENTE    PIC 9(007).
           05 R_CLI_CGC               PIC 9(014).
		   05 R_CLI_RAZAO_SOCIAL      PIC X(040).
           05 R_CLI_LATITUDE          PIC S9(003)V9(008).
           05 R_CLI_LONGITUDE         PIC S9(003)V9(008).
      *
       FD VENDEDOR-TXT
           RECORD CONTAINS 76 CHARACTERS.
       01 REG-VEN-TXT.
           05 R_VEN_CODIGO_VENDEDOR   PIC 9(003).
           05 R_VEN_CPF               PIC 9(011).
		   05 R_VEN_NOME_VENDEDOR     PIC X(040).
           05 R_VEN_LATITUDE          PIC S9(003)V9(008).
           05 R_VEN_LONGITUDE         PIC S9(003)V9(008).
      *
       FD ARQIMP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS WRL-ARQ-IMPRESSAO.
       01 REGIMP PIC X(132).
      *
       FD ARQ-ERR
           RECORD CONTAINS 100 CHARACTERS.
       01 REGERR.
		   05 ERRO-MSG            PIC X(040).
	       05 ERRO-REG            PIC X(060).
      *
       FD ARQ-CSV
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS WRL-ARQ-CSV.
       01 REGCSV PIC X(1000).
      *
