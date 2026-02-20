      *> ============================================================
      *> Yuzu — COBOL Metrics Processor
      *> Compilar: cobc -x -o yuzu-metrics metrics.cob
      *> Uso:      ./yuzu-metrics "JSON_STRING"
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YUZU-METRICS.
       AUTHOR. YUZU-PROJECT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> Input raw
           01 WS-INPUT            PIC X(4096).
           01 WS-INPUT-LEN        PIC 9(4).

      *> Campos parseados del JSON
           01 WS-TOTAL-REQ        PIC 9(10)   VALUE 0.
           01 WS-OK-REQ           PIC 9(10)   VALUE 0.
           01 WS-ERR-REQ          PIC 9(10)   VALUE 0.
           01 WS-ACTIVE           PIC 9(6)    VALUE 0.
           01 WS-AVG-LAT          PIC 9(8)V99 VALUE 0.
           01 WS-LAST-LAT         PIC 9(8)V99 VALUE 0.
           01 WS-MIN-LAT          PIC 9(8)V99 VALUE 0.
           01 WS-MAX-LAT          PIC 9(8)V99 VALUE 0.
           01 WS-UPTIME           PIC 9(10)   VALUE 0.
           01 WS-LAST-PATH        PIC X(256)  VALUE SPACES.
           01 WS-LAST-METHOD      PIC X(8)    VALUE SPACES.
           01 WS-VERSION          PIC X(16)   VALUE SPACES.
           01 WS-NAME             PIC X(32)   VALUE SPACES.

      *> Métricas calculadas
           01 WS-ERROR-RATE       PIC 9(3)V99 VALUE 0.
           01 WS-SUCCESS-RATE     PIC 9(3)V99 VALUE 0.
           01 WS-REQ-PER-MIN      PIC 9(8)V99 VALUE 0.
           01 WS-UPTIME-MIN       PIC 9(8)    VALUE 0.
           01 WS-UPTIME-HOURS     PIC 9(6)    VALUE 0.
           01 WS-UPTIME-DAYS      PIC 9(4)    VALUE 0.

      *> Alertas
           01 WS-ALERT-HIGH-LAT   PIC X       VALUE 'N'.
           01 WS-ALERT-HIGH-ERR   PIC X       VALUE 'N'.
           01 WS-ALERT-LOW-REQ    PIC X       VALUE 'N'.

      *> Estado general
           01 WS-STATUS           PIC X(16)   VALUE SPACES.
           01 WS-STATUS-CODE      PIC 9        VALUE 0.
               88 STATUS-OK       VALUE 1.
               88 STATUS-WARN     VALUE 2.
               88 STATUS-CRIT     VALUE 3.

      *> Auxiliares
           01 WS-TEMP             PIC 9(10)V99 VALUE 0.
           01 WS-TOKEN            PIC X(256)   VALUE SPACES.
           01 WS-IDX              PIC 9(4)     VALUE 0.
           01 WS-DUMMY            PIC X(64)    VALUE SPACES.
           01 WS-OUTPUT           PIC X(4096)  VALUE SPACES.
           01 WS-FORMATTED-LAT    PIC ZZZ9.99  VALUE 0.

      *> Delimitadores JSON — evita escapes invalidos en COBOL
           01 WS-DL-TOTAL         PIC X(20)
               VALUE 'total_requests":'.
           01 WS-DL-OK            PIC X(16)
               VALUE 'ok_requests":'.
           01 WS-DL-ERR           PIC X(20)
               VALUE 'error_requests":'.
           01 WS-DL-AVG           PIC X(20)
               VALUE 'avg_latency_ms":'.
           01 WS-DL-LAST          PIC X(22)
               VALUE 'last_latency_ms":'.
           01 WS-DL-UPTIME        PIC X(20)
               VALUE 'uptime_seconds":'.

       PROCEDURE DIVISION.
       MAIN.
           ACCEPT WS-INPUT FROM COMMAND-LINE
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT))
               TO WS-INPUT-LEN

           IF WS-INPUT-LEN = 0
               DISPLAY "ERROR:NO_INPUT"
               STOP RUN
           END-IF

           PERFORM PARSE-JSON
           PERFORM CALCULATE-METRICS
           PERFORM EVALUATE-ALERTS
           PERFORM DETERMINE-STATUS
           PERFORM OUTPUT-METRICS
           STOP RUN.

      *> ============================================================
      *> Parsear JSON simple campo por campo
      *> ============================================================
       PARSE-JSON.
           MOVE SPACES TO WS-DUMMY WS-TOKEN
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-TOTAL
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY ","
                   INTO WS-TOKEN WS-DUMMY
               END-UNSTRING
               MOVE FUNCTION NUMVAL(
                   FUNCTION TRIM(WS-TOKEN(1:10))) TO WS-TOTAL-REQ
           END-IF

           MOVE SPACES TO WS-DUMMY WS-TOKEN
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-OK
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY ","
                   INTO WS-TOKEN WS-DUMMY
               END-UNSTRING
               MOVE FUNCTION NUMVAL(
                   FUNCTION TRIM(WS-TOKEN)) TO WS-OK-REQ
           END-IF

           MOVE SPACES TO WS-DUMMY WS-TOKEN
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-ERR
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY ","
                   INTO WS-TOKEN WS-DUMMY
               END-UNSTRING
               MOVE FUNCTION NUMVAL(
                   FUNCTION TRIM(WS-TOKEN)) TO WS-ERR-REQ
           END-IF

           MOVE SPACES TO WS-DUMMY WS-TOKEN
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-AVG
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY ","
                   INTO WS-TOKEN WS-DUMMY
               END-UNSTRING
               MOVE FUNCTION NUMVAL(
                   FUNCTION TRIM(WS-TOKEN)) TO WS-AVG-LAT
           END-IF

           MOVE SPACES TO WS-DUMMY WS-TOKEN
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-LAST
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY ","
                   INTO WS-TOKEN WS-DUMMY
               END-UNSTRING
               MOVE FUNCTION NUMVAL(
                   FUNCTION TRIM(WS-TOKEN)) TO WS-LAST-LAT
           END-IF

           MOVE SPACES TO WS-DUMMY WS-TOKEN
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-UPTIME
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY ","
                   INTO WS-TOKEN WS-DUMMY
               END-UNSTRING
               MOVE FUNCTION NUMVAL(
                   FUNCTION TRIM(WS-TOKEN)) TO WS-UPTIME
           END-IF.

      *> ============================================================
      *> Calcular métricas derivadas
      *> ============================================================
       CALCULATE-METRICS.
           IF WS-TOTAL-REQ > 0
               COMPUTE WS-ERROR-RATE   = (WS-ERR-REQ / WS-TOTAL-REQ) * 100
               COMPUTE WS-SUCCESS-RATE = (WS-OK-REQ  / WS-TOTAL-REQ) * 100
           END-IF

           IF WS-UPTIME > 0
               COMPUTE WS-REQ-PER-MIN = (WS-TOTAL-REQ / WS-UPTIME) * 60
               COMPUTE WS-UPTIME-MIN  = WS-UPTIME / 60
               COMPUTE WS-UPTIME-HOURS= WS-UPTIME / 3600
               COMPUTE WS-UPTIME-DAYS = WS-UPTIME / 86400
           END-IF.

      *> ============================================================
      *> Evaluar alertas
      *> ============================================================
       EVALUATE-ALERTS.
           IF WS-AVG-LAT > 100
               MOVE 'Y' TO WS-ALERT-HIGH-LAT
           END-IF

           IF WS-ERROR-RATE > 5
               MOVE 'Y' TO WS-ALERT-HIGH-ERR
           END-IF

           IF WS-TOTAL-REQ = 0 AND WS-UPTIME > 60
               MOVE 'Y' TO WS-ALERT-LOW-REQ
           END-IF.

      *> ============================================================
      *> Determinar estado general
      *> ============================================================
       DETERMINE-STATUS.
           MOVE 1 TO WS-STATUS-CODE
           MOVE "OK" TO WS-STATUS

           IF WS-ALERT-HIGH-LAT = 'Y' OR WS-ALERT-LOW-REQ = 'Y'
               MOVE 2 TO WS-STATUS-CODE
               MOVE "WARNING" TO WS-STATUS
           END-IF

           IF WS-ALERT-HIGH-ERR = 'Y'
               MOVE 3 TO WS-STATUS-CODE
               MOVE "CRITICAL" TO WS-STATUS
           END-IF.

      *> ============================================================
      *> Output formateado para C++ y Assembly
      *> ============================================================
       OUTPUT-METRICS.
           DISPLAY "STATUS:"       FUNCTION TRIM(WS-STATUS)
           DISPLAY "STATUS_CODE:"  WS-STATUS-CODE
           DISPLAY "TOTAL_REQ:"    WS-TOTAL-REQ
           DISPLAY "OK_REQ:"       WS-OK-REQ
           DISPLAY "ERR_REQ:"      WS-ERR-REQ
           DISPLAY "SUCCESS_RATE:" WS-SUCCESS-RATE
           DISPLAY "ERROR_RATE:"   WS-ERROR-RATE
           DISPLAY "AVG_LAT:"      WS-AVG-LAT
           DISPLAY "LAST_LAT:"     WS-LAST-LAT
           DISPLAY "REQ_PER_MIN:"  WS-REQ-PER-MIN
           DISPLAY "UPTIME_SEC:"   WS-UPTIME
           DISPLAY "UPTIME_MIN:"   WS-UPTIME-MIN
           DISPLAY "UPTIME_HOURS:" WS-UPTIME-HOURS
           DISPLAY "UPTIME_DAYS:"  WS-UPTIME-DAYS
           DISPLAY "ALERT_HIGH_LAT:" WS-ALERT-HIGH-LAT
           DISPLAY "ALERT_HIGH_ERR:" WS-ALERT-HIGH-ERR
           DISPLAY "ALERT_LOW_REQ:"  WS-ALERT-LOW-REQ.
