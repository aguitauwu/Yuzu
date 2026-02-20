      *> Yuzu - COBOL Metrics Processor
      *> Compilar: cobc -x -o yuzu-metrics metrics.cob
      *> Uso:      ./yuzu-metrics "JSON_STRING"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YUZU-METRICS.
       AUTHOR. YUZU-PROJECT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           01 WS-INPUT            PIC X(4096).
           01 WS-INPUT-LEN        PIC 9(4).

           01 WS-TOTAL-REQ        PIC 9(10)    VALUE 0.
           01 WS-OK-REQ           PIC 9(10)    VALUE 0.
           01 WS-ERR-REQ          PIC 9(10)    VALUE 0.
           01 WS-ACTIVE           PIC 9(6)     VALUE 0.
           01 WS-AVG-LAT          PIC 9(8)V99  VALUE 0.
           01 WS-LAST-LAT         PIC 9(8)V99  VALUE 0.
           01 WS-MIN-LAT          PIC 9(8)V99  VALUE 0.
           01 WS-MAX-LAT          PIC 9(8)V99  VALUE 0.
           01 WS-UPTIME           PIC 9(10)    VALUE 0.
           01 WS-LAST-PATH        PIC X(256)   VALUE SPACES.
           01 WS-LAST-METHOD      PIC X(8)     VALUE SPACES.
           01 WS-VERSION          PIC X(16)    VALUE SPACES.
           01 WS-NAME             PIC X(32)    VALUE SPACES.

           01 WS-ERROR-RATE       PIC 9(3)V99  VALUE 0.
           01 WS-SUCCESS-RATE     PIC 9(3)V99  VALUE 0.
           01 WS-REQ-PER-MIN      PIC 9(8)V99  VALUE 0.
           01 WS-UPTIME-MIN       PIC 9(8)     VALUE 0.
           01 WS-UPTIME-HOURS     PIC 9(6)     VALUE 0.
           01 WS-UPTIME-DAYS      PIC 9(4)     VALUE 0.

           01 WS-ALERT-HIGH-LAT   PIC X        VALUE 'N'.
           01 WS-ALERT-HIGH-ERR   PIC X        VALUE 'N'.
           01 WS-ALERT-LOW-REQ    PIC X        VALUE 'N'.

           01 WS-STATUS           PIC X(16)    VALUE SPACES.
           01 WS-STATUS-CODE      PIC 9        VALUE 0.
               88 STATUS-OK       VALUE 1.
               88 STATUS-WARN     VALUE 2.
               88 STATUS-CRIT     VALUE 3.

           01 WS-TOKEN            PIC X(256)   VALUE SPACES.
           01 WS-TOKEN2           PIC X(256)   VALUE SPACES.
           01 WS-DUMMY            PIC X(4096)  VALUE SPACES.

      *> Delimitadores - PIC X exacto al largo del VALUE
      *> t-o-t-a-l-_-r-e-q-u-e-s-t-s-"-:  = 16
           01 WS-DL-TOTAL         PIC X(16)
               VALUE 'total_requests":'.
      *> o-k-_-r-e-q-u-e-s-t-s-"-:         = 13
           01 WS-DL-OK            PIC X(13)
               VALUE 'ok_requests":'.
      *> e-r-r-o-r-_-r-e-q-u-e-s-t-s-"-:  = 16
           01 WS-DL-ERR           PIC X(16)
               VALUE 'error_requests":'.
      *> a-v-g-_-l-a-t-e-n-c-y-_-m-s-"-:  = 16
           01 WS-DL-AVG           PIC X(16)
               VALUE 'avg_latency_ms":'.
      *> l-a-s-t-_-l-a-t-e-n-c-y-_-m-s-"-:= 17
           01 WS-DL-LAST          PIC X(17)
               VALUE 'last_latency_ms":'.
      *> u-p-t-i-m-e-_-s-e-c-o-n-d-s-"-:  = 16
           01 WS-DL-UPTIME        PIC X(16)
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

       PARSE-JSON.
      *> TOTAL_REQUESTS
           MOVE SPACES TO WS-DUMMY WS-TOKEN WS-TOKEN2
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-TOTAL
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY "," OR "}"
                   INTO WS-TOKEN2 WS-DUMMY
               END-UNSTRING
               IF WS-TOKEN2 NOT = SPACES
                   MOVE FUNCTION NUMVAL(
                       FUNCTION TRIM(WS-TOKEN2)) TO WS-TOTAL-REQ
               END-IF
           END-IF

      *> OK_REQUESTS
           MOVE SPACES TO WS-DUMMY WS-TOKEN WS-TOKEN2
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-OK
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY "," OR "}"
                   INTO WS-TOKEN2 WS-DUMMY
               END-UNSTRING
               IF WS-TOKEN2 NOT = SPACES
                   MOVE FUNCTION NUMVAL(
                       FUNCTION TRIM(WS-TOKEN2)) TO WS-OK-REQ
               END-IF
           END-IF

      *> ERROR_REQUESTS
           MOVE SPACES TO WS-DUMMY WS-TOKEN WS-TOKEN2
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-ERR
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY "," OR "}"
                   INTO WS-TOKEN2 WS-DUMMY
               END-UNSTRING
               IF WS-TOKEN2 NOT = SPACES
                   MOVE FUNCTION NUMVAL(
                       FUNCTION TRIM(WS-TOKEN2)) TO WS-ERR-REQ
               END-IF
           END-IF

      *> AVG_LATENCY_MS
           MOVE SPACES TO WS-DUMMY WS-TOKEN WS-TOKEN2
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-AVG
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY "," OR "}"
                   INTO WS-TOKEN2 WS-DUMMY
               END-UNSTRING
               IF WS-TOKEN2 NOT = SPACES
                   MOVE FUNCTION NUMVAL(
                       FUNCTION TRIM(WS-TOKEN2)) TO WS-AVG-LAT
               END-IF
           END-IF

      *> LAST_LATENCY_MS
           MOVE SPACES TO WS-DUMMY WS-TOKEN WS-TOKEN2
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-LAST
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY "," OR "}"
                   INTO WS-TOKEN2 WS-DUMMY
               END-UNSTRING
               IF WS-TOKEN2 NOT = SPACES
                   MOVE FUNCTION NUMVAL(
                       FUNCTION TRIM(WS-TOKEN2)) TO WS-LAST-LAT
               END-IF
           END-IF

      *> UPTIME_SECONDS
           MOVE SPACES TO WS-DUMMY WS-TOKEN WS-TOKEN2
           UNSTRING WS-INPUT
               DELIMITED BY WS-DL-UPTIME
               INTO WS-DUMMY WS-TOKEN
           END-UNSTRING
           IF WS-TOKEN NOT = SPACES
               UNSTRING WS-TOKEN DELIMITED BY "," OR "}"
                   INTO WS-TOKEN2 WS-DUMMY
               END-UNSTRING
               IF WS-TOKEN2 NOT = SPACES
                   MOVE FUNCTION NUMVAL(
                       FUNCTION TRIM(WS-TOKEN2)) TO WS-UPTIME
               END-IF
           END-IF.

       CALCULATE-METRICS.
           IF WS-TOTAL-REQ > 0
               COMPUTE WS-ERROR-RATE   =
                   (WS-ERR-REQ / WS-TOTAL-REQ) * 100
               COMPUTE WS-SUCCESS-RATE =
                   (WS-OK-REQ  / WS-TOTAL-REQ) * 100
           END-IF

           IF WS-UPTIME > 0
               COMPUTE WS-REQ-PER-MIN  =
                   (WS-TOTAL-REQ / WS-UPTIME) * 60
               COMPUTE WS-UPTIME-MIN   = WS-UPTIME / 60
               COMPUTE WS-UPTIME-HOURS = WS-UPTIME / 3600
               COMPUTE WS-UPTIME-DAYS  = WS-UPTIME / 86400
           END-IF.

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

       OUTPUT-METRICS.
           DISPLAY "STATUS:"         FUNCTION TRIM(WS-STATUS)
           DISPLAY "STATUS_CODE:"    WS-STATUS-CODE
           DISPLAY "TOTAL_REQ:"      WS-TOTAL-REQ
           DISPLAY "OK_REQ:"         WS-OK-REQ
           DISPLAY "ERR_REQ:"        WS-ERR-REQ
           DISPLAY "SUCCESS_RATE:"   WS-SUCCESS-RATE
           DISPLAY "ERROR_RATE:"     WS-ERROR-RATE
           DISPLAY "AVG_LAT:"        WS-AVG-LAT
           DISPLAY "LAST_LAT:"       WS-LAST-LAT
           DISPLAY "REQ_PER_MIN:"    WS-REQ-PER-MIN
           DISPLAY "UPTIME_SEC:"     WS-UPTIME
           DISPLAY "UPTIME_MIN:"     WS-UPTIME-MIN
           DISPLAY "UPTIME_HOURS:"   WS-UPTIME-HOURS
           DISPLAY "UPTIME_DAYS:"    WS-UPTIME-DAYS
           DISPLAY "ALERT_HIGH_LAT:" WS-ALERT-HIGH-LAT
           DISPLAY "ALERT_HIGH_ERR:" WS-ALERT-HIGH-ERR
           DISPLAY "ALERT_LOW_REQ:"  WS-ALERT-LOW-REQ.
