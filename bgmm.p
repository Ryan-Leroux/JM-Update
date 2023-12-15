PROPATH = "d:\bullseye\1source,\\FS02\bullseye\images\agentphotos\Temporary\MM Logs,\\sancluster\progress\openedge11.1\code\images,\\sancluster\progress\openedge11.1\code\images\virtcat," + PROPATH.

DEF VAR Purge      AS LOG  NO-UNDO.
DEF VAR bglog      AS CHAR NO-UNDO.

DEF TEMP-TABLE bgdet
    FIELD RUN_time AS INT
    FIELD blankslate AS LOG.

DEF BUFFER xx_file   FOR zz_file.
DEF BUFFER yy_file   FOR zz_file.
DEF BUFFER tt_file   FOR zz_file.
DEF BUFFER buf_bgdet FOR bgdet.

DEFINE STREAM bgout.

bglog = "\\FS02\bullseye\images\agentphotos\Temporary\MM Logs\bgprocess.log".

ASSIGN purge      = YES.

SESSION:SUPPRESS-WARNINGS = TRUE.

RUN doLog ("bg process bgmm.p started" + " " + string(TODAY) + " " + string(TIME,"HH:MM:SS")).

RUN getTimes.

FOR EACH bgdet:
    RUN doLog ("runtime" + " " + string(bgdet.RUN_time,"HH:MM:SS")).
END.

DO WHILE TIME < 83000:
    IF CAN-FIND(FIRST zz_file WHERE zz_file.zz_key1 = "mm-RunTime" AND zz_file.zz_key2 = "Refresh") THEN RUN getTimes.

    IF CAN-FIND(FIRST bgdet WHERE bgdet.RUN_time < TIME) THEN DO:

        DO FOR buf_bgdet TRANSACTION:
            FOR EACH bgdet WHERE run_time < TIME:
                ASSIGN Purge = bgdet.blankslate.
                DELETE bgdet.
            END.
        END.

        RUN doLog("starting bgdart.p " + STRING(TIME,"HH:MM:SS")). 

        DO FOR yy_file TRANSACTION:
            FIND xx_file WHERE xx_file.zz_key1 = "MediaManager" AND xx_file.zz_key2 = "bgdart" NO-ERROR.
            IF AVAIL xx_file THEN DELETE xx_file.
            CREATE xx_file.
            ASSIGN Xx_file.zz_key1    = "MediaManager"
                   xx_file.zz_key2    = "bgdart"
                   xx_file.zz_char[1] = STRING(TIME)
                   xx_file.zz_char[2] = STRING(TIME,"HH:MM:SS").
    
            RELEASE xx_file.
        END.

        RUN bgdart.p.

        DO FOR yy_file TRANSACTION:
            FIND xx_file WHERE xx_file.zz_key1 = "MediaManager" AND xx_file.zz_key2 = "bgdart" NO-ERROR.
            IF AVAIL xx_file THEN DELETE xx_file.
        END.

        RUN doLog("Finished bgdart.p...starting mm.p " + STRING(TIME,"HH:MM:SS")).

        RUN mm.p (purge).
        RUN doLog("mm.p Finished " + STRING(TIME,"HH:MM:SS")).

    END.
    PAUSE 30 NO-MESSAGE.
END.

RUN doLog ("bg process bgmm.p quiting" + " " + string(TODAY) + " " + string(TIME,"HH:MM:SS")).

QUIT.


PROCEDURE getTimes:
    DEF BUFFER bdet FOR bgdet.

    RUN doLog("bgmm.p getting run times... " + STRING(TIME,"HH:MM:SS")).
    EMPTY TEMP-TABLE bgdet.

    DO FOR bdet TRANSACTION:
        FOR EACH zz_file NO-LOCK WHERE zz_file.zz_key1 = "MM-RunTime":
            IF zz_file.zz_key2 = "Refresh" THEN DO:
                FIND tt_file EXCLUSIVE-LOCK WHERE tt_file.zz_key1 = "MM-RunTime" AND tt_file.zz_key2 = "Refresh" NO-WAIT NO-ERROR.
                IF NOT AVAIL(tt_file) THEN DO:
                    IF LOCKED tt_File THEN DO:
                        RUN mgemail.p ("Bullseye Database","terryp@lowen.com","","","JM Refresh Lock","Active Lock as of " + STRING(TIME, "HH:MM:SS"),"",FALSE).
                    END.
                END.
                ELSE DO:
                    DELETE tt_file. /*only use to trigger rebuild of bgdet's after start of process*/
                END.
            END.
            ELSE DO:
                IF int(zz_file.zz_key2) < TIME THEN NEXT.
        
                CREATE bgdet.
                ASSIGN bgdet.RUN_time   = INT(zz_file.zz_key2)
                       bgdet.blankslate = IF WEEKDAY(TODAY) = 1 THEN YES ELSE zz_file.zz_log[1]. /*complete rebuild on sundays...clears out folders and speeds up UI*/
            END.
        END.
        IF AVAIL bgdet THEN RELEASE bgdet.
    END.
END PROCEDURE. 

PROCEDURE doLog:
    DEFINE INPUT PARAMETER cmessage AS CHAR NO-UNDO.

    output stream bgout TO value(bglog) APPEND.
    put stream bgout UNFORMATTED cmessage SKIP.
    output stream bgout CLOSE.

END PROCEDURE.


