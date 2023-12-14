{imageShare.i}
{mgemaildef.i}
DEFINE {1} SHARED VARIABLE homeFolder            AS CHARACTER  NO-UNDO.
    homeFolder = imageShare + "agentphotos\temporary\mtl1".
DEFINE {1} SHARED VARIABLE csFileLoc             AS CHARACTER  NO-UNDO.
    csFileLoc = imageShare + "AgentPhotos\temporary\CS6".
DEFINE {1} SHARED VARIABLE logFolder             AS CHARACTER  NO-UNDO INITIAL "\\qbtest\bullseye\scripts\logfiles".
DEFINE {1} SHARED VARIABLE jpgFolder             AS CHARACTER  NO-UNDO.
    jpgFolder = imageShare + "AgentPhotos\Temporary\JPEG".
DEFINE {1} SHARED VARIABLE ranFolder             AS CHARACTER  NO-UNDO.
    ranFolder = imageShare + "AgentPhotos\Temporary\CS6\CompletedBatches".
DEFINE {1} SHARED VARIABLE cutQueue              AS CHARACTER  NO-UNDO.
    cutQueue = "\\zundg3xl3200-1\c$\ProgramData\Zund Cut Center\JobQueue".
DEFINE {1} SHARED VARIABLE mmLogs                AS CHARACTER  NO-UNDO.       
    mmLogs = imageShare + "AgentPhotos\Temporary\MM Logs".
DEFINE {1} SHARED VARIABLE lastCheckIn           AS INTEGER    NO-UNDO.
    lastCheckin = TIME.
DEFINE {1} SHARED VARIABLE PcFolder              AS CHARACTER  NO-UNDO.
    PcFolder = "\\lowen\dfssancluster\SignArt\PrepCenter\HotFolders".
DEFINE {1} SHARED VARIABLE AutoArt               AS CHARACTER  NO-UNDO.
DEFINE {1} SHARED VARIABLE EnableDynNest         AS LOGICAL    NO-UNDO INITIAL YES. /*reverts back to prepcenter or turns on dynamic nest*/
DEFINE {1} SHARED VARIABLE EnableDynCorexRebatch AS LOGICAL    NO-UNDO INITIAL NO.  /*This overrides the corex list of ellagible parts for dynnamic nest to rebatch partials*/
DEFINE {1} SHARED VARIABLE EnablePartialRebatch  AS LOGICAL    NO-UNDO INITIAL NO.  /*Turns on dynamic nest of partial beds to reduce waste*/
DEFINE {1} SHARED VARIABLE EnableMultiBatching   AS LOGICAL    NO-UNDO INITIAL NO. /*batching within batching*/
DEFINE {1} SHARED VARIABLE CutFileFolder         AS CHARACTER  NO-UNDO.
    CutFileFolder = "\\fs02\bullseye\images\agentphotos\Temporary\mtl1\PrepCenterCutFiles".


DEFINE {1} SHARED TEMP-TABLE ttArt NO-UNDO
    FIELD ttTempSeq         AS INT
    FIELD ttPointSeq        AS INT
    FIELD ttSize            AS CHAR
    FIELD ttType            AS CHAR
    FIELD ttDirect          AS LOG
    FIELD ttPart            AS CHAR
    FIELD ttDue             AS DATE
    FIELD ttoffice          AS CHAR
    FIELD ttso              AS CHAR
    FIELD ttItemNo          AS INT
    FIELD ttDest            AS CHAR
    FIELD ttSides           AS INT
    FIELD ttItemseq         AS INT
    FIELD ttQty             AS INT
    FIELD ttFile            AS CHAR
    FIELD ttInvPart         AS CHAR
    FIELD tthotfolder       AS INT
    FIELD ttSwitch          AS LOG
    FIELD ttSteelTent       AS LOG
    FIELD ttExploded        AS LOG
    FIELD ttCustom          AS LOG
    FIELD ttRecid           AS RECID
    FIELD ttZundNest        AS LOG
    FIELD ttZundFile        AS CHAR
    FIELD ttSubType         AS CHAR
    FIELD ttHasBleed        AS LOG
    FIELD ttHorzFlute       AS LOG
    FIELD ttVertFlute       AS LOG
    FIELD ttDelayedReprint  AS LOGICAL
    FIELD ttReprintId       AS INTEGER 
    FIELD ttReasonCode      AS CHAR.

DEFINE {1} SHARED TEMP-TABLE print_det NO-UNDO
    FIELD pSo         AS CHAR
    FIELD pItem       AS INT.


PROCEDURE checkIn:
    DEFINE INPUT PARAMETER cType AS CHAR NO-UNDO.
    IF cType = "Start" THEN DO:
        c_to_addr = "progressgroup@lowen.com".
        ASSIGN c_subject     = "MM-CheckIn"
               c_msg         = "Media Manager started running as of " + STRING(TIME, "HH:MM:SS").
        RUN mgemail.p ("Bullseye Database",c_to_addr,"","",c_subject,c_msg,"",FALSE).
    END.
    ELSE IF cType = "Lock" THEN DO:
        c_to_addr = "progressgroup@lowen.com".
        ASSIGN c_subject     = "JM Lock"
               c_msg         = "Active Lock as of " + STRING(TIME, "HH:MM:SS").
        RUN mgemail.p ("Bullseye Database",c_to_addr,"","",c_subject,c_msg,"",FALSE).
    END.
    ELSE DO:
        IF TIME - lastCheckIn > 900 THEN DO:
            c_to_addr = "progressgroup@lowen.com".
            ASSIGN c_subject     = "MM-CheckIn"
                   c_msg         = "Media Manager is still running as of " + STRING(TIME, "HH:MM:SS").
            RUN mgemail.p ("Bullseye Database",c_to_addr,"","",c_subject,c_msg,"",FALSE).
            lastCheckin = TIME.
        END.
    END.
END PROCEDURE.

PROCEDURE GetCurrentProcessId EXTERNAL "KERNEL32.DLL":
    DEFINE RETURN PARAMETER intProcessHandle AS LONG.
END PROCEDURE.
