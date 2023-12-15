{imageShare.i}
{mgemaildef.i}

DEFINE {1} SHARED VARIABLE cHomeFolder           AS CHARACTER  NO-UNDO.   
DEFINE {1} SHARED VARIABLE cBatchImgLoc          AS CHARACTER  NO-UNDO.
DEFINE {1} SHARED VARIABLE cLogFolder            AS CHARACTER  NO-UNDO INITIAL "\\QBTEST\Bullseye\Scripts\Logfiles\".    
DEFINE {1} SHARED VARIABLE cRanFolder            AS CHARACTER  NO-UNDO.      
DEFINE {1} SHARED VARIABLE cLogLoc               AS CHARACTER  NO-UNDO.          
DEFINE {1} SHARED VARIABLE iCheckIn              AS INTEGER    NO-UNDO.

ASSIGN cHomeFolder  = imageShare + "agentphotos\temporary\mtl1"
       cBatchImgLoc = imageShare + "AgentPhotos\temporary\CS6"
       cRanFolder   = imageShare + "AgentPhotos\Temporary\CS6\CompletedBatches"
       cLogLoc      = imageShare + "AgentPhotos\Temporary\MM Logs"
       iCheckIn     = TIME.

DEFINE {1} SHARED TEMP-TABLE ttArt NO-UNDO
    FIELD ttTempSeq         AS INT          /* Template Seq */
    FIELD ttSize            AS CHAR         /* H x W */
    FIELD ttType            AS CHAR         /* Material Type */
    FIELD ttPart            AS CHAR         /* Part Number */
    FIELD ttDue             AS DATE         /* Ship By/Due Date */
    FIELD ttCustNo          AS CHAR         /* Customer Number */
    FIELD ttSo              AS CHAR         /* Sales Order # */
    FIELD ttItemNo          AS INT          /* Item Number */
    FIELD ttALS             AS INT          /* Art Link Seq # */
    FIELD ttSides           AS INT          /* Number of sides 1 or 2 */
    FIELD ttItemseq         AS INT          /* Itemseq # */
    FIELD ttQty             AS INT          /* Art Qty */
    FIELD ttFile            AS CHAR         /* Art File(s) */
    FIELD ttInvPart         AS CHAR         /* Inventory Part */
    FIELD ttHotfolder       AS INT          /* Hotfolder Seq */
    FIELD ttSwitch          AS LOG          /* Switch H & W */
    FIELD ttSteelTent       AS LOG          /* SteelTent or JackUnit */
    FIELD ttExploded        AS LOG          /* Did we break this art into multiples? */
    FIELD ttDynamNest       AS LOG          /* Do we run this through Dynamic Nest? */
    FIELD ttHorzFlute       AS LOG          /* Horizontal Flutes */
    FIELD ttVertFlute       AS LOG          /* Vertical Flutes */
    FIELD ttReprintId       AS INTEGER      /* ID to group reprints */
    FIELD ttReasonCode      AS CHAR.        /* Reprint Reason Code */

DEFINE {1} SHARED TEMP-TABLE print_det NO-UNDO
    FIELD pSo         AS CHAR
    FIELD pItem       AS INT.


PROCEDURE CheckIn:
/*------------------------------------------------------------------------------
 Purpose: Send out JM "Check Up" emails
 
 INPUTS: pType: Which type of check up email do we need to send?
 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pType AS CHAR NO-UNDO.
    
    CASE pType:
        WHEN "Start" THEN ASSIGN c_to_addr = "progressgroup@lowen.com"
                                 c_subject = "MM-CheckIn"
                                 c_msg     = "Job Manager Started Running @ " + STRING(TIME, "HH:MM:SS").    
        OTHERWISE DO:
            IF TIME - iCheckIn > 900 THEN ASSIGN c_to_addr = "progressgroup@lowen.com"
                                                 c_subject = "MM-CheckIn"
                                                 c_msg     = "Media Manager is still running as of " + STRING(TIME, "HH:MM:SS")
                                                 iCheckIn  = TIME.
        END.
    END CASE.
    
    RUN mgemail.p ("Bullseye Database",c_to_addr,"","",c_subject,c_msg,"",FALSE).

END PROCEDURE.


PROCEDURE GetCurrentProcessId EXTERNAL "KERNEL32.DLL":
    DEFINE RETURN PARAMETER iProcessHandle AS LONG.
END PROCEDURE.
