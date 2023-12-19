ROUTINE-LEVEL ON ERROR UNDO, THROW.
DEFINE INPUT  PARAMETER purge      AS LOGICAL NO-UNDO.

DEFINE VARIABLE lCanRun       AS LOG    NO-UNDO.
DEFINE VARIABLE lProdReady   AS LOG    NO-UNDO.
DEFINE VARIABLE mm-pp-handle AS HANDLE NO-UNDO.
DEFINE VARIABLE hTable       AS HANDLE NO-UNDO.
DEFINE VARIABLE dBase        AS CHAR   NO-UNDO.
DEFINE VARIABLE workDate     AS DATE   NO-UNDO.
DEFINE VARIABLE mtlLoc       AS CHAR   NO-UNDO.

DEFINE VARIABLE CorexCnt     AS INT    NO-UNDO.
DEFINE VARIABLE CorexSheets  AS INT    NO-UNDO.
DEFINE VARIABLE PrimeCnt     AS INT    NO-UNDO.
DEFINE VARIABLE PrimeSheets  AS INT    NO-UNDO.
DEFINE VARIABLE CorexOver48  AS CHAR   NO-UNDO.

DEFINE VARIABLE nWorkDays AS INTEGER NO-UNDO.


{mm.i "NEW"}
{mgseclist.i "CorexOver48" CorexOver48}

SESSION:SUPPRESS-WARNINGS = YES.
RUN mm-pp.p PERSISTENT SET mm-pp-handle. 

RUN getDBase     IN mm-pp-handle (OUTPUT dBase).

/******************* Main **********************/
RUN CanIRun         IN mm-pp-handle ("Start", OUTPUT lCanRun).
IF NOT lCanRun THEN DO:
    RUN Logs        IN mm-pp-handle (1,"MM-CanIRun-NO").
END.
ELSE DO:
    
    RUN Logs        IN mm-pp-handle (1,"Start MM.p").
    RUN checkIn     ("Start"). 
    RUN Logs        IN mm-pp-handle (1,"Proc Resets: Start").
    RUN resets      IN mm-pp-handle (purge).
    RUN Logs        IN mm-pp-handle (1,"Proc Resets: Finished").

    
    FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "mm.p-workdays" AND zz_file.zz_key2 = "" NO-ERROR.
    nWorkDays = IF AVAILABLE zz_file AND zz_file.zz_dec[1] > 0 THEN zz_file.zz_dec[1] ELSE 999.


    RUN workdays.p (TODAY,nWorkDays,OUTPUT workDate).
    
    RUN Logs        IN mm-pp-handle (1,"Proc BuildTT: Start").
    FOR EACH so_file NO-LOCK WHERE so_file.site = "7" AND so_file.ship_by <= workDate  BY so_file.ship_by:
        FOR EACH so_items NO-LOCK OF so_file WHERE so_items.ord_status = 5 :
            /*create report info*/
            RUN RptDet  IN mm-pp-handle (so_items.itemseq,so_items.so_no,so_items.ITEM_no).
            RUN Checks  IN mm-pp-handle (so_items.so_no,so_items.ITEM_no,OUTPUT lProdReady).
            IF lProdReady = TRUE THEN RUN BuildTT IN mm-pp-handle (so_items.so_no,so_items.ITEM_no).
        END.
    END.
    RUN Logs         IN mm-pp-handle (1,"Proc BuildTT: Finished").
    
    
    
    RUN Logs         IN mm-pp-handle (1,"Proc CheckImages: Start").
    RUN CheckImages  IN mm-pp-handle (0).
    RUN Logs         IN mm-pp-handle (1,"Proc CheckImages: Finished").

    /*
    RUN removeFaults IN mm-pp-handle.
    RUN Logs         IN mm-pp-handle (1,"MM-RemoveFaults Finished").
    
    /*Corex Batching Code*/
    RUN DelCorexPC    IN mm-pp-handle ("").
    RUN Logs          IN mm-pp-handle (1,"Del PC Corex Files Finished").
    RUN GroupingCorex IN mm-pp-handle ("").
    RUN Logs          IN mm-pp-handle (1,"MM-Grouping Corex Finished").
    
    RUN Grouping     IN mm-pp-handle ("").
    RUN Logs         IN mm-pp-handle (1,"MM-Grouping Finished").

    RUN reGroup      IN mm-pp-handle ("").
    RUN Logs         IN mm-pp-handle (1,"MM-Regroup Finished").

    /*IF purge = NO THEN RUN TrimBeds IN mm-pp-handle ("").
    RUN logs         IN mm-pp-handle (1,"MM-Trimbeds Finished").*/

    IF EnableDynNest THEN DO:
        RUN DynamicNest  IN mm-pp-handle("").
        RUN Logs         IN mm-pp-handle (1,"MM-DynamicNest Finished").
    END.

    RUN releaseAll   IN mm-pp-handle.
    
    RUN TrimDoups    IN mm-pp-handle.
    RUN Logs         IN mm-pp-handle (1,"MM-TrimDoups Finished"). 
        
    RUN printorder   IN mm-pp-handle ("").
    RUN Logs         IN mm-pp-handle (1,"MM-PrintOrder Finished").    

    IF dBase = "Live" THEN DO:
        RUN checkTemplates  IN mm-pp-handle.
        RUN Logs            IN mm-pp-handle (1,"MM-CheckTemplates Finished").
    END.
    
    RUN genXml       IN mm-pp-handle ("").
    RUN Logs         IN mm-pp-handle (1,"MM-GenXML Finished").   
 
    RUN email        IN mm-pp-handle.
    RUN Logs         IN mm-pp-handle (1,"MM-Email Finished").

    RUN gangCheck    IN mm-pp-handle.
    RUN Logs         IN mm-pp-handle (1,"mm-gangcheck Finished").

    RUN CanIRun      IN mm-pp-handle ("Close",OUTPUT lCanRun).
    RUN Logs         IN mm-pp-handle (1,"MM-CanIRun").

      /*clean up*/
    RUN releaseAll   IN mm-pp-handle.
    */
    
    RUN CanIRun      IN mm-pp-handle ("Close",OUTPUT lCanRun).
    RUN Logs         IN mm-pp-handle (1,"MM-CanIRun").
    
    RUN Logs         IN mm-pp-handle (1,"Log ttArt").
    FOR EACH ttArt:
        hTable = BUFFER ttArt:HANDLE.
        RUN LogsTT   IN mm-pp-handle (2,hTable).
    END.
    
    RUN Logs         IN mm-pp-handle (1,"Log Rpt_det").   
    RUN exportRptDet      IN mm-pp-handle.

     /****************** End Main *******************/
 END.
 
 RUN Logs           IN mm-pp-handle (1,"End JM").
                                                                          
IF VALID-HANDLE(mm-pp-handle) THEN DELETE PROCEDURE mm-pp-handle.         
                                                                          
CATCH anyerr AS Progress.Lang.Error:                                      
    RUN Logs         IN mm-pp-handle (1,anyerr:GetMessage(1)).   
END CATCH.                                                                


