ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT PARAMETER pPurge AS LOG    NO-UNDO.

DEFINE VARIABLE CanRun        AS LOG    NO-UNDO.
DEFINE VARIABLE a_ok          AS LOG    NO-UNDO.
DEFINE VARIABLE mm-pp-handle  AS HANDLE NO-UNDO.
DEFINE VARIABLE WorkDate      AS DATE   NO-UNDO.
DEFINE VARIABLE MtlLoc        AS CHAR   NO-UNDO.
DEFINE VARIABLE CorexCnt      AS INT    NO-UNDO.
DEFINE VARIABLE CorexSheets   AS INT    NO-UNDO.
DEFINE VARIABLE PrimeCnt      AS INT    NO-UNDO.
DEFINE VARIABLE PrimeSheets   AS INT    NO-UNDO.
DEFINE VARIABLE CorexOver48   AS CHAR   NO-UNDO.
DEFINE VARIABLE nWorkDays AS INTEGER NO-UNDO.


{mm.i "NEW"}
{mgseclist.i "CorexOver48" CorexOver48}

SESSION:SUPPRESS-WARNINGS = YES.
RUN mm-pp.p PERSISTENT SET mm-pp-handle. 

RUN LogIt IN mm-pp-handle (1,"Start MM.p","","","").

/******************* Main **********************/
RUN CanIRun         IN mm-pp-handle ("Start", OUTPUT CanRun).

IF CanRun = FALSE THEN RUN LogIt IN mm-pp-handle (1,"Procedure:CanIRun = NO").
ELSE DO:
    RUN LogIt       IN mm-pp-handle (1,"Procedure:CanIRun = YES").
    RUN CheckIn     ("Start"). 
    
    RUN LogIt       IN mm-pp-handle (1,"Procedure:Resets = Start").
    RUN Resets      IN mm-pp-handle (pPurge).
    RUN LogIt       IN mm-pp-handle (1,"Procedure:Resets = Finished").

    
    FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "mm.p-workdays" AND zz_file.zz_key2 = "" NO-ERROR.
    nWorkDays = IF AVAILABLE zz_file AND zz_file.zz_dec[1] > 0 THEN zz_file.zz_dec[1] ELSE 999.


    RUN workdays.p (TODAY,nWorkDays,OUTPUT workDate).

    FOR EACH so_file NO-LOCK WHERE so_file.site = "7" AND so_file.ship_by <= workDate  BY so_file.ship_by:
        FOR EACH so_items NO-LOCK OF so_file WHERE so_items.ord_status = 5 :
            /*create report info*/
            RUN RptDet  IN mm-pp-handle (so_items.itemseq,so_items.so_no,so_items.ITEM_no).
            RUN Checks  IN mm-pp-handle (so_items.so_no,so_items.ITEM_no,OUTPUT a_ok).
            RUN BuildTT IN mm-pp-handle (so_items.so_no,so_items.ITEM_no).
        END.
    END.
    RUN LogIt        IN mm-pp-handle (1,"Procedure:Buildtt = Finished").

    RUN CheckImages  IN mm-pp-handle ("").
    RUN LogIt        IN mm-pp-handle (1,"Procedure:CheckImages = Finished").

    RUN RemoveFaults IN mm-pp-handle.
    RUN LogIt        IN mm-pp-handle (1,"Procedure:RemoveFaults = Finished","","","").
    
    /*Corex Batching Code*/
    RUN DelCorexPC    IN mm-pp-handle ("").
    RUN LogIt         IN mm-pp-handle (1,"Procedure:DelCorexPC = Finished").
    RUN GroupingCorex IN mm-pp-handle ("").
    RUN LogIt         IN mm-pp-handle (1,"Procedure:GroupingCorex = Finished").
    
    RUN Grouping     IN mm-pp-handle ("").
    RUN LogIt        IN mm-pp-handle (1,"Procedure:Grouping = Finished").

    RUN Regroup      IN mm-pp-handle ("").
    RUN LogIt        IN mm-pp-handle (1,"Procedure:Regroup = Finished").

    RUN DynamicNest  IN mm-pp-handle("").
    RUN LogIt        IN mm-pp-handle (1,"Procedure:DynamicNest = Finished").

    RUN ReleaseAll   IN mm-pp-handle.
    
    RUN TrimDoups    IN mm-pp-handle.
    RUN LogIt        IN mm-pp-handle (1,"Procedure:TrimDoups = Finished"). 
        
    RUN PrintOrder   IN mm-pp-handle ("").
    RUN LogIt        IN mm-pp-handle (1,"Procedure:PrintOrder = Finished").    

    /* CheckTemplates was only running on LIVE, check why */
    RUN CheckTemplates IN mm-pp-handle.
    RUN LogIt          IN mm-pp-handle (1,"Procedure:CheckTemplates = Finished").
    
    RUN GenXml       IN mm-pp-handle ("").
    RUN LogIt        IN mm-pp-handle (1,"Procedure:GenXML = Finished").   
 
    RUN Email        IN mm-pp-handle.
    RUN LogIt        IN mm-pp-handle (1,"Procedure:Email = Finished").

    RUN GangCheck    IN mm-pp-handle.
    RUN LogIt        IN mm-pp-handle (1,"Procedure:GangCheck = Finished").

    RUN FixAutoArt   IN mm-pp-handle.

    RUN CanIRun      IN mm-pp-handle ("Close",OUTPUT CanRun).
    RUN LogIt        IN mm-pp-handle (1,"Procedure:CanIRun = " + STRING(CanRun)).

    /*clean up*/
    RUN ReleaseAll   IN mm-pp-handle.


    OUTPUT TO VALUE(SESSION:TEMP-DIR + "ttArt.d").
    FOR EACH ttArt: EXPORT ttArt. END.
    OUTPUT CLOSE.

    RUN ExportRptDet      IN mm-pp-handle.

     /****************** End Main *******************/
 END.
                                                                          
IF VALID-HANDLE(mm-pp-handle) THEN DELETE PROCEDURE mm-pp-handle.         
                                                                          
CATCH AnyErr AS Progress.Lang.Error:                                      
    RUN LogIt        IN mm-pp-handle (1,"Error:" + AnyErr:GetMessage(1)).   
END CATCH.                                                                


