ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT PARAMETER pPurge AS LOG    NO-UNDO.

DEFINE VARIABLE hMM-PP        AS HANDLE NO-UNDO.
DEFINE VARIABLE CanRun        AS LOG    NO-UNDO.
DEFINE VARIABLE IsProdReady   AS LOG    NO-UNDO.
DEFINE VARIABLE WorkDate      AS DATE   NO-UNDO.

{mm.i "NEW"}

SESSION:SUPPRESS-WARNINGS = YES.
RUN "F:\Dev\JM Update\JM-Update\mm-pp.p" PERSISTENT SET hMM-PP. 

RUN LogIt IN hMM-PP (1,"Start MM.p").

/******************* Main **********************/
RUN CanIRun         IN hMM-PP ("Start", OUTPUT CanRun).

IF CanRun = FALSE THEN RUN LogIt IN hMM-PP (1,"Procedure:CanIRun = NO").
ELSE DO:
    RUN LogIt       IN hMM-PP (1,"Procedure:CanIRun = YES").
    RUN CheckIn     ("Start"). 
    
    RUN LogIt       IN hMM-PP (1,"Procedure:Resets = Start").
    RUN Resets      IN hMM-PP (pPurge).
    RUN LogIt       IN hMM-PP (1,"Procedure:Resets = Finished").

    RUN workdays.p (TODAY,365,OUTPUT WorkDate).

    
    FOR EACH so_file NO-LOCK WHERE so_file.site = "7" 
                               AND so_file.ship_by <= WorkDate BY so_file.ship_by:
        FOR EACH so_items NO-LOCK OF so_file WHERE so_items.ord_status = 5:
            RUN CreateRptDet  IN hMM-PP (so_items.itemseq,so_items.so_no,so_items.ITEM_no).
            RUN Checks        IN hMM-PP (so_items.so_no,so_items.ITEM_no,OUTPUT IsProdReady).
            IF IsProdReady = YES THEN RUN BuildTT IN hMM-PP (so_items.so_no,so_items.ITEM_no).
        END.
    END.
    RUN LogIt        IN hMM-PP (1,"Procedure:Buildtt = Finished").

    /*
    RUN CheckImages  IN hMM-PP ("").
    RUN LogIt        IN hMM-PP (1,"Procedure:CheckImages = Finished").

    RUN RemoveFaults IN hMM-PP.
    RUN LogIt        IN hMM-PP (1,"Procedure:RemoveFaults = Finished").
    
    /*Corex Batching Code*/
    RUN DelCorexPC    IN hMM-PP ("").
    RUN LogIt         IN hMM-PP (1,"Procedure:DelCorexPC = Finished").
    RUN GroupingCorex IN hMM-PP ("").
    RUN LogIt         IN hMM-PP (1,"Procedure:GroupingCorex = Finished").
    
    RUN Grouping     IN hMM-PP ("").
    RUN LogIt        IN hMM-PP (1,"Procedure:Grouping = Finished").

    RUN Regroup      IN hMM-PP ("").
    RUN LogIt        IN hMM-PP (1,"Procedure:Regroup = Finished").

    RUN DynamicNest  IN hMM-PP("").
    RUN LogIt        IN hMM-PP (1,"Procedure:DynamicNest = Finished").

    RUN ReleaseAll   IN hMM-PP.
    
    RUN TrimDoups    IN hMM-PP.
    RUN LogIt        IN hMM-PP (1,"Procedure:TrimDoups = Finished"). 
        
    RUN PrintOrder   IN hMM-PP ("").
    RUN LogIt        IN hMM-PP (1,"Procedure:PrintOrder = Finished").    

    /* CheckTemplates was only running on LIVE, check why */
    RUN CheckTemplates IN hMM-PP.
    RUN LogIt          IN hMM-PP (1,"Procedure:CheckTemplates = Finished").
    
    RUN GenXml       IN hMM-PP ("").
    RUN LogIt        IN hMM-PP (1,"Procedure:GenXML = Finished").   
 
    RUN Email        IN hMM-PP.
    RUN LogIt        IN hMM-PP (1,"Procedure:Email = Finished").

    RUN GangCheck    IN hMM-PP.
    RUN LogIt        IN hMM-PP (1,"Procedure:GangCheck = Finished").

    RUN FixAutoArt   IN hMM-PP.

    RUN CanIRun      IN hMM-PP ("Close",OUTPUT CanRun).
    RUN LogIt        IN hMM-PP (1,"Procedure:CanIRun = " + STRING(CanRun)).

    /*clean up*/
    RUN ReleaseAll   IN hMM-PP.
    */
    
    RUN CanIRun IN hMM-PP ("Close",OUTPUT CanRun).
    RUN LogIt   IN hMM-PP (1,"CanIRun: " + STRING(CanRun)).
    
    /*RUN LogIt   IN hMM-PP (1,"Log ttArt").
    FOR EACH ttArt:
        hTable = BUFFER ttArt:HANDLE.
        RUN LogTT IN hMM-PP (2,hTable).
    END.*/

    /*RUN ExportRptDet      IN hMM-PP.*/

     /****************** End Main *******************/
 END.
                                                                          
IF VALID-HANDLE(hMM-PP) THEN DELETE PROCEDURE hMM-PP.         
                                                                          
CATCH AnyErr AS Progress.Lang.Error:                                      
    RUN LogIt        IN hMM-PP (1,"Error:" + AnyErr:GetMessage(1)).   
END CATCH.                                                                


