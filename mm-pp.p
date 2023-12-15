USING PROGRESS.Json.ObjectModel.*.

/*-----------------------------------------------------------------------------
  File.............: mm-pp.p
  Description......: Program that holds all the JM procedures
  Input Parameters.:
  Output Parameters:
  Author...........: Terry Penny
  Created..........: ?
-----------------------------------------------------------------------------*/
/*****************************************************************************/
/*                            Modification Log                               */
/*                                                                           */
/*   Date   Userid   Description                                             */
/* -------- -------- ---------------------------------------------------------- 
   12/14/23 ryanle  Initial Code Update                                     
*/
/*****************************************************************************/



/****************** Global Definitions****************************************/
DEFINE VARIABLE iLoop            AS INTEGER   NO-UNDO. /* Loop Counter */
DEFINE VARIABLE iTime            AS INTEGER   NO-UNDO. /* Timer Variable */
DEFINE VARIABLE cPrintTypes      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iErrorCnt        AS INTEGER   NO-UNDO. /* JM Batch Error Count */
DEFINE VARIABLE iRunCnt          AS INTEGER   NO-UNDO. /* JM Batch Run Count   */
DEFINE VARIABLE cDBase           AS CHARACTER NO-UNDO. /* Current Database Connection */
DEFINE VARIABLE iProgStart       AS INTEGER   NO-UNDO. /* JM Start Time */
DEFINE VARIABLE iProgFinish      AS INTEGER   NO-UNDO. /* JM Finish Time */
DEFINE VARIABLE iBatchCnt        AS INTEGER   NO-UNDO. /* JM Batch Count */
DEFINE VARIABLE iBatchSides      AS INTEGER   NO-UNDO. /* JM Batch Sides Count*/
DEFINE VARIABLE iErrorStatus     AS INTEGER   NO-UNDO. /* Error Status Holder */
DEFINE VARIABLE cMiscParts       AS CHARACTER NO-UNDO INITIAL "12P0000,12U0000,12F0000,12R0000,12COREX,12MISC".
DEFINE VARIABLE cProgrammerList  AS CHARACTER NO-UNDO INITIAL "webteam@lowen.com;progressgroup@lowen.com".


/****************** Include Files ********************************************/
{glob_var.i NEW}
{xmltag.i}
{scriptsdir.i}
{networkshare.i}
{mm.i}
{xmlParse.i "NEW"}
{dynamicNest.i}

/****************** Streams **************************************************/
DEFINE STREAM toplvl. 
DEFINE STREAM midlvl.
DEFINE STREAM lowlvl.
DEFINE STREAM S1.
DEFINE STREAM S2.


/****************** Temp Tables **********************************************/
DEFINE TEMP-TABLE mDet
    FIELD mrec AS RECID.

DEFINE TEMP-TABLE idet
    FIELD iName     AS CHAR
    FIELD iPathway  AS CHAR.

DEFINE TEMP-TABLE rpt_det
    FIELD itemseq AS INT
    FIELD so_no   AS CHAR
    FIELD ITEM_no AS INT
    FIELD issue   AS CHAR
    FIELD reason  AS CHAR
    INDEX itemseq AS UNIQUE itemseq.

DEFINE TEMP-TABLE ttMat 
    FIELD ttPart AS CHAR 
    FIELD ttQty  AS INT
    INDEX ttPart AS UNIQUE ttPart.

DEFINE TEMP-TABLE batchdet
    FIELD orderNo  AS CHAR
    FIELD itemNo   AS INT
    FIELD orderQty AS INT
    FIELD gangqty  AS INT.

DEFINE TEMP-TABLE hFolder
    FIELD path AS CHAR.
    
DEFINE TEMP-TABLE ttChg
    FIELD ttPart AS CHAR
    FIELD ttQty  AS INT
    INDEX ttPart AS UNIQUE ttPart.

DEFINE TEMP-TABLE ttorder
    FIELD ttseq  AS INT 
    FIELD ttseq2 AS INT 
    FIELD ttqty  AS INT
    FIELD ttdate AS INT
    FIELD ttInv  AS CHAR
    FIELD ttType AS CHAR
    FIELD ttprty AS DEC.

DEFINE TEMP-TABLE ttSeq
    FIELD ttSeqnum AS INT
    INDEX ttSeqNum AS UNIQUE ttSeqNum.

DEFINE TEMP-TABLE ttdel
    FIELD ttitemseq AS INT
    INDEX ttitemseq AS UNIQUE ttitemseq.

DEFINE TEMP-TABLE tBatches NO-UNDO
    FIELD batchseq AS INTEGER
    INDEX batchseq IS PRIMARY UNIQUE batchseq.

DEFINE TEMP-TABLE saves
    FIELD sBatch  AS CHAR
    FIELD sLoc    AS CHAR
    FIELD sTmpLoc AS CHAR. 

DEFINE TEMP-TABLE issue
    FIELD xSubject AS CHAR
    FIELD xOrder   AS CHAR
    FIELD xItem    AS CHAR
    FIELD xSize    AS CHAR
    FIELD xImage   AS CHAR
    FIELD xBatch   AS CHAR
    FIELD xPos     AS CHAR
    FIELD xSendXml AS CHAR
    FIELD xRecXml  AS CHAR.

DEFINE TEMP-TABLE logs
    FIELD lProcedure  AS CHAR
    FIELD lStartTime  AS CHAR
    FIELD lFinishTime AS CHAR
    FIELD lRunTime    AS CHAR
    FIELD lRunDate    AS CHAR
    FIELD lXml        AS CHAR
    FIELD lResponse   AS CHAR.

DEFINE TEMP-TABLE tempDet LIKE sign_mm_det.
DEFINE TEMP-TABLE tempHdr LIKE nest_mm_hdr
    FIELD order AS INTEGER .

DEFINE TEMP-TABLE dueBy
    FIELD bedseq   AS INT
    FIELD avgDate  AS DATE
    FIELD TYPE     AS CHAR
    INDEX bedseq   AS PRIMARY UNIQUE TYPE bedseq.
    
DEFINE TEMP-TABLE RedoItems
    FIELD itemseq AS INT
    INDEX itemseq AS PRIMARY UNIQUE itemseq.
    
DEFINE TEMP-TABLE ttImages
        FIELD tFileName AS CHARACTER.

DEFINE TEMP-TABLE tmp_ttArt LIKE ttArt.

/****************** Global Buffers *******************************************/
DEFINE BUFFER buf_ttart      FOR ttart.
DEFINE BUFFER b_ttart        FOR ttart.
DEFINE BUFFER bbArt          FOR ttArt.
DEFINE BUFFER b_mm_det       FOR sign_mm_det.
DEFINE BUFFER buf_mm_det     FOR sign_mm_det.
DEFINE BUFFER bb_mm_det      FOR sign_mm_det.
DEFINE BUFFER buf_mm_hdr     FOR sign_mm_hdr.
DEFINE BUFFER b_mm_hdr       FOR sign_mm_hdr.
DEFINE BUFFER bb_mm_hdr      FOR sign_mm_hdr.
DEFINE BUFFER b_mm_reprint   FOR sign_mm_reprint.
DEFINE BUFFER buf_mm_reprint FOR sign_mm_reprint.
DEFINE BUFFER b_signbed      FOR signbed.
DEFINE BUFFER buf_signbed    FOR signbed.
DEFINE BUFFER buf_beddet     FOR signbeddet.
DEFINE BUFFER b_signbeddet   FOR signbeddet.
DEFINE BUFFER b_beddet       FOR signbeddet.
DEFINE BUFFER b_squ_mat      FOR squ_mat.
DEFINE BUFFER buf_ptdet      FOR squ_ptdet.
DEFINE BUFFER b_ptdet        FOR squ_ptdet.
DEFINE BUFFER squdet         FOR squ_ptdet.
DEFINE BUFFER b_items        FOR so_items.
DEFINE BUFFER b_so_items     FOR so_items.
DEFINE BUFFER buf_so_items   FOR so_items.
DEFINE BUFFER b_squ_plan     FOR squ_plan.
DEFINE BUFFER mm_file        FOR zz_file.
DEFINE BUFFER zz_msg         FOR zz_file.
DEFINE BUFFER buf_zz_msg     FOR zz_file.

/************************* End Definitions ***********************************/
    


ASSIGN iProgStart  = TIME
       iProgFinish = ?.

/* **********************  Internal Procedures  *********************** */


PROCEDURE BuildTT:
    DEFINE INPUT PARAMETER cSo      AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER cItem    AS INT   NO-UNDO.

    DEFINE VARIABLE cType           AS CHAR  NO-UNDO.
    DEFINE VARIABLE cTemplate       AS INT   NO-UNDO.
    DEFINE VARIABLE cPointer        AS INT   NO-UNDO.
    DEFINE VARIABLE cSwitch         AS LOG   NO-UNDO.
    DEFINE VARIABLE backout         AS LOG   NO-UNDO.
    DEFINE VARIABLE isReflect       AS LOG   NO-UNDO.
    DEFINE VARIABLE reflectInv      AS CHAR  NO-UNDO INITIAL "LBZCO0624RF,LBZCO0624RF-CG".
    DEFINE VARIABLE reflectBom      AS CHAR  NO-UNDO INITIAL "LMV18-8100,LMV24-8100,LMV24-2100-10,LMV24-7930,LMV24-7930-NL,LMV24-7310-NL,LMV30-8100".
    DEFINE VARIABLE cHeight         AS DEC   NO-UNDO.
    DEFINE VARIABLE cWidth          AS DEC   NO-UNDO.
    DEFINE VARIABLE tmpint          AS INT   NO-UNDO.
    DEFINE VARIABLE totint          AS INT   NO-UNDO.
    DEFINE VARIABLE colorOk         AS LOG   NO-UNDO.
    DEFINE VARIABLE cMaterial       AS CHAR  NO-UNDO.
    DEFINE VARIABLE invPartNo       AS CHAR  NO-UNDO.
    DEFINE VARIABLE lastTTart       AS RECID NO-UNDO.
    DEFINE VARIABLE matRecid        AS RECID NO-UNDO.
    DEFINE VARIABLE cICseq          AS INT   NO-UNDO.
    DEFINE VARIABLE hasBleed        AS LOG   NO-UNDO.
    DEFINE VARIABLE qtyRan          AS INT   NO-UNDO.
    DEFINE VARIABLE qtyNeeded       AS INT   NO-UNDO.
    DEFINE VARIABLE inQueue         AS LOG   NO-UNDO.
    DEFINE VARIABLE blah            AS CHAR  NO-UNDO.
    DEFINE VARIABLE squptrec        AS RECID NO-UNDO.
    DEFINE VARIABLE hotfolderin     AS INT   NO-UNDO.
    DEFINE VARIABLE hotfolderout    AS INT   NO-UNDO.
    DEFINE VARIABLE isPrecut        AS LOG   NO-UNDO INITIAL FALSE.
    
    DEFINE VARIABLE IsDynamicNestCompatable AS LOGICAL NO-UNDO.

    
    ASSIGN isReflect = NO colorOK = FALSE hasBleed = NO.
    FIND so_items NO-LOCK WHERE so_items.so_no = cSo AND so_items.ITEM_no = cITem NO-ERROR.
    FIND so_file NO-LOCK WHERE so_file.so_no = cSo NO-ERROR.
    IF NOT AVAIL so_items OR NOT AVAIL so_file THEN NEXT.

    FIND pt_det NO-LOCK WHERE pt_det.part_no = so_items.part_no NO-ERROR.
    FIND FIRST squ_plan NO-LOCK WHERE squ_plan.itemseq = so_items.itemseq NO-ERROR.
    squptrec = ?.
    IF AVAIL pt_det THEN DO:
        FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = so_items.itemseq AND squ_ptdet.TYPE <> "Frame" NO-ERROR.
        IF AVAIL squ_ptdet THEN DO:
            ASSIGN squptrec = RECID(squ_ptdet).
            IF NOT squ_ptdet.digitalDF  AND NOT squ_ptdet.digitalSF THEN DO:
                RUN ReportIssues(so_items.itemseq,"MM-Missing Sides",so_items.so_no,STRING(so_items.ITEM_no),"","","","","","").
            END.
            RUN FindMatPanel.p (squ_ptdet.subseq,OUTPUT matRecid).
            FIND squ_mat NO-LOCK WHERE RECID(squ_mat) = matRecid NO-ERROR. 

            IF NOT AVAIL squ_mat AND AVAIL squ_plan THEN DO:
                RUN findIcNo(so_items.itemseq,so_items.so_no,so_items.ITEM_No,STRING(squ_plan.ic_no),so_items.part_no,"no",OUTPUT cICseq).
                FIND FIRST b_ptdet NO-LOCK WHERE b_ptdet.itemseq = cICseq AND b_ptdet.TYPE <> "Frame" NO-ERROR.
                IF AVAIL b_ptdet THEN DO:
                    RUN FindMatPanel.p (b_ptdet.subseq,OUTPUT matRecid).
                    FIND FIRST squ_mat NO-LOCK WHERE RECID(squ_mat) = matRecid NO-ERROR. 
                END.
                IF NOT AVAIL squ_mat THEN DO:
                    RUN findIcNo(so_items.itemseq,so_items.so_no,so_items.ITEM_No,STRING(squ_plan.ic_no),so_items.part_no,"YES",OUTPUT cICseq).
                    FIND FIRST b_ptdet NO-LOCK WHERE b_ptdet.itemseq = cICseq AND b_ptdet.TYPE <> "Frame" NO-ERROR.
                    IF AVAIL b_ptdet THEN DO:
                        RUN FindMatPanel.p (b_ptdet.subseq,OUTPUT matRecid).
                        FIND FIRST squ_mat NO-LOCK WHERE RECID(squ_mat) = matRecid NO-ERROR. 
                    END.
                END.
                IF NOT AVAIL squ_mat THEN DO:
                    RUN findIcNo(so_items.itemseq,so_items.so_no,so_items.ITEM_No,STRING(squ_plan.ic_no),so_items.part_no,"Guess",OUTPUT cICseq).
                    FIND FIRST b_ptdet NO-LOCK WHERE b_ptdet.itemseq = cICseq AND b_ptdet.TYPE <> "Frame" NO-ERROR.
                    IF AVAIL b_ptdet THEN DO:
                        RUN FindMatPanel.p (b_ptdet.subseq,OUTPUT matRecid).
                        FIND FIRST squ_mat NO-LOCK WHERE RECID(squ_mat) = matRecid NO-ERROR. 
                    END.
                END.
            END.
        END.
        ELSE DO:
            RUN findIcNo(so_items.itemseq,so_items.so_no,so_items.ITEM_No,STRING(squ_plan.ic_no),so_items.part_no,NO,OUTPUT cICseq).
            FIND FIRST b_ptdet NO-LOCK WHERE b_ptdet.itemseq = cICseq AND b_ptdet.TYPE <> "Frame" NO-ERROR.
            IF AVAIL b_ptdet THEN DO:
                RUN FindMatPanel.p (b_ptdet.subseq,OUTPUT matRecid).
                FIND FIRST squ_mat NO-LOCK WHERE RECID(squ_mat) = matRecid NO-ERROR. 
            END.
            IF NOT AVAIL squ_mat THEN DO:
                RUN findIcNo(so_items.itemseq,so_items.so_no,so_items.ITEM_No,STRING(squ_plan.ic_no),so_items.part_no,YES,OUTPUT cICseq).
                FIND FIRST b_ptdet NO-LOCK WHERE b_ptdet.itemseq = cICseq AND b_ptdet.TYPE <> "Frame" NO-ERROR.
                IF AVAIL b_ptdet THEN DO:
                    RUN FindMatPanel.p (b_ptdet.subseq,OUTPUT matRecid).
                    FIND FIRST squ_mat NO-LOCK WHERE RECID(squ_mat) = matRecid NO-ERROR. 
                END.
            END.
        END. /*end else do*/

        cMaterial = "". InvPartNo = "". cType = "".
        IF NOT AVAIL squ_mat THEN FIND squ_mat NO-LOCK WHERE RECID(squ_mat) = matRecid NO-ERROR.
        IF AVAIL squ_mat THEN DO:
            IF NOT AVAIL squ_ptdet THEN
                FIND squ_ptdet NO-LOCK WHERE squ_ptdet.subseq = squ_mat.subseq NO-ERROR.

            IF squptrec <> ? THEN FIND squ_ptdet NO-LOCK WHERE RECID(squ_ptdet) = squptrec.

            RUN getSubstrate.p ("",so_items.itemseq,OUTPUT cType,OUTPUT cMaterial).
 
            
            IF cType = "" OR cMaterial = "" THEN DO:
                RUN reportIssues(so_items.itemseq,"MM-No Substrate on File",so_items.so_no,STRING(so_items.ITEM_no),"","","","","","").
/*                     LEAVE. */
            END.

            /*figure out sign height and width*/
            ASSIGN cTemplate = 0 
                    cPointer = 0 
                     cSwitch = NO 
                     cHeight = 0 
                      cWidth = 0.
            IF cType <> "Corex" THEN DO:
                   ASSIGN cHeight   = IF pt_det.pressprintingheight = 0 THEN squ_ptdet.pressprintingheight ELSE pt_det.pressprintingheight
                          cWidth    = IF pt_det.pressprintingwidth = 0  THEN squ_ptdet.pressprintingwidth  ELSE pt_det.pressprintingwidth.
                                        
                    /*TS Fix*/
                    IF so_file.cust_no = "217559" THEN DO:
                        IF AVAIL squ_ptdet THEN ASSIGN cHeight = squ_ptdet.pressprintingheight
                                                       cWidth  = squ_ptdet.pressprintingwidth. 
                                                          
                    END.
                    
                    
                    IF cHeight = 0 OR cWidth = 0 THEN ASSIGN cHeight = pt_det.pt_Height
                                                              cWidth = pt_det.pt_Width.
                               
            END.
            ELSE DO:
                IF CAN-DO(cMiscParts,so_items.part_no) THEN DO:
                    /*squ_quoterrequest*/
                    ASSIGN cHeight = squ_ptdet.pt_Height
                           cWidth  = squ_ptdet.pt_Width.
                END.
                ELSE DO:
                    /*if vert flute then flip height and width to get correct signs size to get correct template*/
                    IF squ_ptdet.VERT_flutes = NO AND squ_ptdet.horz_flutes = NO THEN DO:
                        IF pt_det.VERT_flutes = NO AND pt_det.horz_flutes = NO THEN DO:
                            RUN reportIssues(so_items.itemseq,"MM-No Flute Direction",so_items.so_no,STRING(so_items.ITEM_no),cType + string(pt_det.pressprintingheight) + "x" + string(pt_det.pressprintingwidth),"","","","","").
                        END.
                        IF pt_det.VERT_flute THEN DO:
                            ASSIGN cHeight = squ_ptdet.pressprintingwidth
                                   cWidth  = squ_ptdet.pressprintingheight.
                        END.
                        ELSE DO:
                            ASSIGN cHeight = squ_ptdet.PressPrintingHeight
                                   cWidth  = squ_ptdet.pressprintingwidth.
                        END.
                    END.
                    ELSE DO:
                        IF squ_ptdet.VERT_flute THEN DO:
                            ASSIGN cHeight = squ_ptdet.pressprintingwidth
                                   cWidth  = squ_ptdet.pressprintingheight.
                        END.
                        ELSE DO:
                            ASSIGN cHeight = squ_ptdet.PressPrintingHeight
                                   cWidth  = squ_ptdet.pressprintingwidth.
                        END.
                    END.
                END.
            END. 
            
            /*see if its reflective*/
            IF CAN-DO(reflectInv,squ_mat.part_no) THEN ASSIGN isReflect = YES.
            IF isReflect = NO THEN DO:
                FOR EACH bom_file NO-LOCK WHERE bom_file.PARENT = pt_det.part_no:
                    IF CAN-DO(reflectBom,bom_file.part_no) THEN ASSIGN isReflect = YES.
                END.
            END.
            
            IF isReflect = NO AND CAN-DO(cMiscParts,so_items.part_no) THEN DO:
                /*try and look at both reg order and intercompany*/
                FOR EACH b_squ_mat NO-LOCK WHERE b_squ_mat.subseq = squ_mat.subseq:
                    IF CAN-DO(reflectBom,b_squ_mat.part_no) THEN ASSIGN isReflect = YES.
                END.
                FOR EACH b_squ_mat NO-LOCK WHERE b_squ_mat.subseq = squ_ptdet.subseq:
                    IF CAN-DO(reflectBom,b_squ_mat.part_no) THEN ASSIGN isReflect = YES.
                END.
            END.           
            
            invPartNo = squ_mat.part_no.
            IF isReflect THEN DO: /*Add R to inv part to notify its reflective if not already*/
                IF (NOT invPartNo MATCHES "*RF" AND NOT invPartNo MATCHES "*R" AND NOT invPartNo MATCHES "*RF-CG") OR (LOOKUP(invPartNo,"RBZCO3024H-RR,RBZCO2236H-RR") > 0) THEN DO: /*added per Mary - RyanLe*/
                    ASSIGN invPartNo = invPartNo + "RF".
                END.
            END.
            
            RUN CheckDynamicNestCompatability(squ_mat.part_no, cType, IF isReflect THEN cMaterial + " Reflective" ELSE cMaterial, OUTPUT IsDynamicNestCompatable).
            
            IF IsDynamicNestCompatable THEN DO:
                RUN checkprecut.p(so_items.itemseq, OUTPUT isPrecut).
                IF isPrecut THEN IsDynamicNestCompatable = FALSE.

            END.
            
            IF NOT IsDynamicNestCompatable THEN DO:

                /*might have an issue here with steel foldovers.*/
                RUN getTemplate(cType,pt_det.part_no,cHeight,cWidth,(IF cType = "Poly" AND AVAIL squ_ptdet AND squ_ptdet.foldover THEN YES ELSE NO),OUTPUT cTemplate, OUTPUT cPointer, OUTPUT cSwitch).
                
                IF cTemplate = 0 AND NOT can-do("Decal,magnetic,vinyl",cType) THEN DO: /*omegabond,alumalite,*/                    
                    /*Added this to skip over corex*/
                    IF INDEX("corex",cType) = 0 THEN RUN reportIssues(so_items.itemseq,"MM-Can't Find Template",so_items.so_no,STRING(so_items.ITEM_no),cType + string(pt_det.pressprintingheight) + "x" + string(pt_det.pressprintingwidth),"","","","","").
                END.
            
                /*check to see if part has been modified and is ready for prepcenter.*/
                IF CAN-FIND(FIRST zz_file NO-LOCK WHERE zz_file.zz_key1 = "mm-corex" AND zz_file.zz_key2 = so_items.part_no AND zz_file.zz_log[1] = YES) THEN DO:
                    ASSIGN cTemplate = 0.
                END.
            END.

            /*get qty of need panels*/
            RUN getQty(so_items.itemseq,OUTPUT qtyRan, OUTPUT qtyNeeded, OUTPUT inQueue). 
            
            hotfolderin = IF NOT squ_ptdet.pt_hotfolderseq <= 0 THEN squ_ptdet.pt_hotfolderseq ELSE pt_det.pt_hotfolderseq.
            RUN custhotfolder.p(so_items.itemseq,hotfolderin, OUTPUT hotfolderout).

            CREATE ttArt.
            ASSIGN ttArt.ttTempSeq   = cTemplate
                   ttArt.ttSize      = STRING(pt_det.pressprintingheight) + "x" + STRING(pt_det.pressprintingwidth)
                   ttArt.ttPart      = so_items.part_no
                   ttArt.ttDue       = so_file.ship_by
                   ttArt.ttCustNo    = so_file.cust_no
                   ttArt.ttSO        = so_file.so_no
                   ttArt.ttItemNo    = so_items.ITEM_no
                   ttArt.ttSides     = IF squ_ptdet.DigitalDF THEN 2 ELSE 1
                   ttArt.ttItemseq   = so_items.itemseq
                   ttArt.ttInvPart   = invPartNo
                   ttArt.ttQty       = qtyNeeded  
                   ttArt.ttHotFolder = hotfolderout
                   ttArt.ttSwitch    = cSwitch
                   ttArt.ttSteelTent = (IF (squ_ptdet.steeltent OR squ_ptdet.jackunit) THEN YES ELSE NO) /* iLoop = 2 THEN TRUE ELSE FALSE*/
                   ttArt.ttType      = IF isReflect THEN cMaterial + " Reflective" ELSE cMaterial
                   ttArt.ttExploded  = NO
                   ttArt.ttDynamNest = IF cTemplate = 0 THEN YES ELSE NO
                   ttArt.ttHorzFlute = squ_ptdet.horz_flutes
                   ttArt.ttVertFlute = squ_ptdet.VERT_flutes
                   .
            RUN getHotfolder(1,ttArt.ttType,INPUT-OUTPUT ttArt.ttHotFolder, OUTPUT Blah).
              
            lastTTart   = RECID(ttart).
        END. /*if avail squ_mat*/
        ELSE RUN ReportIssues(so_items.itemseq,"MM-Planning",so_items.so_no,STRING(so_items.ITEM_no),"","Unable to find panel","","","","").
    END.
END PROCEDURE.


PROCEDURE CanIRun:
    DEFINE INPUT  PARAMETER toDo AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER c_ok AS LOG  NO-UNDO.
    DEFINE VARIABLE cmdline      AS CHAR NO-UNDO.
    DEFINE VARIABLE lines        AS CHAR NO-UNDO EXTENT 10.

    IF toDo = "Start" THEN DO:
        /*when Jm runs it creates a zz_file record with these parameters. this ensures the program doesnt get ran while it is already running*/
        FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "MediaManager" AND zz_file.zz_key2 = "MM-Running" NO-ERROR.
        IF NOT AVAIL zz_file THEN DO:


            /*creates the zz_file record*/
            RUN zz_control(NO,"PID").
            RUN zz_control(NO,"MM-Running").
            /*OK to run*/
            c_ok = YES.
        END.
        ELSE DO: 
            /*not ok to run*/
            c_ok = NO.
        END.
        IF AVAIL zz_file THEN RELEASE zz_file.
        IF AVAIL mm_file THEN RELEASE mm_file.


    END.
    ELSE DO:
        /*OK to run*/
        RUN zz_control (YES,"MM-Running").
        c_ok = YES.
    END.
    
END PROCEDURE.


PROCEDURE CheckImages:
    /*checks and assign image file locations*/
    DEFINE INPUT PARAMETER cItemseq  AS CHAR NO-UNDO.
    DEFINE VARIABLE cImage     AS CHAR NO-UNDO.
    DEFINE VARIABLE cProg      AS INT  NO-UNDO.
    DEFINE VARIABLE imagecnt   AS INT  NO-UNDO.
    DEFINE VARIABLE lastImage  AS INT  NO-UNDO.
    DEFINE VARIABLE imageSide  AS CHAR NO-UNDO.
    DEFINE VARIABLE imageQty   AS INT  NO-UNDO.
    DEFINE VARIABLE deleteArt  AS LOG  NO-UNDO.
    DEFINE VARIABLE tmpint     AS INT  NO-UNDO.
    DEFINE VARIABLE tempint    AS INT  NO-UNDO.
    DEFINE VARIABLE tempCnt    AS INT  NO-UNDO.
    DEFINE VARIABLE xplodlist  AS CHAR NO-UNDO.
    DEFINE VARIABLE tmpimage   AS CHAR NO-UNDO.
    DEFINE VARIABLE AlreadyRan AS LOG  NO-UNDO.
    DEFINE VARIABLE RanCnt     AS INT  NO-UNDO.
    DEFINE VARIABLE RanTot     AS INT  NO-UNDO.
    DEFINE VARIABLE newName    AS CHAR NO-UNDO.
    DEFINE VARIABLE missingSeq AS LOG  NO-UNDO.
    DEFINE VARIABLE tmpFile    AS CHAR NO-UNDO.
    DEFINE VARIABLE imgGood    AS LOG  NO-UNDO.
    DEFINE VARIABLE RegEngineBuild AS LOGICAL NO-UNDO.
    DEFINE VARIABLE ReprintRan     AS INTEGER NO-UNDO.
    DEFINE VARIABLE ReprintNeed    AS INTEGER NO-UNDO.
    DEFINE VARIABLE ReprintInQueue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE reason AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bso_items FOR so_items.
     
    IF cItemseq <> "" THEN RegEngineBuild = FALSE.
    ELSE RegEngineBuild = TRUE.
    
    RUN setHomeFolder.
                                      
    xplodlist = "".
    
    
    FOR EACH ttArt WHERE IF NOT RegEngineBuild THEN (ttart.ttItemseq = INT(cItemseq) AND ttArt.ttExploded = NO) ELSE ttArt.ttExploded = NO:
        IF RegEngineBuild AND CAN-FIND(FIRST sign_mm_reprint NO-LOCK WHERE sign_mm_reprint.Itemseq = ttArt.ttItemseq AND sign_mm_reprint.COMPLETE = FALSE) THEN DO:
            /* DELAYED REPRINTS*/
            FOR EACH sign_mm_reprint EXCLUSIVE-LOCK WHERE sign_mm_reprint.itemseq = ttArt.ttItemseq AND sign_mm_reprint.COMPLETE = FALSE AND sign_mm_reprint.delayed = TRUE:
                RUN GetReprintQty(sign_mm_reprint.ReprintId,OUTPUT ReprintRan, OUTPUT ReprintNeed, OUTPUT ReprintInQueue).
                IF reprintneed > ttArt.ttQty THEN ReprintNeed = ttArt.ttQty.

                CREATE bbart.
                BUFFER-COPY ttart EXCEPT ttart.ttQty ttart.ttFile TO bbart.
                ASSIGN bbart.ttqty       = ReprintNeed
                       bbart.ttfile      = sign_mm_reprint.Artfile
                       bbart.ttExploded  = YES
                       bbArt.ttReprintId = sign_mm_reprint.ReprintId
                       cImage            = sign_mm_reprint.Artfile.
            END.
            deleteArt = TRUE.           
        END.
        ELSE DO:
            FIND pt_det NO-LOCK WHERE pt_det.part_no = ttArt.ttPart NO-ERROR.
            FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = ttart.ttItemseq AND squ_ptdet.TYPE <> "frame" NO-ERROR.
            IF NOT AVAIL(squ_ptdet) THEN
                FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = ttart.ttItemseq AND squ_ptdet.TYPE = "frame" NO-ERROR. /*hope its here if panel not on this order*/
                
            ASSIGN cImage = ? deleteArt = NO.
            
            IF CAN-FIND(FIRST so_art NO-LOCK WHERE so_art.itemseq = ttart.ttitemseq AND so_art.TYPE = "Mini") THEN DO: /*Artlinks Images*/
                imagecnt = 0. imageQty = 0.
                FOR EACH so_art NO-LOCK WHERE so_art.itemseq = ttart.ttitemseq AND so_art.TYPE = "Mini" BY disp_order:
                    IF so_art.artfile MATCHES "*_RT*" THEN NEXT. /*don't count the second side as diff part*/
                    imagecnt = imagecnt + 1.
                END.
                
                IF imagecnt > 1  OR (squ_ptdet.steeltent = TRUE OR squ_ptdet.jackunit = TRUE) THEN DO:
                    FOR EACH so_art NO-LOCK WHERE so_art.itemseq = ttart.ttitemseq AND so_art.TYPE = "Mini" BY disp_order:
                        ASSIGN cImage = SEARCH(so_art.artfile).
                        
                        IF cImage = ? THEN DO:
                            LEAVE.
                        END.
                        ELSE IF NUM-ENTRIES(cImage,".") > 2 THEN DO:
                            /*if it looks like it has more that one extention get rid of it and email csr*/
                            RUN ReportIssues(ttArt.ttItemseq,"MM-Art Image Name Issue",ttart.ttso,STRING(ttart.ttItemNO),"",ttart.ttFile,"","","","").
                            LEAVE.
                        END.
                        ELSE DO:
                            RanCnt = 0. RanTot = 0.
                            IF RegEngineBuild THEN DO: 
    /*                          /*check to see if some have already printed or are on batches*/ */
                                FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.itemseq = ttart.ttitemseq AND entry(NUM-ENTRIES(sign_mm_det.artfile,"\"),sign_mm_det.artfile,"\") = entry(NUM-ENTRIES(so_art.artfile,"\"),so_art.artfile,"\") BREAK BY sign_mm_det.batchseq:
                                    ASSIGN RanCnt = RanCnt + 1.
                                    IF LAST-OF(sign_mm_det.batchseq) THEN DO:
                                        FIND sign_mm_hdr NO-LOCK OF sign_mm_det NO-ERROR.
                                        IF AVAIL sign_mm_hdr THEN DO:
                                            ASSIGN RanTot = RanTot + (RanCnt * sign_mm_hdr.qty)
                                                   RanCnt = 0.
                                        END.
                                    END.
                                END.
                                /*basically saying if its already ran then skip*/
                                IF ttart.ttqty > 0 AND 0 >= IF ((squ_ptdet.steeltent OR squ_ptdet.jackunit) AND NOT cImage MATCHES "*_RT*" AND NOT cImage MATCHES "*_LT*") THEN (so_art.qty * 2 - rantot) ELSE (so_art.qty - rantot) THEN NEXT.
                            END.

                            /*if image is good, explode ttart record into new records for each so_art mini*/
                            IF squ_ptdet.steeltent OR squ_ptdet.jackunit THEN DO:
                                FIND so_items NO-LOCK WHERE so_items.itemseq = ttArt.ttItemseq   NO-ERROR.
                                IF AVAIL so_items THEN DO:
                                    /*might need something here to try and determine if some of these have been printed if so which ones*/
                                    IF NOT so_items.so_art_override AND imagecnt > so_items.orderqty THEN DO:
                                        RUN ReportIssues(ttArt.ttItemseq,"MM-ArtLink Qty's Incorrect",ttart.ttso,STRING(ttart.ttItemNO),"","","","","","").
                                        LEAVE.
                                    END.
                                END.
                                IF cImage MATCHES "*_RT*" OR cImage MATCHES "*_LT*" THEN DO:
                                    CREATE bbart.
                                    BUFFER-COPY ttart EXCEPT ttart.ttQty ttArt.ttFile TO bbArt.
                                    ASSIGN bbart.ttQty      = so_art.qty - RanTot
                                           bbart.ttFile     = cImage
                                           bbart.ttExploded = YES.
                                END.
                                ELSE DO:
                                    CREATE bbArt.
                                    BUFFER-COPY ttArt EXCEPT ttArt.ttQty ttArt.ttFile TO bbart.
                                    ASSIGN bbart.ttQty  =  2 * so_art.qty - RanTot 
                                           bbart.ttFile = cImage
                                           bbart.ttExploded = YES.
                                END.
                            END.
                            ELSE DO:
                                FIND so_items NO-LOCK WHERE so_items.itemseq = ttArt.ttItemseq NO-ERROR.
                                IF AVAIL so_items THEN DO:
                                    /*might need something here to try and determine if some of these have been printed if so which ones*/
                                    IF NOT so_items.so_art_override AND imagecnt > so_items.orderqty THEN DO:
                                        RUN ReportIssues(ttArt.ttItemseq,"MM-ArtLink Qty's Incorrect",ttart.ttso,STRING(ttart.ttItemNO),"","","","","","").
    /*                                     RUN trimbeds(string(ttart.ttItemseq)). */
                                        LEAVE.
                                    END.
                                END.
                                IF cImage MATCHES "*_RT*" OR cImage MATCHES "*_LT*" THEN DO:
                                    IMAGEside = IF cImage MATCHES "*_RT*" THEN "RT" ELSE "LT".
                                    FIND bbart WHERE bbart.ttitemseq = ttart.ttitemseq AND bbart.ttFile 
                                        = REPLACE(cImage,(IF IMAGEside = "LT" THEN "_LT" ELSE "_RT"),(IF IMAGEside = "LT" THEN "_RT" ELSE "_LT")) NO-ERROR. /*this not correct*/
                                    IF AVAIL bbart THEN DO: 
                                        ASSIGN  bbart.ttfile = bbart.ttfile + (IF bbart.ttfile <> "" THEN "," ELSE "") + cImage
                                                bbart.ttqty = so_art.qty - RanTot.
                                    END.
                                    ELSE DO:
                                        CREATE bbart.
                                        BUFFER-COPY ttart EXCEPT ttart.ttQty ttart.ttFile TO bbart.
                                        ASSIGN bbart.ttqty      = so_art.qty - RanCnt 
                                               bbart.ttfile     = bbart.ttfile + (IF bbart.ttfile <> "" THEN "," ELSE "") + cImage
                                               bbart.ttExploded = YES.
                                    END.
                                END.
                                ELSE DO:
                                    CREATE bbart.
                                    BUFFER-COPY ttart EXCEPT ttart.ttQty ttart.ttFile TO bbart.
                                    ASSIGN bbart.ttqty      = so_art.qty - RanTot
                                           bbart.ttfile     = bbart.ttfile + (IF bbart.ttfile <> "" THEN "," ELSE "") + cImage
                                           bbart.ttExploded = YES.
                                END.
                            END.
                        END.
                        IF NOT AVAIL ttart THEN LEAVE.
                    END.
                    deleteArt = YES. /*delete the original b/c we xploded and created new recs*/
                END.
                ELSE DO:
                    FOR EACH so_art NO-LOCK WHERE so_art.itemseq = ttart.ttitemseq AND so_art.TYPE = "Mini" BY disp_order:
                        ASSIGN cImage = SEARCH(so_art.artfile).
                        IF cImage = ? THEN DO:
    /*                         deleteArt = YES. */
                            LEAVE.
                        END.
                        ELSE IF NUM-ENTRIES(cImage,".") > 2 THEN DO:
                            /*if it looks like it has more that one extention get rid of it and email csr*/
                            RUN ReportIssues(ttArt.ttItemseq,"MM-Art Image Name Issue",ttart.ttso,STRING(ttart.ttItemNO),"",ttart.ttFile,"","","","").
                            LEAVE.
                        END.
                        ELSE DO: 
                            IF INDEX(ttArt.ttFile,cImage) = 0 THEN ASSIGN ttArt.ttFile = ttArt.ttFile + (IF ttArt.ttFile <> "" THEN "," ELSE "") + cImage.
                        END.
                        
                        /*check to see if both names match*/
                        IF NUM-ENTRIES(ttart.ttfile,",") > 1 THEN DO:
                            tmpFile = SEARCH(ENTRY(1,ttart.ttfile)).
                        
                            IF tmpfile <> ? THEN DO:
                                tmpFile = ?.
                                IF ENTRY(1,ttart.ttfile) MATCHES "*_LT*" THEN
                                    tmpFile = REPLACE(ENTRY(1,ttart.ttfile),"_LT","_RT").
                                IF ENTRY(1,ttart.ttfile) MATCHES "*_RT*" THEN
                                    tmpFile = REPLACE(ENTRY(1,ttart.ttfile),"_RT","_LT").
                            END.

                            IF SEARCH(tmpfile) = ? THEN DO:
                                RUN ReportIssues(ttArt.ttItemseq,"MM-Art Image Name Issue",ttart.ttso,STRING(ttart.ttItemNO),"",ttart.ttFile,"","","","").
                                LEAVE.
                            END.
                            
                        END.
                    END.
                END.
            END.
            ELSE IF AVAIL pt_det AND (pt_det.stk_rider = TRUE OR pt_det.zzlog_3 = TRUE OR pt_det.next_day = TRUE) AND pt_det.dart_item = FALSE THEN DO: /*Stock Riders*/               
                FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = ttart.ttItemseq NO-ERROR.
                IF (pt_det.steeltent OR pt_det.jackunit) AND pt_det.prodfile1 <> "" AND pt_det.prodfile2 <> "" THEN DO:
                    DeleteArt = YES.
                    DO iLoop = 1 TO 2:
                        RanCnt = 0. RanTot = 0.
                        IF RegEngineBuild THEN DO: /*if not a rerun then check*/
    /*                          /*check to see if some have already printed or are on batches*/ */
                            /*expects both prod1 and prod2 to be populated to work correctly*/
                            FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.itemseq = ttart.ttitemseq AND
                                sign_mm_det.artfile MATCHES (IF iLoop = 1 THEN "*" + ttart.ttso + "-" + string(ttart.ttItemNo) + ".pdf" 
                                    ELSE "*" + ttart.ttso + "-" + string(ttart.ttItemNo) + "-Left.pdf") BREAK BY sign_mm_det.batchseq:
                                ASSIGN RanCnt = RanCnt + 1.
                                IF LAST-OF(sign_mm_det.batchseq) THEN DO:
                                    FIND sign_mm_hdr NO-LOCK OF sign_mm_det NO-ERROR.
                                    IF AVAIL sign_mm_hdr THEN DO:
                                        ASSIGN RanTot = RanTot + (RanCnt * sign_mm_hdr.qty)
                                               RanCnt = 0.
                                    END.
                                END.
                            END.
                        END.
                        CREATE bbart.
                        BUFFER-COPY ttart EXCEPT ttart.ttQty ttart.ttFile TO bbart.
                        ASSIGN bbart.ttqty      = IF ranTot = 0 THEN so_items.orderqty ELSE IF ttArt.ttQty - RanTot <= 0 THEN 0 ELSE ttArt.ttQty - RanTot /*ttArt.ttQty / 2*/
                               bbart.ttfile     = IF iLoop = 1 THEN pt_det.prodfile1 ELSE pt_det.prodfile2
                               bbart.ttExploded = YES.
                               
                    END.
                    /* so it doesn't report an issue*/
                    cImage = SEARCH(pt_det.prodfile1).
                END.
                ELSE DO:
                            
                    IF SEARCH(cBatchImgLoc + "\" + ttArt.ttso + "-" + STRING(ttArt.ttITEMno) + ".pdf") <> ? THEN DO:
                            ASSIGN ttArt.ttFile = cBatchImgLoc + "\" + ttArt.ttso + "-" + STRING(ttArt.ttITEMno) + ".pdf".
                            cImage = ttart.ttFile.
                    END.
                    ELSE IF pt_det.prodfile1 <> "" AND SEARCH(pt_det.prodfile1) <> ? THEN DO: 
                        ASSIGN ttArt.ttFile = (IF SEARCH(REPLACE(pt_det.prodfile1,".pdf","_ws.pdf")) <> ? THEN
                               SEARCH(REPLACE(pt_det.prodfile1,".pdf","_ws.pdf")) ELSE SEARCH(pt_det.prodfile1)).
                               cImage = pt_det.prodfile1.
                    END.
        
                    IF SEARCH(cBatchImgLoc + "\" + ttArt.ttso + "-" + STRING(ttArt.ttITEMno) + "-LEFT.pdf") <> ? THEN DO:
                            ASSIGN ttArt.ttFile = ttArt.ttFile + (IF ttArt.ttFile = "" THEN "" ELSE ",") +  cBatchImgLoc + "\" + ttArt.ttso + "-" + string(ttArt.ttITEMno) + "-LEFT.pdf".
                            cImage = ttArt.ttFile.
                    END.
                    ELSE IF pt_det.prodfile2 <> "" AND deleteArt = NO AND SEARCH(pt_det.prodfile2) <> ? THEN DO: 
                        ASSIGN ttArt.ttFile = ttArt.ttFile + (IF ttArt.ttFile = "" THEN "" ELSE ",") + (IF SEARCH(REPLACE(pt_det.prodfile2,".pdf","_ws.pdf")) <> ? THEN 
                               SEARCH(REPLACE(pt_det.prodfile2,".pdf","_ws.pdf")) ELSE SEARCH(pt_det.prodfile2))
                               cImage = pt_det.prodfile2.
                    END.
                    
                    IF cImage = ? THEN  reason = "File not found 1".
                     
                END.
            END.
            ELSE DO: /*Dart Images*/ /*images need to be cdr files*/
                IF pt_det.directional AND (pt_det.steeltent OR pt_det.jackunit) THEN DO: /*diff logic for s/f with 2 images - used to say pt_det.steeltent OR pt_det.jackunit but i changed to say directional since -left is never created unless directional*/
                    FIND so_items NO-LOCK WHERE so_items.itemseq = ttart.ttitemseq NO-ERROR.
                    IF NOT AVAIL so_items THEN NEXT.
                    imgGood = YES.
                    DO iLoop = 1 TO 2:
                        ASSIGN cImage = imageShare + "DartProduction\" + ttart.ttSo + "\" + ttart.ttSo + "-" + string(ttArt.ttitemno) + (IF iLoop = 1 THEN ".pdf" ELSE "-left.pdf"). /*changed from ELSE "-left.pdf"*/
                        IF SEARCH(cImage) = ? THEN DO:
                            imgGood = NO.
                        END.
                    END.
                    IF NOT imgGood THEN cImage = ?.
                    IF imgGood THEN DO iLoop = 1 TO 2:
                        ASSIGN cImage = imageShare + "DartProduction\" + ttart.ttSo + "\" + ttart.ttSo + "-" + string(ttArt.ttitemno) + (IF iLoop = 1 THEN ".pdf" ELSE "-left.pdf"). /*changed from ELSE "-left.pdf"*/
                        
                        IF cImage = ? THEN NEXT.
                        RanCnt     = 0. RanTot = 0.
                        IF RegEngineBuild THEN DO: /*if not a rerun then check*/
            /*                          /*check to see if some have already printed or are on batches*/ */
                            FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.itemseq = ttart.ttitemseq AND entry(NUM-ENTRIES(sign_mm_det.artfile,"\"),sign_mm_det.artfile,"\") = entry(NUM-ENTRIES(cImage,"\"),cImage,"\") BREAK BY sign_mm_det.batchseq:
                                ASSIGN RanCnt = RanCnt + 1.
                                IF LAST-OF(sign_mm_det.batchseq) THEN DO:
                                    FIND sign_mm_hdr NO-LOCK OF sign_mm_det NO-ERROR.
                                    IF AVAIL sign_mm_hdr THEN DO:
                                        ASSIGN RanTot = RanTot + (RanCnt * sign_mm_hdr.qty)
                                               RanCnt = 0.
                                    END.
                                END.
                            END.
                            IF ttart.ttqty > 0 AND 0 >= so_items.orderqty - rantot THEN NEXT.
                        END.
            
                        
                        CREATE bbart.
                        BUFFER-COPY ttart EXCEPT ttart.ttQty ttart.ttFile TO bbart.
                        ASSIGN bbart.ttqty  =  so_items.orderqty - RanTot 
                               bbart.ttfile = cImage
                               bbart.ttExploded = YES.
                        
                    END.
                    deleteArt = YES.
                END.
                ELSE IF pt_det.qrcode THEN DO: /*QR Code Items*/
                    ASSIGN tmpImage = imageShare + "DartProduction\" + ttart.ttSo + "\" + ttart.ttSo + "-" + string(ttArt.ttitemno) + ".pdf".
                    TempInt = 1.
                    TempCnt = 0.
                    IF pt_det.qrcodepairs = TRUE THEN DO:
                        FOR EACH LowenTagItems NO-LOCK WHERE LowenTagItems.itemseq = ttart.ttitemseq, EACH LowenTag OF LowenTagItems:
                            DO iloop = 1 TO LowenTagItems.orderqty:
                                TempCnt = TempCnt + 1.
                                IF TempCnt = pt_det.qrcodeqty THEN DO:
                                    cImage = tmpImage.
                                    cImage = REPLACE(cImage,".pdf","-" + STRING(TempInt) + ".pdf").
                                    cImage = SEARCH(cImage).
                                    IF cImage = ? THEN DO:
                                        LEAVE.
                                    END.
                                    ELSE DO:
                                        CREATE bbart.
                                        BUFFER-COPY ttart EXCEPT ttart.ttQty ttart.ttFile TO bbart.
                                        ASSIGN bbArt.ttQty      = 1
                                               bbArt.ttExploded = YES
                                               bbArt.ttFile     = cImage.
                                        ASSIGN TempInt          = TempInt + 1
                                               TempCnt          = 0.
                                    END.
                                END.
                            END.
                        END.
                    END.
                    ELSE DO:
                        FOR EACH LowenTagItems NO-LOCK WHERE LowenTagItems.itemseq = ttart.ttitemseq, EACH LowenTag OF LowenTagItems:
                            cImage = tmpimage.
                            IF INDEX(LowenTag.TYPE,"URL") > 0 THEN cImage = REPLACE(cImage,".pdf","-URL" + STRING(TempInt) + "-Qty-" + STRING(LowenTagItems.orderqty) + ".pdf").
                            IF INDEX(LowenTag.TYPE,"MOBI") > 0 OR (LowenTag.TYPE = "" AND LowenTagItems.reprint = TRUE) THEN cImage = REPLACE(cImage,".pdf","-" + LowenTagItems.CodeKey + "-Qty-" + STRING(LowenTagItems.orderqty) + ".pdf").
                            cImage = SEARCH(cImage).
                            IF cImage = ? THEN DO:
                                LEAVE.
                            END.
                            ELSE DO:
                                CREATE bbart.
                                BUFFER-COPY ttart EXCEPT ttart.ttQty ttart.ttFile TO bbart.
                                ASSIGN bbArt.ttQty      = LowenTagItems.orderqty
                                       bbArt.ttExploded = YES
                                       bbArt.ttFile     = cImage.
                            END.
                            TempInt = TempInt + 1.
                        END.
                    END.
                    deleteArt = YES.
                END.
                ELSE IF CAN-FIND (zz_file NO-LOCK WHERE zz_file.zz_key1 = "SeqNumberLine" AND zz_file.zz_key2 = ttArt.ttPart) THEN DO:
                    
                    /*consecutive id code signs*/
                    FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "SeqNumberLine" AND zz_file.zz_key2 = ttArt.ttPart NO-ERROR.
                    FIND so_items NO-LOCK WHERE so_items.itemseq = ttart.ttItemseq NO-ERROR.

                    IF AVAIL zz_file THEN
                        FIND FIRST so_copy NO-LOCK WHERE so_copy.itemseq = ttArt.ttItemseq AND so_copy.line_no = INTEGER(zz_file.zz_key3) NO-ERROR.

                    missingSeq = FALSE.
                    IF AVAIL so_copy AND AVAIL so_items THEN DO:
                        DO iLoop = 1 TO INT(so_items.orderqty):
                        
                            ASSIGN cImage = imageShare + "DartProduction\" + ttart.ttSo + "\" + ttart.ttSo + "-" + string(ttArt.ttitemno) + "-" + STRING(INTEGER(so_copy.copy_text) +  iLoop - 1) + ".pdf".
                            
                            IF SEARCH(cImage) = ? THEN DO: 
                                cImage = ?.
                                missingSeq = TRUE.
                            END.
                            ELSE DO:
                                IF CAN-FIND(FIRST sign_mm_det NO-LOCK WHERE sign_mm_det.itemseq = ttArt.ttItemseq AND entry(NUM-ENTRIES(sign_mm_det.artfile,"\"),sign_mm_det.artfile,"\") = entry(NUM-ENTRIES(cImage,"\"),cImage,"\")) THEN NEXT.
                                
                                CREATE bbArt.
                                BUFFER-COPY ttArt EXCEPT ttart.ttqty TO bbArt.
                                ASSIGN bbArt.ttQty  = 1
                                       bbArt.ttFile = cImage.
                            END.
                            
                        END.
                    END.
                    IF missingSeq THEN cImage = ?.
                    deleteArt = YES.
                END.
                ELSE DO: /*regular sart/dart*/
                    ASSIGN cImage = imageShare + "DartProduction\" + ttart.ttSo + "\" + ttart.ttSo + "-" + string(ttArt.ttitemno) + ".pdf".
                    IF SEARCH(cImage) = ? THEN DO: 
                        cImage = ?.
                    END.
                    ELSE DO:
                        ASSIGN ttart.ttfile = cImage
                               cImage       = REPLACE(cImage,".pdf","-left.pdf").
                        IF SEARCH(cImage) <> ? THEN ASSIGN ttart.ttfile = ttart.ttfile + "," + cImage.
                    END.
                END.
            END.
        END.
        
        IF cImage <> "" AND cImage <> ? THEN DO:
            IF cImage MATCHES ".jpg" OR cImage MATCHES ".eps" OR cImage MATCHES ".bmp" THEN DO:
                cImage = ?.
            END.
        END.
        
        IF cImage = "" THEN cImage = ?.
        IF cImage = ? THEN RUN ReportIssues(ttArt.ttItemseq,"MM-Art Image Issue" + reason,ttart.ttso,STRING(ttart.ttItemNO),"",ttart.ttFile,"","","","").
        IF deleteArt THEN DELETE ttArt.
    END.

END PROCEDURE.


PROCEDURE CheckMaterial:
    DEFINE INPUT  PARAMETER cSeq AS INT  NO-UNDO.
    DEFINE OUTPUT PARAMETER c_ok AS LOG  NO-UNDO INITIAL NO.
    DEFINE VARIABLE iQty          AS INT  NO-UNDO INITIAL 0.
    DEFINE VARIABLE cPart        AS CHAR NO-UNDO.

    FOR EACH b_ttart WHERE b_ttart.ttItemseq = cSeq:
        ASSIGN iQty  = iQty + b_ttart.ttqty
               cPart = b_ttart.ttinvpart.
    END.
    FIND ttMat WHERE ttMat.ttPart = cPart NO-ERROR.
    IF AVAIL ttMat AND (ttmat.ttQty - iQty > - 1) THEN DO:
        ASSIGN ttmat.ttqty = ttmat.ttqty - iQty
                      c_ok = YES.
    END.
    ELSE ASSIGN c_ok = NO.

END PROCEDURE.


PROCEDURE Checks:
    DEFINE INPUT PARAMETER cSo   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cItem AS INT  NO-UNDO.
    DEFINE OUTPUT PARAMETER c_ok AS LOG  NO-UNDO INITIAL NO.

    DEFINE VARIABLE holdit       AS LOG  NO-UNDO INITIAL NO.
    DEFINE VARIABLE tmpHours     AS DEC  NO-UNDO.
    DEFINE VARIABLE foundHDR     AS LOG  NO-UNDO.
    DEFINE VARIABLE activities   AS CHAR NO-UNDO.
    DEFINE VARIABLE foundDig     AS LOG  NO-UNDO.
    DEFINE VARIABLE tmpBatch     AS CHAR NO-UNDO.
    DEFINE VARIABLE tmpQty       AS INT  NO-UNDO.
    DEFINE VARIABLE tmpint       AS INT  NO-UNDO.
    DEFINE VARIABLE totalHours   AS DEC  NO-UNDO.
    DEFINE VARIABLE totalDays    AS DEC  NO-UNDO.
    DEFINE VARIABLE startDate    AS DATE NO-UNDO.
    DEFINE VARIABLE isPoly       AS LOG  NO-UNDO.
    DEFINE VARIABLE qtyRan       AS INT  NO-UNDO.
    DEFINE VARIABLE qtyNeeded    AS INT  NO-UNDO.
    DEFINE VARIABLE inQueue      AS LOG  NO-UNDO.

    FIND so_items NO-LOCK WHERE so_items.so_no = cSo AND so_items.ITEM_no = cITem NO-ERROR.
    FIND so_file NO-LOCK WHERE so_file.so_no = cSo NO-ERROR.
    IF NOT AVAIL so_items OR NOT AVAIL so_file THEN NEXT.
     

    IF so_file.hold <> "" THEN DO: 
        /*cant print any records with a hold on it. Example: PENDCR or CUSTHOLD*/
        RUN reportIssues(so_items.itemseq,"MM-Checks: Hold",so_items.so_no,STRING(so_items.ITEM_no),"",so_file.hold,"","","","").
    END.

    /*exclude parts on exclusion list - these can be anything from screen printed parts to special charge line items such as 90SC*/
    IF CAN-FIND(FIRST zz_file NO-LOCK WHERE zz_file.zz_key1 = "MM-ExcludePart" AND zz_file.zz_key2 = so_items.part_no) THEN DO: 
        RUN reportIssues(so_items.itemseq,"MM-Checks: Not Running Through JM",so_items.so_no,STRING(so_items.ITEM_no),"","Excluded Part","","","","").
    END.

   /*agent_sart is a next day rider/item*/ /*this check evaluates all digitally printed options and if it doesnt meet any of the options it removes it, has to be sart/dart/stock/custom to print*/
    IF NOT CAN-FIND(FIRST pt_det WHERE pt_det.part_no = so_items.part_no AND pt_det.agent_sart = TRUE) AND NOT CAN-FIND(FIRST pt_det WHERE pt_det.part_no = so_items.part_no AND pt_det.dart_item = TRUE) THEN DO:
        IF NOT CAN-FIND(FIRST so_art WHERE so_art.itemseq = so_items.itemseq AND so_art.TYPE = "Mini") THEN DO:
            IF NOT CAN-FIND(FIRST pt_det WHERE pt_det.part_no = so_items.part_no AND pt_det.stk_rider = TRUE) THEN DO: 
                IF NOT CAN-FIND(FIRST pt_det WHERE pt_det.part_no = so_items.part_no AND pt_det.zzlog_3 = TRUE) THEN DO: /*check to see if stockfile is selected - zz_log3 means stock file*/
                    RUN reportIssues(so_items.itemseq,"MM-Checks: Not Sart/Dart/Stock/Custom",so_items.so_no,STRING(so_items.ITEM_no),"","","","","","").
                END.
            END.
        END.
    END.

    /*make sure print digital and all prior activities have been completed*/
    c_ok       = NO. 
    totalHours = 0.
    isPoly     = YES.
    FOR EACH squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = so_items.itemseq AND squ_ptdet.TYPE <> "Frame",
        EACH squ_act NO-LOCK OF squ_ptdet BY squ_act.order:
        IF squ_act.ActSeq = 8 THEN ASSIGN c_ok = YES. /*actseq = 8 is printing digital*/
        IF NOT CAN-DO(activities,STRING(squ_act.actseq)) AND c_ok = NO THEN /*anything before the printing digital activity*/
            ASSIGN activities = activities + (IF activities = "" THEN "" ELSE ",") + string(squ_act.actseq).

        IF INDEX(squ_ptdet.pt_substrate,"poly") > 0 THEN isPoly = YES. /*might be bug because isPoly will never be no*/
        totalHours = totalHours + squ_act.hours[1].
    END.
    IF isPoly THEN totalHours = totalHours + 12.
    IF c_ok = YES AND NUM-ENTRIES(activities,",") > 0 THEN DO: /*activities is all the actseq's that happen before printing digital*/
        FIND FIRST actlist NO-LOCK WHERE actlist.actseq = INT(ENTRY(NUM-ENTRIES(activities),activities)) NO-ERROR.
        IF AVAIL actlist THEN DO:
            IF NOT CAN-FIND(FIRST h_detail NO-LOCK WHERE H_detail.activity = actlist.labor_type AND H_detail.item_no = string(so_items.ITEM_no) 
                AND H_detail.order_no = so_items.so_no AND H_detail.zzlog_1 = TRUE) THEN c_ok = NO. /*if can't find completed record for last activity before digital print then next*/
        END.
    END.
    IF c_ok = NO THEN DO:
        RUN reportIssues(so_items.itemseq,"MM-Checks: Not Completed Prior Activity",so_items.so_no,STRING(so_items.ITEM_no),"",IF AVAIL actlist THEN ActList.ActDesc ELSE "","","","","").
    END.
    c_ok = NO.

    /* is it close enough to firm date to run? */
    IF so_file.firm THEN DO:
        /* How many days worth of time do we still have to do? */
        ASSIGN totalDays = 2 + IF totalHours / 8 = int(totalHours / 8) THEN totalHours / 8
                           ELSE TRUNCATE(totalHours / 8,0) + 1
               startDate = so_file.ship_by.
        DO WHILE totalDays > 0:
            startDate = startDate - 1.
            IF NOT CAN-DO("1,7",STRING(WEEKDAY(startDate)))
            AND NOT CAN-FIND(holiday WHERE holiday.hol_date = startDate)
            THEN totalDays = totalDays - 1.
        END.

        IF startDate > TODAY THEN DO: /*if the ship is too far out, we dont need to print yet and we can wait*/
            RUN reportIssues(so_items.itemseq,"MM-Checks: Firm Date",so_items.so_no,STRING(so_items.ITEM_no),"","Too far out to run","","","","").
        END.
    END.
   
/*     /*see if its already printed*/                                                                           */
    RUN getQty(so_items.itemseq,OUTPUT qtyRan,OUTPUT qtyNeeded, OUTPUT inQueue).
    IF qtyNeeded <= 0 THEN DO:
        IF inQueue THEN
            RUN reportIssues(so_items.itemseq,"MM-Checks: Queue",so_items.so_no,STRING(so_items.ITEM_no),"","","","","","").
        ELSE
            RUN reportIssues(so_items.itemseq,"MM-Checks: Already Printed Via JM",so_items.so_no,STRING(so_items.ITEM_no),"","","","","","").
    END.
         
    /*if manually printed*/
    IF CAN-FIND(FIRST h_detail NO-LOCK WHERE h_detail.order_no = so_items.so_no AND h_detail.ITEM_no = string(so_items.ITEM_no) AND h_detail.activity = "D11" AND h_detail.batchseq = "") THEN DO: 
        RUN reportIssues(so_items.itemseq,"MM-Checks: Already Printed",so_items.so_no,STRING(so_items.ITEM_no),"","","","","","").
    END.

    /*if activity already marked complete*/
    IF CAN-FIND(FIRST h_detail NO-LOCK WHERE h_detail.order_no = so_items.so_no AND h_detail.ITEM_no = STRING(so_items.ITEM_no) AND h_detail.activity = "D11" AND h_detail.zzLog_1 = YES) THEN DO:
        IF NOT CAN-FIND(FIRST sign_mm_reprint NO-LOCK WHERE sign_mm_reprint.itemseq = so_items.itemseq AND sign_mm_reprint.completed = FALSE) THEN DO:
            RUN reportIssues(so_items.itemseq,"MM-Checks: Already Printed Via JM",so_items.so_no,STRING(so_items.ITEM_no),"","","","","","").
        END.
    END.

    /* fail safe - if it makes it here and has already shipped then we don't want to reprint it*/
    IF CAN-FIND(FIRST h_detail NO-LOCK WHERE h_detail.order_no = so_items.so_no AND h_detail.ITEM_no = STRING(so_items.ITEM_no) AND h_detail.activity = "S2")
        OR CAN-FIND(FIRST h_detail NO-LOCK WHERE h_detail.order_no = so_items.so_no AND h_detail.ITEM_no = STRING(so_items.ITEM_no) AND h_detail.activity = "S3") THEN DO:
            RUN reportIssues(so_items.itemseq,"MM-Checks: Already Printed",so_items.so_no,STRING(so_items.ITEM_no),"","","","","","").
    END.
    

    c_ok = YES.
    IF AVAIL sign_mm_det THEN RELEASE sign_mm_det.
    IF AVAIL sign_mm_hdr THEN RELEASE sign_mm_hdr.

END PROCEDURE.


PROCEDURE CheckDynamicNestCompatability:
    DEFINE INPUT  PARAMETER inventoryPart     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER substrateType     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER substrateMaterial AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER isCompatible      AS LOGICAL   NO-UNDO INITIAL FALSE.

    IF substrateType <> "Poly" THEN RETURN. /*currently only poly is completely ready for dynamic nest*/
    
    FOR EACH tParams NO-LOCK WHERE tParams.substrate = substrateMaterial:
        IF INDEX(tParams.InvPart, inventoryPart) > 0  OR 
            index(inventoryPart, tParams.InvPart) > 0 THEN DO:
            isCompatible = TRUE.
            RETURN.
        END.
    END.
    
END PROCEDURE.


PROCEDURE CheckTemplates:
    DEFINE VARIABLE tmpcmd          AS CHAR         NO-UNDO.
    DEFINE VARIABLE tempSum         AS DEC          NO-UNDO.
    DEFINE VARIABLE xmldata         AS CHAR         NO-UNDO.
    DEFINE VARIABLE templateCnt     AS INT          NO-UNDO.
    DEFINE VARIABLE cTemplate       AS CHAR         NO-UNDO.
    DEFINE VARIABLE fname           AS CHAR         NO-UNDO.
    DEFINE VARIABLE cResponse       AS CHAR         NO-UNDO.
    DEFINE VARIABLE chDart          AS COM-HANDLE   NO-UNDO.
    DEFINE VARIABLE svXMLDATA       AS CHAR         NO-UNDO.
    DEFINE VARIABLE TemplateLoc     AS CHAR         NO-UNDO. 
    
    TemplateLoc = STRING(networkShare + "signart\DigitalBedTemplates\").
     

/*     FOR EACH signbed NO-LOCK WHERE signBed.MatrlType <> "Corex": */
    FOR EACH signbed NO-LOCK WHERE INDEX(signBed.MatrlType,"Steel") > 0:
        tempSum = 0.
        xmldata = "".
        FOR EACH signbeddet NO-LOCK WHERE signbeddet.seq = signbed.seq:
            tempSum = tempSum + Signbeddet.userdec2 + Signbeddet.userdec1.
        END.

        /*find, delete, recreate*/
        ASSIGN cTemplate = STRING(signbed.seq).
        IF LENGTH(cTemplate, "character") = 1 THEN ASSIGN cTemplate = "bed-0" + cTemplate + ".pdf".
        ELSE ASSIGN cTemplate = "bed-" + cTemplate + ".pdf".

        ASSIGN cTemplate = TemplateLoc + cTemplate.
        FIND mm_file WHERE mm_file.zz_key1 = "MM-Templates" AND mm_file.zz_key2 = string(signbed.seq) NO-ERROR.
        IF AVAIL mm_file AND mm_file.zz_dec[1] = DEC(STRING(tempSum,"->>,>>>.99")) AND SEARCH(cTemplate) <> ? THEN DO:
            /*nothing*/
        END.

        IF AVAILABLE mm_file THEN RELEASE mm_file.
    END.
END PROCEDURE.


PROCEDURE ClientApp:
    DEFINE INPUT PARAMETER xmldata    AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cResponse AS CHAR NO-UNDO.
    DEFINE VARIABLE chDart          AS COM-HANDLE   NO-UNDO.

    IF OS-GETENV("computername") <> "qbprod" AND OS-GETENV("computername") <> "qbtest" THEN
        CREATE "ClientApp.SendXML" chDart.
    ELSE 
        CREATE "ClientApp.SendXML" chDart.
    ASSIGN cResponse = chDart:Adhere(INPUT-OUTPUT xmlData).
    RELEASE OBJECT chDart NO-ERROR.
    ASSIGN chDart = ?.
END PROCEDURE.


PROCEDURE CompleteReprint:
    DEFINE INPUT PARAMETER pBatchseq AS INTEGER NO-UNDO.
    DEFINE VARIABLE pCompleted AS LOGICAL NO-UNDO.
    
    FOR EACH buf_mm_det NO-LOCK WHERE buf_mm_det.batchseq = pBatchseq BREAK BY buf_mm_det.ReprintId:   
        IF LAST-OF(buf_mm_det.ReprintId) AND buf_mm_det.ReprintId > 0 THEN DO:
            pCompleted = TRUE.
            FIND sign_mm_reprint NO-LOCK WHERE sign_mm_reprint.reprintid = buf_mm_det.reprintid AND sign_mm_reprint.delayed = TRUE NO-ERROR.
            IF AVAIL sign_mm_reprint THEN DO:
               FOR EACH b_mm_det NO-LOCK WHERE b_mm_det.reprintId = buf_mm_det.reprintid BREAK BY b_mm_det.batchseq:
                   IF LAST-OF(b_mm_det.batchseq) AND b_mm_det.batchseq <> pBatchseq THEN DO:
                       IF CAN-FIND(b_mm_hdr NO-LOCK WHERE b_mm_hdr.batchseq = b_mm_det.batchseq AND b_mm_hdr.run_time = ?) THEN DO:
                           pCompleted = FALSE.
                       END.
                   END.
               END.
            END.
            IF pCompleted THEN DO:
                FIND sign_mm_reprint EXCLUSIVE-LOCK WHERE sign_mm_reprint.reprintid = buf_mm_det.reprintid NO-ERROR.
                IF AVAILABLE sign_mm_reprint THEN DO:
                    ASSIGN sign_mm_reprint.completed = TRUE.
                END.
            END.
        END.
    END.
    IF AVAILABLE sign_mm_reprint THEN RELEASE sign_mm_reprint.
    
END PROCEDURE.


PROCEDURE CorexHW:
    DEFINE INPUT  PARAMETER pHeight   AS DEC NO-UNDO.
    DEFINE INPUT  PARAMETER pWidth    AS DEC NO-UNDO.
    DEFINE INPUT  PARAMETER pPH       AS DEC NO-UNDO.
    DEFINE INPUT  PARAMETER pPW       AS DEC NO-UNDO.
    DEFINE INPUT  PARAMETER pVertF    AS LOG NO-UNDO.
    DEFINE INPUT  PARAMETER pHorzF    AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER pTurning  AS LOG NO-UNDO INITIAL FALSE.
    DEFINE OUTPUT PARAMETER pSwitched AS LOG NO-UNDO INITIAL FALSE.
    
    IF pVertF THEN pTurning = TRUE.
    IF pPH = pWidth AND pPW = pHeight THEN pSwitched = TRUE.
    
END PROCEDURE.


PROCEDURE CreateAndSetReprintData:
DEFINE VARIABLE NewReprintSeq AS INTEGER NO-UNDO.

    FOR EACH ttArt:
        ASSIGN newReprintseq = NEXT-VALUE(signReprintSeq).
        DO WHILE CAN-FIND(b_mm_reprint WHERE reprintid = newReprintseq):
            ASSIGN newReprintseq = NEXT-VALUE(signReprintSeq).
        END.
        
        CREATE sign_mm_reprint.
        ASSIGN sign_mm_reprint.Itemseq    = ttArt.ttItemseq 
               sign_mm_reprint.so_no      = ttArt.ttso
               sign_mm_reprint.item_no    = ttArt.ttItemNo
               sign_mm_reprint.Delayed    = FALSE
               sign_mm_reprint.Qty        = ttArt.ttQty
               sign_mm_reprint.CreateDate = DATETIME(TODAY,MTIME)
               sign_mm_reprint.Artfile    = ttArt.ttFile
               sign_mm_reprint.ReasonCode = ttArt.ttReasonCode
               sign_mm_reprint.ReprintId  = newReprintseq.
               
        ttArt.ttReprintId = sign_mm_reprint.ReprintId.
    END.
    IF AVAILABLE sign_mm_reprint THEN RELEASE sign_mm_reprint.

END PROCEDURE.


PROCEDURE DynamicNest:
    DEFINE INPUT PARAMETER cItemseq AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE pLoop        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE switch       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE nextSeq      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE pCnt         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE pSQIN        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE INVpart      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hotFolderseq AS INTEGER   NO-UNDO.
    DEFINE VARIABLE nestCnt      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE turning      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE switched     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cType        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMaterial    AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE TemplateCnt  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE AllSameSize  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE DynamicSize  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTemplate    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE Blah         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE BlahInt      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE isprecut     AS LOGICAL  NO-UNDO INITIAL FALSE.
    
    DEFINE BUFFER bParams FOR tParams.
    DEFINE BUFFER b_tNest FOR tNest.
    DEFINE BUFFER nestArt FOR ttArt.
    

     /*first make sure you have all the info you need...occurs in savedown saves sizes to squ_ptdet*/
    FOR EACH nestArt WHERE nestArt.ttDynamNest = YES BREAK BY nestArt.ttType BY nestArt.ttSides:
        IF nestArt.ttType = ? THEN NEXT.

        FIND tParams NO-LOCK WHERE tParams.substrate = nestArt.ttType NO-ERROR.
        FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = nestArt.ttItemseq AND squ_ptdet.TYPE <> "frame" NO-ERROR.
        IF AVAILABLE squ_ptdet THEN DO:
        
            RUN getSubstrate.p ("",nestArt.ttItemseq,OUTPUT cType,OUTPUT cMaterial).
            RUN savedown (YES, nestArt.ttItemseq,nestArt.ttFile,cType, STRING(squ_ptdet.pressPrintingHeight) + "x" + STRING(squ_ptdet.pressPrintingWidth), NO,NO).
        END.
        RELEASE squ_ptdet.
    END.
        
    /***************************/
    /*****Zund Replacement******/
    /***************************/ 
    EMPTY TEMP-TABLE tNest.
    FOR EACH ttart WHERE ttArt.ttDynamNest = YES BREAK BY ttArt.ttType  BY ttArt.ttInvPart BY ttArt.ttSides :
        IF INDEX(ttArt.ttType,"Corex") > 0 AND LOOKUP(STRING(ttArt.ttTempSeq),"28,29") = 0 THEN NEXT. /*Ryanle - added Corex filter for Prime Center Project*/
        IF ttArt.ttType = ? THEN NEXT.
        
        FIND FIRST tParams NO-LOCK WHERE tParams.substrate = ttArt.ttType  AND tParams.invpart = ttArt.ttinvpart NO-ERROR. 
        IF NOT AVAIL tParams THEN                                                                                          
        FIND FIRST tParams NO-LOCK WHERE tParams.substrate = ttArt.ttType  AND tParams.PRIMARY = TRUE NO-ERROR.
        FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = ttArt.ttItemseq AND squ_ptdet.TYPE <> "frame" NO-ERROR.
        IF AVAILABLE squ_ptdet THEN DO:
            
            DO pLoop = 1 TO ttArt.ttQty:
                turning = FALSE. switch = FALSE. switched = FALSE.
                IF INDEX(ttArt.ttType,"Corex") > 0 THEN DO: 
                    RUN corexHW (squ_ptdet.pt_height,squ_ptdet.pt_width,squ_ptdet.PressPrintingHeight,squ_ptdet.PressPrintingWidth,squ_ptdet.VERT_flutes,squ_ptdet.horz_flutes,OUTPUT turning, OUTPUT switched).
                    IF turning AND switched     THEN switch = FALSE.
                    IF turning AND NOT switched THEN switch = TRUE.
                END.
                ELSE switch = NO.                
                    
                CREATE tNest.
                ASSIGN tNest.itemseq   = ttArt.ttItemseq
                       tNest.actHeight = IF turning THEN squ_ptdet.ActualImgWidth ELSE squ_ptdet.ActualImgHeight
                       tNest.actWidth  = IF turning THEN squ_ptdet.ActualImgHeight ELSE squ_ptdet.ActualImgWidth
                       tNest.panelH    = IF NOT switch THEN squ_ptdet.PressPrintingHeight ELSE squ_ptdet.PressPrintingWidth             
                       tNest.panelW    = IF NOT switch THEN squ_ptdet.PressPrintingWidth  ELSE squ_ptdet.PressPrintingHeight
                       tNest.sqIn      = tNest.actHeight * tNest.actWidth
                       tNest.substrate = ttArt.ttType
                       tNest.Artfile   = ttArt.ttFile
                       tNest.hasBleed  = squ_ptdet.PressPrintingHeight < squ_ptdet.ActualImgHeight OR squ_ptdet.PressPrintingWidth < squ_ptdet.ActualImgWidth
                       tNest.template  = tParams.template.
                                                                       
            END.
        END.
        
        IF LAST-OF(ttArt.ttSides) OR LAST-OF(ttArt.ttType)  OR LAST-OF(ttArt.ttInvPart) THEN DO:

            RUN DynamicNest.p(INPUT-OUTPUT TABLE tNest,ttArt.ttType,""). /*might need a change here...dont want a diff setup for all reflectives... ex 24 ga steel reflective*/
            
            FOR EACH tNest BREAK BY tNest.bedid BY tNest.posX:
            
                FIND FIRST b_ttart NO-LOCK WHERE b_ttart.ttItemseq = tNest.itemseq AND b_ttart.ttFile = tNest.artfile NO-ERROR.
                IF AVAILABLE b_ttart THEN DO:
                
                    IF FIRST-OF(tNest.bedid) THEN DO:   
                        /*create the header*/
                        nextSeq = NEXT-VALUE(seq-mm-batch).
                        DO WHILE CAN-FIND(sign_mm_hdr WHERE sign_mm_hdr.batchseq = nextSeq):
                            nextSeq = NEXT-VALUE(seq-mm-batch).
                        END.

                        CREATE sign_mm_hdr.
                        ASSIGN sign_mm_hdr.BATCHseq         = nextseq
                               sign_mm_hdr.runseq           = 0
                               sign_mm_hdr.crt_date         = TODAY
                               sign_mm_hdr.crt_time         = TIME
                               sign_mm_hdr.RUN_date         = ?
                               sign_mm_hdr.RUN_time         = ?
                               sign_mm_hdr.matlType         = b_ttArt.ttType
                               sign_mm_hdr.sides            = b_ttArt.ttsides
                               sign_mm_hdr.bedseq           = 0
                               sign_mm_hdr.PointerSeq       = 0
                               sign_mm_hdr.inv_part         = (IF tNest.INVpart <> "" THEN tNest.INVpart ELSE b_ttart.ttInvPart)
                               sign_mm_hdr.qty              = 1
                               sign_mm_hdr.rerun            = IF cItemseq <> "" THEN YES ELSE NO
                               sign_mm_hdr.reprint          = IF cItemseq <> "" THEN YES ELSE NO
                               /*sign_mm_hdr.fullbed          = TRUE /*full bed*/*/
                               sign_mm_hdr.pt_hotfolderseq  = b_ttArt.tthotfolder
                               sign_mm_hdr.dynamicTemplate  = tNest.template
                               pCnt                         = 1
                               pSQIN                        = 0
                               DynamicSize                  = STRING(tNest.panelH) + "x" + string(tNest.panelW)
                               AllSameSize                  = YES. 
                    END.

                    /*create the details*/
                    /*determine sign_mm_det.artlinkseq*/
                    FIND FIRST so_art NO-LOCK WHERE so_art.itemseq = b_ttart.ttitemseq AND so_art.artfile = ENTRY(1,b_ttart.ttfile,",") NO-ERROR.
                    
                    CREATE sign_mm_det.
                    ASSIGN sign_mm_det.batchseq        = nextseq
                           sign_mm_det.part_no         = b_ttArt.ttPart
                           sign_mm_det.itemseq         = b_ttArt.ttitemseq
                           sign_mm_det.artlinkseq      = IF AVAIL so_art THEN so_art.disp_order ELSE 0
                           sign_mm_det.artfile         = b_ttArt.ttFile
                           sign_mm_det.inv_part        = b_ttArt.ttInvPart
                           sign_mm_det.POSITION        = pCnt
                           sign_mm_det.due_date        = b_ttArt.ttDue
                           sign_mm_det.pt_hotfolderseq = b_ttArt.ttHotfolder
                           sign_mm_det.PointerSeq      = sign_mm_hdr.bedseq
                           sign_mm_det.zzlog_1         = IF b_ttArt.ttCustNo = "53550" THEN YES ELSE NO
                           sign_mm_det.posx            = tNest.posX
                           sign_mm_det.posy            = tNest.posY
                           sign_mm_det.posxback        = tNest.posXback
                           sign_mm_det.posyback        = tNest.posYback
                           sign_mm_det.switch          = tNest.rotated
                           sign_mm_det.reprintID       = ttArt.ttReprintID
                           pCnt                        = pCnt + 1
                           pSQIN                       = pSQIN + tNest.sqIn.
                         
                    RELEASE so_art NO-ERROR.  
                    
                    IF STRING(tNest.panelH) + "x" + string(tNest.panelW) <> DynamicSize THEN AllSameSize = FALSE.
                           
                    IF LAST-OF(tNest.bedid) THEN DO:
                        IF INDEX(sign_mm_hdr.matltype,"Corex") > 0 THEN DO:
                            IF AllSameSize THEN DO:
                                TemplateCnt = 0. cTemplate = 0.
                                RUN getSubstrate.p ("",nestArt.ttItemseq,OUTPUT cType,OUTPUT cMaterial).
                                RUN checkprecut.p(sign_mm_det.itemseq, OUTPUT isprecut).
                                RUN getTemplate((IF isPrecut THEN "Steel" ELSE cType),sign_mm_det.part_no,tnest.panelH,tnest.panelW,NO,OUTPUT cTemplate, OUTPUT Blah, OUTPUT BlahInt).
                               
                                IF cTemplate > 0 THEN DO:
                                    FIND signbed NO-LOCK WHERE signbed.seq = cTemplate NO-ERROR.
                                    IF AVAILABLE signbed THEN DO:
                                        TemplateCnt = signbed.userdec1 * signbed.userdec2.
                                        IF pCnt = TemplateCnt THEN
                                            ASSIGN sign_mm_hdr.bed_eff = 100
                                                   sign_mm_hdr.fullbed = YES.
                                    END.
                                END.
                            END.
                            
                            IF sign_mm_hdr.bed_eff = 0 THEN DO:
                                ASSIGN sign_mm_hdr.bed_eff = pSQIN / 4608 * 100
                                       sign_mm_hdr.fullbed = IF sign_mm_hdr.bed_eff >= 65 THEN YES ELSE NO.
                            END.
                        END.
                        ELSE IF INDEX(sign_mm_hdr.matltype,"omegabond") > 0 OR INDEX(sign_mm_hdr.matltype,"Alumalite") > 0 OR INDEX(sign_mm_hdr.matltype,"Corex") > 0 THEN DO:
                            ASSIGN sign_mm_hdr.bed_eff = pSQIN / 4608 * 100
                                   sign_mm_hdr.fullbed = IF sign_mm_hdr.bed_eff >= 65 THEN YES ELSE NO.
                        END.
                        ELSE IF INDEX(sign_mm_hdr.matltype,"magnetic") > 0 OR INDEX(sign_mm_hdr.matltype,"Decal") > 0 THEN DO:
                            ASSIGN sign_mm_hdr.bed_eff = pSQIN / 4536 * 100
                                   sign_mm_hdr.fullbed = YES.
                            
                        END.
                    END.
                    RELEASE sign_mm_det.
                END.
            END.
            EMPTY TEMP-TABLE tNest.
            IF AVAIL sign_mm_hdr THEN RELEASE sign_mm_hdr.
        END.
    END.
    
    RELEASE squ_ptdet.
    /***************************/
    /********Multi Batch********/
    /***************************/
    /*IF EnableMultiBatching THEN DO:*/
        FIND tParams NO-LOCK WHERE tParams.substrate = "Steel 24ga" NO-ERROR. /*find steel bed because steel is setup to use the whole flatbed printer's bed size. */
        IF AVAILABLE tParams THEN DO:
            FOR EACH bParams NO-LOCK WHERE (tParams.bedHeight / bParams.bedHeight) >= 2 OR (tParams.bedWidth / bParams.bedWidth) >= 2: /*Now find other materials that can fit 2 or more full sheets on the flatbed printers total bed size*/
                /*reprints will send in 12345, we only want to do this on reprints for similar items to prevent batch deletion - jacksonw 03/19/18*/
                IF cItemseq = "12345" AND cMaterial <> bParams.substrate THEN NEXT.
                
                FOR EACH pt_hotfolder NO-LOCK WHERE pt_hotfolder.matlType BEGINS bParams.substrate:
                    DO pLoop = 1 TO 2: /*1 to 2 makes sure we get both sides */
                        EMPTY TEMP-TABLE tNest.
                        FOR EACH sign_mm_hdr WHERE sign_mm_hdr.matltype = bParams.substrate  AND 
                                                   sign_mm_hdr.pt_hotfolderseq = pt_hotfolder.pt_hotfolderseq AND
                                                   sign_mm_hdr.dynamicTemplate = bParams.template AND
                                                   sign_mm_hdr.run_date = ? AND 
                                                   sign_mm_hdr.run_time = ? AND 
                                                   sign_mm_hdr.sides = pLoop AND
                                                   sign_mm_hdr.bedseq = 0   AND
                                                   sign_mm_hdr.fbMachine = 0: /*find records that can be multibatched */
                            DO iLoop = 1 TO sign_mm_hdr.qty:

                                CREATE tNest. /*create the nest records, these are the records that control what gets batched together*/
                                ASSIGN tNest.itemseq   = sign_mm_hdr.batchseq
                                       tNest.actHeight = bParams.bedHeight
                                       tNest.actWidth  = bParams.bedWidth
                                       tNest.panelH    = bParams.bedHeight
                                       tNest.panelW    = bParams.bedWidth
                                       tNest.sqIn      = tNest.panelH * tNest.panelW
                                       tNest.substrate = sign_mm_hdr.matltype
                                       tNest.template  = sign_mm_hdr.DynamicTemplate.                                   
                            END.
                        END.
                        nestCnt = 0.
                        FOR EACH tNest:
                            nestCnt = nestCnt + 1.
                        END.
/*                         IF nestCnt > 1 THEN DO: /*don't waste time running if there is only one bed - *I commented this out because we want all poly sheets to hit the steel bed params now so the i-dots all get added *jacksonw*/ */
                            RUN DynamicNest.p(INPUT-OUTPUT TABLE tNest,tParams.substrate,bParams.substrate).                           
        
                            /*determine if these are batch nested...if so explode else leave as single batch*/
                            FOR EACH tNest BREAK BY tNest.bedid BY tNest.posX:
/*                                 IF NOT CAN-FIND(FIRST b_tNest WHERE b_tNest.bedid = tNest.bedid AND b_tNest.itemseq <> tNest.itemseq) THEN NEXT. /*leave as single batch - *commented out because we want single batches to go to steel beds for i dot reasons*jacksonw*/ */
                                FIND FIRST b_mm_hdr WHERE b_mm_hdr.batchseq = tNest.itemseq NO-ERROR.
                                IF AVAILABLE b_mm_hdr THEN DO:
                                    IF FIRST-OF(tNest.bedid) THEN DO:
                                        /*create the header*/
                                        nextSeq = NEXT-VALUE(seq-mm-batch).
                                        DO WHILE CAN-FIND(sign_mm_hdr WHERE sign_mm_hdr.batchseq = nextSeq):
                                            nextSeq = NEXT-VALUE(seq-mm-batch).
                                        END.
        
                                        CREATE sign_mm_hdr.
                                        ASSIGN sign_mm_hdr.BATCHseq     = nextSeq
                                               sign_mm_hdr.BatchNested  = TRUE
                                               sign_mm_hdr.fullbed      = TRUE
                                               sign_mm_hdr.zzlog_3      = TRUE
                                               pCnt                     = 1.
                                         
                                         BUFFER-COPY b_mm_hdr EXCEPT b_mm_hdr.batchseq b_mm_hdr.BatchNested b_mm_hdr.fullbed b_mm_hdr.zzchar_2 TO sign_mm_hdr.
                                     
                                    END.
                                    /*create the details*/
                                    CREATE nest_mm_hdr.
                                    BUFFER-COPY b_mm_hdr TO nest_mm_hdr.
                                    ASSIGN nest_mm_hdr.NestBatchId = sign_mm_hdr.batchseq
                                           nest_mm_hdr.posX             = tNest.posX
                                           nest_mm_hdr.posy             = tNest.posY
                                           nest_mm_hdr.posxback         = tNest.posXback
                                           nest_mm_hdr.posyback         = tNest.posYback
                                           nest_mm_hdr.switch           = tNest.rotated
                                           nest_mm_hdr.DynamicTemplate  = tNest.template.
        
                                    FOR EACH b_mm_det WHERE b_mm_det.batchseq = b_mm_hdr.batchseq:
        
                                        CREATE nest_mm_det.
                                        BUFFER-COPY b_mm_det TO nest_mm_det.
        
                                        CREATE sign_mm_det.
                                        BUFFER-COPY b_mm_det EXCEPT b_mm_det.batchseq b_mm_det.POSITION TO sign_mm_det.
                                        ASSIGN sign_mm_det.batchseq = sign_mm_hdr.batchseq
                                               sign_mm_det.POSITION = pCnt
                                               pCnt                 = pCnt + 1.

                                        DELETE b_mm_det.
                                    END.
                                    DELETE b_mm_hdr.
                                END.
                            END.
/*                         END. */
                    END.
                END.
            END.
        END.
    /*END.*/
    
    /***************************/
    /*******Nest Partials*******/
    /***************************
    IF lPartialRebatch THEN DO:
        FOR EACH bParams NO-LOCK:
            FOR EACH pt_hotfolder NO-LOCK WHERE pt_hotfolder.matlType BEGINS bParams.substrate: /*ADDED 5/25/17 - TBP*/
                EMPTY TEMP-TABLE ttArt.
                /*only care about corex at the moment*/
                IF INDEX(bParams.substrate,"corex") = 0 THEN NEXT.
                DO pLoop = 1 TO 2:
                    EMPTY TEMP-TABLE tNest.
                    FOR EACH sign_mm_hdr WHERE sign_mm_hdr.matltype = bParams.substrate AND sign_mm_hdr.pt_hotfolderseq = pt_hotfolder.pt_hotfolderseq AND sign_mm_hdr.run_date = ? AND sign_mm_hdr.run_time = ? AND sign_mm_hdr.fullbed = FALSE AND sign_mm_hdr.sides = pLoop:
                        IF sign_mm_hdr.reprint THEN NEXT. /*Don't touch reprints*/
        
                        ASSIGN INVpart      = sign_mm_hdr.inv_part
                               hotFolderseq = sign_mm_hdr.pt_hotfolderseq.
        
                        FOR EACH sign_mm_det OF sign_mm_hdr:
                            FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = sign_mm_det.Itemseq AND squ_ptdet.TYPE <> "frame" NO-ERROR.
                            IF AVAILABLE squ_ptdet THEN DO:
                                turning = FALSE. switch = FALSE. switched = FALSE.
                                IF INDEX(ttArt.ttType,"Corex") > 0 THEN DO:
                                    RUN corexHW (squ_ptdet.pt_height,squ_ptdet.pt_width,squ_ptdet.PressPrintingHeight,squ_ptdet.PressPrintingWidth,squ_ptdet.VERT_flutes,squ_ptdet.horz_flutes,OUTPUT turning, OUTPUT switched).
                                    IF turning AND switched     THEN switch = FALSE.
                                    IF turning AND NOT switched THEN switch = TRUE.
                                END.
                                ELSE switch = NO.
        
                                /*IF EnableDynCorexRebatch OR CAN-FIND(FIRST zz_file NO-LOCK WHERE zz_file.zz_key1 = "MM-COREX2" AND zz_file.zz_key2 = squ_ptdet.part_no) THEN DO:
                                    CREATE tNest.
                                    ASSIGN tNest.itemseq   = squ_ptdet.itemseq
                                           tNest.actHeight = IF NOT turning THEN squ_ptdet.ActualImgHeight ELSE squ_ptdet.ActualImgWidth
                                           tNest.actWidth  = IF NOT turning THEN squ_ptdet.ActualImgWidth  ELSE squ_ptdet.ActualImgHeight
                                           tNest.panelH    = IF NOT switch THEN squ_ptdet.PressPrintingHeight ELSE squ_ptdet.PressPrintingWidth
                                           tNest.panelW    = IF NOT switch THEN squ_ptdet.PressPrintingWidth  ELSE squ_ptdet.PressPrintingHeight
                                           tNest.sqIn      = tNest.panelH * tNest.panelW
                                           tNest.substrate = sign_mm_hdr.matltype
                                           tNest.Artfile   = ttArt.ttFile.
                                END.
                                ELSE DO:*/
                                    IF NOT CAN-FIND(FIRST RedoItems NO-LOCK WHERE RedoItems.itemseq = sign_mm_det.itemseq) THEN DO:
                                        CREATE RedoItems.
                                        ASSIGN RedoItems.itemseq = sign_mm_det.itemseq.
                                    END.
                                /*END.*/
        
                            END.
                            IF NOT CAN-FIND(FIRST tempDet WHERE tempDet.itemseq = sign_mm_det.itemseq) THEN DO:
                                CREATE tempDet.
                                BUFFER-COPY sign_mm_det TO tempDet.
                            END.
                            DELETE sign_mm_det.
                        END.
                        DELETE sign_mm_hdr.
                    END.
        
                    RUN DynamicNest.p(INPUT-OUTPUT TABLE tNest,bParams.substrate,"").
        
                    FOR EACH tNest BREAK BY tNest.bedid BY tNest.posX:
                        FIND FIRST tempDet NO-LOCK WHERE tempDet.itemseq = tNest.itemseq AND tempDet.artfile = tNest.artfile NO-ERROR.
                        IF AVAILABLE tempDet THEN DO:
        
                            IF FIRST-OF(tNest.bedid) THEN DO:
                                /*create the header*/
                                nextSeq = NEXT-VALUE(seq-mm-batch).
                                DO WHILE CAN-FIND(sign_mm_hdr WHERE sign_mm_hdr.batchseq = nextSeq):
                                    nextSeq = NEXT-VALUE(seq-mm-batch).
                                END.
        
                                CREATE sign_mm_hdr.
                                ASSIGN sign_mm_hdr.BATCHseq         = nextseq
                                       sign_mm_hdr.runseq           = 0
                                       sign_mm_hdr.crt_date         = TODAY
                                       sign_mm_hdr.crt_time         = TIME
                                       sign_mm_hdr.RUN_date         = ?
                                       sign_mm_hdr.RUN_time         = ?
                                       sign_mm_hdr.matlType         = bParams.substrate
                                       sign_mm_hdr.sides            = pLoop
                                       sign_mm_hdr.bedseq           = 0
                                       sign_mm_hdr.PointerSeq       = 0
                                       sign_mm_hdr.inv_part         = INVpart
                                       sign_mm_hdr.qty              = 1
                                       sign_mm_hdr.rerun            = IF cItemseq <> "" THEN YES ELSE NO
                                       sign_mm_hdr.reprint          = IF cItemseq <> "" THEN YES ELSE NO
                                       /*sign_mm_hdr.fullbed          = TRUE /*full bed*/*/
                                       sign_mm_hdr.pt_hotfolderseq  = hotFolderseq
                                       sign_mm_hdr.zzlog_3          = TRUE
                                       pCnt                         = 1
                                       pSQIN                        = 0.
                            END.
        
                            /*create the details*/
                            CREATE sign_mm_det.
                            BUFFER-COPY tempDet EXCEPT tempDet.batchseq tempDet.POSITION TO sign_mm_det.
        
                            ASSIGN sign_mm_det.batchseq        = nextSeq
                                   sign_mm_det.POSITION        = pCnt
                                   sign_mm_det.due_date        = b_ttart.ttDue
                                   sign_mm_det.posx            = tNest.posX
                                   sign_mm_det.posy            = tNest.posY
                                   sign_mm_det.posxback        = tNest.posXback
                                   sign_mm_det.posyback        = tNest.posYback
                                   sign_mm_det.switch          = tNest.rotated
                                   pCnt                        = pCnt + 1
                                   pSQIN                       = pSQIN + tNest.sqIn.
        
                            IF LAST-OF(tNest.bedid) THEN DO:
                                ASSIGN sign_mm_hdr.bed_eff = pSQIN / 4608 * 100
                                       sign_mm_hdr.fullbed = IF sign_mm_hdr.bed_eff >= 65 THEN YES ELSE NO.
                            END.
                        END.
                    END.
        
                    /*NOW SEND THE REGULAR ITEMS BACK THROUGH THE JM PROCESS TO RE-TEMPLATE NEST THEM*/
                    FOR EACH RedoItems:
                        RUN TemplateNest(RedoItems.itemseq).
                    END.
                END.
            END.
        END.
    END.
    */
    RELEASE squ_ptdet.
END PROCEDURE.


PROCEDURE Email:
    DEFINE VARIABLE xMsg            AS CHAR   NO-UNDO.
    DEFINE VARIABLE eCnt            AS INT    NO-UNDO.
    DEFINE VARIABLE tmpchar         AS CHAR   NO-UNDO.
    DEFINE VARIABLE tmpissue        AS CHAR   NO-UNDO.
    DEFINE VARIABLE tmpint          AS INT    NO-UNDO.
    DEFINE VARIABLE tmpRack         AS CHAR   NO-UNDO.
    DEFINE VARIABLE FILE1           AS CHAR   NO-UNDO.
    DEFINE VARIABLE file2           AS CHAR   NO-UNDO.
    DEFINE VARIABLE nRow1           AS INT    NO-UNDO.
    DEFINE VARIABLE nRow2           AS INT    NO-UNDO.
    DEFINE VARIABLE tmpSub          AS CHAR   NO-UNDO.
    DEFINE VARIABLE tmpH            AS DEC    NO-UNDO.
    DEFINE VARIABLE tmpw            AS DEC    NO-UNDO.
    DEFINE VARIABLE cSub            AS CHAR   NO-UNDO.
    DEFINE VARIABLE lastActivity    AS CHAR   NO-UNDO.
    DEFINE VARIABLE tmpTime         AS CHAR   NO-UNDO.
    DEFINE VARIABLE s1_handle       AS HANDLE NO-UNDO.
    DEFINE VARIABLE s2_handle       AS HANDLE NO-UNDO.
    DEFINE VARIABLE tLoop           AS INT    NO-UNDO.

    ASSIGN c_to_addr = cProgrammerList
           FILE1     = cLogLoc + "\MmOrderReport" + "-" + REPLACE(STRING(TODAY),"/","") + "-" + STRING(TIME) + ".csv"
           FILE2     = REPLACE(FILE1,".csv","-Full.csv").


    DEFINE BUFFER b_mm_hdr FOR sign_mm_hdr.
    DEFINE BUFFER b_mm_det FOR sign_mm_det.

    ASSIGN eCnt = 0.
    FOR EACH issue BREAK BY xSubject:
        IF issue.xSubject BEGINS "MM" THEN DO:
        END.
        ELSE DO: /*email the artists about an issue on there end*/
             ASSIGN c_subject = issue.xSubject
                    c_msg = "Order: "   + issue.xOrder  + CHR(10)
                          + "ITEM: "    + issue.xItem   + CHR(10)
                          + "Artfile: " + issue.xImage  + CHR(10)
                          + "Date/Time: " + STRING(TODAY) + "/" + STRING(TIME,"HH:MM:SS").
/*              RUN mgemail.p ("{system.i} Database","signartdepartment@lowen.com","progressgroup@lowen.com",c_bcc_addr,c_subject,c_msg,c_attachments,FALSE). */
             RUN mgemail.p ("Bullseye Database",cProgrammerList,"","",c_subject,c_msg,c_attachments,FALSE).
        END.
    END.

    /*create and email report on orders in JM*/
    s1_handle = STREAM S1:HANDLE.
    s2_handle = STREAM S2:HANDLE.

    OUTPUT STREAM S1 TO VALUE(FILE1).
    OUTPUT STREAM S2 TO VALUE(FILE2).

    EXPORT STREAM S1 DELIMITER "," "Regen" + " - Date/Time" STRING(TODAY) + " at " + STRING(TIME,"HH:MM").
    EXPORT STREAM S2 DELIMITER "," "Regen" + " - Date/Time" STRING(TODAY) + " at " + STRING(TIME,"HH:MM").

    EXPORT STREAM S1 DELIMITER "," "".
    EXPORT STREAM S2 DELIMITER "," "".

    EXPORT STREAM S1 DELIMITER "," "SO#"
                         "Item#" 
                         "Ship By"
                         "Firm"
                         "Part#"
                         "Size"
                         "Qty"
                         "Last Activity"
                         "Reason"
                         "Reason Item" .
    EXPORT STREAM S2 DELIMITER "," "SO#"
                         "Item#" 
                         "Ship By"
                         "Firm"
                         "Part#"
                         "Size"
                         "Qty"
                         "Last Activity"
                         "Reason"
                         "Reason Item" .

   
    FOR EACH rpt_det:
        FIND so_items NO-LOCK WHERE so_items.itemseq = rpt_det.itemseq NO-ERROR.
        IF NOT AVAIL so_items THEN NEXT.
        FIND so_file  NO-LOCK WHERE so_file.so_no = so_items.so_no     NO-ERROR.
        FIND pt_det   NO-LOCK WHERE pt_det.part_no = so_items.part_no  NO-ERROR.
        FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = so_items.itemseq AND squ_ptdet.TYPE <> "Frame" NO-ERROR.

        IF AVAIL(so_items) AND AVAIL(so_file) THEN DO:
            IF so_items.ship_qty = so_items.orderqty THEN NEXT.
            IF NOT AVAIL(pt_det) THEN NEXT. /*ex. 90SC*/
            IF NOT CAN-FIND(FIRST squ_act WHERE squ_act.subseq = squ_ptdet.subseq AND squ_act.ActSeq = 8) THEN NEXT. 
            IF rpt_det.issue MATCHES "*Not Sart/Dart/Stock/Custom*" THEN NEXT.
            tmpchar = "". tmpissue = "".  tmpint = 0. tmpRack = "".
            tmpIssue = /* IF num-entries(rpt_det.issue,"|") > 1 THEN "Multiple" ELSE */ rpt_det.issue.
            ASSIGN tmpissue = REPLACE(tmpissue,"MM-","").
    
            /*if in queue then set to blank so it pulls info for report*/
            IF INDEX(tmpissue,"Queue") > 0 THEN ASSIGN tmpIssue = "".

            FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.itemseq = so_items.itemseq BREAK BY sign_mm_det.itemseq:
                IF NOT CAN-DO(tmpchar,STRING(sign_mm_det.batchseq)) THEN DO:
                    ASSIGN tmpchar = tmpchar + (IF tmpchar = "" THEN "" ELSE ",") + string(sign_mm_det.batchseq). /*get all batchnums*/
                END.
                IF LAST-OF(sign_mm_det.itemseq) THEN DO: /*get size*/
                    FIND sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.batchseq = sign_mm_det.batchseq NO-ERROR.
                    IF AVAIL sign_mm_hdr THEN DO:
/*                         FIND signbed NO-LOCK WHERE signbed.seq = sign_mm_hdr.bedseq NO-ERROR. */
                    END.
                END.
                IF AVAIL sign_mm_hdr THEN RELEASE sign_mm_hdr.
            END.
            tmpchar = REPLACE(tmpchar,",","|").
    
            /*get sizes*/
            tmph = 0. tmpw = 0. cSub = "".
            IF AVAIL squ_ptdet THEN DO:
                tmph = squ_ptdet.pressprintingheight.
                tmpw = squ_ptdet.pressprintingwidth.
                cSub = squ_ptdet.pt_substrate.
            END.
            IF tmpH = 0 OR tmpw = 0 THEN DO:
                tmph = pt_det.pressprintingheight.
                tmpw = pt_det.pressprintingwidth.
            END.
            IF cSub = "" THEN
                cSub = pt_det.pt_substrate.
    
            IF tmpIssue MATCHES "*Printed*" THEN DO:
                IF tmpIssue MATCHES "*via jm*" THEN DO:
                    FIND sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.batchseq = IF NUM-ENTRIES(tmpchar) > 0 THEN int(ENTRY(1,tmpchar,"|"))ELSE int(tmpchar) NO-ERROR.
                    IF AVAIL sign_mm_hdr THEN DO:
                        tmpchar = STRING(sign_mm_hdr.RUN_date) + " - " + STRING(sign_mm_hdr.RUN_time,"HH:MM").
                    END.
                    ASSIGN tmpIssue = "Complete".
                END.
                ELSE DO:
                    FIND LAST h_detail NO-LOCK WHERE h_detail.order_no = so_items.so_no 
                        AND h_detail.ITEM_no = STRING(so_items.ITEM_No)
                        AND h_detail.activity = "D11" NO-ERROR.
                    IF AVAIL h_detail THEN DO:
                        tmpchar = STRING(h_detail.DATE) + " - " + string(H_detail.finish_time).
                    END.
                    ASSIGN tmpIssue = "Complete - Manual".
                END.
                FOR EACH so_rack NO-LOCK WHERE so_rack.itemseq = so_Items.itemseq:
                    tmpRack = tmpRack + (IF tmpRack = "" THEN "" ELSE "|") + so_rack.rackdesc.
                END.
                IF tmpRack = "" THEN tmpRack = "N/A".
                tmpChar = tmpRack + " - " + tmpChar.
            END.


            IF tmpIssue = "" AND tmpchar <> "" THEN DO:
                tmpIssue = "JM Queue".
            END.
            IF tmpIssue MATCHES "*Checks:*" THEN DO:
                tmpIssue = REPLACE(tmpIssue,"Checks: ","").
            END.
            IF tmpChar = "" THEN DO:
                tmpChar = rpt_det.reason.
            END.
            IF tmpIssue MATCHES "*Art Image Issue*" THEN DO:
                IF CAN-FIND(FIRST zz_file WHERE zz_file.zz_key1 = "BG-TRUEVIEW" AND zz_file.zz_key2 = string(rpt_det.itemseq)) THEN DO:
                    ASSIGN tmpChar = "Art Queued for Creation".
                END.
                ELSE DO: 
                    FIND FIRST zz_file NO-LOCK WHERE zz_file.zz_key1 = "BG-TRUEVIEW-PROCESSED" AND zz_file.zz_key2 = string(rpt_det.itemseq) NO-ERROR.
                    IF AVAIL zz_file THEN DO:
                        tmpTime = "".
                        IF NUM-ENTRIES(zz_file.zz_char[12],",") > 1 THEN DO:
                            DO iLoop = 1 TO NUM-ENTRIES(zz_file.zz_char[12],","):
                                tmpTime = tmpTime + (IF tmpTime = "" THEN "" ELSE ",") + STRING(INT(ENTRY(iLoop,zz_file.zz_char[12],",")),"HH:MM:SS").
                            END.
                        END.
                        ELSE ASSIGN tmpTime = STRING(INT(zz_file.zz_char[12]),"HH:MM:SS").
                        ASSIGN tmpChar = "Art Processed " + " on " + zz_file.zz_char[11] + " at " + tmpTime.
                    END.
                END.
            END.
    
            IF tmpIssue = "" THEN DO: /*attempt to find non recorded issues*/
                FIND FIRST zz_file NO-LOCK WHERE zz_file.zz_key1 = "mm-delete" AND zz_file.zz_char[3] = STRING(TODAY) AND zz_file.zz_char[5] MATCHES "*" + so_items.so_no + "-" + string(so_items.ITEM_no) + "*" NO-ERROR.
                IF AVAIL zz_file THEN DO:
                    FIND FIRST issue NO-LOCK WHERE issue.xbatch = REPLACE(zz_file.zz_key2,"Batch","") NO-ERROR.
                    IF AVAIL issue THEN DO:
                        ASSIGN tmpissue = REPLACE(issue.xSubject,"MM-","").
                        IF tmpIssue MATCHES "*Hotfolder*" THEN DO:
                            ASSIGN tmpIssue = "IT Issue"
                                   tmpChar  = "Can't Find Correct Hotfolder".
                        END.
                        IF tmpIssue MATCHES "*XML*" THEN DO:
                            ASSIGN tmpIssue = "IT Issue"
                                   tmpChar  = "XML Failed".
                        END.
                    END.
                END.
            END.
            IF tmpIssue = "" THEN DO:
                /*set blanks to IT issue*/
                ASSIGN tmpIssue = "IT Issue"
                       tmpChar  = "".
            END.


            LASTActivity = "".
            FOR EACH h_detail NO-LOCK WHERE h_detail.order_no = so_items.so_no
                AND h_detail.item_no = string(so_items.item_no) BY h_detail.date DESCENDING BY h_detail.start_time DESC:
                IF h_detail.zzlog_1 THEN DO:
                    FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "indcode" AND zz_file.zz_key2 = h_detail.location + h_detail.activity NO-ERROR.
                    IF AVAIL zz_file THEN DO:
                        ASSIGN lastActivity = REPLACE(zz_key3,"sign ","") + " " + string(h_detail.date).
                        LEAVE.
                    END.
                END.
            END.
            IF AVAIL zz_file THEN RELEASE zz_file.
            IF INDEX(tmpIssue,"Complete") = 0 AND tmpIssue <> "JM Queue" THEN DO:
                EXPORT STREAM S1 DELIMITER "," so_items.so_no 
                                     so_items.ITEM_no 
                                     so_file.ship_by
                                     so_file.firm
                                     so_items.part_no
                                     IF tmpH = 0 OR tmpw = 0 THEN "" ELSE STRING(tmph) + "x" + STRING(tmpw)
                                     so_items.orderqty
                                     lastActivity
                                     tmpIssue
                                     tmpchar .
                nRow1 = nRow1 + 1.
            END.
            EXPORT STREAM S2 DELIMITER "," so_items.so_no 
                                     so_items.ITEM_no 
                                     so_file.ship_by
                                     so_file.firm
                                     so_items.part_no
                                     IF tmpH = 0 OR tmpw = 0 THEN "" ELSE STRING(tmph) + "x" + STRING(tmpw)
                                     so_items.orderqty
                                     lastActivity
                                     tmpIssue
                                     tmpchar .
            nRow2 = nRow2 + 1.

        END.
        RELEASE squ_ptdet.
    END.

    EXPORT STREAM S1 DELIMITER "," "".
    EXPORT STREAM S2 DELIMITER "," "".

    EXPORT STREAM S1 DELIMITER "," "JM Start:"        STRING(iProgStart,"HH:MM:SS").
    EXPORT STREAM S2 DELIMITER "," "JM Start:"        STRING(iProgStart,"HH:MM:SS").

    EXPORT STREAM S1 DELIMITER "," "JM Finish:"       STRING(iProgFinish,"HH:MM:SS").
    EXPORT STREAM S2 DELIMITER "," "JM Finish:"       STRING(iProgFinish,"HH:MM:SS").

    EXPORT STREAM S1 DELIMITER "," "Elapsed Time:"    STRING(iProgFinish - iProgStart,"HH:MM:SS").
    EXPORT STREAM S2 DELIMITER "," "Elapsed Time:"    STRING(iProgFinish - iProgStart,"HH:MM:SS").

    EXPORT STREAM S1 DELIMITER "," "Batches Created:" STRING(iBatchCnt).
    EXPORT STREAM S2 DELIMITER "," "Batches Created:" STRING(iBatchCnt).

    EXPORT STREAM S1 DELIMITER "," "Copies:"          STRING(iBatchSides).
    EXPORT STREAM S2 DELIMITER "," "Copies:"          STRING(iBatchSides).

    nRow1 = nRow1 + 3.
    nRow2 = nRow2 + 3.

    IF VALID-HANDLE(s1_handle) THEN OUTPUT STREAM S1 CLOSE.
    IF VALID-HANDLE(s2_handle) THEN OUTPUT STREAM S2 CLOSE.

    DO tLoop = 1 TO 2:
        IF tloop = 1 THEN 
            RUN excelFormat(nRow1,INPUT-OUTPUT FILE1).
        ELSE 
            RUN excelFormat(nRow2,INPUT-OUTPUT FILE2).
    END.
    
    {mgemaildist.i "JobManagerReport"}
/*     c_to_addr = "terryp@lowen.com". */

    DO: /*mail the slimmed down report to everyone*/
        ASSIGN c_subject     = "Job Manager Status Report"
               c_msg         = "Attached spreadsheet of Job Manager report." + CHR(10)
                               + CHR(10)
                               + "Regen Date/Time: " + STRING(TODAY) + " at " + STRING(TIME,"HH:MM")
               c_attachments = FILE1.
        RUN mgemail.p ("Bullseye Database",c_to_addr,c_cc_addr,c_bcc_addr,c_subject,c_msg,c_attachments,FALSE).
    END.
    
    DO: /*mail the full version to select few*/
        ASSIGN c_subject     = "Job Manager Status Report"
               c_to_addr     = cProgrammerList + ";brittanyb@lowen.com;matthews@lowen.com"
               c_msg         = "Attached spreadsheet of the Job Manager report(full version)." + CHR(10)
                               + CHR(10)
                               + "Regen Date/Time: " + STRING(TODAY) + " at " + STRING(TIME,"HH:MM")
               c_attachments = FILE2.
        RUN mgemail.p ("Bullseye Database",c_to_addr,"","",c_subject,c_msg,c_attachments,FALSE).

    END.
END PROCEDURE.


PROCEDURE ExcelFormat:
    DEFINE INPUT PARAMETER nRow AS INT NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER sFile  AS CHAR NO-UNDO.


    DEFINE VARIABLE chExcelApplication      AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE chWorkbook              AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE chWorksheet             AS COM-HANDLE  NO-UNDO.

    {excel.i}

    CREATE "Excel.Application" chExcelApplication.

    chWorkbook  = chExcelApplication:Workbooks:Open(sFile).
    chWorkSheet = chExcelApplication:Sheets:Item(1).

    ASSIGN chWorkSheet:Range("A1:J3")  :FONT:BOLD  = TRUE
           chWorkSheet:COLUMNS("A"):ColumnWidth    = 18
           chWorkSheet:COLUMNS("B"):ColumnWidth    = 16
           chWorkSheet:COLUMNS("C"):ColumnWidth    = 10
           chWorkSheet:COLUMNS("D"):ColumnWidth    = 7
           chWorkSheet:COLUMNS("E"):ColumnWidth    = 17
           chWorkSheet:COLUMNS("F"):ColumnWidth    = 6
           chWorkSheet:COLUMNS("G"):ColumnWidth    = 5
           chWorkSheet:COLUMNS("H"):ColumnWidth    = 33
           chWorkSheet:COLUMNS("I"):ColumnWidth    = 61
           chWorkSheet:COLUMNS("J"):ColumnWidth    = 49
           chWorkSheet:Range("a1:b1" + STRING(nRow))    :HORIZONTALAlignment    = {&range_across} /*merge & center*/
           
           chWorkSheet:Range("I4:J" + STRING(nRow))     :HORIZONTALAlignment    = {&range_left} /*left justified*/
           chWorkSheet:PageSetup:PrintTitleRows    = "$1:$1"
           chWorkSheet:PageSetup:ORIENTATION       = 1
           chWorkSheet:PageSetup:Zoom              = 47.
           chWorkSheet:Range("A4"):SELECT().
           chExcelApplication:ActiveWindow:FreezePanes = TRUE.


    chWorkSheet:Range("A3:J3"):SELECT().
    chExcelApplication:Selection:AutoFilter(?,?,?).
    
    /*chWorkSheet:Range("A3", chWorksheet:Range("J3"):END(-4121)):SORT(chWorksheet:Range("C3"), 1, chWorksheet:Range("A3"), , , chWorksheet:Range("B3"), , 1, , , , , , , ).*/

    sFile = REPLACE(sFile,".csv",".xls").
    {excel-save.i sFile}

    chExcelApplication:VISIBLE = FALSE.
    chExcelApplication:QUIT().
    
    RELEASE OBJECT chWorkSheet.
    RELEASE OBJECT chWorkBook.
    RELEASE OBJECT chExcelApplication.

END PROCEDURE.


PROCEDURE ExportData:
    DEFINE INPUT PARAMETER cType AS CHAR NO-UNDO.
    DEFINE VARIABLE dataFile AS CHAR NO-UNDO.

    dataFile = cLogLoc + "\mmTempTable.d".
    IF cType = "Export" THEN DO:
        IF SEARCH(dataFile) <> ? THEN OS-DELETE VALUE(datafile).
        
        IF SEARCH(dataFile) = ? THEN DO:
            OUTPUT TO VALUE(dataFile).
            FOR EACH rpt_det:
                EXPORT rpt_det.
            END.
            OUTPUT CLOSE.
        END.
    END.
    ELSE DO:
        IF SEARCH(dataFile) <> ? THEN DO:
            INPUT FROM VALUE(dataFile).
            REPEAT:
                CREATE rpt_det.
                IMPORT rpt_det. 
            END.
            INPUT CLOSE.
        END.
    END.
END PROCEDURE.


PROCEDURE ExportPDF:
    DEFINE INPUT PARAMETER eArtfile AS CHAR NO-UNDO.
    DEFINE VARIABLE eFolder AS CHAR NO-UNDO.
    
    RUN setHomeFolder.

    eFolder = "\\fs02\bullseye\images\agentphotos\Temporary\FredOriginals".
    FILE-INFO:FILE-NAME = eFolder.
    IF FILE-INFO:FULL-PATHNAME = ? THEN OS-CREATE-DIR value(cBatchImgLoc).

    OS-COPY VALUE(eArtfile) VALUE(eFolder + "\" + ENTRY(NUM-ENTRIES(eArtfile,"\"),eArtfile,"\")).

END PROCEDURE.


PROCEDURE FileExists:
    DEFINE INPUT  PARAMETER iFile      AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER fileExists AS LOG  NO-UNDO.

    FILE-INFO:FILE-NAME = iFile.
    IF FILE-INFO:FULL-PATHNAME <> ? THEN fileExists = YES. ELSE fileExists = NO.
END PROCEDURE.


PROCEDURE FindIcNo:
    DEFINE INPUT PARAMETER cItemSeq AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER cSo      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cItemNo  AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER cIC      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cPartNo  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER guess    AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cICseq  AS INT  NO-UNDO.
    DEFINE VARIABLE foundBom        AS LOG  NO-UNDO.
    DEFINE VARIABLE icNum           AS INT  NO-UNDO.

    DEFINE BUFFER buf5_squ_ptdet FOR squ_ptdet.
    DEFINE BUFFER buf5_so_items  FOR so_items.

    icNum = 0. cIcSeq = 0.
    /*count what number on the IC this should be*/
    FOR EACH b_items NO-LOCK WHERE b_items.so_no = cSo BY b_items.ITEM_no:
        FIND b_squ_plan NO-LOCK WHERE b_squ_plan.itemseq = b_items.itemseq NO-ERROR.
        IF AVAIL b_squ_plan AND b_squ_plan.ic_no <> "" THEN icNum = icNum + 1. 
        IF b_items.ITEM_no = cItemNo THEN LEAVE.
       
    END.
    IF AVAIL b_squ_plan THEN RELEASE b_squ_plan.

    IF guess = "NO" THEN DO:
        /*check so_post record to see if it tells me*/
        FOR EACH b_items NO-LOCK WHERE b_items.so_no = STRING(cIC) BY b_items.ITEM_no:
            IF CAN-FIND(FIRST so_post NO-LOCK WHERE so_post.so_no = b_items.so_no AND so_post.ITEM_no = b_items.ITEM_no
                        AND so_post.post_so = cSO AND so_post.post_item = cItemNo) THEN DO:
                ASSIGN cICSeq = b_items.itemseq.
            END.
        END.
    END.
    IF guess = "YES" THEN DO:
        IF cICSeq = 0 THEN DO:
            FOR EACH inv_item NO-LOCK WHERE inv_item.inv_no = STRING(cIC) BY inv_item.ITEM_no:
                IF CAN-FIND(FIRST so_post NO-LOCK WHERE so_post.so_no = inv_item.inv_no AND so_post.ITEM_no = inv_item.ITEM_no
                            AND so_post.post_so = cSO AND so_post.post_item = cItemNo) THEN DO:
                    ASSIGN cICSeq = inv_item.itemseq.
                END.
            END.
        END.
    END.
    IF cICseq = 0 THEN DO:
        /*make an educated guess*/
        IF CAN-FIND(FIRST b_items NO-LOCK WHERE b_items.so_no = STRING(cIC)) THEN DO:
            FIND FIRST b_items NO-LOCK WHERE b_items.so_no = STRING(cIC) AND b_items.ITEM_no = cItemNo NO-ERROR.
            IF NOT AVAIL b_items THEN
                FIND FIRST b_items NO-LOCK WHERE b_items.so_no = string(cIC) AND b_items.part_no = cPartNo NO-ERROR.
            IF AVAIL b_items THEN DO:
                ASSIGN cIcSeq = b_items.itemseq.
            END.
            ELSE DO:
                FOR EACH b_items NO-LOCK WHERE b_items.so_no = string(cIC):
                    FOR EACH buf_ptdet NO-LOCK WHERE buf_ptdet.itemseq = b_items.itemseq AND buf_ptdet.TYPE <> "Frame":
                        FOR EACH squ_mat NO-LOCK OF buf_ptdet:
                            IF CAN-FIND(FIRST BOM_file NO-LOCK WHERE bom_file.parent = cPartNo AND bom_file.part_no = squ_mat.part_no) THEN DO:
                                ASSIGN cICSeq = buf_ptdet.itemseq.
                            END.
                        END.
                    END.
                END.
            END.
            IF cICseq = 0 THEN DO:
                FIND b_items NO-LOCK WHERE b_items.so_no = STRING(cIC) AND b_items.ITEM_no = icNum NO-ERROR.
                IF AVAIL b_items THEN DO:
                    FIND FIRST buf_ptdet NO-LOCK WHERE buf_ptdet.itemseq = b_items.itemseq AND buf_ptdet.TYPE <> "Frame" NO-ERROR.
                    IF AVAIL buf_ptdet THEN ASSIGN cICSeq = buf_ptdet.itemseq.
                END.
            END.
            IF cICseq = 0 THEN DO:
                /* if there's only one, use it */
                FIND b_items NO-LOCK WHERE b_items.so_no = STRING(cIC) NO-ERROR.
                IF AVAIL b_items THEN DO:
                    FIND FIRST buf_ptdet NO-LOCK WHERE buf_ptdet.itemseq = b_items.itemseq AND buf_ptdet.TYPE <> "Frame" NO-ERROR.
                    IF AVAIL buf_ptdet THEN ASSIGN cICSeq = buf_ptdet.itemseq.
                END.
            END.
        END.
        ELSE DO:
            FIND FIRST inv_item NO-LOCK WHERE inv_item.inv_no = STRING(cIC) AND inv_item.ITEM_no = cItemNo NO-ERROR.
            IF NOT AVAIL inv_item THEN
                FIND inv_item NO-LOCK WHERE inv_item.inv_no = string(cIC) AND inv_item.part_no = cPartNo NO-ERROR.
            IF AVAIL inv_item THEN DO:
                ASSIGN cIcSeq = inv_item.itemseq.
            END.
            ELSE DO:
                FOR EACH inv_item NO-LOCK WHERE inv_item.inv_no = string(cIC):
                    FOR EACH buf_ptdet NO-LOCK WHERE buf_ptdet.itemseq = inv_item.itemseq AND buf_ptdet.TYPE <> "Frame":
                        FOR EACH squ_mat NO-LOCK OF buf_ptdet:
                            IF CAN-FIND(FIRST BOM_file NO-LOCK WHERE bom_file.parent = cPartNo AND bom_file.part_no = squ_mat.part_no) THEN DO:
                                ASSIGN cICSeq = buf_ptdet.itemseq.
                            END.
                        END.
                    END.
                END.
            END.
            IF cICseq = 0 THEN DO:
                FIND inv_item NO-LOCK WHERE inv_item.inv_no = STRING(cIC) AND inv_item.ITEM_no = icNum NO-ERROR.
                IF AVAIL inv_item THEN DO:
                    FIND FIRST buf_ptdet NO-LOCK WHERE buf_ptdet.itemseq = inv_item.itemseq AND buf_ptdet.TYPE <> "Frame" NO-ERROR.
                    IF AVAIL buf_ptdet THEN ASSIGN cICSeq = buf_ptdet.itemseq.
                END.
            END.
            IF cICSeq = 0 THEN DO:
                FIND buf5_so_items NO-LOCK WHERE buf5_so_items.itemseq = cItemSeq NO-ERROR.
                IF AVAILABLE buf5_so_items THEN DO:
                    FIND buf5_squ_ptdet OF buf5_so_items NO-LOCK  WHERE buf5_squ_ptdet.TYPE <> "FRAME" NO-ERROR.
                    IF AVAILABLE buf5_squ_ptdet THEN DO:
                        FOR EACH inv_item NO-LOCK WHERE inv_item.inv_no = STRING(cIC),
                        EACH squ_ptdet OF inv_item NO-LOCK:
                            IF squ_ptdet.pressprintingheight = buf5_squ_ptdet.pressprintingheight
                            AND squ_ptdet.pressprintingwidth = buf5_squ_ptdet.pressprintingwidth THEN DO:
                                ASSIGN cICSeq = inv_item.itemseq.
                            END.
                        END.
                        IF cICSeq = 0 THEN
                        FOR EACH b_items NO-LOCK WHERE b_items.so_no = STRING(cIC),
                        EACH squ_ptdet OF b_items NO-LOCK:

                            IF squ_ptdet.pressprintingheight = buf5_squ_ptdet.pressprintingheight
                            AND squ_ptdet.pressprintingwidth = buf5_squ_ptdet.pressprintingwidth THEN DO:
                                ASSIGN cICSeq = b_items.itemseq.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.
END PROCEDURE.


PROCEDURE FindSpace:
    DEFINE INPUT  PARAMETER cBed   AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER cSpace AS DEC NO-UNDO.
    DEFINE VARIABLE tmpSpace       AS DEC NO-UNDO.
    DEFINE VARIABLE fspace         AS DEC NO-UNDO.
    DEFINE VARIABLE BSPACE         AS DEC NO-UNDO.
    DEFINE BUFFER bbed       FOR signbed.
    DEFINE BUFFER beddet     FOR signbeddet.
    DEFINE BUFFER b_beddet   FOR signbeddet.
    DEFINE BUFFER buf_det    FOR signbeddet.

    ASSIGN tmpSpace = 0 cSpace = 0.
    FOR EACH bbed NO-LOCK WHERE bbed.seq = cBed,
        LAST beddet NO-LOCK WHERE beddet.seq = cBed :
        IF CAN-FIND(signbeddet WHERE signbeddet.seq = cBed AND signbeddet.userlog2 = YES) THEN DO: /*2+ rows*/
            FIND b_beddet NO-LOCK WHERE b_beddet.seq = cBed AND b_beddet.POSITION = beddet.POSITION - INT(bbed.userdec2) NO-ERROR.
            IF AVAIL b_beddet THEN DO:

                ASSIGN tmpSpace = beddet.userdec1 - b_beddet.userdec1
                       tmpSpace = IF ABSOLUTE(tmpSpace - bbed.imageWidth) < ABSOLUTE(tmpSpace - bbed.imageHeight) THEN bbed.imageWidth ELSE bbed.imageHeight.
            END.
            ELSE DO: /*guessing*/
                tmpSpace = bbed.imagewidth.
            END.
        END.
        ELSE DO: /*1 row*/
            FIND b_beddet NO-LOCK WHERE b_beddet.seq = cBed AND b_beddet.POSITION = beddet.POSITION - 1 NO-ERROR.
            IF AVAIL b_beddet THEN DO:
                ASSIGN tmpSpace = beddet.userdec1 - b_beddet.userdec1
                       tmpSpace = IF ABSOLUTE(tmpSpace - bbed.imageWidth) < ABSOLUTE(tmpSpace - bbed.imageHeight) THEN bbed.imageWidth ELSE bbed.imageHeight.
            END.
            ELSE DO: /*guessing*/
                tmpSpace = bbed.imagewidth.
            END.
        END.
        FIND FIRST buf_det NO-LOCK WHERE buf_det.seq = cBed NO-ERROR.
        IF AVAIL buf_det THEN DO:
            ASSIGN fspace = buf_det.userdec1.
        END.
        FIND LAST buf_det NO-LOCK WHERE buf_det.seq = cBed NO-ERROR.
        IF AVAIL buf_det THEN DO:
            ASSIGN BSPACE = 96 - (beddet.userdec1 + bbed.imageWidth).
        END.

        ASSIGN cSpace = 96 - (beddet.userdec1 + tmpSpace). 
        IF fSpace                       = bSpace                       THEN cSpace = 0.
        ELSE IF fspace + bSpace         = 0                            THEN cSpace = 0.
        ELSE IF ROUND(fspace,1)         = ROUNd(bspace,1)              THEN cSpace = 0.
        ELSE IF ABS(fspace - bspace) < .25                             THEN cSpace = 0.
/*         ELSE IF TRUNCATE(fspace ,0) + 1 = TRUNCATE(bspace,0) + 1 THEN cSpace = 0. */
        ELSE IF fSpace > bSpace                                        
            THEN cSpace = - cSpace.

        IF cBed = 30 OR cBed = 80 OR cBed = 159 OR cBed = 167 OR cBed = 168 OR cBed = 175  THEN 
            cSpace = 0.
    END.

END PROCEDURE.


PROCEDURE GangCheck:
    DEFINE BUFFER buf_mm_det FOR sign_mm_det.
    DEFINE BUFFER buf_mm_hdr FOR sign_mm_hdr.
    
    DEFINE VARIABLE issuedCount AS INT  NO-UNDO.
    DEFINE VARIABLE batchCnt    AS INT  NO-UNDO.    
    DEFINE VARIABLE gangCnt     AS INT  NO-UNDO.
    DEFINE VARIABLE sendEmail   AS LOG  NO-UNDO.
    DEFINE VARIABLE gangFile    AS CHAR NO-UNDO.
    DEFINE VARIABLE qtyRan      AS INT  NO-UNDO.
    DEFINE VARIABLE qtyNeeded   AS INT  NO-UNDO.
    DEFINE VARIABLE inQueue     AS LOG  NO-UNDO.
    
    gangFile = imageShare + "AgentPhotos\Temporary\MM Logs\duplicatecheck.txt".
    OUTPUT TO VALUE(gangFile).
    FOR EACH sign_mm_hdr WHERE RUN_time = ?,
    EACH sign_mm_det OF sign_mm_hdr NO-LOCK BREAK BY itemseq:
        IF LAST-OF(sign_mm_det.itemseq) THEN DO:
            IF itemseq = 0 THEN DO:
                NEXT.
            END.
            FIND so_items OF sign_mm_det NO-LOCK NO-ERROR.
            IF NOT AVAIL so_items THEN NEXT.
            IF so_items.so_no = "2619651" THEN NEXT.

            RUN getQty(so_items.itemseq,OUTPUT qtyRan, OUTPUT qtyNeeded, OUTPUT inQueue).
            IF qtyNeeded < 0 THEN DO:
                IF NOT CAN-FIND(FIRST batchdet WHERE batchdet.orderno = so_items.so_no AND batchdet.itemno = so_items.ITEM_no) THEN DO:
                    sendEmail = YES.
                    CREATE batchdet.
                    ASSIGN batchdet.orderno  = so_items.so_no
                           batchdet.itemno   = so_items.ITEM_no
                           batchdet.orderqty = so_items.orderqty
                           batchdet.gangqty  = qtyRan.
                END.
            END.
    
            issuedCount = 0.
            FOR EACH m_usage NO-LOCK
                WHERE m_usage.order_no = so_items.so_no
                AND m_usage.ITEM_no = STRING(so_items.ITEM_no)
                AND m_usage.part_no = sign_mm_hdr.inv_part
                AND m_usage.IN_process <> ?:
                ASSIGN issuedCount = issuedCount + m_usage.quantity.
            END.
            IF issuedCount >= so_items.orderqty THEN DO:
                sendEmail = YES.
                DISPLAY sign_mm_hdr.batchseq
                        so_items.so_no 
                        so_items.ITEM_no 
                        so_items.orderqty  COLUMN-LABEL "Order!Qty"     FORMAT ">>>9"
                        issuedCount        COLUMN-LABEL "Panels!Issued" FORMAT ">>>9"
                    WITH STREAM-IO.
                batchCnt = 0.
                FOR EACH buf_mm_det NO-LOCK WHERE buf_mm_det.itemseq = sign_mm_det.itemseq
                    BREAK BY buf_mm_det.batchseq /* BY buf_mm_det.itemseq */:
                    batchCnt = batchCnt + 1.
/*                     gangCnt  = gangCnt + 1. */
                    IF LAST-OF(buf_mm_det.batchseq) THEN DO:
                        
                        FIND buf_mm_hdr OF buf_mm_det NO-LOCK.
                        DISPLAY SPACE(5) buf_mm_hdr.batchseq buf_mm_hdr.qty_print batchCnt WITH STREAM-IO.
                        batchCnt = 0.
                    END.
                END.
    
                FOR EACH h_detail NO-LOCK WHERE h_detail.order_no = so_items.so_no
                    AND  h_detail.ITEM_no = STRING(so_items.ITEM_no)
                    AND h_detail.activity = "D11":
                    DISPLAY SPACE(5) h_detail.DATE h_detail.empl_no h_detail.activity h_detail.BATCH h_detail.pieces
                        WITH STREAM-IO.
                END.
                FOR EACH h_detail NO-LOCK WHERE h_detail.order_no = so_items.so_no
                    AND  h_detail.ITEM_no = STRING(so_items.ITEM_no)
                    AND h_detail.activity = "S2":
                    DISPLAY SPACE(5) h_detail.DATE h_detail.empl_no h_detail.activity h_detail.BATCH h_detail.pieces
                        WITH STREAM-IO.
                END.
    
            END.
        END.
    END.
    FOR EACH batchdet:
        DISPLAY batchdet.orderno batchdet.itemno batchdet.orderqty batchdet.gangqty WITH STREAM-IO.
    END.
    OUTPUT CLOSE.

    IF sendEmail THEN DO:
        c_to_addr = cProgrammerList + ";brittanyb@lowen.com;matthews@lowen.com".
        ASSIGN c_subject     = "GangCheck"
               c_msg         = "Attached is a text file that shows possible duplicate runs and any instances of gangs larger than the order quantity".
               c_attachments = gangFile. 
        RUN mgemail.p ("Bullseye Database",c_to_addr,"","",c_subject,c_msg,c_attachments,FALSE).
    END.
END PROCEDURE.


PROCEDURE GenXML:
    DEFINE INPUT PARAMETER cBatch AS CHAR       NO-UNDO.
    DEFINE VARIABLE xmlData       AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE cResponse     AS CHAR       NO-UNDO.
    DEFINE VARIABLE chDart        AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE svXMLDATA     AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE cArtfile      AS CHAR       NO-UNDO.
    DEFINE VARIABLE xcoord        AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE ycoord        AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE NEWPos        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE numRan        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cPosition     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE testRun       AS CHAR       NO-UNDO.
    DEFINE VARIABLE chgCorex      AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE HdrNum        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE OutPutfile    AS CHAR       NO-UNDO.
    DEFINE VARIABLE templog       AS LOG        NO-UNDO.
    DEFINE VARIABLE xtraSpace     AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE tmpHeight     AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE tmpWidth      AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE foundDiff     AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE actheight     AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE actwidth      AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cSides        AS INT        NO-UNDO.
    DEFINE VARIABLE useHalf       AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cFlutes       AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE specCorex     AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cHotfolderseq AS INT        NO-UNDO.
    DEFINE VARIABLE nestcnt       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE hdrCnt        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE errCnt        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE turning       AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE switched      AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE doTurn        AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE template      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE setBedHeight  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE lmultibatch   AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE isReflective  AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE startTime     AS INT        NO-UNDO.
    DEFINE VARIABLE cOutPutFile   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE CorexOver48   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE CorexRef      AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE CADFirstCorex AS LOGICAL    NO-UNDO.
    
    {mgseclist.i "CorexOver48" CorexOver48}
    DEFINE VARIABLE CorexFO       AS LOG NO-UNDO.
    
    DEFINE VARIABLE art AS ArtGenerator.
    art = NEW ArtGenerator().     
     
    
    IF cBatch = "" THEN RUN exportData ("export"). /*run only with engine...not reprints*/
    RUN sethomefolder.

    FOR LAST zz_file EXCLUSIVE-LOCK WHERE zz_file.zz_key1 = "MediaManager" AND zz_file.zz_key2 = "PID":
        ASSIGN zz_file.zz_log[1] = YES. /*mark as made to genxml*/
    END.
    IF AVAIL zz_file THEN RELEASE zz_file.

     DO: /*regular template batches*/
        numRan = 0.
          FOR EACH sign_mm_hdr WHERE IF cBatch <> "" THEN sign_mm_hdr.rerun = YES  AND sign_mm_hdr.run_time = ? AND sign_mm_hdr.bedseq > 0
              ELSE sign_mm_hdr.RUN_time = ? AND sign_mm_hdr.bedseq > 0
                  BY sign_mm_hdr.runseq:
                            
              ASSIGN CorexFO  = FALSE
                     CorexRef = FALSE.
              FOR EACH sign_mm_det WHERE sign_mm_det.batchseq = sign_mm_hdr.batchseq:
                  FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = sign_mm_det.itemseq NO-ERROR.
                  FIND FIRST squ_plan  NO-LOCK WHERE squ_plan.itemseq  = sign_mm_det.itemseq NO-ERROR.
                  
                  IF squ_ptdet.FoldOver = TRUE THEN DO:
                      CorexFO = TRUE.
                  END.                
                  IF AVAILABLE squ_plan THEN DO:
                      IF squ_plan.reflective = TRUE OR (INDEX(squ_ptdet.longdesc,"Reflective") > 0 OR INDEX(squ_ptdet.longdesc,"REF") > 0) THEN CorexRef = TRUE.
                  END.
              END.
              RELEASE squ_ptdet.
              
              IF sign_mm_hdr.zzchar_1 = "PrimeCenter" OR INDEX(sign_mm_hdr.matltype,"Corex") > 0 THEN DO:
                   IF LOOKUP(STRING(sign_mm_hdr.bedseq),CorexOver48) = 0 AND CorexRef = FALSE THEN NEXT.
                   IF CorexFO = TRUE THEN NEXT.
              END.
              
              isReflective = NO. /*ryanle*/
              
              /*set to 'no' so that it only runs each reprint once*/
              IF sign_mm_hdr.rerun = YES THEN ASSIGN sign_mm_hdr.rerun = NO.

              IF INDEX(sign_mm_hdr.matltype,"Magnetic")  > 0 THEN NEXT.

              FIND signbed NO-LOCK WHERE signbed.seq = sign_mm_hdr.bedseq NO-ERROR.
              iTime  = TIME.
              HdrNum = sign_mm_hdr.batchseq.

              tmpWidth = 0. tmpHeight = 0.
              IF AVAIL sign_mm_det THEN RELEASE sign_mm_det.
              IF AVAIL pt_det      THEN RELEASE pt_det.

              ASSIGN tmpWidth  = signbed.imagewidth
                     tmpHeight = signbed.imageheight.

              /*do not send if file is a template*/
              IF sign_mm_hdr.runseq MODULO 2 <> 0 THEN NEXT.
              
              cHotfolderseq = 0.
              RUN getHotfolder(sign_mm_hdr.batchseq,sign_mm_hdr.matlType,INPUT-OUTPUT cHotfolderseq, OUTPUT outputfile).

              IF outputfile = "Error" THEN DO:
                  iErrorCnt = iErrorCnt + 1.
                  RUN trimbeds(STRING(sign_mm_hdr.batchseq)).
                  NEXT.
              END.
              
              /*do not send anything if the files were saved*/
              IF cBatch = "" THEN
                  IF SEARCH(outputfile + "\" + "batch" + string(sign_mm_hdr.batchseq) + "_" + STRING(signbed.imageheight) + "x" + STRING(signbed.imagewidth) + "_" + "1.pdf") <> ? THEN NEXT.

              RUN saveDown(NO,sign_mm_hdr.batchseq,"",sign_mm_hdr.matlType,STRING(signbed.imageheight) + "x" + STRING(signbed.imagewidth),signbed.RESIZE,NO). /*saves down to CS6*/

              IF NOT AVAIL(sign_mm_hdr) THEN NEXT. /*if just deleted then move on*/
              chgCorex = NO.

              RUN imagediff(sign_mm_hdr.batchseq,OUTPUT foundDiff).
              IF INDEX(sign_mm_hdr.matltype,"corex") > 0 THEN foundDiff = YES.
              
              /*reflective Corex Code - 12/17/20 - RyanLe*/
              IF INDEX(sign_mm_hdr.matltype,"COREX") > 0 THEN DO:
                  FOR EACH sign_mm_det NO-LOCK OF sign_mm_hdr:
                      FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = sign_mm_det.itemseq NO-ERROR.
                      IF AVAIL so_items THEN DO:
                          IF INDEX(so_items.partdesc,"REFLECTIVE 4MM CP") = 0 THEN NEXT.
                          FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = so_items.itemseq AND squ_ptdet.type = "Panel" NO-ERROR.
                          IF AVAIL squ_ptdet THEN DO:                          
                              IF squ_ptdet.DigitalSF = TRUE THEN foundDiff = NO. ELSE foundDiff = YES.
                              isReflective = YES.  
                              sign_mm_hdr.bedseq = 19.
                              LEAVE. 
                          END.
                          RELEASE squ_ptdet.                   
                      END. 
                      RELEASE so_items NO-ERROR.  
                  END.
              END.

              ASSIGN iBatchCnt   = iBatchCnt + 1
                     iBatchSides = iBatchSides + (sign_mm_hdr.sides * sign_mm_hdr.qty)
                     cSides      = sign_mm_hdr.sides.
              DO iloop = 1 TO (IF NOT foundDiff THEN 1 ELSE cSides):
                  IF NOT AVAIL(sign_mm_hdr) THEN NEXT.
                  ASSIGN xmlData     = ""
                         svXMLDATA   = ""
                         template    = ""
                         cOutputfile = "".
                         
                  ASSIGN xmldata = xmldata + "<bed>".

                  IF INDEX(sign_mm_hdr.matltype,"corex") > 0 AND iLoop = 1 THEN template = signBed.template1.
                  ELSE IF INDEX(sign_mm_hdr.matltype,"corex") > 0 AND iLoop = 2 THEN template = signBed.template2.
                  IF isReflective = YES THEN template = "". /*Ryanle - relfective corex 12/17/20*/
                  
                  
                  IF template = "" THEN DO:
                      template = "\\lowen\dfssancluster\SignArt\DigitalBedTemplates\Adobe Templates\New AI Bed Template.ait".                      
                  END.

                  RUN xmltag ("bedType",     "CUSTOM",                       INPUT-OUTPUT xmldata).
                  RUN xmltag ("batchno",     sign_mm_hdr.batchseq,           INPUT-OUTPUT xmldata).
                  
                  IF isReflective = YES THEN DO:
                       RUN xmltag ("bedseq","19",INPUT-OUTPUT xmldata). /*Ryanle - relfective corex 12/17/20*/
                  END.
                  ELSE RUN xmltag ("bedseq",      sign_mm_hdr.bedseq,INPUT-OUTPUT xmldata).
                  
                  ASSIGN cOutputfile = "".
                         cOutputfile = outputfile + "\batch" + STRING(sign_mm_hdr.batchseq) + "_" + STRING(signbed.imageheight) + "x" + STRING(signbed.imagewidth) + "_" + STRING(iLoop) + ".pdf".  
                  
                  RUN xmltag ("outputfile",cOutputfile, INPUT-OUTPUT xmldata).
                  RUN xmltag ("tempfile",    template,                       INPUT-OUTPUT xmldata).
                  RUN xmltag ("width",       STRING(tmpWidth) ,              INPUT-OUTPUT xmldata).
                  RUN xmltag ("height",      STRING(tmpHeight),              INPUT-OUTPUT xmldata).
                  
                  IF INDEX(signBed.MatrlType,"corex") > 0 AND isReflective = NO THEN RUN xmltag ("bedHeight","48",INPUT-OUTPUT xmldata).
                  ELSE RUN xmltag ("bedHeight","61",INPUT-OUTPUT xmldata).
                  
                  RUN xmltag ("materialtype",IF INDEX(signbed.matrlType,"Corex") = 0 AND index(sign_mm_hdr.matltype,"corex") > 0 OR isReflective = YES THEN "Steel" ELSE sign_mm_hdr.matltype ,INPUT-OUTPUT xmldata).
                  RUN xmltag ("template",    "false" ,                       INPUT-OUTPUT xmldata).

                  ASSIGN xmldata = xmldata + "<images>".

                  FOR EACH sign_mm_det OF sign_mm_hdr:
                      ASSIGN cArtfile = ""
                             CADFirstCorex = FALSE.
                             
                      IF NUM-ENTRIES(sign_mm_det.artfile,",") > 1 THEN ASSIGN cArtfile = ENTRY(iLoop,sign_mm_det.artfile,",").
                      ELSE ASSIGN cArtfile = sign_mm_det.artfile.

                      IF NOT cArtfile MATCHES "*_CS6*" THEN ASSIGN cartfile = REPLACE(cArtfile,".pdf","_CS6.pdf").
    
                      /*set to use savedown files in the cs6 folder*/
                      IF NUM-ENTRIES(cArtfile,"\") > 0 THEN
                      ASSIGN cArtfile = cBatchImgLoc + "\" + ENTRY(NUM-ENTRIES(cArtfile,"\"),cArtfile,"\").

                      FIND pt_det NO-LOCK WHERE pt_det.part_no = sign_mm_det.part_no NO-ERROR.
                      IF AVAIL squ_ptdet THEN RELEASE squ_ptdet.
                      FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = sign_mm_det.itemseq AND squ_ptdet.TYPE <> "frame" NO-ERROR.
                      IF NOT AVAIL squ_ptdet THEN
                          FIND squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = sign_mm_det.itemseq AND squ_ptdet.TYPE = "frame" NO-ERROR.

                       /*Send the left side then right*/
      /*                 IF NOT pt_Det.steeltent AND NOT pt_det.jackunit THEN DO: */
                      IF NOT squ_ptDet.steeltent AND NOT squ_ptdet.jackunit THEN DO:
                          IF iLoop = 1      AND index(sign_mm_hdr.matltype,"corex") = 0 AND cArtFile MATCHES "*_RT*"   THEN     cArtfile = REPLACE(cArtFile,"_RT","_LT").
                          ELSE IF iLoop = 2 AND index(sign_mm_hdr.matltype,"corex") = 0 AND cArtFile MATCHES "*_LT*"   THEN     cArtfile = REPLACE(cArtFile,"_LT","_RT").
                          ELSE IF iLoop = 1 AND index(sign_mm_hdr.matltype,"corex") > 0 AND cArtFile MATCHES "*_RT*"   THEN     cArtfile = REPLACE(cArtFile,"_RT","_LT").
                          ELSE IF iLoop = 2 AND index(sign_mm_hdr.matltype,"corex") > 0 AND cArtFile MATCHES "*_LT*"   THEN     cArtfile = REPLACE(cArtFile,"_LT","_RT").
                          ELSE IF iLoop = 1 AND index(sign_mm_hdr.matltype,"corex") > 0 AND NOT cArtFile MATCHES "*_LT*"
                              AND NOT cArtFile MATCHES "*_RT*" AND NOT cArtFile MATCHES "*-left*" AND sign_mm_hdr.sides = 2
                              AND SEARCH(REPLACE(cArtFile,"_CS6","-Left_CS6")) <> ? THEN cArtfile = REPLACE(cArtFile,"_CS6","-Left_CS6").
                          /*ELSE IF iLoop = 2 AND index(sign_mm_hdr.matltype,"corex") > 0 AND cArtFile MATCHES "*-left*" THEN     cArtfile = REPLACE(cArtFile,"-left",""). - ryanle removed 10/17/23*/
                      END.

                      /*add code incase there is for ex 2 small size in place of one large sign*/
                      ASSIGN xmldata   = xmldata + "<item>"
                             cPosition = sign_mm_det.POSITION
                             useHalf   = NO.

                      IF iLoop = 2 AND index(sign_mm_hdr.matltype,"corex") > 0 THEN DO:
                          FIND FIRST b_beddet NO-LOCK WHERE b_beddet.seq = sign_mm_hdr.bedseq AND b_beddet.POSITION = sign_mm_det.POSITION NO-ERROR.                         
                          IF AVAIL b_beddet AND (sign_mm_hdr.bedseq <> 30 AND sign_mm_hdr.bedseq <> 31 AND sign_mm_hdr.bedseq <> 81) THEN DO:
                              templog = NO.
                              IF b_beddet.userlog1 = YES THEN templog = YES.
                              IF templog = YES AND sign_mm_det.switch = YES THEN templog = NO.
                              IF templog = NO AND sign_mm_det.switch = YES THEN templog = YES.
                              RUN reNumber(sign_mm_hdr.batchseq /* sign_mm_hdr.bedseq */,
                                          sign_mm_det.POSITION,
                                          (/*IF templog = YES THEN "Vertical" ELSE*/ "Horizontal"),
                                          signBed.userdec1,
                                          signBed.userdec2,
                                          OUTPUT cPosition,
                                          OUTPUT useHalf).
                          END.
                          ELSE IF sign_mm_hdr.bedseq = 81 THEN DO:
                            RUN reNumber(sign_mm_hdr.batchseq,
                                         (sign_mm_det.Position - 1),
                                         "Horizontal",
                                         signBed.userdec1,
                                         signBed.userdec2,
                                         OUTPUT cPosition,
                                         OUTPUT useHalf).    
                          END.
                          ELSE DO:
                              RUN reNumber(sign_mm_hdr.batchseq /* sign_mm_hdr.bedseq */,
                                           sign_mm_det.POSITION,
                                          (/*IF signBed.imageWidth < signbed.imageHeight THEN "Vertical" ELSE*/ "Horizontal"), /*only doing horizontal flips now*/
                                          signBed.userdec1,
                                          signBed.userdec2,
                                          OUTPUT cPosition,
                                          OUTPUT useHalf).
                          END.
                          
                          
                          IF iLoop = 2 AND INDEX(sign_mm_hdr.matltype,"Corex") > 0 THEN DO:
                          
                              IF AVAIL squ_ptdet THEN FIND FIRST squ_act NO-LOCK WHERE squ_act.ActSeq = 42 
                                                                                   AND squ_act.subseq = squ_ptdet.subseq
                                                                                   AND squ_act.order  = 100 NO-ERROR.
                              IF AVAIL squ_act THEN ASSIGN cPosition = sign_mm_det.Position
                                                           CADFirstCorex = TRUE.
                              
                          END.
                      END.

                      RUN xmltag ("seq",       cPosition,            INPUT-OUTPUT xmldata).
                      xmldata = xmldata + "<imagefile>" + cArtfile + "</imagefile>".
                      RUN xmltag ("invpart",   sign_mm_det.inv_part, INPUT-OUTPUT xmldata).
                      FIND FIRST signbeddet NO-LOCK WHERE signbeddet.seq = sign_mm_hdr.bedseq AND signbeddet.POSITION = IF iLoop = 2 AND index(sign_mm_hdr.matltype,"corex") > 0 THEN cPosition ELSE sign_mm_det.POSITION NO-ERROR.
                      IF AVAIL signbeddet THEN DO:

                          ASSIGN xCoord = IF iLoop = 2 AND useHalf THEN signbeddet.userdec1 /* - 48 */ ELSE signbeddet.userdec1
                                 yCoord = signbeddet.userdec2.
                          
      /*                     /*bed slider for rigged corex production fix*/                                                                                    */
                          IF iLoop = 2 AND index(sign_mm_hdr.matltype,"corex") > 0 AND CADFirstCorex = FALSE THEN DO: /*don't do this for arrows*/
                              RUN FindSpace(sign_mm_hdr.bedseq,OUTPUT xtraSpace).
                              ASSIGN xCoord = xCoord + xtraSpace.
                              IF LOOKUP(STRING(sign_mm_hdr.bedseq),"67,68,69") > 0 THEN xCoord = xCoord - xtraSpace. /*ryanle*/
                          END.
                          
                          RUN xmltag ("coordx", xCoord,              INPUT-OUTPUT xmldata).
                          RUN xmltag ("coordy", yCoord,              INPUT-OUTPUT xmldata).


                          specCorex = NO.
                          IF avail(squ_ptdet) AND avail(pt_det) AND index(IF NOT CAN-DO(cMiscParts,sign_mm_det.part_no) THEN pt_det.pt_substrate ELSE squ_ptdet.part_no,"Corex 10mm") > 0 THEN DO:
                              IF squ_ptdet.pt_height = 48 AND squ_ptdet.pt_width = 48 THEN specCorex = TRUE.
                              IF squ_ptdet.pt_height = 12 AND squ_ptdet.pt_width = 48  OR squ_ptdet.pt_height = 48 AND squ_ptdet.pt_width = 12  THEN specCorex = TRUE.
                          END.

                          IF isReflective = YES THEN DO: /*Ryanle - relfective corex 12/17/20*/
                              RUN xmltag("turn","NO",INPUT-OUTPUT xmldata).    
                          END.
                          ELSE DO:
                              IF INDEX(sign_mm_hdr.matltype,"corex") > 0  AND NOT specCorex THEN DO: 
                                      IF squ_ptdet.VERT_flute = NO AND squ_ptdet.horz_flute = NO THEN
                                          RUN xmltag ("turn"  , IF AVAIL pt_det AND pt_det.horz_flutes THEN "NO" ELSE "YES", INPUT-OUTPUT xmldata).
                                      ELSE IF  squ_ptdet.VERT_flutes = YES OR squ_ptdet.horz_flutes = YES THEN
                                          RUN xmltag ("turn"  , IF AVAIL squ_ptdet AND squ_ptdet.horz_flutes THEN "NO" ELSE "YES", INPUT-OUTPUT xmldata).
                                      ELSE
                                          RUN xmltag ("turn"  , IF sign_mm_det.switch THEN NOT signbeddet.userlog1 ELSE signbeddet.userlog1, INPUT-OUTPUT xmldata).
                              END.
                              ELSE IF sign_mm_hdr.runseq MODULO 2 = 0 THEN
                                  RUN xmltag ("turn"  , IF sign_mm_det.switch THEN NOT signbeddet.userlog1 ELSE signbeddet.userlog1, INPUT-OUTPUT xmldata).
                              ELSE
                                  RUN xmltag ("turn"  , NO, INPUT-OUTPUT xmldata).
                          END.
                          RUN xmltag ("toprow", signbeddet.userlog2, INPUT-OUTPUT xmldata).

                          actheight = 0. actwidth = 0.
                          IF AVAIL squ_ptdet THEN DO:
                              actwidth  = IF squ_ptdet.pressPrintingWidth  <> 0 THEN squ_ptdet.pressPrintingWidth  ELSE 0.
                              actheight = IF squ_ptdet.pressPrintingHeight <> 0 THEN squ_ptdet.pressPrintingHeight ELSE 0.
                          END.

                          IF AVAIL pt_det AND (actwidth = 0 OR actheight = 0) THEN DO:
                              actwidth  = IF pt_det.pressPrintingWidth  <> 0  THEN pt_det.pressPrintingWidth  ELSE 0.
                              actheight = IF pt_det.pressPrintingHeight <> 0  THEN pt_det.pressPrintingHeight ELSE 0.

                              IF actwidth = 0 OR actheight = 0 THEN DO:
                                  actwidth  = signbed.imagewidth.
                                  actheight = signbed.imageheight.
                              END.
                          END.
                          RUN xmltag ("ActualHeight",actheight, INPUT-OUTPUT xmldata).
                          RUN xmltag ("ActualWidth", actwidth , INPUT-OUTPUT xmldata).

                      END.
                      ASSIGN xmldata = xmldata + "</item>".
                  END.
                  ASSIGN xmldata = xmldata + "</images>".
                  ASSIGN xmlData = xmlData + "</bed>".
                  
                  ASSIGN xmlData = "<Host>Art-JM1</Host>" + "<XMLData>" + "<callingProgram>" + "MM.P" + "</callingProgram>" + "<Program>" + "Sart" + "</Program>" + "<XML>" + xmlData + "</XML>" + "</XMLData>".
                  
                  ASSIGN svxmlData = xmldata
                           iRunCnt = iRunCnt + 1.
                         
                                    
                  /*New XML Logic - 7/28/22 - RyanLe*/
                  COPY-LOB xmlData TO FILE "\\lowen\dfssancluster\Bullseye\JM\LIVE\XML\" + STRING(MTIME) + "_" + STRING(sign_mm_hdr.batchseq) + "-" + STRING(iLoop) + ".xml".

                  
              END.
          END. /*end sign_mm_hdr*/
     END. /*end template batches*/
     
     /*IF lDynNest THEN DO: /*dynamic nest batches YOUSAF LOOK HERE*/*/
        numRan = 0.
        FOR EACH sign_mm_hdr WHERE IF cBatch <> "" THEN sign_mm_hdr.rerun = YES  AND sign_mm_hdr.run_time = ? AND sign_mm_hdr.bedseq = 0 
                                                   ELSE sign_mm_hdr.RUN_time = ? AND sign_mm_hdr.bedseq = 0 BY sign_mm_hdr.runseq:
  
              CorexFO = FALSE.
              FOR EACH sign_mm_det WHERE sign_mm_det.batchseq = sign_mm_hdr.batchseq:
                  FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = sign_mm_det.itemseq NO-ERROR.
                  FIND FIRST squ_plan NO-LOCK WHERE squ_plan.itemseq = sign_mm_det.itemseq NO-ERROR.
                  
                  IF squ_ptdet.FoldOver = TRUE THEN CorexFO = TRUE.
                  
                  IF AVAILABLE squ_plan AND squ_plan.reflective = TRUE THEN CorexRef = TRUE.
              END.
              RELEASE squ_ptdet.
              
             IF sign_mm_hdr.zzchar_1 = "PrimeCenter" OR INDEX(sign_mm_hdr.matltype,"Corex") > 0 THEN DO:
                   IF LOOKUP(STRING(sign_mm_hdr.bedseq),CorexOver48) = 0 AND CorexRef = FALSE THEN NEXT.
                   IF CorexFO = TRUE THEN NEXT.
              END.
                       
            sign_mm_hdr.zzchar_2 = "".
            EMPTY TEMP-TABLE tempHdr.
            EMPTY TEMP-TABLE tempDet.
            nestcnt = 0.
            
            IF sign_mm_hdr.BatchNested THEN DO:
                /*get all data for subbatches*/
                FOR EACH nest_mm_hdr NO-LOCK WHERE nest_mm_hdr.NestBatchId = sign_mm_hdr.batchseq:

                    CREATE tempHdr.
                    BUFFER-COPY nest_mm_hdr TO tempHdr.
                    ASSIGN nestcnt             = nestcnt + 1
                           tempHdr.order       = nestcnt
                           tempHdr.BatchNested = NO
                           tempHdr.zzchar_1    = "multibatch".
                    
                    errCnt = 0.
                    FOR EACH nest_mm_det NO-LOCK WHERE nest_mm_det.batchseq = nest_mm_hdr.batchseq: 
                        CREATE tempDet.
                        BUFFER-COPY nest_mm_det TO tempDet.
                        errCnt = errCnt + 1.
                    END.
                END.

                cHotfolderseq = 0.
                RUN getHotfolder(tempHdr.batchseq,tempHdr.matlType,INPUT-OUTPUT cHotfolderseq,OUTPUT outputfile).
                
                CREATE tempHdr.
                BUFFER-COPY sign_mm_hdr TO tempHdr.
                ASSIGN nestcnt             = nestcnt + 1
                       tempHdr.order       = nestcnt
                       temphdr.BatchNested = YES
                       hdrCnt              = 0.
                       
                errCnt = 0.
                FOR EACH nest_mm_hdr NO-LOCK WHERE  nest_mm_hdr.NestBatchId = sign_mm_hdr.batchseq:
                    CREATE tempDet.
                    ASSIGN hdrCnt = hdrCnt + 1
                           tempDet.batchseq = sign_mm_hdr.batchseq
                           tempDet.POSITION = hdrCnt
                           tempDet.itemseq  = - 1
                           tempDet.inv_part = tempHdr.inv_part
                           tempDet.posx     = nest_mm_hdr.posx
                           tempDet.posy     = nest_mm_hdr.posy
                           tempDet.posxback = nest_mm_hdr.posxback
                           tempDet.posyback = nest_mm_hdr.posyback
                           tempDet.switch   = nest_mm_hdr.switch
                           tempDet.artfile  = "\\CALDERA-KEY\Public\Substrate Hotfolders\CadFiles\" + "batch" + string(nest_mm_hdr.batchseq) + "_" + "1.pdf".
                           
                                               
                    IF tempHdr.sides = 2 THEN tempDet.artfile  = tempdet.artfile + "," + outputfile + "\" + "batch" + string(nest_mm_hdr.batchseq) + "_" + "2.pdf" .
                    errCnt = errCnt + 1.
                END.
            END.
            ELSE DO:
                CREATE tempHdr.
                BUFFER-COPY sign_mm_hdr TO tempHdr.
                tempHdr.order = 1.
                
                FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.batchseq = sign_mm_hdr.batchseq:
                    CREATE tempDet.
                    BUFFER-COPY sign_mm_det TO tempDet.                    
                END.
            END.
            
            OUTPUT to "\\lowen\dfssancluster\Bullseye\Logs\testingmb.log" APPEND.
            FOR EACH tempHdr BY tempHdr.order:
                EXPORT DELIMITER "," TODAY TIME tempHdr.BatchNested tempHdr.batchseq tempHdr.NestBatchId.
                
                
                FOR EACH tempdet WHERE tempdet.batchseq = tempHdr.batchseq:
                    EXPORT DELIMITER "," "" tempdet.batchseq tempdet.artfile.
                        
                    IF tempHdr.BatchNested AND INDEX(tempdet.artfile,"batch") > 0 THEN DO:
                            IF sign_mm_hdr.zzchar_2 = "" THEN sign_mm_hdr.zzchar_2 = SUBSTRING(ENTRY(1,tempdet.artfile),INDEX(ENTRY(1,tempdet.artfile),"batch")).
                            ELSE sign_mm_hdr.zzchar_2 = sign_mm_hdr.zzchar_2 + "," + SUBSTRING(ENTRY(1,tempdet.artfile),INDEX(ENTRY(1,tempdet.artfile),"batch")).
                            EXPORT DELIMITER "," "zzchar_2: " sign_mm_hdr.zzchar_2.
                    END.
                END.
            END.
            OUTPUT close.
            
            FOR EACH tempHdr BY tempHdr.order:

                /*set to 'no' so that it only runs each reprint once*/
                IF sign_mm_hdr.rerun = YES THEN ASSIGN sign_mm_hdr.rerun = NO.

                FIND FIRST tParams NO-LOCK WHERE tParams.substrate = (IF tempHdr.BatchNested THEN "Steel 24ga" ELSE tempHdr.matltype) 
                                             AND (IF tempHdr.BatchNested THEN TRUE ELSE tParams.template = tempHdr.DynamicTemplate)  NO-ERROR.
                IF NOT AVAILABLE(tParams) THEN NEXT.
                
                iTime  = TIME.
                HdrNum = tempHdr.batchseq.

                tmpWidth = 0. tmpHeight = 0.
                IF AVAIL sign_mm_det THEN RELEASE sign_mm_det.
                IF AVAIL pt_det      THEN RELEASE pt_det.
                
                ASSIGN tmpWidth  = tParams.bedWidth
                       tmpHeight = tParams.bedHeight.
                
                /*do not send if file is a template*/
                IF tempHdr.runseq MODULO 2 <> 0 THEN NEXT.

                cHotfolderseq = 0.
                RUN getHotfolder(tempHdr.batchseq,tempHdr.matlType,INPUT-OUTPUT cHotfolderseq,OUTPUT outputfile).
                IF outputfile = "Error" THEN DO:
                    iErrorCnt = iErrorCnt + 1.
                    RUN trimbeds(STRING(tempHdr.batchseq)).
                    NEXT.
                END.
                
                /*do not send anything if the files were saved*/
                IF cBatch = "" THEN  
                    IF SEARCH(outputfile + "\" + "batch" + string(tempHdr.batchseq) + "_" + "1.pdf") <> ? THEN NEXT.

                IF NOT tempHdr.BatchNested THEN RUN saveDown(NO,tempHdr.batchseq,"",tempHdr.matlType,STRING(tmpHeight) + "x" + STRING(tmpWidth),NO,sign_mm_hdr.BatchNested). /*saves down to CS6*/

                /*verify that nothing failed failed on savedown...if it did then remove the temp records*/
                IF NOT tempHdr.BatchNested THEN DO:
                    IF NOT CAN-FIND(sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.batchseq = tempHdr.batchseq) AND
                       NOT CAN-FIND(nest_mm_hdr NO-LOCK WHERE nest_mm_hdr.batchseq = tempHdr.batchseq) THEN NEXT. /*if just deleted then move on*/
                    FOR EACH tempDet WHERE tempDet.batchseq = tempHdr.batchseq:
                        IF NOT CAN-FIND(FIRST sign_mm_det NO-LOCK WHERE sign_mm_det.itemseq = tempDet.itemseq) THEN
                            DELETE tempDet.
                    END.
                END.
                chgCorex = NO.

                RUN imagediff(tempHdr.batchseq,OUTPUT foundDiff).
                IF INDEX(tempHdr.matltype,"corex") > 0 THEN foundDiff = YES.
                IF tempHdr.bedseq = 0 THEN foundDiff = YES. /*make sure that sheeted/dynamic stuff has both sides printed*/
                
                ASSIGN iBatchCnt   = iBatchCnt + 1
                       iBatchSides = iBatchSides + (tempHdr.sides * tempHdr.qty)
                       cSides      = tempHdr.sides.
                DO iloop = 1 TO (IF NOT foundDiff THEN 1 ELSE cSides):
                    IF NOT AVAIL(tempHdr) THEN NEXT.

                    xmlData = "". svXMLDATA = "".
                    ASSIGN xmldata = xmldata + "<bed>".

                    
                    RUN xmltag ("bedType",     "CUSTOM",                   INPUT-OUTPUT xmldata).
                    RUN xmltag ("batchno",     tempHdr.batchseq,           INPUT-OUTPUT xmldata).
                    RUN xmltag ("bedseq",      tempHdr.bedseq,             INPUT-OUTPUT xmldata).
                    lmultibatch = FALSE.
                    ASSIGN cOutputfile = "".
                    
                    IF (tempHdr.zzchar_1 = "multibatch" OR index(temphdr.matltype,"poly") > 0 /*OR Index(temphdr.matltype,"Vinyl") > 0*/) AND iLOOP = 1 THEN   DO:                   
                        lmultibatch = TRUE.
                        
                        cOutputfile = "\\CALDERA-KEY\Public\Substrate Hotfolders\CadFiles\batch" + STRING(temphdr.batchseq) + "_" + STRING(iLoop) + ".pdf".
                        RUN xmltag ("outputfile",cOutputfile, INPUT-OUTPUT xmldata).                     
                    END.    
                    ELSE DO:
                        cOutputfile = outputfile + "\batch" + STRING(tempHdr.batchseq) + "_" + STRING(iLoop) + ".pdf".
                        RUN xmltag ("outputfile",  cOutputfile, INPUT-OUTPUT xmldata).
                    END.
                        
                    IF tempHdr.zzchar_1 <> "multibatch" AND index(temphdr.matltype,"poly") > 0 AND iLOOP = 1 THEN
                        RUN xmltag ("copyfile",  outputfile + "\" + "batch" + string(tempHdr.batchseq) + "_" + string(Iloop) + ".pdf", INPUT-OUTPUT xmldata).
                    ELSE
                        RUN xmltag ("copyfile", "", INPUT-OUTPUT xmldata).
                      
                    
                    /*Digitech additions - 12/18/20*/  
                    RUN xmltag ("tempfile",    tParams.template          , INPUT-OUTPUT xmldata).
                    RUN xmltag ("width",       STRING(tmpWidth) ,          INPUT-OUTPUT xmldata).
                    RUN xmltag ("height",      STRING(tmpHeight),          INPUT-OUTPUT xmldata).
                    

                    IF INDEX(tParams.template,"New AI Bed Template.ait") > 0 THEN RUN xmltag ("bedHeight","61",INPUT-OUTPUT xmldata).
                    ELSE RUN xmltag ("bedHeight",tParams.bedHeight,INPUT-OUTPUT xmldata).
                    
                    RUN xmltag ("materialtype",(IF tempHdr.BatchNested THEN "Steel" ELSE "Dynamic Steel"),     INPUT-OUTPUT xmldata).
                    RUN xmltag ("template",    (IF tempHdr.BatchNested AND iLoop = 1 THEN "true" ELSE "false"),INPUT-OUTPUT xmldata). /*only need for multinest beds but leave as true for now for testing*/

                    ASSIGN xmldata = xmldata + "<images>".
                    
                    FOR EACH tempDet WHERE tempDet.batchseq = tempHdr.batchseq:
                        ASSIGN cArtfile = "".
                        IF NUM-ENTRIES(tempDet.artfile,",") > 1 THEN
                             ASSIGN cArtfile = ENTRY(iLoop,tempDet.artfile,",").
                        ELSE ASSIGN cArtfile = tempDet.artfile.

                        IF NOT cArtfile MATCHES "*_CS6*" AND NOT tempHdr.BatchNested THEN
                            ASSIGN cartfile = REPLACE(cArtfile,".pdf","_CS6.pdf").

                        /*set to use savedown files in the cs6 folder*/
                        IF NOT tempHdr.BatchNested THEN 
                            ASSIGN cArtfile = cBatchImgLoc + "\" + ENTRY(NUM-ENTRIES(cArtfile,"\"),cArtfile,"\").

                        FIND pt_det NO-LOCK WHERE pt_det.part_no = tempDet.part_no NO-ERROR.
                        IF AVAIL squ_ptdet THEN RELEASE squ_ptdet.
                        FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = tempDet.itemseq AND squ_ptdet.TYPE <> "frame" NO-ERROR.
                        IF NOT AVAIL squ_ptdet THEN
                            FIND squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = tempDet.itemseq AND squ_ptdet.TYPE = "frame" NO-ERROR.

                         /*Send the left side then right*/
                        IF AVAILABLE squ_ptdet AND NOT squ_ptDet.steeltent AND NOT squ_ptdet.jackunit THEN DO:
                            IF iLoop = 1      AND index(tempHdr.matltype,"corex") = 0 AND cArtFile MATCHES "*_RT*"   THEN     cArtfile = REPLACE(cArtFile,"_RT","_LT"). 
                            ELSE IF iLoop = 2 AND index(tempHdr.matltype,"corex") = 0 AND cArtFile MATCHES "*_LT*"   THEN     cArtfile = REPLACE(cArtFile,"_LT","_RT"). 
                            ELSE IF iLoop = 1 AND index(tempHdr.matltype,"corex") > 0 AND cArtFile MATCHES "*_RT*"   THEN     cArtfile = REPLACE(cArtFile,"_RT","_LT"). 
                            ELSE IF iLoop = 2 AND index(tempHdr.matltype,"corex") > 0 AND cArtFile MATCHES "*_LT*"   THEN     cArtfile = REPLACE(cArtFile,"_LT","_RT"). 
                            ELSE IF iLoop = 1 AND index(tempHdr.matltype,"corex") > 0 AND NOT cArtFile MATCHES "*_LT*" 
                                AND NOT cArtFile MATCHES "*_RT*" AND NOT cArtFile MATCHES "*-left*" AND tempHdr.sides = 2
                                AND SEARCH(REPLACE(cArtFile,"_CS6","-Left_CS6")) <> ? THEN cArtfile = REPLACE(cArtFile,"_CS6","-Left_CS6").
                            ELSE IF iLoop = 2 AND index(tempHdr.matltype,"corex") > 0 AND cArtFile MATCHES "*-left*" THEN     cArtfile = REPLACE(cArtFile,"-left",""). 
                        END.

                        /*add code incase there is for ex 2 small size in place of one large sign*/
                        ASSIGN xmldata   = xmldata + "<item>"
                               cPosition = tempDet.POSITION
                               useHalf   = NO.
                               turning = tempDet.switch.
                               
                               
                        turning = tempDet.switch.
                        IF INDEX(tempHdr.matltype,"corex") > 0   AND avail(squ_ptdet) THEN
                            RUN corexHW (squ_ptdet.pt_height,squ_ptdet.pt_width,squ_ptdet.PressPrintingHeight,squ_ptdet.PressPrintingWidth,squ_ptdet.VERT_flutes,squ_ptdet.horz_flutes,OUTPUT turning, OUTPUT switched).
                    
                        RUN xmltag ("seq",    cPosition,                INPUT-OUTPUT xmldata).
                        xmldata = xmldata + "<imagefile>" + cArtfile + "</imagefile>".
                        RUN xmltag ("invpart",tempDet.inv_part,         INPUT-OUTPUT xmldata).
                        IF lmultibatch  THEN
                        DO:
 
                             IF tmpWidth = 31.4375 AND tmpHeight = 24.5 AND tParams.template = "\\lowen\dfssancluster\SignArt\DigitalBedTemplates\Adobe Templates\New AI 150 Poly Template.ait"  THEN DO: /*Ryanle - multibatch*/
                                RUN xmltag ("coordx", IF iLoop = 2 THEN tempDet.posxback ELSE tempDet.posx   , INPUT-OUTPUT xmldata).
                                RUN xmltag ("coordy","24.6875",INPUT-OUTPUT xmldata). 
                            END.
                            ELSE DO:
                                RUN xmltag ("coordx", IF iLoop = 2 THEN tempDet.posxback ELSE tempDet.posx, INPUT-OUTPUT xmldata).
                                RUN xmltag ("coordy", IF iLoop = 2 THEN tempDet.posyback ELSE tempDet.posy, INPUT-OUTPUT xmldata).
                            END.

                        END.
                        ELSE DO:

                            IF tmpWidth = 31.4375 AND tmpHeight = 24.5 AND tParams.template = "\\lowen\dfssancluster\SignArt\DigitalBedTemplates\Adobe Templates\New AI 150 Poly Template.ait"  THEN DO: /*Ryanle - multibatch*/
                                RUN xmltag ("coordx", IF iLoop = 2 THEN tempDet.posxback ELSE tempDet.posx   , INPUT-OUTPUT xmldata).
                                RUN xmltag ("coordy","24.6875",INPUT-OUTPUT xmldata). 
                            END.
                            ELSE DO:
                                RUN xmltag ("coordx", IF iLoop = 2 THEN tempDet.posxback ELSE tempDet.posx, INPUT-OUTPUT xmldata).
                                RUN xmltag ("coordy", IF iLoop = 2 THEN tempDet.posyback ELSE tempDet.posy, INPUT-OUTPUT xmldata).
                            END.

                        END.
                        IF tmpWidth = 31.4375 AND tmpHeight = 24.5 THEN turning = YES. /*ryanle - Multibatch*/ 
                        RUN xmltag ("turn"  , turning,                  INPUT-OUTPUT xmldata).                        
                        RUN xmltag ("toprow", "no",                     INPUT-OUTPUT xmldata).

                        ASSIGN actheight = 0
                               actwidth  = 0.
                        IF AVAIL squ_ptdet THEN ASSIGN actwidth  = IF squ_ptdet.pressPrintingWidth  <> 0 THEN squ_ptdet.pressPrintingWidth  ELSE 0
                                                       actheight = IF squ_ptdet.pressPrintingHeight <> 0 THEN squ_ptdet.pressPrintingHeight ELSE 0.
                        
                        IF AVAIL pt_det AND (actwidth = 0 OR actheight = 0) THEN ASSIGN actwidth  = IF pt_det.pressPrintingWidth  <> 0  THEN pt_det.pressPrintingWidth  ELSE 0
                                                                                        actheight = IF pt_det.pressPrintingHeight <> 0  THEN pt_det.pressPrintingHeight ELSE 0.
                        
                        /*ryanle - Multibatch*/
                        IF tmpWidth = 31.4375 AND tmpHeight = 24.5 THEN DO:
                            RUN xmltag ("ActualHeight",STRING(tmpWidth), INPUT-OUTPUT xmldata).
                            RUN xmltag ("ActualWidth" ,STRING(tmpHeight) , INPUT-OUTPUT xmldata).   
                        END.
                        ELSE DO:
                            RUN xmltag ("ActualHeight",actheight, INPUT-OUTPUT xmldata).
                            RUN xmltag ("ActualWidth", actwidth , INPUT-OUTPUT xmldata).
                        END.
                        ASSIGN xmldata = xmldata + "</item>".
                    END.
                    
                    ASSIGN xmldata   = xmldata + "</images>"
                           xmlData   = xmlData + "</bed>"
                           xmlData   = "<Host>Art-JM1</Host>" + "<XMLData>" + "<callingProgram>" + "MM.P" + "</callingProgram>" + "<Program>" + "Sart" + "</Program>" + "<XML>" + xmlData + "</XML>" + "</XMLData>"
                           svxmlData = xmlData
                           iRunCnt   = iRunCnt + 1.
                    
                    /*New XML Logic - 7/28/22 - RyanLe*/                                      
                    COPY-LOB xmlData TO FILE "\\fs02\bullseye\JM\LIVE\XML\" + STRING(MTIME) + "_" + STRING(temphdr.batchseq) + "-" + STRING(iLoop) + ".xml".
                    
                END.
            END. 
                         
        END. /*end for each sign_mm_hdr*/       
    /*END.*/
          
    iProgFinish = TIME.
END PROCEDURE.


PROCEDURE GET_Mach_Time:
    DEFINE INPUT  PARAMETER bSeq      AS INT NO-UNDO.
    DEFINE INPUT  PARAMETER sbSeq     AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER oMachMins AS DEC NO-UNDO.
    
    DEF VAR tmph          AS INT.
    DEF VAR tmpw          AS INT.
    DEF VAR ominutes      AS DEC.
    DEF VAR t34           AS LOG.
    DEF VAR detcnt        AS INT.
    DEF VAR outputfile    AS CHAR.

    DEF VAR cHotfolderseq AS INT INIT 0.

    FIND signbed NO-LOCK WHERE signbed.seq = sbSeq NO-ERROR.
    detcnt = 0.
    FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.batchseq = bSeq:
        detcnt = detcnt + 1.
    END.

    IF AVAIL signbed THEN DO:
        ASSIGN tmpH = signbed.imageHeight
               tmpW = signbed.imageWidth.
    END.
    ELSE DO:
        ASSIGN tmpH = 96
               tmpW = 48.
    END.

    RUN getHotfolder(sign_mm_hdr.batchseq,sign_mm_hdr.matlType,INPUT-OUTPUT cHotfolderseq, OUTPUT outputfile).
    IF INDEX(outputfile, "34 Text") > 0 THEN t34 = YES. ELSE t34 = NO.
    
    RUN squtime-flatbeddigitalprinting.p  (IF INDEX(sign_mm_hdr.matltype,"Steel") > 0 THEN "Steel" ELSE "Other",detCnt, dec(tmpH),dec(tmpW),IF sign_mm_hdr.sides = 2 THEN YES ELSE NO,IF INDEX(sign_mm_hdr.matltype,"Corex") > 0 THEN YES ELSE NO,cHotfolderseq,OUTPUT oMinutes,OUTPUT oMachMins).
    oMachMins = oMachMins * (sign_mm_hdr.qty - sign_mm_hdr.qty_printed).
END PROCEDURE.


PROCEDURE GetHotFolder:
    DEFINE INPUT        PARAMETER cBatch          AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER cMaterial       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER cHotFolderSeq   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT       PARAMETER cHotfolder      AS CHARACTER NO-UNDO.

    
    DEFINE VARIABLE cPrintNum               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE pLoop                   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE IsReflective            AS LOGICAL   NO-UNDO INITIAL FALSE.
    DEFINE VARIABLE CurrentQuality          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE SendError               AS LOGICAL   NO-UNDO.
    
    IsReflective = cMaterial MATCHES "*Reflect*" OR cMaterial MATCHES "*Ref*".
    
    IF IsReflective THEN DO:
        IF INDEX(cMaterial,"Omegabond Reflective") = 0 THEN cMaterial = REPLACE(cMaterial,"Reflective","").
        ELSE cHotFolderSeq = 35.
        
        IF INDEX(cMaterial,"Steel") > 0 THEN cMaterial = "Steel Ref " + SUBSTRING(cMaterial,7).
        ELSE IF INDEX(cMaterial,"Aluminum") > 0 THEN cMaterial = "Aluminum Ref " + SUBSTRING(cMaterial,10).
        ELSE IF INDEX(cMaterial,"Poly") > 0 THEN cMaterial = "Poly Ref " + SUBSTRING(cMaterial,7).
        ELSE IF INDEX(cMaterial,"PVC") > 0 THEN cMaterial = "PVC Ref". 
        ELSE IF INDEX(cMaterial,"Corex") > 0 THEN DO:
            IF INDEX(cMaterial,"4mm") > 0 THEN cMaterial = "Corex Ref 4mm".
            ELSE cMaterial = "Corex Ref 6mm".    
        END.
        
        cMaterial = REPLACE(cMaterial,".",""). 
        cMaterial = TRIM(cMaterial).
    END.
    
    RUN sethomefolder.
    
    IF cPrintTypes = "" THEN DO:
        FOR EACH pt_hotfolder NO-LOCK:
        IF NOT CAN-DO(cPrintTypes,pt_hotfolder.quality) THEN
            ASSIGN cPrintTypes = cPrintTypes + (IF cPrintTypes = "" THEN "" ELSE ",") + pt_hotfolder.quality.
        END.
    END.

    cPrintNum = 0. cHotFolder = "". /* cHotfolderseq = 0. */
    IF cBATCH > 1 AND cHotFolderSeq = 0 THEN DO:
        FOR FIRST b_mm_det NO-LOCK WHERE b_mm_det.batchseq = cBatch:
            FIND pt_hotfolder NO-LOCK WHERE pt_hotfolder.pt_hotfolderseq = b_mm_det.pt_hotfolderseq NO-ERROR.
            IF AVAILABLE pt_hotfolder THEN DO:
                cHotfolderSeq = pt_hotfolder.pt_hotfolderseq.
            END.
            IF AVAILABLE pt_hotfolder THEN RELEASE pt_hotfolder.
        END.
    END.

    IF cHotFolderSeq = 0 THEN DO:
        /*try and default it to pop 55 solid first*/
        FIND FIRST pt_hotfolder NO-LOCK WHERE pt_hotfolder.matlType = cMaterial AND pt_hotfolder.quality = "Pop 55 Solid"  NO-ERROR.
        IF AVAIL pt_hotfolder THEN DO:
            ASSIGN cHotfolderseq = pt_hotfolder.pt_hotfolderseq.
        END.

        IF cHotFolderSeq = 0 THEN DO:
            DO pLoop = 1 TO NUM-ENTRIES(cPrintTypes):
                FIND FIRST pt_hotfolder NO-LOCK WHERE pt_hotfolder.matlType = cMaterial AND pt_hotfolder.quality =  entry(pLoop,cPrintTypes) NO-ERROR.
                IF AVAIL pt_hotfolder THEN DO:
                    ASSIGN cHotfolderseq = pt_hotfolder.pt_hotfolderseq.
                END.
            END.
        END.
    END.
    
 
    SendError = FALSE.
    FIND FIRST pt_hotfolder NO-LOCK WHERE pt_hotfolder.pt_hotfolderseq = cHotFolderSeq NO-ERROR.
    IF AVAILABLE pt_hotfolder THEN DO:
        IF IsReflective THEN DO:
            CurrentQuality = pt_hotfolder.quality.
            FIND FIRST pt_hotfolder NO-LOCK WHERE pt_hotfolder.quality = CurrentQuality AND pt_hotfolder.matlType = cMaterial NO-ERROR.
        END.
        
        IF AVAILABLE pt_hotfolder THEN ASSIGN cHotfolder    = cHomeFolder + "\" + entry((NUM-ENTRIES(pt_hotfolder.pathway,"\") - 1),pt_hotfolder.pathway,"\") + "\" + entry(NUM-ENTRIES(pt_hotfolder.pathway,"\"),pt_hotfolder.pathway,"\")
                                              cHotfolderseq = pt_hotfolder.pt_hotfolderseq.
        ELSE SendError = TRUE.
    END.
    ELSE SendError = TRUE.
    
    IF SendError THEN DO:
        FOR EACH b_mm_det NO-LOCK WHERE b_mm_det.batchseq = cBatch: 
            RUN reportIssues (b_mm_det.itemseq,"MM-Hotfolder not found",cMaterial,cPrintNum,"","",cBatch,"","","").
        END.
        ASSIGN cHotFolder = "Error".
    END.

    IF AVAILABLE pt_hotfolder THEN RELEASE pt_hotfolder.
END PROCEDURE.


PROCEDURE GetDBase:
    DEFINE VARIABLE cDBParams AS CHAR NO-UNDO.

    cDBParams = DBPARAM(1).
    IF CAN-DO(cDBParams, "-S 2007") THEN cDBase = "Test".
    IF CAN-DO(cDBParams, "-S 2001") THEN cDBase = "Live".
    IF CAN-DO(cDBParams, "-S 2022") THEN cDBase = "QA".
    IF CAN-DO(cDBParams, "-S 2050") THEN cDBase = "Yesterday".
    IF OS-GETENV("computername") = "qbprod" OR OS-GETENV("computername") = "qbtest" THEN cDBase = "Live".

END PROCEDURE.


PROCEDURE GetTemplate:
    DEFINE INPUT  PARAMETER vType   AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER vPart   AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER vHeight AS DEC  NO-UNDO.
    DEFINE INPUT  PARAMETER vWidth  AS DEC  NO-UNDO.
    DEFINE INPUT  PARAMETER vTent   AS LOG  NO-UNDO.
    DEFINE OUTPUT PARAMETER vSeq    AS INT  NO-UNDO.
    DEFINE OUTPUT PARAMETER vSeq2   AS INT  NO-UNDO.
    DEFINE OUTPUT PARAMETER vSwitch AS LOG  NO-UNDO . /*height and width are switched - generally for corex vert flutes*/
    DEFINE VARIABLE vTent1          AS LOG  NO-UNDO.
    DEFINE VARIABLE vTent2          AS LOG  NO-UNDO.
    DEFINE VARIABLE vTent3          AS LOG  NO-UNDO.
    DEFINE VARIABLE vTent4         AS LOG  NO-UNDO.
    DEFINE VARIABLE tmpseq1         AS INT  NO-UNDO.
    DEFINE VARIABLE tmpseq2         AS INT  NO-UNDO.
    DEFINE VARIABLE point1          AS INT  NO-UNDO.
    DEFINE VARIABLE point2          AS INT  NO-UNDO.
    DEFINE VARIABLE tmpCnt1         AS INT  NO-UNDO.
    DEFINE VARIABLE tmpCnt2         AS INT  NO-UNDO.
    DEFINE VARIABLE specTemp        AS CHAR NO-UNDO INITIAL "21,22,32,69,52".
    DEFINE VARIABLE polytab         AS CHAR NO-UNDO INITIAL "P181,P182,P183,P183B,P188QR,P184,P189".
    DEFINE VARIABLE isPolyTab       AS LOG  NO-UNDO INITIAL NO. 
    DEFINE VARIABLE specCorex       AS LOG  NO-UNDO.
    DEFINE VARIABLE vertFlute       AS LOG  NO-UNDO.
    DEFINE VARIABLE horzFlute       AS LOG  NO-UNDO.
    DEFINE VARIABLE specSize        AS LOG  NO-UNDO.
    IF vHeight = 0 OR vWidth = 0 THEN RETURN. /*if not found height or width then throw out*/

    
    ASSIGN tmpseq1 = 0 
           tmpseq2 = 0 
           point1  = 0 
           point2  = 0 
           tmpCnt1 = 0 
           tmpCnt2 = 0 
           vSwitch = NO.

    /*aluminum and acrylic to run just like steel*/
    IF CAN-DO("omegabond,alumalite,Aluminum,Acrylic",vType) THEN ASSIGN vType = "Steel".

    IF CAN-DO("magnetic,Vinyl",vType) AND NOT specSize THEN RETURN.

    specCorex = FALSE.
    IF vType = "Corex" AND AVAIL pt_det THEN DO:
        IF INDEX(pt_det.pt_substrate,"Corex 10mm") > 0 THEN DO:
            IF vHeight = 48 AND vWidth = 48 THEN specCorex = TRUE.
        END.
    END.                                                
    
    IF vType = "Corex" AND NOT specCorex THEN DO: /*special beds for corex*/
        /*figure flutes*/
           
        IF squ_ptdet.VERT_flutes = NO AND squ_ptdet.horz_flutes = NO THEN DO:
            ASSIGN vertFlute = pt_det.VERT_flutes
                   horzFlute = pt_det.horz_flutes.
        END.
        ELSE DO:
            ASSIGN vertFlute = squ_ptdet.VERT_flutes
                   horzFlute = squ_ptdet.horz_flutes.
        END.
        
        IF CAN-FIND(pt_det NO-LOCK WHERE pt_det.part_no = vPart AND pt_det.longdesc MATCHES "*Arrow Shape*") THEN 
            ASSIGN vSeq  = 30
                   vSeq2 = 30.
        IF CAN-FIND(pt_det NO-LOCK WHERE pt_det.part_no = vPart AND pt_det.longdesc MATCHES "*Arrow Shape*") THEN DO:
            FIND FIRST signBed NO-LOCK WHERE signbed.matrlType = "Corex" AND signBed.imageHeight = vheight AND signBed.imageWidth = vwidth AND signbed.note BEGINS "Arrow" NO-ERROR.
            IF AVAIL signbed THEN DO:
                 ASSIGN vSeq  = signbed.seq
                        vSeq2 = signbed.tempseq.
            END.
            IF NOT AVAIL signbed THEN DO:
            FIND FIRST signBed NO-LOCK WHERE signbed.matrlType = "Corex" AND signBed.imageHeight = vwidth AND signBed.imageWidth = vheight AND signbed.note BEGINS "Arrow" NO-ERROR.
            IF AVAIL signbed THEN DO:
                 ASSIGN vSeq  = signbed.seq
                        vSeq2 = signbed.tempseq.
            END.
            END.
        END.
        ELSE IF CAN-FIND(pt_det NO-LOCK WHERE pt_det.part_no = vPart AND pt_det.longdesc MATCHES "*House Shape*") THEN
            ASSIGN vSeq  = 31
                   vSeq2 = 31. 
        ELSE DO:
            
            IF horzFlute THEN DO:
                FIND FIRST signBed NO-LOCK WHERE signbed.matrlType = "Corex" AND signBed.imageHeight = vHeight AND signBed.imageWidth = vWidth NO-ERROR.
                IF AVAIL signbed THEN DO:
                    ASSIGN vSeq  = signbed.seq
                           vSeq2 = signbed.TempSeq.
                END.
            END.
            ELSE DO:
                FIND FIRST signBed NO-LOCK WHERE signbed.matrlType = "Corex" AND signBed.imageHeight = vWidth AND signBed.imageWidth = vHeight NO-ERROR. 
                IF AVAILABLE signbed THEN DO:
                ASSIGN vSeq    = signbed.seq
                       vSeq2   = signbed.TempSeq
                       vSwitch = YES.
                END.
                ELSE DO:
                    FIND FIRST signBed NO-LOCK WHERE signbed.matrlType = "Corex" AND signBed.imageHeight = vHeight AND signBed.imageWidth = vWidth NO-ERROR.
                    IF AVAIL signbed THEN DO:
                        ASSIGN vSeq  = signbed.seq
                               vSeq2 = signbed.TempSeq.      
                    END.
                END.
            END.
        END.
        IF NOT vertFlute AND NOT horzFlute THEN DO:
            ASSIGN vSeq  = 0
                   vSeq2 = 0.
        END.
    END.
    ELSE DO:
        vTent1 = NO. vTent2 = NO. vTent3 = NO. vtent4 = NO.
        IF vType = "Poly" THEN DO:
            IF pt_det.FoldOver OR vTent THEN DO:
                IF (vHeight = 18 AND vWidth = 24) OR (pt_det.PressPrintingHeight = 36 AND pt_det.PressPrintingWidth = 24) THEN vTent1 = TRUE.
                IF ((vHeight = 24 AND vWidth = 24) OR (pt_det.pt_Height = 48 AND pt_det.pt_width = 24) OR (pt_det.PressPrintingHeight = 48 AND pt_det.PressPrintingWidth = 24)) THEN vTent2 = TRUE.
                IF (vHeight = 24 AND vWidth = 30) OR (pt_det.PressPrintingHeight = 48 AND pt_det.PressPrintingWidth = 30) THEN vTent3 = TRUE.
                IF (vHeight = 30 AND vWidth = 24) OR (pt_det.PressPrintingHeight = 60 AND pt_det.PressPrintingWidth = 24) THEN vTent4 = TRUE.
                IF vTent1 THEN 
                    ASSIGN vSeq  = 21
                           vSeq2 = 21.
                ELSE IF vTent2 THEN
                    ASSIGN vSeq  = 22
                           vSeq2 = 22.
                ELSE IF vTent3 THEN
                    ASSIGN vSeq  = 32
                           vSeq2 = 32.
                ELSE IF vTent4 THEN
                    ASSIGN vSeq  = 94
                           vSeq2 = 94.
            END.
            /*if its a polytab rider put on bed 8*/
            IF AVAIL so_items THEN DO:
                IF INDEX(so_items.partdesc,"poly") > 0 AND INDEX(so_items.partdesc,"tab") > 0 AND INDEX(so_items.partdesc,"estab") = 0 THEN ASSIGN isPolyTab = TRUE.
            END.
            IF INDEX(pt_det.longdesc,"poly") > 0 AND INDEX(pt_det.longdesc,"tab") > 0 AND INDEX(pt_det.longdesc,"estab") = 0 THEN ASSIGN isPolyTab = TRUE.

            IF vpart <> "39P183" THEN DO:
                DO iloop = 1 TO NUM-ENTRIES(polytab):
                    IF vpart MATCHES "*" + ENTRY(iLoop,polytab) THEN DO:
                        ASSIGN vSeq  = 8
                               vSeq2 = 8.
                        RETURN.
                    END.
                END.
                IF isPolyTab THEN DO:
                    ASSIGN vSeq  = 8
                           vSeq2 = 8.
                    RETURN.
                END.
            END.
        END.
        IF vSeq = 0 THEN DO:
            FIND FIRST signBed NO-LOCK WHERE signBed.imageHeight = vHeight AND signBed.imageWidth = vWidth AND signbed.matrlType <> "Corex" NO-ERROR.
            IF AVAIL signbed THEN DO:
                ASSIGN vSeq  = signbed.seq
                       vSeq2 = signbed.TempSeq.
            END.
            ELSE DO: 
                FIND FIRST signBed NO-LOCK WHERE signBed.imageHeight = vWidth AND signBed.imageWidth = vHeight AND signbed.matrlType <> "Corex" NO-ERROR. 
                IF AVAILABLE signbed THEN DO:
                    ASSIGN vSeq    = signbed.seq
                           vSeq2   = signbed.TempSeq
                           vSwitch = YES.
                END.
                ELSE DO:
                    FOR EACH signbed NO-LOCK WHERE signbed.matrlType <> "Corex" BY signbed.imageheight BY signbed.imagewidth :
                        IF CAN-DO(specTemp, STRING(signbed.seq)) THEN NEXT.
                        IF vWidth <= signbed.imagewidth AND vHeight <= signbed.imageheight AND signbed.userdec1 * signbed.userdec2 > tmpcnt1 THEN DO:
                            ASSIGN tmpseq1 = signbed.seq 
                                   point1  = signbed.tempseq
                                   tmpCnt1 = signbed.userdec1 * signbed.userdec2.
                        END.
                        IF vHeight <= signbed.imagewidth AND vWidth <= signbed.imageheight AND signbed.userdec1 * signbed.userdec2 > tmpcnt2 THEN DO:
                            ASSIGN tmpseq2 = signbed.seq 
                                   point2  = signbed.tempseq
                                   tmpCnt2 = signbed.userdec1 * signbed.userdec2.
                        END.
                    END.
                    
                    /*found beds both ways now see which is more efficient*/ 
                    IF tmpCnt1 > tmpCnt2 THEN
                        ASSIGN vSeq  = tmpseq1
                               vSeq2 = point1.
                    ELSE
                        ASSIGN vSeq    = tmpseq2
                               vSeq2   = point2
                               vSwitch = YES.

                    IF vHeight = 8 AND vWidth = 18 THEN
                        ASSIGN vSeq  = 6
                               vSeq2 = 6. 
                END.
            END.
        END.
    END.

       
END PROCEDURE.


PROCEDURE GetQty:
    DEFINE INPUT  PARAMETER pSeq    AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER pRan    AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER pNeed   AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER inQueue AS LOG NO-UNDO.
    
    DEFINE VARIABLE reprintsRan     AS INTEGER NO-UNDO.
    DEFINE VARIABLE reprintsNeeded  AS INTEGER NO-UNDO.
    DEFINE VARIABLE reprintsInQueue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE tmpInt          AS INT NO-UNDO.

    inQueue = NO.
    FIND FIRST buf_so_items NO-LOCK WHERE buf_so_items.itemseq = pSeq NO-ERROR.
    FIND FIRST squdet NO-LOCK WHERE squdet.itemseq = pSeq AND squdet.TYPE <> "Frame" NO-ERROR.
    IF AVAIL buf_so_items AND AVAIL squdet THEN DO:
    
        ASSIGN pRan   = 0
               tmpint = 0.
        FOR EACH buf_mm_det NO-LOCK WHERE buf_mm_det.itemseq = pSeq BREAK BY buf_mm_det.batchseq:
            tmpint = tmpint + 1.
            IF LAST-OF(buf_mm_det.batchseq) THEN DO:
                /*inQueue = NO.*/
                FIND buf_mm_hdr OF buf_mm_det NO-LOCK NO-ERROR.
                IF AVAIL buf_mm_hdr THEN DO:
                    IF buf_mm_hdr.RUN_time = ? THEN inQueue = YES.
                    
                    IF buf_mm_hdr.qty_printed > 0 THEN pRan = pRan + (tmpint * buf_mm_hdr.qty_printed).
                    ELSE pRan = pRan + (tmpint * buf_mm_hdr.qty).

                    tmpInt = 0.
                END.
                ELSE DO:
                    ASSIGN pRan   = pRan + tmpInt
                           tmpInt = 0.
                END.
            END.
        END.
        
        pNeed = IF squdet.steeltent OR squdet.jackunit THEN ((2 * buf_so_items.orderqty) - pRan) ELSE buf_so_items.orderqty - pRan.
        
        FOR EACH sign_mm_reprint NO-LOCK WHERE sign_mm_reprint.itemseq = pSeq AND sign_mm_reprint.completed = FALSE:
            RUN GetReprintQty (sign_mm_reprint.ReprintId, OUTPUT reprintsRan, OUTPUT reprintsNeeded, OUTPUT reprintsInQueue).
            ASSIGN pNeed = (IF pNeed <= 0 THEN 0 ELSE pNeed) + reprintsNeeded.
        END.
        
        IF reprintsNeeded <> 0 AND pNeed > buf_so_items.orderqty THEN pneed = buf_so_items.orderqty.
    END.
    
    IF AVAIL buf_so_items THEN RELEASE buf_so_items.
    IF AVAIL squdet       THEN RELEASE squdet.
    IF AVAIL buf_mm_hdr   THEN RELEASE buf_mm_hdr.
END PROCEDURE.


PROCEDURE GetReprintQty:
    DEFINE INPUT  PARAMETER pReprintId AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER pRan       AS INT NO-UNDO INITIAL 0.
    DEFINE OUTPUT PARAMETER pNeed      AS INT NO-UNDO INITIAL 0.
    DEFINE OUTPUT PARAMETER inQueue    AS LOG NO-UNDO INITIAL FALSE.
    
    FIND buf_mm_reprint NO-LOCK WHERE buf_mm_reprint.ReprintId = pReprintId AND buf_mm_reprint.COMPLETED = FALSE NO-ERROR.
    IF AVAILABLE buf_mm_reprint THEN DO:
        FOR EACH buf_mm_det NO-LOCK WHERE buf_mm_det.ReprintId = buf_mm_reprint.ReprintId:
            inQueue = TRUE.
            pRan = pRan + 1. 
        END.
        pNeed = buf_mm_reprint.qty - pRan.
    END.
END PROCEDURE.


PROCEDURE Grouping:
    DEFINE INPUT PARAMETER cItemSeq AS CHAR NO-UNDO.

    DEFINE VARIABLE tmpInt          AS INT  NO-UNDO.
    DEFINE VARIABLE b_ok            AS LOG  NO-UNDO.
    DEFINE VARIABLE nextSeq         AS INT  NO-UNDO.
    DEFINE VARIABLE pcnt            AS INT  NO-UNDO INITIAL 0.
    DEFINE VARIABLE numLeft         AS INT  NO-UNDO.
    DEFINE VARIABLE tmpIseq         AS INT  NO-UNDO.
    DEFINE VARIABLE tmpLog          AS LOG  NO-UNDO.

    
    FOR EACH signbed NO-LOCK:
        FOR EACH ttArt WHERE ttArt.ttTempSeq   = signbed.Seq 
                         AND ttArt.ttDynamNest = FALSE 
                          BY ttArt.ttType BY ttArt.ttInvPart BY ttArt.ttSides BY ttart.ttHotFolder BY ttArt.ttSo BY ttArt.ttItemNo BY ttArt.ttCustNo:
                             
            IF ttart.ttqty < 1 THEN NEXT.
            IF tmpInt <> ttart.ttItemseq THEN DO:
                ASSIGN tmpInt = ttItemseq.
            END.

            /**** Might be able to add the "rule of one" here and save us a few beds *****/
            /*create & fill entire bed,else make complete mix bed, else start new/run over*/
            IF ttart.ttqty = (signbed.userdec1 * signbed.userdec2) THEN DO:


                nextSeq = NEXT-VALUE(seq-mm-batch).
                DO WHILE CAN-FIND(sign_mm_hdr WHERE sign_mm_hdr.batchseq = nextSeq):
                    nextSeq = NEXT-VALUE(seq-mm-batch).
                END.
                
                CREATE sign_mm_hdr.
                ASSIGN sign_mm_hdr.BATCHseq         = nextseq
                       sign_mm_hdr.runseq           = 0
                       sign_mm_hdr.crt_date         = TODAY
                       sign_mm_hdr.crt_time         = TIME
                       sign_mm_hdr.RUN_date         = ?
                       sign_mm_hdr.RUN_time         = ?
                       sign_mm_hdr.matlType         = ttArt.ttType
                       sign_mm_hdr.sides            = ttArt.ttSides
                       sign_mm_hdr.bedseq           = ttArt.ttTempSeq
                       sign_mm_hdr.inv_part         = ttArt.ttInvpart
                       sign_mm_hdr.qty              = 1
                       sign_mm_hdr.rerun            = IF cItemseq <> "" THEN YES ELSE NO
                       sign_mm_hdr.reprint          = IF cItemseq <> "" THEN YES ELSE NO
                       sign_mm_hdr.fullbed          = TRUE /*full bed*/
                       sign_mm_hdr.pt_hotfolderseq  = ttArt.ttHotFolder
                       . 

                ASSIGN pCnt = 1.
                DO iLoop = 1 TO ttart.ttqty:
                    /* get sign_mm_det.artlinkseq */
                    FIND FIRST so_art NO-LOCK WHERE so_art.itemseq = ttart.ttitemseq AND so_art.artfile = ENTRY(1,ttart.ttfile,",") NO-ERROR.
                    
                    CREATE sign_mm_det.
                    ASSIGN sign_mm_det.batchseq        = nextseq
                           sign_mm_det.part_no         = ttArt.ttPart
                           sign_mm_det.artlinkseq      = IF AVAIL so_art THEN so_art.disp_order ELSE 0
                           sign_mm_det.itemseq         = ttArt.ttItemseq
                           sign_mm_det.artfile         = ttArt.ttFile
                           sign_mm_det.inv_part        = ttArt.ttInvPart
                           sign_mm_det.POSITION        = pCnt
                           sign_mm_det.due_date        = ttArt.ttDue
                           sign_mm_det.pt_hotfolderseq = ttArt.ttHotfolder
                           sign_mm_det.PointerSeq      = sign_mm_hdr.bedseq
                           sign_mm_det.switch          = ttArt.ttSwitch
                           sign_mm_det.zzlog_1         = IF ttArt.ttCustNo = "53550" THEN YES ELSE NO
                           sign_mm_det.reprintId       = ttArt.ttReprintId
                           pCnt                        = pCnt + 1.
                           
                    RELEASE so_art NO-ERROR.
                END.
                IF AVAIL sign_mm_det THEN RELEASE sign_mm_det.
                IF AVAIL sign_mm_hdr THEN RELEASE sign_mm_hdr.
            END.
            ELSE DO:
                /*attempt complete mix batch*/
                tmpLog = FALSE.
                FOR EACH sign_mm_hdr WHERE sign_mm_hdr.RUN_date = ? AND sign_mm_hdr.fullbed = FALSE AND sign_mm_hdr.bedseq = ttArt.ttTempSeq 
                    AND sign_mm_hdr.sides = ttart.ttsides AND sign_mm_hdr.matlType = ttArt.ttType AND (IF cItemseq <> "" THEN sign_mm_hdr.reprint = YES ELSE TRUE):
                    IF sign_mm_hdr.inv_part <> ttart.ttinvpart THEN NEXT. /*must have save inv part number on bed*/
                    IF sign_mm_hdr.pt_hotfolderseq <> ttart.ttHotfolder THEN NEXT. /*cant run to diff print speeds at same time*/
                    IF tmpLog = TRUE THEN LEAVE.
                    FIND LAST sign_mm_det NO-LOCK OF sign_mm_hdr NO-ERROR.
                    IF AVAIL sign_mm_det AND ((signbed.userdec1 * signbed.userdec2) - sign_mm_det.POSITION) = ttart.ttqty THEN DO:
                        ASSIGN pCnt = sign_mm_det.POSITION + 1.
                        DO iLoop = 1 TO ttart.ttqty:
                            
                            FIND FIRST so_art NO-LOCK WHERE so_art.itemseq = ttart.ttitemseq AND so_art.artfile = ENTRY(1,ttart.ttfile,",") NO-ERROR. 
                            
                            CREATE b_mm_det.
                            ASSIGN b_mm_det.batchseq        = sign_mm_hdr.batchseq
                                   b_mm_det.part_no         = ttArt.ttPart
                                   b_mm_det.itemseq         = ttArt.ttItemseq
                                   b_mm_det.artlinkseq      = IF AVAIL so_art THEN so_art.disp_order ELSE 0
                                   b_mm_det.artfile         = ttArt.ttFile
                                   b_mm_det.inv_part        = ttArt.ttInvPart
                                   b_mm_det.due_date        = ttArt.ttDue
                                   b_mm_det.POSITION        = pCnt
                                   b_mm_det.PointerSeq      = sign_mm_hdr.bedseq
                                   b_mm_det.switch          = ttArt.ttSwitch
                                   b_mm_det.pt_hotfolderseq = ttArt.ttHotFolder
                                   b_mm_det.zzlog_1         = IF ttArt.ttCustNo = "53550" THEN YES ELSE NO
                                   b_mm_det.reprintId       = ttArt.ttReprintId
                                   pCnt                     = pCnt + 1
                                   sign_mm_hdr.fullbed      = TRUE.
                                   
                                   RELEASE so_art NO-ERROR.
                        END.
                        ASSIGN tmpLog = TRUE.
                    END.
                    IF AVAIL sign_mm_det THEN RELEASE sign_mm_det.
                    IF AVAIL sign_mm_hdr THEN RELEASE sign_mm_hdr.
                    IF AVAIL b_mm_det    THEN RELEASE b_mm_det.
                END.
                IF tmpLog = FALSE THEN DO:
                    /*create new header and cycle through dets*/
                    IF ttart.ttqty <> 0 THEN DO:
                        ASSIGN pCnt = 0 nextseq = 0.
                        DO WHILE ttart.ttqty <> 0:
                            IF pCnt = 0 THEN DO:
                                nextSeq = NEXT-VALUE(seq-mm-batch).
                                DO WHILE CAN-FIND(sign_mm_hdr WHERE sign_mm_hdr.batchseq = nextSeq):
                                    nextSeq = NEXT-VALUE(seq-mm-batch).
                                END.
                                CREATE sign_mm_hdr.
                                ASSIGN sign_mm_hdr.BATCHseq         = nextseq
                                       sign_mm_hdr.runseq           = 0
                                       sign_mm_hdr.crt_date         = TODAY
                                       sign_mm_hdr.crt_time         = TIME
                                       sign_mm_hdr.RUN_date         = ?
                                       sign_mm_hdr.RUN_time         = ?
                                       sign_mm_hdr.matlType         = ttArt.ttType
                                       sign_mm_hdr.sides            = ttArt.ttSides
                                       sign_mm_hdr.bedseq           = ttArt.ttTempSeq
                                       sign_mm_hdr.inv_part         = ttArt.ttInvPart
                                       sign_mm_hdr.qty              = 1
                                       sign_mm_hdr.rerun            = IF cItemseq <> "" THEN YES ELSE NO
                                       sign_mm_hdr.reprint          = IF cItemseq <> "" THEN YES ELSE NO
                                       sign_mm_hdr.pt_hotfolderseq  = ttArt.ttHotFolder
                                       pCnt                         = 1.

                                

                            END.
                       
                            /*Find the art link*/
                            FIND FIRST so_art NO-LOCK WHERE so_art.itemseq = ttart.ttitemseq AND so_art.artfile = ENTRY(1,ttart.ttfile,",") NO-ERROR.                          
                            
                            CREATE sign_mm_det.
                            ASSIGN sign_mm_det.batchseq         = nextseq
                                   sign_mm_det.part_no          = ttArt.ttPart
                                   sign_mm_det.itemseq          = ttArt.ttItemseq
                                   sign_mm_det.artlinkseq       = IF AVAIL so_art THEN so_art.disp_order ELSE 0
                                   sign_mm_det.artfile          = ttArt.ttFile
                                   sign_mm_det.inv_part         = ttArt.ttInvPart
                                   sign_mm_det.due_date         = ttArt.ttDue
                                   sign_mm_det.PointerSeq       = sign_mm_hdr.bedseq
                                   sign_mm_det.pt_hotfolderseq  = ttArt.ttHotFolder
                                   sign_mm_det.switch           = ttArt.ttSwitch
                                   sign_mm_det.zzlog_1          = IF ttArt.ttCustNo = "53550" THEN YES ELSE NO
                                   sign_mm_det.POSITION         = pcnt
                                   sign_mm_det.reprintId        = ttArt.ttReprintId
                                   pCnt                         = pCnt + 1
                                   ttart.ttqty                  = ttArt.ttQty - 1.
                                   
                            IF sign_mm_det.POSITION = (signbed.userdec1 * signbed.userdec2) THEN ASSIGN pCnt                = 0
                                                                                                        sign_mm_hdr.fullbed = TRUE.
                                       
                           RELEASE so_art NO-ERROR.
                        END.
                        IF AVAIL sign_mm_hdr THEN RELEASE sign_mm_hdr.
                        IF AVAIL sign_mm_det THEN RELEASE sign_mm_det.
                    END.
                END.
            END.
        END.
    END.
END PROCEDURE.


PROCEDURE GroupingCorex:
    DEFINE INPUT PARAMETER starter AS CHAR NO-UNDO.
    
    DEFINE VARIABLE xmlData      AS CHARACTER.
    DEFINE VARIABLE sub          AS CHARACTER.
    DEFINE VARIABLE totFiles     AS INT.
    DEFINE VARIABLE fName        AS CHARACTER.
    DEFINE VARIABLE tmpfName     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE tmpfLoc      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ALS          AS INT.
    DEFINE VARIABLE c_msg        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE i            AS INT        NO-UNDO.
    DEFINE VARIABLE FolderList   AS CHAR       NO-UNDO.
    DEFINE VARIABLE cRecipe      AS CHAR       NO-UNDO.
    DEFINE VARIABLE FileList     AS "System.Collections.Generic.List<character>" NO-UNDO.
    DEFINE VARIABLE FailList     AS "System.Collections.Generic.List<character>" NO-UNDO.
    DEFINE VARIABLE art          AS ArtGenerator.
    DEFINE VARIABLE startTime    AS INT.
    DEFINE VARIABLE CorexOver48  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE outputFile   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE FailedFile   AS CHAR       NO-UNDO.
    DEFINE VARIABLE FailedSO     AS CHAR       NO-UNDO.
    DEFINE VARIABLE FailedNo     AS CHAR       NO-UNDO.
    DEFINE VARIABLE inVars       AS JsonObject NO-UNDO.
    DEFINE VARIABLE outVars      AS JsonObject NO-UNDO.
    DEFINE VARIABLE DateCodeYN   AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cSize        AS CHAR       NO-UNDO.
    DEFINE VARIABLE cResponse    AS CHAR       NO-UNDO.
    DEFINE VARIABLE xmlMyrtle    AS CHAR       NO-UNDO.
    
    inVars = NEW JsonObject().
    
    
    {mgseclist.i "CorexOver48" CorexOver48}
    art      = NEW ArtGenerator().
    FileList = NEW "System.Collections.Generic.List<character>"().
    FailList = NEW "System.Collections.Generic.List<character>"().
    
    
        FOR EACH ttArt BY ttArt.ttType BY ttArt.ttinvpart BY ttArt.ttsides BY ttArt.tthotfolder BY ttArt.ttso BY ttArt.ttItemno BY ttArt.ttCustNo:           
           
           IF ttArt.ttQty < 1 AND INDEX(ttArt.ttType,"Corex") > 0 THEN DO:  
               FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = ttArt.ttItemseq NO-ERROR.
               FIND FIRST so_file NO-LOCK WHERE so_file.so_no = so_items.so_no NO-ERROR.      
               RUN ReportIssues(so_items.itemseq,"MM-Prime Center Fail - Qty",so_items.so_no,STRING(so_items.ITEM_no),"","Prime Center Fail - Qty","","","","").
               NEXT.
           END.
           
           IF INDEX(ttArt.ttType,"Corex") = 0 OR ttArt.ttQty < 1 THEN NEXT.
           
           DateCodeYN = FALSE.
                 
           FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = ttArt.ttItemseq NO-ERROR.
           FIND FIRST squ_plan  NO-LOCK WHERE squ_plan.itemseq  = ttArt.ttItemseq NO-ERROR.
           
           IF INDEX(ttArt.ttType,"Corex") > 0 THEN DO:
               IF LOOKUP(STRING(ttArt.ttTempSeq),CorexOver48) > 0 AND squ_ptdet.FoldOver = FALSE THEN NEXT.
               
               IF squ_plan.reflective = TRUE OR (INDEX(squ_ptdet.longdesc,"Reflective") > 0 OR INDEX(squ_ptdet.longdesc,"Ref") > 0) THEN NEXT.
           END.                   
                      
           ASSIGN xmlData = "{PrimeCenter-xml.i}"
                  sub     = ""
                  cRecipe = "".  
           
           /*get substrate - 4mm/6mm/8mm/10mm aka "gang" */
           FIND FIRST partfile NO-LOCK WHERE partfile.part_no = ttArt.ttPart NO-ERROR.         
           IF AVAIL partfile THEN DO:
               IF INDEX(partfile.partdesc,"4mm") > 0 THEN DO:
                   ASSIGN xmlData = REPLACE(xmlData,"[gang]","4mm")
                          sub     = "4mm".    
               END.
               ELSE IF INDEX(partfile.partdesc,"6mm") > 0 THEN DO:
                   ASSIGN xmlData = REPLACE(xmlData,"[gang]","6mm")
                          sub     = "6mm".    
               END.
               ELSE IF INDEX(partfile.partdesc,"8mm") > 0 THEN DO:
                   ASSIGN xmlData = REPLACE(xmlData,"[gang]","8mm")
                          sub     = "8mm".
               END.
               ELSE IF INDEX(partfile.partdesc,"10mm") > 0 THEN DO:
                   ASSIGN xmlData = REPLACE(xmlData,"[gang]","10mm")
                          sub     = "10mm".
               END.                 
           END.
           RELEASE partfile.
           
           
           /* Vert/Horz Flutes - "recipe"*/ 
           
           FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = ttArt.ttItemseq AND squ_ptdet.type <> "FRAME" NO-ERROR.         
           IF AVAIL squ_ptdet THEN DO:
               ASSIGN cRecipe = "Corex_" + (IF squ_ptdet.DigitalSF THEN "SF" ELSE "DF") 
                              + "_"
                              + (IF squ_ptdet.horz_flutes = TRUE THEN "H" ELSE "V")
                              + (IF squ_ptdet.FoldOver = TRUE THEN "_Foldover" ELSE "").    
               
               
               xmlData = REPLACE(xmlData,"[recipe]",cRecipe).
               
               
               /*double check [gang] - generic parts fail in above code*/  
               IF /*INDEX(squ_ptdet.pt_substrate,"Corop 4mm") > 0 OR*/ INDEX(squ_ptdet.pt_substrate,"Corex 4mm") > 0 /*OR INDEX(squ_ptdet.pt_substrate,"4mil Corex") > 0*/ THEN DO:
                   ASSIGN xmlData = REPLACE(xmlData,"[gang]","4mm")
                          sub     = "4mm".    
               END.
               ELSE IF /*INDEX(squ_ptdet.pt_substrate,"Corop 6mm") > 0 OR*/ INDEX(squ_ptdet.pt_substrate,"Corex 6mm") > 0 /*OR INDEX(squ_ptdet.pt_substrate,"6mil Corex") > 0*/ THEN DO:
                   ASSIGN xmlData = REPLACE(xmlData,"[gang]","6mm")
                          sub     = "6mm".    
               END.
               ELSE IF /*INDEX(squ_ptdet.pt_substrate,"Corop 8mm") > 0 OR*/ INDEX(squ_ptdet.pt_substrate,"Corex 8mm") > 0 /*OR INDEX(squ_ptdet.pt_substrate,"8mil Corex") > 0*/  THEN DO:
                   ASSIGN xmlData = REPLACE(xmlData,"[gang]","8mm")
                          sub     = "8mm".
               END.
               ELSE IF /*INDEX(squ_ptdet.pt_substrate,"Corop 10mm") > 0 OR*/ INDEX(squ_ptdet.pt_substrate,"Corex 10mm") > 0 /*OR INDEX(squ_ptdet.pt_substrate,"10mil Corex") > 0*/ THEN DO:
                   ASSIGN xmlData = REPLACE(xmlData,"[gang]","10mm")
                          sub     = "10mm".
               END.
               
               cSize = STRING(squ_ptdet.pressprintingheight) + "x" + STRING(squ_ptdet.pressprintingwidth).
           END.
           RELEASE squ_ptdet.          
           
           /*Get ALS#*/
           ASSIGN ALS = 0.
           FIND FIRST so_item NO-LOCK WHERE so_items.itemseq = ttArt.ttItemseq NO-ERROR.
           
           FIND FIRST so_art NO-LOCK WHERE so_art.itemseq = ttArt.ttItemseq AND so_art.type = "mini" AND so_art.artfile = ENTRY(1,ttArt.ttFile) NO-ERROR.
           IF AVAIL so_art THEN DO:
               ALS = so_art.disp_order.
               IF so_art.Date_Code <> "None" THEN DateCodeYN = YES.
           END.
           ELSE DO:
               IF CAN-FIND(FIRST pt_det NO-LOCK WHERE pt_det.part_no = so_items.part_no
                                                  AND LOOKUP(pt_det.datecodecolor,"White,Black") > 0) THEN DateCodeYN = YES. 
               
               IF CAN-FIND(FIRST zz_file NO-LOCK WHERE zz_file.zz_key1 = "SeqNumberLine" AND zz_file.zz_key2 = ttArt.ttPart) THEN DO:                                   
                   ALS = INT(SUBSTRING(ttArt.ttFile,R-INDEX(ttArt.ttFile,"-") + 1, LENGTH(ttArt.ttFile) - INDEX(ttArt.ttFile,".pdf") + 1)).
               END.
           END.
           RELEASE so_art.
           
           /*create "CS6" file copy*/
           
           IF SEARCH(cBatchImgLoc + REPLACE(SUBSTRING(ENTRY(1,ttArt.ttFile),R-INDEX(ENTRY(1,ttArt.ttFile),"\")),".pdf","_CS6.pdf")) = ? THEN 
           OS-DELETE VALUE(cBatchImgLoc + REPLACE(SUBSTRING(ENTRY(1,ttArt.ttFile),R-INDEX(ENTRY(1,ttArt.ttFile),"\")),".pdf","_CS6.pdf")).
           
           OS-COPY VALUE(ENTRY(1,ttArt.ttFile)) VALUE(cBatchImgLoc + REPLACE(SUBSTRING(ENTRY(1,ttArt.ttFile),R-INDEX(ENTRY(1,ttArt.ttFile),"\")),".pdf","_CS6.pdf")).
              
           IF NUM-ENTRIES(ttArt.ttFile) > 1 THEN DO:    
               
                IF SEARCH(cBatchImgLoc + REPLACE(SUBSTRING(ENTRY(2,ttArt.ttFile),R-INDEX(ENTRY(2,ttArt.ttFile),"\")),".pdf","_CS6.pdf")) = ? THEN
                OS-DELETE VALUE(cBatchImgLoc + REPLACE(SUBSTRING(ENTRY(2,ttArt.ttFile),R-INDEX(ENTRY(2,ttArt.ttFile),"\")),".pdf","_CS6.pdf")).
                   
                OS-COPY VALUE(ENTRY(2,ttArt.ttFile)) VALUE(cBatchImgLoc + REPLACE(SUBSTRING(ENTRY(2,ttArt.ttFile),R-INDEX(ENTRY(2,ttArt.ttFile),"\")),".pdf","_CS6.pdf")).  
                ttArt.ttFile = cBatchImgLoc + "\" + ENTRY(NUM-ENTRIES(ENTRY(1,ttArt.ttFile),"\"),ENTRY(1,ttArt.ttFile),"\") + ","
                             + cBatchImgLoc + "\" + ENTRY(NUM-ENTRIES(ENTRY(2,ttArt.ttFile),"\"),ENTRY(2,ttArt.ttFile),"\"). 
                ttArt.ttFile = REPLACE(ttArt.ttFile,".pdf","_CS6.pdf").                       
           END.  
           ELSE ASSIGN  ttArt.ttFile = cBatchImgLoc + "\" + ENTRY(NUM-ENTRIES(ENTRY(1,ttArt.ttFile),"\"),ENTRY(1,ttArt.ttFile),"\")
                        ttArt.ttFile = REPLACE(ttArt.ttFile,".pdf","_CS6.pdf").
                        
           
           /*Combine filenames - directional images only*/
           tmpfName = "".
           IF NUM-ENTRIES(ttArt.ttFile) > 1 AND INDEX(cRecipe,"_foldover") = 0 AND INDEX(cRecipe,"corex") > 0 THEN DO:
               ASSIGN tmpfName = ENTRY(1,ttArt.ttFile) /*gets the first file's full path*/
                      tmpfLoc = SUBSTRING(tmpfName,1,R-INDEX(tmpfName,"\")). /*Gets the path to the file*/
                              
               art:Preflight(ENTRY(1,ttArt.ttFile),ENTRY(1,ttArt.ttFile),"Discard hidden layer content and flatten visible layers.kfpx").
               art:Preflight(ENTRY(2,ttArt.ttFile),ENTRY(2,ttArt.ttFile),"Discard hidden layer content and flatten visible layers.kfpx").
                    
               art:MergeTwo(ENTRY(1,ttArt.ttFile),ENTRY(2,ttArt.ttFile),"\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\MergedPDFs\" + ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS) + ".pdf").      
               tmpfName = "\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\MergedPDFs\" + ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS) + ".pdf". 
           END.
           ELSE IF NUM-ENTRIES(ttArt.ttFile) > 1 AND INDEX(cRecipe,"_foldover") > 0 AND INDEX(cRecipe,"corex") > 0 THEN DO: /* Myrtle */
               ASSIGN xmlMyrtle = "<front>" + ENTRY(1,ttArt.ttFile,",") + "</front>"
                                + "<back>"  + ENTRY(2,ttArt.ttFile,",") + "</back>"
                                + "<outputfile>\\fs02\bullseye\images\AgentPhotos\temporary\CS6\" + ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS) + "_CS6.pdf</outputfile>"
                                + "<material>Corex</material>"
                                + "<size>" + cSize + "</size>"
                                + "<resize>no</resize>"
                                + "<ActualHeight>" + STRING(ENTRY(1,cSize,"x")) + "</ActualHeight>"
                                + "<ActualWidth>" + STRING(ENTRY(2,cSize,"x")) + "</ActualWidth>"
                                + "<datecolorcode>Black</datecolorcode>"
                                + "<order>" + STRING(ttArt.ttSo) + "</order>"
                                + "<item>" + STRING(ttArt.ttItemNo) + "</item>"
                                + "<PartNumber>" + ttArt.ttPart + "</PartNumber>"
                                + "<FoldOver>yes</FoldOver>".
               xmlMyrtle = "<Host>ART-JM2</Host><XMLData><Program>SART</Program><XML><myrtle>" + xmlMyrtle + "</myrtle></XML></XMLData>". 
               
               RUN clientapp(xmlMyrtle,OUTPUT cResponse).
                
               IF INDEX(cResponse,"Failed") > 0 THEN NEXT.                        
               
               tmpfName = "\\fs02\bullseye\images\AgentPhotos\temporary\CS6\" + ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS) + "_CS6.pdf". 
               
           END.
           
           IF INDEX(cRecipe,"_Foldover") > 0 AND NUM-ENTRIES(ttArt.ttFile) = 1 AND INDEX(cRecipe,"DF") > 0 THEN DO:
               art:Preflight(ttArt.ttFile,"\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\MergedPDFs\" + ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS) + ".pdf","Duplicate Page.kfpx").
               tmpfName = "\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\MergedPDFs\" + ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS) + ".pdf".     
           END.
             
           IF tmpfName = "" THEN tmpfName = ttArt.ttFile.    
           
           /*Add date code - Ticket#54386 */
           IF DateCodeYN = TRUE THEN DO:
               IF inVars:Has("dateCode") THEN inVars:REMOVE("dateCode").
               inVars:Add("dateCode",ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS)).
               art:PreflightVars(ENTRY(1,ttArt.ttFile),
                                 ENTRY(1,ttArt.ttFile),
                                 "Place Datecode v3.kfpx",
                                 inVars).
                                 
               IF NUM-ENTRIES(ttArt.ttFile) > 1 THEN DO:
                   IF inVars:Has("dateCode") THEN inVars:REMOVE("dateCode").
                   inVars:Add("dateCode",ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS)).
                   art:PreflightVars(ENTRY(2,ttArt.ttFile),
                                     ENTRY(2,ttArt.ttFile),
                                     "Place Datecode v3.kfpx",
                                     inVars).         
               END.        
           END.              
           
           /*get customer #*/
           FIND FIRST so_file NO-LOCK WHERE so_file.so_no = ttArt.ttSO NO-ERROR.
           
           /*ASSIGN xmlData = IF tmpfName = "" THEN REPLACE(xmlData,"[filename]","file://" + REPLACE(ttArt.ttFile,"\","/")) ELSE REPLACE(xmlData,"[filename]","file://" + REPLACE(tmpfName,"\","/"))*/
           ASSIGN tmpfName = REPLACE(tmpfName,"\\fs02\","file:///mnt/smb/10.201.12.26/") 
                  tmpfName = REPLACE(tmpfName,"\\lowen\","file:///mnt/smb/10.201.12.28/")
                  tmpfName = REPLACE(tmpfName,"bullseye","Bullseye")
                  tmpfName = REPLACE(tmpfName,"temporary","Temporary")
                  tmpfName = REPLACE(tmpfName,"\","/")
                  xmlData = REPLACE(xmlData,"[filename]",tmpfName).   
                      
                  
           ASSIGN xmlData = REPLACE(xmlData,"[copies]",STRING(ttArt.ttQty))                 
                  xmlData = REPLACE(xmlData,"[jobID]",ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS))
                  xmlData = REPLACE(xmlData,"[customerID]", IF AVAIL so_file THEN STRING(so_file.cust_no) ELSE " ")
                  xmlData = REPLACE(xmlData,"[orderID]",ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS))
                  xmlData = REPLACE(xmlData,"[jobName]"," ")
                  xmlData = REPLACE(xmlData,"[comment]"," ").
           
           RELEASE so_file.          
           
           /*Here we decide where the xml file gets moved to*/
           ASSIGN outputFile = "\\caldera-key\Public\PrimeCenterMaster\" + cRecipe + "\Input\" + ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS) + ".xml".

           
           IF INDEX(outputFile,"\\Input") > 0 THEN DO:
               FailList:ADD(outputFile).
               NEXT.
           END.
             
           IF SEARCH(outputFile) = ? AND INDEX(outputFile,"\\Input") = 0 THEN OUTPUT TO VALUE(outputFile).
           ELSE DO:
               outputFile = "\\caldera-key\Public\PrimeCenterMaster\" + cRecipe + "\Input\" + ttArt.ttSO + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ALS + 1) + ".xml".
               
               OUTPUT TO VALUE(outputFile).
               iErrorStatus = OS-ERROR.
           END.
                    
           
           PUT UNFORMATTED xmlData SKIP.          
           OUTPUT CLOSE.  
                      
           IF SEARCH(outputFile) <> ? AND cRecipe <> "" THEN totFiles = totFiles + 1. 
           ELSE FailList:ADD(outputFile).  
            
        END. /*end ttArt*/
    
    ASSIGN startTime = TIME
           FolderList = "Corex_DF_H\Success\,Corex_DF_V\Success\,Corex_DF_H_Foldover\Success\,Corex_DF_H_Foldover\Failed\,"
                      + "Corex_SF_H\Success\,Corex_SF_H_Foldover\Success\,Corex_SF_V\Success\,Corex_SF_V_Foldover\Success\,"
                      + "Corex_DF_H\Failed\,Corex_DF_V\Failed\,Corex_SF_H\Failed\,Corex_SF_H_Foldover\Failed\,Corex_SF_V\Failed\,"
                      + "Corex_SF_V_Foldover\Failed\,Corex_DF_V_Foldover\Success\,Corex_DF_V_Foldover\Failed".
    DO WHILE totFiles > 0:
        DO i = 1 TO NUM-ENTRIES(FolderList):
            cRecipe = ENTRY(i,FolderList).
            IF TIME - startTime > 1800 THEN DO: /*wait time was 2400 - shortened wait time to 30mins*/
                totFiles = 0.
                LEAVE.
            END.
            
            INPUT FROM OS-DIR("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + ENTRY(i,FolderList)). 
            REPEAT:
                IMPORT fName.
                IF (fName > "" AND fName <> "." AND fName <> "..") AND FileList:CONTAINS("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + cRecipe + fName) = FALSE 
                                                                   AND FailList:CONTAINS("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + cRecipe + fName) = FALSE
                                                                   AND FailList:CONTAINS("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + SUBSTRING(cRecipe,1,INDEX(cRecipe,"\") - 1) + "\Ticket\" + fName) = FALSE
                                                                   AND FileList:CONTAINS("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + SUBSTRING(cRecipe,1,INDEX(cRecipe,"\") - 1) + "\Ticket\" + fName) = FALSE
                THEN DO:
                    IF INDEX(ENTRY(i,FolderList),"Failed") > 0 THEN FailList:ADD("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + cRecipe + fName).
                    ELSE DO: 
                        IF SEARCH("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + SUBSTRING(cRecipe,1,INDEX(cRecipe,"\") - 1) + "\Ticket\" + fName)  <> ? THEN FileList:ADD("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + SUBSTRING(cRecipe,1,INDEX(cRecipe,"\") - 1) + "\Ticket\" + fName).   
                    END.
                    totFiles = totFiles - 1.                   
                END.
            END. 
        END.          
    END.
    
    
    IF FileList:COUNT > 0 THEN DO:
        RUN ReadXML(INPUT FileList). 
    END.
    
    i = 0.
    /*Send failed list to Houston*/
    DO WHILE i < FailList:COUNT:
        c_msg = IF c_msg = "" THEN FailList[i] ELSE (c_msg + "," + FailList[i]).    
        
        
        ASSIGN FailedFile = SUBSTRING(FailList[i],R-INDEX(FailList[i],"\") + 1)
               FailedSO   = SUBSTRING(FailedFile,1,INDEX(FailedFile,"-") - 1).
               FailedNo   = SUBSTRING(FailedFile,INDEX(FailedFile,"-") + 1, 1).
               
        FIND FIRST so_file NO-LOCK WHERE so_file.so_no = FailedSO NO-ERROR.
        FIND FIRST so_items NO-LOCK WHERE so_items.so_no = FailedSO AND so_items.item_no = INT(FailedNo) NO-ERROR.
        IF AVAIL so_file AND AVAIL so_items THEN DO:            
            RUN ReportIssues(so_items.itemseq,"MM-Prime Center Fail",so_items.so_no,STRING(so_items.ITEM_no),"","Prime Center Fail","","","","").
        END.
        RELEASE so_file.
        RELEASE so_items.
        
        i = i + 1.
    END.
    IF FailList:COUNT <> 0 THEN RUN mgemail.p ("Bullseye Database","Houstons@lowen.com;Lancep@lowen.com","RyanLe@lowen.com","","Failed Corex Jobs - PC",c_msg,"",FALSE).
    
    /*loop through all prime center batches - Remove TS watermarks*/
    FOR EACH sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.zzchar_1  = "PrimeCenter" AND sign_mm_hdr.run_date = ?
                                   AND sign_mm_hdr.run_time = ?:
    
        FOR EACH sign_mm_det OF sign_mm_hdr:
            FIND so_items NO-LOCK WHERE so_items.itemseq = sign_mm_det.itemseq NO-ERROR.
            FIND so_file NO-LOCK WHERE so_file.so_no = so_items.so_no AND so_file.cust_no = "217559" NO-ERROR.
            IF AVAIL so_file THEN DO:
                fName = "\\lowen\dfssancluster\bullseye\images\agentphotos\Temporary\mtl1\" + sign_mm_hdr.matltype + "\POP 55\" + STRING(sign_mm_hdr.batchseq) + "_1.pdf".
                IF SEARCH(fName) <> ? 
                AND SEARCH(REPLACE(fName,".pdf","_preview.pdf")) = ? THEN DO:
                
                    OS-COPY VALUE(fName) VALUE(REPLACE(fName,".pdf","_preview.pdf")). 
                            
                    /*Now remove TS watermark from our extra copy*/
                    art:Preflight(fName,fName,"Tradesource Cut Contours and Watermark.kfpx").  
                    
                    fName = REPLACE(fName,"_1.pdf","_2.pdf").
                    IF SEARCH(fName) <> ? AND SEARCH(REPLACE(fName,".pdf","_preview.pdf")) = ? THEN DO:
                        
                        OS-COPY VALUE(fName) VALUE(REPLACE(fName,".pdf","_preview.pdf")). 
                            
                        /*Now remove TS watermark from our extra copy*/
                        art:Preflight(fName,fName,"Tradesource Cut Contours and Watermark.kfpx").   
                    END.
                                     
                    LEAVE. 
                    
                END. 
            END. 
            
        END.                                   
                              
    END.
    
END PROCEDURE.


PROCEDURE ReadXML:
    DEFINE INPUT PARAMETER FileList AS "System.Collections.Generic.List<character>" NO-UNDO. 
    
    DEFINE VARIABLE BatchList AS "System.Collections.Generic.Dictionary<character,int>" NO-UNDO.
    BatchList = NEW "System.Collections.Generic.Dictionary<character,int>"().  
    
    DEFINE VARIABLE hDoc  AS HANDLE NO-UNDO.
    DEFINE VARIABLE hRoot AS HANDLE NO-UNDO. 
    
    CREATE X-DOCUMENT hDoc.
    CREATE X-NODEREF  hRoot.
    
    DEFINE VARIABLE cSO       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemNo   AS INT       NO-UNDO.
    DEFINE VARIABLE cALS      AS INT       NO-UNDO.
    DEFINE VARIABLE cFName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGang     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRecipe   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE i AS INT.
    DO WHILE i < FileList:COUNT:
        hDoc:LOAD("file",FileList[i],FALSE). /*This load the whole file in*/
        hDoc:GET-DOCUMENT-ELEMENT(hRoot). /*Sets the first node - should be "ticket" in this case*/
                
        ASSIGN cSO     = ""
               cItemNo = 0
               cALS    = 0
               cFName  = ""
               cGang   = ""
               cRecipe = "".
               
        IF hRoot:NAME = "Ticket" THEN DO: /*<ticket> is the first node*/
            DEFINE VARIABLE hLvl1 AS HANDLE      NO-UNDO.
            DEFINE VARIABLE hLvl2 AS HANDLE      NO-UNDO.
            DEFINE VARIABLE hLvl3 AS HANDLE      NO-UNDO.
            DEFINE VARIABLE hText AS HANDLE      NO-UNDO.
            
            DEFINE VARIABLE j     AS INT         NO-UNDO.
            DEFINE VARIABLE k     AS INT         NO-UNDO.
            DEFINE VARIABLE l     AS INT         NO-UNDO.
           
            CREATE X-NODEREF hLvl1.
            CREATE X-NODEREF hLvl2.
            CREATE X-NODEREF hLvl3.
            CREATE X-NODEREF hText.
            
            DO j = 1 TO hRoot:NUM-CHILDREN:
                hRoot:GET-CHILD(hLvl1,j). /*Loads the next <tag>*/    
                               
                
                IF hLvl1:NAME = "job" AND hLvl1:ATTRIBUTE-NAMES = "creationTime" THEN DO: /*gets the first <job>*/                 
                    DO k = 1 TO hLvl1:NUM-CHILDREN:
                        hLvl1:GET-CHILD(hLvl2,k).
                        
                        IF hLvl2:NAME = "source" THEN DO:
                            hLvl2:GET-CHILD(hLvl3,2).
                            hLvl3:GET-CHILD(hText,1). 
                            cFName = hText:NODE-VALUE.   /*Batch file*/                      
                        END.
                        
                        IF hLvl2:NAME = "process" THEN DO:
                            hLvl2:GET-CHILD(hLvl3,2).
                            hLvl3:GET-CHILD(hText,1).
                            cGang = hText:NODE-VALUE. /*Gang Value*/
                            
                            hLvl2:GET-CHILD(hLvl3,4).
                            hLvl3:GET-CHILD(hText,1).
                            cRecipe = hText:NODE-VALUE. /*recipe value*/
                        END.
                        
                        IF hLvl2:NAME = "info" THEN DO:
                            hLvl2:GET-CHILD(hLvl3,2).
                            hLvl3:GET-CHILD(hText,1).
                            
                            cSO     = ENTRY(1,hText:NODE-VALUE,"-").      /* SO#    */
                            cItemNo = INT(ENTRY(2,hText:NODE-VALUE,"-")). /* Item # */   
                            cALS    = INT(ENTRY(3,hText:NODE-VALUE,"-")). /* ALS    */
                        END.                           
                    END.   
                END.
                              
                IF hLvl1:NAME = "Layout" THEN RUN ProcessLayoutTag (INPUT hLvl1,INPUT-OUTPUT BatchList,INPUT cSO, INPUT cItemNo, INPUT cALS, INPUT cGang, INPUT cRecipe, INPUT cFName).
            END.                   
        END. /*if ticket*/           
        i = i + 1.    
    END.
    
END PROCEDURE.


PROCEDURE ProcessLayoutTag:
    
    DEFINE INPUT PARAMETER iNode            AS HANDLE                                                 NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER BatchList AS "System.Collections.Generic.Dictionary<character,int>" NO-UNDO.
    DEFINE INPUT PARAMETER cSO              AS CHARACTER                                              NO-UNDO.
    DEFINE INPUT PARAMETER cItemNo          AS INT                                                    NO-UNDO.
    DEFINE INPUT PARAMETER cALS             AS INT                                                    NO-UNDO.   
    DEFINE INPUT PARAMETER cGang            AS CHARACTER                                              NO-UNDO.
    DEFINE INPUT PARAMETER cRecipe          AS CHARACTER                                              NO-UNDO.
    DEFINE INPUT PARAMETER cFName           AS CHARACTER                                              NO-UNDO.
       
    cFName = REPLACE(cFName,"file://","").
    cFName = REPLACE(cFName,"/","\").
    
    DEFINE VARIABLE hLvl1      AS HANDLE NO-UNDO.
    DEFINE VARIABLE hLvl2      AS HANDLE NO-UNDO.
    DEFINE VARIABLE hLvl3      AS HANDLE NO-UNDO.
    DEFINE VARIABLE hText      AS HANDLE NO-UNDO.
     
    DEFINE VARIABLE i          AS INT       NO-UNDO.
    DEFINE VARIABLE j          AS INT       NO-UNDO.
    DEFINE VARIABLE k          AS INT       NO-UNDO.
    DEFINE VARIABLE cBatch     AS INT       NO-UNDO.
    DEFINE VARIABLE nextSeq    AS INT       NO-UNDO.
    DEFINE VARIABLE matRecid   AS RECID     NO-UNDO.
    DEFINE VARIABLE pCnt       AS INT       NO-UNDO.
    DEFINE VARIABLE imgCnt     AS INT       NO-UNDO INITIAL 1.
    DEFINE VARIABLE fPath      AS CHAR      NO-UNDO.
    DEFINE VARIABLE tmpFName   AS CHAR      NO-UNDO.
    DEFINE VARIABLE cCutGUID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse  AS CHAR      NO-UNDO.
    DEFINE VARIABLE success    AS LOGICAL   NO-UNDO.

    
    CREATE X-NODEREF hLvl1.
    CREATE X-NODEREF hLvl2.
    CREATE X-NODEREF hLvl3.
    CREATE X-NODEREF hText.
    
    DEFINE VARIABLE art AS ArtGenerator.
    art = NEW ArtGenerator().

    
    DO i = 1 TO iNode:NUM-CHILDREN:
        iNode:GET-CHILD(hLvl1,i).
        
        IF hLvl1:NAME = "paths" THEN DO:
            hLvl1:GET-CHILD(hLvl2,2).      /*This gets the <output>*/
            hLvl2:GET-CHILD(hText,1).      /*This gets the text in the <output> -> hOutputVal*/    
            fPath = hText:NODE-VALUE.      /*full file path name*/
            
            /*tmpfName = REPLACE(fPath,"S:\PrimeCenterMaster\" + cRecipe + "\Output\",""). /*just the file name*/ */
            tmpfName = REPLACE(fPath,"/mnt/smb/10.201.12.28/SignDigitalOutput/PrimeCenterMaster/" + cRecipe + "/Output/","").
            tmpfName = REPLACE(tmpfName,"/","\").
            
            IF INDEX(fPath,"Side_B.pdf") > 0 THEN NEXT.              

            FIND FIRST so_items NO-LOCK WHERE so_items.so_no = cSO AND so_items.item_no = cItemNo NO-ERROR.
            FIND FIRST so_file  NO-LOCK WHERE so_file.so_no = so_items.so_no NO-ERROR.
            FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.type <> "FRAME" AND squ_ptdet.itemseq = so_items.itemseq NO-ERROR. 
                    
            IF AVAIL squ_ptdet THEN RUN FindMatPanel.p (squ_ptdet.subseq,OUTPUT matRecid).
            FIND FIRST squ_mat NO-LOCK WHERE RECID(squ_mat) = matRecid NO-ERROR.
                    
            IF BatchList:ContainsKey(fPath) = FALSE THEN DO:  /*We haven't created a batch with this file yet*/ 
                                   
                /*Get latest batchseq#*/
                nextSeq = NEXT-VALUE(seq-mm-batch).
                DO WHILE CAN-FIND(sign_mm_hdr WHERE sign_mm_hdr.batchseq = nextSeq):
                    nextSeq = NEXT-VALUE(seq-mm-batch).
                END.
                cBatch = nextSeq.
                    
                BatchList:ADD(fPath,cBatch). /*Add to the list - <filepath,batchseq>*/
                                    
                /*Now we need to create a new batch header record - sign_mm_hdr*/
                CREATE sign_mm_hdr.
                ASSIGN sign_mm_hdr.batchseq = cBatch
                       sign_mm_hdr.runseq   = 0
                       sign_mm_hdr.crt_date = TODAY
                       sign_mm_hdr.crt_time = TIME
                       sign_mm_hdr.run_date = ?
                       sign_mm_hdr.run_time = ?
                       sign_mm_hdr.due_date = ?
                       sign_mm_hdr.matlType = IF cGang = "4MM" THEN "Corex 4mm" 
                                              ELSE IF cGang = "6MM" THEN "Corex 6mm"
                                              ELSE IF cGang = "8MM" THEN "Corex 8mm"
                                              ELSE "Corex 10mm"
                                              
                       sign_mm_hdr.sides = IF INDEX(cRecipe,"DF") > 0 THEN 2 ELSE 1
                       sign_mm_hdr.bedseq = 0 
                       sign_mm_hdr.PointerSeq = 0
                       sign_mm_hdr.inv_part   = IF AVAIL squ_mat THEN squ_mat.part_no ELSE ""
                       sign_mm_hdr.qty        = INT(SUBSTRING(tmpfName,INDEX(tmpfName,'_') + 1,1) + IF SUBSTRING(tmpfName,INDEX(tmpfName,'_') + 2,1) <> '_' THEN SUBSTRING(tmpfName,INDEX(tmpfName,'_') + 2,1) ELSE "")
                       sign_mm_hdr.zzlog_1    = FALSE
                       sign_mm_hdr.rerun      = FALSE
                       sign_mm_hdr.reprint    = FALSE
                       sign_mm_hdr.fullbed    = TRUE 
                       sign_mm_hdr.pt_hotfolderseq = IF cGang = "4MM" THEN 5 ELSE 17 
                       sign_mm_hdr.fbMachine  = 1
                       sign_mm_hdr.zzchar_1  = "PrimeCenter".    
                       
                /*get machine time*/
                RUN Get_Mach_Time(sign_mm_hdr.batchseq, sign_mm_hdr.bedseq, OUTPUT sign_mm_hdr.mach_time).                             
                
                /*Copy out the batch file*/
                tmpfName = "\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + cRecipe + "\Output\" + tmpfName.
                
                IF SEARCH("\\lowen\dfssancluster\bullseye\images\agentphotos\Temporary\mtl1\Corex " + cGang + "\POP 55\batch" + STRING(cBatch) + "_1.pdf") = ? THEN DO:
                    
                    /*Check if double faced*/
                    IF INDEX(cRecipe,"DF") > 0 THEN DO:
                        EMPTY TEMP-TABLE ttImages.
                        success = art:SplitPDF(tmpfName,"\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + cRecipe + "\Output\","split", OUTPUT TABLE ttImages).
                        
                        imgCnt = 1.
                        FOR EACH ttImages NO-LOCK:
                            OS-COPY VALUE(ttImages.tFileName) VALUE("\\lowen\dfssancluster\bullseye\images\agentphotos\Temporary\mtl1\Corex " + cGang + "\POP 55\batch" + STRING(cBatch) + "_" + STRING(imgCnt) + ".pdf").
                            OS-COPY VALUE(ttImages.tFileName) VALUE("\\lowen\dfssancluster\Bullseye\images\agentphotos\Temporary\CS6\CompletedBatches\Batch" + STRING(sign_mm_hdr.batchseq) + "_" + STRING(imgCnt) + ".pdf").
                            
                            art:Preflight("\\lowen\dfssancluster\bullseye\images\agentphotos\Temporary\mtl1\Corex " + cGang + "\POP 55\batch" + STRING(cBatch) + "_" + STRING(imgCnt) + ".pdf",
                                          "\\lowen\dfssancluster\bullseye\images\agentphotos\Temporary\mtl1\Corex " + cGang + "\POP 55\batch" + STRING(cBatch) + "_" + STRING(imgCnt) + ".pdf",
                                          "Replace Black 0413 with Black 0520.kfpx").
                                          
                            RUN pdftopng.p("\\lowen\dfssancluster\bullseye\images\agentphotos\Temporary\mtl1\Corex " + cGang + "\POP 55\batch" + STRING(cBatch) + "_" + STRING(imgCnt) + ".pdf",
                                           "\\fs02\bullseye\images\agentphotos\Temporary\JPEG\batch" + STRING(cBatch) + "_" + STRING(imgCnt) + ".png",
                                          OUTPUT cResponse).                                     
                            imgCnt = imgCnt + 1.    
                        END.   
                    END.
                    ELSE DO:
                        OS-COPY VALUE(tmpfName) VALUE("\\lowen\dfssancluster\bullseye\images\agentphotos\Temporary\mtl1\Corex " + cGang + "\POP 55\batch" + STRING(cBatch) + "_1.pdf").
                        IF AVAIL ttImages THEN OS-COPY VALUE(ttImages.tFileName) VALUE("\\lowen\dfssancluster\Bullseye\images\agentphotos\Temporary\CS6\CompletedBatches\Batch" + STRING(sign_mm_hdr.batchseq) + "_1.pdf").
                                  
                        art:Preflight("\\lowen\dfssancluster\bullseye\images\agentphotos\Temporary\mtl1\Corex " + cGang + "\POP 55\batch" + STRING(cBatch) + "_1.pdf",
                                      "\\lowen\dfssancluster\bullseye\images\agentphotos\Temporary\mtl1\Corex " + cGang + "\POP 55\batch" + STRING(cBatch) + "_1.pdf",
                                      "Replace Black 0413 with Black 0520.kfpx").
                                      
                        RUN pdftopng.p("\\lowen\dfssancluster\bullseye\images\agentphotos\Temporary\mtl1\Corex " + cGang + "\POP 55\batch" + STRING(cBatch) + "_1.pdf",
                                       "\\fs02\bullseye\images\agentphotos\Temporary\JPEG\batch" + STRING(cBatch) + "_1.png",
                                       OUTPUT cResponse).                  
                    END.
                END.

                
            END. /*End BatchList:Contains = FALSE*/
                
            
            IF INDEX(cRecipe,"Foldover") > 0 THEN iNode:GET-CHILD(hLvl1,i + 2).
            ELSE iNode:GET-CHILD(hLvl1,i + 2).
            
            IF hLvl1:NAME = "cutGUID" THEN DO:
                hLvl1:GET-CHILD(hLvl2,1).
                IF LOOKUP(hLvl2:NODE-VALUE,cCutGUID) = 0 THEN cCutGUID = IF cCutGUID = "" THEN hLvl2:NODE-VALUE ELSE "," + hLvl2:NODE-VALUE.
                IF AVAILABLE sign_mm_hdr THEN sign_mm_hdr.zzchar_2 = cCutGUID.
                
                iNode:GET-CHILD(hLvl1,i + 4).
            END.
            
            IF hLvl1:NAME = "Contents" THEN DO:
                
                DO j = 1 TO hLvl1:NUM-CHILDREN:
                    hLvl1:GET-CHILD(hLvl2,j).
                        
                    IF hLvl2:NAME = "Job" THEN DO:
                        DO k = 1 TO hLvl2:NUM-CHILDREN:
                            hLvl2:GET-CHILD(hText,k).
                            IF hText:NAME = "item" THEN DO:
                                
                                pCnt = 0.
                                IF cBatch = 0 THEN cBatch = batchList[fPath].
                                
                                FIND LAST sign_mm_det NO-LOCK WHERE sign_mm_det.batchseq = cBatch  NO-ERROR.
                                IF AVAIL sign_mm_det THEN pCnt = sign_mm_det.position + 1. 
                                ELSE pCnt = 1.
                                RELEASE sign_mm_det.
                                 
                                /*Create the batch details - sign_mm_det*/
                                CREATE sign_mm_det.
                                ASSIGN sign_mm_det.batchseq        = cBatch
                                       sign_mm_det.part_no         = so_items.part_no
                                       sign_mm_det.artlinkseq      = cALS
                                       sign_mm_det.itemseq         = so_items.itemseq
                                       sign_mm_det.artfile         = cFName
                                       sign_mm_det.inv_part        = IF AVAIL squ_mat THEN squ_mat.part_no ELSE ""
                                       sign_mm_det.position        = pCnt
                                       sign_mm_det.due_date        = so_file.ship_by
                                       sign_mm_det.pt_hotfolderseq = IF AVAIL sign_mm_hdr THEN sign_mm_hdr.pt_hotfolderseq ELSE 0
                                       sign_mm_det.PointerSeq      = 1 
                                       sign_mm_det.switch          = YES 
                                       sign_mm_det.zzlog_1         = FALSE. 
                                
                                /*get machine time*/
                                IF AVAIL sign_mm_hdr THEN DO:
                                    RUN Get_Mach_Time(sign_mm_hdr.batchseq, sign_mm_hdr.bedseq, OUTPUT sign_mm_hdr.mach_time).
                                    
                                    FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.batchseq = cBatch:
                                        IF sign_mm_det.due_date < sign_mm_hdr.due_date OR sign_mm_hdr.due_date = ? THEN sign_mm_hdr.due_date = sign_mm_det.due_date.
                                    END.
                                END.
                                RELEASE sign_mm_hdr.
                                       
                            END.
                        END.
                    END.    
                END.
            END. /*end if "contents"*/     
            
            RELEASE sign_mm_det.
            RELEASE sign_mm_hdr.
            RELEASE so_items.
            RELEASE so_file.
            RELEASE squ_ptdet.
            RELEASE squ_mat.
  
        END. /* end if "paths"*/
    END. /*end do loop*/
       
END PROCEDURE.


PROCEDURE DelCorexPC:
    DEFINE INPUT PARAMETER starter AS CHAR NO-UNDO.

    DEFINE VARIABLE filepath   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i          AS INTEGER     NO-UNDO.
    
    DEFINE VARIABLE Folders    AS CHARACTER   NO-UNDO.
    
    Folders = "Corex_DF_H\Ticket\,Corex_SF_H\Ticket\,Corex_DF_V\Ticket\,Corex_SF_V\Ticket\,"
            + "Corex_SF_H_Foldover\Ticket\,Corex_SF_V_Foldover\Ticket\,Corex_DF_V_Foldover\Ticket\,Corex_DF_H_Foldover\Ticket\,Corex_DF_V\Output\,"
            + "Corex_SF_V\Output\,Corex_DF_H\Output\,Corex_SF_H\Output\,Corex_SF_V_Foldover\Output\,"
            + "Corex_SF_H_Foldover\Output\,Corex_DF_H_Foldover\Output\,Corex_DF_V_Foldover\Output\,Corex_SF_V\Failed\,Corex_SF_H\Failed\,Corex_DF_V\Failed\,"
            + "Corex_DF_H\Failed\,Corex_SF_V_Foldover\Failed\,Corex_SF_H_Foldover\Failed\,Corex_DF_V_Foldover\Failed\,Corex_DF_H_Foldover\Failed\,Corex_DF_H\Success\,"
            + "Corex_DF_V\Success\,Corex_SF_H\Success\,Corex_SF_V\Success\,Corex_SF_V_Foldover\Success\,Corex_SF_H_Foldover\Success\,Corex_DF_V_Foldover\Success\,Corex_DF_H_Foldover\Success\".
    
    DO i = 1 TO NUM-ENTRIES(Folders):
        
       INPUT FROM OS-DIR("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + ENTRY(i,Folders)).
       IMPORT ^.
       IMPORT ^.
       REPEAT:
           IMPORT filepath.
           IF SEARCH("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + ENTRY(i,Folders) + filepath) <> ? THEN DO:
               OS-COPY VALUE("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + ENTRY(i,Folders) + filepath) VALUE("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + SUBSTRING(ENTRY(i,Folders),1,10) + "\History" + SUBSTRING(ENTRY(i,Folders),11) + filepath).
               OS-DELETE VALUE("\\lowen\dfssancluster\SignDigitalOutput\PrimeCenterMaster\" + ENTRY(i,Folders) + filepath).
           END.
       END.    
    END. 
    
END PROCEDURE.


PROCEDURE ImageDiff:
    DEFINE INPUT  PARAMETER cNum AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER cLog AS LOG NO-UNDO.

    cLog = NO. /*says there are no difference*/
    FIND b_mm_hdr NO-LOCK WHERE b_mm_hdr.batchseq = cNum NO-ERROR.
    IF AVAIL b_mm_hdr THEN DO:
        FOR EACH b_mm_det OF b_mm_hdr:
            IF NUM-ENTRIES(b_mm_det.artfile,",") > 1 THEN cLog = YES. /*if more than one image have to assume they are different, thus need to send both sides*/ 
            IF cLog THEN LEAVE.
        END.
    END.
END.


PROCEDURE LogIt:
    DEFINE INPUT PARAMETER pSource     AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER pMsg        AS CHAR NO-UNDO.
    
    DEFINE VARIABLE fileOk             AS LOG  NO-UNDO.
    DEFINE VARIABLE tmpUser            AS CHAR NO-UNDO.

    tmpUser = IF current-user-id <> "" THEN current-user-id ELSE OS-GETENV("computername"). 
    
    RUN GetDBase.
    
    CASE cSource:
        WHEN 1 THEN DO:
            RUN fileExists(cLogLoc,OUTPUT fileOk).
            IF fileOk THEN
                OUTPUT TO VALUE(cLogLoc + "\mediaManager.log") APPEND.
            ELSE
                OUTPUT TO VALUE(SESSION:TEMP-DIR + "mediaManager.log") APPEND.

            IF cXML = "" THEN
                PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            ELSE DO:
                IF CAN-FIND(FIRST sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.batchseq = int(cProcedure)) AND cResponse = "" THEN
                    PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") "  Started Batch: " cProcedure SKIP.
                ELSE DO:
                    PUT UNFORMATTED STRING(TODAY) " " cProcedure "  Start = " cstarttime "  Finish = " STRING(TIME,"HH:MM:SS") SKIP.
                    PUT UNFORMATTED cXml SKIP.
                    PUT UNFORMATTED cResponse SKIP.
                END.
            END.
            OUTPUT CLOSE.
        END.
        WHEN 2 THEN DO:
            RUN fileExists(cLogLoc,OUTPUT fileOk).
            IF fileOk THEN
                OUTPUT TO VALUE(cLogLoc + "\" + tmpUser  + "-mediaManagerTryCopy.log") APPEND.
            ELSE
                OUTPUT TO VALUE(SESSION:TEMP-DIR + "mediaManagerTryCopy.log") APPEND.

            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " "Try find runseq = " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
        WHEN 3 THEN DO:
            RUN fileExists(cLogLoc,OUTPUT fileOk).
            IF fileOk THEN
                OUTPUT TO VALUE(cLogLoc + "\" + tmpUser  + "-mediaManagerCopy.log") APPEND.
            ELSE
                OUTPUT TO VALUE(SESSION:TEMP-DIR + "mediaManagerCopy.log") APPEND.

            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
        WHEN 4 THEN DO:
            RUN fileExists(cLogLoc,OUTPUT fileOk).
            IF fileOk THEN 
                OUTPUT TO VALUE(cLogLoc + "\" + tmpUser  + "-MediaManagerSpeed.log") APPEND.
            ELSE
                OUTPUT TO VALUE(SESSION:TEMP-DIR + "MediaManagerSpeed.log") APPEND.

            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
        WHEN 5 THEN DO:
            RUN fileExists(cLogLoc,OUTPUT fileOk).
            IF fileOk THEN 
                OUTPUT TO VALUE(cLogLoc + "\" + tmpUser  + "-JtPrintLog2.log") APPEND.
            ELSE
                OUTPUT to VALUE(SESSION:TEMP-DIR + "JtPrintLog2.log") APPEND.

            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
        WHEN 6 THEN DO:
            RUN fileExists(cLogLoc,OUTPUT fileOk).
            IF fileOk THEN 
                OUTPUT TO VALUE(cLogLoc + "\" + tmpUser  + "-MediaManagerAnswers.log") APPEND.
            ELSE
                OUTPUT to VALUE(SESSION:TEMP-DIR + "MediaManagerAnswers.log") APPEND.

            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
        WHEN 7 THEN DO:
            OUTPUT to VALUE(cLogLoc + "\MediaManagerReprint.log") APPEND.
            IF cXML = "" THEN
                PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            ELSE DO:
                PUT UNFORMATTED STRING(TODAY) " " cProcedure "  Start = " cstarttime "  Finish = " STRING(TIME,"HH:MM:SS") SKIP.
                PUT UNFORMATTED cXml SKIP.
                PUT UNFORMATTED cResponse SKIP.
            END.
            OUTPUT CLOSE.
        END.
        WHEN 8 THEN DO:
            OUTPUT TO VALUE(cLogLoc + "\MmTrimDoupLog.log") APPEND.
            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
        WHEN 9 THEN DO:
            OUTPUT TO VALUE(cLogLoc + "\MediaManagerQueueOrder.log") APPEND.
            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
            
        END.
        WHEN 10 THEN DO:
            RUN fileExists(cLogLoc,OUTPUT fileOk).
            IF fileOk THEN
                OUTPUT TO VALUE(cLogLoc + "\mm-preprocess.log") APPEND.
            ELSE
                OUTPUT TO VALUE(SESSION:TEMP-DIR + "mm-preprocess.log") APPEND.
        
            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
        WHEN 11 THEN DO:
             OUTPUT TO VALUE(cLogLoc + "\MediaManagerMultiBatch.log") APPEND.
             PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
             OUTPUT CLOSE.
        END.
        WHEN 12 THEN DO:
            OUTPUT TO VALUE(cLogLoc + "\MMDynam.log") APPEND.
            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.    
        END.
        WHEN 13 THEN DO:
            OUTPUT TO VALUE(cLogLoc + "\MMCheckImg.log") APPEND.
            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
        WHEN 14 THEN DO:
            OUTPUT TO VALUE(cLogLoc + "\MMCropPartials.log") APPEND.
            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
        WHEN 99 THEN DO: /*added this to log full start to finish*/
            OUTPUT TO VALUE(cLogLoc + "\MM-Ryan-Full.log") APPEND.
            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.    
        END.
        WHEN 999 THEN DO:
            OUTPUT TO VALUE(cLogLoc + "\BatchSpeed.log") APPEND.
            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END. 
        WHEN 100 THEN DO:
            OUTPUT TO VALUE(cLogLoc + "\CorexOnly.log") APPEND.
            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
        WHEN 101 THEN DO:
            OUTPUT TO VALUE(cLogLoc + "\PC.log") APPEND.
            PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
            OUTPUT CLOSE.
        END.
   END CASE.
    
    OUTPUT TO VALUE(cLogLoc + "\MediaManagerDetail.log") APPEND.
    PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " cProcedure SKIP.
    OUTPUT CLOSE.
    

END PROCEDURE.


PROCEDURE PrintOrder:
    DEFINE INPUT PARAMETER cItemseq AS CHAR NO-UNDO.
    DEFINE VARIABLE mmCnt        AS INT  NO-UNDO.
    DEFINE VARIABLE Cnt          AS INT  NO-UNDO.
    DEFINE VARIABLE nextSeq      AS INT  NO-UNDO.
    DEFINE VARIABLE cTemplate    AS CHAR NO-UNDO.
    DEFINE VARIABLE cCreated     AS CHAR NO-UNDO.
    DEFINE VARIABLE tmpbed       AS CHAR NO-UNDO.
    DEFINE VARIABLE beforebeds   AS CHAR NO-UNDO.
    DEFINE VARIABLE bednums      AS CHAR NO-UNDO.
    DEFINE VARIABLE polyBedNums  AS CHAR NO-UNDO.
    DEFINE VARIABLE riderBedNums AS CHAR NO-UNDO.
    DEFINE VARIABLE lastBedseq   AS INT  NO-UNDO.
    DEFINE VARIABLE bedFound     AS LOG  NO-UNDO.
    DEFINE VARIABLE lastRunSeq   AS INT  NO-UNDO.
    DEFINE VARIABLE tmpdate      AS INT  NO-UNDO.
    DEFINE VARIABLE tmpdatecnt   AS INT  NO-UNDO.
    DEFINE VARIABLE dueByType    AS CHAR NO-UNDO.
    DEFINE VARIABLE dueDate      AS DATE NO-UNDO.
    DEFINE VARIABLE tLoop        AS INT  NO-UNDO.
    DEFINE VARIABLE lastbed      AS CHAR NO-UNDO.
    DEFINE VARIABLE cType        AS CHAR NO-UNDO.
    DEFINE VARIABLE cMaterial    AS CHAR NO-UNDO.
    DEFINE VARIABLE pList        AS CHAR NO-UNDO.
    DEFINE VARIABLE blahint      AS INT  NO-UNDO.
    DEFINE VARIABLE blah         AS CHAR NO-UNDO.
    DEFINE VARIABLE outputfile   AS CHAR NO-UNDO.
    DEFINE VARIABLE foundCurrent AS LOG  NO-UNDO.


    IF OS-GETENV("computername") = "qbtest" THEN DO:
        OUTPUT TO d:\bullseye\dave\tmp\mm-printorder.LOG APPEND.
        PUT UNFORMATTED NOW " Start of print Order - cItemseq=" cItemseq SKIP.
        OUTPUT CLOSE.
    END.


    /*find last bedseq we were on so that we can rollover*/
    lastbedseq = 0. bedfound = NO. bednums = "".
    IF cItemseq = "" THEN DO:
         /*account for records left over from previous run*/
        ASSIGN mmCnt = 0.
           
        /*create run order based off of what section has oldest dates*/
        FOR EACH sign_mm_hdr WHERE sign_mm_hdr.run_time = ? AND sign_mm_hdr.PartialRun = NO:
            IF sign_mm_hdr.qty = sign_mm_hdr.qty_printed THEN NEXT. /*Just to make sure no old completed ones make it in*/
            dueDate = ?.

            IF      INDEX(sign_mm_hdr.matltype,"Corex")     > 0 THEN dueByType = "Corex".
            ELSE IF INDEX(sign_mm_hdr.matltype,"Steel")     > 0 THEN dueByType = "Steel".
            ELSE IF INDEX(sign_mm_hdr.matltype,"Poly")      > 0 THEN dueByType = "Poly".
            ELSE IF INDEX(sign_mm_hdr.matltype,"Alumalite") > 0 THEN dueByType = "Alumalite".
            ELSE IF INDEX(sign_mm_hdr.matltype,"Omegabond") > 0 THEN dueByType = "Omegabond".
            ELSE IF INDEX(sign_mm_hdr.matltype,"Magnetic")  > 0 THEN dueByType = "Magnetic".
            ELSE IF INDEX(sign_mm_hdr.matltype,"Vinyl")     > 0 THEN dueByType = "Vinyl".

            FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.batchseq = sign_mm_hdr.batchseq AND sign_mm_hdr.reprint = NO:
                /*set oldest date of each bed*/
                IF sign_mm_det.due_date < dueDate OR dueDate = ? THEN dueDate = sign_mm_det.due_date.

                /*keep track of oldest due date for each section*/
                FIND dueBy WHERE dueBy.bedseq = sign_mm_hdr.bedseq  AND dueBy.type = dueByType NO-ERROR.
                IF NOT AVAIL(dueBy) THEN DO:
                    CREATE dueBy.
                    ASSIGN dueBy.bedseq = sign_mm_hdr.bedseq.
                           dueBy.type   = dueByType.
                    
                END.
                IF sign_mm_det.due_date < dueBy.avgDate OR dueBy.avgDate = ? THEN dueBy.avgDate = sign_mm_det.due_date.
            END.

            RUN GET_mach_time (sign_mm_hdr.batchseq, sign_mm_hdr.bedseq,OUTPUT sign_mm_hdr.mach_time).
            sign_mm_hdr.due_date = dueDate.
        END.

        /*get the last section that was being worked on*/
        lastbed = "".              
        IF CAN-FIND(FIRST zz_file NO-LOCK WHERE zz_file.zz_key1 = "SIGN-DIGITAL-MACHINE-1") THEN DO:

            FOR EACH zz_file NO-LOCK WHERE zz_file.zz_key1 = "Digital" AND zz_file.zz_log[1] = TRUE:

                foundCurrent = FALSE.
                FOR EACH sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.run_date = TODAY 
                    AND sign_mm_hdr.run_time = ?
                    AND sign_mm_hdr.fbMachine = int(zz_file.zz_key2):

                    RUN getSubstrate.p (sign_mm_hdr.matltype,0,OUTPUT cType,OUTPUT cMaterial).                         

                    IF INDEX(cType,"Steel") = 0
                    THEN ASSIGN lastbed = lastbed + (IF lastbed = "" THEN "" ELSE ",") + cType.                                           
                    ELSE ASSIGN lastbed = lastbed + (IF lastbed = "" THEN "" ELSE ",") + string(sign_mm_hdr.bedseq).  
                    foundCurrent = TRUE.

                    LEAVE.
                END.
                IF NOT foundCurrent THEN /* find the last worked on */
                FOR EACH sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.run_date = TODAY 
                    AND sign_mm_hdr.fbMachine = int(zz_file.zz_key2)
                    BY sign_mm_hdr.run_time DESC:

                    RUN getSubstrate.p (sign_mm_hdr.matltype,0,OUTPUT cType,OUTPUT cMaterial).                         

                    IF INDEX(cType,"Steel") = 0
                    THEN ASSIGN lastbed = lastbed + (IF lastbed = "" THEN "" ELSE ",") + cType.                                           
                    ELSE ASSIGN lastbed = lastbed + (IF lastbed = "" THEN "" ELSE ",") + string(sign_mm_hdr.bedseq).  
                    
                    LEAVE.
                END.
            END.

        END.
        
        /*get list of priority items*/
        FOR EACH h_detail NO-LOCK WHERE h_detail.order_no BEGINS "B-"
            AND h_detail.DATE = TODAY
            BREAK BY h_detail.order_no:
            IF FIRST-OF(h_detail.order_no) AND h_detail.activity = "D11" THEN
                ASSIGN pList = pList + (IF pList = "" THEN "" ELSE ",") + replace(h_detail.order_no,"B-","").
        END.
           
        bedNums = "".
        /*put groups in order*/
        FOR EACH dueBy /* WHERE dueBy.type = ENTRY(iLoop,dueByTypes) */ BY dueBy.avgDate:
            /*set place holder for all corex beds*/
            IF INDEX(bednums,"corex")     > 0 AND dueBy.type = "Corex"     THEN NEXT. 
            IF INDEX(bednums,"Alumalite") > 0 AND dueBy.type = "Alumalite" THEN NEXT.
            IF INDEX(bednums,"Omegabond") > 0 AND dueBy.type = "Omegabond" THEN NEXT.
            IF INDEX(bednums,"Vinyl")     > 0 AND dueBy.type = "Vinyl"     THEN NEXT.
            IF INDEX(bednums,"Magnetic")  > 0 AND dueBy.type = "Magnetic"  THEN NEXT.

            IF CAN-DO(lastBed,dueBy.type) THEN NEXT.
            IF CAN-DO(lastBed,STRING(dueBy.bedSeq)) THEN NEXT.


            IF CAN-DO("2,3,4,6",STRING(dueby.bedseq)) THEN
                ASSIGN riderBedNums = riderBedNums + (IF riderBedNums = "" THEN "" ELSE ",") + STRING(dueBy.Bedseq).
            ELSE IF dueBy.type <> "Steel" AND dueByType <>  "Poly" THEN
                ASSIGN bednums = bednums + (IF bednums = "" THEN "" ELSE ",") + dueBy.type.
            ELSE
                ASSIGN bednums = bednums + (IF bednums = "" THEN "" ELSE ",") + string(dueBy.bedseq).
        END.

        /*set order to last working section, poly, riders, then everything else by due date*/
        ASSIGN  bednums = riderBedNums + (IF riderBedNums = "" THEN "" ELSE ",") + bednums
                bednums = lastbed + "," + bednums. 

        /*put priority batches at top*/
        IF pList > "" THEN DO:
            DO iLoop = 1 TO NUM-ENTRIES(pList):
                FOR EACH sign_mm_hdr WHERE sign_mm_hdr.batchseq = INT(ENTRY(iLoop,pList)):
                    ASSIGN mmCnt              = mmCnt + 1
                           sign_mm_hdr.runseq = mmCnt * 10.
                END.
            END.
        END.
        
        
        /*give runseq to the newly created records*/
        IF mmCnt < 100 THEN mmCnt = 100.
        DO iLoop = 1 TO NUM-ENTRIES(bednums):
            IF ENTRY(iLoop,bednums) = "Poly" THEN DO:
                DO tLoop = 1 TO NUM-ENTRIES(polyBedNums):
                    FIND FIRST signbed NO-LOCK WHERE signbed.seq = int(ENTRY(tLoop,polyBedNums)) NO-ERROR.
                    IF AVAIL signbed THEN DO:
                        FOR EACH sign_mm_hdr WHERE sign_mm_hdr.run_time = ? AND INDEX(matlType,"poly") > 0 AND sign_mm_hdr.PartialRun = NO AND sign_mm_hdr.reprint = NO AND sign_mm_hdr.bedseq = signbed.seq /*Partials First*/
                             BY sign_mm_hdr.inv_part BY sign_mm_hdr.fullbed BY sign_mm_hdr.matlType /*BY sign_mm_hdr.sides*/ BY sign_mm_hdr.due_date:
                             
                            ASSIGN mmCnt              = mmCnt + 1
                                   sign_mm_hdr.runseq = mmCnt * 10.
                        END.
                    END.
                END.
            END.
            ELSE IF CAN-DO("Corex,Alumalite,Omegabond,Decal,Magnetic",ENTRY(iLoop,bednums)) THEN DO:
                FOR EACH sign_mm_hdr WHERE sign_mm_hdr.run_time = ? AND sign_mm_hdr.PartialRun = NO AND INDEX(matlType,ENTRY(iLoop,bednums)) > 0 AND sign_mm_hdr.reprint = NO /*Partials First*/
                     BY sign_mm_hdr.inv_part BY sign_mm_hdr.fullbed BY sign_mm_hdr.matlType /*BY sign_mm_hdr.sides*/ BY sign_mm_hdr.due_date:
                    ASSIGN mmCnt              = mmCnt + 1
                           sign_mm_hdr.runseq = mmCnt * 10.
                    
                END.
            END.
            ELSE DO:
                FIND FIRST signbed NO-LOCK WHERE signbed.seq = int(ENTRY(iLoop,bednums)) NO-ERROR.
                IF AVAIL signbed THEN DO:
                    FOR EACH sign_mm_hdr WHERE sign_mm_hdr.run_time = ? AND INDEX(matlType,"steel") > 0 AND sign_mm_hdr.PartialRun = NO AND sign_mm_hdr.bedseq = signbed.seq AND sign_mm_hdr.reprint = NO /*Partials First*/
                         BY sign_mm_hdr.inv_part BY sign_mm_hdr.fullbed BY sign_mm_hdr.matlType /*BY sign_mm_hdr.sides*/ BY sign_mm_hdr.due_date:
                        ASSIGN mmCnt              = mmCnt + 1
                               sign_mm_hdr.runseq = mmCnt * 10.
                               
                               
                    END.
                END.
            END.
        END.

        /*add in the template runs*/
        tmpbed = "".
        FOR EACH sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.run_date = ? AND sign_mm_hdr.reprint = NO BREAK BY sign_mm_hdr.bedseq BY sign_mm_hdr.runseq:
            IF FIRST-OF(sign_mm_hdr.bedseq) THEN DO:

            IF TRUE THEN DO:
                    IF NOT sign_mm_hdr.BatchNested THEN DO:
                        ASSIGN cTemplate = STRING(sign_mm_hdr.bedseq).
                        IF LENGTH(cTemplate, "character") = 1 THEN DO:
                             ASSIGN cTemplate = "bed-0" + cTemplate + ".pdf".
                        END.
                        ELSE DO:
                             ASSIGN cTemplate = "bed-" + cTemplate + ".pdf".
                        END.
                        ASSIGN cTemplate = networkshare + "signart\DigitalBedTemplates\" + cTemplate.
                    END.
                    ELSE DO:
                        blahint = 0.
                        RUN getHotfolder(sign_mm_hdr.batchseq,sign_mm_hdr.matlType,INPUT-OUTPUT blahint,OUTPUT outputfile,OUTPUT blah,OUTPUT blah).
                        ASSIGN cTemplate = outputfile + "\" + "batch" + string(sign_mm_hdr.batchseq) + "_1_template.pdf".
                    END.
                      
                    IF CAN-DO(cCreated,cTemplate) OR sign_mm_hdr.matltype = "Template" THEN NEXT.
                    cCreated = cCreated + (IF cCreated = "" THEN "" ELSE ",") + cTemplate.
                    
                    nextSeq = NEXT-VALUE(seq-mm-batch).
                    DO WHILE CAN-FIND(sign_mm_hdr WHERE sign_mm_hdr.batchseq = nextSeq):
                        nextSeq = NEXT-VALUE(seq-mm-batch).
                    END.
                    IF SEARCH(cTemplate) <> ? THEN DO:
                        CREATE b_mm_hdr.
                        BUFFER-COPY sign_mm_hdr EXCEPT sign_mm_hdr.runseq sign_mm_hdr.batchseq TO b_mm_hdr.
                        ASSIGN b_mm_hdr.BATCHseq    = nextseq
                               b_mm_hdr.runseq      = sign_mm_hdr.runseq - 5
                               b_mm_hdr.fullbed     = YES
                               b_mm_hdr.sides       = 1
                               b_mm_hdr.qty         = 1
                               b_mm_hdr.matltype    = "Template". /*"Decal"*/
    
            
                        FOR FIRST sign_mm_det NO-LOCK OF sign_mm_hdr: /*sign details for template batch*/
                            CREATE b_mm_det.
                            BUFFER-COPY sign_mm_det EXCEPT sign_mm_det.itemseq sign_mm_det.batchseq sign_mm_det.artfile TO b_mm_det.
                            
                            ASSIGN b_mm_det.batchseq        = nextseq
                                   b_mm_det.artfile         = cTemplate
                                   b_mm_det.pt_hotfolderseq = 11. /*Decal*/
    
                        END.
                        RUN GET_mach_time (b_mm_hdr.batchseq, b_mm_hdr.bedseq,OUTPUT b_mm_hdr.mach_time).
                        IF AVAIL b_mm_hdr   THEN RELEASE b_mm_hdr.
                        IF AVAIL b_mm_det   THEN RELEASE b_mm_det.
                    END.
                END.
            END.
        END.

        /************************************************/
        /* Reorder based on what machines are available */
        /************************************************/
        
        IF OS-GETENV("computername") = "qbtest" THEN DO:
            OUTPUT TO d:\bullseye\dave\tmp\mm-printorder.LOG APPEND.
            PUT UNFORMATTED NOW " Start of mm-batch-order" SKIP.
            FOR EACH sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.crt_date = TODAY BREAK BY sign_mm_hdr.fbMachine:
                cnt = cnt + 1.
                IF LAST-OF(sign_mm_hdr.fbMachine) THEN DO:
                    DISPLAY sign_mm_hdr.fbMachine cnt (TOTAL).
                    cnt = 0.
                END.
            END.
            OUTPUT CLOSE.
        END.
        RUN mm-batch-order.p (THIS-PROCEDURE).
        IF OS-GETENV("computername") = "qbtest" THEN DO:
            OUTPUT TO d:\bullseye\dave\tmp\mm-printorder.LOG APPEND.
            PUT UNFORMATTED NOW " Finished mm-batch-order" SKIP.
            FOR EACH sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.crt_date = TODAY BREAK BY sign_mm_hdr.fbMachine:
                cnt = cnt + 1.
                IF LAST-OF(sign_mm_hdr.fbMachine) THEN DO:
                    DISPLAY sign_mm_hdr.fbMachine cnt (TOTAL).
                    cnt = 0.
                END.
            END.
            OUTPUT CLOSE.
        END.
    END.
    ELSE DO:
        /*find last reprint bed and put it after it and/or before all of normal beds*/
        mmCnt = 0.
        FOR LAST sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.run_date = ? AND sign_mm_hdr.PartialRun = NO AND sign_mm_hdr.runseq < 1000 BY sign_mm_hdr.runseq:
            ASSIGN lastRunSeq = sign_mm_hdr.runseq.
            ASSIGN mmCnt = int(lastrunseq / 10) . 
        END.

        IF mmCnt < 30 THEN mmCnt = 30.
        FOR EACH sign_mm_hdr WHERE sign_mm_hdr.RUN_date = ? AND sign_mm_hdr.rerun = YES:
            RUN GET_mach_time (sign_mm_hdr.batchseq, sign_mm_hdr.bedseq,OUTPUT sign_mm_hdr.mach_time).
            ASSIGN mmCnt              = mmCnt + 1
                   sign_mm_hdr.runseq = mmCnt * 10.
            ASSIGN sign_mm_hdr.fbMachine = int(REPLACE(cItemSeq,"MACH","")) NO-ERROR.
/*                    sign_mm_hdr.rerun  = NO. */
        END.
    END.

    RUN mm-reprint-to-top.p (THIS-PROCEDURE).

END PROCEDURE.


PROCEDURE RecordHdrDelete:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER inBatchNo   AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER inProcedure AS CHAR NO-UNDO.
    
    FIND zz_file WHERE zz_file.zz_key1 = "mm-delete" AND zz_file.zz_key2 = "batch" + STRING(inBatchNo) NO-ERROR.
    IF NOT AVAIL zz_file THEN DO:
        CREATE zz_file.
        ASSIGN zz_file.zz_key1 = "mm-delete"
               zz_file.zz_key2 = "batch" + STRING(inBatchNo)
               zz_file.zz_char[1] = "mm-pp.p"
               zz_file.zz_char[2] = inProcedure
               zz_file.zz_char[3] = STRING(TODAY)
               zz_file.zz_char[4] = STRING(TIME,"HH:MM:SS")
               zz_file.zz_char[6] = "LIVE".
    
    END.
    RELEASE zz_file.

END PROCEDURE.


PROCEDURE Regroup:
    DEFINE INPUT PARAMETER cItemseq AS CHAR NO-UNDO.
    DEFINE VARIABLE tmpchar         AS CHAR NO-UNDO.
    DEFINE VARIABLE tmpint          AS INT  NO-UNDO.
    DEFINE VARIABLE numLeft         AS INT  NO-UNDO.
    DEFINE VARIABLE tmpseq          AS INT  NO-UNDO.
    DEFINE VARIABLE pCnt            AS DEC  NO-UNDO.
    DEFINE VARIABLE sCnt            AS DEC  NO-UNDO.

    IF cItemseq = "" THEN DO:
        /*pass 1 - attempt for perfect matches*/
        FOR EACH signbed NO-LOCK:
            FOR EACH sign_mm_hdr WHERE sign_mm_hdr.run_date = ? AND sign_mm_hdr.bedseq = signbed.seq AND sign_mm_hdr.fullbed = FALSE: 
                IF sign_mm_hdr.reprint THEN NEXT.
                FIND LAST sign_mm_det OF sign_mm_hdr NO-ERROR.
                IF AVAIL sign_mm_det THEN DO:
                    FOR EACH b_mm_hdr WHERE b_mm_hdr.run_date = ? AND b_mm_hdr.fullbed = FALSE AND b_mm_hdr.bedseq = signbed.seq AND b_mm_hdr.sides = sign_mm_hdr.sides 
                        AND b_mm_hdr.batchseq <> sign_mm_hdr.batchseq AND b_mm_hdr.pt_hotfolderseq = sign_mm_hdr.pt_hotfolderseq AND b_mm_hdr.inv_part = sign_mm_hdr.inv_part:
                        IF b_mm_hdr.reprint THEN NEXT.
                        FIND LAST b_mm_det OF b_mm_hdr NO-ERROR.
                        ASSIGN numleft = 0 tmpint = 0 tmpchar = "" pCnt = 0 sCnt = 0.
                        IF AVAIL b_mm_det THEN DO:
                            IF b_mm_det.POSITION + sign_mm_det.POSITION = (signbed.userdec1 * signbed.userdec2) THEN DO:
                                ASSIGN pCnt = b_mm_det.POSITION.
    
                                FOR EACH sign_mm_det OF sign_mm_hdr:
                                    CREATE buf_mm_det.
                                    BUFFER-COPY sign_mm_det EXCEPT sign_mm_det.batchseq sign_mm_det.POSITION TO buf_mm_det.
                                    ASSIGN pCnt                = pCnt + 1
                                           buf_mm_det.batchseq = b_mm_hdr.batchseq
                                           buf_mm_det.POSITION = pCnt.
    
                                    DELETE sign_mm_det.
                                END.
                                ASSIGN b_mm_hdr.fullbed = TRUE.
                                RUN RecordHdrDelete(sign_mm_hdr.batchseq, "reGroup").
                                DELETE sign_mm_hdr.
                                LEAVE.
                            END.
                            ELSE NEXT.
                        END.
                        ELSE NEXT.
                        IF NOT AVAILABLE sign_mm_det THEN LEAVE.
                    END. /*end fE b_mm_hdr*/
                END.
            END.
        END.
        IF AVAILABLE sign_mm_det THEN RELEASE sign_mm_det.
        
        /*pass 2 - now fill in the gaps*/
        DO iloop = 1 TO 2: /*picks up combinable partials on second pass*/ /*6/16/15*/
            FOR EACH signbed NO-LOCK:
                FOR EACH sign_mm_hdr WHERE sign_mm_hdr.run_date = ? AND sign_mm_hdr.fullbed = FALSE AND sign_mm_hdr.bedseq = signbed.seq /*AND INDEX(sign_mm_hdr.matlType,"Corex") = 0*/ :
                    IF sign_mm_hdr.reprint THEN NEXT.
                    DO WHILE sign_mm_hdr.fullbed = FALSE:
                        FIND FIRST b_mm_hdr WHERE b_mm_hdr.run_date = ? AND b_mm_hdr.fullbed = FALSE AND b_mm_hdr.bedseq = sign_mm_hdr.bedseq AND b_mm_hdr.sides = sign_mm_hdr.sides 
                            AND b_mm_hdr.batchseq <> sign_mm_hdr.batchseq AND b_mm_hdr.inv_part = sign_mm_hdr.inv_part AND b_mm_hdr.pt_hotfolderseq = sign_mm_hdr.pt_hotfolderseq AND b_mm_hdr.reprint = NO NO-ERROR.
                        IF AVAIL b_mm_hdr THEN DO:
                            DO WHILE CAN-FIND(FIRST b_mm_det OF b_mm_hdr):
                                FIND LAST sign_mm_det OF sign_mm_hdr NO-ERROR.
                                IF AVAIL sign_mm_det THEN DO:
                                    IF sign_mm_det.POSITION = (signbed.userdec1 * signbed.userdec2) THEN DO:
                                        ASSIGN sign_mm_hdr.fullbed = TRUE.
                                        LEAVE.
                                    END.
                                    ELSE DO:
                                        FOR LAST b_mm_det OF b_mm_hdr:
                                            CREATE buf_mm_det.
                                            BUFFER-COPY b_mm_det EXCEPT b_mm_det.batchseq b_mm_det.POSITION TO buf_mm_det.
                                            ASSIGN buf_mm_det.batchseq = sign_mm_hdr.batchseq
                                                   buf_mm_det.POSITION = sign_mm_det.POSITION + 1.
        
                                            IF b_mm_det.POSITION = 1 THEN DO:
                                                RUN RecordHdrDelete(b_mm_hdr.batchseq, "reGroup(2)").
                                                DELETE b_mm_hdr.
                                            END.
                                            DELETE b_mm_det.
                                        END.
                                    END.
                                END.
                                ELSE LEAVE.
                            END.
                        END.
                        ELSE LEAVE.
                    END.
                END.
            END.
        END.
        IF AVAILABLE sign_mm_det THEN RELEASE sign_mm_det.
    END.
    ELSE DO:
        DO iloop = 1 TO 2: /*picks up combinable partials on second pass*/ /*6/16/15*/
            FOR EACH signbed NO-LOCK:
                FOR EACH sign_mm_hdr WHERE sign_mm_hdr.run_date = ? AND sign_mm_hdr.rerun = YES AND sign_mm_hdr.fullbed = FALSE AND sign_mm_hdr.bedseq = signbed.seq /*AND INDEX(sign_mm_hdr.matlType,"Corex") = 0*/ :
                    DO WHILE sign_mm_hdr.fullbed = FALSE:
                        FIND FIRST b_mm_hdr WHERE b_mm_hdr.run_date = ? AND b_mm_hdr.rerun = YES AND b_mm_hdr.fullbed = FALSE AND b_mm_hdr.bedseq = sign_mm_hdr.bedseq AND b_mm_hdr.sides = sign_mm_hdr.sides 
                            AND b_mm_hdr.batchseq <> sign_mm_hdr.batchseq AND b_mm_hdr.pt_hotfolderseq = sign_mm_hdr.pt_hotfolderseq AND b_mm_hdr.inv_part = sign_mm_hdr.inv_part NO-ERROR.
                        IF AVAIL b_mm_hdr THEN DO:
                            DO WHILE CAN-FIND(FIRST b_mm_det OF b_mm_hdr):
                                FIND LAST sign_mm_det OF sign_mm_hdr NO-ERROR.
                                IF AVAIL sign_mm_det THEN DO:
                                    IF sign_mm_det.POSITION = (signbed.userdec1 * signbed.userdec2) THEN DO:
                                        ASSIGN sign_mm_hdr.fullbed = TRUE.
                                        LEAVE.
                                    END.
                                    ELSE DO:
                                        FOR LAST b_mm_det OF b_mm_hdr:
                                            CREATE buf_mm_det.
                                            BUFFER-COPY b_mm_det EXCEPT b_mm_det.batchseq b_mm_det.POSITION TO buf_mm_det.
                                            ASSIGN buf_mm_det.batchseq = sign_mm_hdr.batchseq
                                                   buf_mm_det.POSITION = sign_mm_det.POSITION + 1.
        
                                            IF b_mm_det.POSITION = 1 THEN DO:
                                                RUN RecordHdrDelete(b_mm_hdr.batchseq, "reGroup(3)").
                                                DELETE b_mm_hdr.
                                            END.
                                            DELETE b_mm_det.
                                        END.
                                    END.
                                END.
                                ELSE LEAVE.
                            END.
                        END.
                        ELSE LEAVE.
                    END.
                END.
            END.
        END.
        IF AVAILABLE sign_mm_det THEN RELEASE sign_mm_det.
    END.

END PROCEDURE.


PROCEDURE ReleaseAll:
    /*this is an attempt to release anything that might still be locked...*/
    /*... in order to prevent record locks in other programs              */

    /*release all buffers*/
    IF AVAILABLE  b_mm_det       THEN RELEASE b_mm_det.
    IF AVAILABLE  buf_mm_det     THEN RELEASE buf_mm_det.
    IF AVAILABLE  bb_mm_det      THEN RELEASE bb_mm_det.
    IF AVAILABLE  buf_mm_hdr     THEN RELEASE buf_mm_hdr.
    IF AVAILABLE  b_mm_hdr       THEN RELEASE b_mm_hdr.
    IF AVAILABLE  bb_mm_hdr      THEN RELEASE bb_mm_hdr.
    IF AVAILABLE  b_mm_reprint   THEN RELEASE b_mm_reprint.
    IF AVAILABLE  buf_mm_reprint THEN RELEASE buf_mm_reprint.
    IF AVAILABLE  b_signbed      THEN RELEASE b_signbed.
    IF AVAILABLE  buf_signbed    THEN RELEASE buf_signbed.
    IF AVAILABLE  buf_beddet     THEN RELEASE buf_beddet.
    IF AVAILABLE  b_signbeddet   THEN RELEASE b_signbeddet.
    IF AVAILABLE  b_beddet       THEN RELEASE b_beddet.
    IF AVAILABLE  b_squ_mat      THEN RELEASE b_squ_mat.
    IF AVAILABLE  buf_ptdet      THEN RELEASE buf_ptdet.
    IF AVAILABLE  b_ptdet        THEN RELEASE b_ptdet.
    IF AVAILABLE  b_items        THEN RELEASE b_items.
    IF AVAILABLE  b_so_items     THEN RELEASE b_so_items.
    IF AVAILABLE  b_squ_plan     THEN RELEASE b_squ_plan.
    IF AVAILABLE  mm_file        THEN RELEASE MM_FILE.
    IF AVAILABLE  zz_msg         THEN RELEASE zz_msg.

    /*release all normal records*/
    IF AVAILABLE  sign_mm_det     THEN RELEASE sign_mm_det.
    IF AVAILABLE  sign_mm_hdr     THEN RELEASE sign_mm_hdr.
    IF AVAILABLE  sign_mm_reprint THEN RELEASE sign_mm_reprint.
    IF AVAILABLE  signbed         THEN RELEASE signbed.
    IF AVAILABLE  signbeddet      THEN RELEASE signbeddet.
    IF AVAILABLE  squ_mat         THEN RELEASE squ_mat.
    IF AVAILABLE  squ_ptdet       THEN RELEASE squ_ptdet.
    IF AVAILABLE  so_items        THEN RELEASE so_items.
    IF AVAILABLE  squ_plan        THEN RELEASE squ_plan.
    IF AVAILABLE  zz_file         THEN RELEASE zz_file.
END PROCEDURE.


PROCEDURE RemoveFaults:
    DEFINE VARIABLE cItemseq AS INT NO-UNDO.
    DEFINE VARIABLE tmpCnt   AS INT NO-UNDO.
    DEFINE VARIABLE alphaOk  AS LOG NO-UNDO.
    DEFINE VARIABLE cICseq   AS INT NO-UNDO.


    FOR EACH tmp_ttArt: DELETE tmp_ttArt. END.
    FOR EACH ttArt: 

        FIND rpt_det WHERE rpt_det.itemseq = ttArt.ttItemSeq NO-ERROR.
        IF AVAILABLE rpt_det AND rpt_det.issue > ""
            AND INDEX(rpt_det.issue,"Queue") = 0 THEN NEXT.

        CREATE tmp_ttArt.
        BUFFER-COPY ttArt TO tmp_ttArt.
    END.

    FOR EACH rpt_det WHERE rpt_det.issue <> "":
        RUN trimbeds(STRING(rpt_det.itemseq)). /*deletes all ttart records with this itemseq*/
    END.

    FOR EACH tmp_ttArt BY tmp_ttArt.ttDue:
        EMPTY TEMP-TABLE ttchg.
        IF CAN-FIND(zz_file WHERE zz_file.zz_key1 = "MATERIAL-IGNORE" AND zz_file.zz_key2 = string(tmp_ttArt.ttItemseq)) THEN NEXT. 
        FIND so_items NO-LOCK WHERE so_items.itemseq = tmp_ttArt.ttItemseq NO-ERROR.
        IF AVAIL so_items THEN DO:
            FIND squ_plan NO-LOCK WHERE squ_plan.itemseq = tmp_ttArt.ttItemseq NO-ERROR.
            IF AVAIL squ_plan AND squ_plan.ic_no <> "" THEN DO:
                alphaOk = FALSE.
                RUN findIcNo(so_items.itemseq,so_items.so_no,so_items.ITEM_No,STRING(squ_plan.ic_no),so_items.part_no,NO,OUTPUT cICseq).
                FIND b_items NO-LOCK WHERE b_items.ITEMseq = cICSeq NO-ERROR.
                IF NOT AVAIL b_items THEN alphaOk = TRUE.
                IF AVAIL b_items AND b_items.orderqty = b_items.ship_qty THEN alphaOk = TRUE.
            END.
            ELSE alphaOK = TRUE.
        END.
        ELSE alphaOk = FALSE.

        IF alphaOk = TRUE THEN DO:
            FOR EACH m_usage NO-LOCK WHERE m_usage.order_no = tmp_ttArt.ttSo
                AND m_usage.ITEM_no = STRING(tmp_ttArt.ttItemNo)
                AND m_usage.IN_process = ?:
                IF CAN-DO("LFBGB4848,LFBGB4848W,LFBTH4848,LFBTH4848W",m_usage.part_no) THEN NEXT.
                /*fix for double tmp_ttArt recs due to steeltents*/
                IF CAN-FIND(FIRST mdet WHERE mdet.mRec = RECID(m_usage)) THEN NEXT.
                ELSE DO:
                    CREATE mdet.
                    ASSIGN mdet.mRec = RECID(m_usage).
                END.

                FIND partfile NO-LOCK WHERE partfile.part_no = m_usage.part_no NO-ERROR.
                IF AVAILABLE partfile AND partfile.prd_line = "S1H" THEN NEXT.
    
                FIND ttMat WHERE ttMat.ttPart = m_usage.part_no NO-ERROR.
                IF NOT AVAILABLE ttMat THEN DO:
                    /* Include all of site 7 and 11 inventory here */
                    CREATE ttMat.
                    ASSIGN ttMat.ttPart = m_usage.part_no.
                    FOR EACH sitepart NO-LOCK WHERE sitepart.part_no = m_usage.part_no:
    
                        IF m_usage.part_no = tmp_ttArt.ttInvPart THEN DO: 
                            IF sitepart.site = "7" THEN ttMat.ttQty = ttMat.ttQty + sitepart.on_hand.
                        END.
                        ELSE DO:
                            IF CAN-DO("7,11",sitepart.site) THEN
                                ASSIGN ttMat.ttQty = ttMat.ttQty + sitepart.on_hand.
                        END.
                    END.
                END.
/*                 ttMat.ttQty = ttMat.ttQty - (tmp_ttArt.ttqty * (m_usage.quantity / so_items.orderqty)). */
                ASSIGN ttMat.ttQty = ttMat.ttQty - m_usage.quantity.
    
                FIND FIRST ttchg WHERE ttChg.ttPart = ttMat.ttPart NO-ERROR.
                IF NOT AVAIL ttChg THEN DO:
                    CREATE ttChg.
                    ASSIGN ttChg.ttPart = ttMat.ttPart.
                END.
                ASSIGN ttChg.ttQty = m_usage.quantity.
    
    
                IF ttMat.ttQty < 0 AND ttMat.ttPart <> "CUSTOM MATERIAL" THEN DO:
                    RUN ReportIssues(tmp_ttArt.ttItemseq,"MM-Not Enough Material",tmp_ttArt.ttso,STRING(tmp_ttArt.ttItemNO),"",ttMat.ttPart,"","","","").
                    RUN trimbeds(STRING(tmp_ttArt.ttItemseq)).
                    /*back out of material qty changes*/
                    FOR EACH ttchg:
                        FIND ttmat WHERE ttMat.ttPart = ttChg.ttPart NO-ERROR.
                        IF AVAIL ttMat THEN 
                            ASSIGN ttMat.ttQty = ttMat.ttQty + ttChg.ttQty.
                    END.
                    LEAVE.
                END.
            END.
        END.
        ELSE DO:
            RUN ReportIssues(tmp_ttArt.ttItemseq,"MM-Not Enough Material",tmp_ttArt.ttso,STRING(tmp_ttArt.ttItemNO),"","Special Blanks - Alpha Order","","","","").
            RUN trimbeds(STRING(tmp_ttArt.ttItemseq)).
        END.
    END.
    
END PROCEDURE.


PROCEDURE RemoveFIB:
    /*remove failed image batch*/
    DEFINE INPUT  PARAMETER pBatchList AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER whereFrom  AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER anyStarted AS LOG  NO-UNDO.


    anyStarted = NO.
    DO iLoop = 1 TO NUM-ENTRIES(pBatchList):
        FIND sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.batchseq = int(ENTRY(iLoop,pBatchList)) NO-ERROR.
        IF AVAIL(sign_mm_hdr) THEN DO:
            IF sign_mm_hdr.qty_printed > 0 OR sign_mm_hdr.run_time <> ? THEN anyStarted = YES.
        END.
    END.

    IF anyStarted THEN RETURN.

    DO iLoop = 1 TO NUM-ENTRIES(pBatchList):
        RUN deleteMMhdr.p(int(ENTRY(iLoop,pBatchList)),whereFrom,"removeFIB",cDBase). 
    END.
END PROCEDURE.


PROCEDURE RemoveFiles:
    DEFINE INPUT PARAMETER inFile  AS CHAR NO-UNDO.

    DEFINE VARIABLE fname    AS CHAR   NO-UNDO.
    DEFINE VARIABLE tmpfile  AS CHAR   NO-UNDO.
    DEFINE VARIABLE tmpfile2 AS CHAR   NO-UNDO.
    DEFINE VARIABLE topHand  AS HANDLE NO-UNDO.
    DEFINE VARIABLE midHand  AS HANDLE NO-UNDO.
    DEFINE VARIABLE lowHand  AS HANDLE NO-UNDO.
    /*goes 3 levels deep to delete files...replacement for deleting entire top directory*/


    IF inFile > "" THEN DO:
        inFile = inFile + "\".
        ASSIGN FILE-INFO:FILE-NAME = inFile.
        IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
            INPUT STREAM toplvl FROM OS-DIR(inFile).
            topHand = STREAM toplvl:HANDLE.
            IMPORT STREAM toplvl ^.
            IMPORT STREAM toplvl ^.
            REPEAT:
                IMPORT STREAM toplvl fname.
                FILE-INFO:FILE-NAME = infile + fname.
                IF INDEX(FILE-INFO:FILE-TYPE,"D") > 0 THEN DO:
                    tmpfile = FILE-INFO:FULL-PATHNAME + "\".
                    INPUT STREAM midlvl FROM OS-DIR(tmpfile).
                    midHand = STREAM midlvl:HANDLE.
                    IMPORT STREAM midlvl ^.
                    IMPORT STREAM midlvl ^.
                    REPEAT:
                        IMPORT STREAM midlvl fname.
                        FILE-INFO:FILE-NAME =  tmpfile + fname.
                        IF INDEX(FILE-INFO:FILE-TYPE,"D") > 0 THEN DO:
                            tmpfile2 = FILE-INFO:FULL-PATHNAME + "\".
                            INPUT STREAM lowlvl FROM OS-DIR(tmpfile2).
                            lowHand = STREAM lowlvl:HANDLE.
                            IMPORT STREAM lowlvl ^.
                            IMPORT STREAM lowlvl ^.
                            REPEAT:
                                IMPORT STREAM lowlvl fname.
                                FILE-INFO:FILE-NAME =  tmpfile2 + fname.
                                IF INDEX(FILE-INFO:FILE-TYPE,"F") > 0 AND INDEX(FILE-INFO:FILE-TYPE,"W") > 0 THEN DO:
                                    OS-DELETE value(FILE-INFO:FULL-PATHNAME).
                                END.
                               
                            END.
                        END.
                        ELSE IF INDEX(FILE-INFO:FILE-TYPE,"F") > 0 AND INDEX(FILE-INFO:FILE-TYPE,"W") > 0 THEN DO:
                            OS-DELETE value(FILE-INFO:FULL-PATHNAME).
                        END.
                    END.
                END.
                ELSE IF INDEX(FILE-INFO:FILE-TYPE,"F") > 0 AND INDEX(FILE-INFO:FILE-TYPE,"W") > 0 THEN DO:
                    OS-DELETE value(FILE-INFO:FULL-PATHNAME).
                END.
            END.
        END.
    END.
    IF VALID-HANDLE(topHand) THEN INPUT STREAM topLvl CLOSE.
    IF VALID-HANDLE(midHand) THEN INPUT STREAM midLvl CLOSE.
    IF VALID-HANDLE(lowHand) THEN INPUT STREAM lowlvl CLOSE.
END PROCEDURE.


PROCEDURE RemoveSingle:
    DEFINE INPUT  PARAMETER cItemseq AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER cSo      AS CHAR    NO-UNDO.
    DEFINE OUTPUT PARAMETER cItem    AS INTEGER NO-UNDO.

    /*Just reports the order and item so we can log it*/
    FIND b_so_items NO-LOCK WHERE b_so_items.itemseq = cItemseq NO-ERROR.
    IF AVAIL b_so_items THEN DO:
        ASSIGN cSo   = b_so_items.so_no
               cItem = b_so_items.ITEM_no. 
    END.
     /*deletes the bad bleed items*/
    FOR EACH bb_mm_det WHERE bb_mm_det.itemseq = cItemseq BREAK BY bb_mm_det.batchseq:
        IF LAST-OF(bb_mm_det.batchseq) THEN DO:
            FIND bb_mm_hdr WHERE bb_mm_hdr.batchseq = bb_mm_det.batchseq NO-ERROR.
            IF AVAIL bb_mm_hdr THEN DO:
                IF NOT CAN-FIND(FIRST buf_mm_det WHERE buf_mm_det.batchseq = bb_mm_det.batchseq AND buf_mm_det.itemseq <> bb_mm_det.itemseq) THEN DO:
                    DELETE bb_mm_hdr.
                    
                END.
                ELSE DO:
                    ASSIGN bb_mm_hdr.fullbed = FALSE.
                END.
            END.
        END.
        DELETE bb_mm_det.
    END.
END PROCEDURE.


PROCEDURE Renumber:
    DEFINE INPUT  PARAMETER cBed      AS INT  NO-UNDO.
    DEFINE INPUT  PARAMETER oPosition AS INT  NO-UNDO.
    DEFINE INPUT  PARAMETER cType     AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER iHorz     AS INT  NO-UNDO.
    DEFINE INPUT  PARAMETER iVert     AS INT  NO-UNDO.
    DEFINE OUTPUT PARAMETER nPosition AS INT  NO-UNDO.
    DEFINE OUTPUT PARAMETER useHalf   AS LOG  NO-UNDO.

    DEFINE VARIABLE nRow    AS INT NO-UNDO.
    DEFINE VARIABLE oRow    AS INT NO-UNDO.
    DEFINE VARIABLE tmpPos  AS INT NO-UNDO.
    DEFINE VARIABLE pLoop   AS INT NO-UNDO.
    DEFINE VARIABLE maxPos  AS INT NO-UNDO.
    DEFINE VARIABLE lastCol AS DEC NO-UNDO.
    DEFINE VARIABLE numPos  AS INT NO-UNDO.

    
    nPosition = 0. tmpPos = 0. nRow = 0. oRow = 0.
    IF cType = "Horizontal" THEN DO:
        /*IF oPosition MODULO 2 = 1 THEN nPosition = (iTotNum - 1) - oPosition + 1. /*odd */
        ELSE nPosition = iTotNum - oPosition + 2.                                 /*even*/ */
        DO pLoop = 1 TO iHorz:
            IF oPosition <= (pLoop * iVert) THEN DO:
                ASSIGN oRow = pLoop.
                LEAVE.
            END.
        END.
        ASSIGN nRow      = iHorz + 1 - oRow.
               nPosition = (nRow * iVert) - ((oRow * ivert) - oPosition).
/*         IF cBed = 31 THEN DO:                                                  /********************************************/ */
/*             IF oPosition MODULO 2 = 0 THEN nPosition = oPosition - 1. /*even*/ /*Will need to comment this if they use zune*/ */
/*             ELSE nPosition = oPosition + 1.                           /*odd */ /********************************************/ */
/*         END.                                                                   /********************************************/ */
    END.
     IF cType = "Vertical" THEN DO:
         /*IF oPosition MODULO 2 = 0 THEN nPosition = oPosition - 1. /*even*/
         ELSE nPosition = oPosition + 1.                           /*odd */
         DO pLoop = 1 TO iVert:
             IF oPosition <= (pLoop * iVert) THEN DO:
                 oRow = pLoop.
                 LEAVE.
             END.
         END.
         ASSIGN nRow      = oRow
                nPosition = ((nRow - 1) * iVert) + (((nRow * iVert) + 1) - oPosition).*/
     END.

    /*half or full sheet?*/
    maxPos = 0. useHalf = NO. lastCol = 0. numPos = 0.
    FIND b_mm_hdr NO-LOCK WHERE b_mm_hdr.batchseq = cBed NO-ERROR.
    IF AVAIL b_mm_hdr THEN DO:
        FOR LAST b_mm_det OF b_mm_hdr BY b_mm_det.POSITION:
            FIND buf_signbed NO-LOCK WHERE buf_signbed.seq = b_mm_hdr.bedseq NO-ERROR.
            IF AVAIL buf_signbed THEN DO:
                numPos = buf_signbed.userdec1 /* * buf_signbed.userdec2 */ .
                LastCol = NumPos / 2.
                
                IF lastCol < 1 THEN DO: /*only 1 position*/
                    useHalf = NO.
                    LEAVE.
                END.
                IF lastCol = 1 THEN DO: /*only 2 position*/
                    useHalf = YES.
                    LEAVE.
                END.
                
                IF lastCol <> INT(lastCol) THEN lastCol = INT(lastCol) - 1.
                ELSE IF LastCol MODULO 2 = 1 THEN lastCol = lastCol - 1.

                maxPos = lastCol * buf_signbed.userdec2. /*largest position in that coloumn*/
                useHalf = IF b_mm_det.POSITION > maxPos THEN NO ELSE YES.
            END.
        END.
    END.
    
    
END PROCEDURE.


PROCEDURE ReportIssues:
    DEFINE INPUT PARAMETER tmpSeq  AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER subject AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER OrderNo AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ItemNo  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cSize   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cImage  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cBatch  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cPos    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER SendXml AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER RecXml  AS CHAR NO-UNDO.
    DEFINE VARIABLE fileOk AS LOG NO-UNDO.

    FIND rpt_det WHERE rpt_det.itemseq = tmpSeq NO-ERROR.
    IF AVAIL rpt_det THEN DO:
        IF NOT rpt_det.issue MATCHES "*" + subject + "*" THEN DO:
            ASSIGN rpt_det.issue  = rpt_det.issue + (IF rpt_det.issue = "" THEN "" ELSE "|") + subject.
            IF cImage <> "" THEN
                ASSIGN rpt_det.reason = rpt_det.reason + (IF rpt_det.reason = "" THEN "" ELSE "|") + cImage.
        END.
    END.

    CREATE issue.
    ASSIGN issue.xSubject = subject
           issue.xOrder   = OrderNo
           issue.xItem    = ItemNo
           issue.xSize    = cSize
           issue.xImage   = cImage
           issue.xBatch   = cBatch
           issue.xPos     = cPos
           issue.xSendXml = SendXml
           issue.xRecXml  = RecXml.
    
    IF OS-GETENV("computername") = "qbprod" OR OS-GETENV("computername") = "qbtest" OR OS-GETENV("computername") = "pennyt" THEN DO:
        RUN fileExists(cLogLoc,OUTPUT fileOk). 
        IF fileOk THEN
            OUTPUT TO VALUE(cLogLoc + "\MediaManagerlost-" + REPLACE(STRING(TODAY),"/","") + ".csv") APPEND.
        ELSE 
            OUTPUT TO VALUE("\\qbtest\bullseye\scripts\logfiles\MediaManagerlost-" + REPLACE(STRING(TODAY),"/","") + ".csv") APPEND.
        EXPORT DELIMITER "," issue.
        OUTPUT CLOSE.
    END.
END PROCEDURE.


PROCEDURE Resequense:
    DEFINE VARIABLE newSeq AS INT NO-UNDO.

    /*get all of the current batch numbers*/
    FOR EACH sign_mm_hdr WHERE sign_MM_hdr.RUN_date = ?:
        IF NOT CAN-FIND(FIRST ttSeq WHERE ttSeq.ttSeqNum = sign_mm_hdr.batchseq) THEN DO:
            CREATE ttSeq.
            ASSIGN ttSeq.ttSeqNum = sign_mm_hdr.batchseq.
        END.
        FOR EACH sign_mm_det OF sign_mm_hdr:
            ASSIGN sign_mm_det.batchseq = - sign_mm_det.batchseq.
        END.
        ASSIGN sign_mm_hdr.batchseq = - sign_mm_hdr.batchseq.
    END.

    /*put them in order*/
    FOR EACH sign_mm_hdr WHERE sign_mm_hdr.RUN_date = ? BY sign_mm_hdr.runseq:
        FOR FIRST ttSeq BY ttSeq.ttSeqNum:
            ASSIGN newSeq = ttSeq.ttSeqNum.
            DELETE ttSeq. /*make sure only try to use once*/
        END.
        FOR EACH sign_mm_det OF sign_mm_hdr:
            ASSIGN sign_mm_det.batchseq = newSeq.
        END.
        ASSIGN sign_mm_hdr.batchseq = newSeq.
    END.
END PROCEDURE.


PROCEDURE Resets:
    DEFINE INPUT PARAMETER purge      AS LOG  NO-UNDO.
    DEFINE INPUT PARAMETER blankslate AS LOG NO-UNDO.

    DEFINE VARIABLE delme           AS LOG  NO-UNDO.
    DEFINE VARIABLE delme2          AS LOG  NO-UNDO.
    DEFINE VARIABLE cnt             AS INT  NO-UNDO.
    DEFINE VARIABLE tmpcmd          AS CHAR NO-UNDO.
    DEFINE VARIABLE tmpRunSeq       AS INT  NO-UNDO.
    DEFINE VARIABLE templateCnt     AS INT  NO-UNDO.
    DEFINE VARIABLE templateSaved   AS INT  NO-UNDO.
    DEFINE VARIABLE tryCnt          AS INT  NO-UNDO.
    DEFINE VARIABLE cLocation       AS CHAR NO-UNDO.
    DEFINE VARIABLE BailOut         AS LOG  NO-UNDO.
    DEFINE VARIABLE bailOut2        AS LOG  NO-UNDO.
    DEFINE VARIABLE lineCnt         AS INT  NO-UNDO.
    DEFINE VARIABLE tmpBedSeq       AS INT  NO-UNDO.
    DEFINE VARIABLE cPathWay        AS CHAR NO-UNDO.
    DEFINE VARIABLE fname           AS CHAR NO-UNDO.
    DEFINE VARIABLE startbatch      AS INT  NO-UNDO.
    DEFINE VARIABLE hasBleed        AS LOG  NO-UNDO.
    DEFINE VARIABLE rFlag           AS LOG  NO-UNDO.
    DEFINE VARIABLE qtyRan          AS INT  NO-UNDO.
    DEFINE VARIABLE qtyNeeded       AS INT  NO-UNDO.
    DEFINE VARIABLE inQueue         AS LOG  NO-UNDO.
    DEFINE VARIABLE currDir         AS CHAR NO-UNDO.

    EMPTY TEMP-TABLE ttart.
    EMPTY TEMP-TABLE ttmat.
    EMPTY TEMP-TABLE ttorder.
    EMPTY TEMP-TABLE ttDel.
    EMPTY TEMP-TABLE saves.

    RUN sethomefolder.
    /*get all files in hotfolders*/
    FOR EACH pt_hotfolder NO-LOCK:
        cpathway = cHomeFolder + "\" + entry((NUM-ENTRIES(pt_hotfolder.pathway,"\") - 1),pt_hotfolder.pathway,"\") + "\" + entry(NUM-ENTRIES(pt_hotfolder.pathway,"\"),pt_hotfolder.pathway,"\") + "\".
       
        IF NOT CAN-FIND(FIRST hFolder WHERE hFolder.path = cpathway) THEN DO:
            CREATE hFolder.
            ASSIGN hFolder.path = cpathway.
        END.
        
        FILE-INFO:FILE-NAME = cpathway.
        IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
            INPUT FROM OS-DIR(cpathway).
            IMPORT ^.
            IMPORT ^.
            REPEAT:
                IMPORT fname.
                FILE-INFO:FILE-NAME  = fname.
                CREATE idet.
                ASSIGN idet.iname    = fname
                       idet.iPathway = cpathway + fname.
            END.
        END.
    END.
        
    IF blankSlate = YES THEN DO:
        /*reset records so they delete*/
        FOR EACH sign_mm_hdr WHERE sign_mm_hdr.RUN_date = ? BY sign_mm_hdr.runseq:
            IF sign_mm_hdr.qty_printed > 0 THEN 
                ASSIGN sign_mm_hdr.SAVE_bed = YES. /*save beds in progress*/
            ELSE IF sign_mm_hdr.reprint THEN
                ASSIGN sign_mm_hdr.SAVE_bed = YES. /*save reprints*/
            ELSE 
                ASSIGN sign_mm_hdr.save_bed = NO.
                
            IF INDEX(sign_mm_hdr.matltype,"Corex") > 0 THEN sign_mm_hdr.save_bed = NO.
        END.
    END.

    /*tells it to keep full beds and only rebuild partials*/
    IF blankSlate = NO THEN DO:
        FOR EACH sign_mm_hdr WHERE sign_mm_hdr.RUN_date = ? BY sign_mm_hdr.runseq:
            IF NOT sign_mm_hdr.matltype = "template" THEN DO:
                rFlag = NO.
                FOR EACH sign_mm_det OF sign_mm_hdr NO-LOCK BREAK BY sign_mm_det.itemseq:
                    IF LAST-OF(sign_mm_det.itemseq) THEN DO:
    
                       /*evaluate everything on saved beds to make sure something strange didn't happen*/
                        FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = sign_mm_det.itemseq NO-ERROR.
                        IF AVAIL so_items THEN DO:
                            FIND so_file NO-LOCK WHERE so_file.so_no = so_items.so_no NO-ERROR.
                            /*have we ran all of them?*/
                            RUN getQty (so_items.itemseq, OUTPUT qtyRan, OUTPUT qtyNeeded, OUTPUT inQueue).
                            IF qtyNeeded < 0 THEN rFlag = YES.
    
                            /*has it been marked complete?*/
                            IF CAN-FIND(FIRST h_detail NO-LOCK WHERE h_detail.order_no = so_items.so_no AND h_detail.ITEM_no = STRING(so_items.ITEM_no) AND h_detail.activity = "D11" AND h_detail.zzLog_1 = YES) THEN DO:
                               rFlag = YES.
                            END.

                            IF so_items.ord_status <> 5 THEN rFlag = YES.

                            IF AVAIL(so_file) AND so_file.hold <> "" THEN rFlag = YES.
                        END.
                        ELSE rFlag = YES.
                    END.
                END.
            END.
            ELSE rFlag = YES.

            /*keep full beds and reprints*/
            IF (sign_mm_hdr.fullbed = YES AND rFlag = NO) OR (hasBleed AND rFlag = NO) OR sign_mm_hdr.reprint = YES THEN
                ASSIGN sign_mm_hdr.save_bed = YES.
            ELSE 
                ASSIGN sign_mm_hdr.SAVE_bed = NO.
                
            IF INDEX(sign_mm_hdr.matltype,"Corex") > 0 THEN sign_mm_hdr.save_bed = NO.
        END.

        IF AVAIL sign_mm_hdr THEN RELEASE sign_mm_hdr.
    END.

    /*******************************/
    /****Folder Maintenance*********/
    /*******************************/
    IF blankSlate = YES THEN DO:
        FILE-INFO:FILE-NAME = cHomeFolder.
        /*remove all images inside the mtl1 folders*/
        RUN removeFiles(cHomeFolder).
    END.
    
 
    /*Maintenance zz trigger records that didn't get deleted*/ 
    FOR EACH zz_file WHERE zz_file.zz_key1 = "MM-SentToRip":
        IF (TODAY - date(zz_file.zz_date[1]) > 2) OR (zz_file.zz_date[1] = ?) THEN DELETE zz_file.
    END.

     /*make sure CS6 folder is still there else make it*/
    ASSIGN FILE-INFO:FILE-NAME = cBatchImgLoc.
    IF FILE-INFO:FULL-PATHNAME = ? THEN OS-CREATE-DIR value(cBatchImgLoc).



    /*make sure the Logs folder is there*/
    ASSIGN FILE-INFO:FILE-NAME = cLogLoc.
    IF FILE-INFO:FULL-PATHNAME = ? THEN OS-CREATE-DIR VALUE(cLogLoc).



    /*every sunday housekeeping on CS6 folder*/
    IF WEEKDAY(TODAY) = 1 THEN DO:
        ASSIGN FILE-INFO:FILE-NAME = cBatchImgLoc.
        IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
            INPUT FROM OS-DIR(cBatchImgLoc).
            IMPORT ^.
            IMPORT ^.
            REPEAT:
                IMPORT fname.
                FILE-INFO:FILE-NAME = cBatchImgLoc + "\" + fname.
                IF fname = "CompletedBatches" THEN NEXT.
                IF FILE-INFO:FILE-MOD-DATE < TODAY - 1 THEN
                    OS-DELETE value(FILE-INFO:FULL-PATHNAME).
            END.
        END.
    END.




    /*clean up csv files we've recently created*/
    IF WEEKDAY(TODAY) = 1 THEN DO:
        FILE-INFO:FILE-NAME = cLogLoc.
        IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
            INPUT FROM OS-DIR(cLogLoc).
            IMPORT ^.
            IMPORT ^.
            REPEAT:
                IMPORT fname.
                IF fname BEGINS "MmOrderReport" OR fname BEGINS "MediaManagerlost" THEN DO:
                    OS-DELETE value(cLogLoc + "\" + fname).
                END.
            END.
        END.
    END.
    
    FOR EACH pt_hotfolder NO-LOCK:
        ASSIGN CurrDir = cHomeFolder + "\" + entry((NUM-ENTRIES(pt_hotfolder.pathway,"\") - 1),pt_hotfolder.pathway,"\").
        INPUT FROM OS-DIR(currDir).
        IMPORT ^. /*gets rid of "."  */
        IMPORT ^. /*gets rid of ".." */
        REPEAT:
            IMPORT fName.
          
            cpathway = currDir + "\" + fname + "\".
            IF NOT CAN-FIND(FIRST hFolder WHERE hFolder.path = cpathway) THEN DO:
                OS-DELETE VALUE(cpathway).
            END.
        END.
    END.

    /*now rebuild directories and subfolders*/
    ASSIGN FILE-INFO:FILE-NAME = cHomeFolder.
    IF FILE-INFO:FULL-PATHNAME = ? THEN OS-CREATE-DIR value(cHomeFolder).

    FOR EACH zz_file WHERE zz_file.zz_key1 = "MM-Hotfolder-Order" BY zz_file.zz_dec[1]:
        IF NOT CAN-DO(cPrintTypes,zz_file.zz_key2) THEN
            ASSIGN cPrintTypes = cPrintTypes + (IF cPrintTypes = "" THEN "" ELSE ",") + zz_file.zz_key2.
    END.
    
    FOR EACH pt_hotfolder NO-LOCK:
        tmpcmd = cHomeFolder + "\" + entry((NUM-ENTRIES(pt_hotfolder.pathway,"\") - 1),pt_hotfolder.pathway,"\") + "\" + entry(NUM-ENTRIES(pt_hotfolder.pathway,"\"),pt_hotfolder.pathway,"\").
        ASSIGN FILE-INFO:FILE-NAME = tmpcmd.
        tmpcmd = "mkdir" + " " + chr(34) + tmpcmd + CHR(34).
        IF FILE-INFO:FULL-PATHNAME = ? THEN OS-COMMAND SILENT VALUE(tmpcmd).
    END.
    /*******************************/
    /****End Folder Maintenance*****/
    /*******************************/

    /*Delete batches we are not keeping*/
    cnt = 0.
    FOR EACH sign_mm_hdr WHERE sign_mM_hdr.RUN_date = ?:
        ASSIGN Delme  = TRUE
               Delme2 = TRUE .
               
        IF sign_mm_hdr.SAVE_bed = YES THEN delme = FALSE. 
        IF CAN-FIND (FIRST sign_mm_det WHERE sign_mm_det.batchseq = sign_mm_hdr.batchseq AND sign_mm_det.itemseq = 0) THEN delme = TRUE.
        IF delme = FALSE THEN NEXT.
        
        FOR EACH sign_mm_det OF sign_mm_hdr:
            DELETE sign_mm_det.
        END.
        
        RUN RecordHdrDelete(sign_mm_hdr.batchseq, "resets").
        DELETE sign_mm_hdr.
    END.

    EMPTY TEMP-TABLE ttdel.
END PROCEDURE.


PROCEDURE RptDet:
    DEFINE INPUT PARAMETER cItemseq AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER cSo      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cItem    AS INT  NO-UNDO.
    /*create a report record in case there are issues. These records are what show on the spreadsheet*/
    CREATE rpt_det.
    ASSIGN rpt_det.itemseq = cItemseq
           rpt_det.so_no   = cSo
           rpt_det.ITEM_no = cItem.

END PROCEDURE.


PROCEDURE RuleOfOne:
    DEFINE INPUT PARAMETER bednum  AS INT NO-UNDO.
    DEFINE INPUT PARAMETER batchNo AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER c_ok   AS LOG NO-UNDO.

    FIND b_signbed NO-LOCK WHERE b_signbed.seq = bednum NO-ERROR.
    IF AVAIL b_signbed THEN DO:
        FIND LAST b_mm_det NO-LOCK WHERE b_mm_det.batchseq = batchno NO-ERROR.
        IF (b_signbed.userdec1 * b_signbed.userdec2) - 1 = b_mm_det.POSITION THEN
            ASSIGN c_ok = YES.
    END.
END PROCEDURE.


PROCEDURE SaveDown:
    DEFINE INPUT PARAMETER isTTart   AS LOGICAL    NO-UNDO.
    DEFINE INPUT PARAMETER batchno   AS INT        NO-UNDO.
    DEFINE INPUT PARAMETER cFile     AS CHAR       NO-UNDO.
    DEFINE INPUT PARAMETER Material  AS CHAR       NO-UNDO.
    DEFINE INPUT PARAMETER imgSize   AS CHAR       NO-UNDO.
    DEFINE INPUT PARAMETER doReSize  AS LOG        NO-UNDO.
    DEFINE INPUT PARAMETER NestRecs  AS LOG        NO-UNDO.
    
    DEFINE VARIABLE cType            AS CHAR       NO-UNDO.
    DEFINE VARIABLE xmlData          AS CHAR       NO-UNDO.
    DEFINE VARIABLE cResponse        AS CHAR       NO-UNDO.
    DEFINE VARIABLE chDart           AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE pLoop            AS INT        NO-UNDO.
    DEFINE VARIABLE cArtfile         AS CHAR       NO-UNDO.
    DEFINE VARIABLE val2             AS CHAR       NO-UNDO.
    DEFINE VARIABLE cSo              AS CHAR       NO-UNDO.
    DEFINE VARIABLE cItem            AS INT        NO-UNDO.
    DEFINE VARIABLE cItemseq         AS INT        NO-UNDO.
    DEFINE VARIABLE actwidth         AS DEC        NO-UNDO.
    DEFINE VARIABLE actheight        AS DEC        NO-UNDO.
    DEFINE VARIABLE newName          AS CHAR       NO-UNDO.

    
    IF NOT isTTart THEN DO:
        FOR EACH b_mm_det /* NO-LOCK */ WHERE b_mm_det.batchseq = batchno:
            ASSIGN cArtfile = b_mm_det.artfile.
            RUN saveDown2(1,b_mm_det.itemseq,b_mm_det.part_no,Material,imgSize,doReSize,batchno,INPUT-OUTPUT cArtfile).
            IF AVAIL b_mm_det THEN ASSIGN b_mm_det.artfile = cArtfile.
        END.
        IF NestRecs THEN DO:
            FOR EACH nest_mm_det /* NO-LOCK */ WHERE nest_mm_det.batchseq = batchno:
                ASSIGN cArtfile = nest_mm_det.artfile.
                RUN saveDown2(1,nest_mm_det.itemseq,nest_mm_det.part_no,Material,imgSize,doReSize,batchno,INPUT-OUTPUT cArtfile).
                IF AVAIL nest_mm_det THEN ASSIGN nest_mm_det.artfile = cArtfile.
            END.
        END.
    END.
    ELSE DO:
        FOR EACH ttart WHERE ttArt.ttItemseq = batchno AND ttArt.ttFile = cFile:
            RUN saveDown2(10,ttArt.ttItemseq,ttArt.ttPart,ttArt.ttType,imgSize,doReSize,batchno,INPUT-OUTPUT ttArt.ttFile).
        END.
    END.
END PROCEDURE.


PROCEDURE SaveDown2:
    DEFINE INPUT PARAMETER pLogNum   AS INTEGER    NO-UNDO.
    DEFINE INPUT PARAMETER pItemseq  AS INTEGER    NO-UNDO.
    DEFINE INPUT PARAMETER pPartNo   AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER pMaterial AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER pImgSize  AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER pDoReSize  AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER pBatchNo  AS INTEGER    NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pArtfile AS CHAR NO-UNDO.

    DEFINE VARIABLE cType            AS CHAR       NO-UNDO.
    DEFINE VARIABLE xmlData          AS CHAR       NO-UNDO.
    DEFINE VARIABLE cResponse        AS CHAR       NO-UNDO.
    DEFINE VARIABLE chDart           AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE pLoop            AS INT        NO-UNDO.
    DEFINE VARIABLE cArtfile         AS CHAR       NO-UNDO.
    DEFINE VARIABLE val2             AS CHAR       NO-UNDO.
    DEFINE VARIABLE cSo              AS CHAR       NO-UNDO.
    DEFINE VARIABLE cItem            AS INT        NO-UNDO.
    DEFINE VARIABLE cItemseq         AS INT        NO-UNDO.
    DEFINE VARIABLE actwidth         AS DEC        NO-UNDO.
    DEFINE VARIABLE actheight        AS DEC        NO-UNDO.
    DEFINE VARIABLE newName          AS CHAR       NO-UNDO.
    DEFINE VARIABLE xmlfile          AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE nodeValue        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE isFoldOver       AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE tImages          AS CHAR       NO-UNDO.
    DEFINE VARIABLE tDelStatus       AS CHAR       NO-UNDO.
    DEFINE VARIABLE tSO              AS CHAR       NO-UNDO.
    
    DEFINE VARIABLE art AS ArtGenerator.
    art = NEW ArtGenerator().

    DEFINE VARIABLE inVars AS JsonObject.
    inVars = NEW JsonObject().

    DEFINE VARIABLE outVars AS JsonObject.
    
    RUN setHomeFolder.
     
    IF      INDEX(pMaterial,"Steel") > 0     THEN cType = "Steel".
    ELSE IF INDEX(pMaterial,"Poly")  > 0     THEN cType = "Poly" .
    ELSE IF INDEX(pMaterial,"Corex") > 0     THEN cType = "Corex".
    ELSE IF INDEX(pMaterial,"Magnetic")  > 0 THEN cType = "Magnetic".
    ELSE IF INDEX(pMaterial,"omegabond") > 0 THEN ctype = "Omegabond".
    ELSE IF INDEX(pMaterial,"Alumalite") > 0 THEN cType = "Alumalite".

       
    FIND pt_det NO-LOCK WHERE pt_det.part_no = pPartNo NO-ERROR.
    FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = pItemseq AND squ_ptdet.TYPE <> "frame" NO-ERROR.
    IF AVAIL squ_ptdet THEN DO:
        actwidth   = IF squ_ptdet.pressPrintingWidth  <> 0 THEN squ_ptdet.pressPrintingWidth ELSE 0.
        actheight  = IF squ_ptdet.pressPrintingHeight <> 0 THEN squ_ptdet.pressPrintingHeight ELSE 0.
        isFoldOver = squ_ptdet.FoldOver.
    END.
    IF AVAIL pt_det AND (actwidth = 0 OR actheight = 0) THEN DO:
        actwidth   = IF pt_det.pressPrintingWidth  <> 0  THEN pt_det.pressPrintingWidth ELSE 0.
        actheight  = IF pt_det.pressPrintingHeight <> 0  THEN pt_det.pressPrintingHeight ELSE 0.
        isFoldOver = pt_det.FoldOver.   
    END.
    
    /*IF lDynNest THEN DO:*/
    /*patch for dynamic nest passing sizes*/
    FIND FIRST buf_mm_det NO-LOCK WHERE buf_mm_det.itemseq = pItemseq NO-ERROR.
    IF AVAILABLE buf_mm_det THEN DO:
            FIND buf_mm_hdr NO-LOCK WHERE buf_mm_hdr.batchseq = buf_mm_det.batchseq NO-ERROR.
            IF AVAILABLE buf_mm_hdr AND buf_mm_hdr.bedseq = 0 THEN DO:
                pImgSize = STRING(actheight) + "x" + STRING(actwidth).
            END.
    END.
    IF AVAILABLE buf_mm_hdr THEN RELEASE buf_mm_hdr.
    IF AVAILABLE buf_mm_hdr THEN RELEASE buf_mm_det.
    /*END.*/
    
    FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = pItemseq NO-ERROR. /*ryanle*/
    FIND FIRST so_file NO-LOCK WHERE so_file.so_no = so_items.so_no NO-ERROR. 
    IF AVAIL pt_det AND pt_det.FoldOver AND pArtfile MATCHES "*DartProduction*" THEN DO: /*if its a tent/foldover and didn't come from art dept*/

        IF pImgSize = "24x38"    THEN pImgSize = "18x24".
        IF pImgSize = "24x48.32" THEN pImgSize = "24x24".
        IF pImgSize = "48.25x30" THEN pImgSize = "24x30".
        IF pImgSize = "60x24"    THEN pImgSize = "30x24".

               
          ASSIGN cArtFile = cBatchImgLoc + "\" + so_items.so_no + "-" + STRING(so_items.item_no) + "-" + STRING(MTIME) + "_CS6.pdf".
               
            xmlData = "".
            xmldata = "<front>" + (IF NUM-ENTRIES(pArtfile,",") > 1 THEN ENTRY(1,pArtfile,",") ELSE pArtfile) + "</front>"
                      + "<back>" + (IF num-entries(pArtfile,",") > 1 THEN entry(2,pArtfile,",") ELSE IF ((SEARCH(REPLACE(pArtfile,".pdf","-LEFT.pdf")) <> ? AND num-entries(pArtfile,",") < 1)) THEN REPLACE(pArtfile,".pdf","-LEFT.pdf") ELSE pArtfile) + "</back>"
                      + "<outputfile>" + cArtFile + "</outputfile>".
            RUN xmltag ("material",    cType,                                                    INPUT-OUTPUT xmldata).
            RUN xmltag ("size",        pImgSize,                                                 INPUT-OUTPUT xmldata).
            RUN xmltag ("resize",      pDoReSize,                                                INPUT-OUTPUT xmldata).
            RUN xmltag ("ActualHeight" ,actheight,                                               INPUT-OUTPUT xmldata).
            RUN xmltag ("ActualWidth"  , actwidth ,                                              INPUT-OUTPUT xmldata).
            
            
            RUN xmltag ("order",       IF AVAIL so_items THEN STRING(so_items.so_no)   ELSE "",  INPUT-OUTPUT xmldata). /*added so_items.so_no, was blank   - Ryanle*/
            FIND FIRST so_art NO-LOCK WHERE so_art.itemseq = pItemSeq AND so_art.artfile = (IF NUM-ENTRIES(pArtfile,",") > 1 THEN ENTRY(1,pArtfile,",") ELSE pArtfile) NO-ERROR. 
            IF AVAIL so_art THEN DO:
                RUN xmltag ("item" , IF AVAIL so_items THEN (STRING(so_items.item_no) + "-" + string(so_art.disp_order)) ELSE "", INPUT-OUTPUT xmldata). /*ryanle*/       
            END.
            ELSE RUN xmltag ("item" , IF AVAIL so_items THEN STRING(so_items.item_no) ELSE "", INPUT-OUTPUT xmldata). /*ryanle*/ 
            
            RUN xmltag ("PartNumber",  IF AVAIL pt_det   THEN pt_det.part_no           ELSE "",  INPUT-OUTPUT xmldata).
            RUN xmltag ("FoldOver"     ,STRING(isFoldOver),                                      INPUT-OUTPUT xmldata).
    
            xmldata = "<Host>ART-JM1</Host><XMLData><Program>SART</Program><XML><myrtle>" + xmldata + "</myrtle></XML></XMLData>".
                        
            OUTPUT TO VALUE("\\fs02\bullseye\JM\LIVE\XML\Fred-Myrtle\" + STRING(MTIME) + "_" + STRING(pItemseq) + "-FM.xml").
            PUT UNFORMATTED xmlData SKIP.
            OUTPUT CLOSE.
            
            /*if TS item, then run through PDFTB*/
            IF so_file.cust_no = "3198298" THEN DO:
                IF inVars:HAS("dateCode") THEN inVars:REMOVE("dateCode").
                inVars:ADD("dateCode",so_items.so_no + "-" + string(so_items.item_no)).
                outVars = art:PreflightVars(cArtfile,REPLACE(cArtfile,".pdf","_TS.pdf"),"Tradesource Template Watermark.kfpx",inVars).                     
            END.
            
        /*END.*/
        ASSIGN pArtfile = cArtfile.
    END.
    ELSE DO:
        IF NUM-ENTRIES(pArtfile,",") > 1 THEN DO:
            DO pLoop = 1 TO NUM-ENTRIES(pArtfile,","):
                ASSIGN cArtfile = ""
                       newName  = ""
                       cArtfile = ENTRY(pLoop,pArtfile).
                       
                 
                
                    cArtfile = cBatchImgLoc + "\" + so_items.so_no + "-" + string(so_items.ITEM_no) + "-" + STRING(MTIME) + (IF pLoop = 2 THEN "-LEFT.pdf" ELSE ".pdf").
                    
                ASSIGN cArtfile = REPLACE(cArtfile,".pdf","_CS6.pdf").
                
                /*IF SEARCH(cArtFile) = ? THEN DO:*/
                    xmlData = "".
                    xmldata = "<infile>" + entry(pLoop,pArtfile,",") + "</infile><outputfile>"
                              + cArtFile + "</outputfile>".
                    RUN xmltag ("material",     cType,      INPUT-OUTPUT xmldata).
                    RUN xmltag ("size",         pImgSize,   INPUT-OUTPUT xmldata).
                    RUN xmltag ("resize",       pDoReSize,  INPUT-OUTPUT xmldata).
                    RUN xmltag ("ActualHeight", actheight,  INPUT-OUTPUT xmldata).
                    RUN xmltag ("ActualWidth",  actwidth ,  INPUT-OUTPUT xmldata).
                                          
                    /*ryanle - check for signoff art*/
                    RUN xmltag ("order",IF AVAIL so_items THEN STRING(so_items.so_no) ELSE "", INPUT-OUTPUT xmldata).
                        
                    FIND FIRST so_art NO-LOCK WHERE so_art.itemseq = pItemSeq AND so_art.artfile = entry(pLoop,pArtfile,",") NO-ERROR. 
                    IF AVAIL so_art THEN DO:
                        RUN xmltag ("item" , IF AVAIL so_items THEN (STRING(so_items.item_no) + "-" + string(so_art.disp_order)) ELSE "", INPUT-OUTPUT xmldata). /*ryanle*/       
                    END.
                    ELSE RUN xmltag ("item" , IF AVAIL so_items THEN STRING(so_items.item_no) ELSE "", INPUT-OUTPUT xmldata). /*ryanle*/

                    xmldata = "<Host>ART-JM1</Host><XMLData><Program>SART</Program><XML><fred>" + xmldata + "</fred></XML></XMLData>".
                    RELEASE so_art.

                    OUTPUT TO VALUE("\\fs02\bullseye\JM\LIVE\XML\Fred-Myrtle\" + STRING(MTIME) + "_" + STRING(pItemseq) + "-FM.xml").
                    PUT UNFORMATTED xmlData SKIP.
                    OUTPUT CLOSE.
                                       
                /*END.*/

                ASSIGN pArtfile = REPLACE(pArtfile,ENTRY(pLoop,pArtfile),cArtFile).

            END.
        END.
        ELSE DO: /*FRED*/
            ASSIGN cArtfile = ""
                   newName  = ""
                   cArtfile = pArtfile
                   cArtfile = cBatchImgLoc + "\" + so_items.so_no + "-" + string(so_items.ITEM_no) + "-" + STRING(MTIME) + (IF pArtfile = pt_det.prodfile2 THEN "-Left.pdf" ELSE ".pdf")
                   cArtfile = REPLACE(cArtFile,".pdf","_CS6.pdf").
    
            /*IF SEARCH(cArtfile) = ? THEN DO:*/
                xmlData = "".
                xmldata = "<infile>" + pArtfile + "</infile><outputfile>"
                          + cArtFile + "</outputfile>".
                RUN xmltag ("material",    cType,     INPUT-OUTPUT xmldata).
                RUN xmltag ("size",        pImgSize,  INPUT-OUTPUT xmldata).
                RUN xmltag ("ActualHeight",actheight, INPUT-OUTPUT xmldata).
                RUN xmltag ("ActualWidth", actwidth , INPUT-OUTPUT xmldata).              
                
                /*ryanle - check for signoff art*/
                RUN xmltag ("order",IF AVAIL so_items THEN STRING(so_items.so_no) ELSE "", INPUT-OUTPUT xmldata).
                        
                FIND FIRST so_art NO-LOCK WHERE so_art.itemseq = pItemSeq AND so_art.artfile = pArtfile NO-ERROR. 
                IF AVAIL so_art THEN DO:
                    RUN xmltag ("item" , IF AVAIL so_items THEN (STRING(so_items.item_no) + "-" + string(so_art.disp_order)) ELSE "", INPUT-OUTPUT xmldata). /*ryanle*/       
                END.
                ELSE RUN xmltag ("item" , IF AVAIL so_items THEN STRING(so_items.item_no) ELSE "", INPUT-OUTPUT xmldata). /*ryanle*/

                RELEASE so_art NO-ERROR.                              
                
                
                xmldata = "<Host>ART-JM1</Host><XMLData><Program>SART</Program><XML><fred>" + xmldata + "</fred></XML></XMLData>".
    
                OUTPUT TO VALUE("\\fs02\bullseye\JM\LIVE\XML\Fred-Myrtle\" + STRING(MTIME) + "_" + STRING(pItemseq) + "-FM.xml").
                PUT UNFORMATTED xmlData SKIP.
                OUTPUT CLOSE.               

                ASSIGN pArtfile = cArtfile.
        END.
        
    END.
END PROCEDURE.


PROCEDURE SetHomeFolder:

    RUN getDBase.
    IF cDBase <> "Live" THEN DO:
          ASSIGN cHomeFolder  = imageShare + "AgentPhotos\temporary\mtl1-Test"
                 cRanFolder   = imageShare + "AgentPhotos\temporary\CS6-Test\CompletedBatches"
                 cBatchImgLoc = imageShare + "AgentPhotos\temporary\CS6-Test".
    END.
                                           
END PROCEDURE.


PROCEDURE StripOutValue:
    DEFINE INPUT  PARAMETER pData  AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER pNode  AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER pValue AS CHAR NO-UNDO.
    DEFINE VARIABLE hstart AS INT NO-UNDO.
    DEFINE VARIABLE hend   AS INT NO-UNDO.
    
    ASSIGN hstart = R-INDEX(pData,pNode)
           hend   = R-INDEX(pData,REPLACE(pNode,"doc","/doc"))
           pValue = SUBSTRING(pData,hstart + LENGTH(pNode),hend - hstart - LENGTH(pNode)) .
END PROCEDURE.    


PROCEDURE TemplateNest:
    DEFINE INPUT PARAMETER cItemseq AS INT NO-UNDO.
    
    FIND so_items NO-LOCK WHERE so_items.itemseq = cItemseq NO-ERROR.
    IF AVAILABLE so_items THEN DO:
        RUN buildtt      (so_items.so_no,so_items.ITEM_no).
        RUN checkImages  (so_items.itemseq).
    
        RUN grouping     ("").
        RUN regroup      ("").
        RUN DynamicNest ("12345").
        
        RUN trimdoups.
        RUN releaseAll.
    END.    
END.


PROCEDURE TrimBeds:
    DEFINE INPUT PARAMETER deleteNo AS CHAR NO-UNDO.
    DEFINE VARIABLE c_ok            AS LOG  NO-UNDO.

    IF deleteNo = "" THEN DO:
        DO WHILE CAN-FIND(FIRST sign_mm_hdr WHERE sign_mm_hdr.qty_printed = 0 AND sign_mm_hdr.fullbed = FALSE):
            EMPTY TEMP-TABLE ttDel.
            FOR EACH sign_mm_hdr WHERE sign_mm_hdr.qty_printed = 0 AND sign_mm_hdr.fullbed = FALSE:
                RUN RuleOfOne(sign_mm_hdr.bedseq, sign_mm_hdr.batchseq, OUTPUT c_ok).
                IF c_ok = YES THEN DO:
                    ASSIGN sign_mm_hdr.fullbed = TRUE.
                    NEXT.
                END.
                FIND FIRST sign_mm_det NO-LOCK OF sign_mm_hdr NO-ERROR.
                IF AVAIL sign_mm_det THEN DO:
                    EMPTY TEMP-TABLE tBatches.
                    RUN signbatchremove.p("HDR",sign_mm_det.itemseq,sign_mm_det.artlinkseq,OUTPUT TABLE tBatches). 
            
                    FOR EACH tBatches:
                        RUN deleteMMhdr.p(tBatches.batchseq,"mm.p","trimbeds",cDBase).
                    END.
                END.
            END.
            RUN regroup ("").
        END.
    END.
    ELSE IF CAN-FIND(FIRST sign_mm_hdr WHERE sign_mm_hdr.batchseq = INT(deleteNo)) THEN DO:
        FOR EACH sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.batchseq = INT(deleteNo):
            FIND FIRST sign_mm_det NO-LOCK OF sign_mm_hdr NO-ERROR.
            IF AVAIL sign_mm_det THEN DO:
                EMPTY TEMP-TABLE tBatches.
                RUN signbatchremove.p("HDR",sign_mm_det.itemseq,sign_mm_det.artlinkseq,OUTPUT TABLE tBatches). 
                FOR EACH tBatches:
                    RUN deleteMMhdr.p(tBatches.batchseq,"mm.p","trimbeds",cDBase).
                END.
            END.
        END.
    END.
    ELSE DO:
        FOR EACH ttart WHERE ttart.ttItemSeq = int(deleteNo):
            DELETE ttart.
        END.
    END.
END PROCEDURE.


PROCEDURE TrimDoups:
    DEFINE VARIABLE isCopy     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE numDet     AS INTEGER NO-UNDO.
    DEFINE VARIABLE failed     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE nHash      AS INTEGER NO-UNDO.
    DEFINE VARIABLE tmpString  AS CHAR    NO-UNDO.
    DEFINE VARIABLE iLoop      AS INTEGER NO-UNDO.
    DEFINE VARIABLE itemCnt    AS INTEGER NO-UNDO.
    DEFINE VARIABLE doupSeq    AS INTEGER NO-UNDO.
    DEFINE VARIABLE artString  AS CHAR    NO-UNDO.
    DEFINE VARIABLE md5        AS CHAR    NO-UNDO.
    DEFINE VARIABLE multiplier AS INT     NO-UNDO.
    DEFINE VARIABLE badbeds    AS CHAR    NO-UNDO.

    /*only look at records that were created throught JM batching not prepcenter*/
    FOR EACH sign_mm_hdr WHERE  sign_mm_hdr.RUN_date = ? 
                           AND (sign_mm_hdr.fullbed = TRUE OR sign_mm_hdr.bedseq = 0):
                        /* AND (IF lDynNest = FALSE THEN sign_mm_hdr.bedseq <> 0 ELSE TRUE):*/
        IF sign_mm_hdr.zzchar_1 = "PrimeCenter" THEN NEXT.
        tmpString = "".
        FOR EACH sign_mm_det NO-LOCK OF sign_mm_hdr:
            artString = "".
            IF CAN-FIND(FIRST so_art NO-LOCK WHERE so_art.itemseq = sign_mm_det.itemseq) THEN DO:
                DO iLoop = 1 TO NUM-ENTRIES(sign_mm_det.artfile,","):
                    FIND so_art NO-LOCK WHERE so_art.itemseq = sign_mm_det.itemseq AND so_art.artfile = entry(iLoop,sign_mm_det.artfile) NO-ERROR.
                    IF AVAIL so_art THEN ASSIGN artString = artString + string(so_art.artseq).
                END.
            END.
/*             FIND so_art NO-LOCK WHERE so_art.itemseq = sign_mm_det.itemseq AND so_art.artfile = sign_mm_det.artfile NO-ERROR.                                              */
/*             ASSIGN tmpString = tmpstring + string(sign_mm_det.itemseq) +  sign_mm_det.part_no +  sign_mm_det.artfile + IF AVAIL so_art THEN STRING(so_art.artseq) ELSE "". */
            tmpString = tmpstring + string(sign_mm_det.itemseq) +  sign_mm_det.part_no +  sign_mm_det.artfile + artString.
        END.
/*         sign_mm_hdr.cloneValue = ENCODE(tmpString). */
        RUN md5.p (tmpString,OUTPUT md5).
        sign_mm_hdr.cloneValue = md5.
    END.

    FOR EACH sign_mm_hdr WHERE  sign_mm_hdr.RUN_date = ? 
                           AND (sign_mm_hdr.fullbed = TRUE OR sign_mm_hdr.bedseq = 0) 
                           /*AND (IF lDynNest = FALSE THEN sign_mm_hdr.bedseq <> 0 ELSE TRUE)*/:
        IF sign_mm_hdr.zzchar_1 = "PrimeCenter" THEN NEXT.
        IF sign_mm_hdr.cloneValue = "" THEN NEXT.
        FOR EACH b_mm_hdr WHERE  b_mm_hdr.RUN_date = ? 
                            AND (b_mm_hdr.fullbed = TRUE OR b_mm_hdr.bedseq = 0) 
                            /*AND (IF lDynNest = FALSE THEN b_mm_hdr.bedseq <> 0 ELSE TRUE)*/
                            AND b_mm_hdr.bedseq = sign_mm_hdr.bedseq:
            IF b_mm_hdr.cloneValue = ""                   THEN NEXT.
            IF b_mm_hdr.batchseq   = sign_mm_hdr.batchseq THEN NEXT.

            IF sign_mm_hdr.cloneValue = b_mm_hdr.cloneValue THEN DO:
                ASSIGN sign_mm_hdr.Qty = sign_mm_hdr.Qty + 1.
                FOR EACH b_mm_det OF b_mm_hdr:
                    DELETE b_mm_det.
                END.
                DELETE b_mm_hdr.
            END.
        END.
        IF sign_mm_hdr.qty > 1 THEN DO: /*check to see if the doup count is correct*/
            itemCnt = 0. doupSeq = 0.
            FOR EACH sign_mm_det OF sign_mm_hdr:
                itemCnt = itemCnt + 1.
                doupSeq = sign_mm_det.itemseq.
            END.
            FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = doupSeq NO-ERROR.
            IF AVAIL so_items THEN DO:
                FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = so_items.itemseq AND squ_ptdet.TYPE <> "Frame" NO-ERROR.
                IF AVAIL squ_ptdet THEN
                    multiplier = IF squ_ptdet.FoldOver OR squ_ptdet.jackunit OR squ_ptdet.steeltent THEN 2 ELSE 1.
                ELSE 
                    multiplier = 1.
                    
                IF so_item.orderqty * multiplier < sign_mm_hdr.qty * itemCnt THEN DO:
                    IF NOT CAN-DO(badbeds,STRING(sign_mm_hdr.batchseq)) THEN DO:
                        ASSIGN badbeds = badbeds + (IF badbeds = "" THEN "" ELSE ",") + string(sign_mm_hdr.batchseq).
                    END.
                    
                END.
            END.
            IF AVAIL so_items THEN RELEASE so_items.
            IF AVAIL squ_ptdet THEN RELEASE squ_ptdet.
        END.

    END.
    IF badbeds <> "" THEN DO:
        ASSIGN c_subject     = "Verify Job Manager Batch 'Copies' Qty"
               c_to_addr     = cProgrammerList 
               c_msg         = "Copies Qty on following batch(es) may be incorrect. Please verify!" + CHR(10) + badbeds.

        RUN mgemail.p ("Bullseye Database",c_to_addr,"","",c_subject,c_msg,"",FALSE).
    END.

    IF AVAIL sign_mm_hdr THEN RELEASE sign_mm_hdr.
    IF AVAIL sign_mm_det THEN RELEASE sign_mm_det.
    IF AVAIL b_mm_hdr    THEN RELEASE b_mm_hdr.
    IF AVAIL b_mm_det    THEN RELEASE b_mm_det.

END PROCEDURE.


PROCEDURE WipeSlate:
    DEFINE INPUT PARAMETER deleteNo AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER outfile  AS CHAR NO-UNDO.
    DEFINE VARIABLE fName           AS CHAR NO-UNDO.

    RUN trimBeds(deleteNo).

    INPUT FROM OS-DIR(outfile).
    IMPORT ^. /*gets rid of "."  */
    IMPORT ^. /*gets rid of ".." */
    REPEAT:
        IMPORT fName.
        IF fName MATCHES "*batch" + deleteNo + "*" THEN DO:
            OS-DELETE value(outfile + "\" + fname).
        END.
    END.
END PROCEDURE.


PROCEDURE ZundRotate:
    DEFINE INPUT PARAMETER cInputFile AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cRotate    AS CHAR NO-UNDO.
    DEFINE VARIABLE cResponse AS CHAR NO-UNDO.
    DEFINE VARIABLE xmldata   AS CHAR NO-UNDO.

    xmldata = "".
    RUN xmltag ("zundfile",cInputFile,INPUT-OUTPUT xmldata).
    RUN xmltag ("rdegree" ,cRotate   ,INPUT-OUTPUT xmldata).

    xmldata = "<Host>ART-JM2</Host>" + "<XMLData>" + "<Program>" + "Sart" + "</Program>" + "<XML><zundrotate>" + xmldata + "</zundrotate></XML></XMLData>".

    RUN clientapp(xmlData,OUTPUT cResponse).
    IF INDEX(cResponse,"Success") = 0 THEN DO: 
        ASSIGN c_to_addr = cProgrammerList.
               c_subject = "Zund Rotate Failed"
               c_msg     = cResponse + CHR(10) + CHR(10) + "File= " + cInputFile.
               
        RUN mgemail.p ("Bullseye Database",c_to_addr,"","",c_subject,c_msg,"",FALSE).
        RUN ReportIssues("","MM-XML Not Successful","","","","",(IF AVAIL sign_mm_hdr THEN STRING(sign_mm_hdr.batchseq) ELSE ""),"",xmldata,cResponse).
    END.
END PROCEDURE.


PROCEDURE ZZ_Control:
    DEFINE INPUT PARAMETER delme AS LOG  NO-UNDO.
    DEFINE INPUT PARAMETER which AS CHAR NO-UNDO.
    DEFINE VARIABLE cmdline   AS CHAR NO-UNDO.
    DEFINE VARIABLE pList     AS CHAR NO-UNDO.
    DEFINE VARIABLE isPrimary AS LOG NO-UNDO.
    
    DEFINE VARIABLE iProcessHandle AS INT NO-UNDO. /* Current Proccess ID */
    

    RUN progList.p(OUTPUT pList).
    /*does this come from bgmm or mm-look?*/
    isPrimary = IF INDEX(pList,"mm-look") = 0 THEN YES ELSE NO.

    IF NOT delMe THEN DO: /*creating*/
        CASE which:
            WHEN "MM-Running" THEN DO:
                FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "MediaManager" AND zz_file.zz_key2 = "MM-Running" NO-ERROR.
                IF NOT AVAIL zz_file THEN DO:
                    CREATE zz_file.
                    ASSIGN zz_file.zz_key1    = "MediaManager"
                           zz_file.zz_key2    = "MM-Running"
                           zz_file.zz_char[1] = STRING(TIME)
                           zz_file.zz_log[1]  = isPrimary.
                END.
            END.
            WHEN "MM-Reprint" THEN DO:
                FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "MediaManager" AND zz_file.zz_key2 = "MM-Reprint" NO-ERROR.
                IF NOT AVAIL zz_file THEN DO:
                    CREATE zz_file.
                    ASSIGN zz_file.zz_key1    = "MediaManager"
                           zz_file.zz_key2    = "MM-Reprint"
                           zz_file.zz_char[1] = STRING(TIME)
                           zz_file.zz_log[1]  = isPrimary.
                END.
            END.
            WHEN "PID" THEN DO:
                iProcessHandle = 0.
                RUN GetCurrentProcessId (OUTPUT iProcessHandle).
                
                /*kill any open bgmm.p processes that are not this one*/
                FOR EACH mm_file WHERE mm_file.zz_key1 = "MediaManager" AND mm_file.zz_key2 = "PID":
                    IF STRING(iProcessHandle) <> mm_file.zz_char[1] THEN DO:
                        cmdline = "taskkill /t /f /pid " + mm_file.zz_char[1].
                        OS-COMMAND SILENT VALUE(cmdline).
                    END.
                    DELETE mm_file.
                END.
                
                IF iProcessHandle <> 0 THEN DO:
                    CREATE mm_file.
                    ASSIGN mm_file.zz_key1    = "MediaManager"
                           mm_file.zz_key2    = "PID"
                           mm_file.zz_char[1] = STRING(iProcessHandle)
                           mm_file.zz_char[2] = STRING(TIME).
                END.
            END.
            WHEN "JM-ReprintLock" THEN DO:
                FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "MediaManager" AND zz_file.zz_key2 = "JM-ReprintLock" NO-ERROR.
                IF NOT AVAILABLE zz_file THEN DO:
                    CREATE zz_file.
                    ASSIGN zz_file.zz_key1 = "MediaManager"
                           zz_file.zz_key2 = "JM-ReprintLock". 
                END.   
            END.
        END CASE.
        IF AVAILABLE zz_file THEN RELEASE zz_file.
        IF AVAILABLE mm_file THEN RELEASE mm_file.
    END.
    ELSE DO: /*deleting*/
        CASE which:
            WHEN "MM-Running" THEN DO:
                FOR EACH zz_file WHERE zz_file.zz_key1 = "MediaManager" AND zz_file.zz_key2 = "MM-Running":
                    DELETE zz_file.
                END.
            END.
            WHEN "MM-Reprint" THEN DO:
                FOR EACH zz_file WHERE zz_file.zz_key1 = "MediaManager" AND zz_file.zz_key2 = "MM-Reprint":
                    DELETE zz_File.
                END.
            END.
            WHEN "PID" THEN DO:
                iProcessHandle = 0.
                RUN GetCurrentProcessId (OUTPUT iProcessHandle).
                
                /*kill any open bgmm.p processes that are not this one*/
                FOR EACH mm_file WHERE mm_file.zz_key1 = "MediaManager" AND mm_file.zz_key2 = "PID":
                    IF STRING(iProcessHandle) <> mm_file.zz_char[1] THEN DO:
                        cmdline = "taskkill /t /f /pid " + mm_file.zz_char[1].
                        OS-COMMAND SILENT VALUE(cmdline).
                    END.
                    DELETE mm_file.
                END.
            END.
            WHEN "JM-ReprintLock" THEN DO:
                FIND FIRST zz_file WHERE zz_file.zz_key1 = "MediaManager" AND zz_file.zz_key2 = "JM-ReprintLock" NO-ERROR.
                IF AVAILABLE zz_file THEN DELETE zz_file.                   
            END.
        END CASE.
    END.
    IF AVAILABLE zz_file THEN RELEASE zz_file.
    IF AVAILABLE mm_file THEN RELEASE mm_file.
END PROCEDURE.


PROCEDURE ZZ_MessageCreate:
    DEFINE INPUT PARAMETER pType      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pUser      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pMessage   AS CHARACTER NO-UNDO.
    
    CREATE zz_msg.
    ASSIGN zz_msg.zz_key1    = "MediaManagerMessage"
           zz_msg.zz_key2    = STRING(RECID(zz_msg))
           zz_msg.zz_key3    = pUser
           zz_msg.zz_char[1] = pType
           zz_msg.zz_char[2] = pMessage
           zz_msg.zz_char[3] = STRING(DATE(TODAY))
           zz_msg.zz_char[4] = STRING(TIME,"HH:MM:SS")
           zz_msg.zz_char[5] = STRING(DATETIME(DATE(TODAY),MTIME))
           zz_msg.zz_log[1]  = FALSE.
    RELEASE zz_msg.
END PROCEDURE.


PROCEDURE ZZ_MessageDelete:
    DEFINE INPUT PARAMETER msgRecid AS CHARACTER NO-UNDO.
    
    FIND buf_zz_msg WHERE RECID(buf_zz_msg) = INTEGER(msgRecid) NO-ERROR.
    IF AVAILABLE buf_zz_msg THEN DO:
        DELETE buf_zz_msg.
    END.
END PROCEDURE.


PROCEDURE ZZ_MessageDeleteAll:
    FOR EACH zz_msg NO-LOCK WHERE zz_msg.zz_key1 = "MediaManagerMessage":
        RUN zz_messageDelete (zz_msg.zz_key2).
    END.
END PROCEDURE.


PROCEDURE ZZ_MessageDisplay:
    DEFINE INPUT  PARAMETER pType    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER pUser    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pNumMsgs AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pMessage AS CHARACTER NO-UNDO.
   
    pMessage = "". pNumMsgs = 0.
    FOR EACH zz_msg NO-LOCK WHERE zz_msg.zz_key1 = "MediaManagerMessage" AND 
                                  zz_msg.zz_key3 = pUser AND 
                                  (IF pType = "All" THEN TRUE ELSE zz_msg.zz_char[1] = pType) AND 
                                  zz_msg.zz_log[1] = FALSE 
                                  BY zz_msg.zz_char[5]:
        pNumMsgs = pNumMsgs + 1.
        RUN zz_messageFormat (zz_msg.zz_char[2],zz_msg.zz_char[5],zz_msg.zz_char[1],INPUT-OUTPUT pMessage).
        RUN zz_messageDelete (zz_msg.zz_key2).
    END.
END PROCEDURE.


PROCEDURE ZZ_MessageFormat:
    DEFINE INPUT        PARAMETER pMsg     AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER pMsgDT   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER pMsgType AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pMessage AS CHARACTER NO-UNDO.
    
    pMessage = pMessage + 
               pMsgDT   + CHR(10) +
               "*** " + pMsgType + " ***" + CHR(10) +
               pMsg     + CHR(10) + CHR(10).
    
END PROCEDURE.


PROCEDURE ExportRptDet:
    OUTPUT TO VALUE(SESSION:TEMP-DIR + "rpt_det.d").
    FOR EACH rpt_det: EXPORT rpt_det. END.
    OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE RerunCustomSize:
    DEFINE INPUT PARAMETER iBatchseq    AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER dBedHeight   AS DECIMAL      NO-UNDO.
    DEFINE INPUT PARAMETER dBedWidth    AS DECIMAL      NO-UNDO.
    DEFINE INPUT PARAMETER dStartX      AS DECIMAL      NO-UNDO.
    DEFINE INPUT PARAMETER dStartY      AS DECIMAL      NO-UNDO.
    DEFINE INPUT PARAMETER dGap         AS DECIMAL      NO-UNDO.
    DEFINE INPUT PARAMETER iNumPerSheet AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER cTemplate    AS CHARACTER    NO-UNDO.   
    
    DEFINE VARIABLE iItemsCnt           AS INTEGER      NO-UNDO.
    DEFINE VARIABLE dX                  AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE dY                  AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE cXML                AS CHARACTER    NO-UNDO.
    
    FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.batchseq = iBatchseq:
        FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = sign_mm_det.itemseq NO-ERROR.
        FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = sign_mm_det.itemseq AND squ_ptdet.type = "Panel" NO-ERROR.
        
        IF iItemsCnt < iNumPerSheet THEN DO:
            IF iItemsCnt = 0 THEN DO:
                dX = dStartX.
            END.
            ELSE DO:
                dX = dX + squ_ptdet.pressprintingheight + dGap.    
            END.
            dY = dStartY.
            
            iItemsCnt = iItemsCnt + 1.                  
        END.
        
        IF iItemsCnt = iNumPerSheet THEN DO:
            /*Write XML*/
            
            
            /*Do back side*/
            
            iItemsCnt = 0.    
        END.        
        
    END.
END PROCEDURE.

