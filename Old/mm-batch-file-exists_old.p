/* mm-batch-file-exists.p*/


DEFINE INPUT  PARAMETER mm-pp-handle AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER pRec         AS RECID     NO-UNDO.
DEFINE OUTPUT PARAMETER oFile        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oExist       AS LOGICAL   NO-UNDO.

DEFINE VARIABLE pathFound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chotfolderseq AS INTEGER   NO-UNDO.
DEFINE VARIABLE outputfile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE zundSF        AS CHARACTER NO-UNDO.
DEFINE VARIABLE zundDF        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPathway      AS CHARACTER NO-UNDO.

{mm.i "NEW"}

DEFINE BUFFER buff_sign_mm_hdr FOR sign_mm_hdr.

FIND buff_sign_mm_hdr NO-LOCK WHERE RECID(buff_sign_mm_hdr) = pRec NO-ERROR.

IF NOT AVAILABLE buff_sign_mm_hdr THEN NEXT.


FIND signbed NO-LOCK WHERE signbed.seq = buff_sign_mm_hdr.bedseq NO-ERROR.

IF buff_sign_mm_hdr.runseq MODULO 2 <> 0 THEN DO:
    ASSIGN oFile = STRING(buff_sign_mm_hdr.bedseq).
    IF LENGTH(oFile, "character") = 1 THEN ASSIGN oFile = "bed-0" + oFile + ".pdf".
    ELSE ASSIGN oFile = "bed-" + oFile + ".pdf".
    ASSIGN oFile = "\\fs02\dfssancluster\SignArt\DigitalBedTemplates\" + oFile.
    IF buff_sign_mm_hdr.BatchNested THEN DO:
        FIND FIRST sign_mm_det NO-LOCK WHERE sign_mm_det.batchseq = int(buff_sign_mm_hdr.batchseq) NO-ERROR.
        IF AVAIL sign_mm_det THEN DO:
            oFile = sign_mm_det.artfile.
            RELEASE sign_mm_det.
        END.
    END.

    oExist = TRUE.
END.
ELSE DO:
    pathfound = FALSE.
    cHotfolderseq = buff_sign_mm_hdr.pt_hotfolderseq.
    RUN gethotfolder IN mm-pp-handle(buff_sign_mm_hdr.batchseq,buff_sign_mm_hdr.matlType,INPUT-OUTPUT cHotfolderseq,OUTPUT outputfile,OUTPUT zundSF,OUTPUT zundDF).
    IF INDEX(outputfile,"\In") > 0 THEN outputfile = REPLACE(outputfile,"\In","").
    ASSIGN cpathway = outputfile + "\Batch" + string(buff_sign_mm_hdr.batchseq) +  (IF AVAIL signbed THEN "_" + string(signbed.imageheight) + "x" + STRING(signbed.imagewidth) ELSE "") + "_1.pdf".
    
    IF SEARCH(cpathway) <> ? THEN DO:
        ASSIGN oFile = cPathway
               pathfound  = TRUE. 
    END.
    ELSE IF SEARCH(outputfile + "\Batch" + string(buff_sign_mm_hdr.batchseq) +  (IF AVAIL signbed THEN "_" + string(signbed.imagewidth) + "x" + STRING(signbed.imageheight) ELSE "") + "_1.pdf") <> ? THEN DO:
        ASSIGN cPathway  = outputfile + "\Batch" + string(buff_sign_mm_hdr.batchseq) +  (IF AVAIL signbed THEN "_" + string(signbed.imagewidth) + "x" + STRING(signbed.imageheight) ELSE "") + "_1.pdf"
               oFile     = cPathway
               pathfound = TRUE.   
    END.
    ELSE DO:
        cPathway = ranfolder + "\Batch" + string(buff_sign_mm_hdr.batchseq) +  (IF AVAIL signbed THEN "_" + string(signbed.imageheight) + "x" + STRING(signbed.imagewidth) ELSE "") + "_1.pdf".
        IF SEARCH(cPathway) <> ? THEN DO:
            ASSIGN oFile = cPathway
                   pathfound  = TRUE.
        END.
    END.

    IF oFile = "" THEN oFile = ?.
    oExist = oFile <> ?.
    /*DISPLAY oFile FORMAT "x(75)".*/

END.
