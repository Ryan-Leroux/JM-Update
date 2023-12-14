DEFINE INPUT  PARAMETER pBatch    AS CHAR    NO-UNDO.
DEFINE OUTPUT PARAMETER pImgFound AS LOGICAL NO-UNDO.
    
{mm.i}

DEF VAR fname    AS CHAR NO-UNDO.
DEF VAR cpathway AS CHAR NO-UNDO.
DEF VAR iLoop    AS INT  NO-UNDO.

DEFINE TEMP-TABLE idet
    FIELD iName     AS CHAR
    FIELD iPathway  AS CHAR.

    
/*get all images that have not been completed*/
FOR EACH pt_hotfolder NO-LOCK:
    cpathway = Homefolder + "\" + entry((num-entries(pt_hotfolder.pathway,"\") - 1),pt_hotfolder.pathway,"\") + "\" + entry(num-entries(pt_hotfolder.pathway,"\"),pt_hotfolder.pathway,"\") + "\".
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

/*was image found in mtl1 folders*/
pImgFound = NO.
DO iLoop = 1 TO NUM-ENTRIES(pBatch):
    IF CAN-FIND(FIRST idet WHERE idet.iName BEGINS "Batch" + entry(iLoop,pBatch)) THEN DO:
        ASSIGN pImgFound = YES.
    END.
END.


IF pImgFound = YES THEN RETURN.

/*get all completed images*/
DO iLoop = 1 TO NUM-ENTRIES(pBatch):
    FIND sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.batchseq = int(entry(iLoop,pBatch)) NO-ERROR.
    IF AVAIL sign_mm_hdr THEN DO:
        FIND signbed NO-LOCK WHERE signbed.seq = sign_mm_hdr.bedseq NO-ERROR.
        cPathway = ranfolder + "\Batch" + string(sign_mm_hdr.batchseq) +  (IF AVAIL signbed THEN "_" + string(signbed.imageheight) + "x" + STRING(signbed.imagewidth) ELSE "") + "_1.pdf".
        IF SEARCH(cPathway) <> ? THEN DO:
            ASSIGN pImgFound = YES.
        END.
    END.
END.

/* FILE-INFO:FILE-NAME = cpathway.                  */
/* IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:         */
/*     INPUT FROM OS-DIR(cpathway).                 */
/*     IMPORT ^.                                    */
/*     IMPORT ^.                                    */
/*     REPEAT:                                      */
/*         IMPORT fname.                            */
/*         FILE-INFO:FILE-NAME  = fname.            */
/*         CREATE idet.                             */
/*         ASSIGN idet.iname    = fname             */
/*                idet.iPathway = cpathway + fname. */
/*     END.                                         */
/* END.                                             */
/*                                                                                            */
/*                                                                                            */
/* /*was image found in completed batches folder*/                                            */
/* pImgFound = NO.                                                                            */
/* DO iLoop = 1 TO NUM-ENTRIES(pBatch):                                                       */
/*     IF CAN-FIND(FIRST idet WHERE idet.iName BEGINS "Batch" + entry(iLoop,pBatch)) THEN DO: */
/*         ASSIGN pImgFound = YES.                                                            */
/*     END.                                                                                   */
/* END                                                                                        */
    


