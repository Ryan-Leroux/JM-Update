DEFINE INPUT  PARAMETER ptSubseq AS INT   NO-UNDO.
DEFINE OUTPUT PARAMETER matRecid AS RECID NO-UNDO.

FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.subseq = ptSubseq AND squ_ptdet.TYPE <> "Frame" NO-ERROR.

FOR EACH squ_mat NO-LOCK WHERE squ_mat.subseq = ptSubseq:
    /*IF ptSubseq = 4314574 THEN message "here" view-as alert-box message.*/
    FIND partfile NO-LOCK WHERE partfile.part_no = squ_mat.part_no NO-ERROR.


/*     DISPLAY squ_mat.zzchar_1 FORMAT "x(50)". */
/*     UPDATE squ_mat.zzchar_1.                 */
/*     MESSAGE squ_mat.zzchar_1 VIEW-AS ALERT-BOX. */
    IF AVAILABLE partfile THEN DO:
        IF partfile.prd_line = "S3X" THEN NEXT.
        IF partfile.prd_line = "S1H" THEN NEXT.
        IF AVAIL squ_ptdet THEN
        
        IF  INDEX(squ_ptdet.pt_substrate,"Banner")   = 0 
        AND INDEX(squ_ptdet.pt_substrate,"Vinyl")    = 0 
        AND INDEX(squ_ptdet.pt_substrate,"Magnetic") = 0 
        AND squ_mat.zzchar_1 MATCHES "*Vinyl*" THEN NEXT. /*changed "Decal" to "Vinyl" in 2nd index statement*/
        
        IF INDEX(squ_mat.zzchar_1,"laminate") > 0 THEN NEXT.

        IF squ_mat.zzchar_1 MATCHES "*panel*" 
            OR squ_mat.zzchar_1 MATCHES "*Corrugated*" 
            OR squ_mat.zzchar_1 MATCHES "*Fold Over*" 
            OR squ_mat.zzchar_1 MATCHES "*Corex*" 
            OR squ_mat.zzchar_1 MATCHES "*aluminum*" 
            OR squ_mat.zzchar_1 MATCHES "*Poly*" 
            OR squ_mat.zzchar_1 MATCHES "*reflective*" 
            OR squ_mat.zzchar_1 MATCHES "*Omegabond*"    
            OR squ_mat.zzchar_1 MATCHES "*sheet*" 
            OR squ_mat.zzchar_1 MATCHES "*PVC*" 
            OR squ_mat.zzchar_1 MATCHES "*Magnetic*" 
            OR squ_mat.zzchar_1 MATCHES "*Rider*"    
            OR squ_mat.zzchar_1 MATCHES "*Vinyl*" 
            OR squ_mat.zzchar_1 matches "*Alumalite*" 
            OR squ_mat.zzchar_1 MATCHES "*Panel*" 
            OR squ_mat.zzchar_1 MATCHES "*Brushed Alum*"
            OR squ_mat.zzchar_1 MATCHES "*Static Cling*" THEN DO:

            ASSIGN matRecid = RECID(squ_mat).
            IF squ_mat.zzchar_1 MATCHES "*panel*"  THEN
                LEAVE.
            ELSE
                NEXT.
        END.
        IF squ_mat.part_no = "LBZCO0624" 
            OR squ_mat.part_no = "AIBZCO0624" 
            OR squ_mat.part_no = "LMZ242472" 
            OR squ_mat.part_no = "LMZ241272" 
            OR squ_mat.part_no = "LMY203682" 
            OR squ_mat.part_no = "BSP240108R"
            OR squ_mat.part_no = "Custom Material"
            OR squ_mat.part_no BEGINS "LMXBN" 
            OR squ_mat.part_no BEGINS "LMXLX" THEN DO:
            ASSIGN matRecid = RECID(squ_mat).
            LEAVE.
        END.
    END.
END.
