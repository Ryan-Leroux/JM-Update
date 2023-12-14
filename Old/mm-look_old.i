DEFINE {1} SHARED TEMP-TABLE tdet
    FIELD tAvgDue        AS DATE
    FIELD tBatch         AS CHAR
    FIELD tCDate         AS DATE
    FIELD tRDate         AS DATE
    FIELD tDone          AS DATE
    FIELD tBed           AS CHAR
    FIELD tRtime         AS INT
    FIELD tSides         AS CHAR
    FIELD tFile          AS CHAR
    FIELD tBedseq        AS CHAR
    FIELD tQty           AS INT
    FIELD tBin           AS CHAR
    FIELD tMat           AS CHAR
    FIELD tPrinted       AS INT
    FIELD tInv           AS CHAR
    FIELD tCustom        AS CHAR
    FIELD tSeq           AS INT
    FIELD RipSent        AS LOG
    FIELD tFrontp        AS LOG
    FIELD tBackp         AS LOG
    FIELD materialposted AS LOG
    FIELD tCutFile       AS CHAR
    FIELD tMachTime      AS INT
    FIELD tMtString      AS CHAR
    FIELD hotfolderseq   AS INT
    FIELD batchnested    AS LOG
    FIELD WhiteInk       AS LOG
    FIELD Ripped         AS LOG
    FIELD DispOrder      AS INT
    FIELD zzchar_2       AS CHAR
    FIELD tFullBed       AS LOG
    INDEX tBatch AS UNIQUE tBatch.

DEFINE {1} SHARED TEMP-TABLE fdet
    FIELD fBatch      AS CHAR
    FIELD fSo         AS CHAR
    FIELD fItemNo     AS CHAR
    FIELD fPartNo     AS CHAR
    FIELD fArtLinkSeq AS INT
    FIELD fItemseq    AS INT
    FIELD fPos        AS CHAR
    FIELD fMulti      AS CHAR
    FIELD fOnbed      AS CHAR
    FIELD fTotal      AS CHAR
    FIELD fPrinted    AS CHAR
    FIELD fFile       AS CHAR
    FIELD fLocs       AS CHAR
    FIELD fRack       AS CHAR
    FIELD fDue        AS DATE
    FIELD fInvPart    AS CHAR.

DEFINE {1} SHARED TEMP-TABLE posDet
    FIELD pItemseq AS INT
    FIELD pPos     AS INT.
    

DEFINE {1} SHARED TEMP-TABLE edet
    FIELD eEmpNo   AS CHAR
    FIELD eName    AS CHAR
    FIELD eOnClock AS CHAR
    FIELD eBatch   AS CHAR
    FIELD eHrsId   AS CHAR
    INDEX eEmpNo AS UNIQUE eEmpNo.

DEFINE {1} SHARED TEMP-TABLE logs
    FIELD lProcedure  AS CHAR
    FIELD lStartTime  AS CHAR
    FIELD lFinishTime AS CHAR
    FIELD lRunTime    AS CHAR
    FIELD lRunDate    AS CHAR
    FIELD lXml        AS CHAR
    FIELD lResponse   AS CHAR.

DEFINE {1} SHARED TEMP-TABLE pathways
    FIELD pathway AS CHAR.

DEFINE {1} SHARED TEMP-TABLE idet
    FIELD iName     AS CHAR
    FIELD iPathway  AS CHAR.

DEFINE {1} SHARED TEMP-TABLE ranDet
    FIELD Name     AS CHAR
    FIELD Pathway  AS CHAR.

DEFINE {1} SHARED TEMP-TABLE ttdel
    FIELD ttitemseq AS INT
    INDEX ttitemseq AS UNIQUE ttitemseq.

DEFINE TEMP-TABLE job_det
    FIELD so_no        AS CHAR
    FIELD item_no      AS CHAR
    FIELD oper_seq     AS INTEGER
    FIELD job_location AS CHAR
    FIELD job_activity AS CHAR
    INDEX so_no IS PRIMARY so_no item_no.

DEFINE {1} SHARED TEMP-TABLE tLogOff
    FIELD tBatch        AS CHAR
    FIELD tLoggedIn     AS INTEGER
    FIELD tLoggingOut   AS INTEGER
    FIELD askedQuestion AS LOGICAL
    FIELD tComplete     AS LOGICAL
    INDEX tBatch IS PRIMARY UNIQUE tBatch.

DEFINE {1} SHARED TEMP-TABLE printDet  NO-UNDO
    FIELD itemseq AS INTEGER.

DEFINE {1} SHARED TEMP-TABLE tBatches NO-UNDO
    FIELD batchseq AS INTEGER
    INDEX batchseq IS PRIMARY UNIQUE batchseq.
    

