&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          system           PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: sotrckiq.w

  Description: UPS Tracking Info Browser
  
  Input Parameters: start-key
      <none>

  Output Parameters:
      <none>

  Author: David Hopper

  Created: 04/21/00

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/************************************************************************/
/* Modification Log                                                     */
/*   Date   Userid   Description                                        */
/* -------- -------- -------------------------------------------------- */
/* 04/21/00 dhopper  Initial Coding                                     */
/************************************************************************/
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_Running) EQ 0 &THEN
  {glob_var.i " " "new"}
  DEFINE INPUT  PARAMETER start-key  AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER actions    AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER this-recid AS RECID     NO-UNDO.
&ELSE
  {glob_var.i "new" "new"}
  DEFINE VARIABLE start-key  AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE actions    AS LOGICAL   INITIAL TRUE NO-UNDO.
  DEFINE VARIABLE this-recid AS RECID                  NO-UNDO. 
  start-key = "Digital-2".
&ENDIF


  {barvar.i "NEW"}
  {networkshare.i}
  {VPE.I}
  {mm.i "NEW"}
  
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE find-recid         AS RECID                    NO-UNDO.
DEFINE VARIABLE find-recid2        AS RECID                    NO-UNDO.
DEFINE VARIABLE find-recid3        AS RECID                    NO-UNDO.
DEFINE VARIABLE appl               AS CHARACTER INITIAL "SO"   NO-UNDO.
DEFINE VARIABLE add_rec            AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE numMsgs       AS INTEGER   NO-UNDO.
DEFINE VARIABLE pList         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppSrv       AS HANDLE    NO-UNDO.
DEFINE VARIABLE AppServCon    AS CHARACTER NO-UNDO.
DEFINE VARIABLE progHandle    AS HANDLE    NO-UNDO.
DEFINE VARIABLE cMsg          AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOK           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE setName       AS HANDLE    NO-UNDO.
DEFINE VARIABLE proName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE errorCnt      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLoop         AS INTEGER   NO-UNDO.
DEFINE VARIABLE par           AS CHAR      NO-UNDO.
DEFINE VARIABLE dbase         AS CHAR      NO-UNDO.
DEFINE VARIABLE cOverLay      AS CHAR      NO-UNDO.
DEFINE VARIABLE DropFileName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE machineCode   AS CHARACTER NO-UNDO INITIAL "1".
DEFINE VARIABLE currentBatch  AS CHARACTER NO-UNDO.
DEFINE VARIABLE hColumn       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hColumn2      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hColumn3      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hColumn4      AS HANDLE    NO-UNDO.
DEFINE VARIABLE superUser     AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpbatch      AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpBatch2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpBatch3     AS CHARACTER NO-UNDO.
DEFINE VARIABLE zBatch        AS CHARACTER NO-UNDO.
DEFINE VARIABLE zMat          AS CHARACTER NO-UNDO.
DEFINE VARIABLE mm2Handle     AS HANDLE    NO-UNDO.
DEFINE VARIABLE mm-pp-handle  AS HANDLE    NO-UNDO.
DEFINE VARIABLE applyClose    AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
DEFINE VARIABLE nextRunSeq    AS INTEGER   NO-UNDO.
DEFINE VARIABLE qtyRan        AS INTEGER   NO-UNDO.
DEFINE VARIABLE tryCnt        AS INTEGER   NO-UNDO.
DEFINE VARIABLE foundHDR      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE answer        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE issueIT       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE tmpQty        AS INTEGER   NO-UNDO.
DEFINE VARIABLE jpgFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpBed        AS CHAR      NO-UNDO.
DEFINE VARIABLE tCnt          AS INTEGER   NO-UNDO.
DEFINE VARIABLE tmpint        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lastsize      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTime         AS INTEGER   NO-UNDO.
DEFINE VARIABLE OrderNo       AS CHAR      NO-UNDO.
DEFINE VARIABLE ItemNo        AS INTEGER   NO-UNDO.
DEFINE VARIABLE isRider       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE doReprint     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE otherside     AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VARIABLE midRunCorex   AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VARIABLE BackSide      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE firstbackside AS LOGICAL   NO-UNDO.
DEFINE VARIABLE corexrunseq   AS INTEGER   NO-UNDO.
DEFINE VARIABLE accessUsers   AS CHARACTER NO-UNDO INITIAL "printersign,terryp".
DEFINE VARIABLE zzMsg         AS CHARACTER NO-UNDO.
DEFINE VARIABLE thisMachine   AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE destDir2      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bClockOut     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE reprintMachine AS INTEGER  NO-UNDO.
DEFINE VARIABLE cMode         AS CHARACTER NO-UNDO.


DEFINE VARIABLE previewvpe     AS COM-HANDLE NO-UNDO.
/* DEFINE VARIABLE chPreviewImage AS COM-HANDLE NO-UNDO. */

{mgseclist.i "SuperUsers" SuperUser}
SuperUser = SuperUser + ",Houstons,Lancep".
{mm-look.i NEW}

DEFINE TEMP-TABLE ttBatch
    FIELD batchseq AS INTEGER
    INDEX batchseq IS PRIMARY UNIQUE batchseq.
    
DEFINE TEMP-TABLE ttReprint
    FIELD ttNeed     AS INT
    FIELD ttOrderQty AS INT 
    FIELD ttQty      AS INT
    FIELD ttSO       AS CHAR
    FIELD ttItemNo   AS INT
    FIELD ttALS      AS INT
    FIELD ttFile     AS CHAR
    FIELD ttDelayed  AS LOGICAL INITIAL FALSE
    FIELD ttArtRecid AS RECID.
    
DEFINE TEMP-TABLE ttCorexReprint
    FIELD ttQty         AS INT
    FIELD ttSO          AS CHAR
    FIELD ttItemSeq     AS INT
    FIELD ttALS         AS INT
    FIELD ttItemNo      AS INT
    FIELD ttArtFile     AS CHAR
    INDEX ttCR IS PRIMARY UNIQUE ttALS ttItemSeq.
    

DEFINE BUFFER ffdet       FOR fdet.
DEFINE BUFFER ttdet       FOR tdet.
DEFINE BUFFER t_det       FOR tdet.
DEFINE BUFFER tt_det      FOR tdet.
DEFINE BUFFER mm_file     FOR zz_file.
DEFINE BUFFER b_mm_hdr    FOR sign_mm_hdr.
DEFINE BUFFER b_mm_det    FOR sign_mm_det.
DEFINE BUFFER buf_mm_hdr  FOR sign_mm_hdr.
DEFINE BUFFER buf_mm_det  FOR sign_mm_det.
DEFINE BUFFER buff_mm_det FOR sign_mm_det.
DEFINE BUFFER buff_mm_hdr FOR sign_mm_hdr.
DEFINE BUFFER X_file      FOR zz_file.
DEFINE BUFFER bbArt       FOR ttArt.
DEFINE BUFFER b_signbed   FOR signbed.
DEFINE BUFFER buf_signbed FOR signbed.
DEFINE BUFFER b_detail    FOR h_detail.
DEFINE BUFFER manpower_zz_file FOR zz_file.


SESSION:DATA-ENTRY-RETURN = TRUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME det

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fdet edet tdet JDF-Item

/* Definitions for BROWSE det                                           */
&Scoped-define FIELDS-IN-QUERY-det fSo fItemNo fArtLinkSeq fpartNO fInvPart fPos fOnbed fTotal fPrinted fLocs fFile   
&Scoped-define ENABLED-FIELDS-IN-QUERY-det   
&Scoped-define SELF-NAME det
&Scoped-define QUERY-STRING-det FOR EACH fdet NO-LOCK WHERE fdet.fBatch = tdet.tBatch INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-det OPEN QUERY det FOR EACH fdet NO-LOCK WHERE fdet.fBatch = tdet.tBatch INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-det fdet
&Scoped-define FIRST-TABLE-IN-QUERY-det fdet


/* Definitions for BROWSE emp                                           */
&Scoped-define FIELDS-IN-QUERY-emp edet.eName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-emp   
&Scoped-define SELF-NAME emp
&Scoped-define QUERY-STRING-emp FOR EACH edet BY eName
&Scoped-define OPEN-QUERY-emp OPEN QUERY {&SELF-NAME} FOR EACH edet BY eName.
&Scoped-define TABLES-IN-QUERY-emp edet
&Scoped-define FIRST-TABLE-IN-QUERY-emp edet


/* Definitions for BROWSE HDR                                           */
&Scoped-define FIELDS-IN-QUERY-HDR /* tdet.tcustom */ /* tdet.tavgdue */ tdet.tbatch tdet.tMachTime tdet.tcDate tdet.tDone tdet.tmat tdet.tbedseq tdet.tseq tdet.tavgdue tdet.tBed tdet.tSide tdet.tPrinted tdet.tqty tdet.tinv tdet.tBin tdet.materialposted   
&Scoped-define ENABLED-FIELDS-IN-QUERY-HDR   
&Scoped-define SELF-NAME HDR
&Scoped-define OPEN-QUERY-HDR IF tgl_show:CHECKED IN FRAME Default-frame THEN     OPEN QUERY {&SELF-NAME} FOR EACH tdet NO-LOCK BY string(tdet.tRDate) BY string(tdet.tRtime) INDEXED-REPOSITION. ELSE     OPEN QUERY {&SELF-NAME} FOR EACH tdet NO-LOCK BY tdet.tseq  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-HDR tdet
&Scoped-define FIRST-TABLE-IN-QUERY-HDR tdet


/* Definitions for BROWSE JDFQueue                                      */
&Scoped-define FIELDS-IN-QUERY-JDFQueue JDF-Item.BatchSeq ~
JDF-Item.ManualSend AlreadySentJDF() 
&Scoped-define ENABLED-FIELDS-IN-QUERY-JDFQueue 
&Scoped-define QUERY-STRING-JDFQueue FOR EACH JDF-Item ~
      WHERE JDF-Item.Printer = thisMachine NO-LOCK ~
    BY JDF-Item.RunSeq INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-JDFQueue OPEN QUERY JDFQueue FOR EACH JDF-Item ~
      WHERE JDF-Item.Printer = thisMachine NO-LOCK ~
    BY JDF-Item.RunSeq INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-JDFQueue JDF-Item
&Scoped-define FIRST-TABLE-IN-QUERY-JDFQueue JDF-Item


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-det}~
    ~{&OPEN-QUERY-emp}~
    ~{&OPEN-QUERY-HDR}~
    ~{&OPEN-QUERY-JDFQueue}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-8 RECT-83 RECT-84 RECT-85 RECT-86 ~
btnRefresh tgl_Show findSo findItem btnFind chkDigital1 chkDigital2 ~
chkDigital3 HDR emp btn_start btn_stop btnIn JDFQueue btnViewCAD btnEmail ~
btnStartRIP det btnOverlay btnPrev btnNext btnLoc btnFile btnPdf 
&Scoped-Define DISPLAYED-OBJECTS tgl_Show findSo findItem Tg-multibatch ~
chkDigital1 chkDigital2 chkDigital3 numRecords cMachTime batchInfo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AlreadySentJDF WINDOW-1 
FUNCTION AlreadySentJDF RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Details 
       MENU-ITEM m_Reprint_Item LABEL "Reprint Item"  
       MENU-ITEM m_Print_Test_Colors LABEL "Print Test Colors"
       MENU-ITEM m_Gateway_Test LABEL "Gateway Test"  
       MENU-ITEM m_Test_History LABEL "Test History"  
       MENU-ITEM m_Move_Batch   LABEL "Move Batch"    
       MENU-ITEM m_Regen_DART_Files LABEL "Regen DART Files"
       MENU-ITEM m_Corex_Sheet_Size LABEL "Corex Sheet Size"
       MENU-ITEM RegenPCCorex   LABEL "Regen PC Corex".

DEFINE MENU MENU-BAR-WINDOW-1 MENUBAR
       SUB-MENU  m_Details      LABEL "Details"       
       MENU-ITEM m_JM_Queue     LABEL "JM Queue"      
       MENU-ITEM m_Close        LABEL "Close"         .

DEFINE MENU POPUP-MENU-det 
       MENU-ITEM m_View_JT      LABEL "View JT"       .

DEFINE MENU POPUP-MENU-HDR TITLE "Toggle Partial"
       MENU-ITEM m_Reprint_Template LABEL "Reprint Template"
       MENU-ITEM m_Reprint_Batch LABEL "Reprint Batch" 
       MENU-ITEM m_Send_Cut_File LABEL "Send Cut File" 
              DISABLED
       MENU-ITEM m_Regen_Batch_Image2 LABEL "Regen Batch Image"
       MENU-ITEM m_Rotate_Batch_Images LABEL "Rotate Batch Images"
       MENU-ITEM m_Toggle_Partial LABEL "Toggle Partial"
       MENU-ITEM m_Print_Color_Target LABEL "Print Color Target"
       MENU-ITEM m_Insert_Template LABEL "Insert Template".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE WebBrowser AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chWebBrowser AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnEmail 
     LABEL "Remove Item" 
     SIZE 25 BY 1.52
     BGCOLOR 12 FONT 17.

DEFINE BUTTON btnFile 
     LABEL "Open File" 
     SIZE 30 BY 1.52
     FONT 9.

DEFINE BUTTON btnFind 
     LABEL "Search" 
     SIZE 19.6 BY 1.24
     FONT 9.

DEFINE BUTTON btnIn 
     LABEL "Printer - IN" 
     SIZE 27 BY 1.52
     FONT 9.

DEFINE BUTTON btnLoc 
     LABEL "Open File Location" 
     SIZE 40 BY 1.52
     FONT 9.

DEFINE BUTTON btnMaterial 
     LABEL "Issue Material" 
     SIZE 28 BY 1.52
     BGCOLOR 10 FONT 17.

DEFINE BUTTON btnNext 
     LABEL ">" 
     SIZE 10 BY 1 TOOLTIP "Side 2"
     FONT 9.

DEFINE BUTTON btnOut 
     LABEL "Printer - OUT" 
     SIZE 27 BY 1.52
     FONT 9.

DEFINE BUTTON btnOverlay 
     LABEL "Show Overlay" 
     SIZE 15 BY 1.

DEFINE BUTTON btnPdf 
     LABEL "Open PDF" 
     SIZE 30 BY 1.52
     FONT 9.

DEFINE BUTTON btnPrev 
     LABEL "<" 
     SIZE 10 BY 1 TOOLTIP "Side 1"
     FONT 9.

DEFINE BUTTON btnPurge 
     LABEL "Purge" 
     SIZE 30 BY 1.52 TOOLTIP "Makes all beds"
     FONT 9.

DEFINE BUTTON btnRefresh 
     LABEL "Refresh Data" 
     SIZE 25 BY 1.24
     FONT 17.

DEFINE BUTTON btnReset 
     LABEL "Reset Batches" 
     SIZE 30 BY 1.52 TOOLTIP "Makes only full beds"
     FONT 9.

DEFINE BUTTON btnStartRIP 
     LABEL "Start RIP Queue" 
     SIZE 42 BY 1.14.

DEFINE BUTTON btnViewCAD 
     LABEL "View CAD" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_start 
     IMAGE-UP FILE "start-on.gif":U
     IMAGE-INSENSITIVE FILE "start-off.gif":U
     LABEL "Start Batch" 
     SIZE 27 BY 1.57 TOOLTIP "Click to start batch".

DEFINE BUTTON btn_stop 
     IMAGE-UP FILE "stop-on.gif":U
     IMAGE-INSENSITIVE FILE "stop-off.gif":U
     LABEL "Stop Batch" 
     SIZE 27 BY 1.57 TOOLTIP "Click to finish end time on batch.".

DEFINE VARIABLE batchInfo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 124.8 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE cMachTime AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 11.4 BY 1.24
     FONT 17 NO-UNDO.

DEFINE VARIABLE findItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.24 TOOLTIP "Item Number"
     BGCOLOR 15 FONT 17 NO-UNDO.

DEFINE VARIABLE findSo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.24 TOOLTIP "Order Number"
     BGCOLOR 15 FONT 9 NO-UNDO.

DEFINE VARIABLE numRecords AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 11.4 BY 1.24
     FONT 17 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.6 BY 10.1
     BGCOLOR 15 FGCOLOR 15 .

DEFINE RECTANGLE RECT-83
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 149.4 BY 19.14.

DEFINE RECTANGLE RECT-84
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 30.2 BY 2
     BGCOLOR 10 .

DEFINE RECTANGLE RECT-85
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 27.2 BY 2
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-86
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 23.6 BY 1.67
     BGCOLOR 0 FGCOLOR 0 .

DEFINE VARIABLE chkDigital1 AS LOGICAL INITIAL no 
     LABEL "D1" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .81 TOOLTIP "Indicates if Digital printer #1 is on." NO-UNDO.

DEFINE VARIABLE chkDigital2 AS LOGICAL INITIAL no 
     LABEL "D2" 
     VIEW-AS TOGGLE-BOX
     SIZE 7.6 BY .81 TOOLTIP "Indicates if Digital printer #2 is on." NO-UNDO.

DEFINE VARIABLE chkDigital3 AS LOGICAL INITIAL no 
     LABEL "D3" 
     VIEW-AS TOGGLE-BOX
     SIZE 8.2 BY .81 TOOLTIP "Indicates if Digital Printer #3 is active" NO-UNDO.

DEFINE VARIABLE Tg-multibatch AS LOGICAL INITIAL no 
     LABEL "Multi Batch Enabled" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tgl_Show AS LOGICAL INITIAL no 
     LABEL "Show Completed" 
     VIEW-AS TOGGLE-BOX
     SIZE 33.8 BY 1.19
     FONT 9 NO-UNDO.

DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 20 BY 1.52
     FONT 9.

DEFINE BUTTON btnSend 
     LABEL "Send" 
     SIZE 20 BY 1.52
     FONT 9.

DEFINE VARIABLE cMessage AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 142 BY 8.76
     BGCOLOR 15 FONT 9 NO-UNDO.

DEFINE VARIABLE cCC AS CHARACTER FORMAT "X(256)":U INITIAL "brittanyb@lowen.com" 
     LABEL "CC" 
      VIEW-AS TEXT 
     SIZE 138.6 BY .91
     FONT 9 NO-UNDO.

DEFINE VARIABLE cTo AS CHARACTER FORMAT "X(256)":U INITIAL "Maryp@lowen.com" 
     LABEL "TO" 
      VIEW-AS TEXT 
     SIZE 138.4 BY .91
     FONT 9 NO-UNDO.

DEFINE VARIABLE RADIO-cTo AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "IT", 1,
"Artist", 2,
"CSR", 3
     SIZE 41.8 BY .95
     FONT 17 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY det FOR 
      fdet SCROLLING.

DEFINE QUERY emp FOR 
      edet SCROLLING.

DEFINE QUERY HDR FOR 
      tdet SCROLLING.

DEFINE QUERY JDFQueue FOR 
      JDF-Item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS det WINDOW-1 _FREEFORM
  QUERY det DISPLAY
      fSo         LABEL "Order"   WIDTH 16
     fItemNo     LABEL "Item"    WIDTH 8
     fArtLinkSeq COLUMN-LABEL "Art!Seq" WIDTH 8
     fpartNO     LABEL "Part"    FORMAT "x(30)" WIDTH 23
     fInvPart    LABEL "Inv Part"  FORMAT "x(30)" WIDTH 40
     fPos        LABEL "Seq"     WIDTH 12
     fOnbed      COLUMN-LABEL "This!Bed" WIDTH 8 
     fTotal      COLUMN-LABEL "Total!Qty" WIDTH 10
     fPrinted    COLUMN-LABEL "Qty!Printed" FORMAT "x(25)" WIDTH 28
     fLocs       COLUMN-LABEL "Other!Batches" FORMAT "x(40)" WIDTH 30
     fFile       LABEL "Artfile" FORMAT "x(60)" WIDTH 100
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 167 BY 19.19
         BGCOLOR 15 FONT 9 ROW-HEIGHT-CHARS .86.

DEFINE BROWSE emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS emp WINDOW-1 _FREEFORM
  QUERY emp DISPLAY
      edet.eName    LABEL "Employee" FORMAT "x(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 77.2 BY 8.86
         BGCOLOR 15 FONT 9
         TITLE BGCOLOR 15 "Employees Logged In To Printer" FIT-LAST-COLUMN.

DEFINE BROWSE HDR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS HDR WINDOW-1 _FREEFORM
  QUERY HDR NO-LOCK DISPLAY
      /*       tdet.tcustom  LABEL ""          WIDTH 3 */
/*      tdet.tavgdue   LABEL "avgDue"    WIDTH 10 */
     tdet.tbatch    LABEL "Batch"      WIDTH 15
     tdet.tMachTime LABEL "Mins"       WIDTH 12
     tdet.tcDate    LABEL "Created"    WIDTH 18
     tdet.tDone     LABEL "Completed"  WIDTH 20
     tdet.tmat      LABEL "Material"   WIDTH 18
     tdet.tbedseq   LABEL "BedSeq"     WIDTH 14
     tdet.tseq      LABEL "Runseq"     WIDTH 14
     tdet.tavgdue   LABEL "Due Date"   WIDTH 18
     tdet.tBed      LABEL "Size"       WIDTH 12
     tdet.tSide     LABEL "Sides"      WIDTH 11
     tdet.tPrinted  LABEL "Prtd"       WIDTH 8
     tdet.tqty      LABEL "Copies"     WIDTH 12
     
     tdet.tinv   LABEL "Part No." FORMAT "x(25)" WIDTH 25
     tdet.tBin   LABEL "Bin"  FORMAT "x(15)"     WIDTH 12
     tdet.materialposted LABEL "Posted"          WIDTH 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 170.2 BY 15.95
         BGCOLOR 15 FONT 9 FIT-LAST-COLUMN.

DEFINE BROWSE JDFQueue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS JDFQueue WINDOW-1 _STRUCTURED
  QUERY JDFQueue NO-LOCK DISPLAY
      JDF-Item.BatchSeq FORMAT "x(256)":U WIDTH 12
      JDF-Item.ManualSend FORMAT "yes/no":U WIDTH 15.2
      AlreadySentJDF() COLUMN-LABEL "Already! Sent" WIDTH 8.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 42 BY 6.19
         TITLE "JDF Queue" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnRefresh AT ROW 1.1 COL 179.6 WIDGET-ID 10
     tgl_Show AT ROW 1.14 COL 3.6 WIDGET-ID 8
     findSo AT ROW 1.14 COL 53.2 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     findItem AT ROW 1.14 COL 76.6 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     btnFind AT ROW 1.19 COL 84.4 WIDGET-ID 66
     Tg-multibatch AT ROW 1.71 COL 278 WIDGET-ID 102
     chkDigital1 AT ROW 1.76 COL 235 HELP
          "Indicates if FB7600 machine #1 is on." WIDGET-ID 96
     chkDigital2 AT ROW 1.76 COL 242.8 HELP
          "Indicates if FB7600 machine #2 is on." WIDGET-ID 98
     chkDigital3 AT ROW 1.76 COL 250.6 WIDGET-ID 114
     HDR AT ROW 2.57 COL 2.6 WIDGET-ID 100
     emp AT ROW 3.14 COL 211 WIDGET-ID 300
     btn_start AT ROW 4.48 COL 289.8 HELP
          "Click to start mixing." WIDGET-ID 16
     btn_stop AT ROW 6.91 COL 289.8 HELP
          "Click to finish mixing and generate the material transactions." WIDGET-ID 18
     btnIn AT ROW 12.29 COL 220.8 WIDGET-ID 52
     btnOut AT ROW 12.29 COL 249.6 WIDGET-ID 54
     JDFQueue AT ROW 12.43 COL 277.2 WIDGET-ID 600
     btnMaterial AT ROW 13.33 COL 178.2 WIDGET-ID 34
     btnViewCAD AT ROW 15.05 COL 241 WIDGET-ID 116
     btnEmail AT ROW 15.71 COL 179.8 WIDGET-ID 44
     btnStartRIP AT ROW 16.95 COL 228 WIDGET-ID 104
     det AT ROW 18.71 COL 2.6 WIDGET-ID 200
     btnOverlay AT ROW 18.95 COL 282.6 WIDGET-ID 46
     btnPrev AT ROW 18.95 COL 298.2 WIDGET-ID 40
     btnNext AT ROW 18.95 COL 308.6 WIDGET-ID 42
     btnLoc AT ROW 38 COL 3 WIDGET-ID 2
     btnFile AT ROW 38 COL 44 WIDGET-ID 60
     btnReset AT ROW 38 COL 127.8 WIDGET-ID 4
     btnPurge AT ROW 38 COL 160.6 WIDGET-ID 6
     btnPdf AT ROW 38 COL 288 WIDGET-ID 24
     numRecords AT ROW 1.24 COL 122.8 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     cMachTime AT ROW 1.24 COL 159.4 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     batchInfo AT ROW 18.95 COL 169.8 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     "-" VIEW-AS TEXT
          SIZE 1.6 BY .62 AT ROW 1.38 COL 76.6 WIDGET-ID 68
          FONT 17
     " Trade Source" VIEW-AS TEXT
          SIZE 22 BY 1.29 AT ROW 9.76 COL 181.2 WIDGET-ID 90
          BGCOLOR 29 FONT 8
     " Printed" VIEW-AS TEXT
          SIZE 22 BY 1.29 AT ROW 3.48 COL 181 WIDGET-ID 78
          BGCOLOR 14 FONT 9
     "Key" VIEW-AS TEXT
          SIZE 6.8 BY .95 AT ROW 2.33 COL 188.2 WIDGET-ID 28
          FONT 9
     "Active Machines:" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.86 COL 216 WIDGET-ID 100
     " Template" VIEW-AS TEXT
          SIZE 22 BY 1.29 AT ROW 8.19 COL 181 WIDGET-ID 84
          BGCOLOR 22 FONT 9
     "Est Hours:" VIEW-AS TEXT
          SIZE 20 BY 1.14 AT ROW 1.24 COL 141.2 WIDGET-ID 94
          FONT 9
     "Order:" VIEW-AS TEXT
          SIZE 12.6 BY 1.14 AT ROW 1.14 COL 42.4 WIDGET-ID 74
          FONT 9
     "Already Ripped" VIEW-AS TEXT
          SIZE 22.8 BY 1.29 AT ROW 11.43 COL 181 WIDGET-ID 110
          BGCOLOR 13 FGCOLOR 0 FONT 8
     " Partial" VIEW-AS TEXT
          SIZE 22 BY 1.29 AT ROW 6.62 COL 181 WIDGET-ID 82
          BGCOLOR 11 FONT 9
     "Copies:" VIEW-AS TEXT
          SIZE 14.4 BY 1.05 AT ROW 1.29 COL 110.2 WIDGET-ID 72
          FONT 9
     " Printing" VIEW-AS TEXT
          SIZE 22 BY 1.29 AT ROW 5.05 COL 181 WIDGET-ID 80
          BGCOLOR 10 FONT 9
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 320 BY 38.76.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     RECT-8 AT ROW 2.81 COL 178.8 WIDGET-ID 26
     RECT-83 AT ROW 18.76 COL 169.8 WIDGET-ID 32
     RECT-84 AT ROW 13.1 COL 177.2 WIDGET-ID 48
     RECT-85 AT ROW 15.48 COL 178.6 WIDGET-ID 50
     RECT-86 AT ROW 9.57 COL 180.4 WIDGET-ID 112
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 320 BY 38.76.

DEFINE FRAME FRAME-A
     RADIO-cTo AT ROW 1.24 COL 15.6 NO-LABEL WIDGET-ID 4
     cMessage AT ROW 5.14 COL 6 NO-LABEL WIDGET-ID 18
     btnSend AT ROW 14.14 COL 51.2 WIDGET-ID 20
     btnCancel AT ROW 14.14 COL 81 WIDGET-ID 22
     cTo AT ROW 2.57 COL 7 COLON-ALIGNED WIDGET-ID 12
     cCC AT ROW 3.71 COL 7 COLON-ALIGNED WIDGET-ID 16
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.6 ROW 2.62
         SIZE 170.2 BY 15.91
         TITLE "Report Issues" WIDGET-ID 400.

DEFINE FRAME FRAME-B
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 170.8 ROW 19.95
         SIZE 147.8 BY 17.86 WIDGET-ID 500.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 38.48
         WIDTH              = 319.2
         MAX-HEIGHT         = 63.62
         MAX-WIDTH          = 512
         VIRTUAL-HEIGHT     = 63.62
         VIRTUAL-WIDTH      = 512
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-WINDOW-1:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-B:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB HDR chkDigital3 DEFAULT-FRAME */
/* BROWSE-TAB emp FRAME-A DEFAULT-FRAME */
/* BROWSE-TAB JDFQueue btnOut DEFAULT-FRAME */
/* BROWSE-TAB det btnStartRIP DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN batchInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnMaterial IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnOut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnOverlay:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnPurge IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnPurge:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnReset IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnReset:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cMachTime IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       det:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU POPUP-MENU-det:HANDLE
       det:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       HDR:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU POPUP-MENU-HDR:HANDLE.

/* SETTINGS FOR FILL-IN numRecords IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       numRecords:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR TOGGLE-BOX Tg-multibatch IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-A
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-A:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cCC IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       cTo:READ-ONLY IN FRAME FRAME-A        = TRUE.

ASSIGN 
       RADIO-cTo:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE det
/* Query rebuild information for BROWSE det
     _START_FREEFORM
OPEN QUERY det FOR EACH fdet NO-LOCK WHERE fdet.fBatch = tdet.tBatch INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE det */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE emp
/* Query rebuild information for BROWSE emp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH edet BY eName
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE emp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE HDR
/* Query rebuild information for BROWSE HDR
     _START_FREEFORM
IF tgl_show:CHECKED IN FRAME Default-frame THEN
    OPEN QUERY {&SELF-NAME} FOR EACH tdet NO-LOCK BY string(tdet.tRDate) BY string(tdet.tRtime) INDEXED-REPOSITION.
ELSE
    OPEN QUERY {&SELF-NAME} FOR EACH tdet NO-LOCK BY tdet.tseq  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE HDR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE JDFQueue
/* Query rebuild information for BROWSE JDFQueue
     _TblList          = "system.JDF-Item"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "system.JDF-Item.RunSeq|yes"
     _Where[1]         = "JDF-Item.Printer = thisMachine"
     _FldNameList[1]   > system.JDF-Item.BatchSeq
"JDF-Item.BatchSeq" ? ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > system.JDF-Item.ManualSend
"JDF-Item.ManualSend" ? ? "logical" ? ? ? ? ? ? no ? no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"AlreadySentJDF()" "Already! Sent" ? ? ? ? ? ? ? ? no ? no no "8.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE JDFQueue */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.91
       COLUMN          = 308
       HEIGHT          = 1
       WIDTH           = 4.4
       WIDGET-ID       = 56
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME WebBrowser ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 20.05
       COLUMN          = 171.2
       HEIGHT          = 17.62
       WIDTH           = 146.6
       WIDGET-ID       = 22
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME FRAME-B:HANDLE
       ROW             = 1.05
       COLUMN          = 1.2
       HEIGHT          = 17.67
       WIDTH           = 146.8
       WIDGET-ID       = 86
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
/* WebBrowser OCXINFO:CREATE-CONTROL from: {8856F961-340A-11D0-A96B-00C04FD705A2} type: WebBrowser */
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {E4809925-0E64-4CF6-B7EE-229DE4963BE0} type: VPE */
      CtrlFrame:MOVE-AFTER(FRAME FRAME-A:HANDLE).
      WebBrowser:MOVE-AFTER(FRAME FRAME-B:HANDLE).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WINDOW-1 WINDOW-1
ON WINDOW-CLOSE OF WINDOW-1
DO:
  /* This event will close the window and terminate the procedure.  */
   
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel WINDOW-1
ON CHOOSE OF btnCancel IN FRAME FRAME-A /* Cancel */
DO:
  ASSIGN cTo      :SCREEN-VALUE IN FRAME frame-a = ""
         cCC      :SCREEN-VALUE IN FRAME frame-a = ""
         cMessage :SCREEN-VALUE IN FRAME frame-a = ""
         FRAME frame-a:HIDDEN                    = TRUE
         radio-cTo:SCREEN-VALUE IN FRAME frame-a = "1".
/*          goCrop:HIDDEN IN FRAME default-frame    = TRUE. */
  APPLY "value-changed":u TO radio-cto.
  ENABLE ALL WITH FRAME default-frame.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEmail WINDOW-1
ON CHOOSE OF btnEmail IN FRAME DEFAULT-FRAME /* Remove Item */
DO:
  DEFINE VARIABLE cRemove  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE tmpChar  AS CHAR    NO-UNDO.
  DEFINE VARIABLE tmpMsg   AS CHAR    NO-UNDO.


  FIND FIRST b_detail NO-LOCK WHERE b_detail.order_no = "b-" + tdet.tbatch AND b_detail.finish_time = "" AND b_detail.activity = "D11" NO-ERROR.
  IF AVAIL b_detail THEN DO:
      RUN mm-msg.w("Must stop running batch before batch removal!",0,YES,1,OUTPUT qtyran, OUTPUT answer).
      RETURN.
  END.

  IF NOT AVAIL tdet THEN DO:
      RUN mm-msg.w("Must select a batch to remove!",0,YES,1,OUTPUT qtyran, OUTPUT answer).
      RETURN.
  END.

  RUN mm-msg.w("Are you sure you want to remove this batch and all other related batches?",1,YES,1,OUTPUT qtyran, OUTPUT answer).
  cRemove = answer.
  IF NOT cRemove THEN RETURN.
  
  RUN mm-msg.w("Was this batch started, but not finished?(highlighted yellow)",1,YES,1,OUTPUT qtyran, OUTPUT answer).
  cRemove = answer.
  IF cRemove THEN DO:
      FIND FIRST sign_mm_hdr WHERE sign_mm_hdr.batchseq = INT(tdet.tbatch) NO-ERROR.
      IF AVAIL sign_mm_hdr THEN ASSIGN sign_mm_hdr.qty_printed = sign_mm_hdr.qty.
      RELEASE sign_mm_hdr.     
  END.

  ASSIGN cRemove = NO.
  RUN mm-msg.w("Have all incorrect images been selected?",1,YES,1,OUTPUT qtyran, OUTPUT answer).
  IF answer = YES AND det:NUM-SELECTED-ROWS IN FRAME default-frame <= 0 THEN DO: 
      RUN mm-msg.w("Must select the image(s) with issues!",0,YES,1,OUTPUT qtyran, OUTPUT answer).
      RETURN.
  END.
  cRemove = answer.

  IF cRemove THEN DO:
      /*send email before delete*/
      DISABLE ALL WITH FRAME default-frame.
      ENABLE ALL WITH FRAME frame-a.
      ASSIGN FRAME frame-a:VISIBLE = TRUE
             FRAME frame-a:HIDDEN  = FALSE
             Radio-cTo    :HIDDEN  = TRUE.

      tmpChar = "". tmpMsg = "Bad item(s): " + CHR(10).
      IF det:NUM-SELECTED-ROWS IN FRAME default-frame > 0 THEN DO i = 1 TO det:NUM-SELECTED-ROWS IN FRAME default-frame:
            det:FETCH-SELECTED-ROW(i).
            IF NOT CAN-DO(tmpChar,fdet.fSo + "-" + fdet.fitemNO + "-" + STRING(fdet.fArtlinkseq)) THEN DO:
                ASSIGN tmpChar = tmpChar + (IF tmpChar = "" THEN "" ELSE ",") + fdet.fSo + "-" + fdet.fItemNo + "-" + STRING(fdet.fArtlinkseq)
                       tmpMsg  = tmpMsg  + fdet.fSo + "-" + fdet.fItemNo + "-" + STRING(fdet.fArtlinkseq) + CHR(10).
            END.
      END.

      ASSIGN cMessage:SCREEN-VALUE IN FRAME frame-a = tmpMsg.
  END.
  ELSE DO:
      RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFile WINDOW-1
ON CHOOSE OF btnFile IN FRAME DEFAULT-FRAME /* Open File */
DO:
    IF det:NUM-SELECTED-ROWS > 0 THEN DO i = 1 TO det:NUM-SELECTED-ROWS:
    det:FETCH-SELECTED-ROW(i).

      IF AVAIL(fdet) AND R-INDEX(fdet.fFile,"\") > 0 THEN DO:
            OS-COMMAND SILENT VALUE("start " + CHR(34) + CHR(34) + " " + chr(34) + fdet.fFile + CHR(34)).
      END.  
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFind
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFind WINDOW-1
ON CHOOSE OF btnFind IN FRAME DEFAULT-FRAME /* Search */
DO:
  IF findSo:SCREEN-VALUE <> "" AND findItem:SCREEN-VALUE <> "" THEN DO:
      FIND FIRST fdet NO-LOCK WHERE fdet.fso = findSo:SCREEN-VALUE 
                                AND fdet.fItemNo = findItem:SCREEN-VALUE NO-ERROR.
      IF AVAIL fdet THEN DO:
          FIND tdet WHERE tdet.tbatch = fdet.fbatch NO-ERROR.
          REPOSITION hdr TO RECID(RECID(tdet)).
          APPLY "VALUE-CHANGED":U TO hdr.
      END.
      ELSE MESSAGE "Cannot find that item!" VIEW-AS ALERT-BOX.
  END.
  ELSE MESSAGE "Must have Order and Item numbers for search" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnIn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnIn WINDOW-1
ON CHOOSE OF btnIn IN FRAME DEFAULT-FRAME /* Printer - IN */
DO:
  RUN clockIn (TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoc WINDOW-1
ON CHOOSE OF btnLoc IN FRAME DEFAULT-FRAME /* Open File Location */
DO:
  DEFINE VARIABLE iSubDir  AS CHAR NO-UNDO.
  DEFINE VARIABLE iArtFile AS CHAR NO-UNDO.
  IF det:NUM-SELECTED-ROWS > 0 THEN DO i = 1 TO det:NUM-SELECTED-ROWS:
      det:FETCH-SELECTED-ROW(i).

      IF AVAIL(fdet) AND R-INDEX(fdet.fFile,"\") > 0 THEN DO:
      
        iArtFile = "". iSubDir = "".
        IF NUM-ENTRIES(fdet.fFile,",") > 1 THEN 
           ASSIGN iArtFile = ENTRY(1,fdet.fFile,",").
        ELSE 
           ASSIGN iArtFile = fdet.fFile.
    
        ASSIGN iSubdir = SUBSTR(iArtFile,1,R-INDEX(iArtFile,"\")).    
        IF iSubdir > "" THEN RUN IExplore.p(iSubDir).
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMaterial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMaterial WINDOW-1
ON CHOOSE OF btnMaterial IN FRAME DEFAULT-FRAME /* Issue Material */
DO:
  FIND tDet WHERE RECID(tDet) = find-recid NO-ERROR.
  IF AVAILABLE tDet THEN DO:
      IF tdet.tseq MODULO 2 <> 0 THEN DO:
          RUN mm-msg.w("Unable to issue material to template!",0,YES,1,OUTPUT qtyran, OUTPUT answer).
          RETURN.
      END.

     IF tdet.materialPosted 
          THEN RUN mm-msg.w("Are you sure you want to unissue the material?",1,YES,1,OUTPUT qtyran, OUTPUT answer).
          ELSE RUN mm-msg.w("Are you sure you want to issue the material to the job?",1,YES,1,OUTPUT qtyran, OUTPUT answer).

     issueIt = answer.

     IF issueIt THEN DO:
      
          RUN mm-material-issue.p (int(tDet.tbatch),tDet.materialposted).
    
          ASSIGN tDet.materialposted = NOT tDet.materialposted.
    
          APPLY "VALUE-CHANGED":U TO BROWSE hdr.
          RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext WINDOW-1
ON CHOOSE OF btnNext IN FRAME DEFAULT-FRAME /* > */
DO:
    DEFINE VARIABLE cFile   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE nFile   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse AS CHARACTER NO-UNDO.
    DEFINE VARIABLE mgmsg_handle AS HANDLE    NO-UNDO.
    DEFINE VARIABLE tmpFile AS CHAR NO-UNDO.
    
    DEFINE VARIABLE art AS ArtGenerator.
    art = NEW ArtGenerator().

    IF tDet.tSides <> "D/F" THEN DO:
        Message "This batch is only one sided" view-as alert-box.
        RETURN.    
    END.    
    
    tmpFile = "\\lowen\dfssancluster\Bullseye\images\agentphotos\Temporary\mtl1\" + tDet.tMat + "\POP 55\batch" + tDet.tBatch + "_" + STRING(tDet.tBed) + "_2.pdf". 
    
    IF INDEX(tDet.tMat,"Corex") > 0 OR INDEX(tDet.tMat,"Poly") > 0 THEN DO:
        tmpFile = "\\lowen\dfssancluster\Bullseye\images\agentphotos\Temporary\mtl1\" + tDet.tMat + "\POP 55\batch" + tDet.tBatch + "_2.pdf".
        
        IF SEARCH(tmpFile) = ? THEN tmpFile = "\\lowen\dfssancluster\Bullseye\images\agentphotos\Temporary\mtl1\" + tDet.tMat + "\POP 55\batch" + tDet.tBatch + "_" + tDet.tBed + "_2.pdf".
    END.
    
    /*Check for a white ink file - else create one*/
    IF tDet.WhiteInk = YES THEN DO:
        IF SEARCH(REPLACE(tmpFile,"_2.pdf","_2_white.pdf")) <> ? THEN tmpFile = REPLACE(tmpFile,"_2.pdf","_2_white.pdf").
        ELSE DO:
            RUN MGMSG.w PERSISTENT SET mgmsg_handle ("Generating White Ink Preview...").
            art:Preflight(tmpFile,REPLACE(tmpFile,"_2.pdf","_2_white.pdf"),"White Ink Preview and Color Check Test.kfpx").
            RUN CLOSE_me IN mgmsg_handle NO-ERROR.  
            tmpFile = REPLACE(tmpFile,"_2.pdf","_2_white.pdf").  
        END.    
    END.
    ELSE DO:
        /* Look for a "preview" file */
        IF SEARCH(REPLACE(tmpFile,".pdf","_preview.pdf")) <> ? THEN tmpFile = REPLACE(tmpFile,".pdf","_preview.pdf"). 
        ELSE IF SEARCH("\\fs02\bullseye\images\agentphotos\Temporary\mtl1\Templates\Production 95\Batch" + tDet.tBatch + "_1_template.pdf") <> ? THEN tmpFile = "\\fs02\bullseye\images\agentphotos\Temporary\mtl1\Templates\Production 95\Batch" + tDet.tBatch + "_1_template.pdf".
        

    END.
    
    IF SEARCH(tmpFile) <> ? THEN DO:
        ASSIGN chWebBrowser :VISIBLE = TRUE
               FRAME frame-b:HIDDEN  = TRUE
               FRAME frame-b:VISIBLE = FALSE.
        FRAME frame-b:MOVE-TO-BOTTOM().  
        chWebBrowser :WebBrowser:Navigate(tmpFile).    
    END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOut WINDOW-1
ON CHOOSE OF btnOut IN FRAME DEFAULT-FRAME /* Printer - OUT */
DO:
  RUN clockIn (FALSE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOverlay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOverlay WINDOW-1
ON CHOOSE OF btnOverlay IN FRAME DEFAULT-FRAME /* Show Overlay */
DO:
  IF cOverLay <> "" THEN DO:
      ASSIGN cOverLay = REPLACE(cOverLay,".pdf","_overlay.pdf").
      IF SEARCH(cOverLay) <> ? THEN DO:
          ASSIGN webBrowser:VISIBLE = TRUE
                 btnPDF    :VISIBLE = TRUE.
          SESSION:SET-WAIT-STATE("General").
          chWebBrowser:WebBrowser:Navigate(cOverlay).
          SESSION:SET-WAIT-STATE("").
        
      END.
      ELSE DO:
          ASSIGN webBrowser:HIDDEN = TRUE
                 btnPDF    :HIDDEN = TRUE.
      END.
  END.
  ELSE MESSAGE "Overlay not available" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPdf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPdf WINDOW-1
ON CHOOSE OF btnPdf IN FRAME DEFAULT-FRAME /* Open PDF */
DO:
  IF AVAIL(tdet) AND R-INDEX(tdet.tfile,"\") > 0 THEN DO:
        OS-COMMAND SILENT VALUE("start " + CHR(34) + CHR(34) + " " + chr(34) + replace(tdet.tfile,".jpg",".pdf") + CHR(34)).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrev WINDOW-1
ON CHOOSE OF btnPrev IN FRAME DEFAULT-FRAME /* < */
DO:
  DEFINE VARIABLE nFile        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cResponse    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE mgmsg_handle AS HANDLE    NO-UNDO.
  
  DEFINE VARIABLE art AS ArtGenerator.
  art = NEW ArtGenerator().
  
  IF SEARCH(REPLACE(tDet.tFile,".pdf","_preview.pdf")) <> ? THEN tDet.tFile = REPLACE(tDet.tFile,".pdf","_preview.pdf").
  
  IF SEARCH(tDet.tFile) = ? AND SEARCH("\\fs02\bullseye\images\agentphotos\Temporary\mtl1\Templates\Production 95\Batch" + tDet.tBatch + "_1_template.pdf") <> ? THEN DO:
      tDet.tFile = "\\fs02\bullseye\images\agentphotos\Temporary\mtl1\Templates\Production 95\Batch" + tDet.tBatch + "_1_template.pdf".
  END.
  
  ASSIGN tdet.tFile = REPLACE(tDet.tFile,"_White.pdf",".pdf")
         tdet.tFile = REPLACE(tdet.tFile,"_2.pdf","_1.pdf").

  IF AVAIL tdet AND tdet.tFile <> "" THEN DO:

     IF tdet.WhiteInk = YES AND thisMachine = 1 THEN DO:      
        IF SEARCH(REPLACE(tdet.tFile,".pdf","_white.pdf")) = ? THEN DO:
            RUN MGMSG.w PERSISTENT SET mgmsg_handle ("Generating White Ink Preview...").
            art:Preflight(tdet.tFile,REPLACE(tdet.tFile,".pdf","_white.pdf"),"White Ink Preview and Color Check Test.kfpx").
            RUN CLOSE_me IN mgmsg_handle NO-ERROR.
        END.
        
        tdet.tFile = REPLACE(tdet.tFile,".pdf","_white.pdf").
     END.
      
        
     chWebBrowser :VISIBLE = TRUE.
     FRAME frame-b:HIDDEN  = TRUE.
     FRAME frame-b:VISIBLE = FALSE.
     FRAME frame-b:MOVE-TO-BOTTOM().  
     chWebBrowser :WebBrowser:Navigate(tdet.tFile).

   
  END.
  ELSE DO:
      ASSIGN webBrowser:HIDDEN = TRUE
             btnPDF    :HIDDEN = TRUE.
  END.
  otherside = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPurge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPurge WINDOW-1
ON CHOOSE OF btnPurge IN FRAME DEFAULT-FRAME /* Purge */
DO:
    DEFINE VARIABLE goAhead AS LOG NO-UNDO INITIAL NO.
/*     MESSAGE "WARNING!...This will delete all unprinted beds and rebuild batches. Continue?" VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE goAhead. */
    RUN mm-msg.w("WARNING!...This will delete all unprinted beds and rebuild batches. Continue?",1,YES,1,OUTPUT qtyran, OUTPUT answer).
    goAhead = answer.
    IF goAhead THEN DO:
        APPLY "Value-Changed":U TO tgl_show.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefresh WINDOW-1
ON CHOOSE OF btnRefresh IN FRAME DEFAULT-FRAME /* Refresh Data */
DO:
  
  ASSIGN tmpbatch = ""
         tgl_show:PRIVATE-DATA = STRING(tgl_show:CHECKED IN FRAME {&FRAME-NAME}).
  APPLY "Value-Changed":U TO tgl_show.

 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset WINDOW-1
ON CHOOSE OF btnReset IN FRAME DEFAULT-FRAME /* Reset Batches */
DO:
    DEFINE VARIABLE goAhead AS LOG NO-UNDO INITIAL NO.
    RUN mm-msg.w("WARNING!...This will delete all unprinted beds and rebuild batches. Continue?",1,YES,1,OUTPUT qtyran, OUTPUT answer).
    goAhead = answer.
    IF goAhead THEN DO:
        APPLY "Value-Changed":U TO tgl_show.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME btnSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSend WINDOW-1
ON CHOOSE OF btnSend IN FRAME FRAME-A /* Send */
DO:
  DEFINE VARIABLE emailto   AS CHAR NO-UNDO.
  DEFINE VARIABLE emailcc   AS CHAR NO-UNDO.
  DEFINE VARIABLE sendto    AS CHAR NO-UNDO.
  DEFINE VARIABLE SENDcc    AS CHAR NO-UNDO.
  DEFINE VARIABLE dets      AS CHAR NO-UNDO.
  DEFINE VARIABLE images    AS CHAR NO-UNDO.
  DEFINE VARIABLE detimg    AS CHAR NO-UNDO.
  DEFINE VARIABLE delStatus AS CHAR NO-UNDO.
  DEFINE VARIABLE emailimg  AS CHAR NO-UNDO.
  DEFINE VARIABLE tLoop     AS INT  NO-UNDO.
  DEFINE VARIABLE cResponse AS CHAR NO-UNDO.
  
  /*Prime Center Variables*/
  DEFINE VARIABLE xmlData   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE tQty      AS INT       NO-UNDO.
  DEFINE VARIABLE cRecipe   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cItemseq  AS INT       NO-UNDO.
  DEFINE VARIABLE cALS      AS INT       NO-UNDO.
  DEFINE VARIABLE cRunseq   AS INT       NO-UNDO.
  DEFINE VARIABLE cBatch    AS INT       NO-UNDO.
  DEFINE VARIABLE j         AS INT       NO-UNDO.

  DEFINE VARIABLE BatchList  AS "System.Collections.Generic.List<character>" NO-UNDO.
  BatchList = NEW "System.Collections.Generic.List<character>"().

  ASSIGN sendto = cTo:SCREEN-VALUE IN FRAME frame-a
         SENDcc = cCC:SCREEN-VALUE IN FRAME frame-a.

  RUN LogStart("Start btnSend").
  IF cMessage:SCREEN-VALUE = "" THEN DO: 
      RUN mm-msg.w("Please fill in email body!",0,YES,1,OUTPUT qtyran, OUTPUT answer).
      RETURN.
  END.
  
  ASSIGN dets     = ""
         detimg   = ""
         emailimg = "".
  
  IF det:NUM-SELECTED-ROWS IN FRAME default-frame > 0 THEN DO:
      IF INDEX(tdet.tMat,"Corex") > 0 AND tdet.tbedseq = "0" AND tdet.tbed = "Mixed" THEN DO:
          
          cBatch = INT(tDet.tBatch).          
          
          DO i = 1 TO det:NUM-SELECTED-ROWS IN FRAME default-frame:
              det:FETCH-SELECTED-ROW(i).
              FOR EACH so_items WHERE so_items.so_no = fdet.fSo AND so_items.item_no = INT(fdet.fItemNo):
                  ASSIGN so_items.ord_status = 15
                         so_items.note = cMessage:SCREEN-VALUE IN FRAME frame-a.
                  RUN oestchg.p(so_items.so_no).
              END. 
              
              ASSIGN cItemseq = fDet.fItemseq
                     cALS     = fArtLinkSeq.
              
              FOR EACH ttArt:
                  DELETE ttArt.
              END.
               
              DEFINE VARIABLE tempQty AS INT.      
              FOR EACH sign_mm_det WHERE sign_mm_det.batchseq = INT(tDet.tBatch) BREAK BY sign_mm_det.itemseq:                  
                  IF sign_mm_det.itemseq = cItemseq AND sign_mm_det.artlinkseq = cALS THEN DO:
                      DELETE sign_mm_det.   
                      NEXT.
                  END.
                  
                  IF LAST-OF(sign_mm_det.itemseq) THEN DO:
                      
                      FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = sign_mm_det.itemseq NO-ERROR.
                      IF AVAIL so_items THEN RUN BuildTT IN mm-pp-handle (so_items.so_no,so_items.ITEM_no).
                      RELEASE so_items.
                      DELETE sign_mm_det NO-ERROR.
                  END.
                  ELSE DO:
                      DELETE sign_mm_det NO-ERROR.  
                  END. 
              END.
              
              FIND sign_mm_hdr WHERE sign_mm_hdr.batchseq = INT(tDet.tBatch) NO-ERROR.
              IF AVAIL sign_mm_hdr THEN DO:
                  cRunseq = sign_mm_hdr.runseq.
                  
                  FIND zz_file WHERE zz_file.zz_key1 = "mm-delete" AND zz_file.zz_key2 = "batch" + STRING(sign_mm_hdr.batchseq) NO-ERROR.
                  IF NOT AVAIL zz_file THEN DO:
                        CREATE zz_file.
                        ASSIGN zz_file.zz_key1 = "mm-delete"
                               zz_file.zz_key2 = "batch" + STRING(sign_mm_hdr.batchseq)
                               zz_file.zz_char[1] = "mm-look.w"
                               zz_file.zz_char[2] = "btnSend"
                               zz_file.zz_char[3] = string(today)
                               zz_file.zz_char[4] = STRING(TIME,"HH:MM:SS")
                               zz_file.zz_char[6] = "Live".
                    
                  END.
                  RELEASE zz_file.
                                  
                  DELETE sign_mm_hdr.
              END.     
    
          END.
          
          FOR EACH ttArt:
              FIND FIRST fdet NO-LOCK WHERE fdet.fitemseq = ttArt.ttitemseq AND fdet.fBatch = tdet.tBatch NO-ERROR. 
              ASSIGN ttArt.ttFile = fDet.fFile
                     ttArt.ttQty  = INT(fdet.fOnBed).                                       
          END.
          
          RUN DelCorexPC    IN mm-pp-handle ("").
          RUN GroupingCorex IN mm-pp-handle ("").
          
          FIND LAST sign_mm_hdr WHERE sign_mm_hdr.zzchar_1 = "PrimeCenter" NO-ERROR.
          IF AVAIL sign_mm_hdr THEN sign_mm_hdr.runseq = cRunseq.
          RELEASE sign_mm_hdr.
           
          
          ASSIGN c_msg         = cMessage:SCREEN-VALUE IN FRAME frame-a + CHR(10) + CHR(10) + "These orders were removed from the JM Queue" 
                 c_subject     = "Batch " + string(tdet.tbatch) + " was edited in Job Manager"
                 c_to_addr     = sendto  /* emailto */
                 c_cc_addr     = sendcc /* emailcc  */
                 c_attachments = "".

          RUN mgemail.p ("Bullseye Database",c_to_addr,c_cc_addr,c_bcc_addr,c_subject,c_msg,c_attachments,FALSE).
           
          APPLY "choose":U TO btnCancel.
          
          /*Delete old batch file*/
          OS-DELETE VALUE("\\fs02\bullseye\images\agentphotos\Temporary\mtl1\" + tdet.tMat + "\POP 55\Batch" + STRING(cBatch) + "_1.pdf").
         
          APPLY "choose":U TO btnRefresh.
          
          LEAVE.      
      END. 
  END.
  

  IF det:NUM-SELECTED-ROWS IN FRAME default-frame > 0 THEN DO i = 1 TO det:NUM-SELECTED-ROWS IN FRAME default-frame:
    det:FETCH-SELECTED-ROW(i).
      FOR EACH so_items WHERE so_items.so_no = fdet.fSo AND so_items.ITEM_no = int(fdet.fItemNo):
          ASSIGN so_items.ord_status = 15
                 so_items.note       = cMessage:SCREEN-VALUE IN FRAME frame-a.
          
          RUN oestchg.p(so_items.so_no).
          
          dets = dets + (IF dets = "" THEN "" ELSE ",") + STRING(so_items.itemseq) + "-" + STRING(fdet.fartlinkseq).
          RUN deleteMMcs6.p (so_items.itemseq,"",OUTPUT images, OUTPUT delStatus).
          
          IF NUM-ENTRIES(images) > 0 THEN DO:
              DO tLoop = 1 TO NUM-ENTRIES(images):
                  IF NOT CAN-DO(detimg,ENTRY(tLoop,images)) THEN DO:
                      detimg = detimg + (IF detimg = "" THEN "" ELSE ",") + entry(tLoop,images).
                  END.
              END.
          END.
          
      END.
  END.

  /*move to session dir so and then attach those so that email prog doesn't lock the files we want to delete*/
  DO iLoop = 1 TO NUM-ENTRIES(detimg):
    OS-COPY VALUE(ENTRY(iLoop,detimg)) VALUE(SESSION:TEMP-DIR + entry(NUM-ENTRIES(ENTRY(iLoop,detimg),"\"),ENTRY(iLoop,detimg),"\")).
    emailimg = emailimg + (IF emailimg = "" THEN "" ELSE ",") + SESSION:TEMP-DIR + entry(NUM-ENTRIES(ENTRY(iLoop,detimg),"\"),ENTRY(iLoop,detimg),"\").
  END.

  ASSIGN c_msg         = cMessage:SCREEN-VALUE IN FRAME frame-a + CHR(10) + CHR(10) + "These orders were removed from the JM Queue" 
         c_subject     = "Batch " + string(tdet.tbatch) + " was removed from Job Manager"
         c_to_addr     = sendto /* emailto */
         c_cc_addr     = sendcc /* emailcc  */
         c_attachments = emailimg.

  RUN mgemail.p ("Bullseye Database",c_to_addr,c_cc_addr,c_bcc_addr,c_subject,c_msg,c_attachments,FALSE).


  DO iLoop = 1 TO NUM-ENTRIES(dets):
      RUN wipeSlate(ENTRY(iloop,dets)).
  END.


  SESSION:SET-WAIT-STATE("general").
  FOR EACH tbatches:       
        FIND FIRST tDet NO-LOCK WHERE tDet.tBatch = STRING(tBatches.batchseq) NO-ERROR.
        IF NOT AVAIL tDet THEN NEXT.
        RUN LogStart(STRING(tBatches.batchseq) + " " + "tCutFile = " + tdet.tCutfile).
        
        IF tDet.tCutFile = "" THEN DO:
            
            /*delete and regen the .png file*/
            IF SEARCH(tDet.tFile) <> ? THEN DO:
                chWebBrowser :WebBrowser:Navigate("\\qbtest\bullseye\images\blank.bmp").
                PAUSE 5 NO-MESSAGE.
                 
                IF INDEX(tDet.tFile,"_preview") > 0 THEN tDet.tFile = REPLACE(tDet.tFile,"_preview","").
                
                OS-DELETE VALUE(tDet.tFile).
                RUN LogStart("Error Deleting Batch File (" + STRING(OS-ERROR) + ") - " + tdet.tFile).
                
                /*OS-COMMAND SILENT VALUE().*/
                OS-DELETE VALUE(REPLACE(tDet.tFile,".pdf","_old.pdf")).
                RUN LogStart("Error Deleting Batch File (" + STRING(OS-ERROR) + ") - " + REPLACE(tdet.tFile,".pdf","_old.pdf")).
                
                OS-DELETE VALUE(REPLACE(tDet.tFile,".pdf","_preview.pdf")).
                RUN LogStart("Error Deleting Batch File (" + STRING(OS-ERROR) + ") - " + REPLACE(tdet.tFile,".pdf","_preview.pdf")).
                
                OS-DELETE VALUE(REPLACE(tdet.tFile,".pdf",".png")).
                RUN LogStart("Error Deleting Batch File (" + STRING(OS-ERROR) + ") - " + REPLACE(tdet.tFile,".pdf",".png")).
                
            END.
            
            /*JM Gang and genart*/
            FOR EACH buff_mm_hdr WHERE buff_mm_hdr.batchseq = int(tbatches.batchseq) AND buff_mm_hdr.RUN_time = ? BY buff_mm_hdr.runseq:
                ASSIGN buff_mm_hdr.rerun = YES.
            END.
            RUN genxml IN mm-pp-handle ("notblank").
            
            PAUSE 90 NO-MESSAGE.
            APPLY "Choose" TO btnPrev.
            
        END.
  END.
  SESSION:SET-WAIT-STATE("").
  APPLY "choose":U TO btnCancel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnStartRIP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartRIP WINDOW-1
ON CHOOSE OF btnStartRIP IN FRAME DEFAULT-FRAME /* Start RIP Queue */
DO:
    /*DEFINE VARIABLE lcnt AS INT  NO-UNDO INITIAL 0.
    IF thisMachine = 1 THEN DO:
        FOR EACH ttDet NO-LOCK BY ttDet.tseq:
            IF NOT CAN-FIND(FIRST zz_file WHERE zz_file.zz_key1 = "sgBatch-RIP" AND zz_file.zz_key2 = ttDet.tBatch) THEN DO:
                CREATE zz_file.
                ASSIGN zz_file.zz_key1 = "sgBatch-RIP"
                       zz_file.zz_key2 = ttDet.tBatch
                       zz_file.zz_char[1] = ttDet.tBed
                       zz_file.zz_char[2] = ttDet.tMat
                       zz_file.zz_char[3] = STRING(ttDet.tBedSeq) 
                       zz_file.zz_char[4] = STRING(ttDet.tQty)
                       zz_file.zz_char[5] = ttDet.tSides
                       zz_file.zz_date[1] = TODAY
                       zz_file.zz_dec[1]  = ttDet.tSeq.
                
                IF SEARCH("\\lowen\dfssancluster\Bullseye\Logs\JDF\Digitech\Printer\batch" + ttDet.tBatch + "_" + ttDet.tBed + "_1.xml") <> ? 
                OR SEARCH("\\lowen\dfssancluster\Bullseye\Logs\JDF\Digitech\Printer\batch" + ttDet.tBatch + "_1.xml") <> ? THEN zz_file.zz_log[3] = YES.
                RELEASE zz_file.
            END.
            lcnt = lcnt + 1.
            IF lcnt = 5 THEN DO:
                /*update JDFBrowse*/
                {&OPEN-QUERY-JDFQueue}
                JDFQueue:REFRESH() IN FRAME DEFAULT-FRAME NO-ERROR.    
                LEAVE.
            END.
        END.
    END.*/              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnViewCAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewCAD WINDOW-1
ON CHOOSE OF btnViewCAD IN FRAME DEFAULT-FRAME /* View CAD */
DO:
    DEF VAR ImageName AS CHAR NO-UNDO.
    DEF VAR iOutput AS CHAR NO-UNDO.

    IF AVAIL fDet THEN DO:
        FIND partfile NO-LOCK WHERE partfile.part_no = fDet.fInvPart NO-ERROR.
        ImageName = partfile.cad_file.

        IF ImageName = "" AND partfile.img_file <> "" THEN
            ASSIGN imageName = "\\lowen\dfssancluster\bullseye\images\eData\" + partfile.img_file.

        IF SEARCH(imageName) <> ? THEN RUN shellex.p (ImageName, OUTPUT iOutput).
        ELSE MESSAGE "No CAD file available..." VIEW-AS ALERT-BOX MESSAGE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_start WINDOW-1
ON CHOOSE OF btn_start IN FRAME DEFAULT-FRAME /* Start Batch */
DO:
    
    DEFINE VARIABLE onHold       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE tmp          AS CHAR    NO-UNDO.
    DEFINE VARIABLE vtype        AS CHAR    NO-UNDO.
    DEFINE VARIABLE chkrp        AS LOGICAL NO-UNDO.
    DEFINE VARIABLE jdfOK        AS LOGICAL NO-UNDO.
    DEFINE VARIABLE templateFile AS CHAR    NO-UNDO.
    
    chkrp = NO.
    
    
    RUN logStart ("btn_start Batch: " + tDet.tBatch:SCREEN-VALUE IN BROWSE hdr + " : " + tDet.tMat).  
    
    FIND FIRST tDet WHERE RECID(tDet) = find-recid NO-ERROR.
    IF AVAILABLE tdet AND tDet.tMat = "Template" AND tDet.tBed = "Mixed" THEN DO:
        templateFile = "\\CALDERA-KEY\Public\Substrate Hotfolders\CadFiles\batch" + tDet.tBatch + "_1_template.pdf".
        IF SEARCH(templateFile) <> ? THEN ASSIGN tDet.tFile = templateFile.
        
        templateFile = "\\fs02\bullseye\images\agentphotos\Temporary\mtl1\" + tDet.zzchar_2 + "\batch" + tDet.tBatch + "_1_template.pdf".
        IF SEARCH(templateFile) <> ? THEN ASSIGN tDet.tFile = templateFile.
    END.
    RELEASE tDet.  

    accessUsers = accessUsers + "," + current-user-id.
    IF LOOKUP(current-user-id,accessUsers) > 0 THEN DO:
       
       FIND FIRST tDet WHERE RECID(tDet) = find-recid NO-ERROR.
       IF AVAILABLE tdet THEN DO:
          RUN logStart ("btn_start 1"). 
          
          IF NOT CAN-FIND(FIRST eDet) THEN DO:
              RUN mm-msg.w("No one is logged into the Printers right now.",0,YES,1,OUTPUT qtyran, OUTPUT answer).
              RETURN NO-APPLY.
          END.

          IF tDet.tMat = "Template" AND tDet.tBed = "0" THEN
              ASSIGN tDet.tFile = "\\CALDERA-KEY\Public\Substrate Hotfolders\CadFiles\batch" + tDet.tBatch + "_1_template.pdf".

          
          FIND pt_hotfolder NO-LOCK WHERE pt_hotfolder.pt_hotfolderseq = tDet.hotfolderseq NO-ERROR.
          IF AVAIL pt_hotfolder THEN tmp = pt_hotfolder.quality.
          RELEASE pt_hotfolder.

          
          /*Gateway test logic*/
          RUN gettype(tDet.tMat, OUTPUT vtype).
          IF vtype <> "" THEN DO:
               FIND FIRST fbGateways NO-LOCK WHERE fbGateways.fbGWDate = TODAY AND fbGateways.fbgwresult = TRUE AND vType = fbGateways.fbgwSubstrate AND fbGateways.fbgwSpeed = tmp AND fbGateways.fbgwMachine = thismachine NO-ERROR.
               IF NOT AVAIL fbGateways THEN DO:
                   RUN logStart ("btn_start 3 - RUN GatewayTest.w").
                   RUN gatewaytest.w(vType, tDet.hotfolderseq, thismachine, INPUT TABLE eDet).
                   RETURN.
               END.
          END.        

          RUN logStart ("btn_start 4 - RUN HoldCheck").
          RUN holdCheck(tdet.tbatch, OUTPUT onHold).
          RUN logStart ("btn_start 5 - Post HoldCheck").
          
          IF onHold THEN DO:
              RUN mm-msg.w("One of the items in this batch is on hold...",0,YES,1,OUTPUT qtyran, OUTPUT answer).
              RUN logStart ("btn_start 6 - Item was on hold, returning...").
              RETURN NO-APPLY.
          END.
          
          FOR EACH fdet NO-LOCK WHERE fdet.fbatch = tdet.tbatch:
              ASSIGN tmpQty = 0
                     tmpint = 0.
                     
              FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = fdet.fitemseq NO-ERROR.
              
              FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.itemseq = so_items.itemseq BREAK BY sign_mm_det.batchseq:
                  tmpint = tmpint + 1.
                  
                  FIND FIRST sign_mm_reprint NO-LOCK WHERE sign_mm_reprint.itemseq = sign_mm_det.itemseq NO-ERROR.
                  IF AVAIL sign_mm_reprint THEN chkrp = YES.
                  
                  IF LAST-OF(sign_mm_det.batchseq) THEN DO:
                      FIND sign_mm_hdr NO-LOCK OF sign_mm_det NO-ERROR.
                      IF AVAIL sign_mm_hdr THEN DO:
                          ASSIGN tmpQty    =  tmpQty + (tmpint * sign_mm_hdr.qty_printed)
                                 tmpInt    = 0
                                 doReprint = sign_mm_hdr.reprint.
                                 
                          IF chkrp = YES THEN doreprint = YES.
                      END.
                      RELEASE sign_mm_hdr.
                  END.
                  
              END.

              FIND pt_det NO-LOCK WHERE pt_det.part_no = so_items.part_no NO-ERROR.
              IF AVAIL pt_det AND pt_det.steeltent THEN tmpQty = IF tmpQty / 2 <> int(tmpqty / 2) THEN (tmpQty / 2) - 1 ELSE tmpQty / 2.
              IF doReprint = NO THEN DO:
                  IF (AVAIL so_items AND tmpQty > so_items.orderqty) OR
                     (CAN-FIND(FIRST h_detail NO-LOCK WHERE h_detail.order_no = fdet.fso 
                                                        AND h_detail.ITEM_no  = string(fdet.fITEMno) 
                                                        AND h_detail.artDispOrder = fdet.fArtLinkSeq 
                                                        AND h_detail.activity = "D11" 
                                                        AND h_detail.batchseq = "")) THEN DO:

                         RUN mm-msg.w(STRING("WARNING!..This batch cannot be ran! " + "Items related to order: " + fdet.fSo + " item: " + fdet.fItemno  + " have already been ran." + "Item must be removed before starting batch."),0,YES,1,OUTPUT qtyran, OUTPUT answer).
                         RETURN NO-APPLY.
                  END.
              END.
          END.

          RUN logStart ("btn_start 7 - file: " + tdet.tfile + " SEARCH: " + SEARCH(tDet.tFile)).
          IF SEARCH(tdet.tfile) <> ? THEN DO:
              ASSIGN zBatch = tDet.tBatch
                     zMat   = tDet.tMat.
                                               
              RUN logStart ("btn_start 8 - RUN Batch_Details").
              RUN batch_details("Start",zBatch). /*assigns the sign_mm_hdr record as started  */
              RUN logStart ("btn_start 9 - END Batch_Details").

              RUN logStart ("btn_start 10 - RUN currentBatch").
              RUN currentBatch.
              RUN logStart ("btn_start 11 - END currentBatch").

              RUN logStart ("btn_start 12 - RUN laborUpdate").
              RUN laborUpdate ("*",zBatch).
              RUN logStart ("btn_start 13 - END laborUpdate").

            
              IF NOT CAN-FIND(FIRST zz_file WHERE zz_file.zz_key1 = "MM-SentToRip" AND zz_file.zz_key2 = zbatch) THEN DO:
                  RUN logStart ("btn_start 14 - RUN movePDF").             
                  RUN movePDF(zBatch,zMat,"btn_start").     /*moves from our folders to bart's hotfolders*/
                  RUN logStart ("btn_start 15 - END movePDF").             
              END.


              /*sends then next beds/templates to hotfolders*/
              IF VALID-HANDLE(mm2handle) THEN DO:
                  FIND tt_det NO-LOCK WHERE tt_det.tbatch = zbatch NO-ERROR.
                  IF AVAIL tt_det THEN nextRunseq = tt_det.tseq.
                 
                
                RUN logStart ("btn_start 16 - RUN PlugAndChug in mm-look2").
                RUN PlugAndChug IN mm2Handle(tt_det.tBatch,tt_det.tFile).
                RUN logStart ("btn_start 17 - END PlugAndChug in mm-look2").
                
              END.
              
              IF VALID-HANDLE (mm2Handle) THEN DO:
                  RUN logStart ("btn_start 18 - RUN RunTime in mm-look2").
                  RUN RunTime IN mm2Handle(TIME).
                  RUN logStart ("btn_start 19 - END RunTime in mm-look2").
              END.
              tmpbatch = zbatch.
              
              RUN logStart ("btn_start 20 - Value Changed tgl_show").
              APPLY "VALUE-CHANGED":U TO tgl_show IN FRAME {&FRAME-NAME}.
              RUN logStart ("btn_start 21 - END Value Changed tgl_show").              
              
          END.
          ELSE RUN mm-msg.w(STRING("Can not start batch " + tdet.tbatch + "." + " Job Manager has not finished creating related PDF's!"),0,YES,1,OUTPUT qtyran, OUTPUT answer).
      END.
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_stop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_stop WINDOW-1
ON CHOOSE OF btn_stop IN FRAME DEFAULT-FRAME /* Stop Batch */
DO:
  DEFINE VARIABLE corexCnt AS INT NO-UNDO.  
  FIND tDet WHERE RECID(tDet) = find-recid NO-ERROR.
  IF AVAILABLE tdet THEN RUN logStart ("btn_stop 1 - Batch: " + tDet.tBatch:SCREEN-VALUE IN BROWSE hdr).
  
 
  IF CAN-DO(accessUsers,current-user-id) THEN DO:
        RUN logStart ("btn_stop 2").
        RUN currentBatch.
        RUN logStart ("btn_stop 3").
        
        IF currentBatch > "" THEN DO:
            RUN logStart ("btn_stop 4").
            RUN laborUpdate ("*","").
            RUN logStart ("btn_stop 5").
            IF VALID-HANDLE(mm2handle) THEN DO:
                RUN logStart ("btn_stop 6").
                RUN runTime IN mm2handle(0).
                RUN logStart ("btn_stop 7").
            END.
            RUN logStart ("btn_stop 8").
            APPLY "VALUE-CHANGED":U TO tgl_show IN FRAME {&FRAME-NAME}.
            RUN logStart ("btn_stop 9").
        END.
        ELSE RUN mm-msg.w("There are no current batches running.",0,YES,1,OUTPUT qtyran, OUTPUT answer).
    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME chkDigital1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL chkDigital1 WINDOW-1
ON VALUE-CHANGED OF chkDigital1 IN FRAME DEFAULT-FRAME /* D1 */
DO:
    IF chkDigital1
    THEN RUN mm-msg.w("Please confirm that you want to take Digital 1 offline."  ,1,YES,1,OUTPUT qtyran, OUTPUT answer).
    ELSE RUN mm-msg.w("Please confirm that you want to reactivate Digital 1.",1,YES,1,OUTPUT qtyran, OUTPUT answer).

  IF answer THEN DO:
      FIND zz_file WHERE zz_file.zz_key1 = "Digital" AND zz_file.zz_key2 = "1" NO-ERROR.
      IF AVAILABLE zz_file THEN ASSIGN zz_file.zz_log[1] = NOT zz_file.zz_log[1]
                                       chkDigital1.
      RELEASE zz_file.
      SESSION:SET-WAIT-STATE("GENERAL").
      RUN mm-batch-order.p (mm-pp-handle).
      APPLY "CHOOSE":U TO btnRefresh IN FRAME {&FRAME-NAME}.
      SESSION:SET-WAIT-STATE("").
  END.
  ELSE DO:
      DISPLAY chkDigital1 WITH FRAME {&FRAME-NAME}. /* puts it back as it was */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME chkDigital2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL chkDigital2 WINDOW-1
ON VALUE-CHANGED OF chkDigital2 IN FRAME DEFAULT-FRAME /* D2 */
DO:
  IF chkDigital2
  THEN RUN mm-msg.w("Please confirm that you want to take Digital 2 offline."  ,1,YES,1,OUTPUT qtyran, OUTPUT answer).
  ELSE RUN mm-msg.w("Please confirm that you want to reactivate Digital 2.",1,YES,1,OUTPUT qtyran, OUTPUT answer).

  IF answer THEN DO:
      FIND zz_file WHERE zz_file.zz_key1 = "Digital" AND zz_file.zz_key2 = "2" NO-ERROR.
      IF AVAILABLE zz_file THEN ASSIGN zz_file.zz_log[1] = NOT zz_file.zz_log[1]
                                       chkDigital2.
      RELEASE zz_file.
      
      SESSION:SET-WAIT-STATE("GENERAL").
      RUN mm-batch-order.p (mm-pp-handle).
      APPLY "CHOOSE":U TO btnRefresh IN FRAME {&FRAME-NAME}.
      SESSION:SET-WAIT-STATE("").
  END.
  ELSE DO:
      DISPLAY chkDigital2 WITH FRAME {&FRAME-NAME}. /* puts it back as it was */
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME chkDigital3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL chkDigital3 WINDOW-1
ON VALUE-CHANGED OF chkDigital3 IN FRAME DEFAULT-FRAME /* D3 */
DO:
    IF chkDigital3
    THEN RUN mm-msg.w("Please confirm that you want to take Digital 3 offline."  ,1,YES,1,OUTPUT qtyran, OUTPUT answer).
    ELSE RUN mm-msg.w("Please confirm that you want to reactivate Digital 3.",1,YES,1,OUTPUT qtyran, OUTPUT answer).

    IF answer THEN DO:
          FIND zz_file WHERE zz_file.zz_key1 = "Digital" AND zz_file.zz_key2 = "3" NO-ERROR.
          IF AVAILABLE zz_file THEN ASSIGN zz_file.zz_log[1] = NOT zz_file.zz_log[1]
                                           chkDigital3.
          RELEASE zz_file.
          
          SESSION:SET-WAIT-STATE("GENERAL").
          RUN mm-batch-order.p (mm-pp-handle).
          APPLY "CHOOSE":U TO btnRefresh IN FRAME {&FRAME-NAME}.
          SESSION:SET-WAIT-STATE("").
    END.
    ELSE DO:
        DISPLAY chkDigital3 WITH FRAME {&FRAME-NAME}. /* puts it back as it was */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame WINDOW-1 OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    RUN empRefresh.

    /*update JDFBrowse*/
    {&OPEN-QUERY-JDFQueue}
    JDFQueue:REFRESH() IN FRAME DEFAULT-FRAME NO-ERROR.

    IF CAN-FIND(zz_file NO-LOCK WHERE zz_file.zz_key1 = "MediaManager" AND zz_file.zz_key2 = "MM-Running") THEN DO:
        ASSIGN tmpbatch = IF AVAIL tdet THEN tdet.tbatch ELSE ""
               tgl_show:PRIVATE-DATA IN FRAME {&FRAME-NAME} = STRING(tgl_show:CHECKED IN FRAME {&FRAME-NAME}).
          APPLY "Value-Changed":U TO tgl_show IN FRAME {&FRAME-NAME}.
        
    END.
    RUN zz_messageDisplay IN mm-pp-handle ("All",current-user-id,OUTPUT numMsgs,OUTPUT zzMsg).

    IF zzMsg <> "" THEN
        RUN mm-msg.w(zzMsg,0,YES,numMsgs,OUTPUT qtyran, OUTPUT answer).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME det
&Scoped-define SELF-NAME det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL det WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF det IN FRAME DEFAULT-FRAME
DO:
  DEF VAR secondparam AS CHAR NO-UNDO.

  IF AVAIL fdet THEN DO: 
      {run_prog.i ""signqu.w"" "(fdet.fSo,Actions)"}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL det WINDOW-1
ON ROW-DISPLAY OF det IN FRAME DEFAULT-FRAME
DO:
    IF fdet.fRack <> "" THEN DO:
        ASSIGN fPrinted   :BGCOLOR IN BROWSE det = 12
               fPrinted   :FGCOLOR IN BROWSE det = 15     .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME emp
&Scoped-define SELF-NAME emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL emp WINDOW-1
ON VALUE-CHANGED OF emp IN FRAME DEFAULT-FRAME /* Employees Logged In To Printer */
DO:
  ASSIGN find-recid3 = RECID(edet).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME HDR
&Scoped-define SELF-NAME HDR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL HDR WINDOW-1
ON MOUSE-MENU-DOWN OF HDR IN FRAME DEFAULT-FRAME
DO:
  DEF VAR ClickRow AS INTEGER NO-UNDO. 
    DEF VAR CellHeight AS INTEGER NO-UNDO. 
    DEF VAR MouseY AS INTEGER NO-UNDO. 
    
    ASSIGN    MouseY = LAST-EVENT:Y  
              CellHeight = FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(SELF:FONT) + 4. 
                /* Cell height is the height of the font + 4 pixels spacing */ 
    
    ClickRow  = TRUNC(MouseY / CellHeight , 0). 
    
    IF ClickRow < 1 THEN ClickRow = 1. 
    
    /* NO-ERROR to avoid selection of row out of scope of display window  */ 
    SELF:SELECT-ROW(ClickRow) NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL HDR WINDOW-1
ON MOUSE-SELECT-CLICK OF HDR IN FRAME DEFAULT-FRAME
DO:
  tmpBatch = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL HDR WINDOW-1
ON ROW-DISPLAY OF HDR IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE isLate     AS LOGICAL NO-UNDO.
  DEFINE VARIABLE totalDays  AS INTEGER NO-UNDO. 
  DEFINE VARIABLE startDate  AS DATE    NO-UNDO.
  DEFINE VARIABLE totalhours AS DECIMAL NO-UNDO.

  
  IF tdet.tRDate <> ? AND tdet.tRtime = ? 
  AND CAN-FIND(FIRST h_detail WHERE h_detail.order_no = "B-" + tDet.tBatch AND h_detail.activity = "D11")
  THEN DO: /*Green*/
      ASSIGN tdet.tbatch   :BGCOLOR IN BROWSE hdr = 10
             tdet.tcDate   :BGCOLOR IN BROWSE hdr = 10 
             tdet.tDone    :BGCOLOR IN BROWSE hdr = 10 
             tdet.tBed     :BGCOLOR IN BROWSE hdr = 10  
             tdet.tBin     :BGCOLOR IN BROWSE hdr = 10
             tdet.tQty     :BGCOLOR IN BROWSE hdr = 10
             tdet.tprinted :BGCOLOR IN BROWSE hdr = 10
             tdet.tSide    :BGCOLOR IN BROWSE hdr = 10
             tdet.tinv     :BGCOLOR IN BROWSE hdr = 10
             tdet.tMachTime:BGCOLOR IN BROWSE hdr = 10
             tdet.materialposted:BGCOLOR IN BROWSE hdr = 10.
  END.
  ELSE IF tdet.tseq MODULO 2 <> 0 THEN DO:              /*orange*/
      ASSIGN tdet.tbatch   :BGCOLOR IN BROWSE hdr = 22
             tdet.tcDate   :BGCOLOR IN BROWSE hdr = 22 
             tdet.tDone    :BGCOLOR IN BROWSE hdr = 22 
             tdet.tBed     :BGCOLOR IN BROWSE hdr = 22   
             tdet.tBin     :BGCOLOR IN BROWSE hdr = 22
             tdet.tQty     :BGCOLOR IN BROWSE hdr = 22
             tdet.tprinted :BGCOLOR IN BROWSE hdr = 22
             tdet.tSide    :BGCOLOR IN BROWSE hdr = 22
             tdet.tinv     :BGCOLOR IN BROWSE hdr = 22
             tdet.tMachTime:BGCOLOR IN BROWSE hdr = 22
             tdet.materialposted:BGCOLOR IN BROWSE hdr = 22.
  END.
  ELSE IF tdet.tDone <> ? AND tdet.tRtime <> ? THEN DO: /*yellow*/
      ASSIGN tdet.tbatch   :BGCOLOR IN BROWSE hdr = 14
             tdet.tcDate   :BGCOLOR IN BROWSE hdr = 14 
             tdet.tDone    :BGCOLOR IN BROWSE hdr = 14 
             tdet.tBed     :BGCOLOR IN BROWSE hdr = 14   
             tdet.tSide    :BGCOLOR IN BROWSE hdr = 14
             tdet.tBin     :BGCOLOR IN BROWSE hdr = 14
             tdet.tQty     :BGCOLOR IN BROWSE hdr = 14
             tdet.tprinted :BGCOLOR IN BROWSE hdr = 14
             tdet.tinv     :BGCOLOR IN BROWSE hdr = 14
             tdet.tMachTime:BGCOLOR IN BROWSE hdr = 14
             tdet.materialposted:BGCOLOR IN BROWSE hdr = 14.
  END.
  ELSE IF tdet.tfullbed = FALSE THEN DO:  
      ASSIGN tdet.tbatch   :BGCOLOR IN BROWSE hdr = 11   /*blue*/
             tdet.tcDate   :BGCOLOR IN BROWSE hdr = 11 
             tdet.tDone    :BGCOLOR IN BROWSE hdr = 11 
             tdet.tBed     :BGCOLOR IN BROWSE hdr = 11 
             tdet.tBin     :BGCOLOR IN BROWSE hdr = 11
             tdet.tQty     :BGCOLOR IN BROWSE hdr = 11
             tdet.tprinted :BGCOLOR IN BROWSE hdr = 11
             tdet.tSide    :BGCOLOR IN BROWSE hdr = 11
             tdet.tinv     :BGCOLOR IN BROWSE hdr = 11
             tdet.tMachTime:BGCOLOR IN BROWSE hdr = 11
             tdet.materialposted:BGCOLOR IN BROWSE hdr = 11.         
  END.
  
  FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.batchseq = INT(tDet.tBatch):
      FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = sign_mm_det.itemseq NO-ERROR.
      FIND FIRST so_file NO-LOCK WHERE so_file.so_no = so_items.so_no NO-ERROR.

      IF AVAIL so_file AND so_file.cust_no = "217559" THEN DO: /*Trade Source*/
          ASSIGN tdet.tbatch   :BGCOLOR IN BROWSE hdr = 29   /*purple*/
                 tdet.tcDate   :BGCOLOR IN BROWSE hdr = 29 
                 tdet.tDone    :BGCOLOR IN BROWSE hdr = 29 
                 tdet.tBed     :BGCOLOR IN BROWSE hdr = 29 
                 tdet.tBin     :BGCOLOR IN BROWSE hdr = 29
                 tdet.tQty     :BGCOLOR IN BROWSE hdr = 29
                 tdet.tprinted :BGCOLOR IN BROWSE hdr = 29
                 tdet.tSide    :BGCOLOR IN BROWSE hdr = 29
                 tdet.tinv     :BGCOLOR IN BROWSE hdr = 29
                 tdet.tMachTime:BGCOLOR IN BROWSE hdr = 29
                 tdet.materialposted:BGCOLOR IN BROWSE hdr = 29.
          LEAVE.
      END.
      
  END.


  IF SEARCH("\\lowen\dfssancluster\Bullseye\Logs\JDF\Digitech\Printer\batch" + tDet.tBatch + "_" + tDet.tBed + "_1.xml") <> ? 
  OR SEARCH("\\lowen\dfssancluster\Bullseye\Logs\JDF\Digitech\Printer\batch" + tDet.tBatch + "_1.xml") <> ? THEN DO:
      ASSIGN tDet.tBatch:BGCOLOR IN BROWSE hdr = 13.    
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL HDR WINDOW-1
ON VALUE-CHANGED OF HDR IN FRAME DEFAULT-FRAME
DO:
  IF tmpBatch = "" OR NOT CAN-FIND(FIRST tdet WHERE tdet.tbatch = tmpBatch) THEN DO:
      ASSIGN find-recid = RECID(tdet).
      {&open-query-det}
      FIND tDet WHERE RECID(tDet) = find-recid NO-ERROR.

      ASSIGN btnMaterial:SENSITIVE IN FRAME {&FRAME-NAME} = AVAIL tDet
             btnMaterial:LABEL     IN FRAME {&FRAME-NAME} = IF AVAILABLE tDet AND tDet.materialPosted
                                                            THEN "Unissue Material" ELSE "Issue Material".
      
       IF AVAIL tdet AND tdet.tfront THEN APPLY "Choose" TO btnNext.
       ELSE APPLY "Choose" TO btnPrev.
      IF AVAIL tdet AND NOT CAN-FIND(tdet WHERE tdet.trdate <> ? AND tdet.trtime = ?) AND valid-handle(mm2handle) THEN DO:
          RUN SendInfo(FIND-recid).
      END.
  END.
  ELSE DO:
      FIND FIRST tdet NO-LOCK WHERE tdet.tbatch = tmpbatch NO-ERROR.
      ASSIGN find-recid = RECID(tdet).
         {&open-query-det}
         FIND tDet WHERE RECID(tDet) = find-recid NO-ERROR.
         ASSIGN btnMaterial:SENSITIVE IN FRAME {&FRAME-NAME} = AVAIL tDet
                btnMaterial:LABEL     IN FRAME {&FRAME-NAME} = IF AVAILABLE tDet AND tDet.materialPosted
                                                               THEN "Unissue Material" ELSE "Issue Material".
        REPOSITION hdr TO RECID(find-recid).
  END.

  /*create "info box" string*/
  IF AVAIL tdet THEN DO:
    batchINFO:SCREEN-VALUE = "Batch: " + tdet.tbatch + "    " + "Size: " + tdet.tbed + "    " + string(tdet.tQty) + "-" + tdet.tsides + "    " + "Part: " + tdet.tInv + "    " + IF LOOKUP(tdet.tMat,"Magnetic,Vinyl") > 0 THEN tdet.tmat ELSE "".
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Close WINDOW-1
ON CHOOSE OF MENU-ITEM m_Close /* Close */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Corex_Sheet_Size
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Corex_Sheet_Size WINDOW-1
ON CHOOSE OF MENU-ITEM m_Corex_Sheet_Size /* Corex Sheet Size */
DO:
    IF thisMachine = 1 THEN RUN CorexSheetSize.w("1").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Gateway_Test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Gateway_Test WINDOW-1
ON CHOOSE OF MENU-ITEM m_Gateway_Test /* Gateway Test */
DO:
    RUN gatewaytest.w("", 0, thismachine, INPUT TABLE eDet).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Insert_Template
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Insert_Template WINDOW-1
ON CHOOSE OF MENU-ITEM m_Insert_Template /* Insert Template */
DO:
    DEFINE BUFFER bhdr FOR sign_mm_hdr.
    DEFINE VARIABLE nextSeq AS INTEGER     NO-UNDO.

    IF AVAIL tdet AND tdet.tMat <> "Template" THEN DO:
        FIND FIRST bhdr NO-LOCK WHERE bhdr.batchseq = INT(tdet.tbatch) NO-ERROR.
        IF AVAIL bhdr THEN DO:
            
            nextSeq = NEXT-VALUE(seq-mm-batch).
            DO WHILE CAN-FIND(sign_mm_hdr WHERE sign_mm_hdr.batchseq = nextSeq):
                nextSeq = NEXT-VALUE(seq-mm-batch).
            END.
            
            CREATE sign_mm_hdr.
            ASSIGN sign_mm_hdr.batchseq = nextSeq
                   sign_mm_hdr.matltype = "Template"
                   sign_mm_hdr.zzchar_2 = tDet.tMat
                   sign_mm_hdr.mach_time = 3.
            IF bhdr.runseq = 0 THEN sign_mm_hdr.runseq = 0. ELSE sign_mm_hdr.runseq = bhdr.runseq - 5.
            BUFFER-COPY bhdr EXCEPT batchseq matltype runseq zzchar_2 mach_time TO sign_mm_hdr.
            RELEASE sign_mm_hdr.
            
            IF SEARCH("\\fs02\bullseye\images\agentphotos\Temporary\mtl1\Poly 150\POP 55\batch" + STRING(bhdr.batchseq) + "_1_Template.pdf") <> ? THEN DO:
                OS-COPY VALUE("\\fs02\bullseye\images\agentphotos\Temporary\mtl1\Poly 150\POP 55\batch" + STRING(bhdr.batchseq) + "_1_Template.pdf") VALUE("\\fs02\bullseye\images\agentphotos\Temporary\mtl1\Poly 150\POP 55\batch" + STRING(nextSeq) + "_1_Template.pdf").
            END.
            ELSE IF SEARCH("\\fs02\bullseye\images\agentphotos\Temporary\mtl1\Poly 090\POP 55\batch" + STRING(bhdr.batchseq) + "_1_Template.pdf") <> ? THEN DO:
                OS-COPY VALUE("\\fs02\bullseye\images\agentphotos\Temporary\mtl1\Poly 090\POP 55\batch" + STRING(bhdr.batchseq) + "_1_Template.pdf") VALUE("\\fs02\bullseye\images\agentphotos\Temporary\mtl1\Poly 090\POP 55\batch" + STRING(nextSeq) + "_1_Template.pdf").   
            END.
            MESSAGE "Template Created" VIEW-AS ALERT-BOX MESSAGE.
            
            APPLY "choose" TO btnRefresh IN FRAME default-frame.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_JM_Queue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_JM_Queue WINDOW-1
ON CHOOSE OF MENU-ITEM m_JM_Queue /* JM Queue */
DO:
    RUN JM-Queue.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Move_Batch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Move_Batch WINDOW-1
ON CHOOSE OF MENU-ITEM m_Move_Batch /* Move Batch */
DO:
  RUN movebatch.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Print_Color_Target
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Print_Color_Target WINDOW-1
ON CHOOSE OF MENU-ITEM m_Print_Color_Target /* Print Color Target */
DO:
    IF AVAIL tdet THEN DO: 
        DEFINE VARIABLE jdfTemplate AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE curTime     AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE oScale      AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE cSub        AS CHARACTER   NO-UNDO.


        ASSIGN jdfTemplate = "{JDFColorCheck.i}".
               curTime     = STRING(TIME).
               cSub        = tDet.tMat. 

        /*Set Id*/
        ASSIGN jdfTemplate = REPLACE(jdfTemplate,"colorcheck-1","colorcheck-" + curTime)
               jdfTemplate = REPLACE(jdfTemplate,"ColorCheck.pdf","ColorCheck-" + curTime + ".pdf").

        IF INDEX(cSub,"Acrylic") > 0 THEN DO:
            ASSIGN oScale = "Acrylic".           
        END.
        ELSE IF INDEX(csub,"Alumalite") > 0 THEN DO:
            ASSIGN oScale = "Alumalite".
        END.
        ELSE IF INDEX(cSub,"Aluminum") > 0 THEN DO:
            IF INDEX(cSub,"Brushed") > 0 THEN DO:
                oScale = "Brushed Aluminum".    
            END.
            ELSE IF INDEX(cSub,"Reflective") > 0 THEN DO:
                IF INDEX(cSub,"040") > 0 THEN oScale = "Aluminum Ref 040". ELSE oScale = "Aluminum Ref 063".    
            END.
            ELSE DO:
                IF INDEX(cSub,"040") > 0 THEN oScale = "Aluminum 040". ELSE oScale = "Aluminum 063".        
            END.
        END.
        ELSE IF INDEX(cSub,"Corex") > 0 THEN DO:
            IF INDEX(cSub,"Reflective") > 0 THEN DO:
                IF INDEX(cSub,"4mm") > 0 THEN oScale = "Corex Ref 4mm". ELSE oScale = "Corex Ref 10mm".  
            END.
            ELSE DO:
                IF INDEX(cSub,"4mm") > 0 THEN oScale = "Corex 4mm". ELSE oScale = "Corex 10mm".    
            END.    
        END.
        ELSE IF INDEX(cSub,"Gloss Paper") > 0 THEN ASSIGN oScale = "Gloss Paper".
        ELSE IF INDEX(cSub,"Lexan")       > 0 THEN ASSIGN oScale = "Lexan".
        ELSE IF INDEX(cSub,"Magnetic")    > 0 THEN ASSIGN oScale = "Magnetic".
        ELSE IF INDEX(cSub,"Omegabond") > 0 THEN DO:
            IF INDEX(cSub,"Reflective") > 0 THEN ASSIGN oScale = "Omegabond Ref". ELSE oScale = "Omegabond".    
        END.
        ELSE IF INDEX(cSub,"Poly") > 0 THEN DO:
            IF      INDEX(cSub,"050") > 0 THEN ASSIGN oScale = "Poly 050".
            ELSE IF INDEX(cSub,"090") > 0 THEN ASSIGN oScale = "Poly 090".
            ELSE IF INDEX(cSub,"150") > 0 THEN ASSIGN oScale = "Poly 150".
        END.
        ELSE IF INDEX(cSub,"PVC") > 0 THEN DO:
            IF INDEX(cSub,"Reflective") > 0 THEN ASSIGN oScale = "PVC Ref". ELSE oScale = "PVC".
        END.
        ELSE IF INDEX(cSub,"Vinyl") > 0 THEN DO:
            IF INDEX(cSub,"Reflective") > 0 THEN ASSIGN oScale = "Vinyl Ref".
            ELSE IF INDEX(cSub,"Rigid") > 0 THEN ASSIGN oScale = "Rigid Vinyl".
            ELSE ASSIGN oScale = "Vinyl".
        END.
        ELSE IF INDEX(cSub,"Sintra") > 0          THEN ASSIGN oScale = "Sintra".
        ELSE IF INDEX(cSub,"Styrene") > 0         THEN ASSIGN oScale = "Styrene".
        ELSE IF INDEX(cSub,"White Cardboard") > 0 THEN ASSIGN oScale = "White Cardboard".
        ELSE IF INDEX(cSub,"Templates") > 0       THEN ASSIGN oScale = "Templates".
        ELSE IF INDEX(cSub,"Steel") > 0 THEN DO:
             IF INDEX(cSub,"20ga") > 0 THEN DO:
                IF INDEX(cSub,"Reflective") > 0 THEN oScale = "Steel Ref 20ga". ELSE oScale = "Steel 20ga".
             END.
             ELSE IF INDEX(cSub,"24ga") > 0 THEN DO:
                IF INDEX(cSub,"Reflective") > 0 THEN oScale = "Steel Ref 24ga". ELSE oScale = "Steel 24ga".    
             END.
        END.
        ELSE oScale = "". /*make this the default*/   

        jdfTemplate = REPLACE(jdfTemplate,"[DeviceID]","Digitech-TruFire:" + oScale).

    
        /*delete Jettifiles*/
        IF SEARCH("\\calder-rip03\JETIFILES\ColorCheck-" + curTime) <> ? THEN OS-DELETE VALUE("\\caldera-rip03\JETIFILES\ColorCheck-" + curTime) NO-ERROR.  
    
        /*PDF Copy*/
        OS-COPY VALUE("\\fs04\RIP\LatexPrinters\Color Management\ColorCheck\ColorCheck.pdf")  VALUE("\\fs04\RIP\LatexPrinters\Color Management\ColorCheck\ColorCheck-" + curTime + ".pdf").  
    
        /*XML Copy for me*/
        OUTPUT TO VALUE("\\lowen\dfssancluster\Bullseye\Logs\JDF\Digitech\ColorCheckCopies\ColorCheck-" + curTime + ".xml").
        PUT UNFORMATTED JDFTemplate SKIP.
        OUTPUT CLOSE.
    
        /*Send copy to the Printer*/
        OS-COPY VALUE("\\lowen\dfssancluster\Bullseye\Logs\JDF\Digitech\ColorCheckCopies\ColorCheck-" + curTime + ".xml")  VALUE("\\retro-v2\jdf\").
    
        JDFTemplate = SUBSTRING(JDFTemplate,1,INDEX(JDFTemplate,"<Mode") - 1) + SUBSTRING(JDFTemplate,INDEX(JDFTemplate,"</ResourceLink")). 
        /*JDF Copy for me*/
        OUTPUT TO VALUE("\\lowen\dfssancluster\Bullseye\Logs\JDF\Digitech\ColorCheckCopies\ColorCheck-" + curTime + ".jdf").
        PUT UNFORMATTED JDFTemplate SKIP.
        OUTPUT CLOSE.
    
        /*Send copy to the RIP*/
        OS-COPY VALUE("\\lowen\dfssancluster\Bullseye\Logs\JDF\Digitech\ColorCheckCopies\ColorCheck-" + curTime + ".jdf")  VALUE("\\caldera-rip03\Public\hotfolder\Nexio\NoTile\").
   
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Print_Test_Colors
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Print_Test_Colors WINDOW-1
ON CHOOSE OF MENU-ITEM m_Print_Test_Colors /* Print Test Colors */
DO:
  RUN logs IN mm-pp-handle (1,"Sent ColorTestFile","","","").

  ASSIGN destDir2 = "\\CALDERA-RIP04\Public\Substrate Hotfolders\95 Production\pt1mm_Templates\bed-08.pdf".
  RUN mm-hotfoldermachine.p (thisMachine,INPUT-OUTPUT destDir2).
  OS-COPY VALUE("\\lowen\dfssancluster\SignArt\DigitalBedTemplates\bed-08.pdf") VALUE(destDir2).
  
  ASSIGN destDir2 = "\\CALDERA-RIP04\Public\Substrate Hotfolders\55 Solid\1mm Steel\ColorTestFile.pdf".
  RUN mm-hotfoldermachine.p (thisMachine,INPUT-OUTPUT destDir2).
  OS-COPY VALUE("\\lowen\dfssancluster\SignArt\DigitalBedTemplates\ColorTestFile.pdf") VALUE(destDir2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Regen_Batch_Image2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Regen_Batch_Image2 WINDOW-1
ON CHOOSE OF MENU-ITEM m_Regen_Batch_Image2 /* Regen Batch Image */
DO:
    DEF VAR images    AS CHAR NO-UNDO.
    DEF VAR delStatus AS CHAR NO-UNDO.
    DEF VAR cResponse AS CHAR NO-UNDO.

    IF (CAN-DO(superUser,current-user-id) OR current-user-id = "printersign") AND AVAIL(tdet) THEN DO:
        SESSION:SET-WAIT-STATE("general").
        RUN zz_control  IN mm-pp-handle (NO,"MM-Reprint").
        
            FOR EACH buff_mm_hdr WHERE buff_mm_hdr.batchseq = int(tdet.tbatch) AND buff_mm_hdr.RUN_time = ? BY buff_mm_hdr.runseq:
                FOR EACH buff_mm_det OF buff_mm_hdr BREAK BY buff_mm_det.itemseq:
                    IF LAST-OF(buff_mm_det.itemseq) THEN DO:
                         RUN deleteMMcs6.p(buff_mm_det.itemseq,"Delete",OUTPUT images,OUTPUT delStatus). /*delete temp images*/
                         FIND so_items NO-LOCK WHERE so_items.itemseq = buff_mm_det.itemseq NO-ERROR.
                         IF AVAIL so_items THEN DO:
                            RUN CreateSartDart.p (RECID(so_items)). /*recreate the production art*/
                         END.
                    END.
                END.
                ASSIGN buff_mm_hdr.rerun = YES.
            END.
            RUN genxml IN mm-pp-handle ("notblank").
            
            /*delete and regen the .png file*/
            OS-DELETE VALUE(REPLACE(tdet.tFile,".pdf",".png")).
            RUN pdftopng (tdet.tFile,REPLACE(tdet.tFile,".pdf",".png"),OUTPUT cResponse).

        SESSION:SET-WAIT-STATE("").
        RUN zz_control  IN mm-pp-handle (YES,"MM-Reprint").
        APPLY "choose":u TO btnRefresh IN FRAME {&FRAME-NAME}.
    END.
    ELSE RUN mm-msg.w("You do are not authorized to access this function.",0,YES,1,OUTPUT qtyran, OUTPUT answer).
    IF AVAIL buff_mm_hdr THEN RELEASE buff_mm_hdr.
    IF AVAIL buff_mm_det THEN RELEASE buff_mm_det.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Regen_DART_Files
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Regen_DART_Files WINDOW-1
ON CHOOSE OF MENU-ITEM m_Regen_DART_Files /* Regen DART Files */
DO:
    RUN RegenDART.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Reprint_Batch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Reprint_Batch WINDOW-1
ON CHOOSE OF MENU-ITEM m_Reprint_Batch /* Reprint Batch */
DO:
  
  IF tdet.tseq MODULO 2 = 0 THEN DO:
      cMode = "reprint".
      RUN movepdf(tdet.tbatch,"Batch","reprint").
  END.
  ELSE 
      RUN mm-msg.w("Not avail on a template.",0,YES,1,OUTPUT qtyran, OUTPUT answer).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Reprint_Item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Reprint_Item WINDOW-1
ON CHOOSE OF MENU-ITEM m_Reprint_Item /* Reprint Item */
DO:
    DEFINE VARIABLE rVar        AS LOG       NO-UNDO.
    DEFINE VARIABLE CorexOver48 AS CHAR      NO-UNDO.
    DEFINE VARIABLE CorexU48    AS LOG       NO-UNDO.
    DEFINE VARIABLE CorexFO     AS LOG       NO-UNDO.
    
    {mgseclist.i "CorexOver48" CorexOver48}
    

  start-key = "Item".
  EMPTY TEMP-TABLE ttArt.
  EMPTY TEMP-TABLE print_det. 
  
  RUN mmReprint.w(start-key,mm-pp-handle,OUTPUT OrderNo, OUTPUT ItemNo, OUTPUT isRider,OUTPUT reprintMachine).

  IF orderNO = "99999" THEN DO:
      EMPTY TEMP-TABLE ttArt.
      EMPTY TEMP-TABLE print_det.
      RUN logs IN mm-pp-handle (7,"Bailout","","","").
      /*RUN zz_control  IN mm-pp-handle (YES,"JM-ReprintLock").*/
      RETURN.
  END.

  IF CAN-FIND(FIRST print_det) THEN DO:
      RUN setHomeFolder IN mm-pp-handle.

      FOR EACH print_det:
          FIND so_items NO-LOCK WHERE so_items.so_no = print_det.pSo AND so_items.ITEM_no = Print_det.pItem NO-ERROR.              
          IF AVAIL so_items THEN DO:
              RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " BuildTT","","","").
              RUN buildtt     IN mm-pp-handle (so_items.so_no,so_items.ITEM_no).
              RUN checkImages IN mm-pp-handle (so_items.itemseq).
          END.
      END.
      start-key = "Image".   
                                                                                
      RUN mmReprint.w(start-key,mm-pp-handle,OUTPUT OrderNo, OUTPUT ItemNo,OUTPUT rVar,OUTPUT reprintMachine). 

      IF orderNO = "99999" THEN DO:
          EMPTY TEMP-TABLE ttArt.
          EMPTY TEMP-TABLE print_det.
          RUN logs IN mm-pp-handle (7,"Bailout","","","").
          /*RUN zz_control  IN mm-pp-handle (YES,"JM-ReprintLock").*/
          RETURN.
      END.

      FOR EACH ttArt:
          RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " Rerunning So#: " + string(ttArt.ttso) + " Item#: " + string(ttArt.ttItemNo) + " Qty: " + string(ttArt.ttQty) + " ttType: " + ttArt.ttType + " TempSeq: " + STRING(ttArt.ttTempSeq) + " IsDelayed: " + string(ttDelayedReprint) + " artfile: " + ttArt.ttFile,"","","").
      END. 
      RUN zz_control  IN mm-pp-handle (NO,"MM-Reprint").
      RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " Pre CreateAndSetReprintData","","","").
      RUN CreateAndSetReprintData IN mm-pp-handle.
      RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " Post CreateAndSetReprintData","","","").

      /*Prime Center Code*/
      IF CAN-FIND(FIRST ttArt WHERE INDEX(ttArt.ttType,"Corex") > 0) THEN DO:
          
          FOR EACH ttArt WHERE INDEX(ttArt.ttType,"Corex") > 0:
            FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = ttArt.ttItemseq NO-ERROR.
            IF squ_ptdet.FoldOver = TRUE THEN CorexFO = TRUE.
            IF CorexFO = TRUE THEN LEAVE.
          END.
          RELEASE squ_ptdet.
          
          IF CAN-FIND(FIRST ttArt WHERE INDEX(ttArt.ttType,"Corex") > 0 AND LOOKUP(STRING(ttArt.ttTempSeq),CorexOver48) = 0) THEN CorexU48 = TRUE. 
          
          IF CorexU48 = TRUE OR CorexFO = TRUE THEN DO:
              RUN DelCorexPC    IN mm-pp-handle ("").
              RUN logs          IN mm-pp-handle (7,"Del PC Corex Files Finished - MMLook","","","").
              RUN GroupingCorex IN mm-pp-handle ("").
              RUN logs          IN mm-pp-handle (7,"MM-Grouping Corex Finished - MMLook","","","").
          END.
      END.
     
      RUN grouping    IN mm-pp-handle ("12345").
      RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " Post Grouping","","","").
      RUN regroup     IN mm-pp-handle ("12345").
      RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " Post reGroup","","","").
          
      IF EnableDynNest THEN DO:
        RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " Pre DynamiNest","","","").
        RUN DynamicNest IN mm-pp-handle ("12345").
        RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " Post DynamiNest","","","").
      END.
      RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " Pre Trimdoups","","","").
      RUN trimdoups   IN mm-pp-handle.
      RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " Pre printorder","","","").
      RUN printorder  IN mm-pp-handle ("MACH" + STRING(max(1,reprintMachine))).
      RUN logs IN mm-pp-handle (7,"User: " + current-user-id + " Pre genXML","","","").
      RUN genXml      IN mm-pp-handle ("12345").
      RUN logs        IN mm-pp-handle (7,"End RePrint","","","").         
      RUN zz_control  IN mm-pp-handle (YES,"MM-Reprint").
      RUN releaseAll  IN mm-pp-handle.
  END.
  
  APPLY "VALUE-CHANGED":U TO tgl_show IN FRAME {&FRAME-NAME}.   
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Reprint_Template
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Reprint_Template WINDOW-1
ON CHOOSE OF MENU-ITEM m_Reprint_Template /* Reprint Template */
DO:
    cMode = "reprint".
    RUN movepdf(tdet.tbatch,"template","template").   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Rotate_Batch_Images
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Rotate_Batch_Images WINDOW-1
ON CHOOSE OF MENU-ITEM m_Rotate_Batch_Images /* Rotate Batch Images */
DO:
    DEF VAR cHotfolder AS CHAR.
    DEF VAR zundsf AS CHAR.
    DEF VAR zunddf AS CHAR.
    DEF VAR tmpimg AS CHAR.
    DEF VAR cHotfolderseq AS INT.
    IF CAN-DO(superUser,current-user-id) AND AVAIL(tdet) THEN DO:
          SESSION:SET-WAIT-STATE("general").
          IF tdet.tCutFile <> "" THEN DO:
              /*zund gang and genArt*/

              RUN gethotfolder IN mm-pp-handle(1,tdet.tMat,OUTPUT cHotfolderseq,OUTPUT cHotfolder, OUTPUT ZundSF, OUTPUT ZundDF).
              DO iloop = 1 TO (IF tdet.tsides = "d/f" THEN 2 ELSE 1):
                  tmpimg = cHotfolder + "\" + "batch" + string(tdet.tbatch) + "_" + string(iLoop) + ".pdf".

                  RUN zundRotate IN mm-pp-handle (tmpImg,IF iLoop = 1 THEN "270" ELSE "90").
/*                   PAUSE 5 NO-MESSAGE. */
                  RUN zundJPG IN mm-pp-handle (tmpImg).
              END.
          END.
          SESSION:SET-WAIT-STATE("").
      END.
      ELSE RUN mm-msg.w("You do are not authorized to access this function.",0,YES,1,OUTPUT qtyran, OUTPUT answer).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Send_Cut_File
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Send_Cut_File WINDOW-1
ON CHOOSE OF MENU-ITEM m_Send_Cut_File /* Send Cut File */
DO:
  IF AVAIL(tdet) AND tdet.tCutFile > "" THEN DO:
      IF SEARCH(tdet.tCutFile) <> ? THEN DO:
          ASSIGN tdet.tCutfile = REPLACE(tdet.tcutfile,"/","\").
          OS-COPY VALUE(tdet.tCutFile) VALUE(cutQueue + "\" + ENTRY(NUM-ENTRIES(tdet.tCutFile,"\"),tdet.tCutFile,"\")).
      END.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Test_History
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Test_History WINDOW-1
ON CHOOSE OF MENU-ITEM m_Test_History /* Test History */
DO:
  RUN gwhist.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Toggle_Partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Toggle_Partial WINDOW-1
ON CHOOSE OF MENU-ITEM m_Toggle_Partial /* Toggle Partial */
DO:
    DEFINE VARIABLE lFullBed AS LOGICAL NO-UNDO.
    DEFINE VARIABLE tmpBatch AS INT     NO-UNDO.
    
    IF AVAILABLE tdet THEN DO:
        FIND sign_mm_hdr EXCLUSIVE-LOCK WHERE sign_mm_hdr.batchseq = int(tdet.tbatch) NO-ERROR .
        IF AVAILABLE sign_mm_hdr THEN ASSIGN sign_mm_hdr.fullbed = NOT sign_mm_hdr.fullbed
                                             lFullBed            = sign_mm_hdr.fullbed
                                             tmpBatch            = sign_mm_hdr.batchseq.
        RELEASE sign_mm_hdr.
            
         MESSAGE "Batch No:" + STRING(tmpBatch) + " is " + (IF lFullBed THEN " Full " ELSE " Patrial ") + " Now" VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_View_JT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_View_JT WINDOW-1
ON CHOOSE OF MENU-ITEM m_View_JT /* View JT */
DO:
    DEFINE VARIABLE prt AS CHAR    NO-UNDO.
    DEFINE VARIABLE bOK AS LOGICAL NO-UNDO.
    
    IF AVAIL(fdet) THEN DO:
       FIND so_items NO-LOCK WHERE so_items.so_no = fdet.fso AND so_items.ITEM_no = int(fdet.fitemno) NO-ERROR.
       IF AVAIL so_items THEN DO:
           prt = SESSION:TEMP-DIR + so_items.so_no + "-" + STRING(so_items.item_no) + "-" + STRING(fdet.fartlinkseq) + ".pdf".
           RUN signsowo.p (RECID(so_items),fdet.fArtLinkSeq,prt,OUTPUT bOK).
           IF SEARCH(prt) <> ? THEN DO:
               OS-COPY VALUE(prt) VALUE("\\lowen\dfssancluster\Bullseye\JTs\").
           END.
           
           OS-COMMAND SILENT VALUE("start " + prt).
          
            /*{run_prg1.i ""signsowo-single.p"" "(recid(so_items))"}*/
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME RADIO-cTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-cTo WINDOW-1
ON VALUE-CHANGED OF RADIO-cTo IN FRAME FRAME-A
DO:
  DEFINE VARIABLE sendto  AS CHAR NO-UNDO.
  DEFINE VARIABLE SENDcc  AS CHAR NO-UNDO.
  
  ASSIGN sendto = "" cto:SCREEN-VALUE IN FRAME frame-a = "".
  IF det:NUM-SELECTED-ROWS IN FRAME default-frame > 0 THEN DO i = 1 TO det:NUM-SELECTED-ROWS IN FRAME default-frame:
      det:FETCH-SELECTED-ROW(i).
  END.
  ASSIGN sendto = "Brittanyb@lowen.com"
         sendcc = "Progressgroup@lowen.com".
  IF sendto <> "" THEN DO:
      ASSIGN cTo:SCREEN-VALUE IN FRAME frame-a = sendto
             cCC:SCREEN-VALUE IN FRAME frame-a = SENDcc.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RegenPCCorex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RegenPCCorex WINDOW-1
ON CHOOSE OF MENU-ITEM RegenPCCorex /* Regen PC Corex */
DO:
    /*Delete and resend all Corex batches that go through Prime Center*/

    DEFINE VARIABLE filepath   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE fName      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iTmpALS    AS INTEGER     NO-UNDO.
    
    DEFINE VARIABLE Folders    AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE xmlData    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE sub        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cRecipe    AS CHARACTER   NO-UNDO.

    
    EMPTY TEMP-TABLE ttCorexReprint.

    RUN logs IN mm-pp-handle (100,"Start RegenPCCorex","","","").
    /*SESSION:SET-WAIT-STATE("GENERAL").*/
    
    FOR EACH tDet NO-LOCK WHERE INDEX(tDet.tMat,"Corex") > 0 AND tDet.tBedseq = "0":
        FOR EACH fDet NO-LOCK WHERE fDet.fBatch = tDet.tBatch:
            FIND FIRST ttCorexReprint WHERE ttCorexReprint.ttItemseq = fDet.fItemseq AND ttCorexReprint.ttALS = fDet.fArtLinkSeq NO-ERROR.   
            IF AVAIL ttCorexReprint THEN ASSIGN ttCorexReprint.ttQty = ttCorexReprint.ttQty + INT(fDet.fOnBed).
            ELSE DO:
                CREATE ttCorexReprint.
                ASSIGN ttCorexReprint.ttItemseq = fDet.fitemseq
                       ttCorexReprint.ttALS     = fDet.fArtLinkSeq
                       ttCorexReprint.ttQty     = INT(fDet.fOnBed)
                       ttCorexReprint.ttArtFile = fDet.fFile.
                       
                /*Get SO#*/
                FIND FIRST so_items NO-LOCK WHERE so_items.itemseq = fDet.fItemseq NO-ERROR.
                IF AVAIL so_items THEN ASSIGN ttCorexReprint.ttSO     = so_items.so_no
                                              ttCorexReprint.ttItemNo = so_items.item_no.
                                              
                RUN logs IN mm-pp-handle (100,"Create ttCorexReprint - SO: " + ttCorexReprint.ttSO + "-" + STRING(ttCorexReprint.ttItemNo),"","","").
                
                RELEASE so_items.
                RELEASE ttCorexReprint.                   
            END.             
        END. 
        
    END.
    
    APPLY "choose":U TO btnRefresh IN FRAME DEFAULT-FRAME. /*refresh the queue*/
    RUN logs IN mm-pp-handle (100,"Done building ttCorexReprint","","","").
    
    EMPTY TEMP-TABLE ttArt.
    EMPTY TEMP-TABLE print_det.
    
    FOR EACH ttCorexReprint:
        RUN BuildTT      IN mm-pp-handle (ttCorexReprint.ttSO, ttCorexReprint.ttItemNo).
        RUN CheckImages  IN mm-pp-handle (ttCorexReprint.ttItemseq).             
    END.
    RUN logs IN mm-pp-handle (100,"Done Running BuildTT","","","").
    
    
    FOR EACH ttArt:
        FIND FIRST so_art WHERE so_art.itemseq = ttArt.ttItemseq AND so_art.type = "mini" AND so_art.artfile = ENTRY(1,ttArt.ttFile) NO-ERROR.
        IF AVAILABLE so_art THEN iTmpALS = so_art.disp_order. ELSE iTmpALS = 0. 
        FIND FIRST ttCorexReprint NO-LOCK WHERE ttCorexReprint.ttItemseq = ttArt.ttItemseq AND ttCorexReprint.ttALS = iTmpALS NO-ERROR.
          
        MESSAGE ttArt.ttSo + "-" + STRING(ttArt.ttItemNo) + "-" + STRING(ttCorexReprint.ttALS) SKIP STRING(ttArt.ttQty) SKIP STRING(ttCorexReprint.ttQty).   
        IF AVAIL ttCorexReprint THEN ASSIGN ttArt.ttQty = ttCorexReprint.ttQty.       
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Tg-multibatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg-multibatch WINDOW-1
ON VALUE-CHANGED OF Tg-multibatch IN FRAME DEFAULT-FRAME /* Multi Batch Enabled */
DO:
  tg-multibatch:checked = EnableMultiBatching.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgl_Show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgl_Show WINDOW-1
ON VALUE-CHANGED OF tgl_Show IN FRAME DEFAULT-FRAME /* Show Completed */
DO:
  IF tgl_show:PRIVATE-DATA <> string(tgl_show:CHECKED IN FRAME {&FRAME-NAME}) THEN tmpBatch = "".
  IF tgl_show:CHECKED IN FRAME {&FRAME-NAME} THEN
      RUN buildTT(YES).
  ELSE RUN buildtt(NO).
 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME det
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF VALID-HANDLE(mm-pp-handle) THEN RUN releaseAll IN mm-pp-handle.
  IF VALID-HANDLE(mm-pp-handle) THEN DELETE PROCEDURE mm-pp-handle.
  RUN disable_UI.
END.   
/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.) */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {title.i}
  {colorset.i}
  {mgcenter.i}
  
  ASSIGN FRAME {&FRAME-NAME}:BGCOLOR = bg-color
         FRAME {&FRAME-NAME}:FGCOLOR = fg-color
         c_clientname  = OS-GETENV("computername")
         thisMachine = IF start-key BEGINS "Digital-" THEN INT(ENTRY(2,start-key,'-')) ELSE thisMachine. /*ryanle*/
  
  ASSIGN {&WINDOW-NAME}:TITLE = "Digital-" + STRING(thisMachine). 
  
  FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "Digital" AND zz_file.zz_key2 = "1" NO-ERROR.
  IF AVAILABLE zz_file THEN chkDigital1 = zz_file.zz_log[1].
    
  FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "Digital" AND zz_file.zz_key2 = "2" NO-ERROR.
  IF AVAILABLE zz_file THEN chkDigital2 = zz_file.zz_log[1].
    
  FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "Digital" AND zz_file.zz_key2 = "3" NO-ERROR.
  IF AVAILABLE zz_file THEN chkDigital3 = zz_file.zz_log[1].
  RELEASE zz_file NO-ERROR.
  

  RUN enable_UI.
  {mgx_proc.i}

   /*get column handles*/
  ASSIGN hColumn  = BROWSE hdr:GET-BROWSE-COLUMN(5)
         hColumn2 = BROWSE hdr:GET-BROWSE-COLUMN(6)
         hColumn3 = BROWSE hdr:GET-BROWSE-COLUMN(7)
         hColumn4 = BROWSE hdr:GET-BROWSE-COLUMN(8)
         hColumn :VISIBLE = IF CAN-DO(superUser,current-user-id)  THEN TRUE ELSE FALSE
         hColumn2:VISIBLE = IF CAN-DO(superUser,current-user-id)  THEN TRUE ELSE FALSE
         hColumn3:VISIBLE = IF CAN-DO(superUser,current-user-id)  THEN TRUE ELSE FALSE.
         hColumn4:VISIBLE = IF CAN-DO(superUser,current-user-id)  THEN TRUE ELSE FALSE.

  
  MESSAGE "Would you like to display the second JM screen? (Y/N)" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.
  IF lChoice = TRUE THEN RUN mm-look2.w PERSISTENT SET mm2handle ("",THIS-PROCEDURE,bg-color).
  
  RUN mm-pp.p    PERSISTENT SET mm-pp-handle .
  RUN getDBase IN mm-pp-handle (OUTPUT dBase). 

  IF dBase <> "Live" THEN DO:
      ASSIGN homeFolder = imageShare + "AgentPhotos\temporary\mtl1-Test"
             ranFolder = imageShare + "AgentPhotos\temporary\CS6-Test\CompletedBatches".
  END.

  APPLY "Choose":U TO btnRefresh.
  APPLY "Choose":U TO btnCancel.
  
  ASSIGN btnOverlay:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.


  /*if closed in the middle of a run should open mm-look2.w to the same one on open*/
  IF CAN-FIND(tdet WHERE tdet.trdate <> ? AND tdet.trtime = ?) AND valid-handle(mm2handle) THEN DO:
      FIND tdet WHERE tdet.trdate <> ? AND tdet.trtime = ? NO-ERROR.
      IF AVAIL tdet THEN DO:
        RUN sendInfo(RECID(tdet)).
      END.
  END.

  /*manually set these b/c they always change back*/
  chWebBrowser:WebBrowser:AddressBar = FALSE.
  chWebBrowser:WebBrowser:MENUBAR    = FALSE.
  chWebBrowser:WebBrowser:StatusBar  = FALSE.
  chWebBrowser:WebBrowser:ToolBar    = 0.
  

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appsrv-build WINDOW-1 
PROCEDURE appsrv-build :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER showRan   AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER tCnt     AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER mtString AS CHAR    NO-UNDO.


SESSION:SET-WAIT-STATE("GENERAL").

CREATE SERVER hAppSrv.
/* GET-KEY-VALUE SECTION "AppServerConnection" KEY "ConnectString" VALUE AppServCon. */
/* hAppSrv:CONNECT(AppServCon) NO-ERROR.                                             */

IF dBase = "live" THEN
    hAppSrv:CONNECT("-S 5162 -H qbprod -AppService remoteservices -sessionModel session-free") NO-ERROR.
ELSE
    hAppSrv:CONNECT("-S 5162 -H qbprod -AppService devremoteservices -sessionModel session-free") NO-ERROR.

IF NOT hAppSrv:CONNECTED() THEN hAppSrv = SESSION:HANDLE.  /* if no appserver run on local session */

RUN mm-look-build.p ON hAppSrv (showRan, OUTPUT table tdet,OUTPUT table fdet,OUTPUT tcnt,OUTPUT mtString ).

IF ERROR-STATUS:ERROR = YES THEN DO:
    c_msg = "Program: " + STRING("mm-look-build.p").
    DO errorCnt = 1 TO ERROR-STATUS:NUM-MESSAGES:
        c_msg = c_msg + " " + "Error Number: " + string(ERROR-STATUS:GET-NUMBER(errorCnt)) + "Error Code: " + string(ERROR-STATUS:GET-MESSAGE(errorCnt)).
    END.
    RUN mgemail.p ("{system.i} Database","progressgroup@lowen.com","","AppSrv.i Error",c_msg,"",FALSE).
END.


IF hAppSrv <> SESSION:HANDLE THEN DO:
    hAppSrv:DISCONNECT() NO-ERROR.
    DELETE OBJECT hAppSrv NO-ERROR.
END.  
SESSION:SET-WAIT-STATE("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE batch_details WINDOW-1 
PROCEDURE batch_details :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cType  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER iBatch AS INT  NO-UNDO.


    FIND sign_mm_hdr WHERE sign_mm_hdr.batchSeq = iBatch NO-ERROR.
    IF AVAIL sign_mm_hdr THEN DO:
        
        IF cType = "Start" THEN       ASSIGN sign_mm_hdr.RUN_date = TODAY
                                             sign_mm_hdr.RUN_time = ?.
        ELSE IF cType = "Finish" THEN ASSIGN sign_mm_hdr.RUN_time = TIME.    
        RELEASE sign_mm_hdr.
    END.
    ELSE RUN mm-msg.w("Batch(" + STRING(iBatch) + ") record not available!",0,YES,1,OUTPUT qtyran, OUTPUT answer). 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildTT WINDOW-1 
PROCEDURE BuildTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ShowRan  AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cnt             AS INTEGER   NO-UNDO.
DEFINE VARIABLE fName           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPathway        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mtString        AS CHARACTER NO-UNDO.

RUN logStart ("BuildTT Start - " + STRING(ShowRan)).

SESSION:SET-WAIT-STATE("general").
EMPTY TEMP-TABLE tdet.
EMPTY TEMP-TABLE fdet.
EMPTY TEMP-TABLE idet.
EMPTY TEMP-TABLE edet.
EMPTY TEMP-TABLE ranDet.


ASSIGN tCnt = 0
       cnt  = 0.

RUN logStart ("BuildTT - RUN mm-look-build.p").
RUN mm-look-build.p (mm-pp-handle,thisMachine,showRan,OUTPUT table tdet,OUTPUT table fdet,OUTPUT tcnt, OUTPUT mtString).
RUN logStart ("BuildTT - END mm-look-build.p").

 /*get rid of templates if there are no actual files to go with them */
FOR EACH tdet NO-LOCK WHERE tdet.trdate = ? AND tDet.tMat = "Template":
    IF NOT CAN-FIND(FIRST ttdet WHERE ttdet.tRDate = ? AND ttdet.tbedseq = tdet.tbedseq AND ttdet.tbatch <> tdet.tbatch) THEN DELETE tdet.
END.


RUN logStart ("BuildTT - RUN empRefresh").
RUN empRefresh.
RUN logStart ("BuildTT - END empRefresh").

{&open-query-hdr}

RUN logStart ("BuildTT 3 Value-Changed hdr").
APPLY "Value-changed":U TO BROWSE hdr.
RUN logStart ("BuildTT 4 END Value-Changed hdr").
SESSION:SET-WAIT-STATE("").

ASSIGN numRecords:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(tCnt)
       cMachTime :SCREEN-VALUE IN FRAME {&FRAME-NAME} = mtString.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clockIn WINDOW-1 
PROCEDURE clockIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER clockIn AS LOGICAL NO-UNDO.

DEFINE VARIABLE pValue AS CHARACTER NO-UNDO.

DEFINE VARIABLE zzRec AS RECID NO-UNDO.

RUN mgupdate.w ("Clock " + IF clockIn THEN "In" ELSE "Out","Scan your badge","x(12)","",OUTPUT pValue).
pValue = REPLACE(pValue,"-"," ").

/************************/
/* Temporary Time Card? */
/************************/
IF pValue BEGINS "TEMP" THEN DO:
    FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "TempTimeCard"
    AND zz_file.zz_key2 = ENTRY(2,pValue," ") NO-ERROR.
    IF AVAILABLE zz_file THEN DO:
        FIND employee NO-LOCK WHERE employee.empl_no = zz_file.zz_key3 NO-ERROR.
        IF AVAILABLE employee THEN pValue = employee.empl_no + " " + string(employee.badge_seq).
    END.
    RELEASE zz_file.
END.
ELSE DO:
    FIND employee NO-LOCK WHERE employee.empl_no = ENTRY(1,pValue," ") NO-ERROR.
END.
IF AVAILABLE employee AND employee.termdate = ? THEN DO:

    IF clockIn THEN DO:

        FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "SIGN-DIGITAL-MACHINE-" + machineCode
            AND zz_file.zz_key2 = IF employee.empl_no = "9995" THEN pValue ELSE employee.empl_no NO-ERROR.
        IF AVAILABLE zz_file THEN DO:
            RUN mm-msg.w("This employee is already logged in.",0,YES,1,OUTPUT qtyran, OUTPUT answer).
            RELEASE zz_file.
            RETURN.
        END.
        RELEASE zz_file.

        IF employee.empl_no = "9995" THEN DO:
              FIND manpower_zz_file NO-LOCK WHERE manpower_zz_file.zz_key1 = "BADGE:MANPOWER"
                                              AND manpower_zz_file.zz_key2 = ENTRY(2,pValue," ") NO-ERROR.
        END.

        CREATE zz_file.
        CREATE edet.
        ASSIGN zz_file.zz_key1    = "SIGN-DIGITAL-MACHINE-" + machineCode
               zz_file.zz_key2    = IF employee.empl_no = "9995" THEN pValue ELSE employee.empl_no
               zz_file.zz_date[1] = TODAY
               zz_file.zz_dec[1]  = TIME
               edet.eEmpNo        = IF employee.empl_no = "9995" THEN pValue ELSE employee.empl_no
               edet.eName         = IF employee.empl_no = "9995" AND AVAIL manpower_zz_file
                                    THEN CAPS(manpower_zz_file.zz_char[1]) ELSE employee.name_1st + " " + employee.name.
        RELEASE zz_file.
        {&OPEN-QUERY-emp}

        /****************************************************************************/
        /* Now we either put them on material handling or the current batch running */
        /****************************************************************************/
        RUN currentBatch. /* currentBatch = "" means none are currently running */
        RUN laborUpdate (IF employee.empl_no = "9995" THEN pValue ELSE employee.empl_no,currentBatch).
    END.
    ELSE DO: /******************** clock out ********************/
        FIND zz_file WHERE zz_file.zz_key1 = "SIGN-DIGITAL-MACHINE-" + machineCode
            AND zz_file.zz_key2 = (IF employee.empl_no = "9995" THEN pValue ELSE employee.empl_no) NO-ERROR.
        IF NOT AVAILABLE zz_file THEN DO:
            RUN mm-msg.w("This employee is not logged in.",0,YES,1,OUTPUT qtyran, OUTPUT answer).
            RETURN.
        END.
        ASSIGN bClockOut = TRUE
               zzRec = RECID(zz_file).
        RUN laborUpdate (IF employee.empl_no = "9995" THEN pValue ELSE employee.empl_no,"").

        FIND eDet WHERE eDet.eEmpNo = IF employee.empl_no = "9995" THEN pValue ELSE employee.empl_no NO-ERROR.
        IF AVAILABLE eDet THEN DELETE eDet.
        FIND zz_file WHERE RECID(zz_file) = zzRec NO-ERROR.
        IF AVAILABLE zz_file THEN DELETE zz_file.
        RUN empRefresh.
        {&OPEN-QUERY-emp}

    END.
    RELEASE zz_file NO-ERROR.
END.
ELSE IF pValue > "" THEN DO:
    RUN mm-msg.w("Invalid Employee Number.",0,YES,1,OUTPUT qtyran, OUTPUT answer).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close_h_detail WINDOW-1 
PROCEDURE close_h_detail :
DEFINE INPUT PARAMETER c_complete AS LOGICAL NO-UNDO.
DEFINE VARIABLE c_stop_cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE c_total_sqin AS DECIMAL NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load WINDOW-1  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "mm-look.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    CtrlFrame-2:NAME = "CtrlFrame-2":U
    chWebBrowser = WebBrowser:COM-HANDLE
    UIB_S = chWebBrowser:LoadControls( OCXFile, "WebBrowser":U)
    WebBrowser:NAME = "WebBrowser":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "mm-look.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE currentBatch WINDOW-1 
PROCEDURE currentBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER h_buf FOR h_detail.

    currentBatch = "".
    FOR EACH h_buf NO-LOCK WHERE h_buf.order_no BEGINS "B-"
                            AND (h_buf.finish_time = "" AND h_buf.activity = "D11"):
        
        FIND sign_mm_hdr NO-LOCK WHERE sign_mm_hdr.batchseq = INT(REPLACE(h_buf.order_no,"b-",""))
                                   AND sign_mm_hdr.fbMachine = thisMachine NO-ERROR.
        IF AVAILABLE sign_mm_hdr THEN DO:
            ASSIGN currentBatch = REPLACE(h_buf.order_no,"b-","").
            RELEASE sign_mm_hdr.
            LEAVE.
        END.
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1 
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
    {mgepw.i}
    /* Delete the WINDOW we created */
/*   IF VALID-HANDLE(mm2handle) THEN do: */
/*       RUN DISABLE_UI IN mm2handle.    */
/*   END.                                */

  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doPaperwork WINDOW-1 
PROCEDURE doPaperwork :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER tmpBatch AS CHAR NO-UNDO.

DEFINE VARIABLE cOK        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE allPrinted AS LOGICAL   NO-UNDO.
DEFINE VARIABLE c_prt      AS CHARACTER NO-UNDO.

DEFINE BUFFER buf_so_items    FOR so_items.
DEFINE BUFFER buf_sign_mm_hdr FOR sign_mm_hdr.
DEFINE BUFFER buf_sign_mm_det FOR sign_mm_det.

EMPTY TEMP-TABLE printDet NO-ERROR.

/********************************************************************/
/* Cycle through items of this batch and check to see if            */
/* all of the batches that have that itemseq are completely printed */
/********************************************************************/

FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.batchseq = INT(tmpBatch) BREAK BY sign_mm_det.itemseq:
    IF FIRST-OF(sign_mm_det.itemseq) THEN DO:
        allPrinted = TRUE.
        FOR EACH buf_sign_mm_det NO-LOCK WHERE buf_sign_mm_det.itemseq = sign_mm_det.itemseq,
        EACH buf_sign_mm_hdr OF buf_sign_mm_det NO-LOCK:
            IF buf_sign_mm_hdr.qty <> buf_sign_mm_hdr.qty_printed THEN allPrinted = FALSE.
        END.
        IF allPrinted THEN DO:
            /****commmented out until Job manager up and running*****/
            CREATE printDet.
            ASSIGN printDet.itemseq = sign_mm_det.itemseq.
        END.
    END.
END.

FOR EACH printDet,
EACH so_items OF printdet NO-LOCK:
    
    FIND FIRST zz_file NO-LOCK WHERE zz_file.zz_key1 = "signsowo-" + so_items.so_no + "-" + string(so_items.itemseq) NO-ERROR.

    IF NOT AVAILABLE zz_file THEN DO:
        c_prt = networkshare + "bullseye\prs\signsowo-digital.prs".
    END.

    /******************************************/
    /* Print the pack slip if not yet printed */
    /******************************************/
    IF NOT CAN-FIND(FIRST zz_file WHERE zz_file.zz_key1 = "PACK-PRINTED-" + so_items.so_no) THEN DO:
        c_prt = networkshare + "bullseye\prs\signsopk-digital.prs".
    END.

    RUN soprlog.p (so_items.so_no,8,current-user-id).
    FIND so_file EXCLUSIVE WHERE so_file.so_no = so_items.so_no NO-WAIT NO-ERROR.
    IF AVAILABLE so_file THEN DO:

        IF  so_file.print_date = ? OR
            so_file.print_date = 01/02/50 OR
            so_file.print_date = 01/04/50 THEN
        DO:
           ASSIGN so_file.print_date = TODAY.
        END.
        RELEASE so_file.
   END. /* avail so_file */

    FIND zz_file WHERE zz_file.zz_key1 = "ORDER-RECEIVED-BY-PRODUCTION"
    AND zz_file.zz_key2 = so_items.so_no NO-ERROR.
    IF NOT AVAILABLE zz_file THEN DO:
        CREATE zz_file.
        ASSIGN zz_file.zz_key1    = "ORDER-RECEIVED-BY-PRODUCTION"
               zz_file.zz_key2    = so_items.so_no
               zz_file.zz_char[1] = current-user-id
               zz_file.zz_date[1] = TODAY
               zz_file.zz_dec[1]  = TIME
               zz_file.zz_dec[2]  = so_items.item_no
               zz_file.zz_dec[3]  = so_items.itemseq.       
    END.
    RELEASE zz_file.

END. /* each printDet */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE empRefresh WINDOW-1 
PROCEDURE empRefresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE eDet.
FOR EACH zz_file NO-LOCK WHERE zz_file.zz_key1 = "SIGN-DIGITAL-MACHINE-" + STRING(machineCode),
    EACH employee NO-LOCK WHERE employee.empl_no = entry(1,zz_file.zz_key2," "):
        
        CREATE edet.
        ASSIGN edet.eEmpNo   = zz_file.zz_key2
               edet.eName    = employee.name_1st + " " + employee.name.
    
        IF employee.empl_no = "9995" THEN
        FOR FIRST manpower_zz_file NO-LOCK WHERE manpower_zz_file.zz_key1 = "BADGE:MANPOWER"
            AND manpower_zz_file.zz_key2 = ENTRY(2,zz_file.zz_key2," "):
            ASSIGN edet.eName = CAPS(manpower_zz_file.zz_char[1]).
        END.
END.

FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "Digital" AND zz_file.zz_key2 = "1" NO-ERROR.
IF AVAILABLE zz_file THEN ASSIGN chkDigital1 = zz_file.zz_log[1].

FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "Digital" AND zz_file.zz_key2 = "2" NO-ERROR.
IF AVAILABLE zz_file THEN ASSIGN chkDigital2 = zz_file.zz_log[1].

FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "Digital" AND zz_file.zz_key2 = "3" NO-ERROR.
IF AVAILABLE zz_file THEN ASSIGN chkDigital3 = zz_file.zz_log[1].
RELEASE zz_file NO-ERROR.

DISPLAY chkDigital1 chkDigital2 chkDigital3 WITH FRAME {&FRAME-NAME}.
{&OPEN-QUERY-emp}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableWindow WINDOW-1 
PROCEDURE EnableWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
---------------------------------------------------------------*/
  {enable_w.i}
  CASE proc_type:

  END CASE.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.
  DISPLAY tgl_Show findSo findItem Tg-multibatch chkDigital1 chkDigital2 
          chkDigital3 numRecords cMachTime batchInfo 
      WITH FRAME DEFAULT-FRAME IN WINDOW WINDOW-1.
  ENABLE RECT-8 RECT-83 RECT-84 RECT-85 RECT-86 btnRefresh tgl_Show findSo 
         findItem btnFind chkDigital1 chkDigital2 chkDigital3 HDR emp btn_start 
         btn_stop btnIn JDFQueue btnViewCAD btnEmail btnStartRIP det btnOverlay 
         btnPrev btnNext btnLoc btnFile btnPdf 
      WITH FRAME DEFAULT-FRAME IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY RADIO-cTo cMessage cTo cCC 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RADIO-cTo cMessage btnSend btnCancel cTo 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCorexFlutes WINDOW-1 
PROCEDURE GetCorexFlutes :
/*------------------------------------------------------------------------------
  Purpose: Get the recipe value to send to Prime Center    
  Parameters:  INPUT cItemseq - Current items itemseq
               INPUT-OUTPUT cRecipe - Recipe value for Prime Center
  Notes:       
------------------------------------------------------------------------------*/

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCorexGang WINDOW-1 
PROCEDURE GetCorexGang :
/*------------------------------------------------------------------------------
  Purpose: Get the "Gang" value for prime center   
  Parameters:  INPUT         cItemseq - Current item's itemseq
               INPUT-OUTPUT  cGang    - Gang value that we determine
  Notes:       
------------------------------------------------------------------------------*/

           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gettype WINDOW-1 
PROCEDURE gettype :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER tempsub AS CHARACTER.
DEFINE OUTPUT PARAMETER tmptype AS CHARACTER.
 IF      INDEX(tempsub,"Steel") > 0     THEN tmptype = "Steel".
    ELSE IF INDEX(tempsub,"Poly")  > 0     THEN tmptype = "Poly" .
    ELSE IF INDEX(tempsub,"Corex") > 0     THEN tmptype = "Corex".
    ELSE IF INDEX(tempsub,"Alumalite") > 0 THEN tmptype = "Alumalite".
    ELSE IF INDEX(tempsub,"Aluminum")  > 0 AND INDEX(tempsub, "reflect") = 0 THEN tmptype = tempsub.
    ELSE IF INDEX(tempsub,"OmegaBond") > 0 THEN tmptype = "OmegaBond".
    ELSE IF INDEX(tempsub,"PVC") > 0       THEN tmptype = "PVC".
    ELSE IF INDEX(tempsub,"Corop") > 0     THEN tmptype = "Corex".
    ELSE IF INDEX(tempsub,"Magnetic") > 0  THEN tmptype = "Magnetic".
    ELSE IF INDEX(tempsub,"Decal") > 0     THEN tmptype = "Vinyl".
    ELSE IF INDEX(tempSub, "Vinyl") > 0    THEN tmptype = "Vinyl".
    ELSE IF INDEX(tempsub,"Lexan") > 0     THEN tmptype = "Lexan".
    ELSE IF INDEX(tempsub,"Acrylic") > 0   THEN tmptype = "Acrylic".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GroupCorex WINDOW-1 
PROCEDURE GroupCorex :
/*------------------------------------------------------------------------------
  Purpose: Send Corex items back through Prime Center    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE xmlData      AS CHARACTER NO-UNDO. /*Holds the XML formatting*/
DEFINE VARIABLE cALS         AS INT       NO-UNDO. /*Holds the current ALS Number*/
DEFINE VARIABLE cRecipe      AS CHARACTER NO-UNDO. /*Holds the current recipe*/
DEFINE VARIABLE cSub         AS CHARACTER NO-UNDO. /*Holds the current substrate*/
DEFINE VARIABLE totFiles     AS INT NO-UNDO. /*Total Number of Files*/
DEFINE VARIABLE iALS         AS INTEGER   NO-UNDO. /*Art Link Seq for line item*/
DEFINE VARIABLE cOutputFile  AS CHARACTER NO-UNDO. /*Combined files*/
DEFINE VARIABLE cNewFileLoc  AS CHARACTER NO-UNDO. /*Combined files*/
DEFINE VARIABLE i            AS INT       NO-UNDO. /*counter variable*/

DEFINE VARIABLE Art       AS ArtGenerator. 

    Art = NEW ArtGenerator().
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE holdCheck WINDOW-1 
PROCEDURE holdCheck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iBatch AS INT. 
DEFINE OUTPUT PARAMETER onHold AS LOG.

DEFINE VARIABLE holdSo  AS CHAR NO-UNDO.
DEFINE VARIABLE holdMsg AS CHAR NO-UNDO.


    ASSIGN holdSO  = ""
           holdMsg = "".
    FOR EACH sign_mm_det NO-LOCK WHERE sign_mm_det.batchseq = iBatch,
        EACH so_items OF sign_mm_det NO-LOCK,
        EACH so_file OF so_items NO-LOCK:
            
        IF so_file.hold > "" THEN IF INDEX(holdSO,so_file.so_no) = 0 THEN holdSO = holdSO + (IF holdSO = "" THEN "" ELSE ",") + so_file.so_no.
    END.
    
    IF holdSO > "" THEN DO:
        holdMsg = "Cannot start this batch as the following orders are now on hold:" + CHR(10) + "Orders:" + holdSO.
        RUN mm-msg.w(holdMsg,0,YES,1,OUTPUT qtyran, OUTPUT answer). 
        onHold = YES.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls WINDOW-1 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
chCtrlFrame-2 = chCtrlFrame-2:VPE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laborUpdate WINDOW-1 
PROCEDURE laborUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cEmps    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cBatch   AS CHARACTER NO-UNDO. /* Batch Starting */

    DEFINE VARIABLE bBatchComplete      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE bBatchQuestionAsked AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE bShowRack           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cIndCode            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE availtommorrow      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE clearCnt            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE totalEmps           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE itemseqlist         AS CHARACTER NO-UNDO. 

    DEFINE BUFFER buf_employee FOR employee.
    DEFINE BUFFER h_buf        FOR h_detail.

    ASSIGN current_hrs_id = "".

    RUN logStart ("LaborUpdate Start - Batch: " + cBatch).
    RUN empRefresh. /* Make sure we have the latest employees off all machines */
    
    /*****************************************************************/
    /* What batches are starting?                                    */
    /* Need to start the batch they may have started on this machine */
    /* and the batches that are still running on other machines      */
    /*****************************************************************/

    EMPTY TEMP-TABLE ttBatch.
    IF cBatch > "" THEN DO:
        CREATE ttBatch.
        ASSIGN ttBatch.batchseq = INT(cBatch).
        RUN logStart ("LaborUpdate 1 - Create ttBatch: " + cBatch).
    END.

    FOR EACH h_buf NO-LOCK WHERE h_buf.order_no BEGINS "B-"
                            AND (h_buf.finish_time = "" AND h_buf.activity = "D11")
                            AND REPLACE(h_buf.order_no,"b-","") <> currentBatch:
        
        FIND ttBatch NO-LOCK WHERE ttBatch.batchseq = INT(REPLACE(h_buf.order_no,"b-","")) NO-ERROR.
        IF NOT AVAILABLE ttBatch THEN DO:
            CREATE ttBatch.
            ASSIGN ttBatch.batchseq = INT(REPLACE(h_buf.order_no,"b-","")).
            RUN logStart ("LaborUpdate 2 - Create ttBatch: " + STRING(ttBatch.batchseq)).
        END.
    END.


    /* added this logic for when an employee is clocking off in the */
    /* middle of a run and check to see if others are still working */
    IF bClockOut THEN DO:
        EMPTY TEMP-TABLE ttBatch.
        FOR EACH eDet NO-LOCK:
            totalEmps = totalEmps + 1.
        END.
        IF totalEmps > 1 THEN bBatchQuestionAsked = TRUE. /* fake it out below so it doesn't ask */
    END.

    FOR EACH eDet:

        IF cEmps = "*" OR eDet.eEmp = cEmps THEN DO:

            EMPTY TEMP-TABLE job_det.

            FIND buf_employee NO-LOCK WHERE buf_employee.empl_no = entry(1,eDet.eEmp," ").

            /*****************************************/
            /*          What are we starting?        */
            /*****************************************/
            IF CAN-FIND(FIRST ttBatch) THEN
                FOR EACH ttBatch NO-LOCK:
                    CREATE job_det.
                    ASSIGN job_det.so_no        = "B-" + string(ttBatch.batchseq)
                           job_det.job_activity = "D11"
                           job_det.job_location = "F".
                END.
            ELSE ASSIGN cIndCode = "X11".
            
            /*****************************************/
            /* Create a record to close current jobs */
            /*****************************************/          
            FOR EACH h_detail NO-LOCK WHERE h_detail.empl_no = buf_employee.empl_no
                                        AND h_detail.badge_no = (IF buf_employee.empl_no = "9995" THEN ENTRY(2,eDet.eEmp," ") ELSE "")
                                        AND h_detail.finish_time = "":

                                                                        
                IF h_detail.order_no BEGINS "B-" AND NOT bBatchQuestionAsked
                                                 AND NOT CAN-FIND(ttBatch WHERE ttBatch.batchseq = INT(REPLACE(h_detail.order_no,"b-",""))) THEN DO:

                    RUN currentBatch.

                    ASSIGN bshowrack = YES.

                    IF AVAIL buf_mm_hdr THEN RELEASE buf_mm_hdr.

                    RUN mm-msg.w("Was batch " + currentBatch + " completed?",1,YES,1,OUTPUT qtyran, OUTPUT answer).
                    RUN logStart ("LaborUpdate 5 - Was batch " + currentBatch + "combpleted? - " + STRING(answer)).

                    ASSIGN bBatchQuestionAsked = TRUE
                           bBatchComplete      = answer.

                    FIND tDet WHERE tDet.tBatch = currentBatch NO-ERROR.
                    IF bBatchComplete THEN DO:
                        IF AVAILABLE tdet THEN DO:
                            FIND sign_mm_hdr EXCLUSIVE-LOCK WHERE sign_mm_hdr.batchseq = int(tdet.tbatch) NO-ERROR.
                            IF AVAIL sign_mm_hdr THEN DO:
                                
                                
                                ASSIGN sign_mm_hdr.qty_printed = sign_mm_hdr.qty
                                       sign_mm_hdr.RUN_time    = TIME.
                                RUN logStart ("LaborUpdate 6 - Update Qty_Printed = " + STRING(sign_mm_hdr.qty_printed)).

                                /*FIND LAST WORKED ON ITEM FOR MM-BATCH-ORDER.p*/
                                FIND FIRST zz_file WHERE zz_file.zz_key1 = "JMLASTORDER" AND zz_file.zz_dec[1] = sign_mm_hdr.fbMachine NO-ERROR.
                                IF AVAIL zz_file THEN DELETE zz_file.
                                CREATE zz_file.
                                ASSIGN zz_file.zz_key1   = "JMLASTORDER"           
                                       zz_file.zz_key2   = tDet.tBatch             
                                       zz_file.zz_dec[1] = sign_mm_hdr.fbMachine   
                                       zz_file.zz_dec[2] = sign_mm_hdr.bedseq      
                                       zz_file.zz_char[1] = sign_mm_hdr.matltype   
                                       zz_file.zz_dec[3]  = TIME                   
                                       zz_file.zz_date[1] = TODAY.
                                RELEASE zz_file.
                                
                                RUN CompleteReprint IN mm-pp-handle (sign_mm_hdr.batchseq).
                            
                                IF LOOKUP(sign_mm_hdr.matlType,"Vinyl,Corex 4mm,Corex 10mm,Lexan,Magnetic") = 0 THEN DO:
   
                                    /*create records for clear coat prog*/
                                    IF sign_mm_hdr.bedseq = 0 THEN DO: /*sheet items*/
                                        
                                        RUN logStart ("LaborUpdate 7 - Create SignClearJob").
                                        CREATE signClearJobs. /*batch SCJ*/
                                        ASSIGN signClearJobs.so_no      = "B-" + STRING(sign_mm_hdr.batchseq)
                                               signClearJobs.ITEM_no    = 0
                                               signClearJobs.itemseq    = 0
                                               signClearJobs.crtDate    = TODAY
                                               signClearJobs.crtTime    = TIME
                                               signClearJobs.material   = sign_mm_hdr.matltype
                                               signClearJobs.qty        = sign_mm_hdr.qty_printed
                                               signClearJobs.artlinkseq = 0
                                               signClearJobs.zzchar_1   = "mm-look.w".
                
                                        IF INDEX(sign_mm_hdr.matltype,"Poly") = 0 THEN ASSIGN signClearJobs.availDate = TODAY 
                                                                                              signClearJobs.availTime = TIME.
                                        RELEASE signClearJobs.
                                        
                                    END.
                                    ELSE DO:
                                        
                                        clearCnt = 0.
                                        FOR EACH sign_mm_det OF sign_mm_hdr NO-LOCK BREAK BY sign_mm_det.itemseq BY sign_mm_det.artlinkseq: /*added artlinkseq*/
                                            clearCnt = clearCnt + 1.
                                            IF LAST-OF(sign_mm_det.artlinkseq) THEN DO:
                                                FIND so_items NO-LOCK WHERE so_items.itemseq = sign_mm_det.itemseq NO-ERROR.
                                                IF AVAIL so_items THEN DO:
                                                    FIND signClearJobs EXCLUSIVE-LOCK WHERE signClearJobs.so_no = so_items.so_no AND signClearJobs.ITEM_no = so_items.ITEM_no AND signClearJobs.artlinkseq = sign_mm_det.artlinkseq NO-ERROR.
                                                    IF NOT AVAIL(signClearJobs) THEN DO:
                                                        RUN logStart ("LaborUpdate 8 - Create SignClearJob").
                                                        CREATE signClearJobs.
                                                        ASSIGN signClearJobs.so_no     = so_items.so_no
                                                               signClearJobs.ITEM_no   = so_items.ITEM_no
                                                               signClearJobs.itemseq   = so_items.itemseq
                                                               signClearJobs.artlinkseq = sign_mm_det.artlinkseq
                                                               signClearJobs.crtDate   = TODAY
                                                               signClearJobs.crtTime   = TIME
                                                               signClearJobs.material  = sign_mm_hdr.matltype
                                                               signClearJobs.zzchar_1  = "mm-look.w".
                                                    END.
                                                        
                                                    ASSIGN signClearJobs.qty = signClearJobs.qty + (clearCnt * sign_mm_hdr.qty_printed).
                
                                                    IF INDEX(sign_mm_hdr.matltype,"Poly") = 0 THEN ASSIGN signClearJobs.availDate = TODAY 
                                                                                                          signClearJobs.availTime = TIME.
                                                    RELEASE so_items.
                                                    RELEASE signClearJobs.
                                                END.
                                                clearCnt = 0.
                                            END.
                                        END.
                                    END. /* create signclearjobs */
                                END. /* matltype <> "DECAL" */
                            END.
                            RELEASE sign_mm_hdr.
                        END.
                        
                        IF AVAILABLE tDet AND NOT tDet.materialPosted 
                                          AND tdet.tseq MODULO 2 = 0  THEN DO:
                            
                            RUN mm-msg.w("Do you want to issue the material to the job?",1,YES,1,OUTPUT qtyran, OUTPUT answer).
                            RUN logStart ("LaborUpdate 9 - Issue Material? - " + STRING(answer)).

                            issueIt = answer.
                            IF issueIt THEN DO:
                                RUN logStart ("LaborUpdate 10 - RUN mm-material-issue.p").
                                RUN mm-material-issue.p (int(tDet.tbatch),tDet.materialposted).
                                ASSIGN tDet.materialposted = NOT tDet.materialposted.
                                APPLY "VALUE-CHANGED":U TO BROWSE hdr.
                                RUN logStart ("LaborUpdate 11 - END mm-material-issue.p").
                            END.
                            
                        END.
                        
                         IF AVAIL tDet THEN DO:
                            FIND X_file WHERE x_file.zz_key1 = "MM-SentToRip" AND x_file.zz_key2 = string(tdet.tBatch) NO-ERROR.
                            IF AVAIL x_file THEN DELETE x_file.
                        END.
                        
                    END.
                    ELSE DO:
                        qtyRan = 0.
                        RUN mm-msg.w("How many runs of " + currentBatch + " were completed?",tdet.tqty,NO,1,OUTPUT qtyran, OUTPUT answer).
                        RUN logStart ("LaborUpdate 12 - Runs Completed? - " + STRING(qtyran)).

                        FIND sign_mm_hdr EXCLUSIVE-LOCK WHERE sign_mm_hdr.batchseq = int(tdet.tbatch) NO-ERROR.
                        IF AVAIL sign_mm_hdr THEN DO:
                            IF qtyRan <> 0 THEN ASSIGN sign_mm_hdr.qty_printed = qtyRan
                                                       sign_mm_hdr.RUN_time    = TIME.                           
                            ELSE ASSIGN sign_mm_hdr.run_date = ?.
                            RUN logStart ("LaborUpdate 13 - Update Qty_Printed: " + STRING(qtyRan)).
                        END.
                        RELEASE sign_mm_hdr.
                    END.
                END.
                
            END.

            ItemSeqList = "".
            FOR EACH sign_mm_det WHERE sign_mm_det.batchseq = INT(tDet.tBatch:SCREEN-VALUE IN BROWSE hdr) BREAK BY sign_mm_det.itemseq:
                IF FIRST-OF(sign_mm_det.itemseq) THEN ASSIGN itemseqlist = itemseqlist + (IF itemseqlist = "" THEN "" ELSE ",") + string(sign_mm_det.itemseq).
            END.
            
            RUN logStart ("LaborUpdate 14 - RUN TimeCard2.p").
            RUN timecard2.p (buf_employee.empl_no,IF buf_employee.empl_no = "9995" THEN ENTRY(2,eDet.eEmp," ") ELSE "",bShowRack,cIndCode,"F","","",bBatchComplete,itemseqlist,thisMachine,TABLE job_det).
            RUN logStart ("LaborUpdate 15 - END TimeCard2.p").

        END.
    END.
    

    IF bBatchComplete THEN RUN doPaperwork(currentBatch).


    FIND mm_file EXCLUSIVE-LOCK WHERE mm_file.zz_key1 = "MM-SentToRip" AND mm_file.zz_key2 = currentBatch NO-ERROR.
    IF AVAIL mm_file THEN DELETE mm_file.
    bClockOut = FALSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE logStart WINDOW-1 
PROCEDURE logStart :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER cStep AS CHARACTER NO-UNDO.
DEFINE VARIABLE startFname AS CHAR NO-UNDO.

startFname = "\\fs02\bullseye\Logs\mm-look-start-Printer-" + STRING(thisMachine) + ".txt".

OUTPUT TO VALUE(startFname) APPEND.
PUT UNFORMATTED NOW " " ETIME " " cStep " machine: " OS-GETENV("computername") SKIP.
OUTPUT CLOSE.
             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE movePDF WINDOW-1 
PROCEDURE movePDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER batchNo  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER matType  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER sentfrom AS CHARACTER NO-UNDO. /*new printer change*/

DEFINE VARIABLE fName           AS CHAR NO-UNDO.
DEFINE VARIABLE CurrDir         AS CHAR NO-UNDO.
DEFINE VARIABLE destDir         AS CHAR NO-UNDO.
DEFINE VARIABLE found           AS LOG  NO-UNDO.
DEFINE VARIABLE cTemplate       AS CHAR NO-UNDO.
DEFINE VARIABLE cTemplate2      AS CHAR NO-UNDO.
DEFINE VARIABLE copyTime        AS INT  NO-UNDO.
DEFINE VARIABLE tmpMat          AS CHAR NO-UNDO.
DEFINE VARIABLE lcnt            AS INT  NO-UNDO INITIAL 0.
DEFINE VARIABLE didCopy         AS LOG  NO-UNDO.
DEFINE VARIABLE osCommand       AS CHAR NO-UNDO.
DEFINE VARIABLE jdfOK           AS LOG  NO-UNDO.

DEFINE BUFFER btDet FOR tDet.


ASSIGN fname   = ""
       CurrDir = ""
       destDir = ""
       found   = NO
       didcopy = YES
       lcnt    = 0.
       
            
IF AVAIL tDet THEN DO: 
    IF cMode <> "Reprint" THEN LEAVE.
    
    RUN logstart("SendJDF Start - batch# " + tDet.tBatch).
    IF NOT CAN-FIND(FIRST JDF-Item WHERE JDF-Item.BatchSeq = tDet.tBatch) THEN DO:
        
        CREATE JDF-Item.
        ASSIGN JDF-Item.BatchSeq  = tDet.tBatch
               JDF-Item.BedSize   = tDet.tBed
               JDF-Item.Substrate = tDet.tMat
               JDF-Item.BedSeq    = STRING(tDet.tBedSeq)
               JDF-Item.Qty       = STRING(tDet.tQty)
               JDF-Item.Sides     = tDet.tSides
               JDF-Item.RunSeq    = MTIME
               JDF-Item.Printer   = thisMachine.
        
        CASE SentFrom:
            WHEN "Reprint" THEN JDF-Item.ManualSend = YES.
            WHEN "Template" THEN ASSIGN JDF-Item.ManualSend = YES
                                        JDF-Item.Substrate  = "Template".
        END CASE.
          
                                                              
        /*check if partial bed*/
        IF tDet.tFullBed = NO THEN ASSIGN JDF-Item.Partial = YES.
            
        RELEASE JDF-Item.
    END.  
    
    IF cMode = "reprint" THEN cMode = "".
    
    /*update JDFBrowse*/
    {&OPEN-QUERY-JDFQueue}
    JDFQueue:REFRESH() IN FRAME DEFAULT-FRAME NO-ERROR.
                      
END.
 
IF AVAIL zz_file THEN RELEASE zz_file.           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessLayoutTag WINDOW-1 
PROCEDURE ProcessLayoutTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunProg WINDOW-1 
PROCEDURE RunProg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER sKey  AS CHARACTER NO-UNDO.
{run_prog.i pName (sKey)}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendInfo WINDOW-1 
PROCEDURE SendInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cRecid AS RECID NO-UNDO.

FIND tDet WHERE RECID(tDet) = cRecid NO-ERROR.
RUN PlugAndChug IN mm2Handle(tdet.tBatch,tdet.tFile).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSharedVar WINDOW-1 
PROCEDURE SetSharedVar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p_status AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER EmpNo    AS CHARACTER NO-UNDO.

 FIND FIRST employee NO-LOCK WHERE employee.empl_no = EmpNo NO-ERROR.

 ASSIGN current_hrs_id     = ""
        indirect_code      = (IF p_status BEGINS "indirect" THEN "X6" ELSE "")
        direct_code        = "D11"
        c_emp_sv           = IF AVAIL employee THEN employee.empl_no ELSE ""
        c_emp_name_sv      = IF AVAIL employee THEN (employee.name_1st + " " + employee.NAME) ELSE ""
        c_order_sv         = ""
        c_badge_no         = employee.empl_no
        c_location         = "F".

FOR FIRST h_detail NO-LOCK WHERE h_detail.empl_no = employee.empl_no
    AND h_detail.finish_time = "":
    ASSIGN current_hrs_id = h_detail.hrs_id.
END.
RELEASE h_detail NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartBatch WINDOW-1 
PROCEDURE StartBatch :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE start_indirect WINDOW-1 
PROCEDURE start_indirect :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER EmpNo  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER HrsId  AS CHARACTER NO-UNDO.

      CREATE h_detail.
      ASSIGN h_detail.empl_no     = EmpNo
             h_detail.date        = TODAY
             h_detail.badge_no    = c_badge_no
             h_detail.activity    = "X6"
             h_detail.burden      = 0
             h_detail.finish_time = ""
             h_detail.hrs_id      = HrsId
             h_detail.item_no     = ""
             h_detail.last_mod    = TODAY
             h_detail.location    = "D"
             h_detail.minutes     = 0
             h_detail.oper_seq    = 0
             h_detail.order_no    = ""
             h_detail.pieces      = 0
             h_detail.posted_date = ?
             h_detail.pr_date     = ?
             h_detail.printerpa   = ""
             h_detail.start_time  = current_time
             h_detail.user_id     = c_clientname
             h_detail.Wage        = current_wage
             h_detail.zzchar_1    = ""
             h_detail.zzchar_2    = ""
             h_detail.zzdate_1    = ?
             h_detail.zzdec_1     = 0
             h_detail.zzint_1     = 0
             h_detail.zzlog_1     = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wipeslate WINDOW-1 
PROCEDURE wipeslate :
/*------------------------------------------------------------------------------
  Purpose: to get rid of all images and records to thing that have already been ran
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER deleteNo AS CHAR  NO-UNDO. /*itemseq - ALS#*/

DEFINE VARIABLE cItemseq        AS INT  NO-UNDO.
DEFINE VARIABLE cALS            AS INT  NO-UNDO.
DEFINE VARIABLE fName           AS CHAR NO-UNDO.
DEFINE VARIABLE found           AS LOG  NO-UNDO.
DEFINE VARIABLE currDir         AS CHAR NO-UNDO.
DEFINE VARIABLE cArtFile        AS CHAR NO-UNDO.

ASSIGN cItemseq = INT(ENTRY(1,deleteNo,"-"))
       cALS     = INT(ENTRY(2,deleteNo,"-")).

RUN logStart ("WipeSlate - cItemseq: " + STRING(cItemseq) + " cALS: " + STRING(cALS)). 
DEFINE BUFFER bb_mm_det FOR sign_mm_det.

IF CAN-FIND(FIRST sign_mm_det WHERE sign_mm_det.itemseq = cItemseq AND sign_mm_det.artlinkseq = cALS) THEN DO:
    RUN signbatchremove.p("",cItemseq,cALS,OUTPUT TABLE tBatches). 

    FOR EACH tBatches:
        RUN deleteMMdet.p(tBatches.batchseq,cItemseq,cALS,"mm-look.p","wipeslate",dbase).
    END.
END.
APPLY "Value-Changed":U TO tgl_show IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AlreadySentJDF WINDOW-1 
FUNCTION AlreadySentJDF RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  IF AVAIL JDF-Item THEN DO:
      IF SEARCH("\\lowen\dfssancluster\Bullseye\Logs\JDF\Digitech\Printer\batch" + JDF-Item.BatchSeq + "_" + JDF-Item.BedSize + "_1.xml") <> ?
      OR SEARCH("\\lowen\dfssancluster\Bullseye\Logs\JDF\Digitech\Printer\batch" + JDF-Item.BatchSeq + "_1.xml") <> ? THEN RETURN TRUE.
      ELSE RETURN FALSE.
  END.
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

