&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
  DEFINE INPUT PARAMETER start-key   AS CHAR      NO-UNDO.
  DEFINE INPUT PARAMETER mmHandle    AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER mmColor     AS INT       NO-UNDO.
&ELSE
  {glob_var.i "new" "new"}
  DEFINE VARIABLE start-key AS CHAR                NO-UNDO.
  DEFINE VARIABLE mmHandle  AS HANDLE              NO-UNDO.
  DEFINE VARIABLE mmColor   AS INT                 NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE find-recid         AS RECID                    NO-UNDO.
DEFINE VARIABLE appl               AS CHARACTER INITIAL "SO"   NO-UNDO.
DEFINE VARIABLE add_rec            AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE startTime          AS INTEGER                  NO-UNDO.
DEFINE VARIABLE cTime              AS CHAR                     NO-UNDO.
DEFINE VARIABLE tmpColor           AS INT                      NO-UNDO.
DEFINE VARIABLE tmpint             AS INT                      NO-UNDO.
DEFINE VARIABLE tryCnt             AS INT                      NO-UNDO.
DEFINE VARIABLE mm-pp-han          AS HANDLE                   NO-UNDO.

/* {imageShare.i} */

DEFINE VARIABLE DropFileName       AS CHARACTER                NO-UNDO.

SESSION:DATA-ENTRY-RETURN = TRUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cBatch cMat cQty cTemp cTimeLeft cTotalTime ~
FindPanels1 RECT-86 
&Scoped-Define DISPLAYED-OBJECTS cBatch cMat cQty cTemp cTimeLeft ~
cTotalTime FindPanels1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU MENU-BAR-WINDOW-1 MENUBAR
       MENU-ITEM m_Close        LABEL "Close"         .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE WebBrowser AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chWebBrowser AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cBatch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Batch" 
      VIEW-AS TEXT 
     SIZE 26.8 BY 1.38
     FONT 46 NO-UNDO.

DEFINE VARIABLE cMat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Material" 
      VIEW-AS TEXT 
     SIZE 70.4 BY 1.38
     FONT 46 NO-UNDO.

DEFINE VARIABLE cQty AS CHARACTER FORMAT "X(256)":U 
     LABEL "Qty" 
      VIEW-AS TEXT 
     SIZE 40.4 BY 1.86
     FONT 46 NO-UNDO.

DEFINE VARIABLE cTemp AS CHARACTER FORMAT "X(256)":U 
     LABEL "Template" 
      VIEW-AS TEXT 
     SIZE 40 BY 1.38
     FONT 46 NO-UNDO.

DEFINE VARIABLE cTimeLeft AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
      VIEW-AS TEXT 
     SIZE 18.2 BY 1.81
     FONT 45 NO-UNDO.

DEFINE VARIABLE cTotalTime AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 20 BY 1.76
     FONT 45 NO-UNDO.

DEFINE VARIABLE FindPanels1 AS CHARACTER FORMAT "X(256)":U INITIAL "Precut Panels" 
      VIEW-AS TEXT 
     SIZE 32.2 BY 1.76
     FGCOLOR 12 FONT 43 NO-UNDO.

DEFINE RECTANGLE RECT-86
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 268.8 BY .19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cBatch AT ROW 4.67 COL 15.8 WIDGET-ID 6
     cMat AT ROW 6.76 COL 10.2 WIDGET-ID 8
     cQty AT ROW 8.81 COL 20.6 WIDGET-ID 10
     cTemp AT ROW 10.86 COL 8.2 WIDGET-ID 12
     cTimeLeft AT ROW 11.52 COL 145 RIGHT-ALIGNED WIDGET-ID 16
     cTotalTime AT ROW 11.57 COL 150.6 NO-LABEL WIDGET-ID 58
     FindPanels1 AT ROW 8.95 COL 136.2 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     "Printing" VIEW-AS TEXT
          SIZE 25.6 BY 2.1 AT ROW 1.24 COL 72.2 WIDGET-ID 46
          FONT 46
     "/" VIEW-AS TEXT
          SIZE 3 BY 1.91 AT ROW 11.48 COL 146.6 WIDGET-ID 56
          FONT 45
     RECT-86 AT ROW 3.43 COL 1 WIDGET-ID 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1.05
         SIZE 272 BY 36.57
         FONT 44.


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
         COLUMN             = 173.8
         ROW                = 2.57
         HEIGHT             = 34.48
         WIDTH              = 272.6
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN cBatch IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       cBatch:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN cMat IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       cMat:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN cQty IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       cQty:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN cTemp IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       cTemp:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN cTimeLeft IN FRAME DEFAULT-FRAME
   ALIGN-R                                                              */
ASSIGN 
       cTimeLeft:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN cTotalTime IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       cTotalTime:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FindPanels1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       FindPanels1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.33
       COLUMN          = 155.8
       HEIGHT          = 1.71
       WIDTH           = 8.4
       WIDGET-ID       = 60
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME WebBrowser ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 14.57
       COLUMN          = 4
       HEIGHT          = 20.71
       WIDTH           = 265.8
       WIDGET-ID       = 44
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
/* WebBrowser OCXINFO:CREATE-CONTROL from: {8856F961-340A-11D0-A96B-00C04FD705A2} type: WebBrowser */

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


&Scoped-define SELF-NAME CtrlFrame-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 WINDOW-1 OCX.Tick
PROCEDURE CtrlFrame-2.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    
    IF startTime <> 0 THEN DO:
        tmpColor = IF TIME - startTime >= tmpInt THEN 12 ELSE 9.
        cTime = STRING(TIME - startTime,"HH:MM:SS").
        cTime = SUBSTRING(cTime,4).
        
        ASSIGN cTimeLeft:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cTime
               cTimeLeft:FGCOLOR IN FRAME {&FRAME-NAME}      = tmpColor. 
    
    END.
    ELSE ASSIGN cTimeLeft:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END PROCEDURE.

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
/*   IF VALID-HANDLE(mmHandle) THEN  */
/*       RUN DISABLE_UI IN mmHandle. */
  RUN disable_UI.
END.
/* /* These events will close the window and terminate the procedure.      */ */
/* /* (NOTE: this will override any user-defined triggers previously       */ */
/* /*  defined on the window.)                                             */ */
/* ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:                                      */
/*   IF VALID-HANDLE(mmHandle) THEN                                           */
/*       APPLY "CLOSE":U TO mmHandle.                                         */
/*   APPLY "CLOSE":U TO THIS-PROCEDURE.                                       */
/*   RETURN NO-APPLY.                                                         */
/* END.                                                                       */
/*                                                                            */
/* ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:                        */
/*   IF VALID-HANDLE(mmHandle) THEN                                           */
/*       APPLY "CLOSE":U TO mmHandle.                                         */
/*   APPLY "CLOSE" TO THIS-PROCEDURE.                                         */
/*   RETURN NO-APPLY.                                                         */
/* END.                                                                       */


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

  IF VALID-HANDLE(mmHandle) THEN bg-color = mmColor.

  ASSIGN  {&WINDOW-NAME}      :TITLE   = w-title
          FRAME {&FRAME-NAME}:BGCOLOR = bg-color
          FRAME {&FRAME-NAME}:FGCOLOR = fg-color.

  RUN enable_UI.
  {mgx_proc.i}
  {mm.i}

  FIND zz_file NO-LOCK WHERE zz_file.zz_key1 = "mm-look2.w" AND zz_file.zz_key2 = current-user-id NO-ERROR.
  IF AVAILABLE zz_file THEN DO:
      ASSIGN {&WINDOW-NAME}:X = zz_file.zz_dec[3]
             {&WINDOW-NAME}:Y = zz_file.zz_dec[4]
             {&WINDOW-NAME}:HEIGHT-PIXELS = zz_file.zz_dec[1] /*738*/
             {&WINDOW-NAME}:WIDTH-PIXELS  = zz_file.zz_dec[2] /*1360*/ .

      RELEASE zz_file.

  END.


  /*manually set these b/c they always change back*/
  chWebBrowser:WebBrowser:AddressBar = FALSE.
  chWebBrowser:WebBrowser:MENUBAR    = FALSE.
  chWebBrowser:WebBrowser:StatusBar  = FALSE.
  chWebBrowser:WebBrowser:ToolBar    = 0.

  SESSION:SUPPRESS-WARNINGS = YES.
  RUN mm-pp.p PERSISTENT SET mm-pp-han.

/*   IF VALID-HANDLE(mmHandle) THEN RUN setHandle IN mmHandle(THIS-PROCEDURE). */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

OCXFile = SEARCH( "mm-look2.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    CtrlFrame-2:NAME = "CtrlFrame-2":U
    chWebBrowser = WebBrowser:COM-HANDLE
    UIB_S = chWebBrowser:LoadControls( OCXFile, "WebBrowser":U)
    WebBrowser:NAME = "WebBrowser":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "mm-look2.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
      FIND zz_file EXCLUSIVE-LOCK WHERE zz_file.zz_key1 = "mm-look2.w" AND zz_file.zz_key2 = current-user-id NO-WAIT NO-ERROR.
      IF AVAILABLE zz_file THEN DO:
          ASSIGN zz_file.zz_dec[1] = {&WINDOW-NAME}:HEIGHT-PIXELS
                 zz_file.zz_dec[2] = {&WINDOW-NAME}:WIDTH-PIXELS 
                 zz_file.zz_dec[3] = {&WINDOW-NAME}:X 
                 zz_file.zz_dec[4] = {&WINDOW-NAME}:Y.
      END.
    
      IF NOT CAN-FIND(FIRST zz_file WHERE zz_file.zz_key1 = "mm-look2.w" AND zz_file.zz_key2 = current-user-id) THEN DO:
          CREATE zz_file.
          ASSIGN zz_file.zz_key1   = "mm-look2.w"
                 zz_file.zz_key2   = current-user-id
                 zz_file.zz_dec[1] = {&WINDOW-NAME}:HEIGHT-PIXELS
                 zz_file.zz_dec[2] = {&WINDOW-NAME}:WIDTH-PIXELS 
                 zz_file.zz_dec[3] = {&WINDOW-NAME}:X 
                 zz_file.zz_dec[4] = {&WINDOW-NAME}:Y.
      END.
      RELEASE zz_file.
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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
  DISPLAY cBatch cMat cQty cTemp cTimeLeft cTotalTime FindPanels1 
      WITH FRAME DEFAULT-FRAME IN WINDOW WINDOW-1.
  ENABLE cBatch cMat cQty cTemp cTimeLeft cTotalTime FindPanels1 RECT-86 
      WITH FRAME DEFAULT-FRAME IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE plugAndChug WINDOW-1 
PROCEDURE plugAndChug :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER vBatch  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER vFile   AS CHAR NO-UNDO.


DEFINE VARIABLE CurrCnt     AS INT        NO-UNDO.
DEFINE VARIABLE PrepCnt     AS INT        NO-UNDO.
DEFINE VARIABLE NextCnt     AS INT        NO-UNDO.
DEFINE VARIABLE tmpTime     AS CHAR       NO-UNDO.
DEFINE VARIABLE newSeq      AS INT        NO-UNDO.
DEFINE VARIABLE tmpChar     AS CHAR       NO-UNDO.
DEFINE VARIABLE tmpMat      AS CHAR       NO-UNDO.
DEFINE VARIABLE CurrBed     AS CHAR       NO-UNDO.
DEFINE VARIABLE saveMat     AS CHAR       NO-UNDO.
DEFINE VARIABLE currInv     AS CHAR       NO-UNDO.
DEFINE VARIABLE prepInv     AS CHAR       NO-UNDO.
DEFINE VARIABLE nextInv     AS CHAR       NO-UNDO.
DEFINE VARIABLE jpgFile     AS CHAR       NO-UNDO.
DEFINE VARIABLE PreviewVPE  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE oMinutes    AS INTEGER    NO-UNDO.
DEFINE VARIABLE oMachMins   AS INTEGER    NO-UNDO.
DEFINE VARIABLE detCnt      AS INTEGER    NO-UNDO.
DEFINE VARIABLE currInvPart AS CHAR       NO-UNDO.
DEFINE VARIABLE tmpH        AS INT        NO-UNDO.
DEFINE VARIABLE tmpW        AS INT        NO-UNDO.
DEFINE VARIABLE hasBleed    AS LOG        NO-UNDO.


                               
DEFINE BUFFER b_mm_hdr   FOR sign_mm_hdr.
DEFINE BUFFER buf_mm_hdr FOR sign_mm_hdr.
DEFINE BUFFER b_mm_det   FOR sign_mm_det.
DEFINE BUFFER buf_mm_det FOR sign_mm_det.

{VPE.I}

    
FIND buf_mm_hdr NO-LOCK WHERE buf_mm_hdr.batchseq = int(vBatch) NO-ERROR.
IF AVAIL buf_mm_hdr THEN DO:
    
    detCnt = 0.
    FOR EACH sign_mm_det OF buf_mm_hdr NO-LOCK:
        detCnt = detCnt + 1.
    END.
    
    FIND signbed NO-LOCK WHERE signbed.seq = buf_mm_hdr.bedseq NO-ERROR.
    FIND LAST sign_mm_det NO-LOCK OF buf_mm_hdr NO-ERROR.
    FIND sitepart NO-LOCK WHERE sitepart.site = "7" AND sitepart.part_no = buf_mm_hdr.inv_part NO-ERROR.
    
    tmpMat = "".
    IF AVAIL(sign_mm_det) THEN DO:
        ASSIGN tmpmat      = sign_mm_det.inv_part
               currInvPart = sign_mm_det.inv_part.
               
        FIND FIRST squ_ptdet NO-LOCK WHERE squ_ptdet.itemseq = sign_mm_det.itemseq NO-ERROR.
        IF AVAIL squ_ptdet THEN DO:
            RUN checkForBleed IN mm-pp-han(squ_ptdet.subseq,OUTPUT hasBleed).
            IF hasBleed THEN
                ASSIGN findPanels1:VISIBLE IN FRAME {&FRAME-NAME} = TRUE
                       findPanels1:FGCOLOR IN FRAME {&FRAME-NAME} = 12
                       findPanels1:FGCOLOR IN FRAME {&FRAME-NAME} = 12.
            ELSE 
                ASSIGN findPanels1:HIDDEN  IN FRAME {&FRAME-NAME} = TRUE
                       findPanels1:FGCOLOR IN FRAME {&FRAME-NAME} = ?
                       findPanels1:FGCOLOR IN FRAME {&FRAME-NAME} = ?.
 
            RELEASE squ_ptdet.
        END.
    END.
    IF AVAIL(sign_mm_det) AND AVAIL(sitepart) THEN tmpmat = tmpmat + " " + "(" + sitepart.bin + ")".

    CurrCnt = 0.
    IF (AVAIL signbed AND INDEX(signbed.matrlType,"steel") = 0) OR (NOT AVAIL signbed) THEN DO:
        FOR EACH b_mm_hdr NO-LOCK WHERE b_mm_hdr.run_time = ? AND b_mm_hdr.inv_part = buf_mm_hdr.inv_part:
            CurrCnt = CurrCnt + b_mm_hdr.qty - b_mm_hdr.qty_printed.
        END.
    END.
    ELSE DO:
        FOR EACH b_mm_hdr NO-LOCK WHERE b_mm_hdr.bedseq   = buf_mm_hdr.bedseq 
                                    AND b_mm_hdr.matlType = buf_mm_hdr.matlType
                                    AND b_mm_hdr.inv_part = buf_mm_hdr.inv_part:

            IF (b_mm_hdr.RUN_time = ? OR CAN-FIND(FIRST h_detail NO-LOCK WHERE h_detail.order_no = "B-" + vBatch)) AND b_mm_hdr.runseq MODULO 2 = 0 THEN DO:
                FOR LAST b_mm_det NO-LOCK OF b_mm_hdr:
                    currCnt = CurrCnt + (b_mm_det.POSITION * (b_mm_hdr.qty - b_mm_hdr.qty_printed)).
                END.
            END.

        END.
        
    END.

    ASSIGN tmpH = 0
           tmpW = 0.
    IF AVAIL signbed THEN ASSIGN tmpH = signbed.imageHeight
                                 tmpW = signbed.imageWidth.
    ELSE ASSIGN tmpH = 96
                tmpW = 48.
                                                                                           
    ASSIGN tmpTime = STRING(((int(buf_mm_hdr.mach_time) * 60) * (buf_mm_hdr.qty - buf_mm_hdr.qty_printed)),"hh:mm:ss")
           tmpInt  = ((buf_mm_hdr.mach_time * 60) * (buf_mm_hdr.qty - buf_mm_hdr.qty_printed)).


    ASSIGN cBatch    :SCREEN-VALUE IN FRAME  {&FRAME-NAME} = STRING(buf_mm_hdr.batchseq)
           cMat      :SCREEN-VALUE IN FRAME  {&FRAME-NAME} = IF buf_mm_hdr.runseq MODULO 2 <> 0 THEN "" ELSE tmpMat
           cQty      :SCREEN-VALUE IN FRAME  {&FRAME-NAME} = IF buf_mm_hdr.runseq MODULO 2 <> 0 THEN "1 - Template" ELSE (STRING(buf_mm_hdr.qty) + " - " + (IF buf_mm_hdr.sides = 2 THEN "D/F" ELSE "S/F"))
           cTemp     :SCREEN-VALUE IN FRAME  {&FRAME-NAME} = IF buf_mm_hdr.runseq MODULO 2 <> 0 THEN "" ELSE (IF AVAIL signbed THEN STRING(signBed.imageHeight) + " x " + string(signBed.imageWidth) ELSE "")
           cTotalTime:SCREEN-VALUE IN FRAME  {&FRAME-NAME} = SUBSTRING(tmpTime,4) /*tmpTime*/
           newseq  = buf_mm_hdr.runseq
           tmpChar = STRING(buf_mm_hdr.batchseq)
           CurrBed = STRING(buf_mm_hdr.bedseq).

    
    IF vFile <> "" THEN DO:
        SESSION:SET-WAIT-STATE("General").

        chWebBrowser :VISIBLE = TRUE.
        chWebBrowser :WebBrowser:Navigate(vFile).

        SESSION:SET-WAIT-STATE("").
    END.
    ELSE DO:
        ASSIGN webBrowser:HIDDEN = TRUE.
    END.
    
END.
RELEASE buf_mm_hdr.



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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunTime WINDOW-1 
PROCEDURE RunTime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cTime AS INTEGER NO-UNDO.

ASSIGN startTime = cTime.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

