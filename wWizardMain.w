&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : wWizardMain.w 
    Purpose     : Main screen for wizard demo

    Author(s)   : Patrick Tingen
    Created     : 2019

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* TT to keep track of tab contents */
DEFINE TEMP-TABLE ttScreen NO-UNDO
  FIELD iScreen AS INTEGER
  FIELD hTab    AS HANDLE 
  FIELD hFrame  AS HANDLE 
  FIELD hProc   AS HANDLE 
  .
DEFINE VARIABLE giCurrentScreen AS INTEGER NO-UNDO.
DEFINE VARIABLE giLockCounter   AS INTEGER NO-UNDO.

{dsWizard.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnFinish rcScreen btnNext btnStep1 btnStep2 ~
btnStep3 btnPrev 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD lockWindow C-Win 
FUNCTION lockWindow RETURNS LOGICAL
  ( plLockWindow AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFinish  NO-FOCUS
     LABEL "&Finish" 
     SIZE 22 BY 1.43.

DEFINE BUTTON btnNext  NO-FOCUS
     LABEL "&Next  >>" 
     SIZE 22 BY 1.43.

DEFINE BUTTON btnPrev  NO-FOCUS
     LABEL "<<  &Back" 
     SIZE 22 BY 1.43.

DEFINE BUTTON btnStep1  NO-FOCUS
     LABEL "&Step 1" 
     SIZE 22 BY 1.43.

DEFINE BUTTON btnStep2  NO-FOCUS
     LABEL "Step 2" 
     SIZE 22 BY 1.43.

DEFINE BUTTON btnStep3  NO-FOCUS
     LABEL "Step 3" 
     SIZE 22 BY 1.43.

DEFINE RECTANGLE rcScreen
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202 BY 15.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnFinish AT ROW 18.38 COL 181 WIDGET-ID 32
     btnNext AT ROW 18.38 COL 158 WIDGET-ID 36
     btnStep1 AT ROW 1.48 COL 2 WIDGET-ID 10
     btnStep2 AT ROW 1.48 COL 24 WIDGET-ID 14
     btnStep3 AT ROW 1.48 COL 46 WIDGET-ID 28
     btnPrev AT ROW 18.38 COL 135 WIDGET-ID 30
     rcScreen AT ROW 2.91 COL 1.6 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204 BY 19.19 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Wizard Demo"
         HEIGHT             = 19.19
         WIDTH              = 204
         MAX-HEIGHT         = 19.19
         MAX-WIDTH          = 204
         VIRTUAL-HEIGHT     = 19.19
         VIRTUAL-WIDTH      = 204
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Wizard Demo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Wizard Demo */
DO:

  MESSAGE 'yeah'
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

  DEFINE VARIABLE lQuit AS LOGICAL NO-UNDO.

  MESSAGE 'Are you sure you want to quit the wizard?'
    VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lQuit.

  IF lQuit = TRUE THEN 
  DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFinish
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFinish C-Win
ON CHOOSE OF btnFinish IN FRAME DEFAULT-FRAME /* Finish */
DO:
  RUN finishWizard.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext C-Win
ON CHOOSE OF btnNext IN FRAME DEFAULT-FRAME /* Next  >> */
DO:
  RUN moveTab(+1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrev C-Win
ON CHOOSE OF btnPrev IN FRAME DEFAULT-FRAME /* <<  Back */
DO:
  RUN moveTab(-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStep1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStep1 C-Win
ON CHOOSE OF btnStep1 IN FRAME DEFAULT-FRAME /* Step 1 */
DO:
  RUN showScreen(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStep2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStep2 C-Win
ON CHOOSE OF btnStep2 IN FRAME DEFAULT-FRAME /* Step 2 */
DO:
  RUN showScreen(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStep3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStep3 C-Win
ON CHOOSE OF btnStep3 IN FRAME DEFAULT-FRAME /* Step 3 */
DO:
  RUN showScreen(3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  RUN initObject.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE btnFinish rcScreen btnNext btnStep1 btnStep2 btnStep3 btnPrev 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE finishWizard C-Win 
PROCEDURE finishWizard :
/* Run all checks on the subscreens
  */
  DEFINE VARIABLE cError   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCurrent AS INTEGER   NO-UNDO.
  
  DEFINE BUFFER bScreen FOR ttScreen. 

  lockWindow(YES).

  /* View all screens again to make sure they 
  ** saved their data to the dataset */
  iCurrent = giCurrentScreen.
  FOR EACH bScreen:
    RUN showScreen(bScreen.iScreen).
  END.  
  
  /* Back to current screen */
  RUN showScreen(iCurrent).

  lockWindow(NO).

  #CheckScreens:
  FOR EACH bScreen:
    
    RUN ScreenValidate IN bScreen.hProc (OUTPUT cError) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND cError <> '' THEN
    DO:
      RUN showScreen(bScreen.iScreen).
      MESSAGE cError VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RUN showScreen(bScreen.iScreen).
      RETURN.
    END.    
  END. /* #CheckScreens */

  /* Save data, run other program etc */
  APPLY 'close' TO THIS-PROCEDURE.

END PROCEDURE. /* finishWizard */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initObject C-Win 
PROCEDURE initObject :
/* Load the screens and show the first one
*/
  DO WITH FRAME {&FRAME-NAME}:

    lockWindow(YES).

    /* Load all screens and attach to a tab button */
    RUN startScreen(1, 'Step 1:name', btnStep1:HANDLE,'wWizardScreen-1.w').
    RUN startScreen(2, 'Step 2:age', btnStep2:HANDLE,'wWizardScreen-2.w').
    RUN startScreen(3, 'Step 3:color', btnStep3:HANDLE,'wWizardScreen-3.w').

    /* Create a record in the dsWizard dataset */
    CREATE ttData.

    RUN showScreen(1).
    lockWindow(NO).
  END.

END PROCEDURE. /* initObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveTab C-Win 
PROCEDURE moveTab :
/* Move a tab to the left or the right
*/
  DEFINE INPUT PARAMETER piDelta AS INTEGER NO-UNDO.
  
  RUN showScreen IN THIS-PROCEDURE (giCurrentScreen + piDelta).

END PROCEDURE. /* moveTab */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showScreen C-Win 
PROCEDURE showScreen :
/* Activate a screen
*/
  DEFINE INPUT PARAMETER piScreenNr AS INTEGER NO-UNDO.
  DEFINE BUFFER bScreen FOR ttScreen. 

  lockWindow(YES).
  
  /* If target screen does not exist, refuse */
  IF NOT CAN-FIND(bScreen WHERE bScreen.iScreen = piScreenNr) THEN RETURN. 
  
  /* Disable old screen */
  FIND bScreen WHERE bScreen.iScreen = giCurrentScreen NO-ERROR. 
  RUN ScreenHide IN bScreen.hProc NO-ERROR.  
  
  giCurrentScreen = piScreenNr.
  
  FOR EACH bScreen:
    bScreen.hTab:Y             = (IF bScreen.iScreen = piScreenNr THEN   1 ELSE 10).
    bScreen.hTab:HEIGHT-PIXELS = (IF bScreen.iScreen = piScreenNr THEN  40 ELSE 30).
    bScreen.hTab:FONT          = (IF bScreen.iScreen = piScreenNr THEN   6 ELSE  ?).
    bScreen.hFrame:VISIBLE     = (IF bScreen.iScreen = piScreenNr THEN YES ELSE NO).
    bScreen.hTab:BGCOLOR       = (IF bScreen.iScreen = piScreenNr THEN   9 ELSE  ?).
    
  END.  

  /* Enable new screen */
  FIND bScreen WHERE bScreen.iScreen = piScreenNr NO-ERROR.
  RUN ScreenShow IN bScreen.hProc NO-ERROR.
  APPLY 'entry' TO bScreen.hFrame.
  
  /* Adjust sensitivity of next/prev buttons */
  DO WITH FRAME {&frame-name}:
    btnPrev:SENSITIVE = CAN-FIND(FIRST bScreen WHERE bScreen.iScreen < piScreenNr).
    btnNext:SENSITIVE = CAN-FIND(FIRST bScreen WHERE bScreen.iScreen > piScreenNr).   
  END.

  FINALLY:
    lockWindow(NO).
  END. 
  
END PROCEDURE. /* showScreen */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startScreen C-Win 
PROCEDURE startScreen :
/* Start the procedure and capture the frame 
*/
  DEFINE INPUT PARAMETER piScreen AS INTEGER   NO-UNDO. 
  DEFINE INPUT PARAMETER pcTitle  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER phTab    AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER pcScreen AS CHARACTER NO-UNDO.

  DEFINE BUFFER bScreen FOR ttScreen. 
  
  DO WITH FRAME {&FRAME-NAME}:
    CREATE bScreen.
    ASSIGN 
      bScreen.iScreen = piScreen
      bScreen.hTab    = phTab
      phTab:LABEL     = pcTitle.
    
    /* Start subscreen inside the tab */
    RUN VALUE(pcScreen) PERSISTENT SET bScreen.hProc
      ( INPUT THIS-PROCEDURE
      , OUTPUT bScreen.hFrame ).  
      
    /* Attach triggers for 'next' */
    ON 'CTRL-TAB'       OF bScreen.hFrame ANYWHERE PERSISTENT RUN moveTab IN THIS-PROCEDURE (+1).
    ON 'CTRL-PAGE-DOWN' OF bScreen.hFrame ANYWHERE PERSISTENT RUN moveTab IN THIS-PROCEDURE (+1).
    ON 'ALT-N'          OF bScreen.hFrame ANYWHERE PERSISTENT RUN moveTab IN THIS-PROCEDURE (+1).

    /* Attach triggers for 'prev' */
    ON 'CTRL-SHIFT-TAB' OF bScreen.hFrame ANYWHERE PERSISTENT RUN moveTab IN THIS-PROCEDURE (-1).
    ON 'CTRL-PAGE-UP'   OF bScreen.hFrame ANYWHERE PERSISTENT RUN moveTab IN THIS-PROCEDURE (-1).
    ON 'ALT-B'          OF bScreen.hFrame ANYWHERE PERSISTENT RUN moveTab IN THIS-PROCEDURE (-1).
    
    /* Finish */
    ON 'ALT-F'          OF bScreen.hFrame ANYWHERE PERSISTENT RUN finishWizard IN THIS-PROCEDURE.
      
    /* Bind our dataset to the screen */  
    RUN ScreenInit IN bScreen.hProc (INPUT-OUTPUT DATASET dsWizard BIND).
      
    bScreen.hFrame:X = rcScreen:X + 1.
    bScreen.hFrame:Y = rcScreen:Y + 1.
  END.   

END PROCEDURE. /* startScreen */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION lockWindow C-Win 
FUNCTION lockWindow RETURNS LOGICAL
  ( plLockWindow AS LOGICAL ) :
  /* Lock/unlock the window, keep track of nested locks/unlocks
  */
  giLockCounter = giLockCounter + (IF plLockWindow THEN 1 ELSE -1).

  IF giLockCounter > 0 THEN 
    RUN lockWindow.p(INPUT {&WINDOW-NAME}:HANDLE, TRUE).
  ELSE 
    RUN lockWindow.p(INPUT {&WINDOW-NAME}:HANDLE, FALSE).

  RETURN TRUE.

END FUNCTION. /* lockWindow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

