&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : wWizardScreen-2.w 
    Purpose     : Demo for wizard

    Author(s)   : Patrick Tingen
    Created     : 2019

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{dsWizard.i &reference-only=reference-only}

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER phParent AS HANDLE NO-UNDO.
DEFINE OUTPUT PARAMETER phFrame  AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiAge btn7 btn8 btn9 btn4 btn5 btn6 btn1 ~
btn2 btn3 btn0 btnBack 
&Scoped-Define DISPLAYED-OBJECTS fiAge 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn0 
     LABEL "0" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btn1 
     LABEL "1" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btn2 
     LABEL "2" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btn3 
     LABEL "3" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btn4 
     LABEL "4" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btn5 
     LABEL "5" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btn6 
     LABEL "6" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btn7 
     LABEL "7" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btn8 
     LABEL "8" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btn9 
     LABEL "9" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btnBack 
     LABEL "<-" 
     SIZE 11 BY 1.14.

DEFINE VARIABLE fiAge AS CHARACTER FORMAT "X(256)":U 
     LABEL "What is your age" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiAge AT ROW 2.67 COL 21 COLON-ALIGNED WIDGET-ID 34
     btn7 AT ROW 2.67 COL 42 WIDGET-ID 24
     btn8 AT ROW 2.67 COL 48 WIDGET-ID 26
     btn9 AT ROW 2.67 COL 54 WIDGET-ID 28
     btn4 AT ROW 4.1 COL 42 WIDGET-ID 18
     btn5 AT ROW 4.1 COL 48 WIDGET-ID 20
     btn6 AT ROW 4.1 COL 54 WIDGET-ID 22
     btn1 AT ROW 5.52 COL 42 WIDGET-ID 2
     btn2 AT ROW 5.52 COL 48 WIDGET-ID 16
     btn3 AT ROW 5.52 COL 54 WIDGET-ID 14
     btn0 AT ROW 6.95 COL 42 WIDGET-ID 30
     btnBack AT ROW 6.95 COL 48 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.2 BY 8.05 WIDGET-ID 100.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 8.24
         WIDTH              = 64.4
         MAX-HEIGHT         = 20.14
         MAX-WIDTH          = 135.2
         VIRTUAL-HEIGHT     = 20.14
         VIRTUAL-WIDTH      = 135.2
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       fiAge:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO phParent.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn0 C-Win
ON CHOOSE OF btn0 IN FRAME DEFAULT-FRAME /* 0 */
, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9
DO:
  fiAge:SCREEN-VALUE = LEFT-TRIM(fiAge:SCREEN-VALUE + SELF:LABEL,'0').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBack C-Win
ON CHOOSE OF btnBack IN FRAME DEFAULT-FRAME /* <- */
DO:
  IF LENGTH(fiAge:SCREEN-VALUE) > 1 THEN 
    fiAge:SCREEN-VALUE = SUBSTRING(fiAge:SCREEN-VALUE, LENGTH(fiAge:SCREEN-VALUE) - 1).
  ELSE 
    fiAge:SCREEN-VALUE = ''.
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
  phFrame = FRAME {&FRAME-NAME}:HANDLE.
  
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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
  DISPLAY fiAge 
      WITH FRAME DEFAULT-FRAME.
  ENABLE fiAge btn7 btn8 btn9 btn4 btn5 btn6 btn1 btn2 btn3 btn0 btnBack 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenHide C-Win 
PROCEDURE ScreenHide :
/* Update tt with info from screen
*/
  DEFINE BUFFER bData FOR ttData.

  DO WITH FRAME {&frame-name}:
  
    FIND bData.    
    bData.iAge = INTEGER(fiAge:SCREEN-VALUE) NO-ERROR.
    
  END.

END PROCEDURE. /* ScreenHide */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenInit C-Win 
PROCEDURE ScreenInit :
/* Initialize the screen and bind dataset
*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsWizard BIND.

  
END PROCEDURE. /* initScreen */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenShow C-Win 
PROCEDURE ScreenShow :
/* Get latest info from tt and show in screen
*/
  DEFINE BUFFER bData FOR ttData.

  DO WITH FRAME {&frame-name}:
  
    FIND bData.       
    fiAge:SCREEN-VALUE = STRING(bData.iAge).

  END.
  
END PROCEDURE. /* ScreenShow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenValidate C-Win 
PROCEDURE ScreenValidate :
/* Check the contents of the screen when the 
** user finishes the wizard.
*/
  DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iAge AS INTEGER   NO-UNDO.
  
  DO WITH FRAME {&frame-name}:
  
    ASSIGN fiAge.
    iAge = INTEGER(fiAge) NO-ERROR.

    IF iAge = ? OR iAge = 0 OR iAge > 100 THEN 
    DO:
      pcError = 'Please enter a valid age'.
      RETURN. 
    END.

    IF iAge < 18 THEN 
    DO:
      pcError = 'You must be over 18 to use this wizard demo'.
      RETURN. 
    END.
  END.

END PROCEDURE. /* ScreenValidate */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME