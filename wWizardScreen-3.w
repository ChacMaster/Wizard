&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : wWizardScreen-3.w 
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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 ~
RECT-7 
&Scoped-Define DISPLAYED-OBJECTS fiColor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiColor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Color chosen" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .95
     BGCOLOR 15 FGCOLOR 9 FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 9.8 BY 2
     BGCOLOR 9 FGCOLOR 9 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 9.8 BY 2
     BGCOLOR 2 FGCOLOR 2 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 9.8 BY 2
     BGCOLOR 12 FGCOLOR 12 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 9.8 BY 2
     BGCOLOR 14 FGCOLOR 14 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 9.8 BY 2
     BGCOLOR 13 FGCOLOR 13 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 9.8 BY 2
     BGCOLOR 11 FGCOLOR 11 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 9.8 BY 2
     BGCOLOR 7 FGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiColor AT ROW 6 COL 31 COLON-ALIGNED WIDGET-ID 24
     "What is your favourite color:" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 1.95 COL 7 WIDGET-ID 22
     RECT-1 AT ROW 2.91 COL 40 WIDGET-ID 28
     RECT-2 AT ROW 2.91 COL 30 WIDGET-ID 30
     RECT-3 AT ROW 2.91 COL 10 WIDGET-ID 32
     RECT-4 AT ROW 2.91 COL 20 WIDGET-ID 34
     RECT-5 AT ROW 2.91 COL 50 WIDGET-ID 36
     RECT-6 AT ROW 2.91 COL 60 WIDGET-ID 38
     RECT-7 AT ROW 2.91 COL 70 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.8 BY 7.38 WIDGET-ID 100.


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
         HEIGHT             = 7.71
         WIDTH              = 85.2
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
/* SETTINGS FOR FILL-IN fiColor IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiColor:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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


&Scoped-define SELF-NAME RECT-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RECT-1 C-Win
ON MOUSE-SELECT-CLICK OF RECT-1 IN FRAME DEFAULT-FRAME
DO:
  fiColor:SCREEN-VALUE = 'blue'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RECT-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RECT-2 C-Win
ON MOUSE-SELECT-CLICK OF RECT-2 IN FRAME DEFAULT-FRAME
DO:
  fiColor:SCREEN-VALUE = 'green'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RECT-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RECT-3 C-Win
ON MOUSE-SELECT-CLICK OF RECT-3 IN FRAME DEFAULT-FRAME
DO:
  fiColor:SCREEN-VALUE = 'red'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RECT-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RECT-4 C-Win
ON MOUSE-SELECT-CLICK OF RECT-4 IN FRAME DEFAULT-FRAME
DO:
  fiColor:SCREEN-VALUE = 'yellow'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RECT-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RECT-5 C-Win
ON MOUSE-SELECT-CLICK OF RECT-5 IN FRAME DEFAULT-FRAME
DO:
  fiColor:SCREEN-VALUE = 'violet'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RECT-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RECT-6 C-Win
ON MOUSE-SELECT-CLICK OF RECT-6 IN FRAME DEFAULT-FRAME
DO:
  fiColor:SCREEN-VALUE = 'cyan'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RECT-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RECT-7 C-Win
ON MOUSE-SELECT-CLICK OF RECT-7 IN FRAME DEFAULT-FRAME
DO:
  fiColor:SCREEN-VALUE = 'gray'.
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
  DISPLAY fiColor 
      WITH FRAME DEFAULT-FRAME.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 
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
    bData.cColor = fiColor:SCREEN-VALUE.
    
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
    fiColor:SCREEN-VALUE = bData.cColor.

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
  
  DO WITH FRAME {&frame-name}:
  
    IF fiColor:SCREEN-VALUE = '' THEN 
      pcError = 'Please enter your favourite color'.
    
  END.

END PROCEDURE. /* ScreenValidate */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME