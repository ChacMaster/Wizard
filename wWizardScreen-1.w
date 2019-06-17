&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : wWizardScreen-1.w 
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
&Scoped-Define ENABLED-OBJECTS fiName 
&Scoped-Define DISPLAYED-OBJECTS fiName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiName AS CHARACTER FORMAT "X(256)":U 
     LABEL "What is your name" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiName AT ROW 3.62 COL 25 COLON-ALIGNED WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 73.6 BY 6.95 WIDGET-ID 100.


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
         HEIGHT             = 7.24
         WIDTH              = 74.8
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
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


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
  DISPLAY fiName 
      WITH FRAME DEFAULT-FRAME.
  ENABLE fiName 
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
    bData.cName = fiName:SCREEN-VALUE.
    
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
    fiName:SCREEN-VALUE = bData.cName.

    APPLY 'ENTRY' TO fiName.
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
  
    IF fiName:SCREEN-VALUE = '' THEN 
      pcError = 'Please enter your name'.
    
  END.

END PROCEDURE. /* ScreenValidate */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME