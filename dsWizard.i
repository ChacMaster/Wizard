/*------------------------------------------------------------------------
    File        : dsWizard.i
    Purpose     : Dataset definition for wizard test

    Author(s)   : Patrick Tingen
    Created     : 2019
*/

DEFINE TEMP-TABLE ttData NO-UNDO {&REFERENCE-ONLY}
  FIELD cName  AS CHARACTER
  FIELD iAge   AS INTEGER
  FIELD cColor AS CHARACTER
  .
  
DEFINE DATASET dsWizard {&REFERENCE-ONLY} FOR ttData.