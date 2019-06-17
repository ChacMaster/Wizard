# Wizard
Demo for a wizard in Progress 4GL

This is a proof of concept for a simple wizard.
The wizard has 3 steps. If you want to use is, just open
the source and check the steps since all is very easy.

The main window initiated the subscreens and handles navigation. The subscreens
themselves take care of showing, saving and validating their part of the data. The
data is shared among the subscreens via a bound dataset.

There are 4 predefined routines in each subscreen:
1) ScreenInit
This routine is called only once; when the subscreen is started. 

2) ScreenShow
Whenever the subscreen comes into view, this procedure is called. Use it to populate the data on the screen since other tabs might have changed it. 

3) ScreenHide
When the subscreen goes out of view, use this procedure to save the data in your dataset. This is important because other tabs might depend on it. 

4) ScreenValidate
This is called when the user presses 'Finish' on the main screen. It returns either a string indicating errors on the screen, empty when no error encountered. 