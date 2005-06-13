// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.views.mbview;


/** <p>implementors provides information about user interface settings on
 * the Module Browser.</p>
  * 
  * @author Leif Frenzel
  */
public interface IUIState {

  boolean isFlatLayout();
  
  void toggleLayout();
}