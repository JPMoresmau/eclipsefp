// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.coview;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.widgets.Display;

import net.sf.eclipsefp.haskell.core.compiler.*;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.preferences.IPreferenceConstants;


/** <p>Document object for the CompilerOutputView.</p>
  * 
  * @author Leif Frenzel
  */
public class CompilerOutputDocument extends Document 
                                    implements ICompilerOutputListener,
                                               IPropertyChangeListener {

  public CompilerOutputDocument() {
    super(   "The compiler output appears here.\n"
           + "(If you have turned it on in the Preferences.)" );
    if( isListening() ) {
      CompilerManager.getInstance().addCompilerOutputListener( this );
    }
    getPrefStore().addPropertyChangeListener( this );
  }
  
  public void dispose() {
    getPrefStore().removePropertyChangeListener( this );
    if( isListening() ) {
      CompilerManager.getInstance().removeCompilerOutputListener( this );
    }
  }
  
  
  // interface methods of ICompilerOutputListener
  ///////////////////////////////////////////////
  
  public void outputProduced( final ICompilerOutput producedOutput ) {
    Runnable rb = new Runnable() {
      public void run() {
        set( producedOutput.getOutput() + producedOutput.getErrors() );
      }
    };
    Display.getDefault().syncExec( rb );
  }


  // interface methods of IPropertyChangeListener
  ///////////////////////////////////////////////
  
  public void propertyChange( final PropertyChangeEvent event ) {
    if( event.getProperty().equals( IPreferenceConstants.SHOW_COMPILER_LOG ) ) {
      if( isListening() ) {
        CompilerManager.getInstance().addCompilerOutputListener( this );
      } else {
        CompilerManager.getInstance().removeCompilerOutputListener( this );
      }
    }
  }


  // helping methods
  //////////////////
  
  private IPreferenceStore getPrefStore() {
    return HaskellUIPlugin.getDefault().getPreferenceStore();
  }
  
  private boolean isListening() {
    return getPrefStore().getBoolean( IPreferenceConstants.SHOW_COMPILER_LOG ); 
  }
}