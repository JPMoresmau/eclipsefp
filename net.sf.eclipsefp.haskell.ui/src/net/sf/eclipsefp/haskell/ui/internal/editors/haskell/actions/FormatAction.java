/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions;

import java.io.File;
import java.util.ResourceBundle;
import net.sf.eclipsefp.haskell.style.stylishhaskell.StylishHaskell;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;


/**
 * This action formats the current document with stylish haskell
 * @author JP Moresmau
 *
 */
public class FormatAction extends TextEditorAction {

  /**
   * @param bundle
   * @param prefix
   * @param editor
   */
  public FormatAction( final ResourceBundle bundle, final String prefix, final ITextEditor editor ) {
    super( bundle, prefix, editor );
  }

  /**
   * @param bundle
   * @param prefix
   * @param editor
   * @param style
   */
  public FormatAction( final ResourceBundle bundle, final String prefix,
      final ITextEditor editor, final int style ) {
    super( bundle, prefix, editor, style );
  }


  @Override
  public void update() {
    super.update();
    if (!isEnabled()) {
      return;
    }
    if (!canModifyEditor()) {
      setEnabled( false );
      return;
    }
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.action.Action#run()
   */
  @Override
  public void run() {
    ITextEditor editor = getTextEditor();
    if (editor instanceof HaskellEditor) {
      HaskellEditor hEditor = (HaskellEditor) editor;
      // this is the temp file in .dist-buildwrapper
      File f=hEditor.getTokenScanner().getTarget();
      if (f!=null){
        // this is the real file
        IFile file=hEditor.findFile();
        IProject p=file.getProject();
        if (p!=null){
          try {
            // go!
            StylishHaskell.runStylishHaskell( ScionManager.getExecutablePath( IPreferenceConstants.STYLISHHASKELL_EXECUTABLE, "stylish-haskell", false ), p, f,file.getCharset() );
            // re read the file on disk and tell the editor about it
            hEditor.getDocument().set( FileUtil.getContents( f,file.getCharset() ) );
          } catch (Exception ioe){
            HaskellUIPlugin.log( ioe );
          }
        }
      }
    }
  }

}
