/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;

/**
 * run the tab checker as a part listener to avoid multiple activation of the editor, a single invocation, etc.
 *
 * @author JP Moresmau
 *
 */
public class TabChecker implements IPartListener{
  private final AbstractDecoratedTextEditor editor;

  public TabChecker(final AbstractDecoratedTextEditor part){
    this.editor=part;
    IPartService service = (IPartService) editor.getSite().getService(IPartService.class);
    service.addPartListener( this );
  }

  @Override
  public void partActivated( final IWorkbenchPart part ) {
    //NOOP

  }
  @Override
  public void partBroughtToTop( final IWorkbenchPart part ) {
    //NOOP

  }
  @Override
  public void partClosed( final IWorkbenchPart part ) {
    //NOOP

  }
  @Override
  public void partDeactivated( final IWorkbenchPart part ) {
    //NOOP

  }
  @Override
  public void partOpened( final IWorkbenchPart part ) {
    checkTabs( editor.getSite().getShell().getDisplay(), getDocument() );
    IPartService service = (IPartService) editor.getSite().getService(IPartService.class);
    service.removePartListener( this );
  }

  private IDocument getDocument() {

    IDocumentProvider docProvider = editor.getDocumentProvider();
    if (docProvider!=null){
      return docProvider.getDocument( editor.getEditorInput() );
    }
    return null;
  }

  private static void checkTabs(final Display display,final IDocument doc){
    final String contents=doc.get();
    if (contents.contains( "\t" )){
      // we should not be in a job
        if (MessageDialog.openConfirm( display.getActiveShell() , UITexts.error_tabs  , UITexts.error_tabs_message )){
          int tw=HaskellUIPlugin.getDefault().getPreferenceStore().getInt( IEditorPreferenceNames.EDITOR_TAB_WIDTH );
          StringBuilder sb=new StringBuilder();
          for (int a=0;a<tw;a++){
            sb.append(" ");
          }
          String contents2=contents.replace( "\t",sb.toString() );
          doc.set(contents2);
        }
    }
  }
}
