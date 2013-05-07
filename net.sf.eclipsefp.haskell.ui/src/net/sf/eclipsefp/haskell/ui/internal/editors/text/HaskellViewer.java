/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.text;

import java.util.ResourceBundle;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.source.IOverviewRuler;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.TextOperationAction;

/**
 * A viewer handling the toggle comment operation
 *
 * @author JP Moresmau
 *
 */
public class HaskellViewer extends ProjectionViewer {

  public static final int TOGGLE_COMMENT=ISourceViewer.INFORMATION+10;

  private String[] defaultPrfs;

  public HaskellViewer( final Composite parent, final IVerticalRuler ruler,
      final IOverviewRuler overviewRuler, final boolean showsAnnotationOverview, final int styles ) {
    super( parent, ruler, overviewRuler, showsAnnotationOverview, styles );

  }

  @Override
  public void setDefaultPrefixes( final String[] defaultPrefixes, final String contentType ) {
    super.setDefaultPrefixes( defaultPrefixes, contentType );
    defaultPrfs=defaultPrefixes;
  }

  @Override
  public void doOperation( final int operation ) {
    if (TOGGLE_COMMENT==operation){
      ITextSelection selection= (ITextSelection) getSelection();
      IDocument document= getDocument();
      try {
        IRegion reg=document.getLineInformation( selection.getStartLine() );
        String s=document.get( reg.getOffset(), reg.getLength() ).trim();
        if (defaultPrfs!=null && defaultPrfs.length>0){
          if (s.startsWith( defaultPrfs[0] )){
            shift(true, false, true);
          } else {
            shift(true, true, true);
          }
        }
      } catch (BadLocationException x) {
        HaskellUIPlugin.log( x );
      }
      return;
    }
    super.doOperation( operation );
  }

  @Override
  public boolean canDoOperation( final int operation ) {
    if (TOGGLE_COMMENT==operation){
      return true;
    }
    return super.canDoOperation( operation );
  }

  /**
   * Create a standard text operation action
   */
  public static TextOperationAction createTextOpAction( final TextEditor ed,final String actionIdName, final String resourcePrefix, final int targetId,
                                                  final String actionDefinitionId ) {
    ResourceBundle bundle = HaskellUIPlugin.getDefault().getResourceBundle();
    TextOperationAction action = new TextOperationAction( bundle, resourcePrefix + ".", ed, targetId );  //$NON-NLS-1$
    action.setActionDefinitionId( actionDefinitionId );
    ed.setAction( actionIdName, action );
    ed.markAsStateDependentAction( actionIdName, true );

    return action;
  }
}
