/**
 *  Copyright (c) 2009 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal;

import java.util.Arrays;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.util.LangUtil;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultIndentLineAutoEditStrategy;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.TextUtilities;

/**
 * Indent strategy: indent after section name
  *
  * @author JP Moresmau
 */
public class CabalAutoIndentStrategy extends DefaultIndentLineAutoEditStrategy {

  @Override
  public void customizeDocumentCommand( final IDocument d, final DocumentCommand c ) {

    // end of line
    if (c.length == 0 && c.text != null && TextUtilities.endsWith(d.getLegalLineDelimiters(), c.text) != -1){
      // add indent level after a cabal section (e.g., global, executable, library)
      try {
        IRegion r = d.getLineInformation( d.getLineOfOffset( c.offset ));
        String s =LangUtil.ltrim(d.get( r.getOffset(), r.getLength() ).toLowerCase());

        // Superclass takes care of autoindentation
        super.customizeDocumentCommand( d, c );

        for (String section:CabalSyntax.sections.keySet()){
          if (s.equals( section ) || s.startsWith( section + " " )){ //$NON-NLS-1$
            char[] ch = new char[getTabWidth()];
            Arrays.fill( ch, ' ' );
            c.text = c.text + new String(ch);
            return;
          }
        }
      } catch (BadLocationException ble) {
        // ignore
      }
    }


  }

  private int getTabWidth() {
    IPreferenceStore prefStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    return prefStore.getInt( IEditorPreferenceNames.EDITOR_CABAL_TAB_WIDTH );
  }
}
