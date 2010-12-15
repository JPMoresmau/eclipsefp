package net.sf.eclipsefp.haskell.ui.internal.editors.cabal;

import java.util.Arrays;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
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
    if (TextUtilities.endsWith(d.getLegalLineDelimiters(), c.text) != -1){
      // Start autoindentation after a cabal section (e.g., global, executable, library)
      try {
        IRegion r = d.getLineInformation( d.getLineOfOffset( c.offset ));
        String s = d.get( r.getOffset(), r.getLength() ).toLowerCase();

        for (String section:CabalSyntax.sections.keySet()){
          if (s.equals( section ) || s.startsWith( section + " " )){ //$NON-NLS-1$
            char[] ch = new char[getTabWidth()];
            Arrays.fill( ch, ' ' );
            c.text = c.text + new String(ch);
          }
        }
      } catch (BadLocationException ble) {
        // ignore
      }
    } else {
      // Superclass takes care of autoindentation
      super.customizeDocumentCommand( d, c );
    }
  }

  private int getTabWidth() {
    IPreferenceStore prefStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    return prefStore.getInt( IEditorPreferenceNames.EDITOR_CABAL_TAB_WIDTH );
  }
}
