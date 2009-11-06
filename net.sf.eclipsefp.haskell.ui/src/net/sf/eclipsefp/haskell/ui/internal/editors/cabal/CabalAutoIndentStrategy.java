package net.sf.eclipsefp.haskell.ui.internal.editors.cabal;

import java.util.Arrays;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellAutoIndentStrategy;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;

/**
 * <p>Indent strategy: indent after section name</p>
  *
  * @author JP Moresmau
 */
public class CabalAutoIndentStrategy extends HaskellAutoIndentStrategy {

  @Override
  public void customizeDocumentCommand( final IDocument document,
                                        final DocumentCommand command ) {

    if (command.text.endsWith( "\n" )){ //$NON-NLS-1$
      try {
        IRegion r=document.getLineInformation( document.getLineOfOffset( command.offset ));
        String s=document.get( r.getOffset(), r.getLength() ).toLowerCase();
        for (String section:CabalSyntax.sections.keySet()){
          if (s.equals( section ) || s.startsWith( section+" " )){ //$NON-NLS-1$
            char[] ch=new char[getTabWidth()];
            Arrays.fill( ch, ' ' );
            command.text=command.text+new String(ch);
          }
        }
      } catch (BadLocationException ble){
        // ignore
      }
    }
    super.customizeDocumentCommand( document, command );
  }

  @Override
  protected int getTabWidth() {
    String key = IEditorPreferenceNames.EDITOR_CABAL_TAB_WIDTH;
    return getPreferenceStore().getInt( key );
  }

  @Override
  protected boolean isSpacesForTabs() {
    return true;
  }
}
