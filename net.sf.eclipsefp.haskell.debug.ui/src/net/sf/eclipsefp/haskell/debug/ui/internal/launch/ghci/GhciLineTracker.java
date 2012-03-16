package net.sf.eclipsefp.haskell.debug.ui.internal.launch.ghci;

import java.util.regex.Matcher;
import net.sf.eclipsefp.haskell.core.util.GHCiSyntax;
import org.eclipse.debug.ui.console.IConsole;
import org.eclipse.debug.ui.console.IConsoleLineTrackerExtension;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;


public class GhciLineTracker implements IConsoleLineTrackerExtension {

  IConsole console;

  @Override
  public void init( final IConsole con ) {
    this.console = con;
  }

  @Override
  public void lineAppended( final IRegion line ) {
    try {
      IDocument doc = console.getDocument();
      String text = doc.get( line.getOffset(), line.getLength() );
      int prompt = text.indexOf( '>' );
      String afterPromptText;
      if (prompt == -1 || prompt == text.length() - 1) {
        afterPromptText = text; // No prompt
      } else {
        afterPromptText = text.substring( prompt + 2 );
      }
      Matcher m = GHCiSyntax.BREAKPOINT_SET_PATTERN.matcher( afterPromptText );
      if (m.matches()) {
        doc.replace( line.getOffset(), line.getLength(), "" ); //$NON-NLS-1$
      }
    } catch (BadLocationException e) {
      // Do nothing
    }
  }

  @Override
  public void dispose() {
    // Do nothing
  }

  @Override
  public void consoleClosed() {
    // Do nothing
  }

}
