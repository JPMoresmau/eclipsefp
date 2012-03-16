package net.sf.eclipsefp.haskell.ui.console;

import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IOConsole;
import org.eclipse.ui.console.IOConsoleOutputStream;

/**
 * Haskell-specific IOConsole class.
 */
public class HaskellConsole extends IOConsole {
  /** Preference value for Haskell console high water mark */
  public final static int HASKELL_CONSOLE_HIGH_WATER_MARK = 32 * 1024; // 32K
  /** Preference value for Haskell console low water mark */
  public final static int HASKELL_CONSOLE_LOW_WATER_MARK = HASKELL_CONSOLE_HIGH_WATER_MARK / 4;

  /**
   * Construct and register the console with the console manager.
   */
  public HaskellConsole(final String name) {
    super(name, HaskellConsole.class.getName(),HaskellUIImages.getImageDescriptor( IImageNames.HASKELL_MISC ),"UTF8",true);

    // Register console with the console manager.
    IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
    mgr.addConsoles(new IConsole[] { this });
  }

  /**
   * Create an output stream writer, ensuring that the console's output is blue.
   */
  public Writer createOutputWriter() {
    final IOConsoleOutputStream outputStream = newOutputStream();
    final Display stdDisplay = HaskellUIPlugin.getStandardDisplay();
    stdDisplay.syncExec( new Runnable() {
      @Override
      public void run() {
        // JP likes blue for the console's text color.
        outputStream.setColor(stdDisplay.getSystemColor( SWT.COLOR_BLUE ));
      }
    });
    try {
      return new OutputStreamWriter(outputStream,"UTF8");
    } catch (UnsupportedEncodingException ioo){
      return new OutputStreamWriter(outputStream);
    }
  }
}
