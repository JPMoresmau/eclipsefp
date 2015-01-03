package net.sf.eclipsefp.haskell.ui.console;

import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.LinkedList;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IOConsole;
import org.eclipse.ui.console.IOConsoleOutputStream;

/**
 * Haskell-specific IOConsole class.
 */
public class HaskellConsole extends IOConsole {

  public List<IOConsoleOutputStream> streams=new LinkedList<>();
  /**
   * Construct and register the console with the console manager.
   */
  public HaskellConsole(final String name) {
    super(name, HaskellConsole.class.getName(),HaskellUIImages.getImageDescriptor( IImageNames.HASKELL_MISC ),FileUtil.UTF8,true);

    // Register console with the console manager.
    IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
    mgr.addConsoles(new IConsole[] { this });
  }

  /**
   * Create an output stream writer, ensuring that the console's output is blue.
   */
  public Writer createOutputWriter() {
    final IOConsoleOutputStream outputStream = newOutputStream();
    // using internals
    outputStream.setActivateOnWrite( HaskellUIPlugin.getDefault().getPreferenceStore().getBoolean( IPreferenceConstants.HASKELL_CONSOLE_ACTIVATE_ON_WRITE ) );
    streams.add(outputStream);
    /*final Display stdDisplay = HaskellUIPlugin.getStandardDisplay();
    stdDisplay.syncExec( new Runnable() {
      @Override
      public void run() {
        // JP likes blue for the console's text color.
        outputStream.setColor(stdDisplay.getSystemColor( SWT.COLOR_BLUE ));
      }
    });*/
    try {
      return new OutputStreamWriter(outputStream,FileUtil.UTF8);
    } catch (UnsupportedEncodingException ioo){
      return new OutputStreamWriter(outputStream);
    }
  }

  public void setActivate(final boolean activate){
    for (IOConsoleOutputStream str:streams){
      str.setActivateOnWrite( activate );
    }
  }
}
