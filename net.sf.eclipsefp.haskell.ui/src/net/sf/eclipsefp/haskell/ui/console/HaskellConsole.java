package net.sf.eclipsefp.haskell.ui.console;

import java.io.OutputStreamWriter;
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


public class HaskellConsole {
  private IOConsole fConsole;
  private final IConsoleCleaner fCleaner;

  public HaskellConsole(final IConsoleCleaner cleaner,final String name) {
    fCleaner = cleaner;
    createIOConsole(name);
  }

  private void createIOConsole(final String name) {
    IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
    fConsole = new IOConsole(name, HaskellConsole.class.getName(),HaskellUIImages.getImageDescriptor( IImageNames.HASKELL_MISC ));
    mgr.addConsoles(new IConsole[] {fConsole});
  }

  public Writer createOutputWriter() {
    final IOConsoleOutputStream outputStream = fConsole.newOutputStream();
    final Display stdDisplay = HaskellUIPlugin.getStandardDisplay();
    stdDisplay.syncExec( new Runnable() {
      public void run() {
        // JP likes blue for the console's text color.
        outputStream.setColor(stdDisplay.getSystemColor( SWT.COLOR_BLUE ));
      }
    });
    Writer outputWriter = new OutputStreamWriter(outputStream);
    return outputWriter;
  }


  public IConsoleCleaner getCleaner() {
    return fCleaner;
  }


  public IOConsole getConsole() {
    return fConsole;
  }


}
