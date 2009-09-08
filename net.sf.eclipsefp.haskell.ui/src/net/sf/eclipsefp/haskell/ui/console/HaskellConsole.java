package net.sf.eclipsefp.haskell.ui.console;

import java.io.OutputStreamWriter;
import java.io.Writer;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.swt.graphics.Color;
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
    fConsole = new IOConsole(name, null);
    mgr.addConsoles(new IConsole[] {fConsole});
  }

  public Writer createOutputWriter() {
    final IOConsoleOutputStream outputStream = fConsole.newOutputStream();
    HaskellUIPlugin.getStandardDisplay().syncExec( new Runnable() {
      public void run() {
        outputStream.setColor(new Color(HaskellUIPlugin.getStandardDisplay(), 0, 0, 255));
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
