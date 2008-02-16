package net.sf.eclipsefp.haskell.ui.internal.preferences;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerManager;
import net.sf.eclipsefp.haskell.ui.console.ConsoleCompilingReporter;
import net.sf.eclipsefp.haskell.ui.console.FakeCleaner;
import net.sf.eclipsefp.haskell.ui.console.IConsoleCleaner;
import net.sf.eclipsefp.haskell.ui.console.RealCleaner;

import org.eclipse.jface.preference.IPreferenceStore;

public class HaskellPreferenceManager implements IPreferenceConstants {

	private final ICompilerManager fCompilerManager;
	private final IPreferenceStore fPreferenceStore;
	private ConsoleCompilingReporter fReporter = null;

	public HaskellPreferenceManager( final ICompilerManager manager,
                                   final IPreferenceStore store ) {
    fCompilerManager = manager;
    fPreferenceStore = store;
  }

	public void activateBuildConsolePreferences() {
    if( null != fReporter ) {
      fCompilerManager.removeCompilerListener( fReporter );
    }

    fReporter = new ConsoleCompilingReporter( createCleaner() );
    fCompilerManager.addCompilerListener( fReporter );
  }

	private IConsoleCleaner createCleaner() {
		return   fPreferenceStore.getBoolean( CLEAR_BUILD_CONSOLE ) 
		       ? new RealCleaner() 
		       : new FakeCleaner();
	}
}
