package net.sf.eclipsefp.haskell.ui.test.preferences;

import org.eclipse.jface.preference.PreferenceStore;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerListener;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerManager;
import net.sf.eclipsefp.haskell.ui.preferences.HaskellPreferenceManager;
import net.sf.eclipsefp.haskell.ui.preferences.IPreferenceConstants;
import junit.framework.TestCase;

import static org.easymock.EasyMock.*;

public class HaskellPreferenceManager_PDETest extends TestCase
                                          implements IPreferenceConstants
{
	
	public void testAddsCompilerReporterOnActivateBuildConsolePreferences() {
		ICompilerManager cmgr = createMock(ICompilerManager.class);
		PreferenceStore store = new PreferenceStore();
		store.setValue(IPreferenceConstants.CLEAR_BUILD_CONSOLE, true);
		cmgr.addCompilerListener((ICompilerListener) anyObject());
		expectLastCall().once();
		replay(cmgr);
		
		HaskellPreferenceManager pmgr = new HaskellPreferenceManager(cmgr, store);
		pmgr.activateBuildConsolePreferences();
		
		verify(cmgr);
	}
	
	public void testRenewsCompilerReporterOnSecondCall() {
		ICompilerManager cmgr = createMock(ICompilerManager.class);
		PreferenceStore store = new PreferenceStore();
		store.setValue(IPreferenceConstants.CLEAR_BUILD_CONSOLE, true);
		cmgr.addCompilerListener((ICompilerListener) anyObject());
		cmgr.removeCompilerListener((ICompilerListener) anyObject());
		cmgr.addCompilerListener((ICompilerListener) anyObject());
		expectLastCall().once();
		replay(cmgr);
		
		HaskellPreferenceManager pmgr = new HaskellPreferenceManager(cmgr, store);
		pmgr.activateBuildConsolePreferences();
		pmgr.activateBuildConsolePreferences();
		
		verify(cmgr);
	}

}
