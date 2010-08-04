package net.sf.eclipsefp.haskell.ui.internal.preferences;

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerListener;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerManager;
import org.eclipse.jface.preference.PreferenceStore;

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
		verify(cmgr);
	}

}
