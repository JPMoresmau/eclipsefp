package net.sf.eclipsefp.haskell.ui.test.editor;

import static org.easymock.EasyMock.*;

import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

import junit.framework.Assert;
import junit.framework.TestCase;

import net.sf.eclipsefp.haskell.ui.editor.HaskellConfiguration;
import net.sf.eclipsefp.haskell.ui.editor.IContentAssistantFactory;

public class HaskellConfigurationTest extends TestCase {
	
	private static class MockContentAssistant extends ContentAssistant {
		private boolean fAutoInsert = false;
		@Override
		public void enableAutoInsert(boolean enabled) { fAutoInsert = enabled; }
		public void verify() { Assert.assertTrue(fAutoInsert); }
	}
	
	public void testProducesAutoInsertingAssitant() {
		IContentAssistantFactory factory = createMock(IContentAssistantFactory.class);
		MockContentAssistant assistant = new MockContentAssistant();
		expect(factory.createAssistant()).andReturn(assistant).once();
		
		replay(factory);
		
		SourceViewerConfiguration conf = new HaskellConfiguration(null, factory);
		IContentAssistant actualAssitant = conf.getContentAssistant(null);
		
		assertSame(assistant, actualAssitant);
		
		verify(factory);
		assistant.verify();
	}

}
