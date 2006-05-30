package net.sf.eclipsefp.haskell.ui.test.editor;

import static org.easymock.classextension.EasyMock.*;

import org.easymock.EasyMock;

import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

import junit.framework.TestCase;

import net.sf.eclipsefp.haskell.ui.editor.HaskellConfiguration;
import net.sf.eclipsefp.haskell.ui.editor.IContentAssistantFactory;

public class HaskellConfigurationTest extends TestCase {
	
	public void testProducesAutoInsertingAssitant() {
		IContentAssistantFactory factory = EasyMock.createMock(IContentAssistantFactory.class);
		ContentAssistant assistant = createNiceMock(ContentAssistant.class);
		expect(factory.createAssistant()).andReturn(assistant).once();
		assistant.enableAutoInsert(true);
		expectLastCall().once();
		
		EasyMock.replay(factory);
		replay(assistant);
		
		SourceViewerConfiguration conf = new HaskellConfiguration(null, factory);
		IContentAssistant actualAssitant = conf.getContentAssistant(null);
		
		assertSame(assistant, actualAssitant);
		
		EasyMock.verify(factory);
		verify(assistant);
	}

}
