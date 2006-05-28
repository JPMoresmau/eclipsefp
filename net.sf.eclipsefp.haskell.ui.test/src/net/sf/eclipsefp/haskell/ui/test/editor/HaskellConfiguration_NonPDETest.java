package net.sf.eclipsefp.haskell.ui.test.editor;

import net.sf.eclipsefp.haskell.ui.editor.HaskellConfiguration;

import org.eclipse.jface.text.contentassist.PromiscuousAssistant;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

import junit.framework.TestCase;

public class HaskellConfiguration_NonPDETest extends TestCase {
	
	public void testProducesAutoInsertingAssitant() {
		SourceViewerConfiguration conf = new HaskellConfiguration(null);
		PromiscuousAssistant assistant = new PromiscuousAssistant(
				conf.getContentAssistant(null));
		assertTrue(assistant.isAutoInserting());
	}

}
