package net.sf.eclipsefp.haskell.ui.test.editor.codeassist.doubles;

import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.TextViewer;

public class StubViewer extends TextViewer {
	
	public StubViewer(final String contents) {
		setDocument(new Document(contents));
	}
	
	

}
