package org.eclipse.jface.text.contentassist;

import org.eclipse.jface.text.contentassist.IContentAssistant;

public class PromiscuousAssistant {

	private ContentAssistant fAssistant;

	public PromiscuousAssistant(IContentAssistant assistant) {
		fAssistant = (ContentAssistant) assistant;
	}
	
	public boolean isAutoInserting() {
		return fAssistant.isAutoInserting();
	}
	
}
