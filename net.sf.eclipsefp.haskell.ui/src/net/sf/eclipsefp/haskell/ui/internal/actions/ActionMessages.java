package net.sf.eclipsefp.haskell.ui.internal.actions;

import org.eclipse.osgi.util.NLS;

public final class ActionMessages extends NLS {

	private static final String BUNDLE_NAME = "plugin"; // This will lead to plugin.properties

	static {
		NLS.initializeMessages(BUNDLE_NAME, ActionMessages.class);
	}

	private ActionMessages() {
		// do not instantiate
	}

	public static String openDefinitionAction_label;
	public static String openDefinitionAction_tooltip;

}
