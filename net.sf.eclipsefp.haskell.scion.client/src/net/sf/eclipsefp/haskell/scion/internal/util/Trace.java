package net.sf.eclipsefp.haskell.scion.internal.util;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;

public class Trace {
	
	private static final String optionId = "logs";

	public synchronized static void trace(String prefix, String message, Object... args) {
		if (ScionPlugin.isTracing(optionId)) {
			System.out.print(prefix);
			System.out.print(" ");
			System.out.println(String.format(message, args));
			System.out.flush();
		}
	}
	
	public synchronized static void trace(String prefix, Throwable ex) {
		if (ScionPlugin.isTracing(optionId)) {
			System.out.print(prefix);
			System.out.print(" ");
			ex.printStackTrace(System.out);
			System.out.flush();
		}
	}
	
}
