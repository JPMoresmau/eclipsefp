package net.sf.eclipsefp.haskell.scion.internal.util;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;

public class Trace {
	
	private static final String optionId = "logs";

	public synchronized static void trace(String prefix, String message, Object... args) {
		if (ScionPlugin.isTracing(optionId)) {
			printPrefix(prefix);
			System.out.println(String.format(message, args));
			System.out.flush();
		}
	}
	
	public synchronized static void trace(String prefix, Throwable ex) {
		if (ScionPlugin.isTracing(optionId)) {
			printPrefix(prefix);
			ex.printStackTrace(System.out);
			System.out.flush();
		}
	}

	private static void printPrefix(String prefix) {
		long time = System.currentTimeMillis();
		System.out.print(time / 1000 + "." + time % 1000);
		System.out.print(" ");
		System.out.print(prefix);
		System.out.print(" ");
	}
	
}
