package net.sf.eclipsefp.haskell.scion.internal.util;

import java.util.IllegalFormatException;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;

public class Trace {
	
	private static final String optionId = "logs";

	public synchronized static void trace(String prefix, String message, Object... args) {
		if (ScionPlugin.isTracing(optionId)) {
			printPrefix(prefix);
			try {
				// if we have no arguments, we could have passed an arbitrary string, that contains % characters that cause issues
				// a real life example was %title% in a yesod quasiquoted file
				if (args.length>0){
					System.out.println(String.format(message, args));
				} else {
					System.out.println(message);
				}
				System.out.flush();
			} catch (IllegalFormatException ife){
				System.err.println(message);
				ife.printStackTrace();
			}
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
	
	public static boolean isTracing() {
		return ScionPlugin.isTracing(optionId);
	}
}
