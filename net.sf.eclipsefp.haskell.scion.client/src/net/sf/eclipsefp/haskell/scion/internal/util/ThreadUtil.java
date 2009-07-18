package net.sf.eclipsefp.haskell.scion.internal.util;

public class ThreadUtil {

	/**
	 * Waits for the specified object.
	 * More or less the same as calling <code>wait(long timeout)</code> on the object,
	 * but this handles spurious wakeups. So in the absence of a notify,
	 * this is guaranteed to wait for at least the specified amount of time.
	 */
	public static void waitTimeout(Object object, long timeout) {
		long endTime = System.currentTimeMillis() + timeout;
		boolean interrupted = true;
		while (interrupted) {
			interrupted = false;
			try {
				object.wait(endTime - System.currentTimeMillis());
			} catch (InterruptedException ex) {
				interrupted = true;
			}
		}
	}
	
}
