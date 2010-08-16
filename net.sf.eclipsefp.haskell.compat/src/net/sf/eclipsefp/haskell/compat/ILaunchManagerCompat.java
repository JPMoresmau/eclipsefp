package net.sf.eclipsefp.haskell.compat;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.eclipse.debug.core.ILaunchManager;

/** ILaunchManager compatibility methods.
 * 
 * ILaunchManager handles the differences between the Galileo and Helios version of the Eclipse API. Eventually, this class
 * goes away when Helios and later Eclipse distributions are the norm.
 *
 * @author B. Scott Michel <scottm@aero.org>
 */
public class ILaunchManagerCompat {
	static Method generateLaunchConfigurationNameM = null;
	
	static {
		Class<? extends Object>[] genLCNParams = new Class<?>[] {
				String.class
		};
		Class<? extends Object> iLM = ILaunchManager.class;
		try {
			// Prefer the Helios version
			generateLaunchConfigurationNameM = iLM.getMethod("generateLaunchConfigurationName", genLCNParams);
		} catch (NoSuchMethodException e) {
			try {
				// Galileo version
				generateLaunchConfigurationNameM = iLM.getMethod("generateUniqueLaunchConfigurationNameFrom", genLCNParams);
			} catch (NoSuchMethodException e1) {
				// Should never happen.
				generateLaunchConfigurationNameM = null;
			}
		}
	}

	/**
	 * Compatibility shim for generateLauchConfigurationName (Helios) and for generateUniqueLaunchConfigurationNameFrom (Galileo).
	 * 
	 * @param mgr ILaunchManager whose method will be invoked.
	 * @param prefix Prefix for the launch configuration.
	 * @return a String that can be used as the name of a launch configuration.
	 */
	public static String generateLaunchConfigurationName(ILaunchManager mgr, final String prefix) {
		Object[] args = new Object[] { prefix };
		String result = null;
		try {
			result = (String) generateLaunchConfigurationNameM.invoke(mgr, args);
		} catch (IllegalArgumentException e) {
			// Cannot not happen
		} catch (IllegalAccessException e) {
			// Cannot not happen
		} catch (InvocationTargetException e) {
			// Cannot happen -- doesn't throw exceptions
		}
		return result;
	}
}
