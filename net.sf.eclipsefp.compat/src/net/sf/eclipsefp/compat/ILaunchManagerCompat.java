/** ILaunchManager compatibility methods, to handle differences between different
 * versions of the Eclipse API.
 * 
 */
package net.sf.eclipsefp.compat;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.eclipse.debug.core.ILaunchManager;

/**
 * @author B. Scott Michel <scottm@aero.org>
 *
 */
public class ILaunchManagerCompat {
	static Method generateLaunchConfigurationNameM = null;
	
	static {
		Class<? extends Object>[] genLCNParams = new Class<?>[] {
				String.class
		};
		Class<? extends Object> iLM = ILaunchManager.class;
		try {
			// Helios version
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
