// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
// Copyright (c) 2011 by Alejandro Serrano
package net.sf.eclipsefp.haskell.core.expressions;

import java.lang.reflect.Method;
import net.sf.eclipsefp.haskell.core.project.HaskellResource;
import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.core.runtime.Platform;

/**
 * <p>
 * For plugins that have to check some properties of elements in the workspace,
 * e.g. whether they are the project executable or the source folder of a
 * Haskell project.
 * </p>
 *
 * <p>
 * This is used in the XML Expression language.
 * </p>
 *
 * @author Leif Frenzel
 * @author Alejandro Serrano
 */
public class HaskellPropertyTester extends PropertyTester
	implements IAdapterFactory
{

	// interface methods of PropertyTester
	// ////////////////////////////////////

	@Override
  public boolean test(final Object receiver, final String property,
			final Object[] args, final Object expectedValue)	{
		Object resource = Platform.getAdapterManager()
			.getAdapter(receiver, HaskellResource.class);

		if (null == resource) {
      return false;
    }
		try {
			Method method = resource.getClass().getMethod(property);
			return ( ( Boolean )method.invoke( resource ) ).booleanValue();
		} catch (Exception e) {
			return false;
		}
	}

	@Override
  public Object getAdapter(final Object adaptable, final Class adapter) {
		if ( adaptable instanceof IResource &&
		     adapter.equals(HaskellResource.class))	{
			return new HaskellResource((IResource) adaptable);
		}
		return null;
	}

	@Override
  public Class[] getAdapterList() {
		return new Class[] {HaskellResource.class};
	}
}