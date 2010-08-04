package net.sf.eclipsefp.haskell.scion.client;

import java.util.Set;

import org.eclipse.core.resources.IFile;

/**
 * This interface is useful to keep the disconnect between the scion code and the cabal parsing code
 * @author JP Moresmau
 *
 */
public interface CabalComponentResolver {
	/**
	 * for a given file being probably a haskell file, give the names of Cabal components the module is contained in
	 * @param file the file
	 * @return the sets of components
	 */
	public Set<String> getComponents(IFile file);
}
