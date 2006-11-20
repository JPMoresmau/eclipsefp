package net.sf.eclipsefp.haskell.core.compiler;

import org.eclipse.core.runtime.IConfigurationElement;

public interface ICompilerManager {

	/**
	 * <p>
	 * returns the ids of all compilers registered with the compiler manager.
	 * </p>
	 */
	public String[] getRegisteredCompilers();

	/**
	 * <p>
	 * returns the currently used Haskell compiler.
	 * </p>
	 */
	public IHaskellCompiler getCompiler();

	/**
	 * <p>
	 * if the compiler specified by id is known in the compiler manager, it is
	 * selected and will be the one returned by getCompiler() from now on.
	 * </p>
	 */
	public boolean selectCompiler(final String id) throws Exception;

	/**
	 * <p>
	 * used by the plugin to register compilers that are declared in the
	 * plugin.xml with the CompilerManager.
	 * </p>
	 */
	public void registerCompiler(final String id,
			final IConfigurationElement info);

	/**
	 * <p>
	 * returns the human-readable name for the compiler with the specified id
	 * (if it is a registered compiler).
	 * </p>
	 */
	public String getCompilerName(final String id);

	public void installCompiler(final String id,
			IHaskellCompiler haskellCompiler);

	public void addCompilerListener(ICompilerListener listener);

	public void removeCompilerListener(ICompilerListener listener);

}