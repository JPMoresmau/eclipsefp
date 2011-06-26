package net.sf.eclipsefp.haskell.core.hlint;

import java.util.Map;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

public class HLintBuilder extends IncrementalProjectBuilder {

  public static String BUILDER_ID = HLintBuilder.class.getName();

	public HLintBuilder() {
		// Do nothing
	}

	@Override
	protected IProject[] build(final int kind, final Map<String, String> args, final IProgressMonitor monitor)
			throws CoreException {
		return null;
	}

}
