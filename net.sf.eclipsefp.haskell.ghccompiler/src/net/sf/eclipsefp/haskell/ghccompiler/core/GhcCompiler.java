// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.compiler.CompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.CompilerOutputItem;
import net.sf.eclipsefp.haskell.core.compiler.DefaultHaskellCompiler;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutputItem;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

/**
 * <p>
 * Implements the compiler specific parts for the Glasgow Haskell compiler
 * (ghc).
 * </p>
 *
 * @author Leif Frenzel
 */
public class GhcCompiler extends DefaultHaskellCompiler {

	private static boolean trace = GhcCompilerPlugin.isTracing();

	private final CompilerParams compilerParams = new CompilerParams();

	private final IProcessRunner fProcessRunner;

	public GhcCompiler() {
		this(new ProcessRunner());
	}

	/**
	 * Constructor for testing
	 */
	public GhcCompiler(final IProcessRunner procRunner) {
		fProcessRunner = procRunner;
	}

	@Override
  public ICompilerOutput compile(final IFile file, final Writer outputWriter) {
		final IProject project = file.getProject();
		IHaskellProject hsProject = HaskellProjectManager.get(project);
		String[] cmdLine = buildCommandLine(file, hsProject);
		IPath src = project.getLocation().append(hsProject.getSourcePath());
		String output = fProcessRunner.execute(
			new File(src.toOSString()), outputWriter, cmdLine);
		return parse(output);
	}

	private String[] buildCommandLine(final IFile file,
			final IHaskellProject haskellProject) {
		if (trace) {
			System.out.println("Constructing command line for file " + file); //$NON-NLS-1$
		}

		IProject project = haskellProject.getResource();
		String outDir = getAbsPath(project, haskellProject.getOutputPath());

		List<String> cmdLine = new ArrayList<String>();
		// command and special options
		cmdLine.add(Util.getCompilerExecutable());
		String libPath = Util.constructLibPath(haskellProject);
		if (!libPath.equals("")) { //$NON-NLS-1$
			cmdLine.add(libPath);
		}
		cmdLine.add("--make"); //$NON-NLS-1$
		cmdLine.add("-odir"); //$NON-NLS-1$
		cmdLine.add(outDir);
		cmdLine.add("-hidir"); //$NON-NLS-1$
		cmdLine.add(outDir);
		cmdLine.add("-ferror-spans"); //$NON-NLS-1$

		String binDir = getAbsPath(project, haskellProject.getBinPath());
		cmdLine.add("-o"); //$NON-NLS-1$
		cmdLine.add(binDir + File.separator + getTargetName(haskellProject));
		cmdLine.addAll(compilerParams.construct());
		cmdLine.add(getFileName(file, haskellProject));
		if (trace) {
			HaskellCorePlugin.dump(cmdLine);
		}
		return cmdLine.toArray(new String[cmdLine.size()]);
	}

	// helping methods
	// ////////////////

	private ICompilerOutput parse(final String messages) {
	  List<ICompilerOutputItem> list = GhcOutputParser.parse(messages);

		CompilerOutput output
		  = new CompilerOutput( 0, messages, new ArrayList<Exception>() );
		for( ICompilerOutputItem item: list ) {
      output.addError( ( CompilerOutputItem )item );
    }
		return output;
	}

	private String getAbsPath(final IProject project, final IPath path) {
		return project.getLocation().toOSString() + File.separator
				+ path.toOSString();
	}

	private String getTargetName(final IHaskellProject haskellProject) {
		String targetName = haskellProject.getTargetName();
		if (targetName.equals("")) { //$NON-NLS-1$
			targetName = "theResult"; //$NON-NLS-1$
		}
		return targetName;
	}

	private String getFileName(final IFile file,
			final IHaskellProject haskellProject) {
		IPath sourcePath = haskellProject.getSourcePath();
		return getSourceRelPath(file, sourcePath).toOSString();
	}

	private IPath getSourceRelPath(final IFile file, final IPath sourcePath) {
		IPath projectRelPath = file.getProjectRelativePath();
		int num = projectRelPath.matchingFirstSegments(sourcePath);
		return projectRelPath.removeFirstSegments(num);
	}

}