// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;
import java.util.ArrayList;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.IHaskellCompiler;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.util.TracingUtil;
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
public class GhcCompiler implements IHaskellCompiler {

	private static boolean trace = GhcCompilerPlugin.isTracing();

	private CompilerParams compilerParams = new CompilerParams();

	private IProcessRunner fProcessRunner;

	public GhcCompiler() {
		this(new ProcessRunner());
	}
	
	/**
	 * Constructor for testing
	 */
	public GhcCompiler(IProcessRunner procRunner) {
		fProcessRunner = procRunner;
	}

	public ICompilerOutput compile(IFile file) {
		final IProject project = file.getProject();
		IHaskellProject hsProject = HaskellProjectManager.get(project);
		String[] cmdLine = buildCommandLine(file, hsProject);
		IPath src = project.getLocation().append(hsProject.getSourcePath());
		String output = fProcessRunner.execute(
			new File(src.toOSString()), cmdLine);
		return parse(output);
	}

	public String[] buildCommandLine(final IFile file,
			final IHaskellProject haskellProject) {
		if (trace) {
			System.out.println("Constructing command line for file " + file);
		}

		IProject project = haskellProject.getResource();
		String outDir = getAbsPath(project, haskellProject.getOutputPath());

		ArrayList<String> cmdLine = new ArrayList<String>();
		// command and special options
		cmdLine.add(Util.getCompilerExecutable());
		String libPath = Util.constructLibPath(haskellProject);
		if (!libPath.equals("")) {
			cmdLine.add(libPath);
		}
		cmdLine.add("--make");
		cmdLine.add("-odir");
		cmdLine.add(outDir);
		cmdLine.add("-hidir");
		cmdLine.add(outDir);
		cmdLine.add("-ferror-spans"); //TODO supported by GHC 6.4.2 but not by every GHC

		String binDir = getAbsPath(project, haskellProject.getBinPath());
		cmdLine.add("-o");
		cmdLine.add(binDir + File.separator + getTargetName(haskellProject));
		cmdLine.addAll(compilerParams.construct());
		cmdLine.add(getFileName(file, haskellProject));
		if (trace) {
			TracingUtil.dump(cmdLine);
		}
		return cmdLine.toArray(new String[cmdLine.size()]);
	}

	// helping methods
	// ////////////////

	private ICompilerOutput parse(String messages) {
		return GhcOutputParser.parse(messages);
	}

	private String getAbsPath(final IProject project, final IPath path) {
		return project.getLocation().toOSString() + File.separator
				+ path.toOSString();
	}

	private String getTargetName(final IHaskellProject haskellProject) {
		String targetName = haskellProject.getTargetName();
		if (targetName.equals("")) {
			targetName = "theResult";
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