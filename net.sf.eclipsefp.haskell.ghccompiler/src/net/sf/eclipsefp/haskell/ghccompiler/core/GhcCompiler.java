// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.core.compiler.AbstractHaskellCompiler;
import net.sf.eclipsefp.haskell.core.compiler.CompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.CompilerOutputItem;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
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
public class GhcCompiler extends AbstractHaskellCompiler {

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

	// interface methods of AbstractHaskellCompiler
	// /////////////////////////////////////////////

	public String[] buildCommandLine(final IFile file,
			final IHaskellProject haskellProject) {
		if (trace) {
			System.out.println("Constructing command line for file " + file);
		}

		IProject project = haskellProject.getResource();
		String outDir = getAbsPath(project, haskellProject.getOutputPath());

		ArrayList cmdLine = new ArrayList();
		// command and special options
		cmdLine.add(Util.getCompilerExecutable());
		String libPath = Util.constructLibPath(haskellProject);
		if (!libPath.equals("")) {
			cmdLine.add(libPath);
		}
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
		return toArray(cmdLine);
	}

	// helping methods
	// ////////////////

	@Override
	public ICompilerOutput compile(IFile file) {
		final IProject project = file.getProject();
		IHaskellProject hsProject = HaskellProjectManager.get(project);
		String[] cmdLine = buildCommandLine(file, hsProject);
		IPath src = project.getLocation().append(hsProject.getSourcePath());
		String output = fProcessRunner.execute(
			new File(src.toOSString()), cmdLine);
		return parse(output);
	}

	private ICompilerOutput parse(String messages) {
		final CompilerOutput output = new CompilerOutput(0, messages, new ArrayList<Exception>(0));
		final CompilerOutputItem item = new CompilerOutputItem();
		
		messages = messages.trim();

		String remainder = extractFileName(item, messages);
		remainder = extractLine(item, remainder);
		remainder = extractColumnRange(item, remainder);
		remainder = extractComment(item, remainder);

		output.addError(item);
		return output;
	}

	private String extractFileName(CompilerOutputItem item, String text) {
		String[] split = text.split(":", 2);
		item.setFileName(split[0]);
		return split[1];
	}

	private String extractLine(CompilerOutputItem item, String text) {
		String[] split = text.split(":", 2);
		item.setLine(Integer.parseInt(split[0]));
		return split[1];
	}

	private String extractColumnRange(CompilerOutputItem item, String text) {
		String[] split = text.split(":", 2);
		String[] columns = split[0].split("-", 2);
		item.setStartColumn(Integer.parseInt(columns[0]));
		item.setEndColumn(Integer.parseInt(columns[1]));
		return split[1];
	}

	private String extractComment(CompilerOutputItem item, String text) {
		item.setComment(text.trim());
		return "";
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

	private final String[] toArray(final List list) {
		String[] result = new String[list.size()];
		list.toArray(result);
		return result;
	}
}