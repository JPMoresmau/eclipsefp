/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.stylishhaskell;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.style.StylePlugin;
import net.sf.eclipsefp.haskell.util.ProcessRunner;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

/**
 * Utilities to invoke stylish-haskell
 * @author JP Moresmau
 *
 */
public class StylishHaskell {

	
	public static void runStylishHaskell(String exe, IProject project, File filePath) throws Exception{
		List<String> cmd=new ArrayList<String>();
		cmd.add(exe);
		cmd.add("-i"); // in place
		String cf=getConfigFile(project);
		if (cf!=null){
			cmd.add("--config="+cf);
		}
		cmd.add(filePath.getAbsolutePath());
		Process p = Runtime.getRuntime()
                .exec( cmd.toArray(new String[cmd.size()]), null, filePath.getParentFile() );
        ProcessRunner.consume( p );
        p.waitFor();
	}
	
	public static String getConfigFile(IProject project){
		IFile f=project.getFile(".stylish-haskell.yaml");
		if (f!=null && f.exists()){
			return f.getFullPath().toOSString();
		}
		IPath pluginp=StylePlugin.getStylePlugin().getStateLocation().append(".stylish-haskell.yaml");
		File jf=pluginp.toFile();
		if (jf.exists() && jf.isFile()){
			return jf.getAbsolutePath();
		}
		return null;
	}
}
