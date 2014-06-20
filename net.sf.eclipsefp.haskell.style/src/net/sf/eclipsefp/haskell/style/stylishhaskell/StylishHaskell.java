/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.stylishhaskell;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.eclipsefp.haskell.style.StylePlugin;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.ProcessRunner;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

/**
 * Utilities to invoke stylish-haskell and manage its configuration files
 * @author JP Moresmau
 *
 */
public class StylishHaskell {

	/**
	 * run stylish haskell
	 * @param exe the path to the exe (may not be full if we hope it's in the path)
	 * @param project the project we're in
	 * @param filePath the file to format
	 * @param charset the charset of the file
	 * @param extensions the Haskell extensions needed to parse the file correctly
	 * @throws Exception
	 */
	public static void runStylishHaskell(String exe, IProject project, File filePath,String charset,Set<String> extensions) throws Exception{
		List<String> cmd=new ArrayList<>();
		cmd.add(exe);
		cmd.add("-i"); // in place
		String cf=getConfigFile(project);
		File tempFile=null;
		if (cf!=null){
			// add the required extensions
			if (extensions!=null && extensions.size()>0){
				SHConfiguration conf=load(cf);
				conf.getLanguageExtensions().addAll(extensions);
				tempFile=File.createTempFile("stylish", ".yaml");
				save(conf,tempFile);
				cf=tempFile.getAbsolutePath();
			}
			
			cmd.add("--config="+cf);
		}
		try {
			cmd.add(filePath.getAbsolutePath());
			// keep old contents
			String contents=FileUtil.getContents(filePath, charset);
	
			// capture errors
			try (StringWriter swErr=new StringWriter();
					StringWriter swOut=new StringWriter()) {
				int code=new ProcessRunner().executeBlocking(filePath.getParentFile(), swOut, swErr,cmd.toArray(new String[cmd.size()]));
				String err=swErr.toString();
				if (code!=0 || err.length()>0){
					// we restore the file contents if we failed
					FileUtil.writeSharedFile(filePath, contents, 1);
					throw new IOException(err);
				}
			}
		} finally {
			if (tempFile!=null){
				tempFile.delete();
			}
		}
	}
	
	/**
	 * get the path of the config file to use for a given project, or null to use defaults
	 * @param project the project
	 * @return
	 */
	public static String getConfigFile(IProject project){
		IFile f=project.getFile(".stylish-haskell.yaml");
		if (f!=null && f.exists()){
			return f.getLocation().toOSString();
		}
		IPath pluginp=StylePlugin.getStylePlugin().getStateLocation().append(".stylish-haskell.yaml");
		File jf=pluginp.toFile();
		if (jf.exists() && jf.isFile()){
			return jf.getAbsolutePath();
		}
		return null;
	}
	
	/**
	 * has this project a specific config?
	 * @param project
	 * @return
	 */
	public static boolean hasProjectConfiguration(IProject project){
		IFile f=project.getFile(".stylish-haskell.yaml");
		return f!=null && f.exists();
	}
	
	/**
	 * get the configuration to display for a project
	 * @param project
	 * @return the configuration, non null (may be the default)
	 */
	public static SHConfiguration getProjectConfiguration(IProject project){
		IFile f=project.getFile(".stylish-haskell.yaml");
		if (f!=null && f.exists()){
			try {
				return load(f.getLocation().toOSString());
			} catch (IOException ioe){
				StylePlugin.logError(ioe);
			}
		}
		return getWorkspaceConfiguration();
	}
	
	/**
	 * set the project configuration
	 * @param conf the configuration, or null to not use a project-specific config
	 * @param project
	 * @throws Exception
	 */
	public static void setProjectConfiguration(SHConfiguration conf,IProject project) throws Exception{
		IFile f=project.getFile(".stylish-haskell.yaml");
		if (conf!=null){
			
			try {
				try (ByteArrayOutputStream os=new ByteArrayOutputStream()) {
					save(conf, os);
	                if (!f.exists()){
	                    f.create(new ByteArrayInputStream(os.toByteArray()), true,  new NullProgressMonitor());
	                } else {
	                    f.setContents(new ByteArrayInputStream(os.toByteArray()), true, true,  new NullProgressMonitor());
	                }
				}
			} catch (IOException ioe){
				StylePlugin.logError(ioe);
				throw ioe;
			}
		} else if (f.exists()){
			try {
				f.delete(true, new NullProgressMonitor());
			} catch (CoreException ce){
				StylePlugin.logError(ce);
				throw ce;
			}
		}
		project.refreshLocal(IResource.DEPTH_ONE, new NullProgressMonitor());
	}
	
	/**
	 * get the workspace configuration
	 * @return the configuration, non null (may be the default)
	 */
	public static SHConfiguration getWorkspaceConfiguration(){
		IPath pluginp=StylePlugin.getStylePlugin().getStateLocation().append(".stylish-haskell.yaml");
		File jf=pluginp.toFile();
		if (jf.exists() && jf.isFile()){
			try {
				return load(jf.getAbsolutePath());
			} catch (IOException ioe){
				StylePlugin.logError(ioe);
			}
		}
		return new SHConfiguration();
	}
	
	/**
	 * set the workspace configuration
	 * @param conf
	 * @throws IOException
	 */
	public static void setWorkspaceConfiguration(SHConfiguration conf) throws IOException{
		IPath pluginp=StylePlugin.getStylePlugin().getStateLocation().append(".stylish-haskell.yaml");
		File jf=pluginp.toFile();
		jf.getParentFile().mkdirs();
		
		try (OutputStream os=new BufferedOutputStream(new FileOutputStream(jf))) {
			save(conf, os);
		} catch (IOException ioe){
			StylePlugin.logError(ioe);
			throw ioe;
		}
		
	}
	
	/**
	 * load YAML from a file
	 * @param fileLocation
	 * @return
	 * @throws IOException
	 */
	public static SHConfiguration load(String fileLocation) throws IOException{
		File f=new File(fileLocation);
		try (InputStream is=new BufferedInputStream(new FileInputStream(f))) {
			SHConfiguration config=load(is);
			return config;
		}
	}
	
	/**
	 * load YAML from a stream
	 * @param is
	 * @return
	 * @throws IOException
	 */
	public static SHConfiguration load(InputStream is) throws IOException{
		Yaml y=new Yaml();
		Object o=y.load(is);
		SHConfiguration config=new SHConfiguration();
		config.fromYAML(o);
		return config;
	}
	
	/**
	 * save to a stream as YAML
	 * @param config
	 * @param os
	 * @throws IOException
	 */
	public static void save(SHConfiguration config,OutputStream os) throws IOException {
		
		Map<String,Object> o=config.toYAML();
		DumperOptions options = new DumperOptions();
		options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
		Yaml yaml = new Yaml(options);
		String s=yaml.dump(o);
		os.write(s.getBytes()); // platform encoding
	}
	
	/**
	 * save to a file as YAML
	 * @param config
	 * @param f
	 * @throws IOException
	 */
	public static void save(SHConfiguration config,File f) throws IOException {
		if (f.getParentFile()!=null){
			f.getParentFile().mkdirs();
		}
		try (OutputStream os=new BufferedOutputStream(new FileOutputStream(f))) {
			save(config, os);
		}
	}
}
