/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.usage;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;
import net.sf.eclipsefp.haskell.buildwrapper.types.Module;
import net.sf.eclipsefp.haskell.buildwrapper.types.UsageResults;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

/**
 * @author JP Moresmau
 *
 */
public class UsageAPI {
	private UsageDB db=new UsageDB();
	
	public void close(){
		db.close();
	}
	
	public void removeFile(IFile f){
		if (f!=null){
			try {
				db.removeFile(f);
				db.commit();
			} catch (SQLException sqle){
				BuildWrapperPlugin.logError(BWText.error_db, sqle);
			}
		}
	}
	
	public void removeFile(IProject p, String relPath){
		removeFile(p.getFile(relPath));
	}
	
	public void addFile(Component c,IFile f){
		addFile(f.getProject(),c,f.getProjectRelativePath().toOSString());
	}
	
	public void addFile(IProject p,Component c, String relPath){
		IFile f=p.getFile(relPath);
		if (f!=null && f.exists()){
			//BuildWrapperPlugin.log(IStatus.INFO, "Adding "+p.getName()+"/"+f.getProjectRelativePath().toPortableString(), null);
			IFile uf=getUsageFile(p, relPath);
			if (uf!=null){
				//BuildWrapperPlugin.log(IStatus.INFO, "Adding "+p.getName()+"/"+f.getProjectRelativePath().toPortableString()+": usage file found", null);
				JSONArray arr=parseUsageFile(uf);
				if (arr!=null){
					try {
						String pkg=formatPackage(arr.getString(0));
						String module=formatModule(c, arr.getString(1));
						if (module!=null){
							long fileID=db.getFileID(f);
							db.clearUsageInFile(fileID);
							
							//long moduleID=
							db.getModuleID(pkg, module, fileID);
							List<ModuleUsage> modUsages=new ArrayList<ModuleUsage>();
							buildUsage(arr,modUsages);
							db.setModuleUsages(fileID, modUsages);
							db.commit();
						}
					} catch (SQLException sqle){
						BuildWrapperPlugin.logError(BWText.error_db, sqle);
					} catch (JSONException je){
						BuildWrapperPlugin.logError(BWText.error_parsing_usage_file, je);
					}
				}
			}
		}
	}
	
	private void buildUsage(JSONArray arr,List<ModuleUsage> modUsages){
		JSONObject pkgs=arr.optJSONObject(2);
		if (pkgs!=null){
			for (Iterator<String> itPkg=pkgs.keys();itPkg.hasNext();){
				String pkgKey=itPkg.next();
				JSONObject mods=pkgs.optJSONObject(pkgKey);
				if (mods!=null){
					String pkg=formatPackage(pkgKey);
					buildUsageModule(pkg,mods,modUsages);
				}
			}
		}
	}
	
	private void buildUsageModule(String pkg,JSONObject mods,List<ModuleUsage> modUsages){
		for (Iterator<String> itMod=mods.keys();itMod.hasNext();){
			String modKey=itMod.next();
			JSONObject types=mods.optJSONObject(modKey);
			if (types!=null){
				try {
					long modID=db.getModuleID(pkg, modKey, null);
				
					buildUsage(modID,types.optJSONObject("vars"),false,modUsages);
				} catch (SQLException sqle){
					BuildWrapperPlugin.logError(BWText.error_db, sqle);
				}
			}
		}
	}
	
	private void buildUsage(long modID,JSONObject symbols,boolean isType,List<ModuleUsage> modUsages){
		if (symbols!=null){
			for (Iterator<String> itSym=symbols.keys();itSym.hasNext();){
				String symKey=itSym.next();
				JSONArray locs=symbols.optJSONArray(symKey);
				if (locs!=null){
					
					for (int a=0;a<locs.length();a++){
						JSONArray loc=locs.optJSONArray(a);
						if (loc!=null){
							String sloc=loc.toString();
							if (!isType && symKey.length()==0){
								modUsages.add(new ModuleUsage(modID, sloc));
							}
						}
					}
				}
			}
		}
	}
	
	public IFile getFile(Long fileid){
		try {
			return db.getFile(fileid);
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return null;
	}
	
	public List<Module> listLocalModules() {
		try {
			return db.listLocalModules();
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return new ArrayList<Module>();
	}
	
	public boolean knowsProject(IProject p){
		try {
			return db.knowsProject(p.getName());
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return false;
	}
	
	public UsageResults getModuleReferences(String pkg,String module,IProject p){
		try {
			return db.getModuleReferences(pkg, module,p);
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return new UsageResults();
	}
	
	private JSONArray parseUsageFile(IFile uf){
		try {
			InputStream is=uf.getContents();
			try {
				BufferedReader br = new BufferedReader(new InputStreamReader( is,uf.getCharset() ));
				try {
					return new JSONArray(new JSONTokener(br));
				} finally {
					br.close();
				}
			} finally {
				is.close();
			}
		} catch (Exception e){
			BuildWrapperPlugin.logError(BWText.error_parsing_usage_file, e);
		}
		return null;
	}
	
	private IFile getUsageFile(IProject p, String relPath){
		IFolder fldr=p.getFolder(BWFacade.DIST_FOLDER);
		if (fldr!=null && fldr.exists()){
			IFile f=fldr.getFile(relPath);
			IFile f2=((IFolder)f.getParent()).getFile("."+f.getName()+".bwusage");
			if (f2.exists()){
				return f2;
			}
		}
		return null;
	}
	
	private String formatPackage(String pkg){
		String ret=pkg;
		int ix=pkg.lastIndexOf('-');
		if (ix>-1 && ix<pkg.length()-1){
			if (Character.isDigit(pkg.charAt(ix+1))){
				ret=pkg.substring(0,ix);
			}
		}
		return ret;
	}
	
	private String formatModule(Component c,String module){
		String ret=module;
		if("Main".equals(module)){
			if (c.getName()==null){ // Main but a library, ignore
				return null;
			}
			ret=module+" "+c.getName();
		}
		return ret;
	}
	
	
}
