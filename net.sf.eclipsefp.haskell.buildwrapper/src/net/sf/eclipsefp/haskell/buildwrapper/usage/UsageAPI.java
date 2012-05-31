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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;
import net.sf.eclipsefp.haskell.buildwrapper.types.Module;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineResult;
import net.sf.eclipsefp.haskell.buildwrapper.types.SymbolDef;
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
 * The business-oriented API for Usage searches
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
	
	/**
	 * add a given file to the db
	 * @param p the project
	 * @param c the cabal component
	 * @param relPath the file project relative path
	 */
	public void addFile(IProject p,Component c, String relPath){
		IFile f=p.getFile(relPath);
		if (f!=null && f.exists()){
			//BuildWrapperPlugin.log(IStatus.INFO, "Adding "+p.getName()+"/"+f.getProjectRelativePath().toPortableString(), null);
			IFile uf=getUsageFile(p, relPath);
			// check usage file exists AND has been generated after the source file
			if (uf!=null && uf.getLocation().toFile().lastModified()>f.getLocation().toFile().lastModified()){
				//BuildWrapperPlugin.log(IStatus.INFO, "Adding "+p.getName()+"/"+f.getProjectRelativePath().toPortableString()+": usage file found", null);
				JSONArray arr=parseUsageFile(uf);
				if (arr!=null){
					try {
						String pkg=formatPackage(arr.getString(0));
						String module=formatModule(c, arr.getString(1));
						String loc=arr.optString(2);
						if (module!=null){
							long fileID=db.getFileID(f);
							db.clearUsageInFile(fileID);
							
							//long moduleID=
							db.getModuleID(pkg, module, fileID, loc);
							List<ObjectUsage> modUsages=new ArrayList<ObjectUsage>();
							List<ObjectUsage> objUsages=new ArrayList<ObjectUsage>();
							List<ObjectUsage> objDefs=new ArrayList<ObjectUsage>();
							buildUsage(fileID,arr,modUsages,objDefs,objUsages);
							db.setModuleUsages(fileID, modUsages);
							db.setSymbolDefinitions(fileID, objDefs);
							db.setSymbolUsages(fileID, objUsages);
							db.commit();
							JSONObject outline=arr.optJSONObject(4);
							if (outline!=null){
								OutlineResult or=new OutlineResult(f, outline);
								BuildWrapperPlugin.getFacade(p).registerOutline(f, or);
							}
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
	
	private void buildUsage(long fileID,JSONArray arr,List<ObjectUsage> modUsages,List<ObjectUsage> objDefs,List<ObjectUsage> objUsages){
		JSONObject pkgs=arr.optJSONObject(3);
		if (pkgs!=null){
			for (Iterator<String> itPkg=pkgs.keys();itPkg.hasNext();){
				String pkgKey=itPkg.next();
				JSONObject mods=pkgs.optJSONObject(pkgKey);
				if (mods!=null){
					String pkg=formatPackage(pkgKey);
					buildUsageModule(fileID,pkg,mods,modUsages,objDefs,objUsages);
				}
			}
		}
	}
	
	private void buildUsageModule(long fileID,String pkg,JSONObject mods,List<ObjectUsage> modUsages,List<ObjectUsage> objDefs,List<ObjectUsage> objUsages){
		for (Iterator<String> itMod=mods.keys();itMod.hasNext();){
			String modKey=itMod.next();
			JSONObject types=mods.optJSONObject(modKey);
			if (types!=null){
				try {
					long modID=db.getModuleID(pkg, modKey, null,null);
				
					buildUsage(fileID,modID,types.optJSONObject("vars"),false,modUsages,objDefs,objUsages);
					buildUsage(fileID,modID,types.optJSONObject("types"),true,modUsages,objDefs,objUsages);
				} catch (SQLException sqle){
					BuildWrapperPlugin.logError(BWText.error_db, sqle);
				}
			}
		}
	}
	
	private void buildUsage(long fileID,long modID,JSONObject symbols,boolean isType,List<ObjectUsage> modUsages,List<ObjectUsage> objDefs,List<ObjectUsage> objUsages) throws SQLException{
		if (symbols!=null){
			for (Iterator<String> itSym=symbols.keys();itSym.hasNext();){
				String symKey=itSym.next();
				JSONArray locs=symbols.optJSONArray(symKey);
				if (locs!=null){
					
					for (int a=0;a<locs.length();a++){
						JSONObject secLoc=locs.optJSONObject(a);
						if (secLoc!=null){
							String sec=secLoc.optString("s");
							JSONArray loc=secLoc.optJSONArray("l");
							if (sec!=null && loc!=null){
								String sloc=loc.toString();
								if (!isType && symKey.length()==0){
									modUsages.add(new ObjectUsage(modID, sec,sloc));
								} else {
									boolean def=secLoc.optBoolean("d", false);
									int type=UsageQueryFlags.TYPE_VAR;
									if (isType){
										type=UsageQueryFlags.TYPE_TYPE;
									} else if (Character.isUpperCase(symKey.charAt(0))){
										type=UsageQueryFlags.TYPE_CONSTRUCTOR;
									}
									long symbolID=db.getSymbolID(modID, symKey, type);
									if (def){
										//, sec, sloc
										objDefs.add(new ObjectUsage(symbolID, sec, sloc));
									} else {
										objUsages.add(new ObjectUsage(symbolID, sec, sloc));
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	/**
	 * get the IFile identified with the given db identifier
	 * @param fileid
	 * @return
	 */
	public IFile getFile(Long fileid){
		try {
			return db.getFile(fileid);
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return null;
	}
	
	/**
	 * list all modules defined in the workspace
	 * @return
	 */
	public List<Module> listLocalModules() {
		try {
			return db.listLocalModules();
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return new ArrayList<Module>();
	}
	
	/**
	 * do we have info about this project in the db?
	 * @param p
	 * @return
	 */
	public boolean knowsProject(IProject p){
		try {
			return db.knowsProject(p.getName());
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return false;
	}
	
	/**
	 * get all references for a module
	 * @param pkg
	 * @param module
	 * @param p
	 * @param exact
	 * @return
	 */
	public UsageResults getModuleReferences(String pkg,String module,IProject p,boolean exact){
		try {
			return db.getModuleReferences(pkg, module,p,exact);
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return new UsageResults();
	}
	
	/**
	 * get definitions for a module
	 * @param pkg
	 * @param module
	 * @param p
	 * @param exact
	 * @return
	 */
	public UsageResults getModuleDefinitions(String pkg,String module,IProject p,boolean exact){
		try {
			return db.getModuleDefinitions(pkg, module,p,exact);
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return new UsageResults();
	}
	
	/**
	 * get references for a symbol
	 * @param pkg
	 * @param module
	 * @param symbol
	 * @param type
	 * @param p
	 * @param exact
	 * @return
	 */
	public UsageResults getSymbolReferences(String pkg,String module,String symbol, int type,IProject p,boolean exact){
		try {
			return db.getSymbolReferences(pkg, module,symbol,type,p,exact);
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return new UsageResults();
	}
	
	
	/**
	 * get definitions for a symbol
	 * @param pkg
	 * @param module
	 * @param symbol
	 * @param type
	 * @param p
	 * @param exact
	 * @return
	 */
	public UsageResults getSymbolDefinitions(String pkg,String module,String symbol, int type,IProject p,boolean exact){
		try {
			return db.getSymbolDefinitions(pkg, module,symbol,type,p,exact);
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return new UsageResults();
	}
	
	/**
	 * perform an exact search
	 * @param pkg package restriction (null for everything)
	 * @param term term to search for
	 * @param p project restriction (null for everything)
	 * @param typeFlags @see {@link UsageQueryFlags}
	 * @param scopeFlags @see {@link UsageQueryFlags}
	 * @return the results
	 */
	public UsageResults exactSearch(String pkg,String term,IProject p,int typeFlags,int scopeFlags){
		UsageResults ret=new UsageResults();
		if ((typeFlags & UsageQueryFlags.TYPE_MODULE) == UsageQueryFlags.TYPE_MODULE){
			if ((scopeFlags & UsageQueryFlags.SCOPE_DEFINITIONS) ==UsageQueryFlags.SCOPE_DEFINITIONS){
				ret.add(getModuleDefinitions(pkg, term, p,true));
			}
			if ((scopeFlags & UsageQueryFlags.SCOPE_REFERENCES) ==UsageQueryFlags.SCOPE_REFERENCES){
				ret.add(getModuleReferences(pkg, term, p,true));
			}
		}
		int ix=term.lastIndexOf('.');
		if (ix>-1 && ix<term.length()-1){
			String module=term.substring(0,ix);
			String symbol=term.substring(ix+1);
			if (Character.isUpperCase(symbol.charAt(0))){
				symbolSearch(ret, pkg, module, symbol, UsageQueryFlags.TYPE_TYPE, p, typeFlags, scopeFlags,true);
				symbolSearch(ret, pkg, module, symbol, UsageQueryFlags.TYPE_CONSTRUCTOR, p, typeFlags, scopeFlags,true);
			} else {
				symbolSearch(ret, pkg, module, symbol, UsageQueryFlags.TYPE_VAR, p, typeFlags, scopeFlags,true);
			}
		}
		
		return ret;
	}
	
	/**
	 * search for a symbol
	 * @param ret
	 * @param pkg
	 * @param module
	 * @param symbol
	 * @param type
	 * @param p
	 * @param typeFlags
	 * @param scopeFlags
	 * @param exact
	 */
	private void symbolSearch(UsageResults ret,String pkg,String module,String symbol,int type,IProject p,int typeFlags,int scopeFlags,boolean exact){
		if ((typeFlags & type) == type){
			if ((scopeFlags & UsageQueryFlags.SCOPE_DEFINITIONS) ==UsageQueryFlags.SCOPE_DEFINITIONS){
				ret.add(getSymbolDefinitions(pkg, module,symbol,type, p,exact));
			}
			if ((scopeFlags & UsageQueryFlags.SCOPE_REFERENCES) ==UsageQueryFlags.SCOPE_REFERENCES){
				ret.add(getSymbolReferences(pkg,  module,symbol,type, p,exact));
			}
		}
	}
	
	
	/**
	 * perform an like search
	 * @param pkg package restriction (null for everything)
	 * @param term term to search for
	 * @param p project restriction (null for everything)
	 * @param typeFlags @see {@link UsageQueryFlags}
	 * @param scopeFlags @see {@link UsageQueryFlags}
	 * @return the results
	 */
	public UsageResults likeSearch(String pkg,String term,IProject p,int typeFlags,int scopeFlags){
		UsageResults ret=new UsageResults();
		
		
		// replace Eclipse wildcards by SQLLite wildcards
		StringBuilder newTerm=new StringBuilder();
		for (int a=0;a<term.length();a++){
			char c=term.charAt(a);
			Character prev=a>0?term.charAt(a-1):null;
			boolean isEscaped=prev!=null && prev.charValue()=='\\';
			if (c=='%'){
				newTerm.append("\\%");
			} else if (c=='_'){
				newTerm.append("\\_");
			} else if (c=='*'){
				if (isEscaped){
					newTerm.replace(newTerm.length()-1, 1, "*");
				} else {
					newTerm.append("%");
				}
			} else if (c=='?' && !isEscaped){
				if (isEscaped){
					newTerm.replace(newTerm.length()-1, 1, "?");
				} else {
					newTerm.append("_");
				}
			} else {
				newTerm.append(c);
			}
		}
		String sqlLiteTerm=newTerm.toString();
		if ((typeFlags & UsageQueryFlags.TYPE_MODULE) == UsageQueryFlags.TYPE_MODULE){
			if ((scopeFlags & UsageQueryFlags.SCOPE_DEFINITIONS) ==UsageQueryFlags.SCOPE_DEFINITIONS){
				ret.add(getModuleDefinitions(pkg, sqlLiteTerm, p,false));
			}
			if ((scopeFlags & UsageQueryFlags.SCOPE_REFERENCES) ==UsageQueryFlags.SCOPE_REFERENCES){
				ret.add(getModuleReferences(pkg, sqlLiteTerm, p,false));
			}
		}
		if (typeFlags != UsageQueryFlags.TYPE_MODULE){
			// there is a dot: qualified name
			int ix=sqlLiteTerm.lastIndexOf(".");
			List<String> mods=Collections.singletonList(null);
			try {
				if (ix>-1){
					String modTerm=sqlLiteTerm.substring(0,ix);
					mods=db.findModules(pkg, modTerm, p);
				} 
				for (String m:mods){
					symbolSearch(ret, pkg, m, sqlLiteTerm, UsageQueryFlags.TYPE_TYPE, p, typeFlags, scopeFlags,false);
					symbolSearch(ret, pkg, m, sqlLiteTerm, UsageQueryFlags.TYPE_CONSTRUCTOR, p, typeFlags, scopeFlags,false);
					symbolSearch(ret, pkg, m, sqlLiteTerm, UsageQueryFlags.TYPE_VAR, p, typeFlags, scopeFlags,false);
				}
			} catch (SQLException sqle){
				BuildWrapperPlugin.logError(BWText.error_db, sqle);
			} 
		}
		
		
		return ret;
	}
	
	public List<SymbolDef> listDefinedSymbols(IProject p){
		try {
			return db.listDefinedSymbols(p);
		} catch (SQLException sqle){
			BuildWrapperPlugin.logError(BWText.error_db, sqle);
		} 
		return new ArrayList<SymbolDef>();
	}
	
	
	/**
	 * parse the file generated by BuildWrapper
	 * @param uf
	 * @return
	 */
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
	
	/**
	 * get the usage file generated by BuildWrapper
	 * @param p
	 * @param relPath
	 * @return
	 */
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
	
	/**
	 * get the package in the proper format (we drop the version)
	 * @param pkg
	 * @return
	 */
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
	
	/**
	 * get the module in the proper format (Main modules get postfixed by the cabal component name to distinguish them)
	 * @param c
	 * @param module
	 * @return
	 */
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
