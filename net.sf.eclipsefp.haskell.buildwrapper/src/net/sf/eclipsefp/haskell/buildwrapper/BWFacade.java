package net.sf.eclipsefp.haskell.buildwrapper;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import net.sf.eclipsefp.haskell.buildwrapper.types.BWTarget;
import net.sf.eclipsefp.haskell.buildwrapper.types.BuildFlags;
import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalMessages;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;
import net.sf.eclipsefp.haskell.buildwrapper.types.NameDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPoint;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component.ComponentType;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.Note;
import net.sf.eclipsefp.haskell.buildwrapper.types.Note.Kind;
import net.sf.eclipsefp.haskell.buildwrapper.types.Occurrence;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineResult;
import net.sf.eclipsefp.haskell.buildwrapper.types.TokenDef;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.LangUtil;
import net.sf.eclipsefp.haskell.util.OutputWriter;
import net.sf.eclipsefp.haskell.util.SingleJobQueue;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * API facade to buildwrapper: exposes all operations and calls the build wrapper executable
 * @author JP Moresmau
 *
 */
public class BWFacade {
	public static final String DIST_FOLDER=".dist-buildwrapper";
	public static final String DIST_FOLDER_CABAL=DIST_FOLDER+"/dist";
	
	private static final String prefix="build-wrapper-json:";
	
	private static boolean showedNoExeError=false; 
	
	private String bwPath;
	//private String tempFolder=".dist-buildwrapper";
	private String cabalPath;
	private String cabalFile;
	private String cabalShortName;
	
	private String flags;
	private List<String> extraOpts=new LinkedList<String>();
	
	private File workingDir;
	
	private Writer outStream;
	
	private IProject project;
	
	private OutputWriter ow;
	
	private List<Component> components;
	private Map<String, CabalPackage[]> packageDB;
	
	/**
	 * where ever we come from, we only launch one build operation at a time, and lose the intermediate operations
	 */
	private SingleJobQueue buildJobQueue=new SingleJobQueue();
	/**
	 * query for thing at point for a given file, so that we never have more than two jobs at one time
	 */
	private Map<IFile,SingleJobQueue> tapQueuesByFiles=new HashMap<IFile, SingleJobQueue>();
	
	/**
	 * map of outlines for files (key is relative path)
	 */
	private Map<String,OutlineResult> outlines=new HashMap<String, OutlineResult>();
	
	
	/**
	 * map of flag info for files
	 */
	//private Map<IFile, BuildFlagInfo> flagInfos=new HashMap<IFile, BuildFlagInfo>();
	
	
	/**
	 * do we need to set derived on dist dir?
	 */
	private boolean needSetDerivedOnDistDir=false;
	
	private boolean hasCabalProblems=false;
	
	public SingleJobQueue getBuildJobQueue() {
		return buildJobQueue;
	}
	
	public synchronized SingleJobQueue getThingAtPointJobQueue(IFile f){
		SingleJobQueue sjq=tapQueuesByFiles.get(f);
		if (sjq==null){
			sjq=new SingleJobQueue();
			tapQueuesByFiles.put(f, sjq);
		}
		return sjq;
	}
	
	private void deleteCabalProblems(){
		String relCabal=getCabalFile().substring(getProject().getLocation().toOSString().length());
		IFile f=getProject().getFile(relCabal);
		if (f!=null && f.exists()){
			BuildWrapperPlugin.deleteProblems(f);
		}
	}
	
	public JSONArray build(BuildOptions buildOptions){
		JSONArray arrC=null;
		if (buildOptions.isConfigure()){
			arrC=configure(buildOptions);
			if (!isOK(arrC)){
				return arrC;
			}
		}
		
		if (hasCabalProblems){
			deleteCabalProblems();
			hasCabalProblems=false;
		}

		LinkedList<String> command=new LinkedList<String>();
		command.add("build");
		command.add("--output="+buildOptions.isOutput());
		command.add("--cabaltarget="+buildOptions.getTarget().toString());
		JSONArray arr=run(command,ARRAY,false);
		if (arrC!=null){
			for (int a=0;a<arrC.length();a++){
				try {
					arr.put(a, arrC.get(a));
				} catch (JSONException je){
					BuildWrapperPlugin.logError(BWText.process_parse_note_error, je);
				}
			}
		}
		
		return arr;
	}
	
	public boolean parseBuildResult(JSONArray arr){
		if (arr!=null && arr.length()>1){
			Set<IResource> ress=new HashSet<IResource>();
			JSONObject obj=arr.optJSONObject(0);
			if (obj!=null){
				JSONArray files=obj.optJSONArray("fps");
				if (files!=null){
					for (int a=0;a<files.length();a++){
						String s=files.optString(a);
						if (s!=null && s.length()>0){
							final IResource res=project.findMember(s);
							if (res!=null){
								ress.add(res);
								BuildWrapperPlugin.deleteProblems(res);
							} 
						}
					}
				}
			}
			
			JSONArray notes=arr.optJSONArray(1);
			return parseNotes(notes,ress);
		}
		return true;
	}
	
//	private static String escapeFlags(String flag){
//		// not needed any more: we have encoded them in Base 64
//		//flag=flag.replace("\"", "\\\"");
//		return flag;
//	}
	
	/**
	 * build one file
	 * @param file the file to build the temp contents
	 * @return the names in scope or null if the build failed
	 */
	public Collection<NameDef> build1(IFile file){
		//BuildFlagInfo i=getBuildFlags(file);
		String path=file.getProjectRelativePath().toOSString();
		LinkedList<String> command=new LinkedList<String>();
		command.add("build1");
		command.add("--file="+path);
		//long t0=System.currentTimeMillis();
		//command.add("--buildflags="+escapeFlags(i.getFlags()));
		JSONArray arr=run(command,ARRAY);
		//long t1=System.currentTimeMillis();
		//BuildWrapperPlugin.logInfo("build:"+(t1-t0)+"ms");
		if (arr!=null && arr.length()>1){
			Set<IResource> ress=new HashSet<IResource>();
			ress.add(file);
			BuildWrapperPlugin.deleteProblems(file);
			JSONArray notes=arr.optJSONArray(1);
			//notes.putAll(i.getNotes());
			
			parseNotes(notes,ress);
			JSONArray names=arr.optJSONArray(0);
			if(names!=null){
				Collection<NameDef> ret=new ArrayList<NameDef>();
				for (int a=0;a<names.length();a++){
					try {
						ret.add(new NameDef(names.getJSONObject(a)));
					} catch (JSONException je){
						BuildWrapperPlugin.logError(BWText.process_parse_note_error, je);
					}
				}
				return ret;
			}
			
			
		}
		return null;
	}
	
//	public BuildFlagInfo getBuildFlags(IFile file){
//		BuildFlagInfo i=flagInfos.get(file);
//		if (i==null){
//			String path=file.getProjectRelativePath().toOSString();
//			LinkedList<String> command=new LinkedList<String>();
//			command.add("getbuildflags");
//			command.add("--file="+path);
//			JSONArray arr=run(command,ARRAY);
//			String s="";
//			JSONArray notes=new JSONArray();
//			if (arr!=null && arr.length()>1){
//				Set<IResource> ress=new HashSet<IResource>();
//				ress.add(file);
//				s=arr.optString(0);
//				notes=arr.optJSONArray(1);
//			}
//			i=new BuildFlagInfo(s, notes);
//			flagInfos.put(file, i);
//		}
//		return i;
//	}
	
	public JSONArray configure(BuildOptions buildOptions){
		parseFlags(); // reset flags in case they have changed
		//BuildWrapperPlugin.deleteProblems(getProject());
		LinkedList<String> command=new LinkedList<String>();
		command.add("configure");
		command.add("--cabaltarget="+buildOptions.getTarget().toString());
		JSONArray arr=run(command,ARRAY);
		if (arr!=null && arr.length()>1){
			JSONArray notes=arr.optJSONArray(1);
			return notes;
		}
		return null;
	}
	
	public void synchronize(boolean force){
		LinkedList<String> command=new LinkedList<String>();
		command.add("synchronize");
		command.add("--force="+force);
		JSONArray arr=run(command,ARRAY);
		if (arr!=null){
			if(arr.length()>1){
		
				JSONArray notes=arr.optJSONArray(1);
				parseNotes(notes);
			}
			JSONArray allPaths=arr.optJSONArray(0);
			if (allPaths!=null){
				JSONArray paths=allPaths.optJSONArray(0);
				if (paths!=null){
					for (int a=0;a<paths.length();a++){
						try {
							String p=paths.getString(a);
							if (p!=null && p.equals(cabalShortName)){
								cabalFileChanged();
							}
							// remove from cache if file has changed
							outlines.remove(p);
						} catch (JSONException je){
							BuildWrapperPlugin.logError(BWText.process_parse_component_error, je);
						}
					}
				}
				JSONArray dels=allPaths.optJSONArray(1);
				if (dels!=null){
					for (int a=0;a<dels.length();a++){
						try {
							String p=dels.getString(a);
							BuildWrapperPlugin.getDefault().getUsageAPI().removeFile(getProject(), p);
						} catch (JSONException je){
							BuildWrapperPlugin.logError(BWText.process_parse_component_error, je);
						}
					}
				}
			}
		}
	}
	
	public void generateUsage(Component c,boolean returnAll){
		LinkedList<String> command=new LinkedList<String>();
		command.add("generateusage");
		command.add("--cabalcomponent="+serializeComponent(c));
		command.add("--returnall="+returnAll);
		JSONArray arr=run(command,ARRAY);
		if (arr!=null){
			if(arr.length()>1){
		
				JSONArray notes=arr.optJSONArray(1);
				parseNotes(notes);
			}
			JSONArray allPaths=arr.optJSONArray(0);
			if (allPaths!=null){
				if (allPaths.length()>0){
					IFolder fldr=getProject().getFolder(BWFacade.DIST_FOLDER);
					if (fldr!=null && fldr.exists()){
						try {
							fldr.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
						} catch (CoreException ce){
							BuildWrapperPlugin.logError(BWText.error_refreshLocal, ce);
						}
					}
				}
				for (int a=0;a<allPaths.length();a++){
					try {
						String p=allPaths.getString(a);
						BuildWrapperPlugin.getDefault().getUsageAPI().addFile(getProject(),c, p);
					} catch (JSONException je){
						BuildWrapperPlugin.logError(BWText.error_parsing_usage_path, je);
					}
				}
			}
		}
	}
	
	public void cabalFileChanged(){
		components=null;
		packageDB=null;
		//flagInfos.clear();
	}
	
	public boolean synchronize1(IFile file,boolean force){
		String path=file.getProjectRelativePath().toOSString();
		LinkedList<String> command=new LinkedList<String>();
		command.add("synchronize1");
		command.add("--file="+path);
		command.add("--force="+force);
		String s=run(command,STRING);
		return s!=null;
	}
	
	public File write(IFile file,String contents){
		/*String path=file.getProjectRelativePath().toOSString();
		LinkedList<String> command=new LinkedList<String>();
		command.add("write");
		command.add("--file="+path);
		//command.add("--contents="+contents);
		//command.add("--contents=\""+contents.replace("\"", "\\\"")+"\"");
		command.add("--contents="+contents.replace("\"", "\\\""));
		String s=run(command,STRING);
		return s!=null;*/
		try {
			String path=file.getProjectRelativePath().toOSString();
			File tgt=new File(new File(workingDir,DIST_FOLDER),path);
			tgt.getParentFile().mkdirs();
			write(file,tgt, contents);
			return tgt;
		} catch (Exception e){
			BuildWrapperPlugin.logError(e.getLocalizedMessage(), e);
		}
		return null;
	}
	
	public boolean write(IFile file,File tgt,String contents){
		outlines.remove(file.getProjectRelativePath().toOSString());
		try {
			FileUtil.writeSharedFile(tgt, contents, 5);
		} catch (Exception e){
			BuildWrapperPlugin.logError(e.getLocalizedMessage(), e);
		}
		return false;
	}
	
	public List<Component> getComponents(){
		if (components!=null){
			return components;
		}
		LinkedList<String> command=new LinkedList<String>();
		command.add("components");
		JSONArray arr=run(command,ARRAY);
		List<Component> cps=new LinkedList<Component>();
		if (arr!=null){
			if (arr.length()>1){
		
				JSONArray notes=arr.optJSONArray(1);
				parseNotes(notes);
			}
			JSONArray objs=arr.optJSONArray(0);
			for (int a=0;a<objs.length();a++){
				try {
					cps.add(parseComponent(objs.getJSONObject(a)));
				} catch (JSONException je){
					BuildWrapperPlugin.logError(BWText.process_parse_component_error, je);
				}
			}
		}
		components=cps;
		return components;
	}
	
	public Map<String, CabalPackage[]> getPackagesByDB() {
		if (packageDB!=null){
			return packageDB;
		}
		LinkedList<String> command=new LinkedList<String>();
		command.add("dependencies");
		JSONArray arr=run(command,ARRAY);
		if (arr==null){
			return new HashMap<String, CabalPackage[]>();
		}
		Map<String, CabalPackage[]> cps=new HashMap<String, CabalPackage[]>();
		if (arr!=null && arr.length()>1){
			JSONArray notes=arr.optJSONArray(1);
			parseNotes(notes);
		}
		JSONArray objs=arr.optJSONArray(0);
		for (int a=0;a<objs.length();a++){
			try {
				JSONArray arr1=objs.getJSONArray(a);
				String fp=arr1.getString(0);
				JSONArray arr2=arr1.getJSONArray(1);
				CabalPackage[] pkgs=new CabalPackage[arr2.length()];
				for (int b=0;b<arr2.length();b++){
					pkgs[b]=parsePackage(arr2.getJSONObject(b));
				}
				cps.put(fp, pkgs);
			} catch (JSONException je){
				BuildWrapperPlugin.logError(BWText.process_parse_package_error, je);
			}
		}
		packageDB=cps;
		return packageDB;
	}
	
	public OutlineResult outline(IFile file){

		String path=file.getProjectRelativePath().toOSString();
		OutlineResult or=outlines.get(path);
		if (or!=null){
			return or;
		}
		//BuildFlagInfo i=getBuildFlags(file);
		LinkedList<String> command=new LinkedList<String>();
		command.add("outline");
		command.add("--file="+path);
		//command.add("--buildflags="+escapeFlags(i.getFlags()));
		JSONArray arr=run(command,ARRAY);
		or=new OutlineResult();
		if (arr!=null){
			if (arr.length()>1){
				JSONArray notes=arr.optJSONArray(1);
				//notes.putAll(i.getNotes());
				boolean b=parseNotes(notes);
				or.setBuildOK(b);
			}
			JSONObject obj=arr.optJSONObject(0);
			if (obj!=null){
				try {
					or=new OutlineResult(file, obj);
				} catch (JSONException je){
					BuildWrapperPlugin.logError(BWText.process_parse_outline_error, je);
				}
			} else {
				// old version pre 0.2.3
				JSONArray objs=arr.optJSONArray(0);
				for (int a=0;a<objs.length();a++){
					try {
						or.getOutlineDefs().add(new OutlineDef(file,objs.getJSONObject(a)));
					} catch (JSONException je){
						BuildWrapperPlugin.logError(BWText.process_parse_outline_error, je);
					}
				}
			}
		}
		registerOutline(file,or);
		return or;
	}
	
	public void registerOutline(IFile file,OutlineResult or){
		String path=file.getProjectRelativePath().toOSString();
		outlines.put(path,or);
	}
	
	public List<TokenDef> tokenTypes(IFile file){
		//long t0=System.currentTimeMillis();
		String path=file.getProjectRelativePath().toOSString();
		LinkedList<String> command=new LinkedList<String>();
		command.add("tokentypes");
		command.add("--file="+path);
		JSONArray arr=run(command,ARRAY);
		//long t01=System.currentTimeMillis();
		List<TokenDef> cps;
		if (arr!=null){
			if (arr.length()>1){
				JSONArray notes=arr.optJSONArray(1);
				parseNotes(notes);
			}
			JSONArray objs=arr.optJSONArray(0);
			cps=new ArrayList<TokenDef>(objs.length());
			String fn=file.getLocation().toOSString();
			for (int a=0;a<objs.length();a++){
				try {
					cps.add(new TokenDef(fn,objs.getJSONObject(a)));
				} catch (JSONException je){
					BuildWrapperPlugin.logError(BWText.process_parse_outline_error, je);
				}
			}
		} else {
			cps=new ArrayList<TokenDef>();
		}
		//long t1=System.currentTimeMillis();
		//BuildWrapperPlugin.logInfo("tokenTypes:"+(t1-t0)+"ms, parsing:"+(t1-t01)+"ms");
		return cps;
	}
	
	public BuildFlags getBuildFlags(IFile file){
		String path=file.getProjectRelativePath().toOSString();
		LinkedList<String> command=new LinkedList<String>();
		command.add("getbuildflags");
		command.add("--file="+path);
		JSONArray arr=run(command,ARRAY);
		if (arr!=null){
			if (arr.length()>1){
				JSONArray notes=arr.optJSONArray(1);
				//notes.putAll(i.getNotes());
				parseNotes(notes);
			}
			JSONObject obj=arr.optJSONObject(0);
			if (obj!=null){
				return new BuildFlags(obj);
			}
		}
		return null;
	}
	
	public List<Occurrence> getOccurrences(IFile file,String s){
		//BuildFlagInfo i=getBuildFlags(file);
		String path=file.getProjectRelativePath().toOSString();
		LinkedList<String> command=new LinkedList<String>();
		command.add("occurrences");
		command.add("--file="+path);
		command.add("--token="+s);
		//command.add("--buildflags="+escapeFlags(i.getFlags()));
		JSONArray arr=run(command,ARRAY);
		List<Occurrence> cps;
		if (arr!=null){
			if (arr.length()>1){
				JSONArray notes=arr.optJSONArray(1);
				//notes.putAll(i.getNotes());
				parseNotes(notes);
			}
			JSONArray objs=arr.optJSONArray(0);
			cps=new ArrayList<Occurrence>(objs.length());
			String fn=file.getLocation().toOSString();
			for (int a=0;a<objs.length();a++){
				try {
					TokenDef td=new TokenDef(fn,objs.getJSONObject(a));
					cps.add(new Occurrence(td));
				} catch (JSONException je){
					BuildWrapperPlugin.logError(BWText.process_parse_outline_error, je);
				}
			}
		} else {
			cps=new ArrayList<Occurrence>();
		}
		return cps;
	}
	
	public ThingAtPoint getThingAtPoint(IFile file,Location location){
		//BuildFlagInfo i=getBuildFlags(file);
		String path=file.getProjectRelativePath().toOSString();
		LinkedList<String> command=new LinkedList<String>();
		command.add("thingatpoint");
		command.add("--file="+path);
		command.add("--line="+location.getStartLine());
		command.add("--column="+(location.getStartColumn()+1));
		//command.add("--buildflags="+escapeFlags(i.getFlags()));
		JSONArray arr=run(command,ARRAY);
		ThingAtPoint tap=null;
		if (arr!=null){
			if (arr.length()>1){
				JSONArray notes=arr.optJSONArray(1);
			//	notes.putAll(i.getNotes());
				parseNotes(notes);
			}
			JSONObject o=arr.optJSONObject(0);
			if (o!=null){
				try {
					tap=new ThingAtPoint(o);
				} catch (JSONException je){
					BuildWrapperPlugin.logError(BWText.process_parse_thingatpoint_error, je);
				}
			}
		}
		return tap;
	}
	
	private boolean parseNotes(JSONArray notes){
		return parseNotes(notes,null);
	}
	
	private boolean isOK(JSONArray notes){
		if (notes!=null){
			try {
				for (int a=0;a<notes.length();a++){	
					JSONObject o=notes.getJSONObject(a);
					String sk=o.getString("s");
					Kind k="Error".equalsIgnoreCase(sk)?Kind.ERROR:Kind.WARNING;
					if (k.equals(Kind.ERROR)){
						return false;
					}
				}
			} catch (JSONException je){
				BuildWrapperPlugin.logError(BWText.process_parse_note_error, je);
			}
		}
		return true;
	}
	
	private boolean parseNotes(JSONArray notes,Set<IResource> ress){
		boolean buildOK=true;
		if (notes!=null){
			try {
				if (ress==null){
					ress=new HashSet<IResource>();
				}
				for (int a=0;a<notes.length();a++){
					JSONObject o=notes.getJSONObject(a);
					String sk=o.getString("s");
					Kind k="Error".equalsIgnoreCase(sk)?Kind.ERROR:Kind.WARNING;
					if (k.equals(Kind.ERROR)){
						buildOK=false;
					}
					JSONObject ol=o.getJSONObject("l");
					String f=ol.getString("f");
					int line=ol.getInt("l");
					int col=ol.getInt("c");
					Location loc=new Location(f, line, col, line, col);
					Note n=new Note(k,loc,o.getString("t"),"");
					IResource res=project.findMember(f);
					// linker errors may have full path
					if (res==null){
						f=f.replace("\\\\", "\\");
						String pos=project.getLocation().toOSString();
						if (f.startsWith(pos)){
							res=project.findMember(f.substring(pos.length()));
						}
					}
					if (res!=null){
						if (ress.add(res)){
							BuildWrapperPlugin.deleteProblems(res);
						}
						try {
							n.applyAsMarker(res);
						} catch (CoreException ce){
							BuildWrapperPlugin.logError(BWText.process_apply_note_error, ce);
						}
						if (res.getLocation().toOSString().equals(getCabalFile())){
							hasCabalProblems=true;
						}
					}
				}
			} catch (JSONException je){
				BuildWrapperPlugin.logError(BWText.process_parse_note_error, je);
			}
		}
		return buildOK;
	}
	
	private Component parseComponent(JSONObject obj){
		boolean buildable=false;
		try {
			if (obj.has("Library")){
				buildable=obj.getBoolean("Library");
				return new Component(ComponentType.LIBRARY, null, getCabalFile(), buildable);
			} else if (obj.has("Executable")){
				buildable=obj.getBoolean("Executable");
				return new Component(ComponentType.EXECUTABLE, obj.getString("e"), getCabalFile(), buildable);
			} else if (obj.has("TestSuite")){
				buildable=obj.getBoolean("TestSuite");
				return new Component(ComponentType.TESTSUITE, obj.getString("t"), getCabalFile(), buildable);
			}
		} catch (JSONException je){
			BuildWrapperPlugin.logError(BWText.process_parse_component_error, je);
		}
		return null;
	}
	
	private String serializeComponent(Component c) {
	 return ComponentType.LIBRARY.equals(c.getType())?"":c.getName();
	}
	
	private CabalPackage parsePackage(JSONObject obj){
		try {
			String name=obj.getString("n");
			String version=obj.getString("v");
			boolean exposed=obj.getBoolean("e");
			JSONArray comps=obj.getJSONArray("d");
			JSONArray mods=obj.getJSONArray("m");
			
			CabalPackage cp=new CabalPackage();
			cp.setName(name);
			cp.setVersion(version);
			cp.setExposed(exposed);
			for (int a=0;a<mods.length();a++){
				cp.getModules().add(mods.getString(a));
			}
			Component[] deps=new Component[comps.length()];
			for (int a=0;a<comps.length();a++){
				Component c=parseComponent(comps.getJSONObject(a));
				deps[a]=c;
			}
			cp.setComponents(deps);
			return cp;
		} catch (JSONException je){
			BuildWrapperPlugin.logError(BWText.process_parse_package_error, je);
		}
		return null;
	}
	

	
	private <T> T run(LinkedList<String> args,JSONFactory<T> f){
		return run(args,f,true);
	}
	
	private <T> T run(LinkedList<String> args,JSONFactory<T> f,boolean canRerun){
	
		if (bwPath==null){
			if (!showedNoExeError){
				BuildWrapperPlugin.logError(BWText.error_noexe, null);
				showedNoExeError=true;
			}
			return null;
		}
		showedNoExeError=false;
		
		args.addFirst(bwPath);
		args.add("--tempfolder="+DIST_FOLDER);
		args.add("--cabalpath="+cabalPath);
		args.add("--cabalfile="+cabalFile);
		args.add("--cabalflags="+flags);
		for (String s:extraOpts){
			args.add("--cabaloption="+s);
		}
		ProcessBuilder pb=new ProcessBuilder();
		pb.directory(workingDir);
		pb.redirectErrorStream(true);
		pb.command(args);
		if (ow!=null && BuildWrapperPlugin.logAnswers) {
			ow.addMessage(LangUtil.join(args, " "));
		}					
		T obj=null;
		try {
			Process p=pb.start();
			BufferedReader br=new BufferedReader(new InputStreamReader(p.getInputStream(),"UTF8"));
			//long t0=System.currentTimeMillis();
			String l=br.readLine();
			boolean goOn=true;
			boolean needConfigure=false;
			boolean needDelete=false;
			while (goOn && l!=null){
				/*if (outStream!=null){
					outStream.write(l);
					outStream.write(PlatformUtil.NL);
					outStream.flush();
				}*/
				
				if (l.startsWith(prefix)){
					if (ow!=null && BuildWrapperPlugin.logAnswers) {
						ow.addMessage(l);
					}					
					String jsons=l.substring(prefix.length()).trim();
					try {
						obj=f.fromJSON(jsons);
					} catch (JSONException je){
						BuildWrapperPlugin.logError(BWText.process_parse_error, je);
					}
					goOn=false;

				} else {
					String ll=l.toLowerCase(Locale.ENGLISH);
					if (ll.contains(CabalMessages.RERUN_CONFIGURE) || ll.contains(CabalMessages.CANNOT_SATISFY)){
						if (ll.contains(CabalMessages.VERSION)){
							needDelete=true;
						}
						needConfigure=true;
					}
					if (ow!=null) {
						ow.addMessage(l);
					}
				}
				if (goOn){
					l=br.readLine();
				}
			}
			if (needConfigure && canRerun){
				if (needDelete){
					try {
						IFolder fldr=project.getFolder(DIST_FOLDER);
						if (fldr.exists()){
							fldr.delete(IResource.FORCE, new NullProgressMonitor());
						}
					} catch (CoreException ce){
						BuildWrapperPlugin.logError(BWText.process_launch_error, ce);
					}
				}
				configure(new BuildOptions().setTarget(BWTarget.Target));
				return run(new LinkedList<String>(args.subList(1, args.size()-4)),f,false);
			}
			// maybe now the folder exists...
			if (needSetDerivedOnDistDir){
				setDerived();
			}
			//long t1=System.currentTimeMillis();
			//BuildWrapperPlugin.logInfo("read run:"+(t1-t0)+"ms");
		} catch (IOException ioe){
			BuildWrapperPlugin.logError(BWText.process_launch_error, ioe);
		}
		return obj;
	}


//	public String getTempFolder() {
//		return tempFolder;
//	}
//
//
//	public void setTempFolder(String tempFolder) {
//		this.tempFolder = tempFolder;
//	}


	public String getCabalPath() {
		return cabalPath;
	}


	public void setCabalPath(String cabalPath) {
		this.cabalPath = cabalPath;
	}


	public String getCabalFile() {
		return cabalFile;
	}


	public void setCabalFile(String cabalFile) {
		this.cabalFile = cabalFile;
		if (this.cabalFile!=null){
			cabalShortName=new File(this.cabalFile).getName();
		}
	}


	public File getWorkingDir() {
		return workingDir;
	}


	public void setWorkingDir(File workingDir) {
		this.workingDir = workingDir;
	}


	public Writer getOutStream() {
		return outStream;
	}

	
	public void setOutStream(Writer outStream) {
		this.outStream = outStream;
		if (ow!=null){
			ow.setTerminate();
		}
		ow=outStream!=null?new OutputWriter("BWFacade.outputWriter: "+project.getName(),outStream) {
			
			@Override
			public void onThrowable(Throwable se) {
				BuildWrapperPlugin.logError(se.getLocalizedMessage(), se);
				
			}
			
			@Override
			public void onIOError(IOException ex) {
				BuildWrapperPlugin.logError(ex.getLocalizedMessage(), ex);
				
			}
		}:null;
		if (ow!=null){
			ow.start();
		}
	}


	public String getBwPath() {
		return bwPath;
	}


	public void setBwPath(String bwPath) {
		this.bwPath = bwPath;
	}
	
	private static interface JSONFactory<T>{
		T fromJSON(String json) throws JSONException;
	}
	
	private static JSONFactory<JSONArray> ARRAY=new JSONFactory<JSONArray>() {
		public JSONArray fromJSON(String json)throws JSONException {
			return new JSONArray(json);
		}
	};
	
//	private static JSONFactory<JSONObject> OBJECT=new JSONFactory<JSONObject>() {
//		public JSONObject fromJSON(String json)throws JSONException {
//			return new JSONObject(json);
//		}
//	};

	private static JSONFactory<String> STRING=new JSONFactory<String>() {
		public String fromJSON(String json)throws JSONException {
			return json;
		}
	};

	
	public IProject getProject() {
		return project;
	}

	public void setProject(IProject project) {
		this.project = project;
		parseFlags();
		setDerived();
	}
	
	/**
	 * clean: delete the .dist-buildwrapper folder, and synchronize the full content
	 * @param mon the progress monitor
	 * @throws CoreException
	 */
	public void clean(IProgressMonitor mon) throws CoreException{
		if (project!=null){
			IFolder fldr=project.getFolder(DIST_FOLDER);
			if (fldr.exists()){
				fldr.delete(IResource.FORCE, mon);
			}
			deleteCabalProblems();
			cabalFileChanged();
			outlines.clear();
			synchronize(false);
		}
	}
	
	/**
	 * set dist folder as derived so that it will be ignored in searches, etc.
	 */
	private void setDerived(){
		if (project!=null){
			IFolder fldr=project.getFolder(DIST_FOLDER);
			if (fldr.exists()){
				if (!fldr.isDerived()){
					try {
						fldr.setDerived(true,new NullProgressMonitor());
					} catch (CoreException ce){
						// log error and leave flag to false, let's hope it'll be better at next run
						BuildWrapperPlugin.logError(BWText.error_derived, ce);
					}
				}
				needSetDerivedOnDistDir=false;
				
			} else {
				needSetDerivedOnDistDir=true;
			}
		}
	}
	
	/**
	 * Return the flags parameter to cabal configure
	 * @return the flags
	 */
	public String getFlags() {
		return flags;
	}
	
	/**
	 * Return the extra options passed to cabal configure
	 * @return the extraOpts
	 */
	public List<String> getExtraOpts() {
		return extraOpts;
	}
	
	private void parseFlags(){
		try {
			String currentProp=project.getPersistentProperty( BuildWrapperPlugin.USERFLAGS_PROPERTY );
	        JSONObject flagO=new JSONObject();
	        if (currentProp!=null && currentProp.length()>0){
	          flagO=new JSONObject( currentProp );
	        }
	        StringBuilder sb=new StringBuilder();
	        String sep="";
	        for (Iterator<String> it=flagO.keys();it.hasNext();){
	        	String s=it.next();
	        	boolean b=flagO.getBoolean(s);
	        	sb.append(sep);
	        	sep=" ";
	        	if (!b){
	        		sb.append("-");
	        	}
	        	sb.append(s);
	        }
	        flags=sb.toString();
	        
	        extraOpts.clear();
	        currentProp=project.getPersistentProperty( BuildWrapperPlugin.EXTRAOPTS_PROPERTY );
	        JSONArray arrOpts=new JSONArray();
	        if (currentProp!=null && currentProp.length()>0){
	        	arrOpts=new JSONArray( currentProp );
		    }
	        for (int a=0;a<arrOpts.length();a++){
	        	String s=arrOpts.getString(a);
	        	if (s!=null && s.length()>0){
	        		extraOpts.add(s);
	        	}
	        }
	        
		} catch (Exception e){
			BuildWrapperPlugin.logError(BWText.error_gettingFlags, e);
		}
	}

	public boolean isInTempFolder(IResource r){
		if (r.getProject().equals(getProject()) && getProject().getFolder(DIST_FOLDER).getFullPath().isPrefixOf(r.getFullPath())){
			return true;
		}
		return false;
	}
}
