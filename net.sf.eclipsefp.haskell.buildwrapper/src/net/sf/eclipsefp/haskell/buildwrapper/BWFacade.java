package net.sf.eclipsefp.haskell.buildwrapper;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.Note;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component.ComponentType;
import net.sf.eclipsefp.haskell.buildwrapper.types.Note.Kind;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class BWFacade implements IBWFacade {
	private static final String prefix="build-wrapper-json:";
	
	private String bwPath;
	private String tempFolder=".dist-buildwrapper";
	private String cabalPath;
	private String cabalFile;
	private String cabalShortName;
	
	private File workingDir;
	
	private Writer outStream;
	
	private IProject project;
	
	
	private List<Component> components;
	private Map<String, CabalPackage[]> packageDB;
	
	public void build(BuildOptions buildOptions){
		BuildWrapperPlugin.deleteProblems(getProject());
		LinkedList<String> command=new LinkedList<String>();
		command.add("build");
		command.add("--output="+buildOptions.isOutput());
		JSONArray arr=run(command,ARRAY);
		if (arr!=null && arr.length()>1){
			JSONArray notes=arr.optJSONArray(1);
			parseNotes(notes);
		}
		
	}
	
	public void synchronize(){
		LinkedList<String> command=new LinkedList<String>();
		command.add("synchronize");
		JSONArray arr=run(command,ARRAY);
		if (arr!=null){
			if(arr.length()>1){
		
				JSONArray notes=arr.optJSONArray(1);
				parseNotes(notes);
			}
			JSONArray paths=arr.optJSONArray(0);
			if (paths!=null){
				for (int a=0;a<paths.length();a++){
					try {
						String p=paths.getString(a);
						if (p!=null && p.equals(cabalShortName)){
							components=null;
							packageDB=null;
						}
					} catch (JSONException je){
						BuildWrapperPlugin.logError(BWText.process_parse_component_error, je);
					}
				}
			}
		}
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
	
	public List<OutlineDef> outline(IFile file){
		String path=file.getProjectRelativePath().toOSString();
		LinkedList<String> command=new LinkedList<String>();
		command.add("outline");
		command.add("--file="+path);
		JSONArray arr=run(command,ARRAY);
		List<OutlineDef> cps=new ArrayList<OutlineDef>();
		if (arr!=null){
			if (arr.length()>1){
				JSONArray notes=arr.optJSONArray(1);
				parseNotes(notes);
			}
			JSONArray objs=arr.optJSONArray(0);
			for (int a=0;a<objs.length();a++){
				try {
					cps.add(new OutlineDef(file,objs.getJSONObject(a)));
				} catch (JSONException je){
					BuildWrapperPlugin.logError(BWText.process_parse_outline_error, je);
				}
			}
		}
		return cps;
	}
	
	private void parseNotes(JSONArray notes){
		if (notes!=null){
			try {
				Set<IResource> ress=new HashSet<IResource>();
				for (int a=0;a<notes.length();a++){
					JSONObject o=notes.getJSONObject(a);
					String sk=o.getString("s");
					Kind k="Error".equalsIgnoreCase(sk)?Kind.ERROR:Kind.WARNING;
					JSONObject ol=o.getJSONObject("l");
					String f=ol.getString("f");
					int line=ol.getInt("l");
					int col=ol.getInt("c");
					Location loc=new Location(f, line, col, line, col);
					Note n=new Note(k,loc,o.getString("t"),"");
					final IResource res=project.findMember(f);
					if (res!=null){
						if (ress.add(res)){
							BuildWrapperPlugin.deleteProblems(res);
						}
						try {
							n.applyAsMarker(res);
						} catch (CoreException ce){
							BuildWrapperPlugin.logError(BWText.process_apply_note_error, ce);
						}
					}
				}
			} catch (JSONException je){
				BuildWrapperPlugin.logError(BWText.process_parse_note_error, je);
			}
		}
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
		args.addFirst(bwPath);
		args.add("--tempfolder="+tempFolder);
		args.add("--cabalpath="+cabalPath);
		args.add("--cabalfile="+cabalFile);
		ProcessBuilder pb=new ProcessBuilder();
		pb.directory(workingDir);
		pb.redirectErrorStream(true);
		pb.command(args);
		T obj=null;
		try {
			Process p=pb.start();
			BufferedReader br=new BufferedReader(new InputStreamReader(p.getInputStream(),"UTF8"));
			String l=br.readLine();
			boolean goOn=true;
			while (goOn && l!=null){
				outStream.write(l);
				outStream.write(PlatformUtil.NL);
				outStream.flush();
				if (l.startsWith(prefix)){
					String jsons=l.substring(prefix.length()).trim();
					try {
						obj=f.fromJSON(jsons);
					} catch (JSONException je){
						BuildWrapperPlugin.logError(BWText.process_parse_error, je);
					}
					goOn=false;
				}
				if (goOn){
					l=br.readLine();
				}
			}
		} catch (IOException ioe){
			BuildWrapperPlugin.logError(BWText.process_launch_error, ioe);
		}
		return obj;
	}


	public String getTempFolder() {
		return tempFolder;
	}


	public void setTempFolder(String tempFolder) {
		this.tempFolder = tempFolder;
	}


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
	
	private static JSONFactory<JSONObject> OBJECT=new JSONFactory<JSONObject>() {
		public JSONObject fromJSON(String json)throws JSONException {
			return new JSONObject(json);
		}
	};

	public IProject getProject() {
		return project;
	}

	public void setProject(IProject project) {
		this.project = project;
	}
}
