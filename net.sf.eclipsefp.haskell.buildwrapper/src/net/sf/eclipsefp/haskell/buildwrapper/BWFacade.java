package net.sf.eclipsefp.haskell.buildwrapper;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.scion.types.BuildOptions;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.Note;
import net.sf.eclipsefp.haskell.scion.types.Note.Kind;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

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
	
	private File workingDir;
	
	private Writer outStream;
	
	private IProject project;
	
	public synchronized void build(BuildOptions buildOptions){
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
	
	public synchronized void synchronize(){
		LinkedList<String> command=new LinkedList<String>();
		command.add("synchronize");
		JSONArray arr=run(command,ARRAY);
		if (arr!=null && arr.length()>1){
			JSONArray notes=arr.optJSONArray(1);
			parseNotes(notes);
		}
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
