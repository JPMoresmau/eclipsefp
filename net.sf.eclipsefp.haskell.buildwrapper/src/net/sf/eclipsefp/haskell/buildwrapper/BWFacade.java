package net.sf.eclipsefp.haskell.buildwrapper;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.util.LinkedList;

import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.scion.types.BuildOptions;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class BWFacade {
	private static final String prefix="build-wrapper-json:";
	
	private String bwPath;
	private String tempFolder=".dist-buildwrapper";
	private String cabalPath;
	private String cabalFile;
	
	private File workingDir;
	
	private Writer outStream;
	
	public synchronized void build(BuildOptions bo){
		LinkedList<String> command=new LinkedList<String>();
		command.add("build");
		command.add("--output="+bo.isOutput());
		JSONArray obj=run(command,ARRAY);
	}
	
	public synchronized void synchronize(){
		LinkedList<String> command=new LinkedList<String>();
		command.add("synchronize");
		JSONArray files=run(command,ARRAY);
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
			while (l!=null){
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
				}
				l=br.readLine();
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
}
