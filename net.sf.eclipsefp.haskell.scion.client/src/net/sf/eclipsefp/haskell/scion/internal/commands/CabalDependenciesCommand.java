package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.types.CabalPackage;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * 
 * @author JP Moresmau
 *
 */
public class CabalDependenciesCommand extends ScionCommand {
	private String fileName;
	private Map<String,CabalPackage[]> packagesByDB=new HashMap<String,CabalPackage[]>();
	
	public CabalDependenciesCommand(IScionCommandRunner runner, IScionServer server, String fileName) {
		super(runner, server, Job.BUILD);
		this.fileName=fileName;
	}
	
	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("cabal-file", fileName);
		return params;
	}
	
	@Override
	protected void doProcessResult(Object result) throws JSONException {
		packagesByDB.clear();
		JSONArray arr=(JSONArray)result;
		for (int a=0;a<arr.length();a++){
			JSONObject arr2=arr.getJSONObject(a);
			Iterator<String> it=arr2.keys();
			while (it.hasNext()){
				String dbName=it.next();
				//String dbName=arr2.getString(0);
				JSONArray arr3=arr2.getJSONArray(dbName);
				CabalPackage[] pkgs=new CabalPackage[arr3.length()];
				for (int b=0;b<arr3.length();b++){
					pkgs[b]=new CabalPackage(arr3.getJSONObject(b));
				}
				packagesByDB.put(dbName, pkgs);
			}
		}
	}
	
	public Map<String, CabalPackage[]> getPackagesByDB() {
		return packagesByDB;
	}

	@Override
	protected String getMethod() {
		return "cabal-dependencies";
	}

}
