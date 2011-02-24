package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;

import org.eclipse.core.resources.IProject;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * 
 * @author JP Moresmau
 *
 */
public class SetUserFlagsCommand extends ScionCommand {
	private JSONObject flagsO=new JSONObject();
	private String cabalFile;
	
	/**
	 * we pass the JSONObject directly since that's how we store it in the project properties
	 * @param flagsO
	 */
	public SetUserFlagsCommand(IProject project,String cabalFile) {
		this.cabalFile=cabalFile;
		try {
			String currentProp=project.getPersistentProperty( ScionPlugin.USERFLAGS_PROPERTY );
	        if (currentProp!=null && currentProp.length()>0){
	          flagsO=new JSONObject( currentProp );
	        }
		} catch (Exception e){
			ScionPlugin.log(ERROR, ScionText.error_gettingFlags, e);
		}
	}

	@Override
	public String getMethod() {
		return "set-user-flags";
	}
	
	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		/*JSONObject flagsO=new JSONObject();
		for (Map.Entry<String, Boolean> e:flags.entrySet()){
			flagsO.put(e.getKey(), e.getValue());
		}*/
		params.put("user-flags", flagsO);
		params.put("cabal-file", cabalFile);
		return params;
	}

	@Override
	protected void doProcessResult() throws JSONException {
		// nothing interesting to do
	}
}
