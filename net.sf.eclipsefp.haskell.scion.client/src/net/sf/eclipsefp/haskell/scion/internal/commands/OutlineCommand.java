package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.types.OutlineDef;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONArray;
import org.json.JSONException;

/**
 * calls outline command
 * @author JP Moresmau
 *
 */
public class OutlineCommand extends ScionCommand {
	private List<OutlineDef> outlineDefs=new ArrayList<OutlineDef>();
	
	public OutlineCommand(IScionCommandRunner runner) {
		super(runner, Job.INTERACTIVE);
	}

	@Override
	protected void doProcessResult(Object result) throws JSONException {
		JSONArray arr = (JSONArray)result;
		for (int a=0;a<arr.length();a++){
			outlineDefs.add(new OutlineDef(arr.getJSONObject(a)));
		}
	}
	
	@Override
	protected String getMethod() {
		return "outline";
	}

	public List<OutlineDef> getOutlineDefs() {
		return outlineDefs;
	}
}
