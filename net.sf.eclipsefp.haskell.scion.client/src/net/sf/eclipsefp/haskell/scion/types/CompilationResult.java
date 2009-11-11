package net.sf.eclipsefp.haskell.scion.types;

import net.sf.eclipsefp.haskell.scion.types.Note.Kind;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class CompilationResult {

	private boolean succeeded;
	private double duration;
	private Note[] notes;
	
	public CompilationResult(JSONObject json) throws JSONException {
		this.succeeded = json.getBoolean("succeeded");
		this.duration = json.getDouble("duration");
		JSONArray notes = json.getJSONArray("notes");
		this.notes = new Note[notes.length()];
		for (int i = 0; i < notes.length(); ++i) {
			this.notes[i] = new Note(notes.getJSONObject(i));
		}
	}
	
	public CompilationResult(String fileName,String errorMessage) {
		this.duration=0.0;
		this.succeeded=false;
		this.notes = new Note[1];
		int ix=errorMessage.indexOf(fileName);
		int line=1;
		int column=1;
		if (ix>-1){
			int ix1=errorMessage.indexOf(':',ix+fileName.length());
			if (ix1>-1){
				int ix2=errorMessage.indexOf(':',ix1+1);
				if (ix2>-1){
					int ix3=errorMessage.indexOf(':',ix2+1);
					if (ix3>-1){
						line=Integer.parseInt(errorMessage.substring(ix1+1,ix2));
						column=Integer.parseInt(errorMessage.substring(ix2+1,ix3));
					}
				}
			}
		}
		this.notes[0]=new Note(Kind.ERROR, new Location(fileName, line,column,1,1), errorMessage, null);
	}
	
	public boolean isSucceeded() {
		return succeeded;
	}
	
	public double getDuration() {
		return duration;
	}
	
	public Note[] getNotes() {
		return notes;
	}
	
}
