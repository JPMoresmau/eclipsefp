package net.sf.eclipsefp.haskell.scion.types;

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
