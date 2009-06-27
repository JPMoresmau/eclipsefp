package net.sf.eclipsefp.haskell.scion.types;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.json.JSONException;
import org.json.JSONObject;

public class Note {

	public enum Kind { ERROR, WARNING, INFO, OTHER };
	
	private Kind kind; // error, warning, info or other
	private Location location;
	private String message;
	
	public Note(JSONObject json) throws JSONException {
		String kind = json.getString("kind");
		if (kind.equals("error"))
			this.kind = Kind.ERROR;
		else if (kind.equals("warning"))
			this.kind = Kind.WARNING;
		else if (kind.equals("info"))
			this.kind = Kind.INFO;
		else
			this.kind = Kind.OTHER;
		
		this.location = new Location(json.getJSONObject("location"));
		
		this.message = json.getString("message");
	}
	
	public Kind getKind() {
		return kind;
	}
	
	public Location getLocation() {
		return location;
	}
	
	public String getMessage() {
		return message;
	}
	
	public void applyAsMarker(IResource resource) throws CoreException {
		if (resource != null && resource.isAccessible()) {
			IMarker marker = resource.createMarker(IMarker.PROBLEM);
	        marker.setAttribute(IMarker.USER_EDITABLE, false);
	        int severity;
	        switch (kind) {
	          case ERROR: severity = IMarker.SEVERITY_ERROR; break;
	          case WARNING: severity = IMarker.SEVERITY_WARNING; break;
	          case INFO: severity = IMarker.SEVERITY_INFO; break;
	          default: severity = IMarker.SEVERITY_INFO; break;
	        }
	        marker.setAttribute(IMarker.SEVERITY, severity);
	        marker.setAttribute(IMarker.LINE_NUMBER, location.getStartLine() + 1);
	        marker.setAttribute(IMarker.CHAR_START, location.getStartColumn());
	        marker.setAttribute(IMarker.CHAR_END, location.getEndColumn());
	        marker.setAttribute(IMarker.MESSAGE, message);
		}
	}
	
	@Override
	public String toString() {
		return String.format("%s:%s: %s", kind.toString(), location.toString(), message);
	}
	
}
