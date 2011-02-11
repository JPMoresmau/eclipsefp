package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.types.Occurrence;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Occurrences command
 * @author JP Moresmau
 *
 */
public class OccurrencesCommand extends ScionCommand {
  /** The current document's contents */
  private String document;
  /**
   * the string to find
   */
  private String query; 
  /** Literate Haskell flag */
  private boolean literate;
  /** The resulting Haskell lexer token */
  private Occurrence[] occurrences;
	
	
	public OccurrencesCommand(String contents, String query, boolean literate) {
		super();
		this.document=contents;
		this.query=query;
		this.literate=literate;
	}
	
	  /**
	   * {@inheritDoc}
	   */
	  @Override
	  protected JSONObject getParams() throws JSONException {
	    JSONObject params = super.getParams();
	    params.put("contents", document);
	    params.put("query", query);
	    params.put("literate", literate);
	    return params;
	  }

	@Override
	public String getMethod() {
		return "occurrences";
	}

	  /**
	   * {@inheritDoc}
	   */
	  @Override
	  protected void doProcessResult() throws JSONException {
	    if (response instanceof JSONObject) {
	      JSONObject o = (JSONObject) response;
    	  JSONArray result = o.optJSONArray("Right");
          if (result != null) {
        	occurrences=new Occurrence[result.length()];
            
            for (int i = 0; i < result.length(); ++i) {
              JSONArray thisTok = (JSONArray) result.get(i);
              int startLine = thisTok.getInt(1);
              int startColumn = thisTok.getInt(2);
              int endColumn = thisTok.getInt(4);
              
              occurrences[i]=new Occurrence(startLine,startColumn+1, endColumn-startColumn);
            }
          }

	    }
	  }
	  
	  public Occurrence[] getOccurrences() {
		return occurrences;
	}
}
