package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 *
 * @author JP Moresmau
 *
 */
public class JSONDescriptionHelper {
  public static final String COND_EXECUTABLES="condExecutables"; //$NON-NLS-1$
  public static final String COND_LIBRARY="condLibrary"; //$NON-NLS-1$
  public static final String COND_TREE_CONSTRAINTS="condTreeConstraints"; //$NON-NLS-1$
  public static final String COND_TREE_COMPONENTS="condTreeComponents"; //$NON-NLS-1$
  public static final String JUST="Just"; //$NON-NLS-1$

  public static Set<String> getNames(final Collection<JSONArray> components) {
    Set<String> ret=new HashSet<>();
    for (JSONArray arr:components){
      try {
             ret.add(arr.getString( 0 ));
      } catch (JSONException je){
        HaskellCorePlugin.log( je );
      }
    }
    return ret;
  }

  public static List<JSONArray> getExecutables(final JSONObject description) {
     Object o=description.opt(  COND_EXECUTABLES);
     List<JSONArray> ret=new ArrayList<>();
     if (o instanceof JSONArray){
       JSONArray arr=(JSONArray)o;
       for (int a=0;a<arr.length();a++){
         Object o2=arr.opt( a );
         if (o2 instanceof JSONArray){
           ret.add((JSONArray)o2);
         }
       }
     }
     return ret;
  }

  public static boolean hasLibrary(final JSONObject description) {
    Object o=description.opt(  COND_LIBRARY);
    return o instanceof JSONObject;
  }

  public static List<JSONArray> getDependencies(final JSONArray arr){
    if (arr!=null){
        return getDependencies(arr.optJSONObject( 1 ));
    }
    return Collections.emptyList();
  }

  public static List<JSONArray> getDependencies(final JSONObject description){
    List<JSONArray> ret=new ArrayList<>();
    if (description!=null){

     for (JSONArray arr:getExecutables(description)){
        ret.addAll( getDependencies( arr ) );
      }
      Object o=description.opt(  COND_LIBRARY);
      if (o instanceof JSONObject){
        ret.addAll(getDependencies( ((JSONObject)o).optJSONObject( JUST ) ));
      }
      JSONArray arr2=description.optJSONArray(  COND_TREE_COMPONENTS);
      if (arr2!=null){
        for (int b=0;b<arr2.length();b++){
          ret.addAll(getDependencies( arr2.optJSONArray( b ) ));
        }
      }
      try {
        arr2=description.optJSONArray(COND_TREE_CONSTRAINTS );
        if (arr2!=null){
          for (int b=0;b<arr2.length();b++){
            ret.add(arr2.getJSONArray( b ));
          }
        }
      } catch (JSONException je){
        HaskellCorePlugin.log( je );
      }
    }
    return ret;
  }

}
