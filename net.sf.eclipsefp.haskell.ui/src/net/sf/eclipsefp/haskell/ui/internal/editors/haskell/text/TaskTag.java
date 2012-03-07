/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import static net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames.EDITOR_TASK_TAGS;
import static net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames.EDITOR_TASK_TAGS_HIGH;
import static net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames.EDITOR_TASK_TAGS_LOW;
import static net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames.EDITOR_TASK_TAGS_NORMAL;
import java.util.HashSet;
import java.util.Set;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.preference.IPreferenceStore;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * the structure representing a Task Tag in the preferences
 * @author JP Moresmau
 *
 */
public class TaskTag implements Cloneable{
    private String name;
    private String priority;
    private int markerPriority;

    public static Set<TaskTag> getTasksTags(final IPreferenceStore prefStore){
      String s=prefStore.getString( EDITOR_TASK_TAGS );
      Set<TaskTag> tags=new HashSet<TaskTag>();
      if (s!=null){

        try {
          JSONObject obj=new JSONObject(s);
          build(tags,obj,EDITOR_TASK_TAGS_HIGH,UITexts.tasks_pref_priority_high);
          build(tags,obj,EDITOR_TASK_TAGS_NORMAL,UITexts.tasks_pref_priority_normal);
          build(tags,obj,EDITOR_TASK_TAGS_LOW,UITexts.tasks_pref_priority_low);

        } catch (JSONException je){
          HaskellUIPlugin.log( je );
        }
      }
      return tags;
    }

    private static void build(final Set<TaskTag> tags,final JSONObject obj,final String key,final String priority) throws JSONException{
      JSONArray arr=obj.getJSONArray( key );
      for (int a=0;a<arr.length();a++){
        tags.add( new TaskTag( arr.getString( a ), priority ) );
      }
    }

    public TaskTag( final String name, final String priority ) {
      super();
      this.name = name;
      setPriority( priority );
    }

    public int getMarkerPriority(){
      return markerPriority;
    }

    /* (non-Javadoc)
    * @see java.lang.Object#clone()
    */
    @Override
    public TaskTag clone(){
      return new TaskTag(name, priority);
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + ( ( name == null ) ? 0 : name.hashCode() );
      return result;
    }

    @Override
    public boolean equals( final Object obj ) {
      if( this == obj ) {
        return true;
      }
      if( obj == null ) {
        return false;
      }
      if( getClass() != obj.getClass() ) {
        return false;
      }
      TaskTag other = ( TaskTag )obj;
      if( name == null ) {
        if( other.name != null ) {
          return false;
        }
      } else if( !name.equals( other.name ) ) {
        return false;
      }
      return true;
    }

    /* (non-Javadoc)
    * @see java.lang.Object#toString()
    */
    @Override
    public String toString() {
      return name;
    }

    public String getName() {
      return name;
    }


    public void setName( final String name ) {
      this.name = name;
    }


    public String getPriority() {
      return priority;
    }


    public void setPriority( final String priority ) {
      this.priority = priority;
      markerPriority=getPriority().equals( UITexts.tasks_pref_priority_high )?IMarker.PRIORITY_HIGH
          :getPriority().equals( UITexts.tasks_pref_priority_normal )?IMarker.PRIORITY_NORMAL
              :IMarker.PRIORITY_LOW;
    }


}