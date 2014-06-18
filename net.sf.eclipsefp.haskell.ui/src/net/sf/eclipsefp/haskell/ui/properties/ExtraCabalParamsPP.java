/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.properties;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.common.ui.dialog.DirectoryListComposite;
import net.sf.eclipsefp.common.ui.dialog.StringListComposite;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.JobFacade;
import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PropertyPage;
import org.json.JSONArray;
import org.json.JSONException;


/**
 * Preference page for project: extra cabal parameters
 * This is to enter parameters that will be passed to cabal configure, but do not reside in the cabal file
 * There are specific components for extra-lib-dirs and extra-include-dirs
 * @author JP Moresmau
 *
 */
public class ExtraCabalParamsPP extends PropertyPage implements
    IWorkbenchPreferencePage {
  private DirectoryListComposite extraLibDirs;
  private DirectoryListComposite extraIncDirs;
  private StringListComposite extraFrees;

  /**
   *
   */
  public ExtraCabalParamsPP() {
    setDescription( UITexts.properties_extra_description );
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
   */
  @Override
  public void init( final IWorkbench arg0 ) {
    // NOOP

  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent ) {
    Composite container = new Composite( parent, SWT.NULL );
    GridLayout layout = new GridLayout(1,true);

    container.setLayout( layout );
    container.setLayoutData( new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL) );

    Label lExtraLibDirs=new Label( container, SWT.NONE );
    lExtraLibDirs.setText( UITexts.properties_extra_libdirs );

    extraLibDirs=new DirectoryListComposite( container, SWT.NONE );
    extraLibDirs.setLayoutData( new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL) );

    Label lExtraIncDirs=new Label( container, SWT.NONE );
    lExtraIncDirs.setText( UITexts.properties_extra_incdirs );

    extraIncDirs=new DirectoryListComposite( container, SWT.NONE );
    extraIncDirs.setLayoutData( new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL) );
    Label lExtraFree=new Label( container, SWT.NONE );
    lExtraFree.setText( UITexts.properties_extra_free );

    extraFrees=new StringListComposite( container, SWT.NONE );
    extraFrees.setAddMessage( UITexts.properties_extra_free_message );
    extraFrees.setAddTitle( UITexts.properties_extra_free_title );
    extraFrees.setLayoutData( new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL) );
    List<String> extraLibs=new ArrayList<>();
    List<String> extraIncs=new ArrayList<>();
    List<String> extraFrees=new ArrayList<>();

    IProject project=( IProject )getElement();
    try {
      String currentProp=project.getPersistentProperty( BuildWrapperPlugin.EXTRAOPTS_PROPERTY );
      JSONArray arr=new JSONArray();
      if (currentProp!=null && currentProp.length()>0){
        arr=new JSONArray(currentProp);
      }
      for (int a=0;a<arr.length();a++){
        String s=arr.optString( a );
        if (s!=null && s.length()>0){
          if (s.startsWith( "--"+CabalSyntax.FIELD_EXTRA_LIB_DIRS.getCabalName())){
            int ix=s.indexOf( '=', CabalSyntax.FIELD_EXTRA_LIB_DIRS.getCabalName().length()+2);
            if (ix>-1){
              String s1=s.substring( ix+1 ).trim();
              extraLibs.add(s1);
            }
          } else if (s.startsWith( "--extra-include-dirs")){
            int ix=s.indexOf( '=', "--extra-include-dirs".length());
            if (ix>-1){
              String s1=s.substring( ix+1 ).trim();
              extraIncs.add(s1);
            }
          } else {
            extraFrees.add(s);
          }
        }
      }

      this.extraLibDirs.setPaths( extraLibs );
      this.extraIncDirs.setPaths( extraIncs );
      this.extraFrees.setPaths(extraFrees);
    } catch (CoreException ce){
      HaskellUIPlugin.log( ce );
      Label l=new Label(container,SWT.MULTI);
      l.setText( ce.getLocalizedMessage() );
    }  catch (JSONException ce){
      HaskellUIPlugin.log( ce );
      Label l=new Label(container,SWT.MULTI);
      l.setText( ce.getLocalizedMessage() );
    }
    Dialog.applyDialogFont( parent );
    return container;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
   */
  @Override
  protected void performDefaults() {
    extraLibDirs.setPaths( Collections.<String>emptyList() );
    extraIncDirs.setPaths( Collections.<String>emptyList() );
    extraFrees.setPaths( Collections.<String>emptyList() );

    super.performDefaults();
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.preference.PreferencePage#performOk()
   */
  @Override
  public boolean performOk() {
    JSONArray arr=new JSONArray();
    for (String s:extraLibDirs.getPaths()){
      arr.put( "--"+CabalSyntax.FIELD_EXTRA_LIB_DIRS.getCabalName()+"="+s );
    }
    for (String s:extraIncDirs.getPaths()){
      arr.put(  "--extra-include-dirs="+s );
    }
    for (String s:extraFrees.getPaths()){
      arr.put(s);
    }
    IProject project=( IProject )getElement();
    try {
      project.setPersistentProperty( BuildWrapperPlugin.EXTRAOPTS_PROPERTY, arr.toString() );

      JobFacade f=BuildWrapperPlugin.getJobFacade( project );
      if (f!=null){
        f.build( new BuildOptions().setOutput(true).setRecompile(true).setConfigure( true ) );
      }

    } catch (CoreException ce){
      HaskellUIPlugin.log( ce );
    }
    return super.performOk();
  }
}
