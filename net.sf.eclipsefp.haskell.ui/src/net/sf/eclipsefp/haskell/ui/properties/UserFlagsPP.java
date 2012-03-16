package net.sf.eclipsefp.haskell.ui.properties;

import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.JobFacade;
import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PropertyPage;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * <p>Let the user set the Cabal flags to non default values</p>
  *
  * @author JP Moresmau
 */
public class UserFlagsPP extends PropertyPage implements
    IWorkbenchPreferencePage {
  private final Map<String,Boolean> current=new HashMap<String, Boolean>();
  private final Map<String,Boolean> defaults=new HashMap<String, Boolean>();
  private final Map<String,Button> buttons=new HashMap<String, Button>();

  public UserFlagsPP() {
    super();
    setDescription( UITexts.properties_userflags_description );

  }

  @Override
  protected Control createContents( final Composite parent ) {
    Composite container = new Composite( parent, SWT.NULL );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    container.setLayout( layout );

    IProject project=( IProject )getElement();
    IFile f=BuildWrapperPlugin.getCabalFile( project );
    if (f!=null && f.exists()){
      try {
        String currentProp=project.getPersistentProperty( BuildWrapperPlugin.USERFLAGS_PROPERTY );
        JSONObject flagO=new JSONObject();
        if (currentProp!=null && currentProp.length()>0){
          flagO=new JSONObject( currentProp );
        }

        PackageDescription pd=PackageDescriptionLoader.load( f );

        for (final PackageDescriptionStanza pds:pd.getStanzas()){
          if (CabalSyntax.SECTION_FLAG.equals( pds.getType())){
            Label l=new Label(container,SWT.NONE);
            l.setText( pds.getName() );
            String desc=pds.getProperties().get( CabalSyntax.FIELD_DESCRIPTION );
            if (desc==null || desc.trim().length()==0){
              desc=pds.getName();
            }
            l.setToolTipText( desc );

            final Button b=new Button(container,SWT.CHECK);
            b.setToolTipText( desc );
            // default for flags is true
            boolean def=true;
            String defS=pds.getProperties().get( CabalSyntax.FIELD_DEFAULT );
            if (defS!=null){
              def=Boolean.parseBoolean( defS );
            }
            defaults.put( pds.getName(), def );
            def=flagO.optBoolean( pds.getName(), def );
            b.setSelection(def);
            current.put( pds.getName(), def);

            buttons.put( pds.getName(), b );
            b.addSelectionListener( new SelectionAdapter() {
              @Override
              public void widgetSelected( final SelectionEvent e ) {
                current.put( pds.getName(), b.getSelection() );
              }
            });
          }
        }
        if (defaults.size()==0){
          Label l=new Label(container,SWT.NONE);
          l.setText( UITexts.properties_userflags_none );
        }
      } catch (CoreException ce){
        HaskellUIPlugin.log( ce );
        Label l=new Label(container,SWT.MULTI);
        l.setText( ce.getLocalizedMessage() );
      }  catch (JSONException ce){
        HaskellUIPlugin.log( ce );
        Label l=new Label(container,SWT.MULTI);
        l.setText( ce.getLocalizedMessage() );
      }
    }

    Dialog.applyDialogFont( parent );
    return container;
  }

  @Override
  public void init( final IWorkbench arg0 ) {
    // NOOP
  }

  @Override
  protected void performDefaults() {
    current.clear();
    for (Map.Entry<String,Button> e:buttons.entrySet()){
      Boolean def=defaults.get( e.getKey() );
      if (def==null){
        def=true;
      }
      e.getValue().setSelection( def );
      // will also populate current
      e.getValue().notifyListeners( SWT.Selection, new Event() );
    }
    super.performDefaults();
  }

  @Override
  public boolean performOk() {
    // no flags, nothing changes
    if (current.size()>0){
      JSONObject flagsO=new JSONObject();
      try {
        for (Map.Entry<String, Boolean> e:current.entrySet()){
          flagsO.put(e.getKey(), e.getValue());
        }
      } catch (JSONException ce){
        HaskellUIPlugin.log( ce );
      }
      IProject project=( IProject )getElement();
      try {
        project.setPersistentProperty( BuildWrapperPlugin.USERFLAGS_PROPERTY, flagsO.toString() );

        /*ScionInstance instance=ScionPlugin.getScionInstance( project );
        if (instance !=null){
          BuildOptions buildOptions=new BuildOptions().setOutput(true).setRecompile(true).setConfigure( true );
          instance.buildProject( buildOptions );
        }*/
        JobFacade f=BuildWrapperPlugin.getJobFacade( project );
        if (f!=null){
          f.build( new BuildOptions().setOutput(true).setRecompile(true).setConfigure( true ) );
        }

      } catch (CoreException ce){
        HaskellUIPlugin.log( ce );
      }
    }
    return super.performOk();
  }

}
