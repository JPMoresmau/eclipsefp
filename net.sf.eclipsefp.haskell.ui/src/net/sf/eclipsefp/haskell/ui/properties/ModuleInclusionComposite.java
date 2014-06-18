package net.sf.eclipsefp.haskell.ui.properties;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.ModuleInclusionType;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;

/**
 * <p>Give options to expose and include modules in Cabal sections</p>
  *
  * @author JP Moresmau
 */
public class ModuleInclusionComposite extends Composite {
  private final Set<PackageDescriptionStanza> included=new HashSet<>();
  private final Set<PackageDescriptionStanza> exposed=new HashSet<>();
  private PackageDescriptionStanza editorStanza=null;

  private final Collection<Button> editorStanzaButtons=new ArrayList<>();

  private final SelectionListener editorButtonL=new SelectionAdapter() {
    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    @Override
    public void widgetSelected( final SelectionEvent e ) {
      if (((Button)e.widget).getSelection()){
        for (Button b:editorStanzaButtons){
          if (b!=e.widget){
            b.setSelection( false );
          }
        }
      }
    }
  };

  /**
   * have we init at all
   */
  private boolean init=false;

  public ModuleInclusionComposite( final Composite parent, final int style ) {
    super( parent, style );
  }

  public void initNoSourceFolder(){
    GridLayout gl=new GridLayout(1,false);
    setLayout( gl );
    Label w=new Label(this,SWT.NONE);
    w.setText( UITexts.module_inclusion_nosourcefolder );
    layout( true, true );
  }


  public boolean isInit() {
    return init;
  }

  /**
   * init the composite with information from the cabal file, or if it's a new module, default inclusion
   * @param srcPath the path to the source folder
   * @param module the module name
   * @param isNew is the module being created?
   */
  public void init(final IFile file,final IResource srcPath, final String module,final boolean isNew){
    init=true;
    for (Control c:getChildren()){
      c.dispose();
    }
    included.clear();
    exposed.clear();
    editorStanzaButtons.clear();
    try {
      String editor=null;
      if (file!=null){
        editor=file.getPersistentProperty( BuildWrapperPlugin.EDITORSTANZA_PROPERTY );
      }

      PackageDescription cabal=PackageDescriptionLoader.load( BuildWrapperPlugin.getCabalFile( srcPath.getProject() ));
      String path=srcPath.getProjectRelativePath().toOSString();
      if (path.length()==0){
        path=".";
      }
      List<PackageDescriptionStanza> l=cabal.getStanzasBySourceDir().get( path );
      if (l!=null && l.size()>0){
        /** defaults to the only one **/
        if (l.size()==1 && editor==null ){
          editor=l.get( 0 ).getName();
          if (editor==null){ // library
            editor="";
          }
        }
        GridLayout gl=new GridLayout(4,false);
        setLayout( gl );

        GridData gdTitle=new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
        gdTitle.horizontalSpan=4;
        Label lTitle=new Label( this, SWT.NONE );
        lTitle.setText( UITexts.module_inclusion_title );
        lTitle.setLayoutData( gdTitle );

        Label lSection=new Label( this, SWT.NONE );
        lSection.setText( UITexts.module_inclusion_field_section );

        Label lInclude=new Label( this, SWT.NONE );
        lInclude.setText( UITexts.module_inclusion_field_include);

        Label lExpose=new Label( this, SWT.NONE );
        lExpose.setText( UITexts.module_inclusion_field_expose );

        Label lEditor=new Label( this, SWT.NONE );
        lEditor.setText( UITexts.module_inclusion_field_editor);

        Collections.sort( l,new Comparator<PackageDescriptionStanza>() {
          @Override
          public int compare( final PackageDescriptionStanza o1,
              final PackageDescriptionStanza o2 ) {
             return o1.toTypeName().compareToIgnoreCase( o2.toTypeName() );
          }
        });

        for (final PackageDescriptionStanza pd:l){
          Label lName=new Label(this,SWT.NONE);
          lName.setText( pd.toTypeName() );

          final Button bInclude=new Button(this,SWT.CHECK);

          ModuleInclusionType mit=pd.getModuleInclusionType( module );

          if ((CabalSyntax.SECTION_EXECUTABLE.equals( pd.getType()) || CabalSyntax.SECTION_TESTSUITE.equals( pd.getType())) && ModuleInclusionType.MAIN.equals( mit )){
             bInclude.setEnabled( false );
          }

          final Button bExpose=new Button(this,SWT.CHECK);
          bExpose.setEnabled( CabalSyntax.SECTION_LIBRARY.equals( pd.getType() ) );

          final Button bEditor=new Button(this,SWT.RADIO);
          bEditor.setEnabled( true );
          editorStanzaButtons.add( bEditor );
          bEditor.addSelectionListener( editorButtonL );
          bEditor.addSelectionListener( new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
              if (bEditor.getSelection()){
                editorStanza=pd;
              } else {
                editorStanza=null;
              }
            }
          } );
          if (bInclude.isEnabled()){
            bInclude.addSelectionListener( new SelectionAdapter() {
              @Override
              public void widgetSelected( final SelectionEvent e ) {
                if(bInclude.getSelection()){
                  included.add( pd );
                  exposed.remove( pd );
                  bExpose.setSelection( false );
                } else {
                  included.remove( pd );
                }
              // bExpose.setEnabled(CabalSyntax.SECTION_LIBRARY.equals( pd.getType() ) && !bInclude.getSelection() );
              }

            });
            if (ModuleInclusionType.INCLUDED.equals( mit ) || isNew){
              bInclude.setSelection( true);
              included.add( pd );
              bExpose.setSelection( false );
              if (isNew){
                bInclude.notifyListeners( SWT.Selection, new Event() );
              }
            }
          }

          if (bExpose.isEnabled()){

            bExpose.addSelectionListener( new SelectionAdapter() {
              @Override
              public void widgetSelected( final SelectionEvent e ) {
                if(bExpose.getSelection()){
                  exposed.add( pd );
                  included.remove( pd );
                  bInclude.setSelection( false );
                } else {
                  exposed.remove( pd );
                }
              //  bInclude.setEnabled( !bExpose.getSelection() );
              }
            });
            if (ModuleInclusionType.EXPOSED.equals( mit ) || (isNew && !bInclude.isEnabled())){
              bExpose.setSelection( true);
              exposed.add( pd );
              bInclude.setSelection( false );
              if (isNew){
                bInclude.notifyListeners( SWT.Selection, new Event() );
              }
            }

          }
          if (editor!=null){
            if (editor.equals( pd.getName() ) || (editor.equals( "" ) && pd.getName()==null)){
              bEditor.setSelection( true );
              editorStanza=pd;
            }
          }
        }
      }

      layout( true, true );
    } catch (CoreException ce){
      HaskellUIPlugin.log( ce );
    }

  }


  public Set<PackageDescriptionStanza> getExposed() {
    return exposed;
  }


  public Set<PackageDescriptionStanza> getIncluded() {
    return included;
  }


  /**
   * @return the editorStanza
   */
  public PackageDescriptionStanza getEditorStanza() {
    return editorStanza;
  }

  public void populateInfo(final ModuleCreationInfo info){
    info.setExposed( getExposed() );
    info.setIncluded( getIncluded() );
    info.setEditorStanza( getEditorStanza() );
  }
}
