package net.sf.eclipsefp.haskell.ui.properties;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.ModuleInclusionType;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

/**
 * <p>Give options to expose and include modules in Cabal sections</p>
  *
  * @author JP Moresmau
 */
public class ModuleInclusionComposite extends Composite {
  private final Set<PackageDescriptionStanza> included=new HashSet<PackageDescriptionStanza>();
  private final Set<PackageDescriptionStanza> exposed=new HashSet<PackageDescriptionStanza>();


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

  public void init(final IResource srcPath, final String module){

    for (Control c:getChildren()){
      c.dispose();
    }
    included.clear();
    exposed.clear();
    try {
      PackageDescription cabal=PackageDescriptionLoader.load( ScionInstance.getCabalFile( srcPath.getProject() ));
      String path=srcPath.getProjectRelativePath().toOSString();
      if (path.length()==0){
        path=".";
      }
      List<PackageDescriptionStanza> l=cabal.getStanzasBySourceDir().get( path );
      if (l!=null && l.size()>0){
        GridLayout gl=new GridLayout(3,false);
        setLayout( gl );

        GridData gdTitle=new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
        gdTitle.horizontalSpan=3;
        Label lTitle=new Label( this, SWT.NONE );
        lTitle.setText( UITexts.module_inclusion_title );
        lTitle.setLayoutData( gdTitle );

        Label lSection=new Label( this, SWT.NONE );
        lSection.setText( UITexts.module_inclusion_field_section );

        Label lInclude=new Label( this, SWT.NONE );
        lInclude.setText( UITexts.module_inclusion_field_include);

        Label lExpose=new Label( this, SWT.NONE );
        lExpose.setText( UITexts.module_inclusion_field_expose );


        Collections.sort( l,new Comparator<PackageDescriptionStanza>() {
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

          if (CabalSyntax.SECTION_EXECUTABLE.equals( pd.getType() ) && ModuleInclusionType.MAIN.equals( mit )){
             bInclude.setEnabled( false );
          }

          final Button bExpose=new Button(this,SWT.CHECK);
          bExpose.setEnabled( CabalSyntax.SECTION_LIBRARY.equals( pd.getType() ) );

          if (bInclude.isEnabled()){
            bInclude.addSelectionListener( new SelectionAdapter() {
              @Override
              public void widgetSelected( final SelectionEvent e ) {
                if(bInclude.getSelection()){
                  included.add( pd );
                  bExpose.setSelection( false );
                } else {
                  included.remove( pd );
                }
                bExpose.setEnabled(CabalSyntax.SECTION_LIBRARY.equals( pd.getType() ) && !bInclude.getSelection() );
              }

            });
            if (ModuleInclusionType.INCLUDED.equals( mit )){
              bInclude.setSelection( true);
              included.add( pd );
              bExpose.setSelection( false );
            }
          }

          if (bExpose.isEnabled()){

            bExpose.addSelectionListener( new SelectionAdapter() {
              @Override
              public void widgetSelected( final SelectionEvent e ) {
                if(bExpose.getSelection()){
                  exposed.add( pd );
                  bInclude.setSelection( false );
                } else {
                  exposed.remove( pd );
                }
                bInclude.setEnabled( !bExpose.getSelection() );
              }
            });
            if (ModuleInclusionType.EXPOSED.equals( mit )){
              bExpose.setSelection( true);
              exposed.add( pd );
              bInclude.setSelection( false );
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
}
