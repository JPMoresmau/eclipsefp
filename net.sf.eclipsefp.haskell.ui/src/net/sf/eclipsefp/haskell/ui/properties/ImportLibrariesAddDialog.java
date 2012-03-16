package net.sf.eclipsefp.haskell.ui.properties;

import java.util.Collection;
import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.model.WorkbenchViewerComparator;

/**
 * <p>Allows adding a dependency to a library</p>
  *
  * @author JP Moresmau
 */
public class ImportLibrariesAddDialog extends Dialog {
  private final CabalPackage cabalPackage=new CabalPackage( );
  private final Collection<CabalPackage> pkgs;
  private final List<Component> components;

  public ImportLibrariesAddDialog( final Shell parentShell,final Collection<CabalPackage> pkgs, final List<Component> components) {
    super( parentShell );
    this.pkgs=pkgs;
    this.components=components;
  }

  @Override
  protected Control createButtonBar( final Composite parent ) {
    Control c=super.createButtonBar( parent );
    getButton( OK ).setEnabled( false );
    return c;
  }

  @Override
  protected int getShellStyle() {
    return super.getShellStyle() | SWT.RESIZE;
  }

  @Override
  protected void configureShell( final Shell newShell ) {
    super.configureShell( newShell );
    newShell.setText( UITexts.libraries_add_title );

  }

  @Override
  protected Control createDialogArea( final Composite parent ) {
    Composite c=(Composite)super.createDialogArea( parent );


    Label l=new Label(c,SWT.NONE);
    l.setText( UITexts.libraries_add_available_title );

    final TableViewer tv=new TableViewer( c,SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    tv.setContentProvider( new ArrayContentProvider() );
    tv.setComparator( new WorkbenchViewerComparator() );
    tv.setLabelProvider( new LabelProvider(){
      @Override
      public Image getImage(final Object element) {
        if (element instanceof CabalPackage){
         return HaskellUIImages.getImage(((CabalPackage)element).isExposed() ? IImageNames.PACKAGE:IImageNames.HIDDEN_PACKAGE);
        }
        return super.getImage( element );
      }
    }
    );
    tv.setInput( pkgs );



    GridData gd=new GridData(GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL | GridData.FILL_BOTH);
    gd.heightHint=200;
    gd.widthHint=260;
    tv.getTable().setLayoutData( gd );

    Group gCompo=new Group(c,SWT.NONE);
    gCompo.setText( UITexts.libraries_add_component_title );
    GridData gdCompo=new GridData(GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL);
    gCompo.setLayoutData( gdCompo );

    gCompo.setLayout( new GridLayout( 1, true ) );

    Button bAll=new Button(gCompo,SWT.RADIO);
    bAll.setText( UITexts.libraries_add_component_all );
    GridData gdAll=new GridData(GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL);
    bAll.setLayoutData( gdAll );

    Button bSelected=new Button(gCompo,SWT.RADIO);
    bSelected.setText( UITexts.libraries_add_component_selected );
    GridData gdSelected=new GridData(GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL);
    bSelected.setLayoutData( gdSelected );

    final ListViewer lvLCompo=new ListViewer( gCompo,SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );
    GridData gdLCompo=new GridData(GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL);
    gdLCompo.horizontalIndent=20;
    lvLCompo.getList().setLayoutData( gdLCompo );
    lvLCompo.setComparator( new WorkbenchViewerComparator() );
    lvLCompo.setLabelProvider( new LabelProvider() );
    lvLCompo.setContentProvider( new ArrayContentProvider() );
    lvLCompo.setInput( components );

    bAll.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
       lvLCompo.getList().setEnabled( false );
       cabalPackage.setComponents( components.toArray( new Component[components.size()] ) );
      }
    });

    bSelected.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
       lvLCompo.getList().setEnabled( true );
      }
    });
    bAll.setSelection( true );
    bAll.notifyListeners( SWT.Selection, new Event() );

    lvLCompo.addSelectionChangedListener( new ISelectionChangedListener() {

      @Override
      public void selectionChanged( final SelectionChangedEvent event ) {
        Object[] objs=((IStructuredSelection)event.getSelection()).toArray();
        Component[] cps=new Component[objs.length];
        for (int a=0;a<objs.length;a++){
          cps[a]=(Component)objs[a];
        }
        cabalPackage.setComponents(cps);

      }
    });

    Group gVersion=new Group(c,SWT.NONE);
    gVersion.setText( UITexts.libraries_add_version_title );
    GridData gdVersion=new GridData(GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL);
    gVersion.setLayoutData( gdVersion );
    gVersion.setLayout( new GridLayout( 2, true ) );

    Button bNone=new Button(gVersion,SWT.RADIO);
    bNone.setText( UITexts.none);
    GridData gdNone=new GridData(GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL);
    gdNone.horizontalSpan=2;
    bNone.setLayoutData( gdNone );

    Button bSpecific=new Button(gVersion,SWT.RADIO);
    bSpecific.setText( UITexts.libraries_add_version_specific );
    GridData gdSpecific=new GridData(GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL);
    bSpecific.setLayoutData( gdSpecific );

    final Text tSpecific=new Text(gVersion,SWT.BORDER);
    GridData gdTSpecific=new GridData(GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL);
    tSpecific.setLayoutData( gdTSpecific );

    bNone.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        tSpecific.setEnabled( false );
        cabalPackage.setVersion( "" );
      }
    });

    bSpecific.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        tSpecific.setEnabled( true );
        if (tSpecific.getText().length()>0){
          cabalPackage.setVersion( tSpecific.getText()  );
        } else {
          IStructuredSelection sel=(IStructuredSelection)tv.getSelection();
          if (!sel.isEmpty()){
            CabalPackage cp=(CabalPackage)sel.getFirstElement();
            tSpecific.setText( "== "+ cp.getVersion() );
          }
        }
      }
    });

    tSpecific.addModifyListener( new ModifyListener() {

      @Override
      public void modifyText( final ModifyEvent e ) {
        cabalPackage.setVersion( tSpecific.getText()  );
      }
    });

    bNone.setSelection( true );
    bNone.notifyListeners( SWT.Selection, new Event() );

    tv.addSelectionChangedListener( new ISelectionChangedListener() {

      @Override
      public void selectionChanged( final SelectionChangedEvent event ) {
        CabalPackage cp=(CabalPackage)((IStructuredSelection)event.getSelection()).getFirstElement();
        cabalPackage.setName( cp.getName() );
        cabalPackage.setExposed( cp.isExposed() );
        if (tSpecific.isEnabled()){
          tSpecific.setText("== "+ cp.getVersion() );
          cabalPackage.setVersion( tSpecific.getText()  );
        } else {
          cabalPackage.setVersion("");
        }
        getButton( OK ).setEnabled( true );
      }
    });

    return c;
  }


  public CabalPackage getCabalPackage() {
    return cabalPackage;
  }
}
