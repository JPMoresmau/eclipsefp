/**
 *  Copyright (c) 2011 by Alejandro Serrano
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.Arrays;
import java.util.Vector;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component.ComponentType;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormPage;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.IFormPart;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;

/**
 * Page for handling the executables defined in a Cabal file.
 * @author Alejandro Serrano
 *
 */
public class ExecutablesPage extends CabalFormPage implements SelectionListener {

  private List execsList;

  private boolean ignoreModify = false;
  private CabalFormEditor formEditor;

  DependenciesSection depsSection;
  SourceDirsSection sourceDirsSection;
  ModulesExecutableSection modulesSection;

  String nextSelected = null;

  public ExecutablesPage( final FormEditor editor, final IProject project ) {
    super( editor, ExecutablesPage.class.getName(), UITexts.cabalEditor_executables, project );
  }

  public PackageDescriptionStanza createNewStanza(final PackageDescription desc, final String name) {
    PackageDescriptionStanza stanza = desc.addStanza(
        CabalSyntax.SECTION_EXECUTABLE, name );
    stanza.setIndent( 2 );
    stanza.update( CabalSyntax.FIELD_BUILD_DEPENDS, "base >= 4" );
    stanza.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, "src" );
    stanza.update( CabalSyntax.FIELD_GHC_OPTIONS, "-Wall -rtsopts" );
    return stanza;
  }

  public java.util.List<PackageDescriptionStanza> getStanzas(
      final PackageDescription description ) {
    return description.getExecutableStanzas();
  }

  public ComponentType getComponentType() {
    return ComponentType.EXECUTABLE;
  }

  @Override
  protected void createFormContent( final IManagedForm managedForm ) {
    ScrolledForm form = managedForm.getForm();
    FormToolkit toolkit = managedForm.getToolkit();
    toolkit.decorateFormHeading( form.getForm() );
    form.updateToolBar();
    form.setText( UITexts.cabalEditor_executables );

    form.getBody().setLayout( createUnequalGridLayout( 2, 6, 12 ) );

    Composite left = toolkit.createComposite( form.getBody() );
    left.setLayout( new GridLayout( 1, true ) );
    left.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    ToolBar tbar = new ToolBar( left, SWT.NULL );
    toolkit.adapt( tbar, true, true );
    GridData tbarGD = new GridData( GridData.FILL_HORIZONTAL );
    tbarGD.grabExcessHorizontalSpace = true;
    tbar.setLayoutData( tbarGD );
    ToolBarManager manager = new ToolBarManager( tbar );

    execsList = new List( left, SWT.SINGLE | SWT.BORDER );
    GridData listGD = new GridData( GridData.FILL_BOTH );
    listGD.grabExcessHorizontalSpace = true;
    listGD.grabExcessVerticalSpace = true;
    listGD.widthHint = 150;
    execsList.setLayoutData( listGD );
    execsList.addSelectionListener( this );
    execsList.setData( FormToolkit.KEY_DRAW_BORDER, FormToolkit.TEXT_BORDER );
    toolkit.adapt( execsList, true, true );

    Action addAction = new Action( UITexts.cabalEditor_add, IAction.AS_PUSH_BUTTON ) {

      @Override
      public void run() {
        String initial="";
        // Default to project name if first executable created
        if (execsList.getItemCount()==0){
          initial=getPackageDescription().getPackageStanza().getName();
        }
        InputDialog dialog = new InputDialog( execsList.getShell(),
            UITexts.cabalEditor_newExecutableString,
            UITexts.cabalEditor_newExecutableString,
            initial, new IInputValidator() {

              @Override
              public String isValid( final String newText ) {
                String value = newText.trim();
                if (value.length()==0) {
                  return UITexts.cabalEditor_newExecutableBlankError;
                }
                for (String s : execsList.getItems()) {
                  if (s.equals( value )) {
                    return UITexts.cabalEditor_newExecutableAlreadyExistsError;
                  }
                }
                return null;
              }
            } );
        if (dialog.open() == Window.OK) {
          PackageDescription lastDescription = formEditor.getPackageDescription();
          String execName = dialog.getValue().trim();
          createNewStanza( lastDescription, execName );
          nextSelected = execName;
          formEditor.getModel().set( lastDescription.dump() );
        }
      }
    };
    addAction.setImageDescriptor( PlatformUI.getWorkbench().getSharedImages()
        .getImageDescriptor( ISharedImages.IMG_OBJ_ADD ) );

    Action removeAction = new Action( UITexts.cabalEditor_remove,
        IAction.AS_PUSH_BUTTON ) {

      @Override
      public void run() {
        PackageDescription lastDescription = formEditor.getPackageDescription();
        PackageDescriptionStanza stanza = sourceDirsSection.getStanza();
        lastDescription.removeStanza( stanza );
        formEditor.getModel().set( lastDescription.dump() );
      }
    };
    removeAction.setImageDescriptor( PlatformUI.getWorkbench()
        .getSharedImages().getImageDescriptor( ISharedImages.IMG_TOOL_DELETE ) );

    manager.add( addAction );
    manager.add( removeAction );
    manager.update( true );

    Composite right = toolkit.createComposite( form.getBody() );
    right.setLayout( createGridLayout( 3, 0, 0 ) );
    right.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    formEditor = ( CabalFormEditor )getEditor();

    depsSection = new DependenciesSection( this, right, formEditor, project );
    managedForm.addPart( depsSection );
    GridData depsGD = new GridData(GridData.FILL_BOTH);
    depsGD.verticalSpan = 2;
    depsGD.grabExcessVerticalSpace = true;
    depsSection.getSection().setLayoutData( depsGD );

    modulesSection = new ModulesExecutableSection( this, right, formEditor, project );
    managedForm.addPart( modulesSection );
    GridData modulesGD = new GridData(GridData.FILL_BOTH);
    modulesGD.verticalSpan = 2;
    modulesGD.grabExcessVerticalSpace = true;
    modulesSection.getSection().setLayoutData( modulesGD );

    sourceDirsSection = new SourceDirsSection( this, right, formEditor, project );
    managedForm.addPart( sourceDirsSection );
    managedForm.addPart( new CompilerOptionsSection( this, right, formEditor, project ) );

    setAllEditable( false );

    toolkit.paintBordersFor( form );
    this.finishedLoading();
  }

  void setAllEditable(final boolean editable) {
    for( IFormPart p: getManagedForm().getParts() ) {
      if( p instanceof CabalFormSection ) {
        ( ( CabalFormSection )p ).setAllEditable( editable );
      }
    }
  }

  void setStanza(final PackageDescriptionStanza stanza) {
    for( IFormPart p: getManagedForm().getParts() ) {
      if( p instanceof CabalFormSection ) {
        ( ( CabalFormSection )p ).setStanza( stanza, ignoreModify );
      }
    }
  }

  @Override
  public void widgetSelected( final SelectionEvent e ) {
    ignoreModify = true;
    PackageDescriptionStanza stanza;
    if (execsList.getSelectionCount() == 0) {
      stanza = null;
    } else {
      String name = execsList.getSelection()[0];
      stanza = getPackageDescription().getComponentStanza( new Component( getComponentType(), name, "", true ) );
    }

   modulesSection.refreshInput( project, this.getPackageDescription(), stanza, true );
    setStanza(stanza);
    ignoreModify = false;
  }

  public void selectStanza(final PackageDescriptionStanza stanza){
    if (execsList.getItemCount()==0){
      nextSelected=stanza.getName();
    } else {
      execsList.setSelection( new String[]{stanza.getName()} );
    }
  }

  @Override
  protected void setPackageDescriptionInternal(
      final PackageDescription packageDescription ) {

    ignoreModify = true;
    java.util.List<String> inList = Arrays.asList( execsList.getItems() );
    @SuppressWarnings ( "unused" )
    Vector<String> inPackage = new Vector<String>();
    for (PackageDescriptionStanza execStanza : this.getStanzas( packageDescription )) {
      inPackage.add( execStanza.getName() );
    }
    for (String listElement : inList) {
      if (inPackage.indexOf( listElement ) == -1) {
        execsList.remove( listElement );
      }
    }
    for (String pkgElement : inPackage) {
      if (inList.indexOf( pkgElement ) == -1) {
        execsList.add( pkgElement );
      }
    }
    if (nextSelected != null) {
      execsList.setSelection( new String[] { nextSelected } );
      nextSelected = null;
    }
    widgetSelected( null );
    ignoreModify = false;
  }

  @Override
  public void widgetDefaultSelected( final SelectionEvent e ) {
    // Do nothing
  }

}
