package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.Arrays;
import java.util.Vector;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.Component.ComponentType;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormPage;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.MenuManager;
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
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.IFormPart;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;

/**
 * Page for handling test-suite stanzas in the Cabal file.
 * @author Alejandro Serrano
 *
 */
public class TestSuitesPage extends CabalFormPage implements SelectionListener {

  private List execsList;
  @SuppressWarnings ( "unused" )
  private boolean ignoreModify = false;
  private CabalFormEditor formEditor;

  DependenciesSection depsSection;
  SourceDirsSection sourceDirsSection;
  ModulesTestSuiteSection modulesSection;
  TestTypeSection typeSection;

  String nextSelected = null;

  public TestSuitesPage( final FormEditor editor, final IProject project ) {
    super( editor, TestSuitesPage.class.getName(), UITexts.cabalEditor_testSuites, project );
  }

  public PackageDescriptionStanza createNewStdioStanza(final PackageDescription desc, final String name) {
    PackageDescriptionStanza stanza = desc.addStanza(
        CabalSyntax.SECTION_TESTSUITE, name );
    stanza.setIndent( 2 );
    stanza.update( CabalSyntax.FIELD_TYPE, CabalSyntax.VALUE_EXITCODE_STDIO_1_0.getCabalName() );
    stanza.update( CabalSyntax.FIELD_BUILD_DEPENDS, "base >= 4" );
    stanza.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, "src" );
    stanza.update( CabalSyntax.FIELD_GHC_OPTIONS, "-Wall -rtsopts" );
    return stanza;
  }

  public PackageDescriptionStanza createNewDetailedStanza(final PackageDescription desc, final String name) {
    PackageDescriptionStanza stanza = desc.addStanza(
        CabalSyntax.SECTION_TESTSUITE, name );
    stanza.setIndent( 2 );
    stanza.update( CabalSyntax.FIELD_TYPE, CabalSyntax.VALUE_DETAILED_0_9.getCabalName() );
    stanza.update( CabalSyntax.FIELD_BUILD_DEPENDS, "base >= 4" );
    stanza.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, "src" );
    stanza.update( CabalSyntax.FIELD_GHC_OPTIONS, "-Wall" );
    return stanza;
  }

  public PackageDescriptionStanza createNewTestFrameworkStanza(final PackageDescription desc, final String name) {
    PackageDescriptionStanza stanza = desc.addStanza(
        CabalSyntax.SECTION_TESTSUITE, name );
    stanza.setIndent( 2 );
    stanza.update( CabalSyntax.FIELD_TYPE, CabalSyntax.VALUE_EXITCODE_STDIO_1_0.getCabalName() );
    stanza.update( CabalSyntax.FIELD_X_USES_TEST_FRAMEWORK, "true" );
    stanza.update( CabalSyntax.FIELD_BUILD_DEPENDS, "base >= 4, HUnit >= 1.2 && < 2, QuickCheck >= 2.4, test-framework >= 0.4.1, test-framework-quickcheck2, test-framework-hunit" );
    stanza.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, "src" );
    stanza.update( CabalSyntax.FIELD_GHC_OPTIONS, "-Wall -rtsopts" );
    return stanza;
  }

  public java.util.List<PackageDescriptionStanza> getStanzas(
      final PackageDescription description ) {
    return description.getTestSuiteStanzas();
  }

  public ComponentType getComponentType() {
    return ComponentType.TESTSUITE;
  }

  @Override
  protected void createFormContent( final IManagedForm managedForm ) {
    ScrolledForm form = managedForm.getForm();
    FormToolkit toolkit = managedForm.getToolkit();
    toolkit.decorateFormHeading( form.getForm() );
    form.updateToolBar();
    form.setText( UITexts.cabalEditor_testSuites );

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

    Action addAction = new Action( UITexts.cabalEditor_add, IAction.AS_DROP_DOWN_MENU ) {
      // Nothing here
    };
    addAction.setImageDescriptor( PlatformUI.getWorkbench().getSharedImages()
        .getImageDescriptor( ISharedImages.IMG_OBJ_ADD ) );

    final Action addStdioAction = new Action( UITexts.cabalEditor_stdioTestSuite, IAction.AS_PUSH_BUTTON ) {

      @Override
      public void run() {
        InputDialog dialog = new InputDialog( execsList.getShell(),
            UITexts.cabalEditor_newTestSuiteString,
            UITexts.cabalEditor_newTestSuiteString,
            "", new IInputValidator() {

              public String isValid( final String newText ) {
                String value = newText.trim();
                if (value.length()==0) {
                  return UITexts.cabalEditor_newTestSuiteBlankError;
                }
                for (String s : execsList.getItems()) {
                  if (s.equals( value )) {
                    return UITexts.cabalEditor_newTestSuiteAlreadyExistsError;
                  }
                }
                return null;
              }
            } );
        if (dialog.open() == Window.OK) {
          PackageDescription lastDescription = formEditor.getPackageDescription();
          String execName = dialog.getValue().trim();
          createNewStdioStanza( lastDescription, execName );
          nextSelected = execName;
          formEditor.getModel().set( lastDescription.dump() );
        }
      }
    };
    final Action addDetailedAction = new Action( UITexts.cabalEditor_detailedTestSuite, IAction.AS_PUSH_BUTTON ) {

      @Override
      public void run() {
        InputDialog dialog = new InputDialog( execsList.getShell(),
            UITexts.cabalEditor_newTestSuiteString,
            UITexts.cabalEditor_newTestSuiteString,
            "", new IInputValidator() {

              public String isValid( final String newText ) {
                String value = newText.trim();
                if (value.length()==0) {
                  return UITexts.cabalEditor_newTestSuiteBlankError;
                }
                for (String s : execsList.getItems()) {
                  if (s.equals( value )) {
                    return UITexts.cabalEditor_newTestSuiteAlreadyExistsError;
                  }
                }
                return null;
              }
            } );
        if (dialog.open() == Window.OK) {
          PackageDescription lastDescription = formEditor.getPackageDescription();
          String execName = dialog.getValue().trim();
          createNewDetailedStanza( lastDescription, execName );
          nextSelected = execName;
          formEditor.getModel().set( lastDescription.dump() );
        }
      }
    };
    final Action addTestFrameworkAction = new Action( UITexts.cabalEditor_testFrameworkTestSuite, IAction.AS_PUSH_BUTTON ) {

      @Override
      public void run() {
        InputDialog dialog = new InputDialog( execsList.getShell(),
            UITexts.cabalEditor_newTestSuiteString,
            UITexts.cabalEditor_newTestSuiteString,
            "", new IInputValidator() {

              public String isValid( final String newText ) {
                String value = newText.trim();
                if (value.length()==0) {
                  return UITexts.cabalEditor_newTestSuiteBlankError;
                }
                for (String s : execsList.getItems()) {
                  if (s.equals( value )) {
                    return UITexts.cabalEditor_newTestSuiteAlreadyExistsError;
                  }
                }
                return null;
              }
            } );
        if (dialog.open() == Window.OK) {
          PackageDescription lastDescription = formEditor.getPackageDescription();
          String execName = dialog.getValue().trim();
          createNewTestFrameworkStanza( lastDescription, execName );
          nextSelected = execName;
          formEditor.getModel().set( lastDescription.dump() );
        }
      }
    };

    addAction.setMenuCreator( new IMenuCreator() {

      Menu menu;
      MenuManager menuManager;

      public Menu getMenu( final Menu parent ) {
        // Not expected
        return null;
      }

      public Menu getMenu( final Control parent ) {
        menuManager = new MenuManager();
        menu = menuManager.createContextMenu( parent );

        menuManager.add( addTestFrameworkAction );
        menuManager.add( addStdioAction );
        menuManager.add( addDetailedAction );

        menuManager.update( true );

        return menu;
      }

      public void dispose() {
        menuManager.dispose();
      }
    } );

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
    depsGD.verticalSpan = 3;
    depsGD.grabExcessVerticalSpace = true;
    depsSection.getSection().setLayoutData( depsGD );

    modulesSection = new ModulesTestSuiteSection( this, right, formEditor, project );
    managedForm.addPart( modulesSection );
    GridData modulesGD = new GridData(GridData.FILL_BOTH);
    modulesGD.verticalSpan = 3;
    modulesGD.grabExcessVerticalSpace = true;
    modulesSection.getSection().setLayoutData( modulesGD );

    typeSection = new TestTypeSection( this, right, formEditor, project );
    managedForm.addPart( typeSection );
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

  void setStanza(final PackageDescriptionStanza stanza,final boolean first) {
    for( IFormPart p: getManagedForm().getParts() ) {
      if( p instanceof CabalFormSection ) {
        ( ( CabalFormSection )p ).setStanza( stanza, first );
      }
    }
  }

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
    setStanza(stanza,true);
    ignoreModify = true;
  }

  @Override
  protected void setPackageDescriptionInternal(
      final PackageDescription packageDescription ) {

    ignoreModify = true;
    java.util.List<String> inList = Arrays.asList( execsList.getItems() );
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

  public void widgetDefaultSelected( final SelectionEvent e ) {
    // Do nothing
  }

}
