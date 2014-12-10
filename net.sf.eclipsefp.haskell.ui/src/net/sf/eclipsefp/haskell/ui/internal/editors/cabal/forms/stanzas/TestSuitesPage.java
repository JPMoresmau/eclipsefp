package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component.ComponentType;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.code.EHaskellCommentStyle;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.core.code.SourceFileGenerator;
import net.sf.eclipsefp.haskell.core.internal.code.CodeGenerator;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.preferences.TemplateVariables;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormPage;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.LangUtil;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.ToolBarManager;
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
 * @author Alejandro Serrano, JP Moresmau
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

  private enum TestType {
    HTF(ICorePreferenceNames.TEMPLATE_MODULE_HTF,ICorePreferenceNames.TEMPLATE_MAIN_HTF){
       @Override
       public void addVariables( final Map<String, String> vars ,final String imports) {
        /**
         * imports
         */
        vars.put( TemplateVariables.IMPORTS_HTF, imports );
        /**
         * generate HTF module header
         */
        if (vars.containsKey( TemplateVariables.MODULE )){
          String mod1=HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_MODULE_HTF, vars );
          vars.put( TemplateVariables.MODULE_HTF, mod1 );
        }
      }
    },
    TASTY(ICorePreferenceNames.TEMPLATE_MODULE_TASTY,ICorePreferenceNames.TEMPLATE_MAIN_TASTY){
       @Override
       public void addVariables( final Map<String, String> vars ,final String imports) {
        /**
         * imports
         */
        vars.put( TemplateVariables.IMPORTS, imports );
        /**
         * generate HTF module header
         */
        if (vars.containsKey( TemplateVariables.MODULE )){
          String mod1=HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_MODULE_TASTY, vars );
          vars.put( TemplateVariables.MODULE_TASTY, mod1 );
        }
      }
    },
    OTHER(ICorePreferenceNames.TEMPLATE_MODULE,ICorePreferenceNames.TEMPLATE_MAIN){
       @Override
       public void addVariables( final Map<String, String> vars ,final String imports) {
        /**
         * imports
         */
        vars.put( TemplateVariables.IMPORTS, imports );

      }
    };


    private String moduleTemplate;
    private String mainTemplate;

    private TestType(final String mod,final String main){
      moduleTemplate=mod;
      mainTemplate=main;

    }


    /**
     * @return the moduleTemplate
     */
    public String getModuleTemplate() {
      return moduleTemplate;
    }


    /**
     * @return the mainTemplate
     */
    public String getMainTemplate() {
      return mainTemplate;
    }

    public abstract void addVariables( final Map<String, String> vars ,String imports);
  }

  private TestType getTestType(final String pref){
    switch(pref){
      case ICorePreferenceNames.TEMPLATE_CABAL_HTF:
          return TestType.HTF;
      case ICorePreferenceNames.TEMPLATE_CABAL_TASTY:
          return TestType.TASTY;
      default:
          return TestType.OTHER;
    }
  }

  public void addStanza(final PackageDescription desc,final String pref,final TestSuiteDialog.TestSuiteDef def,final boolean overwrite){
    Map<String,String> vars=new HashMap<>();
    vars.put( TemplateVariables.PROJECT_NAME, getPackageDescription().getPackageStanza().getName() );
    vars.put( TemplateVariables.SRC, def.getSrc().getProjectRelativePath().toPortableString() );
    vars.put( TemplateVariables.USER_NAME, PlatformUtil.getCurrentUser() );
    vars.put( TemplateVariables.SECTION_NAME,def.getName());
    String t=HaskellCorePlugin.populateTemplate( pref, vars );
    PackageDescriptionStanza pd=PackageDescriptionLoader.loadStanza( t );
    if (pd!=null){

      Set<String> srcs=new HashSet<>();
      boolean needLibrary=false;
      StringBuilder imports=new StringBuilder();

      TestType testType=getTestType( pref );
      /**
       * for each testes module, reference the module directly or just the library
       */
      for (TestSuiteDialog.ModuleDef md:def.getModules()){
        if (md.isLibrary()){
          needLibrary=true;
        } else{
          pd.addToPropertyList( CabalSyntax.FIELD_OTHER_MODULES, md.getModule() );
          srcs.add( md.getSrcPath() );
        }
        /**
         * create test module
         */
        String m=createTestModule( pd, def, md.getModule(),
            testType.getModuleTemplate(),overwrite );
        /**
         * generate import directive
         */
        if (TestType.HTF.equals( testType )){
          vars.put( TemplateVariables.MODULE_NAME,m);
          String mod1=HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_IMPORT_HTF, vars );
          imports.append(mod1);
        } else {
          imports.append("import "+m+PlatformUtil.NL);
        }
      }
      srcs.add( def.getSrc().getProjectRelativePath().toPortableString());
      /**
       * sources
       */
      for (String src:srcs){
        pd.addToPropertyList( CabalSyntax.FIELD_HS_SOURCE_DIRS,  src);
      }
      /**
       * dependency on library
       */
      if (needLibrary){
        pd.addToPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, desc.getPackageStanza().getName() );
        String value=desc.getPackageStanza().getProperties().get(CabalSyntax.FIELD_CABAL_VERSION);
        if (value!=null){
          if (value.startsWith( ">=" )) {
            value=value.substring( 2 ).trim();
          }
          if (value.length()>0 ){
            try {
              double d=Double.parseDouble( value );
              if (d<1.8){
                desc.getPackageStanza().update( CabalSyntax.FIELD_CABAL_VERSION, ">=1.8" );
              }
            } catch (NumberFormatException nfe){
              HaskellUIPlugin.log( nfe );
            }
          }
        } else {
          desc.getPackageStanza().update( CabalSyntax.FIELD_CABAL_VERSION, ">=1.8" );
        }
      }

      /**
       * create main
       */
      if (! ICorePreferenceNames.TEMPLATE_CABAL_DETAILED.equals( pref )){
        createMain( pd, def ,testType,imports.toString(),overwrite);
      }

      desc.addStanza( pd );
    }
  }

  /**
   * create test Main module
   * @param pd
   * @param def
   * @param isHTF
   * @param imports
   * @param overwrite should we overwrite existing files?
   */
  private void createMain( final PackageDescriptionStanza pd,final TestSuiteDialog.TestSuiteDef def,final TestType testType,final String imports,final boolean overwrite){
    final String mainName=def.getName() + "." + EHaskellCommentStyle.USUAL.getFileExtension(); //$NON-NLS-1$

    pd.update( CabalSyntax.FIELD_MAIN_IS, mainName );

    ModuleCreationInfo mci=new ModuleCreationInfo();
    mci.setCommentStyle( EHaskellCommentStyle.USUAL );
    mci.setModuleName( "Main" );
    mci.setSourceContainer( def.getSrc() );
    mci.setProject( sourceDirsSection.getProject() );
    mci.setTemplatePreferenceName(testType.getMainTemplate());
    CodeGenerator cg=new CodeGenerator(){
      /* (non-Javadoc)
       * @see net.sf.eclipsefp.haskell.core.internal.code.CodeGenerator#addVariables(java.util.Map)
       */
      @Override
      protected void addVariables( final Map<String, String> vars ) {
        testType.addVariables( vars, imports );
      }
    };
    SourceFileGenerator gen=new SourceFileGenerator(cg){
      /* (non-Javadoc)
       * @see net.sf.eclipsefp.haskell.core.code.SourceFileGenerator#createFileName(net.sf.eclipsefp.haskell.core.code.EHaskellCommentStyle, java.lang.String)
       */
      @Override
      protected String createFileName( final EHaskellCommentStyle style,
          final String moduleName ) {
        return mainName;
      }
    };
    gen.setOverwrite( overwrite );
    try {
      IFile f=gen.createFile( new NullProgressMonitor(), mci );
      f.setPersistentProperty( BuildWrapperPlugin.EDITORSTANZA_PROPERTY, def.getName() );
    } catch(CoreException ce){
      HaskellUIPlugin.log( ce );
    }
  }

  /**
   * create test module
   * @param pd
   * @param def
   * @param module the module to test
   * @param pref
   * @param overwrite should we overwrite existing files?
   * @return
   */
  private String createTestModule(final PackageDescriptionStanza pd,final TestSuiteDialog.TestSuiteDef def,final String module,final String pref,final boolean overwrite){
    String testModule=module+"Test";
    pd.addToPropertyList( CabalSyntax.FIELD_OTHER_MODULES,testModule );

    String[] mods=testModule.split( "\\." );

    ModuleCreationInfo mci=new ModuleCreationInfo();
    mci.setCommentStyle( EHaskellCommentStyle.USUAL );
    /**
     * module name and parent folders
     */
    mci.setModuleName( mods[mods.length-1] );
    if (mods.length>1){
      mci.setFolders( new Path(LangUtil.join( Arrays.asList( mods ).subList( 0, mods.length-1 ), "/" )));
    }
    mci.setSourceContainer( def.getSrc() );
    mci.setProject( sourceDirsSection.getProject() );
    mci.setTemplatePreferenceName( pref);
    CodeGenerator cg=new CodeGenerator(){
      /* (non-Javadoc)
       * @see net.sf.eclipsefp.haskell.core.internal.code.CodeGenerator#addVariables(java.util.Map)
       */
      @Override
      protected void addVariables( final Map<String, String> vars ) {
        /**
         * import tested module
         */
        vars.put(TemplateVariables.IMPORTS, "import "+module+PlatformUtil.NL );

      }
    };
    SourceFileGenerator gen=new SourceFileGenerator(cg);
    gen.setOverwrite( overwrite );
    try {
      IFile f=gen.createFile( new NullProgressMonitor(), mci );
      f.setPersistentProperty( BuildWrapperPlugin.EDITORSTANZA_PROPERTY, def.getName() );
    } catch(CoreException ce){
      HaskellUIPlugin.log( ce );
    }
    return testModule;
  }

  /*public String createNewStdioStanza(final String name,final Map<String,String> extraVars) {

    return HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_CABAL_STDIO, extraVars );

//    PackageDescriptionStanza stanza = desc.addStanza(
//        CabalSyntax.SECTION_TESTSUITE, name );
//    stanza.setIndent( 2 );
//    stanza.update( CabalSyntax.FIELD_TYPE, CabalSyntax.VALUE_EXITCODE_STDIO_1_0.getCabalName() );
//    stanza.update( CabalSyntax.FIELD_BUILD_DEPENDS, "base >= 4" );
//    stanza.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, "src" );
//    stanza.update( CabalSyntax.FIELD_GHC_OPTIONS, "-Wall -rtsopts" );
//    return stanza;
  }

  public String createNewDetailedStanza(final String name,final Map<String,String> extraVars) {
    return HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_CABAL_DETAILED, extraVars );
  }

  public String createNewTestFrameworkStanza(final String name,final Map<String,String> extraVars) {
    return HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_CABAL_TF, extraVars );
  }
*/
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

    final Action addHTFAction = new NewTestSuiteAction( UITexts.cabalEditor_HTFTestSuite, ICorePreferenceNames.TEMPLATE_CABAL_HTF ) ;

    final Action addTastyAction = new NewTestSuiteAction( UITexts.cabalEditor_TastyTestSuite, ICorePreferenceNames.TEMPLATE_CABAL_TASTY ) ;

    final Action addTestFrameworkAction = new NewTestSuiteAction( UITexts.cabalEditor_testFrameworkTestSuite, ICorePreferenceNames.TEMPLATE_CABAL_TF ) ;

    final Action addStdioAction = new NewTestSuiteAction( UITexts.cabalEditor_stdioTestSuite,ICorePreferenceNames.TEMPLATE_CABAL_STDIO );
    final Action addDetailedAction = new NewTestSuiteAction( UITexts.cabalEditor_detailedTestSuite, ICorePreferenceNames.TEMPLATE_CABAL_DETAILED ) ;

    addAction.setMenuCreator( new IMenuCreator() {

      Menu menu;
      MenuManager menuManager;

      @Override
      public Menu getMenu( final Menu parent ) {
        // Not expected
        return null;
      }

      @Override
      public Menu getMenu( final Control parent ) {
        menuManager = new MenuManager();
        menu = menuManager.createContextMenu( parent );

        menuManager.add( addHTFAction );
        menuManager.add( addTastyAction );
        menuManager.add( addTestFrameworkAction );
        menuManager.add( addStdioAction );
        menuManager.add( addDetailedAction );

        menuManager.update( true );

        return menu;
      }

      @Override
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
    setStanza(stanza,true);
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
    Vector<String> inPackage = new Vector<>();
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

  private class NewTestSuiteAction extends Action {
    private final String pref;

    public NewTestSuiteAction( final String text,final String pref) {
      super( text, IAction.AS_PUSH_BUTTON  );
      this.pref=pref;
    }

    @Override
    public void run() {
      PackageDescription lastDescription = formEditor.getPackageDescription();
      IProject p=sourceDirsSection.getProject();
      Set<String> names=new HashSet<>(Arrays.asList(execsList.getItems()));
      TestSuiteDialog dialog = new TestSuiteDialog( execsList.getShell(),p,lastDescription,names);
      if (dialog.open() == Window.OK) {

        TestSuiteDialog.TestSuiteDef def = dialog.getDefinition();
        //createNewTestFrameworkStanza( lastDescription, execName );
        addStanza( lastDescription, this.pref, def ,dialog.isOverwrite());
        nextSelected = def.getName();
        formEditor.getModel().set( lastDescription.dump() );
      }
    }
  }
}
