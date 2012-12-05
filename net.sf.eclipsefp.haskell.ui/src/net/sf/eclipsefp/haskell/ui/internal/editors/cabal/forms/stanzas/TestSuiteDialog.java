/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.ModuleInclusionType;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.dialog.FolderSelectionDialog;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas.FormEntryModules.ModulesVisitor;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.StatusDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.model.WorkbenchViewerComparator;


/**
 * Dialog to create a new test suite
 * @author JP Moresmau
 *
 */
public class TestSuiteDialog extends StatusDialog {

  /**
   * data for the test suite
   * @author JP Moresmau
   *
   */
  public class TestSuiteDef{
    /**
     * name
     */
    private String name="";
    /**
     * source folder
     */
    private IContainer src=null;
    /**
     * modules to test
     */
    private final Set<ModuleDef> modules=new HashSet<ModuleDef>();

    /**
     * @return the name
     */
    public String getName() {
      return name;
    }


    /**
     * @return the src
     */
    public IContainer getSrc() {
      return src;
    }



    /**
     * @return the modules
     */
    public Set<ModuleDef> getModules() {
      return modules;
    }
  }

  /*public TestSuiteDialog( final List execsList) {
    super( execsList.getShell(), UITexts.cabalEditor_newTestSuiteString,
        UITexts.cabalEditor_newTestSuiteString,
        "", new IInputValidator() {

          @Override
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
  }*/

  private final Set<String> usedNames;

  private final IProject project;

  private final TestSuiteDef def=new TestSuiteDef();

  private final PackageDescription pkgDesc;
  /**
   * should we overwrite existing files?
   */
  private boolean overwrite=true;

  public TestSuiteDialog(final Shell shell,final IProject project,final PackageDescription desc, final Set<String> names){
    super(shell);
    usedNames=names;
    this.project=project;
    this.pkgDesc=desc;
  }



  /**
   * @return the def
   */
  public TestSuiteDef getDefinition() {
    return def;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.window.Window#getShellStyle()
   */
  @Override
  protected int getShellStyle() {

    return super.getShellStyle() | SWT.RESIZE;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.dialogs.StatusDialog#configureShell(org.eclipse.swt.widgets.Shell)
   */
  @Override
  protected void configureShell( final Shell shell ) {
    super.configureShell( shell );
    shell.setText(  UITexts.cabalEditor_newTestSuiteString );
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent ) {
    Composite composite = (Composite) super.createDialogArea( parent );

    Label lName=new Label(composite,SWT.WRAP);
    lName.setText( UITexts.cabalEditor_newTestSuiteString );

    final Text tName=new Text(composite,SWT.SINGLE | SWT.BORDER);
    tName.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
        | GridData.HORIZONTAL_ALIGN_FILL));

    tName.addModifyListener(new ModifyListener() {
        @Override
        public void modifyText(final ModifyEvent e) {
          def.name= tName.getText().trim();
          validateInput();
        }
    });


    Label lSrc=new Label(composite,SWT.WRAP);
    lSrc.setText( UITexts.cabalEditor_newTestSuite_src );

    Composite cSrc=new Composite(composite,SWT.NONE);
    GridLayout glSrc=new GridLayout( 2, false ) ;
    glSrc.marginLeft=0;
    glSrc.marginRight=0;
    cSrc.setLayout(glSrc );
    cSrc.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
        | GridData.HORIZONTAL_ALIGN_FILL));
    final Text tSrc=new Text(cSrc,SWT.SINGLE | SWT.BORDER | SWT.READ_ONLY);
    tSrc.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
        | GridData.HORIZONTAL_ALIGN_FILL));
    Button bSrc=new Button(cSrc,SWT.PUSH);
    bSrc.setText( UITexts.dots );
    bSrc.addSelectionListener( new SelectionAdapter() {
      /* (non-Javadoc)
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        /*ContainerSelectionDialog csd=new ContainerSelectionDialog( getShell(), project, true, UITexts.source_folder_msg );
        csd.setTitle( UITexts.source_folder_title );
        if(Window.OK==csd.open()){
          src=(IPath)csd.getResult()[0];
          tSrc.setText(src.toPortableString());
        }*/
        FolderSelectionDialog fsd=new FolderSelectionDialog( getShell(), project,true );
        if(Window.OK==fsd.open()){
          def.src=(IContainer)fsd.getFirstResult();
          tSrc.setText(def.src.getProjectRelativePath().toPortableString());
          validateInput();
        }
      }
    } );

    final Button bOverwrite=new Button(composite,SWT.CHECK);
    bOverwrite.setText( UITexts.option_overwrite );
    bOverwrite.setSelection( true );
    bOverwrite.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent e) {
        overwrite=bOverwrite.getSelection();
      }
    } );

    Label lMods=new Label(composite,SWT.WRAP);
    lMods.setText( UITexts.cabalEditor_newTestSuite_modules);

    Set<ModuleDef> modules = new HashSet<ModuleDef>();

    for (PackageDescriptionStanza stz:pkgDesc.getStanzas()){
      if (CabalSyntax.SECTION_LIBRARY.equals( stz.getType()) || CabalSyntax.SECTION_EXECUTABLE.equals( stz.getType() )){
        Collection<String> sourceDirs = new HashSet<String>();
        if( stz.getProperties()
            .containsKey( CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName() ) ) {
          //sourceDirs = root.getStanza().getProperties()
          //    .get( CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName() );
          sourceDirs.addAll(stz.getSourceDirs());
        } else {
          sourceDirs.add("");
        }
        for (String s:sourceDirs){
          List<String> thisModules=new ArrayList<String>();
          ModulesVisitor visitor = new ModulesVisitor( thisModules, Collections.singletonList( s ) );
          try {
            project.accept( visitor );
          } catch( CoreException e ) {
            HaskellUIPlugin.log( e );
          }
          for (String mod:thisModules){
            // check we're in library AND the module is exposed
            boolean isLib=CabalSyntax.SECTION_LIBRARY.equals( stz.getType()) && ModuleInclusionType.EXPOSED.equals(stz.getModuleInclusionType( mod ));

            modules.add(new ModuleDef( mod, s, isLib ));
          }
        }
      }
    }


    Table tMods=new Table( composite, SWT.V_SCROLL | SWT.BORDER | SWT.CHECK);
    CheckboxTableViewer tvMods=new CheckboxTableViewer( tMods );
    tMods.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
        | GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_VERTICAL
        | GridData.VERTICAL_ALIGN_FILL));
    tvMods.setContentProvider( new ArrayContentProvider() );
    tvMods.setComparator( new WorkbenchViewerComparator() );
    tvMods.addCheckStateListener( new ICheckStateListener() {

      @Override
      public void checkStateChanged( final CheckStateChangedEvent arg0 ) {
        ModuleDef d=(ModuleDef)arg0.getElement();
        if (arg0.getChecked()){
          def.modules.add( d );
        } else {
          def.modules.remove( d );
        }
      }
    } );
    /*tvMods.getTable().setHeaderVisible( true );
    TableColumn exposedCol = new TableColumn( tvMods.getTable(), SWT.NULL );
    exposedCol.setText( UITexts.cabalEditor_newTestSuite_modules_col_test );
    exposedCol.pack();
    TableColumn otherCol = new TableColumn( tvMods.getTable(), SWT.NULL );
    otherCol.setText( UITexts.cabalEditor_newTestSuite_modules_col_mod );
    otherCol.pack();
     */

    tvMods.setInput( modules );

    validateInput();
    applyDialogFont(composite);
    return composite;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.dialogs.StatusDialog#create()
   */
  @Override
  public void create() {
    super.create();
    // override Eclipse policy of no error on showing dialog
    validateInput();
  }

  private void validateInput(){
    if (def.getName().length()==0) {
      updateStatus( new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), UITexts.cabalEditor_newTestSuiteBlankError ) );
      return;
    }
    if (usedNames.contains( def.getName() )) {
      updateStatus( new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), UITexts.cabalEditor_newTestSuiteAlreadyExistsError ) );
      return;
    }
    if (def.getSrc()==null){
      updateStatus( new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), UITexts.cabalEditor_newTestSuite_src_blank) );
      return;
    }
    updateStatus(new Status(IStatus.OK,HaskellUIPlugin.getPluginId(),""));


  }

  /**
   * a definition of a module to maybe test
   * @author JP Moresmau
   *
   */
  public static class ModuleDef {
    /**
     * module qualified name
     */
    private String module;
    /**
     * source container
     */
    private String srcPath;
    /**
     * are we exposed from a library
     */
    private boolean library;

    public ModuleDef( final String module, final String srcPath, final boolean library ) {
      super();
      this.module = module;
      this.srcPath = srcPath;
      this.library = library;
    }

    public String getModule() {
      return module;
    }

    public void setModule( final String module ) {
      this.module = module;
    }

    public String getSrcPath() {
      return srcPath;
    }

    public void setSrcPath( final String srcPath ) {
      this.srcPath = srcPath;
    }

    public boolean isLibrary() {
      return library;
    }

    public void setLibrary( final boolean library ) {
      this.library = library;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      return module;
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + ( ( module == null ) ? 0 : module.hashCode() );
      result = prime * result + ( ( srcPath == null ) ? 0 : srcPath.hashCode() );
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
      ModuleDef other = ( ModuleDef )obj;
      if( module == null ) {
        if( other.module != null ) {
          return false;
        }
      } else if( !module.equals( other.module ) ) {
        return false;
      }
      if( srcPath == null ) {
        if( other.srcPath != null ) {
          return false;
        }
      } else if( !srcPath.equals( other.srcPath ) ) {
        return false;
      }
      return true;
    }



  }


  public boolean isOverwrite() {
    return overwrite;
  }
}
