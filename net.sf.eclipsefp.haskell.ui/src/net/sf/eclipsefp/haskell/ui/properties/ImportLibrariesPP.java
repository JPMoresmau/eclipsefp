// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PropertyPage;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProvider;

/** <p>The property page for libraries on Haskell projects.</p>
  *
  * @author Leif Frenzel
  * @deprecated
  * Use cabal editor
  */
@Deprecated
public class ImportLibrariesPP extends PropertyPage
                               implements IWorkbenchPreferencePage {

  private final ImportLibrariesViewerPart tablePart;

  public ImportLibrariesPP() {
    tablePart = new ImportLibrariesViewerPart();
    setDescription(UITexts.libraries_description);
    noDefaultAndApplyButton();
  }


  // interface methods of IDialogPage
  ///////////////////////////////////

  @Override
  public Control createContents( final Composite parent ) {
    Composite container = new Composite( parent, SWT.NULL );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    container.setLayout( layout );


    //ScionInstance si=ScionPlugin.getScionInstance( ( IProject )getElement() );
    BWFacade f=BuildWrapperPlugin.getFacade( ( IProject )getElement() );

    tablePart.setMinimumSize( 150, 200 );
    tablePart.createControl( container, SWT.BORDER, 2 );

//      list = new ImportLibrariesList( ( IProject )getElement() );

    tablePart.getTableViewer().setContentProvider( new ArrayContentProvider() );
    ImportLibrariesLabelProvider lp = new ImportLibrariesLabelProvider();
    tablePart.getTableViewer().setLabelProvider( lp );

    if (f!=null && f.getPackagesByDB()!=null){
      List<CabalPackage> list=new ArrayList<>();
      for (CabalPackage[] cps:f.getPackagesByDB().values()){
        for (CabalPackage cp:cps){
          if (cp.getComponents().length>0){
            list.add( cp );
          }
        }
      }
      tablePart.setAllPackages( f.getPackagesByDB().values() );
      tablePart.setComponents( f.getComponents() );
      tablePart.getTableViewer().setInput( list );
    }

//    initializeStates();

    tablePart.setButtonEnabled( 1, false );
    tablePart.setButtonEnabled( 2, false );
    Dialog.applyDialogFont( parent );
    return container;
  }


  // interface methods of IPreferencePage / IWorkbenchPreferencePage
  //////////////////////////////////////////////////////////////////

  @Override
  public void init( final IWorkbench workbench ) {
    // unused
  }

  @Override
  public boolean performOk() {
//    list.save();

    //final ScionInstance si=ScionPlugin.getScionInstance( ( IProject )getElement() );
    final BWFacade fa=BuildWrapperPlugin.getFacade( ( IProject )getElement()  );
    if (fa!=null && fa.getPackagesByDB()!=null) {
      try {
        IFile f=BuildWrapperPlugin.getCabalFile(  ( IProject )getElement() );
        IDocumentProvider prov=new TextFileDocumentProvider();
        prov.connect( f );
        IDocument doc=prov.getDocument( f );
        PackageDescription pd=PackageDescriptionLoader.load( f );

        for (CabalPackage[] cps:fa.getPackagesByDB().values()){
          for (CabalPackage cp:cps){
            // removed
            if (cp.getComponents().length>0 && tablePart.getRemoved().contains( cp.getName())){
              for (Component comp:cp.getComponents()){
                PackageDescriptionStanza pds=pd.getComponentStanza(comp);
                if (pds!=null){
                  removeDependency(doc,cp.getName(),pds);
                  pd=PackageDescriptionLoader.load( doc.get() );
                }
              }
            }

          }
        }
     // added
        for (CabalPackage cp:tablePart.getAdded().values()){
          for (Component comp:cp.getComponents()){
            PackageDescriptionStanza pds=pd.getComponentStanza(comp);
            if (pds!=null){
              String toAdd=cp.getName()+(cp.getVersion().length()>0?" "+cp.getVersion():"");
              RealValuePosition rvp=pds.addToPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, toAdd );
              if (rvp!=null){
                rvp.updateDocument( doc );
                pd=PackageDescriptionLoader.load( doc.get() );
              }
            }
          }
        }

        prov.saveDocument( new NullProgressMonitor(), f, doc, true );

        // Perform the ScionInstance.buildProject() in a Job to maintain UI responsiveness.
        // let the listener do its work
        //BuildOptions buildOptions=new BuildOptions().setOutput(false).setRecompile(true);
        //si.buildProject( buildOptions );
      } catch (CoreException ce){
        HaskellUIPlugin.log( ce );

      }
    }


    return super.performOk();
  }

    private void removeDependency(final IDocument doc,final String prefix,final PackageDescriptionStanza pds){
      RealValuePosition rvp=pds.removePrefixFromPropertyList( CabalSyntax.FIELD_BUILD_DEPENDS, prefix ,",");
      rvp.updateDocument( doc );
      for (PackageDescriptionStanza child:pds.getStanzas()){
        removeDependency( doc, prefix, child );
      }
    }

//  @Override
//  public void performDefaults() {
//    // uncheck all libs by default
//    IImportLibrary[] libs = list.getAll();
//    for( int i = 0; i < libs.length; i++ ) {
//      libs[ i ].setUsed( false );
//      tablePart.getTableViewer().setChecked( libs[ i ], false );
//    }
//    tablePart.getTableViewer().refresh();
//    super.performDefaults();
//  }


  // helping methods
  //////////////////

//  ImportLibrariesList getImportLibList() {
//    return list;
//  }
//
//  private void initializeStates() {
//    IImportLibrary[] libs = list.getAll();
//    for( int i = 0; i < libs.length; i++ ) {
//      tablePart.getTableViewer().setChecked( libs[ i ], libs[ i ].isUsed() );
//    }
//  }
}