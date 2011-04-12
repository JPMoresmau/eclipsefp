package net.sf.eclipsefp.haskell.ui.handlers;

import java.io.File;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.types.CabalPackage;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.preferences.SearchPathsPP;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.text.WordFinder;
import net.sf.eclipsefp.haskell.ui.util.text.WordFinder.EditorThing;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.variables.IStringVariableManager;
import org.eclipse.core.variables.IValueVariable;
import org.eclipse.core.variables.VariablesPlugin;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * Action that opens an editor and scrolls it to the definition of the currently selected element.
 *
 * @author Thomas ten Cate
 */
public class OpenDefinitionHandler extends AbstractHandler {

	public OpenDefinitionHandler() {
		// explicit default constructor
	}

  public Object execute( final ExecutionEvent event ) {
    IEditorPart editor = HandlerUtil.getActiveEditor( event );
    if( !( editor instanceof HaskellEditor ) ) {
      return null;
    }

    final HaskellEditor haskellEditor = ( HaskellEditor )editor;
    WordFinder.getEditorThing( haskellEditor, true, false,new WordFinder.EditorThingHandler() {

      public void handle( final EditorThing thing ) {
        String name = thing.getName();
        char haddockType = thing.getHaddockType();
        final ScionInstance instance=thing.getInstance();
        final IFile file=thing.getFile();
        if( name != null && name.length() > 0 ) {
          Location location = instance.firstDefinitionLocation( name );

          if( location != null ) {
            final Location theLocation = location;
            new UIJob(UITexts.openDefinition_open_job) {
              @Override
              public IStatus runInUIThread( final IProgressMonitor monitor ) {
                try {
                  openInEditor( haskellEditor.getEditorSite().getPage(), theLocation, file.getProject() );
                } catch( PartInitException ex ) {
                  ex.printStackTrace();
                  // too bad
                }

                return Status.OK_STATUS;
              }
            }.schedule();
          } else {
            String module=null;
            String shortName=name;
            int ix = name.lastIndexOf( '.' );
            if( ix > 0 && ix < name.length() - 1 ) {
              module = name.substring( 0, ix );
              shortName = name.substring( ix + 1 );
            }
            // we try to find an id for an object not exported, that scion doesn't
            // know
            // but that we have in the outline
            // short name of course in the outline
            location = haskellEditor.getOutlineLocation( shortName );
            if( location != null ) {
              final Location theLocation = location;
              // Ensure that this happens in the UI thread
              new UIJob(UITexts.openDefinition_select_job) {
                @Override
                public IStatus runInUIThread( final IProgressMonitor monitor ) {
                  selectAndReveal( haskellEditor, haskellEditor.getDocument(), theLocation );
                  return Status.OK_STATUS;
                }
              }.schedule();
            } else {
              if( module!=null ) {
                final String moduleF=module;
                final String shortNameF=shortName;
                final char haddockTypeF=haddockType;
                new Thread(new Runnable(){
                  public void run() {
                    // find in outside location...
                    outer: for( CabalPackage[] pkgs: instance.getPackagesByDB().values() ) {
                      for( CabalPackage cp: pkgs ) {
                        if( cp.getModules() != null
                            && cp.getModules().contains( moduleF ) ) {
                          final String pkg = cp.toString();
                           new UIJob(UITexts.openDefinition_select_job) {
                            @Override
                            public IStatus runInUIThread( final IProgressMonitor monitor ) {
                              openExternalDefinition( haskellEditor.getEditorSite().getPage(), instance.getProject(), pkg, moduleF,
                                  shortNameF, haddockTypeF );
                              return Status.OK_STATUS;
                            }
                          }.schedule();
                          break outer;
                        }
                      }
                    }
                  }
                }).start();

              }
            }
          }
        }

      }
    });

    return null;
  }

	protected static boolean openExternalDefinition(final IWorkbenchPage page, final IProject project,final String pkg, final String module, final String shortName, final char type){
	  int ix=pkg.lastIndexOf( '-' );
    String packageName=pkg;
    String packageVersion="";
    if (ix>-1){
      packageName=pkg.substring( 0,ix );
      if (ix<pkg.length()-2){
        packageVersion=pkg.substring( ix+1 );
      }
    }

    try {

      String moduleHSFile=module.replace( '.', '/' );

      for (IProject p:project.getReferencedProjects()){

        // TODO should we also check on the version
        if (p.hasNature(HaskellNature.NATURE_ID) && p.getName().equals( packageName )){
          IFile f=ScionInstance.getCabalFile( project );
          PackageDescription pd=PackageDescriptionLoader.load(f);
          IResource r=null;
          for (String src:pd.getStanzasBySourceDir().keySet()){
            if (src!=null && src.equals( "." )) { //$NON-NLS-1$
              r=p.findMember( moduleHSFile +"."+ FileUtil.EXTENSION_HS);
              if (r==null || !r.exists()){
                r=p.findMember( moduleHSFile +"."+ FileUtil.EXTENSION_LHS);
              }
            } else {
              IFolder fldr=p.getFolder( src );
              r=fldr.findMember( moduleHSFile +"."+ FileUtil.EXTENSION_HS);
              if (r==null || !r.exists()){
                r=fldr.findMember( moduleHSFile +"."+ FileUtil.EXTENSION_LHS);
              }
            }
            if (r!=null && r.exists() && r instanceof IFile){
              IEditorPart editor = IDE.openEditor(page, (IFile)r, true);
              if (editor instanceof HaskellEditor){
                HaskellEditor hEditor=(HaskellEditor)editor;
                Location location=hEditor.getOutlineLocation( shortName );
                if (location!=null){
                  IDocument document = hEditor.getDocumentProvider().getDocument(editor.getEditorInput());
                  selectAndReveal(hEditor,document,location);
                }
              }
              return true;
            }
          }
        }
      }
    } catch (CoreException ce){
      HaskellUIPlugin.log( ce );
    }


	  String moduleHTMLFile=module.replace( '.', '-' )+".html";
	  //String relFile=pkg+"/"+moduleHTMLFile;
	  String anchor=type+":"+shortName;

	  IHsImplementation hsImpl = CompilerManager.getInstance().getCurrentHsImplementation();

	  IStringVariableManager mgr=VariablesPlugin.getDefault().getStringVariableManager();
    String hsImplBinDir = hsImpl != null ? hsImpl.getBinDir() : "";

    if (hsImplBinDir.length() > 0) {
      IPath hsIBDPath = new Path (hsImplBinDir);
      // Need the extra "/" to make it a proper file URL: file:///C:/...
      if ( PlatformUtil.runningOnWindows() && hsIBDPath.isAbsolute() ) {
        hsImplBinDir = "/".concat( hsIBDPath.toPortableString() );
      }
    }

	  IValueVariable[] vars=new IValueVariable[]{
	      mgr.newValueVariable( "IMPL_BIN", "", true, hsImplBinDir ),
	      mgr.newValueVariable( "PACKAGE_NAME", "", true, packageName ),
	      mgr.newValueVariable( "PACKAGE_VERSION", "", true, packageVersion ),
	      mgr.newValueVariable( "MODULE", "", true, module ),
	      mgr.newValueVariable( "MODULE_HTML", "", true, moduleHTMLFile ),
	      mgr.newValueVariable( "ANCHOR", "", true, anchor ),
	      mgr.newValueVariable( "NAME", "", true, shortName ),
	  };
	  try {
  	  mgr.addVariables( vars );
  	  try {
  	    String s=HaskellUIPlugin.getDefault().getPreferenceStore().getString( IPreferenceConstants.HADDOCK_SEARCH_PATHS );
  	    if (s!=null && s.length()>0){
  	      String[] paths=SearchPathsPP.parseString( s );
  	      for (String p:paths){
	          String fullPath=mgr.performStringSubstitution( p );
	          try {
  	          URL url = new URL( fullPath );
  	          // Ensure that spaces are properly escaped inside the path portion of the URL
  	          if (exists(url)) {
  	            PlatformUI.getWorkbench().getBrowserSupport().createBrowser(pkg+" "+module ).openURL( url );
  	            return true;
  	          }
  	        } catch (Exception ce){
  	          HaskellUIPlugin.log( ce );
  	        }
  	      }
  	    }

  	  }finally {
  	    mgr.removeVariables( vars );
  	  }
	  } catch (CoreException ce){
	    HaskellUIPlugin.log( ce );
	  }
	  /*
	  IHsImplementation hsImpl = CompilerManager.getInstance().getCurrentHsImplementation();
	  if (hsImpl!=null){
	    File bin=new File(hsImpl.getBinDir());
	    File htmlDocs=new File(bin.getParentFile(),"doc/html/libraries");
	    if (htmlDocs.exists()){
	      File htmlFile=new File(htmlDocs,relFile);
	      if (htmlFile.exists()){
	        try {
	          URL url=new URL(htmlFile.toURL().toString()+anchor);
	          PlatformUI.getWorkbench().getBrowserSupport().createBrowser(pkg+" "+module ).openURL( url );
	          return true;
	        } catch (Exception e){
	          HaskellUIPlugin.log( e );
	        }
	      }
	    }
	  }

	  //http://hackage.haskell.org/packages/archive/websockets/0.1.2.3/doc/html/Network-WebSockets.html
	  try {
      URL url=new URL("http://hackage.haskell.org/packages/archive/"+packageName+"/"+packageVersion+"/doc/html/"+moduleHTMLFile +anchor);
      PlatformUI.getWorkbench().getBrowserSupport().createBrowser(pkg+" "+module ).openURL( url );
      return true;
    } catch (Exception e){
      HaskellUIPlugin.log( e );
    }
  */

	  return false;
	}

	private static boolean exists( final URL url ) {
	  try {
      if (url!=null){
        if (url.getProtocol().equalsIgnoreCase( "file" )) {
          return new File(url.getFile()).exists();
        } else if (url.getProtocol().equalsIgnoreCase( "http" )){
          HttpURLConnection conn=(HttpURLConnection)url.openConnection();
          conn.setRequestMethod( "HEAD" );
          conn.connect();
          try {
            return (HttpURLConnection.HTTP_OK==conn.getResponseCode());
          } finally {
            conn.disconnect();
          }
        }
      }
	  } catch (Exception e){
	    HaskellUIPlugin.log( e );
	  }
    return false;
  }

  protected static void openInEditor(final IWorkbenchPage page, final Location location,final IProject p) throws PartInitException {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		URI uri = new File(location.getFileName()).toURI(); //new URI("file", "", location.getFileName(), null, null);
		IFile[] files = root.findFilesForLocationURI(uri, IResource.FILE);
		if (files.length > 0) {
		  IFile file = files[0]; // open only the first file; they should be the same anyway

		  if (!file.getProject().equals( p ) && files.length>1){
		    for (IFile f:files){
		      if (f.getProject().equals( p )) {
		        file = f;
		        break;
		      }
		    }
		  }

		  IEditorPart editor = IDE.openEditor(page, file, true);
      ITextEditor textEditor = (ITextEditor)editor;
      IDocument document = textEditor.getDocumentProvider().getDocument(editor.getEditorInput());
      selectAndReveal(textEditor,document,location);
		}
	}

	protected static void selectAndReveal(final ITextEditor textEditor,final IDocument document,final Location location){
	  if (document==null){
	    return;
	  }
	  try {
      int startOffset = location.getStartOffset(document);
      int length = location.getLength(document);
      textEditor.selectAndReveal(startOffset, length);
    } catch (BadLocationException ex) {
      // ignore
    }
	}
}
