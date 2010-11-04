package net.sf.eclipsefp.haskell.ui.handlers;

import java.io.File;
import java.net.URI;
import java.net.URL;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.types.CabalPackage;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.util.text.WordFinder;
import net.sf.eclipsefp.haskell.util.FileUtil;
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
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.ide.IDE;
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

	public Object execute(final ExecutionEvent event) {
		IEditorPart editor = HandlerUtil.getActiveEditor(event);
		if (editor instanceof HaskellEditor) {
			HaskellEditor haskellEditor = (HaskellEditor)editor;
			IFile file = haskellEditor.findFile();
			ISelection selection = haskellEditor.getSelectionProvider().getSelection();
			if (selection instanceof TextSelection) {
				TextSelection textSel = (TextSelection)selection;
				String name = textSel.getText().trim();
				ScionInstance instance=ScionPlugin.getScionInstance(  file );
				char haddockType=' ';
				//if (name.length()==0){
				  try {

  				  Location l=new Location(file.getLocation().toOSString(),haskellEditor.getDocument(), new Region(textSel.getOffset(),0));
  				  String s=instance.thingAtPoint( l ,true,false);
  				  if (s!=null && s.length()>0){
  				    name=s;
  				    if (name.length()>2 && name.charAt( name.length()-2 )==' '){
  				      haddockType=name.charAt( name.length()-1 );
  				      name=name.substring( 0,name.length()-2 );
  				    }
  				    /*int ix=name.indexOf( ' ' );
  				    if (ix>-1){
  				      name=name.substring( 0,ix );
  				    }
  				    ix=name.lastIndexOf( '.' );
              if (ix>-1){
                name=name.substring( ix+1 );
              }*/
  				  }

				  } catch(BadLocationException ble){
				    ble.printStackTrace();
				  }

				//}
				if (name.length()==0){
          /*try {
            IRegion r=haskellEditor.getDocument().getLineInformationOfOffset( textSel.getOffset() );
            String line=haskellEditor.getDocument().get( r.getOffset(), r.getLength() );
            int off=textSel.getOffset()-r.getOffset();
            name=ParserUtils.getHaskellWord(line,off);
          } catch(BadLocationException ble){
            ble.printStackTrace();
          }*/
				  name=WordFinder.findWord( haskellEditor.getDocument(), textSel.getOffset() );
				}
				if (name!=null && name.length()>0){
  				Location location = instance.firstDefinitionLocation(name);
  				if (location != null) {
  					try {
  						openInEditor(haskellEditor.getEditorSite().getPage(), location,file.getProject());
  					} catch (PartInitException ex) {
  					  ex.printStackTrace();
  						// too bad
  					}
  				} else {
  				  // we try to find an id for an object not exported, that scion doesn't know
  				  // but that we have in the outline
				    location=haskellEditor.getOutlineLocation( name );
				    if (location!=null){
				      selectAndReveal( haskellEditor, haskellEditor.getDocument(), location );
				    } else {
				      int ix=name.lastIndexOf( '.' );
				      if (ix>0 && ix<name.length()-1){
				        String module=name.substring( 0,ix );
				        String shortName=name.substring( ix+1 );
  				      // find in outside location...
  				      outer:for (CabalPackage[] pkgs:instance.getPackagesByDB().values()){
  				        for (CabalPackage cp:pkgs){
  				          if (cp.getModules()!=null && cp.getModules().contains(module)){
  				            String pkg=cp.toString();
  				            openHaddock(haskellEditor.getEditorSite().getPage(),instance.getProject(),pkg,module,shortName,haddockType);
  				            break outer;

  				          }
  				        }
  				      }
				      }

				    }

  				}
				}
			}
		}
		return null;
	}

	protected static boolean openHaddock(final IWorkbenchPage page, final IProject project,final String pkg, final String module, final String shortName, final char type){
	  int ix=pkg.indexOf( '-' );
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
	  String relFile=pkg+"/"+moduleHTMLFile;
	  String anchor="#"+type+":"+shortName;
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
		      if (f.getProject().equals( p )){
		        file=f;
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
	  try {
      int startOffset = location.getStartOffset(document);
      int length = location.getLength(document);
      textEditor.selectAndReveal(startOffset, length);
    } catch (BadLocationException ex) {
      // ignore
    }
	}
}
