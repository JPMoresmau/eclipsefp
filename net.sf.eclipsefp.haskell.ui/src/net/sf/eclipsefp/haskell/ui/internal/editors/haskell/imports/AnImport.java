/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.DataType;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.Documented;
import net.sf.eclipsefp.haskell.browser.items.Function;
import net.sf.eclipsefp.haskell.browser.items.Gadt;
import net.sf.eclipsefp.haskell.browser.items.NewType;
import net.sf.eclipsefp.haskell.browser.items.Packaged;
import net.sf.eclipsefp.haskell.browser.items.TypeClass;
import net.sf.eclipsefp.haskell.browser.items.TypeSynonym;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.ExportDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportExportType;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportSpecDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineResult;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;

/**
 * Represents information about an import: if it's qualified,
 * the list of things that imports or hides...
 * @author Alejandro Serrano
 *
 */
public class AnImport {

//  private final String name;
//  private final String qualifiedName;
//  private final IRegion location;
//  private final boolean isComplete;
//  private final boolean isHiding;
//  private final boolean isQualified;
//  private final String items;
  private final ImportDef importDef;
  private final boolean isMe;

//  public AnImport( final String name, final IRegion location,
//      final boolean isComplete, final boolean isHiding, final String items ) {
//    this( name, location, isComplete, isHiding, false, null, items );
//  }
//
//  public AnImport( final String name, final IRegion location,
//      final boolean isComplete, final boolean isHiding,
//      final boolean isQualified, final String qualifiedName, final String items) {
//    this( name, location, isComplete, isHiding, isQualified, qualifiedName, items, false );
//  }
//
//  public AnImport( final String name, final IRegion location,
//      final boolean isComplete, final boolean isHiding,
//      final boolean isQualified, final String qualifiedName, final String items,
//      final boolean isMe) {
//    this.name = name;
//    this.location = location;
//    this.isComplete = isComplete;
//    this.isHiding = isHiding;
//    this.isQualified = isQualified;
//    this.qualifiedName = qualifiedName;
//    this.items = items;
//    this.isMe = isMe;
//  }

  public static AnImport createMe( final String name ) {
    return new AnImport(new ImportDef(name, null, false, false, null), true);
  }


//
//  public String getItems() {
//    return this.items;
//  }

  public AnImport( final ImportDef importDef, final boolean isMe ) {
    super();
    this.importDef = importDef;
    this.isMe = isMe;
  }


  public ImportDef getImportDef() {
    return importDef;
  }

//  public String[] getItemsList() {
//    return this.items.trim().split( "[ ]*,[ ]*" );
//  }
//
//  public String getName() {
//    return this.name;
//  }
//
//  public IRegion getLocation() {
//    return this.location;
//  }
//
//  public boolean isComplete() {
//    return this.isComplete;
//  }
//
//  public boolean isHiding() {
//    return this.isHiding;
//  }
//
//  public boolean isQualified() {
//    return this.isQualified;
//  }
//
//  public String getQualifiedName() {
//    return this.qualifiedName;
//  }

  public Map<String, FileDocumented> getDeclarations( final IProject project,
      final IFile file, final IDocument doc ) {
    // ArrayList<String> items
    String codeName =  importDef.getAlias() != null && importDef.getAlias().length()>0 ? importDef.getAlias() : importDef.getModule();
    HashMap<String, FileDocumented> r = new HashMap<String, FileDocumented>();
    try {
      List<FileDocumented> decls;
      if (isMe) {
        HaskellEditor ed=HaskellUIPlugin.getHaskellEditor( doc );
        OutlineResult or=ed!=null?ed.getLastOutlineResult():null;
        if (or!=null){
          decls=getDeclarationsFromOutlineResult(file,or);
        } else {
          decls = getDeclarationsFromFile( file, doc );
        }
      } else {
        BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.ALL, null );
        Packaged<Declaration>[] browserDecls = BrowserPlugin.getSharedInstance().getDeclarations( importDef.getModule() );

        if (browserDecls.length > 0) {
          // If the browser found the module
          decls = new ArrayList<FileDocumented>();
          for (Packaged<Declaration> browserDecl : browserDecls) {
            decls.add(new FileDocumented( browserDecl.getElement(),null) );
            if (browserDecl.getElement() instanceof Gadt) {
              Gadt g = (Gadt)browserDecl.getElement();
              for (Constructor c : g.getConstructors()) {
                decls.add(new FileDocumented(  c, null) );
              }
            }
          }
        } else {
          decls = getDeclarationsFromFile( importDef.getModule(), project );
        }
      }

      if (importDef.getChildren()==null) {
        // Add everything
        for (FileDocumented decl : decls) {
          addDeclaration( r, codeName, decl, importDef.isQualified() );
        }
      } else {
        //List<String> itemsExplode = Arrays.asList( this.items.split( "[ ]*,[ ]*" ) );
        Set<String> itemsExplode=new HashSet<String>();
        for (ImportSpecDef isd:importDef.getChildren()){
            itemsExplode.add(isd.getName());
        }
        for (FileDocumented decl : decls) {
          boolean inList = itemsExplode.contains( decl.getDocumented().getName() );
          boolean toAdd = (importDef.isHiding() && !inList) || (!importDef.isHiding() && inList);
          if (toAdd) {
            addDeclaration( r, codeName, decl, importDef.isQualified() );
          }
        }
      }
    } catch( Exception e ) {
      HaskellUIPlugin.log( e );
      r.clear();
    }
    return r;
  }

  private void addDeclaration(final HashMap<String, FileDocumented> r, final String codeName,
      final FileDocumented d, final boolean isQualified) {
    String declName = d.getDocumented().getName();
    declName = declName.startsWith( "(" ) ? declName.substring( 1, declName.length() - 1 ) : declName;
    r.put( codeName + "." + declName, d );
    if (!isQualified) {
      r.put(declName, d);
    }
  }

  private List<FileDocumented> getDeclarationsFromFile( final String module, final IProject project ) {
    try {
      IFile file = ResourceUtil.findFileFromModule( project, module );
      return getDeclarationsFromFile( file, null );
    } catch (Exception e) {
      HaskellUIPlugin.log( e );
      return new ArrayList<FileDocumented>();
    }
  }

  private List<FileDocumented> getDeclarationsFromFile( final IFile file, final IDocument doc ) {
    try {
      if (file!=null){
        BWFacade f=BuildWrapperPlugin.getFacade( file.getProject() );
        if (f!=null){
          OutlineResult or=f.outline( file );
          return getDeclarationsFromOutlineResult( file, or );
        }
      }
    } catch (Exception e) {
      HaskellUIPlugin.log( e );

    }
      return new ArrayList<FileDocumented>();

  }


  private List<FileDocumented> getDeclarationsFromOutlineResult( final IFile file, final OutlineResult or ) {
    ArrayList<FileDocumented> decls = new ArrayList<FileDocumented>();

          for (OutlineDef def : or.getOutlineDefs()) {
            FileDocumented d = outlineToBrowser( def,file );
            if (d != null) {
              decls.add( d );
            }
          }
          for (ExportDef ed:or.getExportDefs()){
            if (ed.getType().equals( ImportExportType.IEModule )){
              decls.addAll( getDeclarationsFromFile( ed.getName(), file.getProject() ) );
            }
          }

      return decls;

  }


  private static Documented outlineToBrowser( final OutlineDef def ) {
    switch (def.getTypes().iterator().next()) {
      case CLASS:
        return new TypeClass( "", new String[0], def.getName(), new String[0], new String[0] );
      case DATA:
        return new DataType( "", new String[0], def.getName(), new String[0], "", new Constructor[0] );
      case TYPE:
        return new NewType( "", new String[0], def.getName(), new String[0], "", new Constructor[0] );
      case FUNCTION:
        return new Function( "", def.getName(), "?" );
      case SYN:
        return new TypeSynonym( "", def.getName(), new String[0], "?" );
      case CONSTRUCTOR:
        return new Constructor( "", def.getName(), "?" );
      default:
          return null;
    }
  }

  public static FileDocumented outlineToBrowser( final OutlineDef def , final IFile file) {
    Documented d=outlineToBrowser( def );
    if (d!=null){
      return new FileDocumented( d, file );
    }
    return null;
  }

  public CompletionProposal addItem(final IDocument doc, final String item, final String label) {
    try {

      String contents = importDef.getLocation().getContents( doc );
      // We had no items
      int en=importDef.getLocation().getEndOffset( doc );
      if (importDef.getChildren()==null) {
        return new CompletionProposal( item, en + 1, 0,
            en + 1 + item.length(), ImageCache.MODULE, label, null, "" );
      }

      int pos = contents.indexOf( '(' );
      if (pos==-1){
        return new CompletionProposal( "("+item+")", en + 1, 0,
            en + 1 + item.length(), ImageCache.MODULE, label, null, "" );
      }
      // We have some items
      // Trim end the elements
      //String toSearch = "(" + items.replaceAll("\\s+$", "");
      //int pos = contents.indexOf( toSearch );
      //int newPos = location.getOffset() + pos + toSearch.length();
      int pos2=contents.lastIndexOf( ')' );
      int insert=en-contents.length()+pos2;
      String contentsToAdd =  item;
      if (importDef.getChildren().size()>0){
        contentsToAdd = ", " + item;
      }
      return new CompletionProposal( contentsToAdd, insert, 0, insert+ contentsToAdd.length(),
          ImageCache.MODULE, label, null, "" );
    } catch (Exception e) {
      HaskellUIPlugin.log( e );
    }
    return null;
  }

  public CompletionProposal removeItem(final IDocument doc, final String item, final String label) {
    try {
      String contents = importDef.getLocation().getContents( doc );
      int ix=contents.indexOf( item );
      if (ix>-1){
        int end=ix+item.length();
        while (contents.charAt( end )==' '){
          end++;
        }
        if (contents.charAt( end )==','){
          end++;
        }
        if (contents.charAt( end )==')' && contents.charAt( ix-1 )!='('){
          ix--;
        }
        int st=importDef.getLocation().getStartOffset( doc );
        String newContents=contents.substring( 0,ix )+contents.substring( end );
        return new CompletionProposal( newContents.toString(), st, contents.length(),
          0, ImageCache.MODULE, label, null, "" );
      }
    } catch (Exception e) {
      HaskellUIPlugin.log( e );
    }
    return null;
  }

  public CompletionProposal replaceItem(final IDocument doc, final String item, final String newItem, final String label) {
    try {
      String contents = importDef.getLocation().getContents( doc );

      int ix=contents.indexOf( item );
      if (ix>-1){
        int end=ix+item.length();

        int st=importDef.getLocation().getStartOffset( doc );
        String newContents=contents.substring( 0,ix )+newItem+contents.substring( end );
        return new CompletionProposal( newContents.toString(), st, contents.length(),
          0, ImageCache.MODULE, label, null, "" );
      }
    } catch (Exception e) {
      HaskellUIPlugin.log( e );
    }
    return null;
  }

  @Override
    public String toString() {
      return importDef.toString();
    }

  public static class FileDocumented {
      private Documented documented;
      private IFile file;

      public FileDocumented( final Documented documented, final IFile file ) {
        super();
        this.documented = documented;
        this.file = file;
      }

      public Documented getDocumented() {
        return documented;
      }

      public void setDocumented( final Documented documented ) {
        this.documented = documented;
      }

      public IFile getFile() {
        return file;
      }

      public void setFile( final IFile file ) {
        this.file = file;
      }

  }
}
