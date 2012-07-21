/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.DataType;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.Documented;
import net.sf.eclipsefp.haskell.browser.items.Function;
import net.sf.eclipsefp.haskell.browser.items.Gadt;
import net.sf.eclipsefp.haskell.browser.items.NewType;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.Packaged;
import net.sf.eclipsefp.haskell.browser.items.TypeClass;
import net.sf.eclipsefp.haskell.browser.items.TypeSynonym;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.buildwrapper.types.ExportDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportExportType;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportSpecDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.NameDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineResult;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
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
      Set<String> visited=new HashSet<String>();
      if (isMe) {
        HaskellEditor ed=HaskellUIPlugin.getHaskellEditor( doc );
        OutlineResult or=ed!=null?ed.getLastOutlineResult():null;
        if (or!=null){
          decls=getDeclarationsFromOutlineResult(file,or,visited);
        } else {
          decls = getDeclarationsFromFile( file,visited );
        }
      } else {
        decls = getDeclarationsFromFile( importDef.getModule(), project,visited );
        if (decls.size()==0){
          BWFacade f=BuildWrapperPlugin.getFacade( file.getProject() );
          if (f!=null){
            // reducing the scope of the query
            for (CabalPackage[] cps:f.getPackagesByDB().values()){
              for (CabalPackage cp:cps){
                if (cp.getModules().contains( importDef.getModule() )){
                  Database pkg=Database.Package( new PackageIdentifier( cp.getName(), cp.getVersion() ) );
                  Packaged<Declaration>[] browserDecls = BrowserPlugin.getSharedInstance().getDeclarations(pkg, importDef.getModule() );
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
                  }
                  break;
                }
              }
            }
          }

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

  private static List<FileDocumented> getDeclarationsFromFile( final String module, final IProject project,final Set<String> visited ) {
    visited.add(module);
    try {
      IFile file = ResourceUtil.findFileFromModule( project, module );
      // search in referenced projects
      if (file==null){
        for( IProject p: project.getReferencedProjects() ) {
          if( p.hasNature( HaskellNature.NATURE_ID )          ) {
            file = ResourceUtil.findFileFromModule( p, module );
            if (file!=null){
              break;
            }
          }
        }
      }
      if (file!=null){
        return getDeclarationsFromFile( file, visited );
      }
    } catch (Exception e) {
      HaskellUIPlugin.log( e );

    }
    return new ArrayList<FileDocumented>();
  }

  public static List<FileDocumented> getDeclarationsFromFile( final IFile file ) {
    Set<String> visited = new HashSet<String>();
    return getDeclarationsFromFile( file, visited );
  }

  private static List<FileDocumented> getDeclarationsFromFile( final IFile file,final Set<String> visited ) {
    try {
      if (file!=null){
        BWFacade f=BuildWrapperPlugin.getFacade( file.getProject() );
        if (f!=null){
          OutlineResult or=f.outline( file );
          return getDeclarationsFromOutlineResult( file, or,visited );
        }
      }
    } catch (Exception e) {
      HaskellUIPlugin.log( e );

    }
      return new ArrayList<FileDocumented>();

  }


  private static List<FileDocumented> getDeclarationsFromOutlineResult( final IFile file, final OutlineResult or,final Set<String> visited ) {
    ArrayList<FileDocumented> decls = new ArrayList<FileDocumented>();

    for (OutlineDef def : or.getOutlineDefs()) {
      outlineToBrowser( def,null,file,decls );
    }
    for (ExportDef ed:or.getExportDefs()){
      if (ed.getType().equals( ImportExportType.IEModule )
         && !visited.contains(ed.getName())) // avoid recursion if a module re-export itself
        {
        decls.addAll( getDeclarationsFromFile( ed.getName(), file.getProject(),visited ) );
      }
    }

    return decls;

  }

  /**
   * transform a NameDef into a Documented object
   * @param def the name
   * @return the Documented or null if not handled
   */
  public static Documented nameToBrowser( final NameDef def) {
    switch (def.getTypes().iterator().next()) {
      case CLASS:
        return new TypeClass("", new String[0], def.getName(), new String[0], new String[0] );
      case DATA:
        return new DataType( "", new String[0], def.getName(), new String[0], "", new Constructor[0] );
      case TYPE:
        return new NewType( "", new String[0], def.getName(), new String[0], "", new Constructor[0] );
      case FUNCTION:
        return new Function( "", def.getName(), def.getTypeSignature() );
      case SYN:
        return new TypeSynonym( "", def.getName(), new String[0], "?" );
      case CONSTRUCTOR:
        return new Constructor( "", def.getName(), def.getTypeSignature() ,"");
      default:
          return null;
    }
  }

  /**
   * build a Documented structure from an outline definition
   * @param def
   * @return
   */
  private static Documented outlineToBrowser( final OutlineDef def, final OutlineDef parent ) {
    switch (def.getTypes().iterator().next()) {
      case CLASS:
        return new TypeClass( def.getComment(), new String[0], def.getName(), new String[0], new String[0] );
      case DATA:
        return new DataType( def.getComment(), new String[0], def.getName(), new String[0], "", new Constructor[0] );
      case TYPE:
        return new NewType( def.getComment(), new String[0], def.getName(), new String[0], "", new Constructor[0] );
      case FUNCTION:
        return new Function( def.getComment(), def.getName(), def.getTypeSignature() );
      case SYN:
        return new TypeSynonym( def.getComment(), def.getName(), new String[0], "?" );
      case CONSTRUCTOR:
        return new Constructor( def.getComment(), def.getName(), def.getTypeSignature() ,parent.getName());
      default:
          return null;
    }
  }

  public static void outlineToBrowser( final OutlineDef def ,final OutlineDef parent , final IFile file,final List<FileDocumented> ret) {
    Documented d=outlineToBrowser( def,parent );
    if (d!=null){
      ret.add( new FileDocumented( d, file ) );
    }
    for (OutlineDef c:def.getChildren()){
      outlineToBrowser(c,def,file,ret);
    }
  }

  public CompletionProposal addItem(final IDocument doc, final String item, final String label) {
    try {
      String ritem=item;
      char c0=ritem.charAt( 0 );
      // operators need to be surrounded by parens
      if (!Character.isLetter(c0) && (c0!='(')){
        ritem="("+ritem+")";
      }
      String contents = importDef.getLocation().getContents( doc );
      // We had no items
      int en=importDef.getLocation().getEndOffset( doc );
      /*if (importDef.getChildren()==null) {
        return new CompletionProposal( " ("+item+")", en, 0,
            en + item.length(), ImageCache.MODULE, label, null, "" );
      }*/

      int pos = contents.indexOf( '(' );
      if (pos==-1){
        return new CompletionProposal( " ("+ritem+")", en, 0,
            ritem.length(), ImageCache.MODULE, label, null, "" );
      }
      // We have some items
      // Trim end the elements
      //String toSearch = "(" + items.replaceAll("\\s+$", "");
      //int pos = contents.indexOf( toSearch );
      //int newPos = location.getOffset() + pos + toSearch.length();
      int pos2=contents.lastIndexOf( ')' );
      int insert=en-contents.length()+pos2;
      String contentsToAdd =  ritem;
      if (importDef.getChildren()!=null && importDef.getChildren().size()>0){
        contentsToAdd = ", " + ritem;
      }
      return new CompletionProposal( contentsToAdd, insert, 0, contentsToAdd.length(),
          ImageCache.MODULE, label, null, "" );
    } catch (Exception e) {
      e.printStackTrace();
      HaskellUIPlugin.log( e );
    }
    return null;
  }

  private static int[] trimRemovedImport(final String contents,final int ixParens,final int start,final int end){
    int tstart=start;
    int tend=end;
    while (contents.charAt( tend )==' '){ // remove spaces after me
      tend++;
    }
    if (contents.charAt( tend )==','){ // remove comma after me
      tend++;
    }
    while (tstart>1 && contents.charAt( tstart-1 )==' '){ // remove spaces before me
      tstart--;
    }

    if (tstart-1==ixParens){//at start: remove spaces after removed comma
      while (contents.charAt( tend )==' '){
        tend++;
      }
    }
    return new int[]{tstart,tend};
  }

  public CompletionProposal removeItem(final IDocument doc, final String item, final String label) {
//    try {
//      String contents = importDef.getLocation().getContents( doc );
//      int ixP=contents.indexOf( "(" );
//      int ix=contents.indexOf( item,ixP );
//      if (ix>-1){
//        int end=ix+item.length();
//        int[] trimmed=trimRemovedImport( contents, ixP, ix, end );
//        ix=trimmed[0];
//        end=trimmed[1];
//        if (contents.charAt( end )==')'){
//          if (contents.charAt( ix-1 )=='('){
//            if (ix-1>ixP){
//              // we're in between (): remove them
//              end++;
//              ix--;
//              trimmed=trimRemovedImport( contents, ixP, ix, end );
//              ix=trimmed[0];
//              end=trimmed[1];
//            }
//          } else {
//            ix--; // remove preceding comma if we're at end
//          }
//        }
//        int st=importDef.getLocation().getStartOffset( doc );
//        String newContents=contents.substring( 0,ix )+contents.substring( end );
//        return new CompletionProposal( newContents.toString(), st, contents.length(),
//          0, ImageCache.MODULE, label, null, "" );
//      }
//    } catch (Exception e) {
//      HaskellUIPlugin.log( e );
//    }
//    return null;
    return removeItem( doc, Collections.singleton( item ),label);
  }

  public CompletionProposal removeItem(final IDocument doc, final Collection<String> items, final String label) {
    try {
      String contents = importDef.getLocation().getContents( doc );
      String newContents=contents;
      int ixP=contents.indexOf( "(" );
      for (String item:items){
        int ix=newContents.indexOf( item,ixP );
        if (ix>-1){
          int end=ix+item.length();
          int[] trimmed=trimRemovedImport( newContents, ixP, ix, end );
          ix=trimmed[0];
          end=trimmed[1];
          if (newContents.charAt( end )==')'){
            if (newContents.charAt( ix-1 )=='('){
              if (ix-1>ixP){
                // we're in between (): remove them
                end++;
                ix--;
                trimmed=trimRemovedImport( newContents, ixP, ix, end );
                ix=trimmed[0];
                end=trimmed[1];
              }
            } else {
              ix--; // remove preceding comma if we're at end
            }
          }

          newContents=newContents.substring( 0,ix )+newContents.substring( end );

        }
      }
      int st=importDef.getLocation().getStartOffset( doc );
      return new CompletionProposal( newContents.toString(), st, contents.length(),
          0, ImageCache.MODULE, label, null, "" );
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
