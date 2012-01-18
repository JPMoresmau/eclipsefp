/**
 * (c) 2011, Alejandro Serrano
 * (c) 2012 by JP Moresmau
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.browser.items.Documented;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineResult;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.AnImport.FileDocumented;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;

/**
 * Manages information and changed in the imports section
 * of a Haskell source file. All imports must be in only
 * one line to be recognized by this parser.
 * @author Alejandro Serrano
 * @author JP Moresmau
 */
public class ImportsManager {

  private final IFile file;
  private final IDocument doc;

  public ImportsManager(final IFile file, final IDocument doc) {
    this.file = file;
    this.doc = doc;
  }

  public ArrayList<AnImport> parseImports() {
    ArrayList<AnImport> r = new ArrayList<AnImport>();
//    int lines = doc.getNumberOfLines();
//    for (int line = 0; line < lines; line++) {
//      try {
//        IRegion reg = doc.getLineInformation( line );
//        String contents = doc.get( reg.getOffset(), reg.getLength() );
//
//        // Take blanks appart
//        int realInit = reg.getOffset();
//        int realLength = reg.getLength();
//        for (int i = 0; i < contents.length(); i++) {
//          if (Character.isWhitespace( i )) {
//            realInit++;
//            realLength--;
//          } else {
//            break;
//          }
//        }
//
//        // Get contents another time
//        reg = new Region( realInit, realLength );
//        contents = doc.get( reg.getOffset(), reg.getLength() );
//        if (contents.startsWith( "import" )) {
//          // We are in an import declaration
//          String[] words = contents.split( "[ \t\n\r]+" );
//          if (words.length > 1) {
//            // We are in an import with more than "import" on it
//            int namePlace = 1;
//            boolean isQualified = false;
//            // See if we have a "qualified"
//            if (words[1].equals("qualified")) {
//              namePlace = 2;
//              isQualified = true;
//            }
//            // Take the name of the import
//            String name = words[namePlace];
//            String items = null;
//            String qualifiedName = null;
//            boolean isHiding = false;
//            // See if we have more things
//            if (words.length > namePlace + 1) {
//              int nextThings = namePlace + 1;
//              // Maybe we have a "as" clause
//              if (words[nextThings].equals("as")) {
//                nextThings++;
//                if (words.length > nextThings) {
//                  qualifiedName = words[nextThings];
//                  nextThings++;
//                }
//              }
//              // Maybe we have a hiding clause
//              if (words.length > nextThings) {
//                if (words[nextThings].equals("hiding")) {
//                  nextThings++;
//                  isHiding = true;
//                }
//              }
//              // Try to find '(' and ')'
//              int beginPar = contents.indexOf( '(' );
//              if (beginPar != -1) {
//                int endPar = contents.indexOf( ')' );
//                items = contents.substring( beginPar + 1, endPar );
//              }
//            }
//
//            // Create the element
//            Location importLocation = new Location(file.getName(), doc, reg);
//            ImportDef def = new ImportDef( name, new Location(file.getName(), doc, reg), isQualified, isHiding, qualifiedName );
//            if (items != null) {
//              List<String> itemsExplode = Arrays.asList( items.split( "[ ]*,[ ]*" ) );
//              List<ImportSpecDef> importDefs = new ArrayList<ImportSpecDef>();
//              for (String item : itemsExplode) {
//                ImportSpecDef importDef = new ImportSpecDef(item, importLocation, ImportExportType.IEAbs);
//                importDefs.add(importDef);
//              }
//              def.setChildren( importDefs );
//            }
//            AnImport imp = new AnImport(def, false);
//            r.add(imp);
//          }
//        }
//
//      } catch (Exception ex) {
//        // We continue with the next line
//      }
//    }
    OutlineResult or=null;
    if (doc!=null){
      HaskellEditor editor=HaskellUIPlugin.getHaskellEditor( doc );
      if (editor!=null){
        or=editor.getLastOutlineResult();
      }
    }
    if (or==null){
      BWFacade f=BuildWrapperPlugin.getFacade( file.getProject() );
      if (f!=null){
        or=f.outline( file );

      }
    }
    if (or!=null){
      for (ImportDef id:or.getImportDefs()){
        AnImport imp = new AnImport( id,false );
        r.add(imp);
      }
    }
    return r;
  }

  public Map<String, Documented> getDeclarations() {

    Map<String, Imported> si=getImportedDeclarations();
    HashMap<String, Documented> r = new HashMap<String, Documented>(si.size());
    for (String i:si.keySet()) {
      r.put(i,si.get( i ).getDocumented().getDocumented()  );
    }

    return r;
  }

  public Map<String, Imported> getImportedDeclarations() {
    ArrayList<AnImport> imports = parseImports();
    // Add Prelude import
    boolean hasPrelude = false;
    HashMap<String, Imported> r = new HashMap<String, Imported>();


    // Add me
    String meName = ResourceUtil.getModuleName( file );

    getImportDeclarations(r, AnImport.createMe( meName ) );

    for (AnImport i : imports) {
      getImportDeclarations(r,i);
      if (i.getImportDef().getModule().equals( "Prelude" )) {
        hasPrelude = true;
      }
    }

    if (!hasPrelude) {
      getImportDeclarations(r, new AnImport(new ImportDef("Prelude", null, false, false, null ),false ));
    }

    return r;
  }

  private void getImportDeclarations(final HashMap<String, Imported> r,final AnImport i){
    Map<String, FileDocumented> ir=i.getDeclarations( file.getProject(), file, doc );
    for (String s:ir.keySet()){
      r.put(s,new Imported( ir.get( s ), i )  );
    }
  }

  public CompletionProposal addImport( final String name, final String place, final String qualified, final String label ) {
    AnImport lastImport = null;
    for (AnImport imp : parseImports()) {
      lastImport = imp;
      String qname = imp.getImportDef().getAlias();
      if (imp.getImportDef().getModule().equals( place ) && ( ((qname == null || qname.length()==0) && qualified == null) || (qname != null && qname.equals( qualified )) ) ) {
        // Change in this import
        if (imp.getImportDef().getChildren()==null) {
          // We didn't need to add an import in first place
          return null;
        }
        if (imp.getImportDef().isHiding()) {
          return imp.removeItem( doc, name, label );
        }
        return imp.addItem( doc, name, label );
      }
    }

    // If we finished here, that means that we need to add a new import
    try {
      // 1. Get line of the last element and find offset
      int line = doc.getLineOfOffset( doc.get().indexOf( "where" ) ) + 1;
      if (lastImport != null) {
        line=lastImport.getImportDef().getLocation().getEndLine()-1;
        //line = doc.getLineOfOffset( lastImport.getLocation().getOffset() );
      }
      int offsetToPut = doc.getLineOffset( line ) + doc.getLineLength( line );
      // 2. Create contents
      String contents;
      if (qualified != null) {
        contents = "import qualified " + place + " as " + qualified + " (" + name + ")";
      } else {
        contents = "import " + place + " (" + name + ")";
      }
      // 3. Create the proposal
      return new CompletionProposal( contents + PlatformUtil.NL, offsetToPut, 0, offsetToPut + contents.length() + 1, ImageCache.MODULE, label, null, "" );
    } catch (Exception e) {
      HaskellUIPlugin.log( e );
      return null;
    }
  }

  public CompletionProposal removeItemInImport(final String name, final int line, final String label) {
    try {
      for (AnImport imp : parseImports()) {
        int importLine =imp.getImportDef().getLocation().getStartLine();
          //doc.getLineOfOffset( imp.getLocation().getOffset() );
        if (importLine-1 == line) {
          return imp.removeItem( doc, name, label );
        }
      }
    } catch (Exception e) {
      HaskellUIPlugin.log( e );
    }
    return null;
  }

  public CompletionProposal replaceItemInImport(final String name,final String newName, final int line, final String label) {
    try {
      for (AnImport imp : parseImports()) {
        int importLine =imp.getImportDef().getLocation().getStartLine();
          //doc.getLineOfOffset( imp.getLocation().getOffset() );
        if (importLine-1 == line) {
          return imp.replaceItem( doc, name,newName, label );
        }
      }
    } catch (Exception e) {
      HaskellUIPlugin.log( e );
    }
    return null;
  }

  public static class Imported {
      private final FileDocumented documented;
      private final AnImport animport;



      public Imported( final FileDocumented documented, final AnImport animport ) {
        super();
        this.documented = documented;
        this.animport = animport;
      }

      public FileDocumented getDocumented() {
        return documented;
      }

      public AnImport getAnimport() {
        return animport;
      }


  }
}
