/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.contentassist.CompletionProposal;

/**
 * Represents information about an import: if it's qualified,
 * the list of things that imports or hides...
 * @author Alejandro Serrano
 *
 */
public class AnImport {

  private final String name;
  private final String qualifiedName;
  private final IRegion location;
  private final boolean isComplete;
  private final boolean isHiding;
  private final boolean isQualified;
  private final String items;
  private final boolean isMe;

  public AnImport( final String name, final IRegion location,
      final boolean isComplete, final boolean isHiding, final String items ) {
    this( name, location, isComplete, isHiding, false, null, items );
  }

  public AnImport( final String name, final IRegion location,
      final boolean isComplete, final boolean isHiding,
      final boolean isQualified, final String qualifiedName, final String items) {
    this( name, location, isComplete, isHiding, isQualified, qualifiedName, items, false );
  }

  public AnImport( final String name, final IRegion location,
      final boolean isComplete, final boolean isHiding,
      final boolean isQualified, final String qualifiedName, final String items,
      final boolean isMe) {
    this.name = name;
    this.location = location;
    this.isComplete = isComplete;
    this.isHiding = isHiding;
    this.isQualified = isQualified;
    this.qualifiedName = qualifiedName;
    this.items = items;
    this.isMe = isMe;
  }

  public static AnImport createMe( final String name ) {
    return new AnImport(name, null, true, false, false, null, null, true);
  }

  public String getItems() {
    return this.items;
  }

  public String[] getItemsList() {
    return this.items.trim().split( "[ ]*,[ ]*" );
  }

  public String getName() {
    return this.name;
  }

  public IRegion getLocation() {
    return this.location;
  }

  public boolean isComplete() {
    return this.isComplete;
  }

  public boolean isHiding() {
    return this.isHiding;
  }

  public boolean isQualified() {
    return this.isQualified;
  }

  public String getQualifiedName() {
    return this.qualifiedName;
  }

  public Map<String, Documented> getDeclarations( final IProject project,
      final IFile file, final IDocument doc ) {
    // ArrayList<String> items
    String codeName =  qualifiedName != null ? qualifiedName : name;
    HashMap<String, Documented> r = new HashMap<String, Documented>();
    try {
      List<Documented> decls;
      if (isMe) {
        decls = getDeclarationsFromFile( file, doc );
      } else {
        BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.ALL, null );
        Packaged<Declaration>[] browserDecls = BrowserPlugin.getSharedInstance().getDeclarations( this.name );

        if (browserDecls.length > 0) {
          // If the browser found the module
          decls = new ArrayList<Documented>();
          for (Packaged<Declaration> browserDecl : browserDecls) {
            decls.add( browserDecl.getElement() );
            if (browserDecl.getElement() instanceof Gadt) {
              Gadt g = (Gadt)browserDecl.getElement();
              for (Constructor c : g.getConstructors()) {
                decls.add( c );
              }
            }
          }
        } else {
          decls = getDeclarationsFromFile( name, project );
        }
      }

      if (items == null) {
        // Add everything
        for (Documented decl : decls) {
          addDeclaration( r, codeName, decl, isQualified );
        }
      } else {
        List<String> itemsExplode = Arrays.asList( this.items.split( "[ ]*,[ ]*" ) );
        for (Documented decl : decls) {
          boolean inList = itemsExplode.contains( decl.getName() );
          boolean toAdd = (isHiding && !inList) || (!isHiding && inList);
          if (toAdd) {
            addDeclaration( r, codeName, decl, isQualified );
          }
        }
      }
    } catch( Exception e ) {
      r.clear();
    }
    return r;
  }

  private void addDeclaration(final HashMap<String, Documented> r, final String codeName,
      final Documented d, final boolean isQualified) {
    String declName = d.getName();
    declName = declName.startsWith( "(" ) ? declName.substring( 1, declName.length() - 1 ) : declName;
    r.put( codeName + "." + declName, d );
    if (!isQualified) {
      r.put(declName, d);
    }
  }

  private List<Documented> getDeclarationsFromFile( final String module, final IProject project ) {
    try {
      IFile file = ResourceUtil.findFileFromModule( project, module );
      ArrayList<Documented> decls = new ArrayList<Documented>();
      BWFacade f=BuildWrapperPlugin.getFacade( project );
      if (f!=null){
        for (OutlineDef def : f.outline( file )) {
          Documented d = outlineToBrowser( def );
          if (d != null) {
            decls.add( d );
          }
        }
      }
      return decls;
    } catch (Exception e) {
      return new ArrayList<Documented>();
    }
  }

  private List<Documented> getDeclarationsFromFile( final IFile file, final IDocument doc ) {
    try {
      ArrayList<Documented> decls = new ArrayList<Documented>();
      BWFacade f=BuildWrapperPlugin.getFacade( file.getProject() );
      if (f!=null){
        for (OutlineDef def : f.outline( file)) {
          Documented d = outlineToBrowser( def );
          if (d != null) {
            decls.add( d );
          }
        }
      }
      return decls;
    } catch (Exception e) {
      return new ArrayList<Documented>();
    }
  }

  public static Documented outlineToBrowser( final OutlineDef def ) {
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

  public CompletionProposal addItem(final IDocument doc, final String item, final String label) {
    try {
      String contents = doc.get(location.getOffset(), location.getLength());
      if (items.trim().length() == 0) {
        // We had no items
        int pos = contents.indexOf( '(' );
        return new CompletionProposal( item, location.getOffset() + pos + 1, 0,
            location.getOffset() + pos + 1 + item.length(), ImageCache.MODULE, label, null, "" );
      }
      // We have some items
      // Trim end the elements
      String toSearch = "(" + items.replaceAll("\\s+$", "");
      int pos = contents.indexOf( toSearch );
      int newPos = location.getOffset() + pos + toSearch.length();
      String contentsToAdd = ", " + item;
      return new CompletionProposal( contentsToAdd, newPos, 0, newPos + contentsToAdd.length(),
          ImageCache.MODULE, label, null, "" );
    } catch (Exception e) {
      return null;
    }
  }

  public CompletionProposal removeItem(final IDocument doc, final String item, final String label) {
    try {
      String contents = doc.get(location.getOffset(), location.getLength());
      int pos = contents.indexOf( '(' );
      StringBuilder newContents = new StringBuilder();
      for (String s : getItemsList()) {
        if (!s.equals( item )) {
          if (newContents.length() > 0) {
            // If not the first one, add a comma
            newContents.append( ", " );
          }
          newContents.append( s );
        }
      }
      return new CompletionProposal( newContents.toString(), location.getOffset() + pos + 1, items.length(),
        0, ImageCache.MODULE, label, null, "" );
    } catch (Exception e) {
      return null;
    }
  }
}
