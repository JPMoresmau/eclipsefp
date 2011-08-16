package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.Packaged;
import org.eclipse.jface.text.IRegion;


public class AnImport {

  private final String name;
  private final String qualifiedName;
  private final IRegion location;
  private final boolean isComplete;
  private final boolean isHiding;
  private final boolean isQualified;
  private final String items;

  public AnImport( final String name, final IRegion location,
      final boolean isComplete, final boolean isHiding, final String items ) {
    this( name, location, isComplete, isHiding, false, null, items );
  }

  public AnImport( final String name, final IRegion location,
      final boolean isComplete, final boolean isHiding,
      final boolean isQualified, final String qualifiedName, final String items ) {
    this.name = name;
    this.location = location;
    this.isComplete = isComplete;
    this.isHiding = isHiding;
    this.isQualified = isQualified;
    this.qualifiedName = qualifiedName;
    this.items = items;
  }

  public String getItems() {
    return this.items;
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

  public Map<String, Declaration> getDeclarations() {
    // ArrayList<String> items
    String codeName =  qualifiedName != null ? qualifiedName : name;
    HashMap<String, Declaration> r = new HashMap<String, Declaration>();
    try {
      BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.ALL, null );
      Packaged<Declaration>[] decls = BrowserPlugin.getSharedInstance().getDeclarations( this.name );

      if (items == null) {
        // Add everything
        for (Packaged<Declaration> decl : decls) {
          Declaration d = decl.getElement();
          r.put( codeName + "." + d.getName(), d );
          if (!isQualified) {
            r.put(d.getName(), d);
          }
        }
      } else {
        List<String> itemsExplode = Arrays.asList( this.items.split( ",[ ]*" ) );
        for (Packaged<Declaration> decl : decls) {
          Declaration d = decl.getElement();
          boolean inList = itemsExplode.contains( d.getName() );
          boolean toAdd = (isHiding && !inList) || (!isHiding && inList);
          if (toAdd) {
            r.put( codeName + "." + d.getName(), d );
            if (!isQualified) {
              r.put(d.getName(), d);
            }
          }
        }
      }
    } catch( Exception e ) {
      r.clear();
    }
    return r;
  }
}
