package net.sf.eclipsefp.haskell.browser.util;

import java.util.ArrayList;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;

/**
 * Generates Html for showing documentation in several places.
 *
 * @author serras
 */
public class HtmlUtil {

  private static void initialPart( final StringBuilder builder ) {
    builder.append( "<html>" );
    builder.append( "<body>" );
    builder.append( "<div style=\"font-size: small\">" );
  }

  private static void finalPart( final StringBuilder builder ) {
    builder.append( "</div>" );
    builder.append( "</body>" );
    builder.append( "</html>" );
  }

  private static void addDocs( final String docs, final StringBuilder builder ) {
    String[] paragraphs = docs.split( "\n\n" );
    for( String paragraph: paragraphs ) {
      builder.append( "<p>" );
      builder.append( paragraph );
      builder.append( "</p>" );
    }
  }

  private static void addPackageModule(
      final ArrayList<PackageIdentifier> pkgs, final String module,
      final StringBuilder builder ) {

    builder.append( "<p>" );
    if( module != null ) {
      builder.append( "<b>" );
      builder.append( UITexts.browser_definedInModules );
      builder.append( "</b>" );
      builder.append( module );
    }

    if( pkgs != null ) {
      if( module != null ) {
        builder.append( "<br />" );
      }

      builder.append( "<b>" );
      builder.append( UITexts.browser_packagedInPackages );
      builder.append( "</b>" );
      boolean first = true;
      for( PackageIdentifier pkg: pkgs ) {
        if( !first ) {
          builder.append( ", " );
        }
        builder.append( pkg.toString() );
        first = false;
      }
    }
    builder.append( "</p>" );
  }

  /**
   * Complete way to generate documentation.
   *
   * @param definition
   *          complete definition of the item to show
   * @param pkgs
   *          list of packages where it is defined, or null to not show it
   * @param module
   *          module where it is defined, or null to not show it
   * @param inAtStart
   *          if true, the information about the item package+module will be
   *          shown before the docs, if false, it will be after
   * @param docs
   *          documentation for the item
   */
  public static String generateDocument( final String definition,
      final ArrayList<PackageIdentifier> pkgs, final String module,
      final boolean inAtStart, final String docs ) {
    StringBuilder builder = new StringBuilder();
    initialPart( builder );

    if( definition != null ) {
      builder.append( "<p style=\"font-family: monospace\">" );
      builder.append( definition );
      builder.append( "</p>" );
    }

    if( inAtStart ) {
      addPackageModule( pkgs, module, builder );
    }

    addDocs( docs, builder );

    if( !inAtStart ) {
      addPackageModule( pkgs, module, builder );
    }

    finalPart( builder );
    return builder.toString();
  }

  /**
   * Complete way to generate documentation, equivalent to
   * {@link HtmlUtil#generateDocument(String, ArrayList, String, boolean, String)}
   * but without package and module info.
   */
  public static String generateDocument( final String definition,
      final String docs ) {
    return generateDocument( definition, null, null, true, docs );
  }

  public static String generateText ( final String text ) {
    StringBuilder builder = new StringBuilder();
    initialPart( builder );
    builder.append( text );
    finalPart( builder );
    return builder.toString();
  }

  public static String generatePackageUrl( final PackageIdentifier item ) {
    if( item.getName().equals( "ghc" ) ) {
      // GHC libraries are a special case
      return "http://www.haskell.org/ghc/docs/" + item.getVersion()
          + "/html/libraries/" + item.toString() + "/";
    } else {
      return "http://hackage.haskell.org/package/" + item.toString();
    }
  }

  public static String generateModuleUrl( final PackageIdentifier pkg, final String module ) {
    String url;
    if( pkg.getName().equals( "ghc" ) ) {
      // GHC libraries are a special case
      url = "http://www.haskell.org/ghc/docs/" + pkg.getVersion()
          + "/html/libraries/" + pkg.toString() + "/";
    } else {
      url = "http://hackage.haskell.org/packages/archive/" + pkg.getName()
          + "/" + pkg.getVersion() + "/doc/html/";
    }
    // Add module name
    url += module.replace( '.', '-' ) + ".html";
    return url;
  }

  public static String generateElementUrl( final PackageIdentifier pkg, final String module, final boolean isFunctionLike, final String name ) {
    String url = generateModuleUrl( pkg, module );
    // Add declaration name
    String itemName = name;
    if( itemName.startsWith( "(" ) ) {
      itemName = itemName.substring( 1, itemName.length() - 1 );
    }
    if( isFunctionLike ) {
      url += "#v:" + itemName;
    } else {
      url += "#t:" + itemName;
    }
    return url;
  }
}
