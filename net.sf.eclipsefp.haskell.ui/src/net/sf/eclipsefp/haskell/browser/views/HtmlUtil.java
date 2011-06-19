package net.sf.eclipsefp.haskell.browser.views;

import java.util.ArrayList;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;


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

  public static String generateDeclaration( final String definition,
      final ArrayList<PackageIdentifier> pkgs, final String docs ) {
    StringBuilder builder = new StringBuilder();
    initialPart( builder );

    builder.append( "<p style=\"font-family: monospace\">" );
    builder.append( definition );
    builder.append( "</p>" );

    addDocs( docs, builder );

    if( pkgs != null ) {
      builder.append( "<p>" );
      builder.append( "<b>Defined in: </b>" );
      boolean first = true;
      for( PackageIdentifier pkg: pkgs ) {
        if( !first ) {
          builder.append( ", " );
        }
        builder.append( pkg.toString() );
        first = false;
      }
      builder.append( "</p>" );
    }

    finalPart( builder );
    return builder.toString();
  }

  public static String generateModule( final String name, final String docs ) {
    if( name == null ) {
      return generateDocumented( null, docs );
    } else {
      return generateDocumented( "module " + name, docs );
    }
  }

  public static String generatePackage( final String name, final String docs ) {
    if( name == null ) {
      return generateDocumented( null, docs );
    } else {
      return generateDocumented( "package " + name, docs );
    }
  }

  public static String generateDocumented( final String definition,
      final String docs ) {
    StringBuilder builder = new StringBuilder();
    initialPart( builder );

    if( definition != null ) {
      builder.append( "<p style=\"font-family: monospace\">" );
      builder.append( definition );
      builder.append( "</p>" );
    }

    addDocs( docs, builder );

    finalPart( builder );
    return builder.toString();
  }
}
