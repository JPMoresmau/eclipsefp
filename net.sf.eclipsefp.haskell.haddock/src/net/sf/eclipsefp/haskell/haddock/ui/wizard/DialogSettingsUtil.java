// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;

import java.io.File;
import java.util.StringTokenizer;

import net.sf.eclipsefp.haskell.haddock.core.*;

import org.eclipse.jface.dialogs.IDialogSettings;

/** <p>transfers state information from a HaddockInfo to a dialog settings
  * memento and vice versa.</p>
  *
  * @author Leif Frenzel
  */
class DialogSettingsUtil {

  /** section identifier constant for the dialog settings memento. */
  private static final String HADDOCK_EXPORT = "haddock";

  static void load( final IDialogSettings settings, final HaddockInfo info ) {
    IDialogSettings section = settings.getSection( HADDOCK_EXPORT );
    if( section != null ) {
      info.setUseTitle( section.getBoolean( "useTitle" ) );
      info.setTitle( section.get( "title" ) );
      
      info.setUseCssFile( section.getBoolean( "useCssFile" ) );
      info.setCssFile( section.get( "cssFile" ) );
      info.setUsePrologueFile( section.getBoolean( "usePrologueFile" ) );
      info.setPrologueFile( section.get( "prologueFile" ) );
      
      info.setExecutable( section.get( "executable" ) );
      info.setOutputDir( section.get( "outputDir" ) );
      
      info.setUseContentsURL( section.getBoolean( "useContentsURL" ) );
      info.setContentsURL( section.get( "contentsURL" ) );
      info.setUseIndexURL( section.getBoolean( "useIndexURL" ) );
      info.setIndexURL( section.get( "indexURL" ) );

      info.setDumpInterface( section.getBoolean( "dumpInterface" ) );
      info.setDumpInterfaceFile( section.get( "dumpInterfaceFile" ) );
      info.setReadInterfaces( section.getBoolean( "readInterfaces" ) );
      InterfaceList parsedList = parse( section.get( "readInterfaceFiles" ) );
      info.setReadInterfaceFiles( parsedList );
      info.setUsePackageName( section.getBoolean( "usePackageName" ) );
      info.setPackageName( section.get( "packageName" ) );
    }
  }

  static void save( final IDialogSettings settings, final HaddockInfo info ) {
    IDialogSettings section = settings.addNewSection( HADDOCK_EXPORT );
    
    section.put( "useTitle", info.isUseTitle() );
    section.put( "title", info.getTitle() );
    
    section.put( "useCssFile", info.isUseCssFile() );
    section.put( "cssFile", info.getCssFile() );
    section.put( "usePrologueFile", info.isUsePrologueFile() );
    section.put( "prologueFile", info.getPrologueFile() );
    
    section.put( "executable", info.getExecutable() );
    section.put( "outputDir", info.getOutputDir() );
    
    section.put( "useContentsURL", info.isUseContentsURL() );
    section.put( "contentsURL", info.getContentsURL() );
    section.put( "useIndexURL", info.isUseContentsURL() );
    section.put( "indexURL", info.getContentsURL() );
    
    section.put( "dumpInterface", info.isDumpInterface() );
    section.put( "dumpInterfaceFile", info.getDumpInterfaceFile() );
    section.put( "readInterfaces", info.isReadInterfaces() );
    section.put( "readInterfaceFiles", encode( info.getReadInterfaceFiles() ) );
    section.put( "usePackageName", info.isUsePackageName() );
    section.put( "packageName", info.getPackageName() );
  }
  
  
  // helping methods
  //////////////////

  private static String encode( final InterfaceList list ) {
    StringBuffer buf = new StringBuffer();
    InterfaceListEntry[] entries = list.getAll();
    for( int i = 0; i < entries.length; i++ ) {
      if( i > 0 ) {
        buf.append( File.pathSeparatorChar );
      }
      buf.append( encodeEntry( entries[ i ] ) );
    }
    return buf.toString();
  }
  
  private static String encodeEntry( final InterfaceListEntry entry ) {
    return entry.getFileName() + "," + ( entry.isUsed() ? "t" : "f" );
  }
  
  private static InterfaceList parse( final String text ) {
    InterfaceList list = new InterfaceList();
    if( text != null && !text.trim().equals( "" ) ) {
      String markedText = text.replace( File.pathSeparatorChar, ';' );
      StringTokenizer stok = new StringTokenizer( markedText, ";" );
      while( stok.hasMoreTokens() ) {
        String token = stok.nextToken();
        InterfaceListEntry entry = parseEntry( token.trim() );
        if( entry != null ) {
          list.add( entry );
        }
      }
    }
    return list;
  }

  private static InterfaceListEntry parseEntry( final String text ) {
    int commaIndex = text.lastIndexOf( ',' );
    boolean enabled = text.charAt( commaIndex + 1 ) == 't';
    String path = text.substring( 0, commaIndex );
    return new InterfaceListEntry( path, enabled );
  }  
}
