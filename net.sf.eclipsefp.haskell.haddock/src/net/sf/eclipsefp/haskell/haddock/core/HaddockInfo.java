// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.core;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.haddock.HaddockPlugin;
import net.sf.eclipsefp.haskell.haddock.core.preferences.IHaddockPreferenceNames;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Platform;

/** <p>encapsulates all info needed for a particular Haddock run.</p>
  *
  * @author Leif Frenzel
  */
public class HaddockInfo implements IHaddockParameters {

  private IProject[] projects;
  private InterfaceList readInterfaceFiles = new InterfaceList();

  private String[] fileList = new String[ 0 ];
  private String outputDir = "";
  private String executable = readExecutableFromPreferences();

  private boolean useTitle = false;
  private String title = "";
  private boolean useCssFile = false;
  private String cssFile = "";
  private boolean usePrologueFile = false;
  private String prologueFile = "";
  private boolean usePackageName = false;
  private String packageName = "";
  private boolean dumpInterface = false;
  private String dumpInterfaceFile = "";
  private boolean readInterfaces = false;
  private boolean useIndexURL = false;
  private String indexURL = "";
  private boolean useContentsURL = false;
  private String contentsURL = "";
  private boolean generateHTMLHelpFiles = false;


  /** <p>creates the Haddock command with the information contained in this
    * <code>HaddockInfo</code>.</p> */
  public List<String> createCommand() {
    List<String> list = new ArrayList<String>();
    list.add( executable );
    list.add( FORMAT_HTML ); // this is the only format currently supported
    list.add( OH_DEAR );
    list.add( outputDir );

    addParam( useTitle, TITLE, title, list );
    addParam( usePrologueFile, PROLOGUE, prologueFile, list );
    addParam( useCssFile, CSS_FILE, cssFile, list );
    addParam( usePackageName, PACKAGE_NAME, packageName, list );
    addParam( dumpInterface, DUMP_HADDOCK_INTERFACE, dumpInterfaceFile, list );
    addReadInterfaces( list );
    addParam( useIndexURL, USE_INDEX, indexURL, list );
    addParam( useContentsURL, USE_CONTENTS, contentsURL, list );

    // the source files to run Haddock over
    for( int i = 0; i < fileList.length; i++ ) {
      list.add( fileList[ i ] );
    }
    return list;
  }


  // attribute setters and getters
  ////////////////////////////////

  public String getOutputDir() {
    return outputDir;
  }

  public void setOutputDir( final String outputDir ) {
    this.outputDir = ( outputDir == null ) ? "" : outputDir;
  }

  public IProject[] getProjects() {
    return projects;
  }

  public void setProjects( final ArrayList<IProject> projects ) {
    if (projects != null) {
      this.projects = new IProject[projects.size()];
      projects.toArray(this.projects);
    } else {
      this.projects = null;
    }
  }

  public String getTitle() {
    return title;
  }

  public void setTitle( final String title ) {
    this.title = ( title == null ) ? "" : title;
  }

  public boolean isUseTitle() {
    return useTitle;
  }

  public void setUseTitle( final boolean useTitle ) {
    this.useTitle = useTitle;
  }

  public String[] getFileList() {
    return fileList;
  }

  public void setFileList( final String[] fileList ) {
    this.fileList = ( fileList == null ) ? new String [ 0 ] : fileList;
  }

  public String getCssFile() {
    return cssFile;
  }

  public void setCssFile( final String cssFile ) {
    this.cssFile = ( cssFile == null ) ? "" : cssFile;
  }

  public String getPrologueFile() {
    return prologueFile;
  }

  public void setPrologueFile( final String prologueFile ) {
    this.prologueFile = ( prologueFile == null ) ? "" : prologueFile;
  }

  public boolean isUseCssFile() {
    return useCssFile;
  }

  public void setUseCssFile( final boolean useCSSFile ) {
    this.useCssFile = useCSSFile;
  }

  public boolean isUsePrologueFile() {
    return usePrologueFile;
  }

  public void setUsePrologueFile( final boolean usePrologueFile ) {
    this.usePrologueFile = usePrologueFile;
  }

  public String getExecutable() {
    return executable;
  }

  public void setExecutable( final String executable ) {
    this.executable = ( executable == null ) ? "" : executable;
  }

  public String getPackageName() {
    return packageName;
  }

  public void setPackageName( final String packageName ) {
    this.packageName = ( packageName == null ) ? "" : packageName;
  }

  public boolean isUsePackageName() {
    return usePackageName;
  }

  public void setUsePackageName( final boolean usePackageName ) {
    this.usePackageName = usePackageName;
  }

  public boolean isDumpInterface() {
    return dumpInterface;
  }

  public void setDumpInterface( final boolean dumpInterface ) {
    this.dumpInterface = dumpInterface;
  }

  public String getDumpInterfaceFile() {
    return dumpInterfaceFile;
  }

  public void setDumpInterfaceFile( final String dumpInterfaceFile ) {
    this.dumpInterfaceFile = ( dumpInterfaceFile == null ) ? ""
                                                           : dumpInterfaceFile;
  }

  public boolean isReadInterfaces() {
    return readInterfaces;
  }

  public void setReadInterfaces( final boolean readInterfaces ) {
    this.readInterfaces = readInterfaces;
  }

  public String getContentsURL() {
    return contentsURL;
  }

  public void setContentsURL( final String contentsURL ) {
    this.contentsURL = ( contentsURL == null ) ? "" : contentsURL;
  }

  public String getIndexURL() {
    return indexURL;
  }

  public void setIndexURL( final String indexURL ) {
    this.indexURL = ( indexURL == null ) ? "" : indexURL;
  }

  public boolean isUseContentsURL() {
    return useContentsURL;
  }

  public void setUseContentsURL( final boolean useContentsURL ) {
    this.useContentsURL = useContentsURL;
  }

  public boolean isUseIndexURL() {
    return useIndexURL;
  }

  public void setUseIndexURL( final boolean useIndexURL ) {
    this.useIndexURL = useIndexURL;
  }

  public boolean isGenerateHTMLHelpFiles() {
    return generateHTMLHelpFiles;
  }

  public void setGenerateHTMLHelpFiles( final boolean generateHTMLHelpFiles ) {
    this.generateHTMLHelpFiles = generateHTMLHelpFiles;
  }

  public InterfaceList getReadInterfaceFiles() {
    return readInterfaceFiles;
  }

  public void setReadInterfaceFiles( final InterfaceList readInterfaceFiles ) {
    this.readInterfaceFiles = readInterfaceFiles;
  }


  // helping methods
  //////////////////

  private void addParam( final boolean selected,
                         final String name,
                         final String value,
                         final List<String> list ) {
    if( selected && value != null && !value.trim().equals( "" ) ) {
      list.add( name );
      list.add( value );
    }
  }

  private void addReadInterfaces( final List<String> list ) {
    InterfaceListEntry[] entries = readInterfaceFiles.getAll();
    for( int i = 0; i < entries.length; i++ ) {
      if( entries[ i ].isUsed() ) {
        list.add( READ_HADDOCK_INTERFACE );
        list.add( entries[ i ].getFileName() );
      }
    }
  }

  private String readExecutableFromPreferences() {
    return Platform.getPreferencesService().getString( HaddockPlugin.getPluginId(), IHaddockPreferenceNames.EXECUTABLE_NAME, null, null );
  }
}
