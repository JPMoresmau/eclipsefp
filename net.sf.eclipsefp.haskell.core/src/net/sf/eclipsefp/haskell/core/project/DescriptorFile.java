// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

/** <p>contains naming constants and default XML generator functionality for
  * the <code>.hsproject</code> file that belongs to each Haskell project
  * in the workspace and provides information about path settings etc.</p> 
  * 
  * @author Leif Frenzel
  */
class DescriptorFile implements IXMLNames {

  private static final String OPEN_TAG  = "<haskellProject>";
  private static final String CLOSE_TAG = "</haskellProject>";
  private static final String XML_PREFIX 
    = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
  
  private static final String NL = "\n";
  private static final String INDENT = "  ";
  
  private String sourcePath;
  private String outputPath;
  private String binPath;
  private String targetName;

// TODO must reduce ugliness here

  DescriptorFile( final String sourcePath, 
                  final String outputPath, 
                  final String binPath,
                  final String targetName ) {
    this.sourcePath = sourcePath;
    this.outputPath = outputPath;
    this.binPath = binPath;
    this.targetName = targetName;
  }

  /** <p>returns the XML representation of this DescriptorFile object, in order
    * to write it to the .hsproject file.</p> */ 
  String toXML() {
    StringBuffer result = new StringBuffer( XML_PREFIX );
    result.append( NL );
    result.append( OPEN_TAG );
    result.append( NL );

    renderTag( result, SOURCE_PATH_ELEMENT, PATH_ATT, sourcePath );
    renderTag( result, OUTPUT_PATH_ELEMENT, PATH_ATT, outputPath );
    renderTag( result, BIN_PATH_ELEMENT, PATH_ATT, binPath );
    renderTag( result, TARGET_NAME_ELEMENT, NAME_ATT, targetName );
    
    result.append( CLOSE_TAG );
    result.append( NL );
    return result.toString();
  }

  void setSourcePath( final String sourcePath ) {
    this.sourcePath = sourcePath;
  }

  void setOutputPath( final String outputPath ) {
    this.outputPath = outputPath;
  }

  void setBinPath( final String binPath ) {
    this.binPath = binPath;
  }
  
  void setTargetName( final String targetName ) {
    this.targetName = targetName;
  }
  
  static String createDescriptorContent( final String sourcePath,
                                         final String outputPath,
                                         final String binPath,
                                         final String targetName ) {
    return new DescriptorFile( sourcePath, 
                               outputPath, 
                               binPath, 
                               targetName ).toXML();
  }  

  static String createEmptyDescriptorContent() {
    return createDescriptorContent( "", "", "", "" );
  }


  // helping methods
  //////////////////
  
  private void renderTag( final StringBuffer result, 
                          final String tagName,
                          final String attName, 
                          final String value ) {
    result.append( INDENT );
    result.append( "<" );
    result.append( tagName );
    result.append( " " );
    result.append( attName );
    result.append( "=\"" );
    result.append( value );
    result.append( "\"/>" );
    result.append( NL );
  }
}