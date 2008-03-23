// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

/**
 * <p>
 * contains naming constants and default XML generator functionality for the
 * <code>.hsproject</code> file that belongs to each Haskell project in the
 * workspace and provides information about path settings etc.
 * </p>
 *
 * @author Leif Frenzel
 */
public class DescriptorFile implements IXMLNames {

	private static final String OPEN_TAG = "<haskellProject>"; //$NON-NLS-1$

	private static final String CLOSE_TAG = "</haskellProject>"; //$NON-NLS-1$

	private static final String XML_PREFIX = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"; //$NON-NLS-1$

	private static final String NL = "\n"; //$NON-NLS-1$

	private static final String INDENT = "  "; //$NON-NLS-1$

	private String fSourcePath;
	private String fOutputPath;
	private String fBinPath;
	private String fTargetName;
	private final String fCompiler;


	DescriptorFile(final String sourcePath, final String outputPath,
			final String binPath, final String targetName,
			final String compiler)
	{
		fSourcePath = sourcePath;
		fOutputPath = outputPath;
		fBinPath = binPath;
		fTargetName = targetName;
		fCompiler = compiler;
	}

	/**
	 * <p>
	 * returns the XML representation of this DescriptorFile object, in order to
	 * write it to the .hsproject file.
	 * </p>
	 */
	String toXML() {
    StringBuffer result = new StringBuffer( XML_PREFIX );
    result.append( NL );
    result.append( OPEN_TAG );
    result.append( NL );

    renderTag( result, SOURCE_PATH_ELEMENT, PATH_ATT, fSourcePath );
    renderTag( result, OUTPUT_PATH_ELEMENT, PATH_ATT, fOutputPath );
    renderTag( result, BIN_PATH_ELEMENT, PATH_ATT, fBinPath );
    renderTag( result, TARGET_NAME_ELEMENT, NAME_ATT, fTargetName );
    result.append( "<" ); //$NON-NLS-1$
    result.append( COMPILER_ELEMENT );
    result.append( ">" ); //$NON-NLS-1$
    result.append( fCompiler );
    result.append( "</" ); //$NON-NLS-1$
    result.append( COMPILER_ELEMENT );
    result.append( ">" ); //$NON-NLS-1$

    result.append( CLOSE_TAG );
    result.append( NL );
    return result.toString();
  }

  void setSourcePath( final String sourcePath ) {
    fSourcePath = sourcePath;
  }

  void setOutputPath( final String outputPath ) {
    fOutputPath = outputPath;
  }

  void setBinPath( final String binPath ) {
    fBinPath = binPath;
  }

  void setTargetName( final String targetName ) {
    fTargetName = targetName;
  }

  public static String createDescriptorContent( final String sourcePath,
      final String outputPath, final String binPath, final String targetName,
      final String compiler ) {
    return new DescriptorFile( sourcePath, outputPath, binPath, targetName,
        compiler ).toXML();
  }

	public static String createEmptyDescriptorContent() {
    return createDescriptorContent( "", "", "", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
  }

	// helping methods
	// ////////////////

	private void renderTag( final StringBuffer result, final String tagName,
      final String attName, final String value ) {
    result.append( INDENT );
    result.append( "<" ); //$NON-NLS-1$
    result.append( tagName );
    result.append( " " ); //$NON-NLS-1$
    result.append( attName );
    result.append( "=\"" ); //$NON-NLS-1$
    result.append( value );
    result.append( "\"/>" ); //$NON-NLS-1$
    result.append( NL );
  }
}