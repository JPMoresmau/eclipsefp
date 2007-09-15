// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

/**
 * <p>
 * contains naming constants and default XML generator functionality for the
 * <code>.hsproject</code> file that belongs to each Haskell project in the
 * workspace and provides information about path settings etc.
 * </p>
 * 
 * @author Leif Frenzel
 */
class DescriptorFile implements IXMLNames {

	private static final String OPEN_TAG = "<haskellProject>";

	private static final String CLOSE_TAG = "</haskellProject>";

	private static final String XML_PREFIX = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";

	private static final String NL = "\n";

	private static final String INDENT = "  ";

	private String fSourcePath;
	private String fOutputPath;
	private String fBinPath;
	private String fTargetName;
	private final String fCompiler;

	// TODO must reduce ugliness here

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
		StringBuffer result = new StringBuffer(XML_PREFIX);
		result.append(NL);
		result.append(OPEN_TAG);
		result.append(NL);

		renderTag(result, SOURCE_PATH_ELEMENT, PATH_ATT, fSourcePath);
		renderTag(result, OUTPUT_PATH_ELEMENT, PATH_ATT, fOutputPath);
		renderTag(result, BIN_PATH_ELEMENT, PATH_ATT, fBinPath);
		renderTag(result, TARGET_NAME_ELEMENT, NAME_ATT, fTargetName);
		result.append("<");
		result.append(COMPILER_ELEMENT);
		result.append(">");
		result.append(fCompiler);
		result.append("</");
		result.append(COMPILER_ELEMENT);
		result.append(">");

		result.append(CLOSE_TAG);
		result.append(NL);
		return result.toString();
	}

	void setSourcePath(final String sourcePath) {
		fSourcePath = sourcePath;
	}

	void setOutputPath(final String outputPath) {
		fOutputPath = outputPath;
	}

	void setBinPath(final String binPath) {
		fBinPath = binPath;
	}

	void setTargetName(final String targetName) {
		fTargetName = targetName;
	}

	static String createDescriptorContent(final String sourcePath,
		final String outputPath,
		final String binPath,
		final String targetName,
		final String compiler)
	{
		return new DescriptorFile(sourcePath, outputPath, binPath,
		                          targetName, compiler).toXML();
	}

	static String createEmptyDescriptorContent() {
		return createDescriptorContent("", "", "", "", "");
	}

	// helping methods
	// ////////////////

	private void renderTag(final StringBuffer result,
		final String tagName,
		final String attName,
		final String value)
	{
		result.append(INDENT);
		result.append("<");
		result.append(tagName);
		result.append(" ");
		result.append(attName);
		result.append("=\"");
		result.append(value);
		result.append("\"/>");
		result.append(NL);
	}
}