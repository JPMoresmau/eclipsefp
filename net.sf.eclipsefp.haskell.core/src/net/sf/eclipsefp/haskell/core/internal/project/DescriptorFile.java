// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

import java.io.StringWriter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.IBuildTarget;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import org.eclipse.core.runtime.IPath;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * <p>
 * contains naming constants and default XML generator functionality for the
 * <code>.hsproject</code> file that belongs to each Haskell project in the
 * workspace and provides information about path settings etc.
 * </p>
 *
 * @author Leif Frenzel
 * @author Thomas ten Cate
 */
public class DescriptorFile implements IXMLNames {

	private final IHaskellProject project;

	private Document document; // used while building

	public DescriptorFile(final IHaskellProject haskellProject) {
		project = haskellProject;
	}

	/**
	 * <p>
	 * returns the XML representation of this DescriptorFile object, in order to
	 * write it to the .hsproject file.
	 * </p>
	 */
	public String toXMLString() {
	  DOMSource source = new DOMSource(toXML());
	  StringWriter stringWriter = new StringWriter();
    StreamResult result = new StreamResult(stringWriter);
	  try {
      TransformerFactory.newInstance().newTransformer().transform( source, result );
    } catch( Exception ex ) {
      HaskellCorePlugin.log( ex );
      return null;
    }
	  return stringWriter.toString();
  }

	public Document toXML() {
	  DocumentBuilder documentBuilder;
    try {
      documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
    } catch( ParserConfigurationException e ) {
      HaskellCorePlugin.log( e );
      return null;
    }

    document = documentBuilder.newDocument();
    Element root = document.createElement( DOCUMENT_ELEMENT );
    document.appendChild( root );

    if (project.getCabalFile() != null) {
      createCabalProjectElement(root);
    } else {
      createSimpleProjectElement(root);
    }

    // TODO TtC replace this by Build Configurations
//    Element compilerElement = addChild( root, COMPILER_ELEMENT );
//    compilerElement.setTextContent( CompilerManager.getInstance().getCompilerName( project.getCompiler().getClass().getName() ) );

    return document;
	}

	protected void createCabalProjectElement(final Element documentElement) {
	  Element cabalProjectElement = addChild( documentElement, CABAL_PROJECT_ELEMENT );
	  cabalProjectElement.setTextContent( project.getCabalFile().getProjectRelativePath().toPortableString() );
	}

	protected void createSimpleProjectElement(final Element documentElement) {
	  Element simpleProjectElement = addChild( documentElement, SIMPLE_PROJECT_ELEMENT );

	  Element sourcePathsElement = addChild( simpleProjectElement, SOURCE_PATHS_ELEMENT );
    for (IPath path : project.getSourcePaths()) {
      Element pathElement = addChild( sourcePathsElement, PATH_ELEMENT);
      pathElement.setTextContent( path.toPortableString() );
    }

    Element binPathElement = addChild( simpleProjectElement, BIN_PATH_ELEMENT );
    binPathElement.setTextContent( project.getBuildPath().toPortableString() );

    Element outputPathElement = addChild( simpleProjectElement, OUTPUT_PATH_ELEMENT );
    outputPathElement.setTextContent( project.getOutputPath().toPortableString() );

    Element targetsElement = addChild( simpleProjectElement, TARGETS_ELEMENT );
    for (IBuildTarget target : project.getTargets()) {
      if (target instanceof IExecutableBuildTarget) {
        IExecutableBuildTarget executableTarget = (IExecutableBuildTarget)target;
        Element executableTargetElement = addChild(targetsElement, EXECUTABLE_TARGET_ELEMENT);
        addChild( executableTargetElement, PATH_ELEMENT ).setTextContent( executableTarget.getPath().toPortableString() );
        addChild( executableTargetElement, MAIN_ELEMENT ).setTextContent( executableTarget.getMain() );
      } else {
        // TODO TtC libraries
      }
    }
	}

	protected Element addChild( final Element parent, final String childName ) {
	  Element child = document.createElement( childName );
	  parent.appendChild( child );
	  return child;
	}

}