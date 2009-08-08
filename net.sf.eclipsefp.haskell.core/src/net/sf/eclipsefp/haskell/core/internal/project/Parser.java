// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

import java.io.InputStreamReader;
import java.io.Reader;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 * <p>
 * A mini parser to get the project descriptor information from the .hsproject
 * file into an IHaskellProject.
 * </p>
 *
 * @author Leif Frenzel
 */
public class Parser implements IXMLNames {

  private final IFile projectDescriptor;
  private final HaskellProject project;

  public Parser(final IFile projectDescriptor, final HaskellProject project) {
    this.projectDescriptor = projectDescriptor;
    this.project = project;
  }

	public void read()
	{
		try {
			Reader reader = new InputStreamReader(projectDescriptor
					.getContents());
			Element rootElement = null;
			try {
				DocumentBuilder parser = DocumentBuilderFactory.newInstance()
						.newDocumentBuilder();
				InputSource inputSource = new InputSource(reader);
				rootElement = parser.parse(inputSource).getDocumentElement();
			} catch( Exception ex ) {
        String msg = "Problem when parsing .hsproject file.\n"; //$NON-NLS-1$
        HaskellCorePlugin.log( msg, ex );
			} finally {
				reader.close();
			}
	    if (rootElement != null && rootElement.getNodeName().equals(DOCUMENT_ELEMENT)) {
	      applyProjectInfo(rootElement);
	      // TODO TtC replace by BuildConfigurations
	      // setupCompiler(rootElement);
	    }
		} catch( Exception ex ) {
      String msg = "Problem when reading .hsproject file.\n"; //$NON-NLS-1$
      HaskellCorePlugin.log( msg, ex );
    }
	}

	// helping methods
	// ////////////////

  private void setupCompiler(final Element rootElement)
  {
    final NodeList list = rootElement.getElementsByTagName(COMPILER_ELEMENT);
    if (list.getLength() > 0) {
      if ("null".equals(list.item(0).getTextContent())) { //$NON-NLS-1$
        project.setCompiler(null);
      }
    }
  }

  private void applyProjectInfo(final Element rootElement)
  {
    NodeList list = rootElement.getElementsByTagName( CABAL_PROJECT_ELEMENT );
    if (list.getLength() > 0) {
      Node projectInfo = list.item( 0 );
      applyCabalProjectInfo((Element)projectInfo);
    }
    list = rootElement.getElementsByTagName( SIMPLE_PROJECT_ELEMENT );
    if (list.getLength() > 0) {
      // we use only the first item
      Node projectInfo = list.item( 0 );
      applySimpleProjectInfo((Element)projectInfo);
      return;
    }
  }

  private void applyCabalProjectInfo(final Element cabalProjectElement) {
    IPath path = Path.fromPortableString( cabalProjectElement.getTextContent() );
    project.setCabalFile( project.getResource().getFile( path ) );
  }

  private void applySimpleProjectInfo(final Element projectInfoElement) {
    NodeList children = projectInfoElement.getChildNodes();
    for (int i = 0; i < children.getLength(); ++i) {
      if (children.item( i ).getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element)children.item( i );
        String name = el.getNodeName();
        if (name.equals( SOURCE_PATHS_ELEMENT )) {
          applySourcePaths(el);
        } else if (name.equals( OUTPUT_PATH_ELEMENT )) {
          applyOutputPath(el);
        } else if (name.equals( BIN_PATH_ELEMENT )) {
          applyBinPath(el);
        } else if (name.equals( TARGETS_ELEMENT )) {
          applyTargets(el);
        }
      }
    }
  }

  private void applySourcePaths(final Element sourcePathsElement) {
    NodeList paths = sourcePathsElement.getElementsByTagName( PATH_ELEMENT );
    for (int i = 0; i < paths.getLength(); ++i) {
      Element path = (Element) paths.item( i );
      project.addSourcePath( Path.fromPortableString( path.getTextContent() ) );
    }
  }

  private void applyOutputPath(final Element outputPathElement) {
    project.setOutputPath( Path.fromPortableString( outputPathElement.getTextContent() ) );
  }

  private void applyBinPath(final Element binPathElement) {
    project.setBuildPath( Path.fromPortableString( binPathElement.getTextContent() ) );
  }

  private void applyTargets(final Element targetsElement) {
    NodeList targets = targetsElement.getChildNodes();
    for (int i = 0; i < targets.getLength(); ++i) {
      if (targets.item(i).getNodeType() == Node.ELEMENT_NODE) {
        Element target = (Element) targets.item( i );
        String name = target.getNodeName();
        if (name.equals(EXECUTABLE_TARGET_ELEMENT)) {
          applyExecutableTarget(target);
        }
      }
    }
  }

  private void applyExecutableTarget(final Element executableTargetElement) {
    String path = getValue(executableTargetElement, PATH_ELEMENT);
    ExecutableBuildTarget target = new ExecutableBuildTarget( Path.fromPortableString( path ) );
    project.addTarget( target );
  }

  private String getValue(final Element element, final String subElementName) {
      NodeList list = element.getElementsByTagName(subElementName);
      if (list.getLength() > 0) {
        Element subElement = (Element) list.item( 0 );
        return subElement.getTextContent();
      }
      return ""; //$NON-NLS-1$
    }

}