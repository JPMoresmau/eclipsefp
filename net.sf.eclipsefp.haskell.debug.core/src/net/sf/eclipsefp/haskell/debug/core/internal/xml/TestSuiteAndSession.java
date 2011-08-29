package net.sf.eclipsefp.haskell.debug.core.internal.xml;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.junit.model.ITestElement;
import org.eclipse.jdt.junit.model.ITestElementContainer;
import org.eclipse.jdt.junit.model.ITestRunSession;
import org.eclipse.jdt.junit.model.ITestSuiteElement;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;

/**
 * Represents a group or entire session in a JUnit file,
 * used here to represent test-framework tests.
 * This file is derived from the work in Eclipse JDT.
 * @author Alejandro Serrano
 *
 */
public class TestSuiteAndSession implements ITestRunSession, ITestSuiteElement {

  private final ITestElementContainer parent;
  private final ITestRunSession session;
  private String name;
  private ArrayList<ITestElement> children;
  private double time;

  public static TestSuiteAndSession parseFile( final File file ) {
    try {
      SAXBuilder parser = new SAXBuilder();
      Document doc = parser.build( file );
      Element root = doc.getRootElement();

      return new TestSuiteAndSession( root );
    } catch( Exception e ) {
      return new TestSuiteAndSession();
    }
  }

  public TestSuiteAndSession() {
    this.parent = null;
    this.session = this;
  }

  public TestSuiteAndSession( final Element elt ) {
    this.parent = null;
    this.session = this;
    initialize( elt );
  }

  public TestSuiteAndSession( final Element elt,
      final ITestElementContainer parent, final ITestRunSession session ) {
    this.parent = parent;
    this.session = session;
    initialize( elt );
  }

  private void initialize( final Element elt ) {
    this.children = new ArrayList<ITestElement>();
    this.name = elt.getAttributeValue( IXMLTags.ATTRIB_NAME,
        IXMLTags.DEFAULT_NAME );
    try {
      String timeVal = elt.getAttributeValue( IXMLTags.ATTRIB_TIME,
          IXMLTags.DEFAULT_TIME );
      time = Double.parseDouble( timeVal );
    } catch( NumberFormatException e ) {
      time = 0;
    }

    for( Element e: ( List<Element> )elt.getChildren() ) {
      if( e.getName().equals( IXMLTags.ELEMENT_TESTSUITE ) ) {
        children.add( new TestSuiteAndSession( e, this, session ) );
      } else if( e.getName().equals( IXMLTags.ELEMENT_TESTCASE ) ) {
        children.add( new TestCase( e, this, session ) );
      }
    }
  }

  public ITestElement[] getChildren() {
    return children.toArray( new ITestElement[ children.size() ] );
  }

  public String getTestRunName() {
    return name;
  }

  public String getSuiteTypeName() {
    return name;
  }

  public double getElapsedTimeInSeconds() {
    return time;
  }

  public Result getTestResult( final boolean includeChildren ) {
    if( !includeChildren ) {
      return Result.OK;
    }

    Result result = Result.OK;
    for( ITestElement child: children ) {
      if( child.getTestResult( true ).equals( Result.OK ) ) {
        result = Result.FAILURE;
        break;
      }
    }
    return result;
  }

  public ITestElementContainer getParentContainer() {
    return parent;
  }

  public ITestRunSession getTestRunSession() {
    return session;
  }

  public FailureTrace getFailureTrace() {
    return null;
  }

  public ProgressState getProgressState() {
    return ProgressState.COMPLETED;
  }

  public IJavaProject getLaunchedProject() {
    return null;
  }

}
