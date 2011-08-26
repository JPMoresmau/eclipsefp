package net.sf.eclipsefp.haskell.debug.core.internal.xml;

import org.eclipse.jdt.junit.model.ITestCaseElement;
import org.eclipse.jdt.junit.model.ITestElementContainer;
import org.eclipse.jdt.junit.model.ITestRunSession;
import org.jdom.Element;

/**
 * Represents a JUnit testcase, used in the presentation of
 * test-framework test resuts.
 * The code was taken from Eclipse JDT.
 * @author Alejandro Serrano
 *
 */
public class TestCase implements ITestCaseElement {

  private final ITestElementContainer parent;
  private final ITestRunSession session;
  private String name;
  private double time;
  private FailureTrace trace;

  public TestCase( final Element elt,
      final ITestElementContainer parent, final ITestRunSession session ) {
    this.parent = parent;
    this.session = session;
    initialize(elt);
  }

  private void initialize( final Element elt ) {
    this.name = elt.getAttributeValue( IXMLTags.ATTRIB_NAME, IXMLTags.DEFAULT_NAME );
    try {
      String timeVal = elt.getAttributeValue( IXMLTags.ATTRIB_TIME,
          IXMLTags.DEFAULT_TIME );
      time = Double.valueOf( timeVal );
    } catch( NumberFormatException e ) {
      time = 0;
    }

    Element failure = elt.getChild( IXMLTags.ELEMENT_FAILURE );
    if (failure == null) {
      trace = null; // The result was OK
    } else {
      trace = new FailureTrace( failure.getText(), null, null );
    }
  }

  public String getTestMethodName() {
    return name;
  }

  public Result getTestResult( final boolean includeChildren ) {
    return trace != null ? Result.FAILURE : Result.OK;
  }

  public double getElapsedTimeInSeconds() {
    return time;
  }

  public FailureTrace getFailureTrace() {
    return trace;
  }

  public ITestElementContainer getParentContainer() {
    return parent;
  }

  public ITestRunSession getTestRunSession() {
    return session;
  }

  public ProgressState getProgressState() {
    return ProgressState.COMPLETED;
  }

  public String getTestClassName() {
    return null;
  }

}
