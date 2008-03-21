package net.sf.eclipsefp.haskell.core.internal.project.provisionary;

import java.util.List;
import junit.framework.TestCase;
import org.eclipse.core.resources.IMarker;


public class MarkerDesc_Test extends TestCase {

  public void testUnMarshal_robustness() {
    assertEquals( 0, MarkerDesc.unmarshal( null ).size() );
    assertEquals( 0, MarkerDesc.unmarshal( new String[ 0 ] ).size() );
    assertEquals( 0, MarkerDesc.unmarshal( new String[] { "" } ).size() );
  }

  public void testUnmarshal_singleCompleteMarker() {
    String[] args = new String[] { "/a", "1", "Bla", "-1", "-1", "Error" };
    List<MarkerDesc> result = MarkerDesc.unmarshal( args );
    assertEquals( 1, result.size() );
    assertEquals( "/a", result.get( 0 ).getFileName() );
    assertEquals( 1, result.get( 0 ).getLine() );
    assertEquals( "Bla", result.get( 0 ).getMessage() );
    assertEquals( MarkerDesc.UNSPECIFIED, result.get( 0 ).getCharStart() );
    assertEquals( MarkerDesc.UNSPECIFIED, result.get( 0 ).getCharEnd() );
    assertEquals( IMarker.SEVERITY_ERROR, result.get( 0 ).getSeverity() );
  }

  public void testUnmarshal_incompleteMarker() {
    String[] args = new String[] {
      "/a", "1", "Bla", "-1", "1", "Error",
      "/a", "1", "Bla", "3"
    };
    List<MarkerDesc> result = MarkerDesc.unmarshal( args );
    assertEquals( 2, result.size() );
    assertEquals( MarkerDesc.UNSPECIFIED, result.get( 0 ).getCharStart() );
    assertEquals( 1, result.get( 0 ).getCharEnd() );
    assertEquals( 3, result.get( 1 ).getCharStart() );
    assertEquals( MarkerDesc.UNSPECIFIED, result.get( 1 ).getCharEnd() );
  }

  public void testUnmarshal_multipleCompleteMarkers() {
    String[] args = new String[] {
        "/a", "1", "Bli", "-1", "-1", "Info",
        "/a", "2", "Bla", "-1", "-1", "Warning",
        "/a", "3", "Blubb", "-1", "-1", "Error"
    };
    List<MarkerDesc> result = MarkerDesc.unmarshal( args );
    assertEquals( 3, result.size() );
    assertEquals( IMarker.SEVERITY_INFO, result.get( 0 ).getSeverity() );
    assertEquals( IMarker.SEVERITY_WARNING, result.get( 1 ).getSeverity() );
    assertEquals( IMarker.SEVERITY_ERROR, result.get( 2 ).getSeverity() );
  }
}
