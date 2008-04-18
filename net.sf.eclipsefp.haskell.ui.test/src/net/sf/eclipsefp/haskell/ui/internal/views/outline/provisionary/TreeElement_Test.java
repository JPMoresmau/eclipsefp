// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.outline.provisionary;

import java.util.List;
import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;

/** tests marshalling/unmarshalling tree elements
  *
  * @author Leif Frenzel
  */
public class TreeElement_Test extends TestCase {

  // protocol: data TreeElement = TreeElement Int String String
  // level - text - image key
  // top-level elements are on level 0

  public void testUnmarshal_empty() {
    assertTrue( TreeElement.unmarshal( null ).isEmpty() );
    assertTrue( TreeElement.unmarshal( new String[ 0 ] ).isEmpty() );
    assertTrue( TreeElement.unmarshal( new String[] { "" } ).isEmpty() );
  }

  public void testUnmarshal_singleIncomplete() {
    String[] content = new String[] { "0", "t" };
    List<ITreeElement> retVal = TreeElement.unmarshal( content );
    assertEquals( 0, retVal.size() );
  }

  public void testUnmarshal_single() {
    String[] content = new String[] { "0", "t", "i" };
    List<ITreeElement> retVal = TreeElement.unmarshal( content );
    assertEquals( 1, retVal.size() );
    assertEquals( 0, retVal.get( 0 ).getChildren().size() );
    assertNull( retVal.get( 0 ).getParent() );
    assertEquals( "t", retVal.get( 0 ).getText() );
    assertEquals( "i", retVal.get( 0 ).getImageKey() );
  }

  public void testUnmarshal_multipleIncomplete() {
    String[] content = new String[] { "0", "t1", "i1", "0", "t2", "i2", "1" };
    List<ITreeElement> retVal = TreeElement.unmarshal( content );
    assertEquals( 2, retVal.size() );

    assertEquals( 0, retVal.get( 0 ).getChildren().size() );
    assertNull( retVal.get( 0 ).getParent() );
    assertEquals( "t1", retVal.get( 0 ).getText() );
    assertEquals( "i1", retVal.get( 0 ).getImageKey() );

    assertEquals( 0, retVal.get( 1 ).getChildren().size() );
    assertNull( retVal.get( 1 ).getParent() );
    assertEquals( "t2", retVal.get( 1 ).getText() );
    assertEquals( "i2", retVal.get( 1 ).getImageKey() );
  }

  public void testUnmarshal_multiple() {
    String[] content = new String[] { "0", "t1", "i1", "0", "t2", "i2" };
    List<ITreeElement> retVal = TreeElement.unmarshal( content );
    assertEquals( 2, retVal.size() );

    assertEquals( 0, retVal.get( 0 ).getChildren().size() );
    assertNull( retVal.get( 0 ).getParent() );
    assertEquals( "t1", retVal.get( 0 ).getText() );
    assertEquals( "i1", retVal.get( 0 ).getImageKey() );

    assertEquals( 0, retVal.get( 1 ).getChildren().size() );
    assertNull( retVal.get( 1 ).getParent() );
    assertEquals( "t2", retVal.get( 1 ).getText() );
    assertEquals( "i2", retVal.get( 1 ).getImageKey() );
  }

  public void testUnmarshal_simpleNested() {
    String[] content = new String[] { "0", "t1", "i1", "1", "t2", "i2" };
    List<ITreeElement> retVal = TreeElement.unmarshal( content );
    assertEquals( 2, retVal.size() );

    assertEquals( 1, retVal.get( 0 ).getChildren().size() );
    assertNull( retVal.get( 0 ).getParent() );
    assertEquals( "t1", retVal.get( 0 ).getText() );
    assertEquals( "i1", retVal.get( 0 ).getImageKey() );

    assertEquals( 0, retVal.get( 1 ).getChildren().size() );
    assertEquals( retVal.get( 0 ), retVal.get( 1 ).getParent() );
    assertEquals( "t2", retVal.get( 1 ).getText() );
    assertEquals( "i2", retVal.get( 1 ).getImageKey() );
  }

  public void testUnmarshal_MultipleNested() {
    String[] content = new String[] {
      "0", "", "",
      "1", "", "",
      "0", "", "",
      "1", "", "",
      "1", "", "",
      "2", "", "",
      "0", "", ""
    };
    List<ITreeElement> retVal = TreeElement.unmarshal( content );
    assertEquals( 7, retVal.size() );

    assertEquals( null, retVal.get( 0 ).getParent() );
    assertEquals( retVal.get( 0 ), retVal.get( 1 ).getParent() );
    assertEquals( null, retVal.get( 2 ).getParent() );
    assertEquals( retVal.get( 2 ), retVal.get( 3 ).getParent() );
    assertEquals( retVal.get( 2 ), retVal.get( 4 ).getParent() );
    assertEquals( retVal.get( 4 ), retVal.get( 5 ).getParent() );
    assertEquals( null, retVal.get( 6 ).getParent() );

    assertEquals( 1, retVal.get( 0 ).getChildren().size() );
    assertEquals( 0, retVal.get( 1 ).getChildren().size() );
    assertEquals( 2, retVal.get( 2 ).getChildren().size() );
    assertEquals( 0, retVal.get( 3 ).getChildren().size() );
    assertEquals( 1, retVal.get( 4 ).getChildren().size() );
    assertEquals( 0, retVal.get( 5 ).getChildren().size() );
    assertEquals( 0, retVal.get( 6 ).getChildren().size() );
  }
}
