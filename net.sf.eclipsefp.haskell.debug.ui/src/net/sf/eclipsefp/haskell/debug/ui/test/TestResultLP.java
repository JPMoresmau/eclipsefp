/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.test;

import net.sf.eclipsefp.haskell.debug.core.test.TestResult;
import net.sf.eclipsefp.haskell.debug.core.test.TestSuite;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;


/**
 * Label provider for the test tree UI
 * @author JP Moresmau
 *
 */
public class TestResultLP extends LabelProvider {

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( final Object element ) {
    if (element instanceof TestResult){
      TestResult tr=(TestResult)element;
      String key=null;
      switch (tr.getStatus()){
        case ERROR:
            key=IImageNames.TEST_ERR;
            break;
        case FAILURE:
            key=IImageNames.TEST_FAIL;
            break;
        case OK:
          key=IImageNames.TEST_OK;
          break;
        case PENDING:
          key=IImageNames.TEST;
          break;
        case RUNNING:
          key=IImageNames.TEST_RUN;
          break;
      }
      if (key!=null){
        return HaskellUIImages.getImage( key );
      }
    } else if (element instanceof TestSuite){
      return getImage(( (TestSuite )element).getRoot());
    }
    return super.getImage( element );
  }

}
