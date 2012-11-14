/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.test;

import java.util.Collection;
import net.sf.eclipsefp.haskell.debug.core.test.TestResult;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;


/**
 * Content provider for the test UI tree
 * @author JP Moresmau
 *
 */
public class TestResultCP implements ITreeContentProvider {
  private final Object[] empty=new Object[0];
  private Object[] tests=empty;
  private Object input;

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  @Override
  public void dispose() {
    tests=empty;
    input=null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  @Override
  public void inputChanged( final Viewer paramViewer, final Object paramObject1,
      final Object paramObject2 ) {
    input=paramObject2;
    if (paramObject2 instanceof Collection<?>){
      tests=((Collection<?>)paramObject2).toArray();
    } else {
      tests=empty;
    }

  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getElements(java.lang.Object)
   */
  @Override
  public Object[] getElements( final Object paramObject ) {
    if ((paramObject instanceof Collection<?>)){
      return tests;
    }
    return empty;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object paramObject ) {
    if ((paramObject instanceof Collection<?>)){
      return tests;
    } else if (paramObject instanceof TestResult){
      return ((TestResult)paramObject).getChildren().toArray();
    }
    return empty;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object paramObject ) {
   if ((paramObject instanceof Collection<?>)){
     return null;
   }
   return input;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  @Override
  public boolean hasChildren( final Object paramObject ) {
    if ((paramObject instanceof Collection<?>)){
      return ((Collection<?>)paramObject).size()>0;
    } else if (paramObject instanceof TestResult){
      return ((TestResult)paramObject).getChildren().size()>0;
    }
    return false;
  }

}
