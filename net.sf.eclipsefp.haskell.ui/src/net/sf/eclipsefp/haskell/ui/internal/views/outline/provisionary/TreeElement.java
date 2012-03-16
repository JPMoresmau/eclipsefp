// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.outline.provisionary;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;
import net.sf.eclipsefp.haskell.core.internal.project.provisionary.ICohatoeData;
import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;


public class TreeElement implements ITreeElement, ICohatoeData {

  private final List<ITreeElement> children = new ArrayList<ITreeElement>();
  private final String imageKey;
  private Object parent;
  private final String text;


  private TreeElement( final String text, final String imageKey ) {
    this.text = text;
    this.imageKey = imageKey;
  }

  private void setParent( final ITreeElement parent ) {
    this.parent = parent;
  }

  private void addChild( final ITreeElement child ) {
    children.add( child );
  }

  public static List<ITreeElement> unmarshal( final String[] args ) {
    List<ITreeElement> result;
    if( args == null ) {
      result = Collections.emptyList();
    } else {
      result = convert( args );
    }
    return result;
  }


  // interface methods of ICohatoeData
  ////////////////////////////////////

  @Override
  public List<String> marshal() {
    // if you need it, write a test case and implement it
    throw new UnsupportedOperationException();
  }


  // interface methods of ITreeElement
  ////////////////////////////////////

  @Override
  public List<?> getChildren() {
    return children;
  }

  @Override
  public String getImageKey() {
    return imageKey;
  }

  @Override
  public Object getParent() {
    return parent;
  }

  @Override
  public String getText() {
    return text;
  }


  // helping methods
  //////////////////

  private static List<ITreeElement> convert( final String[] args ) {
    List<ITreeElement> result = new ArrayList<ITreeElement>();
    int index = 0;
    Stack<TreeElement> stack = new Stack<TreeElement>();
    while( args.length > index + 2 ) {
      try {
        int level = Integer.parseInt( args[ index++ ] );
        String text = args[ index++ ];
        String imageKey = args[ index++ ];
        TreeElement elem = new TreeElement( text, imageKey );
        if( level > stack.size() ) {
          // last elem from result = new parent
          TreeElement newParent = ( TreeElement )result.get( result.size() - 1 );
          stack.push( newParent );
          elem.setParent( newParent );
          newParent.addChild( elem );
        } else if( stack.size() > level ) {
          while( stack.size() > level ) {
            stack.pop();
          }
          if( !stack.isEmpty() ) {
            TreeElement newParent = stack.peek();
            elem.setParent( newParent );
            newParent.addChild( elem );
          }
        } else {
          // same level, keep parent from last
          if( !stack.isEmpty() ) {
            TreeElement newParent = stack.peek();
            elem.setParent( newParent );
            newParent.addChild( elem );
          }
        }
        result.add(  elem );
      } catch( final NumberFormatException numfex ) {
        // ignore this triple then
      }
    }
    return result;
  }
}
