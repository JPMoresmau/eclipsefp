// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.editor.text;

import java.util.*;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;

/** <p>Determines all markers for the given line and collects, concatenates, 
  * and formats their messages.</p>
  *
  * @author Leif Frenzel
  */
public class AnnotationHover implements IAnnotationHover {

  // interface methods of IAnnotationHover
  ////////////////////////////////////////
  
  public String getHoverInfo( final ISourceViewer sv, final int line ) {
    String result = null;
    
    List annotations = getAnnotations( sv, line );
    if( annotations != null ) {
      if( annotations.size() == 1 ) {
        Annotation annotation = ( Annotation )annotations.get( 0 );
        result = formatAnnotation( annotation );
      } else {
        List messages = collectMessages( annotations );
        if( messages.size() == 1 ) {
          result = ( ( String )messages.get( 0 ) );
        } else {
          result = formatMultipleMessages( messages );
        }
      }
    }
    return result;
  }
  
  
  // helping methods
  //////////////////

  private String formatAnnotation( final Annotation annotation ) {
    String result = null;
    String message = annotation.getText();
    if( message != null && message.trim().length() > 0 ) {
      result = message.trim();
    }
    return result;
  }

  private List collectMessages( final List annotations ) {
    List result = new ArrayList();
    Iterator it = annotations.iterator();
    while( it.hasNext() ) {
      Annotation annotation = ( Annotation )it.next();
      result.add( formatAnnotation( annotation ) );
    }
    return result;
  }

  private String formatMultipleMessages( final List messages ) {
    StringBuffer sb = new StringBuffer();
    sb.append( "Multiple markers at this line:\n" );
    Iterator iter = messages.iterator();
    while( iter.hasNext() ) {
      sb.append( "\n  - " );
      sb.append( iter.next() );
    }
    return sb.toString();
  }

  // TODO refactor
  
  private int compareRulerLine( final Position position, 
                                final IDocument document, 
                                final int line ) {
    int result = 0;
    if( position.getOffset() > -1 && position.getLength() > -1 ) {
      try {
        int annotationLine = document.getLineOfOffset( position.getOffset() );
        int posEnd = position.getOffset() + position.getLength();
        if( line == annotationLine ) {
          result = 1;
        } else if(    annotationLine < line
                   && line <= document.getLineOfOffset( posEnd ) ) {
          result = 2;
        }
      } catch( BadLocationException badlox ) {
        // ignored
      }
    }
    return result;
  }

  private List getAnnotations( final ISourceViewer viewer, final int line ) {
    List result = null;
    IAnnotationModel model = viewer.getAnnotationModel();
    if( model != null ) {
      result = new ArrayList();
      IDocument document = viewer.getDocument();
  
      Iterator it = model.getAnnotationIterator();
      Map messagesAtPosition = new HashMap();
      while( it.hasNext() ) {
        Object obj = it.next();
        if( obj instanceof Annotation ) {
          Position position = model.getPosition( ( Annotation )obj );
          if( position == null ) {
            continue;
          }
  
          String text = ( ( Annotation )obj ).getText();
          if( isDuplicate( messagesAtPosition, position, text ) ) {
            continue;
          }
  
          switch( compareRulerLine( position, document, line ) ) {
          case 1:
            result.add( obj );
            break;
          }
        }
      }
    }
    return result;
  }

  private boolean isDuplicate( final Map messagesAtPosition,
                               final Position position, 
                               final String message ) {
    if( messagesAtPosition.containsKey( position ) ) {
      Object value = messagesAtPosition.get( position );
      if( message.equals( value ) ) {
        return true;
      }

      if( value instanceof List ) {
        List messages = ( List )value;
        if( messages.contains( message ) ) {
          return true;
        }
        messages.add( message );
      } else {
        ArrayList messages = new ArrayList();
        messages.add( value );
        messages.add( message );
        messagesAtPosition.put( position, messages );
      }
    } else {
      messagesAtPosition.put( position, message );
    }
    return false;
  }
}