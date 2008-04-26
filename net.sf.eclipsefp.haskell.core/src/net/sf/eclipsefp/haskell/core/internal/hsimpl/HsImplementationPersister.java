// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.hsimpl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.runtime.IStatus;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/** <p>helper for serializing and de-serializing Haskell implementation
  * descriptors.</p>
  *
  * @author Leif Frenzel
  *
  */
public class HsImplementationPersister {

  private static final String PREAMBLE
    = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"; //$NON-NLS-1$
  private static final String ELEM_TOP_LEVEL = "hsImpls"; //$NON-NLS-1$
  private static final String ELEM_HS_IMPL = "hsImpl"; //$NON-NLS-1$

  private static final String ATT_NAME    = "name"; //$NON-NLS-1$
  private static final String ATT_BIN_DIR = "binDir"; //$NON-NLS-1$
  private static final String ATT_VERSION = "version"; //$NON-NLS-1$
  private static final String ATT_TYPE    = "type"; //$NON-NLS-1$

  public static String toXML( final List<IHsImplementation> impls ) {
    StringBuilder sb = new StringBuilder( PREAMBLE );
    sb.append( "<" ); //$NON-NLS-1$
    sb.append( ELEM_TOP_LEVEL );
    sb.append( ">\n" ); //$NON-NLS-1$
    for( IHsImplementation impl: impls ) {
      toXML( impl, sb );
    }
    sb.append( "</" ); //$NON-NLS-1$
    sb.append( ELEM_TOP_LEVEL );
    sb.append( ">\n" ); //$NON-NLS-1$
    return sb.toString();
  }

  public static void fromXML( final String xml, final List<IHsImplementation> impls ) {
    if( !isEmpty( xml ) ) {
      try {
        DocumentBuilder parser = DocumentBuilderFactory.newInstance()
        .newDocumentBuilder();
        InputStream is = new ByteArrayInputStream( xml.getBytes() );
        Element rootElement = parser.parse( is ).getDocumentElement();
        if( rootElement != null ) {
          load( rootElement, impls );
        }
      } catch( final Exception ex ) {
        HaskellCorePlugin.log( "Parsing Haskell Implementations", ex ); //$NON-NLS-1$
      }
    }
  }


  // helping functions
  ////////////////////

  private static void toXML( final IHsImplementation impl,
                             final StringBuilder sb ) {
    sb.append( "  <" ); //$NON-NLS-1$
    sb.append( ELEM_HS_IMPL );
    appendAtt( ATT_NAME, impl.getName(), sb );
    appendAtt( ATT_BIN_DIR, impl.getBinDir(), sb );
    appendAtt( ATT_VERSION, impl.getVersion(), sb );
    appendAtt( ATT_TYPE, impl.getType().name(), sb );
    sb.append( "/>\n" ); //$NON-NLS-1$
  }

  private static void appendAtt( final String att,
                                 final String value,
                                 final StringBuilder sb ) {
    sb.append( " " ); //$NON-NLS-1$
    sb.append( att );
    sb.append( "=\"" ); //$NON-NLS-1$
    sb.append(  value );
    sb.append( "\"" ); //$NON-NLS-1$
  }

  private static void load( final Element rootElement,
                            final List<IHsImplementation> impls) {
    NodeList list = rootElement.getElementsByTagName( ELEM_HS_IMPL );
    for( int i = 0; i < list.getLength(); i++ ) {
      Node item = list.item( i );
      NamedNodeMap attributes = item.getAttributes();
      if( attributes != null ) {
         String name = loadAtt( attributes, ATT_NAME );
         String binDir = loadAtt( attributes, ATT_BIN_DIR );
         String version = loadAtt( attributes, ATT_VERSION );
         String sType = loadAtt( attributes, ATT_TYPE );
         if(    !isEmpty( name )
             && !isEmpty( binDir )
             && !isEmpty( version )
             && !isEmpty( sType ) ) {
           HsImplementationType type = HsImplementationType.valueOf( sType );
           HsImplementation hsi = new HsImplementation();
           hsi.setType( type );
           hsi.setName( name );
           hsi.setBinDir( binDir );
           hsi.setVersion( version );
           IStatus[] status = hsi.validate();
           for( IStatus st: status ) {
            if( st.matches( IStatus.ERROR ) ) {
              HaskellCorePlugin.getDefault().getLog().log( st );
            }
           }
           impls.add( hsi );
         }
      }
    }
  }

  private static String loadAtt( final NamedNodeMap attributes,
                                 final String attName ) {
    String result = null;
    Node pathNode = attributes.getNamedItem( attName );
    if( pathNode != null ) {
      result = pathNode.getNodeValue();
    }
    return result;
  }

  private static boolean isEmpty( final String candidate ) {
    return candidate == null || candidate.trim().length() == 0;
  }
}
