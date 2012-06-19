// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.code;

import net.sf.eclipsefp.haskell.core.code.EHaskellCommentStyle;
import net.sf.eclipsefp.haskell.util.PlatformUtil;


/** <p>helping class that generates Haskell source code.</p>
  *
  * @author Leif Frenzel
  */
public class CodeGenerator {

  // TODO honor code template for generating this
  public String createModuleContent( final String[] folderNames,
                                     final String name,
                                     final EHaskellCommentStyle style ) {
    StringBuilder sb = new StringBuilder();
    sb.append( PlatformUtil.NL );
    sb.append(getPrefixFor( style ));
    sb.append( "module " ); //$NON-NLS-1$
    for( int i = 0; i < folderNames.length; i++ ) {
      sb.append( folderNames[ i ] );
      sb.append( "." ); //$NON-NLS-1$
    }
    sb.append( name );
    sb.append( " where" ); //$NON-NLS-1$
    sb.append( PlatformUtil.NL);
    sb.append( getSuffixFor(style) );
    return sb.toString();
  }


  // helping methods
  //////////////////

  private static String getSuffixFor( final EHaskellCommentStyle style ) {
    return ( EHaskellCommentStyle.TEX == style ) ? "\\end{code}" : ""; //$NON-NLS-1$ //$NON-NLS-2$
  }

  private static String getPrefixFor( final EHaskellCommentStyle style ) {
    String result = ""; //$NON-NLS-1$
    if( EHaskellCommentStyle.LITERATE == style ) {
      result = "> "; //$NON-NLS-1$
    } else if( EHaskellCommentStyle.TEX == style ) {
      result = "\\begin{code}\n"; //$NON-NLS-1$
    }
    return result;
  }

}
