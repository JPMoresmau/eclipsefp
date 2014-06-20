// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.code;

import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.code.EHaskellCommentStyle;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.preferences.TemplateVariables;
import net.sf.eclipsefp.haskell.util.PlatformUtil;


/** <p>helping class that generates Haskell source code.</p>
  *
  * @author Leif Frenzel
  */
public class CodeGenerator {

  public String createModuleContent( final String projectName,
      final String[] folderNames,
      final String name,
      final EHaskellCommentStyle style ) {
    return createModuleContent( projectName, folderNames, name, style, ICorePreferenceNames.TEMPLATE_MODULE );
  }

  protected void addVariables(final Map<String,String> vars){
    // NOOP
  }

  public String createModuleContent( final String projectName,
                                     final String[] folderNames,
                                     final String name,
                                     final EHaskellCommentStyle style,
                                     String pref) {
    StringBuilder sb = new StringBuilder();
    // why start with a new line?
    //sb.append( PlatformUtil.NL );
    sb.append(getPrefixFor( style ));
    StringBuilder module=new StringBuilder();
    StringBuilder src=new StringBuilder();
    for( int i = 0; i < folderNames.length; i++ ) {
      module.append( folderNames[ i ] );
      module.append( "." ); //$NON-NLS-1$
      if (src.length()>0){
        src.append("/");//$NON-NLS-1$
      }
      src.append(folderNames[ i ]);
    }
    module.append(name);
    Map<String,String> vars=new HashMap<>();
    vars.put( TemplateVariables.MODULE_NAME, module.toString() );
    vars.put( TemplateVariables.PROJECT_NAME, projectName );
    vars.put( TemplateVariables.SRC, src.toString() );
    vars.put( TemplateVariables.USER_NAME, PlatformUtil.getCurrentUser() );
    vars.put( TemplateVariables.IMPORTS,""); //$NON-NLS-1$
    vars.put( TemplateVariables.IMPORTS_HTF,""); //$NON-NLS-1$
    addVariables(vars);
    if (pref==null){
      pref=ICorePreferenceNames.TEMPLATE_MODULE;
    }
    if(!pref.equals( ICorePreferenceNames.TEMPLATE_MODULE )){
      String mod1=HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_MODULE, vars );
      vars.put( TemplateVariables.MODULE, mod1 );
      addVariables(vars);
    }

    String mod=HaskellCorePlugin.populateTemplate( pref, vars );
    sb.append(mod);

//    sb.append( "module " ); //$NON-NLS-1$
//    for( int i = 0; i < folderNames.length; i++ ) {
//      sb.append( folderNames[ i ] );
//      sb.append( "." ); //$NON-NLS-1$
//    }
//    sb.append( name );
//    sb.append( " where" ); //$NON-NLS-1$
//    sb.append( PlatformUtil.NL);


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
