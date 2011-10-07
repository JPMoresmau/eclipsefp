package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.DeclarationType;
import net.sf.eclipsefp.haskell.browser.items.Documented;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.ImportsManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateVariable;
import org.eclipse.jface.text.templates.TemplateVariableResolver;


public class HsTypeNameTemplateVariable extends TemplateVariableResolver {
  /** The template variable name */
  private static final String NAME = "typeName";
  /** Associated Scion-server instance, if supplied. */
//  private final ScionInstance scion;


  public HsTypeNameTemplateVariable() {
    super( NAME, UITexts.HaskellTemplateVariables_typeName_description );
 //   scion = null;
  }

//  public HsTypeNameTemplateVariable( final String type, final String description ) {
//    super( type, description );
//    scion = null;
//  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void resolve( final TemplateVariable variable, final TemplateContext context ) {
    if( context instanceof DocumentTemplateContext ) {
      DocumentTemplateContext docContext = ( DocumentTemplateContext )context;
      IDocument doc = docContext.getDocument();

      Assert.isNotNull( doc );

      IFile file = HaskellUIPlugin.getFile( doc );
      if( file != null ) {
        Assert.isTrue( FileUtil.hasHaskellExtension( file ) );
        Assert.isTrue( ResourceUtil.isInHaskellProject( file ) );

//        final ScionInstance si = getScionInstance( file );
//
//        Assert.isNotNull( si );
//
//        Map<String, String> completions = si.completionsForTypes( file, doc );
//
//        if (completions != null) {
//          List<String> keys = new ArrayList<String>( completions.keySet() );
//
//          Collections.sort(keys);
//          variable.setValues( keys.toArray( new String[keys.size()] ) );
//        } else {
//          variable.setValue( new String() );
//          variable.setResolved( false );
//        }
        ImportsManager mgr = new ImportsManager( file, doc );
        Map<String, Documented> decls = mgr.getDeclarations();
        List<String> keys = new ArrayList<String>();
        for ( String s : decls.keySet() ) {
          Documented d = decls.get( s );
          if (d instanceof Declaration){
            if (DeclarationType.TYPE_SYNONYM.equals(((Declaration)d).getType())
                || DeclarationType.NEW_TYPE.equals(((Declaration)d).getType())
                || DeclarationType.DATA_TYPE.equals(((Declaration)d).getType())){
              keys.add(s);
            }

          }
        }
        Collections.sort(keys);
        variable.setValues( keys.toArray( new String[keys.size()] ) );
      } else {
        variable.setValue( new String() );
      }
    } else {
      variable.setValue( new String() );
    }

    variable.setResolved( true );
  }

  /** scion-server instance accessor */
//  private final ScionInstance getScionInstance(final IFile file) {
//    return (scion == null ? ScionPlugin.getScionInstance( file ) : scion);
//  }
}
