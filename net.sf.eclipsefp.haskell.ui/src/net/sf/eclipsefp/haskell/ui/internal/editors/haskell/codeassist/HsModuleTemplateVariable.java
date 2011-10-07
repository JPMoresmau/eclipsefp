package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateVariable;
import org.eclipse.jface.text.templates.TemplateVariableResolver;

/**
 * Template variable resolver for module names
  *
  * @author B. Scott Michel (bscottm@ieee.org)
  * @deprecated
 */
public class HsModuleTemplateVariable extends TemplateVariableResolver {
  /** The template variable name */
  private static final String NAME = "module";
  /** Associated Scion-server instance, if supplied. */
//  private final ScionInstance scion;

  /**
   * Create a new module name template proposal variable resolver.
   */
  public HsModuleTemplateVariable() {
    super( NAME, UITexts.HaskellTemplateVariables_module_description );
//    scion = null;
  }

  /**
   * Create a new module name template variable resolver, with an associated Scion-server instance.
   */
//  public HsModuleTemplateVariable(final ScionInstance scion) {
//    super( NAME, UITexts.HaskellTemplateVariables_module_description );
//    this.scion = scion;
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

        //final ScionInstance si = getScionInstance( file );
        //Assert.isNotNull( si );

        List<String> result = new ArrayList<String>();

        result.addAll( BrowserPlugin.getSharedInstance().getCachedModuleNames() );
//        for (String s : si.moduleGraph()) {
//          if (!result.contains( s )) {
//            result.add( s );
//          }
//        }
//        for (String s : si.listExposedModules()) {
//          if (!result.contains( s )) {
//            result.add( s );
//          }
//        }

        String[] strResults = new String[ result.size() ];
        result.toArray( strResults );
        variable.setValues( strResults );
      } else {
        variable.setValue( new String() );
      }
    } else {
      variable.setValue( new String() );
    }

    variable.setResolved( true );
  }

//  /** scion-server instance accessor */
//  private final ScionInstance getScionInstance(final IFile file) {
//    return (scion == null ? ScionPlugin.getScionInstance( file ) : scion);
//  }
}
