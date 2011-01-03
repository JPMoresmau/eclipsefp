package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
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
 * Container class for Haskell-specific template proposal variables.
  *
  * @author B. Scott Michel
 */
public class HaskellTemplateVariables {
  public static class Module extends TemplateVariableResolver {
    /** The template variable name */
    private static final String NAME = "module";
    /**
     * Create a new <code>Module</code> template proposal variable resolver.
     */
    public Module() {
      super(NAME, UITexts.HaskellTemplateVariables_module_description);
    }
    /**
     *
     */
    @Override
    public void resolve(final TemplateVariable variable, final TemplateContext context) {
      if (context instanceof DocumentTemplateContext) {
        DocumentTemplateContext docContext = ( DocumentTemplateContext ) context;
        IDocument doc = docContext.getDocument();

        Assert.isNotNull( doc );

        IFile file = HaskellUIPlugin.getFile(doc);
        if( file != null) {
          Assert.isTrue( FileUtil.hasHaskellExtension( file ) );
          Assert.isTrue(  ResourceUtil.isInHaskellProject( file ) );

          final ScionInstance si = ScionPlugin.getScionInstance( file );

          Assert.isNotNull( si );

          List<String> result = new ArrayList<String>();

          result.addAll( si.moduleGraph( ) );
          result.addAll( si.listExposedModules( ) );

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
  }
}
