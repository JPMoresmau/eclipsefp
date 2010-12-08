package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import org.eclipse.jface.text.templates.GlobalTemplateVariables;
import org.eclipse.jface.text.templates.TemplateContextType;

/**
 * The Haskell source code template context: This provides a context in the org.eclipse.ui.editors.templates
 * extension for source code completions.
  *
  * @author B. Scott Michel
 */
public class HSCodeTemplateContextType extends TemplateContextType {
  /** The context's name, as used in the plugin.xml file. This links the templates' XML file to the completion processor. */
  public static final String CONTEXT_TYPE = HSCodeTemplateContextType.class.getPackage().getName() + ".hscodetemplates";

  /**
   * Constructor for the Haskell source code template context. This adds the cursor, word selection and line selection
   * variable resolvers to the context.
   */
  public HSCodeTemplateContextType() {
    addResolver( new GlobalTemplateVariables.Cursor() );
    addResolver( new GlobalTemplateVariables.WordSelection() );
    addResolver( new GlobalTemplateVariables.LineSelection() );
  }
}
