package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.templates.ContextTypeRegistry;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateCompletionProcessor;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.text.templates.persistence.TemplateStore;
import org.eclipse.swt.graphics.Image;

/**
 * Haskell source code template completion processor.
 *
 * @author B. Scott Michel
 */
public class HSCodeTemplateAssistProcessor extends TemplateCompletionProcessor {
  /**
   * Reverse lex the document to extract the prefix for the completion. For example, if the current character at the offset
   * is part of an identifier, work backward until a non-identifier lex token is reached.
   *
   * <p>Things this function lexes:
   * <ul>
   * <li>Identifiers that don't start with underscore</li>
   * <li>Starts of comments, "{-" and "--"</li>
   * <li>Groups of Unicode symbol characters, to catch tokens like "=>"</li>
   * </ul>
   *
   * @return The collected prefix string or the empty string if nothing was collected.
   */
  @Override
  protected String extractPrefix( final ITextViewer viewer, final int offset ) {
    return HaskellContentAssistProcessor.lexCompletionPrefix( viewer.getDocument(), offset );
  }

  @Override
  protected Template[] getTemplates( final String contextTypeId ) {
    HSCodeTemplateManager manager = HSCodeTemplateManager.getInstance();
    TemplateStore tStore = manager.getTemplateStore();
    return tStore.getTemplates();
  }

  @Override
  protected TemplateContextType getContextType( final ITextViewer viewer, final IRegion region ) {
    HSCodeTemplateManager manager = HSCodeTemplateManager.getInstance();
    ContextTypeRegistry ctxRegistry = manager.getContextTypeRegistry();

    return ctxRegistry.getContextType( HSCodeTemplateContextType.CONTEXT_TYPE );
  }

  @Override
  protected Image getImage( final Template template ) {
    return HaskellUIImages.getImage( IImageNames.TESTSUITE_STANZA );
    // We need a template icon...
    // return HaskellUIPlugin.getDefault().getImageRegistry().get( HaskellUIPlugin.ICON_TEMPLATE );
  }

  @Override
  public ICompletionProposal[] computeCompletionProposals( final ITextViewer viewer, final int offset ) {
    ITextSelection selection = ( ITextSelection )viewer.getSelectionProvider().getSelection();
    int theOffset = offset;

    // adjust offset to end of normalized selection
    if ( selection.getOffset() == theOffset ) {
      theOffset = selection.getOffset() + selection.getLength();
    }

    String prefix = extractPrefix( viewer, theOffset );
    if (prefix == null) {
      return new ICompletionProposal[ 0 ];
    }

    Region region = new Region( theOffset - prefix.length(), prefix.length() );
    TemplateContext context = createContext( viewer, region );
    if( context == null ) {
      return new ICompletionProposal[ 0 ];
    }

    context.setVariable( "selection", selection.getText() ); // name of the selection variables {line, word_selection //$NON-NLS-1$

    Template[] templates = getTemplates( context.getContextType().getId() );
    String contextId = context.getContextType().getId();
    List<ICompletionProposal> matches = new ArrayList<ICompletionProposal>();

    for( int i = 0; i < templates.length; i++ ) {
      try {
        Template template = templates[ i ];
        context.getContextType().validate( template.getPattern() );

        if (prefix.length() > 0 ) {
          String templateName = template.getName();
          if ( templateName.startsWith( prefix ) && template.matches( prefix, contextId ) ) {
            matches.add( createProposal( template, context, ( IRegion )region, getRelevance( template, prefix ) ) );
          }
        }
      } catch( TemplateException e ) {
        continue;
      }
    }
    return matches.toArray( new ICompletionProposal[ matches.size() ] );
  }
}
