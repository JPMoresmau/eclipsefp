package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.List;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
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
import org.eclipse.swt.graphics.Image;

/**
 * Haskell source code template completion processor.
 *
 * @author B. Scott Michel
 */
public class HSCodeTemplateAssistProcessor extends TemplateCompletionProcessor {
  @Override
  protected String extractPrefix( final ITextViewer viewer, final int offset ) {
    int i = offset;
    IDocument document = viewer.getDocument();
    if( i > document.getLength() ) {
      return "";
    }
    try {
      while( i > 0 ) {
        char ch = document.getChar( i - 1 );
        if( !Character.isJavaIdentifierPart( ch ) ) {
          break;
        }
        i--;
      }
      if( i > 0 ) {
        int j = i;
        if( document.getChar( j - 1 ) == '<' ) {
          i--;
        }
      }
      return document.get( i, offset - i );
    } catch( BadLocationException e ) {
      return "";
    }
  }

  @Override
  protected Template[] getTemplates( final String contextTypeId ) {
    HSCodeTemplateManager manager = HSCodeTemplateManager.getInstance();
    return manager.getTemplateStore().getTemplates();
  }

  @Override
  protected TemplateContextType getContextType( final ITextViewer viewer, final IRegion region ) {
    HSCodeTemplateManager manager = HSCodeTemplateManager.getInstance();
    ContextTypeRegistry ctxRegistry = manager.getContextTypeRegistry();

    return ctxRegistry.getContextType( HSCodeTemplateContextType.CONTEXT_TYPE );
  }

  @Override
  protected Image getImage( final Template template ) {
    return null;
    // We need a template icon...
    // return HaskellUIPlugin.getDefault().getImageRegistry().get( HaskellUIPlugin.ICON_TEMPLATE );
  }

  @Override
  public ICompletionProposal[] computeCompletionProposals( final ITextViewer viewer, final int offset ) {
    ITextSelection selection = ( ITextSelection )viewer.getSelectionProvider().getSelection();
    int theOffset = offset;
    // adjust offset to end of normalized selection
    if( selection.getOffset() == theOffset ) {
      theOffset = selection.getOffset() + selection.getLength();
    }
    String prefix = extractPrefix( viewer, theOffset );
    Region region = new Region( theOffset - prefix.length(), prefix.length() );
    TemplateContext context = createContext( viewer, region );
    if( context == null ) {
      return new ICompletionProposal[ 0 ];
    }
    context.setVariable( "selection", selection.getText() ); // name of the selection variables {line, word_selection //$NON-NLS-1$
    Template[] templates = getTemplates( context.getContextType().getId() );
    List<ICompletionProposal> matches = new ArrayList<ICompletionProposal>();
    for( int i = 0; i < templates.length; i++ ) {
      Template template = templates[ i ];
      try {
        context.getContextType().validate( template.getPattern() );
      } catch( TemplateException e ) {
        continue;
      }
      if( !prefix.equals( "" ) && prefix.charAt( 0 ) == '<' ) {
        prefix = prefix.substring( 1 );
      }
      if( !prefix.equals( "" )
          && ( template.getName().startsWith( prefix ) && template.matches(
              prefix, context.getContextType().getId() ) ) ) {
        matches.add( createProposal( template, context, ( IRegion )region,
            getRelevance( template, prefix ) ) );
      }
    }
    return matches.toArray( new ICompletionProposal[ matches.size() ] );
  }


}
