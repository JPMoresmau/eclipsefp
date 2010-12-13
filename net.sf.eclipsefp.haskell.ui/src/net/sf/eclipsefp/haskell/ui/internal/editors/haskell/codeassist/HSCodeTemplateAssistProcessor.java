package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.util.HaskellText;
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
    IDocument document = viewer.getDocument();

    // If we're beyond the document limit (how?), return an empty string
    if( offset > document.getLength() ) {
      return new String();
    }

    try {
      IRegion lineAt = document.getLineInformationOfOffset( offset );
      int i = lineAt.getOffset();
      char ch = document.getChar( i );

      // Is this a comment block -> possible Haddock template completion context
      while (   Character.isSpaceChar( ch )
             && i <= offset ) {
        ch = document.getChar( ++i );
      }

      if (   (ch == '{' || ch == '-' )
          && document.getChar(i + 1) == '-' ) {
        if (i + 1 == offset) {
          // Smells like a comment and a Haddock template completion
          return document.get(i, i + 1);
        }

        // We're somewhere in the middle of a comment, which means there are no
        // possible template completions.
        return null;
      }

      // Otherwise, collect an identifier but don't go past the beginning of the current line:
      i = offset - 1;

      boolean stopscan = false;
      while (i > lineAt.getOffset() && !stopscan) {
        ch = document.getChar( i );
        if (HaskellText.isHaskellIdentifierPart(document.getChar( i ))) {
          --i;
        } else {
          stopscan = true;
        }
      }

      return document.get( i, offset - i );
    } catch( BadLocationException e ) {
      // Dunno how we'd generate this exception, but handle it anyway.
      return new String();
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
