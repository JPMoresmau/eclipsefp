// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.ca;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.templates.ContextTypeRegistry;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.text.templates.TemplateProposal;
import org.eclipse.jface.text.templates.persistence.TemplateStore;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.graphics.Image;

/** <p>the completion processor for content assist on the Cabal editor.</p>
  *
  * @author Leif Frenzel
  */
public class CabalCompletionProcessor implements IContentAssistProcessor {

  private static final String ID_CONTEXT
    = "cabalPackageDescriptionTemplates"; //$NON-NLS-1$

  // interface methods of IContentAssistProcessor
  ///////////////////////////////////////////////

  public ICompletionProposal[] computeCompletionProposals( final ITextViewer tv,
                                                           final int offset ) {
    ICompletionProposal[] result = new ICompletionProposal[ 0 ];
    ISelection selection = tv.getSelectionProvider().getSelection();
    if( selection instanceof ITextSelection ) {
      ITextSelection tsel = ( ITextSelection )selection;
      int realOffset = adjustOffset( offset, tsel );
      String prefix = getPrefix( tv, realOffset );
      Region region= new Region( realOffset - prefix.length(),
                                 prefix.length() + tsel.getLength() );
      result = computeTemplateProposals( tv, region );
    }
    return result;
  }

  public IContextInformation[] computeContextInformation( final ITextViewer viewer,
                                                          final int offset ) {
    // unused
    return null;
  }

  public char[] getCompletionProposalAutoActivationCharacters() {
    // unused
    return null;
  }

  public char[] getContextInformationAutoActivationCharacters() {
    // unused
    return null;
  }

  public IContextInformationValidator getContextInformationValidator() {
    // unused
    return null;
  }

  public String getErrorMessage() {
    // unused
    return null;
  }


  // helping methods
  //////////////////

  private ICompletionProposal[] computeTemplateProposals( final ITextViewer viewer,
                                                          final IRegion region ) {
    TemplateContext context = createContext( viewer, region );
    TemplateProposal[] result = new TemplateProposal[ 0 ];
    if( context != null ) {
      ISelectionProvider selectionProvider = viewer.getSelectionProvider();
      ISelection selection = selectionProvider.getSelection();
      if( selection instanceof ITextSelection ) {
        ITextSelection textSel = ( ITextSelection )selection;
        // name of the selection variables {line, word}_selection
        context.setVariable( "selection", textSel.getText() );  //$NON-NLS-1$

        Template[] templates = getTemplates( context );
        List<TemplateProposal> matches = new ArrayList<TemplateProposal>();
        for( int i= 0; i < templates.length; i++ ) {
          Template template= templates[ i ];
          try {
            context.getContextType().validate( template.getPattern() );
            Image img = HaskellUIImages.getImage( IImageNames.TEMPLATE );
            matches.add( new TemplateProposal( template,
                                               context,
                                               region,
                                               img ) );
          } catch( final TemplateException tex ) {
            // ignored
          }
        }
        result = matches.toArray( new TemplateProposal[ matches.size() ] );
      }
    }
    return result;
  }

  private Template[] getTemplates( final TemplateContext context ) {
    String id = context.getContextType().getId();
    TemplateStore templateStore = TemplateProvider.getTemplateStore();
    Template[] templates = templateStore.getTemplates( id );
    return templates;
  }

  private TemplateContext createContext( final ITextViewer viewer,
                                         final IRegion region ) {
    TemplateContext result = null;
    ContextTypeRegistry reg = TemplateProvider.getContextTypeRegistry();
    TemplateContextType contextType = reg.getContextType( ID_CONTEXT );
    if( contextType != null ) {
      result = new DocumentTemplateContext( contextType,
                                            viewer.getDocument(),
                                            region.getOffset(),
                                            region.getLength() );
    }
    return result;
  }

  private int adjustOffset( final int offset, final ITextSelection tsel ) {
    int result;
    if( tsel.getOffset() != offset ) {
      result = tsel.getOffset();
    } else {
      result = offset;
    }
    return result;
  }

  private String getPrefix( final ITextViewer viewer, final int offset ) {
    String result = ""; //$NON-NLS-1$
    int index = offset;
    IDocument document = viewer.getDocument();
    if( index <= document.getLength() ) {
      try {
        while( index > 0 ) {
          char ch = document.getChar( index - 1 );
          if (! Character.isLetterOrDigit(ch)) {
            break;
          }
          index--;
        }
        result = document.get( index, offset - index );
      } catch( BadLocationException e ) {
        // ignore
      }
    }
    return result;
  }
}
