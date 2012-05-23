package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

/**
 * <p>Add type signature</p>
  *
  * @author JP Moresmau
 */
public class MissingTypeWarningResolution extends MarkerCompletion {
  private final String toSearch;



  public MissingTypeWarningResolution( final String toSearch ) {
    super();
    this.toSearch = toSearch;
  }

  @Override
  public String getLabel() {
    return UITexts.resolve_missingtype;
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,final IDocument document){
    String msg=marker.getAttribute(IMarker.MESSAGE,""); //$NON-NLS-1$
    //String toSearch=GhcMessages.WARNING_INFERREDTYPE_START;
    int ix=msg.toLowerCase().indexOf(  toSearch);
    if (ix>-1){
      String type=msg.substring(ix+toSearch.length()).trim();

      int ixStartType=type.indexOf( "::" );
      if (ixStartType>-1){
        for (int a=ixStartType;a<type.length();a++){
          if (Character.isUpperCase( type.charAt( a ) )){
            break;
          } else if (Character.isLowerCase(  type.charAt( a ) )){
            int start=a;
            // I don't remember why we search for :
            int end=type.indexOf( ":",a );
            // but if we have :: inside, it's a kind signature, maybe?, so we don' truncate
            if (end>-1 && type.indexOf( ":",end+1 )!=end+1){
              type=type.substring( 0,start )+type.substring( end+1,type.length() );
              break;
            }
          }
        }
        // if we're in Main, symbol name gets prefixed with Main.
        int lidx=type.substring(0,ixStartType).lastIndexOf( '.' );
        if (lidx>-1){
          type=type.substring(lidx+1);
        }
      }


      int line=marker.getAttribute(IMarker.LINE_NUMBER, 0);
      try {

        final int offset=document.getLineOffset( line-1 );
        final String txt=type+PlatformUtil.NL;
        final ICompletionProposal comp=new CompletionProposal(getLineStartAddition(txt,marker.getResource()) , offset, 0, txt.length(),HaskellUIImages.getImage( IImageNames.TYPE_SIGNATURE ),getLabel(),null,null );

        // we ensure KindSignatures is enabled if we have type *?
        // But it could be in Cabal file?
//        if (type.contains( "*" )){
//          return new ICompletionProposal() {
//
//            @Override
//            public Point getSelection( final IDocument paramIDocument ) {
//             return comp.getSelection( paramIDocument );
//            }
//
//            @Override
//            public Image getImage() {
//              return comp.getImage();
//            }
//
//            @Override
//            public String getDisplayString() {
//              return comp.getDisplayString();
//            }
//
//            @Override
//            public IContextInformation getContextInformation() {
//              return null;
//            }
//
//            @Override
//            public String getAdditionalProposalInfo() {
//              return null;
//            }
//
//            @Override
//            public void apply( final IDocument paramIDocument ) {
//              comp.apply( paramIDocument );
//              ICompletionProposal p=new AddLanguagePragmaResolution( "KindSignatures" ).getCompletionProposal( marker, document );
//              if (p!=null){
//                p.apply( paramIDocument );
//              }
//            }
//          };
//
//
//        }
        return comp;

      } catch( BadLocationException ex ) {
        HaskellUIPlugin.log( ex );
      }
    }
    return null;
  }

}
