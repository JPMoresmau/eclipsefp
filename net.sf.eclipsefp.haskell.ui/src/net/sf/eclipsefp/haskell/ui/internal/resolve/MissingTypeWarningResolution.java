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

  /**
   * extract the proper type from the message
   * @param prefix
   * @param msg
   * @return
   */
  public static String extractTypeFromMessage(final String prefix,final String msg){
    int ix=msg.toLowerCase().indexOf(  prefix);
    if (ix>-1){
      String type=msg.substring(ix+prefix.length()).trim();
      // replace [Char] by String
      type=type.replace("[Char]","String");

      int ixStartType=type.indexOf( "::" );
      if (ixStartType>-1){
          StringBuilder sb=new StringBuilder();
          String name= type.substring( 0,ixStartType );
       // if we're in Main, symbol name gets prefixed with Main.
          int lidx=name.lastIndexOf( '.' );
          if (lidx>-1){
            name=name.substring(lidx+1);
          }
          sb.append(name );
          sb.append("::" );
//        for (int a=ixStartType;a<type.length();a++){
//          if (Character.isUpperCase( type.charAt( a ) )){
//            break;
//          } else if (Character.isLowerCase(  type.charAt( a ) )){
            int start=ixStartType+2;
            // package names can appear in signature
            int end=type.indexOf( ":",start );
            while (end>-1){
            // but if we have :: inside, it's a kind signature, maybe?, so we don't truncate
              if (type.indexOf( ":",end+1 )!=end+1){
                int startPkg1=type.substring(start,end).lastIndexOf( ' ' );
                int startPkg2=type.substring(start,end).lastIndexOf( '(' );
                int startPkg=Math.max( startPkg1, startPkg2 );
                if (startPkg==-1){
                  startPkg=start;
                } else {
                  startPkg+=start+1;
                }
                sb.append(type.substring( start,startPkg ));
                start=end+1;
              } else {
                sb.append(type.substring( start,end ));
                sb.append("::");
                start=end+2;
              }

              end=type.indexOf( ":",start );
            }
            sb.append(type.substring( start ));
                //type=type.substring( 0,startPkg )+type.substring( end+1,type.length() );
//              break;
//            }
//          }
//        }
        // if we're in Main, symbol name gets prefixed with Main.
//        int lidx=type.substring(0,ixStartType).lastIndexOf( '.' );
//        if (lidx>-1){
//          type=type.substring(lidx+1);
//        }
            return sb.toString();
      }
      return type;
    }
    return null;
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,final IDocument document){
    String msg=marker.getAttribute(IMarker.MESSAGE,""); //$NON-NLS-1$
    //String toSearch=GhcMessages.WARNING_INFERREDTYPE_START;
    String type=extractTypeFromMessage( toSearch, msg );
    if (type!=null){
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
