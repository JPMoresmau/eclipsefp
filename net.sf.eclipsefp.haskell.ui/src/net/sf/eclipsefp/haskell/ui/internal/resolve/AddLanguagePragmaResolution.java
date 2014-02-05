package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.osgi.util.NLS;

/**
 * <p>Add language pragma</p>
  *
  * @author JP Moresmau
 */
public class AddLanguagePragmaResolution extends MarkerCompletion {

  private final String pragma;

  public AddLanguagePragmaResolution( final String pragma ) {
    super();
    this.pragma = pragma;
  }

  @Override
  public String getLabel() {
    return NLS.bind( UITexts.resolve_addpragma, pragma,"LANGUAGE" );//$NON-NLS-1$
  }


  /**
   * @return the pragma
   */
  public String getPragma() {
    return pragma;
  }

  protected String getPragmaStart(){
    return "{-# LANGUAGE "; //$NON-NLS-1$
  }

  protected String getPragmaEnd(){
    return "#-}"; //$NON-NLS-1$
  }

  protected String getSeparator(){
    return ", "; //$NON-NLS-1$
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,final IDocument document){
   //int line=marker.getAttribute(IMarker.LINE_NUMBER, 0);
    try {
      //int offset=document.getLineOffset( line-1 );
      int ix=0;
      int pragmaOffset=0;
      int lineOffset=0;

      if (FileUtil.hasLiterateExtension( marker.getResource() )){
        while (ix<document.getNumberOfLines()){
          IRegion r=document.getLineInformation( ix );
          String l=document.get( r.getOffset(), r.getLength() ).trim();
          if (!l.startsWith( ">" )){
            ix++;
          } else {
            break;
          }
        }
      }
      if (ix>0){
        lineOffset=document.getLineOffset( ix );
      }
      String repl=getPragmaStart()+pragma+" "+getPragmaEnd()+PlatformUtil.NL; //$NON-NLS-1$
      while (ix<document.getNumberOfLines()){
        IRegion r=document.getLineInformation( ix );
        String l=document.get( r.getOffset(), r.getLength() ).trim();
        if (FileUtil.hasLiterateExtension( marker.getResource() )){
          l=l.substring( 1 ).trim();
        }
        if (l.startsWith(getPragmaStart())){
          int ixEnd=l.indexOf( getPragmaEnd(),getPragmaStart().length());
          if (ixEnd>-1){
            if (l.contains( pragma )){
              return null;
            }
            pragmaOffset=r.getOffset()+l.substring( 0,ixEnd ).trim().length();
            repl=getSeparator()+pragma;
          }
        } else if (l.startsWith( "module" )){ //$NON-NLS-1$
          break;
        }
        ix++;
      }

      if (pragmaOffset==0){
        repl=getLineStartAddition( repl, marker.getResource() );
        if (FileUtil.hasLiterateExtension( marker.getResource() )){
          repl=repl+PlatformUtil.NL;
        }
      }

      return new DiscreteCompletionProposal(repl, lineOffset+pragmaOffset, 0,null,getLabel(),null,null );
    } catch( BadLocationException ex ) {
      HaskellUIPlugin.log( ex );
    }
    return null;
  }

}
