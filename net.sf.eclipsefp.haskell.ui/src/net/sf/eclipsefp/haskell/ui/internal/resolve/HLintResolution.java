/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.hlint.HLintFixer;
import net.sf.eclipsefp.haskell.hlint.HLintFixer.HLintFix;
import net.sf.eclipsefp.haskell.hlint.Suggestion;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.osgi.util.NLS;


/**
 * @author JP Moresmau
 *
 */
public class HLintResolution extends MarkerCompletion {


  /* (non-Javadoc)
   * @see org.eclipse.ui.IMarkerResolution#getLabel()
   */
  public String getLabel() {
    return UITexts.resolve_hlint;
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.resolve.MarkerCompletion#getCompletionProposal(org.eclipse.core.resources.IMarker, org.eclipse.jface.text.IDocument)
   */
  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    int line=marker.getAttribute( IMarker.LINE_NUMBER, -1 );
    if (line>0){
        Suggestion s=new Suggestion();
        s.fromString( marker.getAttribute( HaskellCorePlugin.ATT_HLINT_SUGGESTION,"" ));
        if (s.getPre()!=null && s.getLocation()!=null){
          try {
            int offset=document.getLineOffset( line-1 )+s.getLocation().getColumn()-1;
            HLintFix fix=HLintFixer.fix( document.get(), offset, s );
            if (fix.isFullMatch()){
              String add=getAdditionalInfo(s) ;
              String label=NLS.bind( UITexts.resolve_hlint_explain,add);
              return new MarkerCompletionProposal( fix.getValue(), offset, fix.getLength(), fix.getLength(),label,marker,add);
            }
          } catch( BadLocationException ex ) {
            HaskellUIPlugin.log( ex );
          }
        }

    }
    return null;
  }

  private String getAdditionalInfo(final Suggestion s){
    String pre=s.getPreText();
    String post=s.getPostText();
    if (pre!=null){
      if (post!=null){
        return NLS.bind( UITexts.resolve_hlint_replace,pre,post);
      } else {
        return NLS.bind( UITexts.resolve_hlint_remove,pre);
      }
    }
    return null;
  }

}
