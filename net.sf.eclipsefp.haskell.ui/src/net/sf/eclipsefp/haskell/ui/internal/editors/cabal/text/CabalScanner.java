// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.text;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.IWordDetector;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;

/** <p>token scanner for the cabal file content.</p>
  *
  * @author Leif Frenzel
  */
public class CabalScanner extends RuleBasedScanner {


  public CabalScanner() {
    IRule[] rules= new IRule[] { createKeywordRule() };
    setRules( rules );
  }

  @Override
  public IToken nextToken() {
    return super.nextToken();
  }

  // helping methods
  //////////////////

  private IRule createKeywordRule() {
    Color keyColor = ColorProvider.getInstance().getColor( ColorProvider.KEYWORD );
    IToken token= new Token( new TextAttribute( keyColor, null, SWT.BOLD ) );
    Color sectionColor = ColorProvider.getInstance().getColor( ColorProvider.SECTION );
    IToken sectionToken= new Token( new TextAttribute( sectionColor, null, SWT.BOLD ) );
    WordRule wordRule= new CaseInsensitiveWordRule( new SimpleWordDetector() );
    for( CabalSyntax keyword: CabalSyntax.values() ) {
      if (keyword.isSectionHeader()){
        wordRule.addWord( keyword.getCabalName(), sectionToken );
      } else {
        wordRule.addWord( keyword.getCabalName() + ":", token ); //$NON-NLS-1$
      }
    }

    wordRule.setColumnConstraint( 0 ); // only react if this is at the line beginning
    return wordRule;
  }


  // inner classes
  ////////////////

  private static final class SimpleWordDetector implements IWordDetector {

    // interface methods of IWordDetector
    /////////////////////////////////////

    @Override
    public boolean isWordStart( final char ch ) {
      return isWordPart( ch );
    }

    @Override
    public boolean isWordPart( final char ch ) {
      return !Character.isWhitespace( ch );
    }
  }
}
