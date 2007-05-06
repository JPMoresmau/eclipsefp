// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors.text;

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

  private static final String[] KEYWORDS = new String[] {
    "name", "version", "cabal-version", "license", "license-file", "copyright",  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "author", "maintainer", "stability", "homepage", "package-url", "synopsis",      //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$//$NON-NLS-4$//$NON-NLS-5$ //$NON-NLS-6$
    "description", "category", "tested-with", "build-depends", "data-files",      //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$//$NON-NLS-4$//$NON-NLS-5$
    "extra-source-files", "extra-tmp-files", "exposed-modules", "executable", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "main-is", "buildable", "other-modules", "hs-source-dirs", "extensions",      //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$//$NON-NLS-4$//$NON-NLS-5$
    "ghc-options", "ghc-prof-options", "hugs-options", "nhc-options", "includes",  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "include-dirs", "c-sources", "extra-libraries", "extra-lib-dirs", "cc-options",   //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$//$NON-NLS-5$
    "ld-options", "frameworks"  //$NON-NLS-1$//$NON-NLS-2$
  };


  public CabalScanner() {
    IRule[] rules= new IRule[] { createKeywordRule() };
    setRules( rules );
  }
  
  
  // helping methods
  //////////////////
  
  private IRule createKeywordRule() {
    Color keyColor = ColorProvider.getInstance().getColor( ColorProvider.KEYWORD );
    IToken token= new Token( new TextAttribute( keyColor, null, SWT.BOLD ) );
    WordRule wordRule= new CaseInsensitiveWordRule( new SimpleWordDetector() );
    for( String keyword: KEYWORDS ) {
      wordRule.addWord( keyword + ":", token ); //$NON-NLS-1$
    }
    wordRule.setColumnConstraint( 0 ); // only react if this is at the line beginning
    return wordRule;
  }

  
  // inner classes
  ////////////////
  
  private static final class SimpleWordDetector implements IWordDetector {
    
    // interface methods of IWordDetector
    /////////////////////////////////////
    
    public boolean isWordStart( final char ch ) {
      return isWordPart( ch );
    }

    public boolean isWordPart( final char ch ) {
      return !Character.isWhitespace( ch );
    }
  }
}
