// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.text;

import java.util.ArrayList;

import net.sf.eclipsefp.haskell.core.codeassist.HaskellSyntax;

import org.eclipse.jface.text.rules.*;

import net.sf.eclipsefp.haskell.ui.editor.syntax.*;
import net.sf.eclipsefp.haskell.ui.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.util.text.WordDetector;


/** <p>Tokenizes Haskell code into keywords, whitespaces, string and number 
  * literals etc.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellCodeScanner extends RuleBasedScanner 
                                implements IEditorPreferenceNames {

  /** <p>constructs a new HaskellCodeScanner and specifies the scanning 
   * rules.</p> */
  public HaskellCodeScanner() {
    ScannerManager man = ScannerManager.getInstance();
    IToken keywordToken = man.createToken( EDITOR_KEYWORD_COLOR, 
                                           EDITOR_KEYWORD_BOLD );
    IToken stringToken = man.createToken( EDITOR_STRING_COLOR,
                                          EDITOR_STRING_BOLD );
    IToken defaultToken = man.createToken( EDITOR_DEFAULT_COLOR, 
                                           EDITOR_DEFAULT_BOLD );
    ArrayList list = new ArrayList();

    // strings and characters
    list.add( new SingleLineRule( "\"", "\"", stringToken, '\\' ) );
    // generic whitespace rule
    list.add( new WhitespaceRule( new WhitespaceDetector() ) );

    // word rule for functions
    list.add( createFunctionRule( defaultToken, keywordToken ) );

    // add rule for the special case of the -> keyword
    list.add( new ArrowRule( keywordToken ) );
    
    applyRulesList( list );
    setDefaultReturnToken( defaultToken );
  }

  private WordRule createFunctionRule( final IToken defaultToken, 
                                       final IToken keywordToken ) {
    WordRule result = new WordRule( new WordDetector(), defaultToken );
    
    ScannerManager man = ScannerManager.getInstance();
    IToken functionToken = man.createToken( EDITOR_FUNCTION_COLOR, 
                                            EDITOR_FUNCTION_BOLD );
    String[] functions = HaskellSyntax.getFunctions();
    for( int i = 0; i < functions.length; i++ ) {
      result.addWord( functions[ i ], functionToken );
    }
    
    String[] keywords = HaskellSyntax.getKeywords();
    for( int i = 0; i < keywords.length; i++ ) {
      result.addWord( keywords[ i ], keywordToken );
    }
    
    return result;
  }

  private void applyRulesList( final ArrayList list ) {
    IRule[] rules = new IRule[ list.size() ];
    list.toArray( rules );
    setRules( rules );
  }
}