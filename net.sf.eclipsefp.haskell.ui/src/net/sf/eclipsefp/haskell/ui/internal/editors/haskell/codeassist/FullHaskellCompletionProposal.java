/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.resolve.AddPackageDependencyProposal;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;


/**
 * Haskell completion proposal
 * It may on completion add the proper import, and the proper package dependency in the Cabal file
 * @author JP Moresmau
 *
 */
public class FullHaskellCompletionProposal implements ICompletionProposal {
  private final String fDisplayString;
  private final String fReplacementString;
  private final int fReplacementOffset;
  private final int fReplacementLength;
  private final int fCursorPosition;
  private final Image fImage;
  private final String fAdditionalProposalInfo;

  private final String pkg;
  private final String module;
  private final String realImport;

  /**
   *
   */
  public FullHaskellCompletionProposal(final String replacementString, final int replacementOffset, final int replacementLength, final int cursorPosition, final Image image, final String displayString,final String additionalProposalInfo, final String pkg, final String m,final String real) {
    Assert.isNotNull(replacementString);
    Assert.isTrue(replacementOffset >= 0);
    Assert.isTrue(replacementLength >= 0);
    Assert.isTrue(cursorPosition >= 0);

    this.fReplacementString = replacementString;
    this.fReplacementOffset = replacementOffset;
    this.fReplacementLength = replacementLength;
    this.fCursorPosition = cursorPosition;
    this.fImage = image;
    this.fDisplayString = displayString;
    this.fAdditionalProposalInfo =additionalProposalInfo;
    this.pkg=pkg;
    this.module=m;
    this.realImport=real;
  }

  @Override
  public void apply( final IDocument document ) {
    try{
      document.replace(this.fReplacementOffset, this.fReplacementLength, this.fReplacementString);

      if (module!=null){

        HaskellEditor ed=HaskellUIPlugin.getHaskellEditor( document );
        if (ed!=null){
          if (pkg!=null){
            new AddPackageDependencyProposal( pkg ).apply( ed.findFile() );
          }
          String imp=realImport!=null?realImport:fReplacementString;
          ed.getImportsManager().addImport( imp, module, null, imp ).apply( document );
        }
      }

    } catch (BadLocationException localBadLocationException) {
      // noop
    }

  }

  @Override
  public Point getSelection( final IDocument paramIDocument ) {
    return new Point(this.fReplacementOffset + this.fCursorPosition, 0);
  }

  @Override
  public String getAdditionalProposalInfo() {
   return fAdditionalProposalInfo;
  }

  @Override
  public String getDisplayString() {
   return fDisplayString;
  }

  @Override
  public Image getImage() {
    return fImage;
  }

  @Override
  public IContextInformation getContextInformation() {
    return null;
  }



}
