/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;


/**
 * A completion proposal that does not change the selection in the document
 * we had to reproduce the CompletionProposal implementation since it's final
 * @author JP Moresmau
 *
 */
public class DiscreteCompletionProposal implements ICompletionProposal {


  /** The string to be displayed in the completion proposal popup. */
  private final String fDisplayString;
  /** The replacement string. */
  private final String fReplacementString;
  /** The replacement offset. */
  private final int fReplacementOffset;
  /** The replacement length. */
  private final int fReplacementLength;
  /** The image to be displayed in the completion proposal popup. */
  private final Image fImage;
  /** The context information of this proposal. */
  private final IContextInformation fContextInformation;
  /** The additional info of this proposal. */
  private final String fAdditionalProposalInfo;

  /**
   * Creates a new completion proposal based on the provided information. The replacement string is
   * considered being the display string too. All remaining fields are set to <code>null</code>.
   *
   * @param replacementString the actual string to be inserted into the document
   * @param replacementOffset the offset of the text to be replaced
   * @param replacementLength the length of the text to be replaced
   */
  public DiscreteCompletionProposal(final String replacementString, final int replacementOffset, final int replacementLength) {
    this(replacementString, replacementOffset, replacementLength, null, null, null, null);
  }

  /**
   * Creates a new completion proposal. All fields are initialized based on the provided information.
   *
   * @param replacementString the actual string to be inserted into the document
   * @param replacementOffset the offset of the text to be replaced
   * @param replacementLength the length of the text to be replaced
   * @param image the image to display for this proposal
   * @param displayString the string to be displayed for the proposal
   * @param contextInformation the context information associated with this proposal
   * @param additionalProposalInfo the additional information associated with this proposal
   */
  public DiscreteCompletionProposal(final String replacementString, final int replacementOffset, final int replacementLength, final Image image, final String displayString, final IContextInformation contextInformation, final String additionalProposalInfo) {
    Assert.isNotNull(replacementString);
    Assert.isTrue(replacementOffset >= 0);
    Assert.isTrue(replacementLength >= 0);

    fReplacementString= replacementString;
    fReplacementOffset= replacementOffset;
    fReplacementLength= replacementLength;
    fImage= image;
    fDisplayString= displayString;
    fContextInformation= contextInformation;
    fAdditionalProposalInfo= additionalProposalInfo;
  }

  /*
   * @see ICompletionProposal#apply(IDocument)
   */
  @Override
  public void apply(final IDocument document) {
    try {
      document.replace(fReplacementOffset, fReplacementLength, fReplacementString);
    } catch (BadLocationException x) {
      // ignore
    }
  }

  /*
   * @see ICompletionProposal#getSelection(IDocument)
   */
  @Override
  public Point getSelection(final IDocument document) {
    return null;
  }

  /*
   * @see ICompletionProposal#getContextInformation()
   */
  @Override
  public IContextInformation getContextInformation() {
    return fContextInformation;
  }

  /*
   * @see ICompletionProposal#getImage()
   */
  @Override
  public Image getImage() {
    return fImage;
  }

  /*
   * @see ICompletionProposal#getDisplayString()
   */
  @Override
  public String getDisplayString() {
    if (fDisplayString != null) {
      return fDisplayString;
    }
    return fReplacementString;
  }

  /*
   * @see ICompletionProposal#getAdditionalProposalInfo()
   */
  @Override
  public String getAdditionalProposalInfo() {
    return fAdditionalProposalInfo;
  }
}

