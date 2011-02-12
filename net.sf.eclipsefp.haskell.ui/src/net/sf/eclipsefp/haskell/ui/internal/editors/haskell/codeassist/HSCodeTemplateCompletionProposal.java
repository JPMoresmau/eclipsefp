package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateProposal;
import org.eclipse.swt.graphics.Image;

public class HSCodeTemplateCompletionProposal extends TemplateProposal {
  public HSCodeTemplateCompletionProposal( final Template template, final TemplateContext context, final IRegion region,
                                           final Image image, final int relevance )
  {
    super( template, context, region, image, relevance );
  }

  @Override
  public String getAdditionalProposalInfo() {
    // Could do something creative here.
    return null;
  }
}
