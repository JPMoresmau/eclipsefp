package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateProposal;
import org.eclipse.swt.graphics.Image;


public class HSCodeTemplateCompletionProposal extends TemplateProposal {
  private final Template fTemplate;
  private final TemplateContext fContext;
  private final Image fImage;
  private final IRegion fRegion;
  private final int fRelevance;
  private final String fDisplayString;

  public HSCodeTemplateCompletionProposal( final Template template, final TemplateContext context, final IRegion region, final Image image,
                                           final int relevance )
  {
    super( template, context, region, image, relevance );
    fTemplate = template;
    fContext = context;
    fImage = image;
    fRegion = region;
    fRelevance = relevance;
    fDisplayString = null;
  }

  @Override
  public String getAdditionalProposalInfo() {
    // Could do something creative here.
    return null;
  }
}
