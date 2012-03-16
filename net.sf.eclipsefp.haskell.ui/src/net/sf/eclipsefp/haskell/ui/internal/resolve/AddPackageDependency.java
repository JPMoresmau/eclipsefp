package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.osgi.util.NLS;


/**
 * <p>Add package to build-depends of all components when missing</p>
  *
  * @author JP Moresmau
 */
public class AddPackageDependency extends MarkerCompletion {
  private final String value;

  public AddPackageDependency( final String value ) {
    super();
    this.value = value;
  }

  @Override
  public void run( final IMarker marker ) {
    getCompletionProposal( marker, null ).apply( null );
  }


  /**
   * @return the pkg
   */
  public String getValue() {
    return value;
  }

  @Override
  public String getLabel() {
    return NLS.bind( UITexts.resolve_addpackage, value );
  }

  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    return new AddPackageDependencyProposal( value, marker );
  }

}
