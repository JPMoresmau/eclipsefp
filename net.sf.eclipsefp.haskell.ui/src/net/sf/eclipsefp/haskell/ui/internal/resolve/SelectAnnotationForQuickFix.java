package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.MarkerAnnotation;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * <p>An action that allows calling QuickFix from the annotation ruler, since the Eclipse code is only in the JDT</p>
  *
  * @author JP Moresmau
 */
public class SelectAnnotationForQuickFix extends TextEditorAction {
  private final MarkerAnnotation ann;
  private final SourceViewer sourceViewer;

  public SelectAnnotationForQuickFix(
      final ITextEditor editor,final SourceViewer sourceViewer,final MarkerAnnotation ann ) {
    super( HaskellUIPlugin.getDefault().getResourceBundle(), "QuickFix.", editor ); //$NON-NLS-1$
    this.ann=ann;
    this.sourceViewer=sourceViewer;
  }

  @Override
  public String getText() {
    String s=ann.getText();
    int ix=s.indexOf( '\n' );
    if (ix>-1){
      s=s.substring( 0,ix ).trim();
    }
    return NLS.bind( UITexts.quickfix_marker_annotation_name, super.getText(), s);
    //return super.getText()+" ("+ann.getText()+" )";
  }

  @Override
  public void run(){
    IAnnotationModel model= getAnnotationModel();
    Position position= model.getPosition(ann);
    if (position == null) {
      return;
    }

    getTextEditor().selectAndReveal(position.offset, position.length);
    sourceViewer.getQuickAssistAssistant().showPossibleQuickAssists();

  }

  protected IAnnotationModel getAnnotationModel() {
    IDocumentProvider provider= getTextEditor().getDocumentProvider();
    return provider.getAnnotationModel(getTextEditor().getEditorInput());
  }


}
