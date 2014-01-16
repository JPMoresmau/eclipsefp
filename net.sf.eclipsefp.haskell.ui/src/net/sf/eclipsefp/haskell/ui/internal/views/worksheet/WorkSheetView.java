/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.views.worksheet;

import java.util.List;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.IPage;
import org.eclipse.ui.part.PageBook;
import org.eclipse.ui.part.PageBookView;


/**
 * The view showing the worksheet: expressions to evaluate on the current haskell module
 * @author JP Moresmau
 *
 */
public class WorkSheetView extends PageBookView {
  public static final String ID="net.sf.eclipsefp.haskell.ui.internal.views.worksheet.WorkSheetView";
  /**
   *
   */
  public WorkSheetView() {

  }

  /**
   * evaluate the expressions in the current page if it's hooked to the given editor
   * @param editor
   */
  public void eval(final HaskellEditor editor){
    WorkSheetViewPage p=(WorkSheetViewPage)getCurrentPage();
    if (p!=null && p.getEditor()==editor){
      p.eval();
    }
  }

  /**
   * get all eval composites from the current page if it's hooked to the given editor
   * @param editor
   * @return
   */
  public List<EvalComposite> getEvalComposites(final HaskellEditor editor){
    WorkSheetViewPage p=(WorkSheetViewPage)getCurrentPage();
    if (p!=null && p.getEditor()==editor){
      return p.getEvalComposites();
    }
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.part.PageBookView#createDefaultPage(org.eclipse.ui.part.PageBook)
   */
  @Override
  protected WorkSheetViewPage createDefaultPage( final PageBook book ) {
    WorkSheetViewPage page=new WorkSheetViewPage();
    initPage( page );
    page.createControl(book);
    return page;
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.part.PageBookView#doCreatePage(org.eclipse.ui.IWorkbenchPart)
   */
  @Override
  protected PageRec doCreatePage( final IWorkbenchPart part ) {
    if (part instanceof HaskellEditor) {
      WorkSheetViewPage page=createDefaultPage(getPageBook());
      page.setEditor( (HaskellEditor )part);
      return new PageRec(part, page);
    }
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.part.PageBookView#doDestroyPage(org.eclipse.ui.IWorkbenchPart, org.eclipse.ui.part.PageBookView.PageRec)
   */
  @Override
  protected void doDestroyPage( final IWorkbenchPart part, final PageRec pageRecord ) {
    IPage page = pageRecord.page;
    page.dispose();
    pageRecord.dispose();

  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.part.PageBookView#getBootstrapPart()
   */
  @Override
  protected IWorkbenchPart getBootstrapPart() {
    IWorkbenchPage page = getSite().getPage();
    if (page != null) {
      return page.getActiveEditor();
    }

    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.part.PageBookView#isImportant(org.eclipse.ui.IWorkbenchPart)
   */
  @Override
  protected boolean isImportant( final IWorkbenchPart part ) {
    //We only care about Haskell editors
    return (part instanceof HaskellEditor);
  }

}
