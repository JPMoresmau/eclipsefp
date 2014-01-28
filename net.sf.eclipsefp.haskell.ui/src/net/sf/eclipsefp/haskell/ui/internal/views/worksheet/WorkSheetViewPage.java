/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.views.worksheet;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.SharedScrolledComposite;
import org.eclipse.ui.part.Page;


/**
 * A page of expressions to evaluae on the linked haskell editor
 * @author JP Moresmau
 *
 */
public class WorkSheetViewPage extends Page {
  /**
   * the type of markers
   * we use markers to save expressions on files, since they're persistent and unlimited in size
   */
  public static String MARKER_TYPE="net.sf.eclipsefp.haskell.ui.worksheet";
  /**
   * the expression attribute
   */
  public static String MARKER_EXPRESSION="expression";
  /**
   * the index attribute, to ensure consistent ordering over restarts
   */
  public static String MARKER_INDEX="index";
  /**
   * type of result
   */
  public static String MARKER_RESULT_TYPE="resultType";

  /**
   * the hooked editor
   */
  private HaskellEditor editor;

  /**
   * the main composite
   */
  private Composite mainComposite;
  /**
   * the composite for scrolling
   */
  private SharedScrolledComposite sc1;

  /**
   * the list of evaluation composite, one for each expression, in index order
   */
  private final List<EvalComposite> evalComposites=new ArrayList<EvalComposite>();

  private final AddExpressionAction addAction=new AddExpressionAction();
  private final RemoveAllExpressionsAction removeAllAction=new RemoveAllExpressionsAction();

  /**
   *
   */
  public WorkSheetViewPage() {

  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.part.Page#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent ) {
    Composite mainComp= new Composite(parent, SWT.NONE);
    mainComp.setFont(parent.getFont());
    GridData gd=new GridData(SWT.FILL,SWT.FILL,true,true);
    mainComp.setLayoutData( gd );
    //GridLayout layout= new GridLayout(1,true);
    FillLayout layout=new FillLayout();
    layout.marginHeight= 0;
    layout.marginWidth= 0;
    mainComp.setLayout(layout);

    sc1 = new SharedScrolledComposite(mainComp,SWT.V_SCROLL | SWT.H_SCROLL){
      // no implementation required
    };
    //gd=new GridData(SWT.FILL,SWT.FILL,true,true);
    sc1.setLayoutData(gd);
    mainComposite=new Composite(sc1,SWT.NONE);
    sc1.setExpandHorizontal(true);
    sc1.setExpandVertical(true);
    sc1.setContent( mainComposite );

    //RowLayout l=new RowLayout(SWT.VERTICAL);
    //l.fill=true;
    //l.pack=true;
    GridLayout l=new GridLayout( 1, true );
    l.marginBottom=0;
    l.marginHeight=0;
    l.marginLeft=0;
    l.marginRight=0;
    l.marginTop=0;
    l.marginWidth=0;


    mainComposite.setLayout( l );
    mainComposite.setBackground( mainComposite.getDisplay().getSystemColor( SWT.COLOR_LIST_BACKGROUND ) );
    //mainComposite.setBackground( parent.getBackground() );
    removeAllAction.setEnabled(false);
    build();
    layout();

    //sc1.reflow( true );
    addAction.setEnabled(false);
    IActionBars actionBars= getSite().getActionBars();
    registerToolbarActions(actionBars);
  }

  private void registerToolbarActions( final IActionBars actionBars ) {
    IToolBarManager toolBarManager= actionBars.getToolBarManager();
    toolBarManager.add(addAction);
    toolBarManager.add( removeAllAction );
  }

  /**
   * relayout with scrolling
   */
  public void layout(){
    sc1.reflow( true );
    mainComposite.layout();
  }

  /**
   * @return the evalComposites
   */
  public List<EvalComposite> getEvalComposites() {
    return evalComposites;
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.part.Page#getControl()
   */
  @Override
  public Control getControl() {
    if (sc1!=null && !sc1.isDisposed()){
      return sc1.getParent();
    }
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.part.Page#setFocus()
   */
  @Override
  public void setFocus() {
    if (mainComposite!=null && !mainComposite.isDisposed()){
      mainComposite.setFocus();
    }

  }

  public HaskellEditor getEditor() {
    return editor;
  }


  public void setEditor( final HaskellEditor editor ) {
    this.editor = editor;
    addAction.setEnabled( this.editor!=null );
    build();
  }

  /**
   * build UI from file markers
   */
  private void build(){
    if (mainComposite!=null && !mainComposite.isDisposed()){
      for (Control c:mainComposite.getChildren()){
        c.dispose();
      }
      evalComposites.clear();
      if (this.editor!=null){
        final IFile f=this.editor.findFile();
        if (f!=null){


          try {
            IMarker[] mks=f.findMarkers( MARKER_TYPE, true, IResource.DEPTH_ZERO );
            List<EvalExpression> exprs=new ArrayList<EvalExpression>();
            for (IMarker mk:mks){
              EvalExpression expr=new EvalExpression(mk);
              if (expr.isValid()){
                exprs.add( expr );
              }
            }
            Collections.sort( exprs );
            for (EvalExpression expr:exprs){
              addComposite(expr);
            }
            eval();
          } catch (CoreException ce){
            HaskellUIPlugin.log( ce );
          }
        }
      }
    }
  }

  /**
   * save expression to file markers
   */
  public void save(){
    final IFile f=this.editor.findFile();
    if (f!=null){
      try {
        // delete existing
        f.deleteMarkers(  MARKER_TYPE, true, IResource.DEPTH_ZERO );
        // recreate
        for (EvalComposite ec:evalComposites){
          ec.getEvalExpression().addMarker( f );
        }
      } catch (CoreException ce){
        HaskellUIPlugin.log( ce );
      }
    }

  }

  /**
   * remove a given expression from the list
   * remove the associated marker
   * ensure indices are kept consistent
   * @param exprToRemove
   */
  private void remove(final EvalExpression exprToRemove){
    final IFile f=this.editor.findFile();
    if (f!=null){
      try {
        IMarker[] mks=f.findMarkers( MARKER_TYPE, true, IResource.DEPTH_ZERO );
        List<EvalExpression> exprs=new ArrayList<EvalExpression>();
        int remove=0;
        for (IMarker mk:mks){
          EvalExpression expr=new EvalExpression(mk);
          if (expr.getExpression().equals(exprToRemove.getExpression()) && expr.getIndex()==exprToRemove.getIndex()){
            mk.delete();
            remove++;
          }
          if (expr.isValid()){
            exprs.add( expr );
          }
        }
        for (EvalExpression e:exprs){
          if (e.getIndex()>exprToRemove.getIndex()){
            e.setIndex( e.getIndex()-remove );
          }
        }
      } catch (CoreException ce){
        HaskellUIPlugin.log( ce );
      }
    }
  }

  /**
   * remove a evaluation composite and the associated expression marker
   * @param comp
   */
  public void remove(final EvalComposite comp){
    evalComposites.remove( comp );
    remove(comp.getEvalExpression());
    save();
    comp.dispose();
    removeAllAction.setEnabled( evalComposites.size()>0 );
    layout();

  }


  /**
   * @return the mainComposite
   */
  public Composite getMainComposite() {
    return mainComposite;
  }

  /**
   * add an expression in a new composite
   * @param expr the expression
   * @return the created composite
   */
  private EvalComposite addComposite(final EvalExpression expr){
    EvalComposite ec=new EvalComposite( this, expr );
    GridData gd=new GridData(GridData.FILL_HORIZONTAL);
    ec.setLayoutData( gd );
    evalComposites.add(ec);
    removeAllAction.setEnabled( true );
    return ec;
  }

  /**
   * launch evaluation of all expressions in a job
   */
  public void eval(){
    if (this.editor!=null){
      final IFile f=this.editor.findFile();
      if (f!=null){
        BWFacade bwf=BuildWrapperPlugin.getFacade( f.getProject() );
        if (bwf!=null){
          // clone the list to avoid potential concurrent modifications
          BuildWrapperPlugin.getJobFacade( f.getProject() ).eval( f, new ArrayList<EvalComposite>(evalComposites) );
        }
      }
    }
  }

  /**
   * action: add a new expression
   *
   * @author JP Moresmau
   *
   */
  private class AddExpressionAction extends Action{
    public AddExpressionAction(){
      super();
      PlatformUI.getWorkbench().getHelpSystem().setHelp(this, "WorkSheet.AddExpressionAction"); //$NON-NLS-1$
      setText(UITexts.worksheet_addexpression);
      setToolTipText(UITexts.worksheet_addexpression);
      setDescription(UITexts.worksheet_addexpression);
      setImageDescriptor( PlatformUI.getWorkbench().getSharedImages().getImageDescriptor( ISharedImages.IMG_OBJ_ADD ) );
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
     EvalExpressionDialog id=new EvalExpressionDialog( getSite().getShell(), UITexts.worksheet_addexpression_title, UITexts.worksheet_addexpression_message, new EvalExpression() );
     if (id.open()==Window.OK){
       EvalExpression expr=new EvalExpression();
       expr.setExpression( id.getValue() );
       expr.setIndex( evalComposites.size() );
       expr.setResultType( id.getResultType() );
       EvalComposite ec= addComposite( expr );
       final IFile f=editor.findFile();
       if (f!=null){
         try {
           expr.addMarker( f);
         }catch (CoreException ce){
           HaskellUIPlugin.log( ce );
         }
         BWFacade bwf=BuildWrapperPlugin.getFacade( f.getProject() );
         if (bwf!=null){
           BuildWrapperPlugin.getJobFacade( f.getProject() ).eval( f, Collections.singletonList( ec ) );
         }
       }
       layout();
     }
    }
  }

  /**
   * action: remove all expressions
   *
   * @author JP Moresmau
   *
   */
  private class RemoveAllExpressionsAction extends Action{

    /**
     *
     */
    public RemoveAllExpressionsAction() {
      super();
      PlatformUI.getWorkbench().getHelpSystem().setHelp(this, "WorkSheet.RemoveAllExpressionsAction"); //$NON-NLS-1$
      setText(UITexts.worksheet_removeallexpressions_tooltip);
      setToolTipText(UITexts.worksheet_removeallexpressions_tooltip);
      setDescription(UITexts.worksheet_removeallexpressions_tooltip);
      setImageDescriptor( PlatformUI.getWorkbench().getSharedImages().getImageDescriptor( ISharedImages.IMG_ELCL_REMOVEALL ) );

    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
      if (MessageDialog.openConfirm( getSite().getShell(), UITexts.worksheet_removeallexpressions_title, UITexts.worksheet_removeallexpressions_message )){
        for (EvalComposite c:evalComposites){
          c.dispose();
        }
        evalComposites.clear();
        save();
        layout();
        removeAllAction.setEnabled( false );
      }
    }
  }
}
