/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class DependenciesSection extends CabalFormSection {

  ActionContributionItem addAction;
  ActionContributionItem removeAction;

  DependenciesSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.cabalEditor_dependencies, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    DependenciesFormEntry entry = new DependenciesFormEntry();
    setCustomFormEntry( entry, CabalSyntax.FIELD_BUILD_DEPENDS, toolkit,
        container );
    GridData entryGD = new GridData( GridData.FILL_BOTH );
    entryGD.heightHint = 120;
    entry.getControl().setLayoutData( entryGD );

    // Create toolbar
    ToolBarManager toolBarManager = new ToolBarManager( SWT.FLAT
        | SWT.HORIZONTAL );
    ToolBar toolbar = toolBarManager.createControl( this.getSection() );
    addAction = new ActionContributionItem( entry.getAddAction() );
    removeAction = new ActionContributionItem( entry.getRemoveAction() );
    toolBarManager.add( addAction );
    toolBarManager.add( removeAction );
    toolBarManager.update( true );
    this.getSection().setTextClient( toolbar );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

  @Override
  public void setAllEditable( final boolean editable ) {
    super.setAllEditable( editable );
    addAction.getAction().setEnabled( editable );
    removeAction.getAction().setEnabled( editable );
  }
}
