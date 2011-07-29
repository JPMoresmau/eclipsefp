// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

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

/**
 * <p>
 * form section for tested-with compilers
 * </p>
 *
 * @author Leif Frenzel
 */
class TestedWithSection extends CabalFormSection {

  ActionContributionItem addAction;
  ActionContributionItem removeAction;

  TestedWithSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.advancedPage_cabalTestedWith, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    CompilerFormEntry compiler = new CompilerFormEntry();
    setCustomFormEntry( compiler, CabalSyntax.FIELD_TESTED_WITH, toolkit,
        container );
    GridData entryGD = new GridData( GridData.FILL_BOTH );
    entryGD.heightHint = 105;
    compiler.getControl().setLayoutData( entryGD );

    // Create toolbar
    ToolBarManager toolBarManager = new ToolBarManager( SWT.FLAT
        | SWT.HORIZONTAL );
    ToolBar toolbar = toolBarManager.createControl( this.getSection() );
    addAction = new ActionContributionItem( compiler.getAddAction() );
    removeAction = new ActionContributionItem( compiler.getRemoveAction() );
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
