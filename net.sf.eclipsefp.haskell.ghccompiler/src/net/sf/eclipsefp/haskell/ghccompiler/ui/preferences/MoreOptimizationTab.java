// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.dialog.DialogField;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcParameter;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcParameterType;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/** <p>Tab for individual compiler optimizations on the ghc preference
  * page.</p>
  *
  * @author Leif Frenzel
  */
public class MoreOptimizationTab extends GhcCompilerTab  {

  public MoreOptimizationTab( final IPreferenceStore store ) {
    super( store );
  }

  // interface methods of Tab
  ///////////////////////////

  @Override
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 4, false ) );

    /*createField( composite, OPT_CASE_MERGE, 2 );
    createField( composite, OPT_DICTS_STRICT, 2 );
    createField( composite, OPT_DO_ETA_REDUCTION, 2 );
    createField( composite, OPT_DO_LAMBDA_ETA_EXPANSION, 2 );
    createField( composite, OPT_FOLDR_BUILD_ON, 2 );
    createField( composite, OPT_IGNORE_INTERFACE_PRAGMAS, 2 );
    createField( composite, OPT_LET_NO_ESCAPE, 2 );
    createField( composite, OPT_OMIT_INTERFACE_PRAGMAS, 2 );

    createField( composite, OPT_NO_CSE, 2 );
    createField( composite, OPT_NO_PRE_INLINING, 2 );
    createField( composite, OPT_NUMBERS_STRICT, 2 );
    createField( composite, OPT_USAGESP, 2 );
    */
    for (GhcParameter p:GhcParameter.values()){
      if (GhcParameterType.OPTIMIZATION_SPECIFIC.equals( p.getType() )){
        createField( composite, p, 2 );
      }
    }

    return composite;
  }

  private void createField( final Composite composite,
                            final GhcParameter p,
                            final int span ) {
    DialogField field = createBooleanField( composite, p );
    GridData gd = new GridData();
    gd.horizontalSpan = span;
    field.setLayoutData( gd );
  }
}