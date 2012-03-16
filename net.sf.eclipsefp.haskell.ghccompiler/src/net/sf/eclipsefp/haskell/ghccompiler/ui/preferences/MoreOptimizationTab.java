// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import net.sf.eclipsefp.common.ui.dialog.DialogField;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcParameter;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcParameterType;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.PropertyChangeEvent;
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
  private final List<DialogField> fields=new LinkedList<DialogField>();

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
        fields.add( createField( composite, p, 2 ));
      }
    }

    return composite;
  }

  private DialogField createField( final Composite composite,
                            final GhcParameter p,
                            final int span ) {
    DialogField field = createBooleanField( composite, p );
    GridData gd = new GridData();
    gd.horizontalSpan = span;
    field.setLayoutData( gd );
    return field;
  }

  @Override
  public void propertyChange( final PropertyChangeEvent event ) {
    Iterator<DialogField> it=fields.iterator();
    for (GhcParameter p:GhcParameter.values()){
      if (GhcParameterType.OPTIMIZATION_SPECIFIC.equals( p.getType() )){
        it.next().setInfo( getFromStore(p.getName()) );
      }
    }

  }
}