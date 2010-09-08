package net.sf.eclipsefp.haskell.ui.internal.util;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;


public class UIUtils {

  // helping functions
  ////////////////////

  public static void createMessageLabel( final Composite parent,
                                    final String text,
                                    final int hspan,
                                    final int wrapwidth ) {
    Label label = new Label( parent, SWT.WRAP );
    label.setFont( parent.getFont() );
    label.setText( text );

    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.horizontalSpan = hspan;
    gridData.widthHint = wrapwidth;
    label.setLayoutData( gridData );
  }

  public static void createLineSpacer( final Composite parent, final int lines ) {
    Label lbl = new Label( parent, SWT.NONE );

    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.heightHint = lines;
    lbl.setLayoutData( gridData );
  }

  public static Composite createMainComposite( final Composite parent ) {
    Composite result = new Composite( parent, SWT.NONE );
    GridLayout gridLayout = new GridLayout( 2, false );
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    result.setLayout( gridLayout );
    result.setFont( parent.getFont() );
    return result;
  }

  public static Table createTable( final Composite parent ) {
    int style = SWT.CHECK | SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION;
    Table table = new Table( parent, style );
  
    GridData gridData = new GridData( GridData.FILL_BOTH );
    gridData.widthHint = 450;
    table.setLayoutData( gridData );
    table.setFont( parent.getFont() );
    table.setHeaderVisible( true );
    table.setLinesVisible( true );
  
    return table;
  }

}
