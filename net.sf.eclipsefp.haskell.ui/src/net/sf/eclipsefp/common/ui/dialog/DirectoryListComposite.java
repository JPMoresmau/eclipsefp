/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.common.ui.dialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;


/**
 * An extension of StringListComposite, for choosing directories
 * @author JP Moresmau
 *
 */
public class DirectoryListComposite extends StringListComposite {
  /**
   * reopen dialog on last chosen path for convenience
   */
  private static String lastPath=null;

  /**
   * @param parent
   * @param style
   */
  public DirectoryListComposite( final Composite parent, final int style ) {
    super( parent, style );

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.common.ui.dialog.StringListComposite#onAdd()
   */
  @Override
  protected String onAdd() {
    DirectoryDialog dd=new DirectoryDialog( getShell() ,SWT.OPEN);
    if (lastPath!=null){
      dd.setFilterPath( lastPath );
    }
    String s=dd.open();
    if (s!=null){
      lastPath=s;
    }
    return s;
  }

}
