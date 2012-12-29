/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.visual.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * @author JP Moresmau
 *
 */
public class Desktop extends Composite {

	/**
	 * @param parent
	 * @param style
	 */
	public Desktop(Composite parent, int style) {
		super(parent, style);
		setBackground(getShell().getDisplay().getSystemColor(SWT.COLOR_WHITE));
		setLayout(null);
		Bubble b=new Bubble(this,SWT.NONE);
		
		setBounds(0, 0, b);
	}
	
	public void setBounds(int x,int y,Control c){
		Point p=c.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		c.setBounds(x, y, p.x, p.y);
	}

	
	public static void main(String[] args) {
		Display display = new Display();
		Shell shell = new Shell(display);
		shell.setText ("Shell");
		shell.setSize (200, 200);
		shell.setLayout(new GridLayout(1,false));
		
		Desktop d=new Desktop(shell,SWT.NONE);
		d.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL));
		
		shell.open ();
		
		
		while (!shell.isDisposed ()) {
			if (!display.readAndDispatch ()) display.sleep ();
		}
		display.dispose ();
	}
}
