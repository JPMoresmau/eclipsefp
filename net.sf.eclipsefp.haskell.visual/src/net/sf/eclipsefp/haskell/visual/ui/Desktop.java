/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.visual.ui;

import net.sf.eclipsefp.haskell.visual.ui.text.VisualTexts;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

/**
 * @author JP Moresmau
 *
 */
public class Desktop extends Composite {
	private Point mouseDownPoint;
	
	/**
	 * @param parent
	 * @param style
	 */
	public Desktop(Composite parent, int style) {
		super(parent, style);
		setBackground(getShell().getDisplay().getSystemColor(SWT.COLOR_WHITE));
		setLayout(null);
		Bubble b=new Bubble(this,SWT.NONE);
		Menu ctxMenu=new Menu(this);
		
		addMouseListener(new MouseAdapter() {
			/* (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDown(MouseEvent e) {
				mouseDownPoint=new Point(e.x, e.y);
			}
		});
		
		MenuItem ctxAdd=new MenuItem(ctxMenu, SWT.PUSH);
		ctxAdd.setText(VisualTexts.desktop_bubble_new);
		ctxAdd.addSelectionListener(new SelectionAdapter() {
			/* (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				Bubble nb=new Bubble(Desktop.this,SWT.NONE);
				
				Desktop.this.setBounds(mouseDownPoint!=null?mouseDownPoint.x:e.x, mouseDownPoint!=null?mouseDownPoint.y:e.y, nb);
			}
		});
		setMenu(ctxMenu);
		
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
