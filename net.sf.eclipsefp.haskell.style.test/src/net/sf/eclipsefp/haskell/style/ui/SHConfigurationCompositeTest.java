/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.ui;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.sf.eclipsefp.haskell.style.stylishhaskell.SHConfiguration;
import net.sf.eclipsefp.haskell.style.stylishhaskell.StylishHaskell;
import net.sf.eclipsefp.haskell.style.stylishhaskell.ui.SHConfigurationComposite;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * test composite
 * @author JP Moresmau
 *
 */
public class SHConfigurationCompositeTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Display display=new Display();

		Shell shell=new Shell(display);
		shell.setLayout(new GridLayout(1,true));
		final SHConfigurationComposite comp=new SHConfigurationComposite(shell, SWT.NONE);
		comp.setConfiguration(new SHConfiguration());
		Button b=new Button(shell,SWT.PUSH);
		b.setText("click");
		b.addSelectionListener(new SelectionAdapter() {
			/* (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				SHConfiguration conf=comp.getConfiguration();
				ByteArrayOutputStream baos=new ByteArrayOutputStream();
				try {
					StylishHaskell.save(conf, baos);
					System.out.println(new String(baos.toByteArray()));
				} catch (IOException ioe){
					ioe.printStackTrace();
				}
			}
		});
		shell.open();
		while (!shell.isDisposed ()) {
	         if (!display.readAndDispatch ())
	            display.sleep ();
	      }
	      display.dispose ();
	}

}
