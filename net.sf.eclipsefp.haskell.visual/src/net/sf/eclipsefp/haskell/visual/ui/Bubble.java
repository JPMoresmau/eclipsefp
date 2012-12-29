/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.visual.ui;


import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Resource;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * @author JP Moresmau
 *
 */
public class Bubble extends Composite {
	private static final int SIZE_PIXELS=15;
	private static final int MIN_WIDTH=30;
	private static final int MIN_HEIGHT=30;
	
	private StyledText st;
	
	private List<Resource> resources=new ArrayList<Resource>();
	
	private boolean isSizing=false;
	private boolean isMoving=false;
	private int diffMoveX=0;
	private int diffMoveY=0;
	
	/**
	 * @param parent
	 * @param style
	 */
	public Bubble(final Desktop parent, int style) {
		super(parent, style);

		this.setLayout(new GridLayout(1, false));
		st=new StyledText(this, SWT.NONE);
		st.setLayoutData(new GridData(GridData.FILL_BOTH | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL));
		st.setText("first line\n\tsecond line\n\tthird line\n\nfourth line\n");
		
		final Cursor moveCursor=new Cursor(getDisplay(), SWT.CURSOR_SIZEALL);
		resources.add(moveCursor);
		final Cursor sizeCursor=new Cursor(getDisplay(), SWT.CURSOR_SIZESE);
		resources.add(sizeCursor);
		
		
		this.addMouseMoveListener(new MouseMoveListener() {
						
			@Override
			public void mouseMove(MouseEvent paramMouseEvent) {
				Rectangle r=getBounds();
				int x=paramMouseEvent.x;
				int y=paramMouseEvent.y;
				System.out.println(r.x+"->"+x);
				System.out.println(r.y+"->"+y);
				if (isSizing){
					if (x>MIN_WIDTH && y>MIN_HEIGHT){
						setSize(x, y);
					}
				} else if (isMoving){
					int newx=x-diffMoveX;
					int newy=y-diffMoveY;
					parent.setBounds(r.x+newx, r.y+newy, Bubble.this);
				} else {
					//if (getBounds().contains(x, y)){
						if (x>(r.width)-SIZE_PIXELS && y>(+r.height)-SIZE_PIXELS){
							setCursor(sizeCursor);
						} else {
							setCursor(moveCursor);
						}
					/*} else {
						setCursor(null);
					}*/
				}
			}
		});
		this.addMouseListener(new MouseListener() {
			
			@Override
			public void mouseUp(MouseEvent paramMouseEvent) {
				isSizing=false;
				isMoving=false;
			}
			
			@Override
			public void mouseDown(MouseEvent paramMouseEvent) {
				Rectangle r=getBounds();
				int x=paramMouseEvent.x;
				int y=paramMouseEvent.y;
				//if (getBounds().contains(x, y)){
					if (x>(r.width)-SIZE_PIXELS && y>(r.height)-SIZE_PIXELS){
						isSizing=true;
						isMoving=false;
						return;
					} else {
						isSizing=false;
						isMoving=true;
						diffMoveX=x;
						diffMoveY=y;
						return;
					}
				//} 
				//isSizing=false;
				//isMoving=false;
			}
			
			@Override
			public void mouseDoubleClick(MouseEvent paramMouseEvent) {
				// TODO Auto-generated method stub
				
			}
		});
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Widget#dispose()
	 */
	@Override
	public void dispose() {
		for (Resource r:resources){
			r.dispose();
		}
		super.dispose();
	}
	

}
