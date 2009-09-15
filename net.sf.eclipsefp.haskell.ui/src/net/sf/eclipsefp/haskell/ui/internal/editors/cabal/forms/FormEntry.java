// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.IFormColors;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Hyperlink;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;

/** copied from PDE UI internals and modified
  *
  * The helper class for creating entry fields with label and text. Optionally,
  * a button can be added after the text. The attached listener reacts to all
  * the events. Entring new text makes the entry 'dirty', but only when 'commit'
  * is called is 'valueChanged' method called (and only if 'dirty' flag is set).
  * This allows delayed commit.
  */
public class FormEntry {

	private Control label;
	private Text textField;
	private Button btnBrowse;
	private String value = ""; //$NON-NLS-1$
	private boolean dirty;
	boolean isIgnoreModify = false;
	private IFormEntryListener formEntryListener;

	public static final int F_DEFAULT_TEXT_WIDTH_HINT = 100;

	public FormEntry( final Composite parent,
	                  final FormToolkit toolkit,
	                  final String labelText,
	                  final int style ) {
		createControl( parent, toolkit, labelText, style, null, false, 0, 0 );
	}

	public FormEntry( final Composite parent,
	                  final FormToolkit toolkit,
	                  final String labelText,
			              final String browseText,
			              final boolean linkLabel ) {
		this( parent, toolkit, labelText, browseText, linkLabel, 0 );
	}

	public FormEntry( final Composite parent,
	                  final FormToolkit toolkit,
	                  final String labelText,
			              final String browseText,
			              final boolean linkLabel,
			              final int indent ) {
		createControl( parent, toolkit, labelText, SWT.SINGLE, browseText, linkLabel, indent, 0 );
	}

	public FormEntry( final Composite parent,
	                  final FormToolkit toolkit,
	                  final String labelText,
			              final int indent,
			              final int tcolspan ) {
		createControl( parent, toolkit, labelText, SWT.SINGLE, null, false, indent, tcolspan );
	}

	private void createControl(final Composite parent, final FormToolkit toolkit,
			final String labelText, final int style, final String browseText, final boolean linkLabel, final int indent, final int tcolspan) {
		if (linkLabel) {
			Hyperlink link = toolkit.createHyperlink(parent, labelText,
					SWT.NULL);
			label = link;
		} else {
			if (labelText != null) {
				label = toolkit.createLabel(parent, labelText);
				label.setForeground(toolkit.getColors().getColor(IFormColors.TITLE));
			}
		}
		textField = toolkit.createText(parent, "", style); //$NON-NLS-1$
		addListeners();
		if (browseText != null) {
			btnBrowse = toolkit.createButton(parent, browseText, SWT.PUSH);
			btnBrowse.addSelectionListener(new SelectionAdapter() {
				@Override
        public void widgetSelected(final SelectionEvent e) {
					if (formEntryListener != null) {
            formEntryListener.browseButtonSelected(FormEntry.this);
          }
				}
			});
		}
		fillIntoGrid(parent, indent, tcolspan);
		// Set the default text width hint and let clients modify accordingly
		// after the fact
		setTextWidthHint(F_DEFAULT_TEXT_WIDTH_HINT);
	}
	public void setEditable(final boolean editable) {
		textField.setEditable(editable);
		if (label instanceof Hyperlink) {
      ((Hyperlink)label).setUnderlined(editable);
    }

		if (btnBrowse!=null) {
      btnBrowse.setEnabled(editable);
    }
	}
	private void fillIntoGrid(final Composite parent, final int indent, final int tcolspan) {
		Layout layout = parent.getLayout();
		int tspan;
		if (layout instanceof GridLayout) {
			int span = ((GridLayout) layout).numColumns;
			if (tcolspan > 0) {
        tspan = tcolspan;
      } else {
        tspan = btnBrowse != null ? span - 2 : span - 1;
      }
			GridData gd;
			if (label != null) {
				gd = new GridData(GridData.VERTICAL_ALIGN_CENTER);
				gd.horizontalIndent = indent;
				label.setLayoutData(gd);
			}
			gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
			gd.horizontalSpan = tspan;
			if (label != null) {
				gd.horizontalIndent = 3;
			}
			gd.grabExcessHorizontalSpace = (tspan == 1);
			gd.widthHint = 10;

	    if ((textField.getStyle() & SWT.MULTI)>0){
	      gd.heightHint=100;
	    }
			textField.setLayoutData(gd);
			if (btnBrowse != null) {
				gd = new GridData(GridData.VERTICAL_ALIGN_CENTER);
				btnBrowse.setLayoutData(gd);
			}
		} else if (layout instanceof TableWrapLayout) {
			int span = ((TableWrapLayout) layout).numColumns;
			if (tcolspan > 0) {
        tspan = tcolspan;
      } else {
        tspan = btnBrowse != null ? span - 2 : span - 1;
      }
			TableWrapData td;
			if (label != null) {
				td = new TableWrapData();
				td.valign = TableWrapData.MIDDLE;
				td.indent = indent;
				label.setLayoutData(td);
			}
			td = new TableWrapData(TableWrapData.FILL);
			td.colspan = tspan;
			if (label != null) {
				td.indent = 3;
			}
			td.grabHorizontal = (tspan == 1);
			td.valign = TableWrapData.MIDDLE;
			textField.setLayoutData(td);
			if (btnBrowse != null) {
				td = new TableWrapData(TableWrapData.FILL);
				td.valign = TableWrapData.MIDDLE;
				btnBrowse.setLayoutData(td);
			}
		}
	}

	public void setFormEntryListener( final IFormEntryListener listener ) {
    if( label != null && label instanceof Hyperlink ) {
      if( this.formEntryListener != null ) {
        ( ( Hyperlink )label ).removeHyperlinkListener( this.formEntryListener );
      }
      if( listener != null ) {
        ( ( Hyperlink )label ).addHyperlinkListener( listener );
      }
    }
    this.formEntryListener = listener;
  }

	/**
	 * If dirty, commits the text in the widget to the value and notifies the
	 * listener. This call clears the 'dirty' flag.
	 */
	public void commit() {
	  if (dirty) {
			value = textField.getText();
			//notify
			if (formEntryListener != null) {
        formEntryListener.textValueChanged(this);
      }
		}
		dirty = false;
	}

	public void cancelEdit() {
		dirty = false;
	}

	public Text getText() {
		return textField;
	}

	public Control getLabel() {
		return label;
	}

	public Button getButton() {
		return btnBrowse;
	}

	/**
	 * Returns the current entry value. If the entry is dirty and was not
	 * commited, the value may be different from the text in the widget.
	 */
	public String getValue() {
		return value.trim();
	}

	/** Returns true if the text has been modified. */
	public boolean isDirty() {
		return dirty;
	}

	public void setValue( final String value ) {
		if( textField != null ) {
			textField.setText( value != null ? value : "" ); //$NON-NLS-1$
		}
		this.value = ( value != null ) ? value : ""; //$NON-NLS-1$
	}

	/** Sets the value of this entry with the possibility to turn the
  	* notification off. */
	public void setValue( final String value, final boolean blockNotification ) {
		isIgnoreModify = blockNotification;
		setValue( value );
		isIgnoreModify = false;
	}

	public void setVisible( final boolean visible ) {
		if( label != null ) {
			label.setVisible( visible );
		}
		if( textField != null ) {
			textField.setVisible( visible );
		}
		if( btnBrowse != null ) {
			btnBrowse.setVisible( visible );
		}
	}

	/** If GridData was used, set the width hint.  If TableWrapData was used
	  * set the max width.  If no layout data was specified, this method does
	  * nothing. */
	public void setTextWidthHint( final int width ) {
		Object data = getText().getLayoutData();
		if( data instanceof GridData ) {
			( ( GridData )data ).widthHint = width;
		} else if( data instanceof TableWrapData ) {
			( ( TableWrapData )data ).maxWidth = width;
		}
	}


	// helping functions
	////////////////////

  private void keyReleaseOccured( final KeyEvent evt ) {
    if( evt.character == '\r' ) {
      // commit value
      if( dirty ) {
        commit();
      }
    } else if( evt.character == '\u001b' ) { // Escape character
      if( !value.equals( textField.getText() ) ) {
        textField.setText( value != null ? value : "" ); // restore old //$NON-NLS-1$
      }
      dirty = false;
    }
    if( formEntryListener != null ) {
      formEntryListener.selectionChanged( FormEntry.this );
    }
  }

  private void editOccured( final ModifyEvent evt ) {
    if( !isIgnoreModify ) {
      dirty = true;
      if( formEntryListener != null ) {
        formEntryListener.textDirty( this );
      }
    }
  }

  private void addListeners() {
    textField.addKeyListener( new KeyAdapter() {
      @Override
      public void keyReleased( final KeyEvent evt ) {
        keyReleaseOccured( evt );
      }
    } );
    textField.addModifyListener( new ModifyListener() {
      public void modifyText( final ModifyEvent evt ) {
        if( !textField.getText().equals( value ) ) {
          editOccured( evt );
        }
      }
    } );
    textField.addFocusListener( new FocusAdapter() {
      @Override
      public void focusGained( final FocusEvent evt ) {
        if( formEntryListener != null ) {
          formEntryListener.focusGained( FormEntry.this );
        }
      }
      @Override
      public void focusLost( final FocusEvent evt ) {
        if( dirty ) {
          commit();
        }
      }
    } );
  }
}
