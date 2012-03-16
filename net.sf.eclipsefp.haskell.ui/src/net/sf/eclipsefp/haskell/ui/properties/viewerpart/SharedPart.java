// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties.viewerpart;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

public abstract class SharedPart {

  private boolean enabled;
  private final String[] buttonLabels;
  private Control[] controls;
  private Composite buttonContainer;

  public SharedPart( final String[] buttonLabels ) {
    this.buttonLabels = buttonLabels;
    enabled = true;
  }

  public void setButtonEnabled( final int index, final boolean enabled ) {
    if( controls != null && index >= 0 && controls.length > index ) {
      Control c = controls[ index ];
      if( c instanceof Button ) {
        c.setEnabled( enabled );
      }
    }
  }

  public void createControl( final Composite parent,
                             final int style,
                             final int span ) {
    createMainControl( parent, style, span - 1 );
    if( buttonLabels != null && buttonLabels.length > 0 ) {
      buttonContainer = new Composite( parent, SWT.NULL );
      GridData gd = new GridData( GridData.FILL_VERTICAL );
      buttonContainer.setLayoutData( gd );
      buttonContainer.setLayout( createButtonsLayout() );

      controls = new Control[ buttonLabels.length ];
      SelectionHandler listener = new SelectionHandler( this );
      for( int i = 0; i < buttonLabels.length; i++ ) {
        String label = buttonLabels[ i ];
        if( label != null ) {
          Button button = createButton( buttonContainer, label, i );
          button.addSelectionListener( listener );
          controls[ i ] = button;
        } else {
          createEmptySpace( buttonContainer, 1 );
        }
      }
    }
  }


  // template methods
  ///////////////////

  protected abstract void createMainControl( Composite parent,
                                             int style,
                                             int span );

  protected abstract void buttonSelected( Button button, int index );

  protected abstract void createMainLabel( Composite parent, int span );

  protected Button createButton( final Composite parent,
                                 final String label,
                                 final int index ) {
    Button button = new Button( parent, SWT.PUSH );
    button.setText( label );
    GridData gd = new GridData(   GridData.FILL_HORIZONTAL
                                | GridData.VERTICAL_ALIGN_BEGINNING );
    button.setLayoutData( gd );
    button.setData( new Integer( index ) );
    return button;
  }

  protected void updateEnabledState() {
    for( int i = 0; i < controls.length; i++ ) {
      Control c = controls[ i ];
      if( c instanceof Button ) {
        c.setEnabled( isEnabled() );
      }
    }
  }


  // helping methods
  //////////////////

  private GridLayout createButtonsLayout() {
    GridLayout layout = new GridLayout();
    layout.marginWidth = 0;
    layout.marginHeight = 0;
    return layout;
  }

  protected Label createEmptySpace( final Composite parent, final int span ) {
    Label label = new Label( parent, SWT.NULL );
    GridData gd = new GridData( GridData.VERTICAL_ALIGN_BEGINNING );
    gd.horizontalSpan = span;
    gd.widthHint = 0;
    gd.heightHint = 0;
    label.setLayoutData( gd );
    return label;
  }


  // attribute getters and setters
  ////////////////////////////////

  public void setEnabled( final boolean enabled ) {
    if( enabled != this.enabled ) {
      this.enabled = enabled;
      updateEnabledState();
    }
  }

  public boolean isEnabled() {
    return enabled;
  }


  // inner classes
  ////////////////

  private class SelectionHandler implements SelectionListener {

    private final SharedPart part;

    private SelectionHandler( final SharedPart part ) {
      this.part = part;
    }

    @Override
    public void widgetSelected( final SelectionEvent e ) {
      buttonSelected( e );
    }

    @Override
    public void widgetDefaultSelected( final SelectionEvent e ) {
      buttonSelected( e );
    }

    private void buttonSelected( final SelectionEvent e ) {
      Integer index = ( Integer )e.widget.getData();
      part.buttonSelected( ( Button )e.widget, index.intValue() );
    }
  }
}