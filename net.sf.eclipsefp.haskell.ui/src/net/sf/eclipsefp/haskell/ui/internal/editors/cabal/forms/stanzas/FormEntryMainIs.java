package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

/**
 * Form entry for selecting the 'main-is' module for an executable.
 * @author Alejandro Serrano
 *
 */
public class FormEntryMainIs extends FormEntryModules {

  public FormEntryMainIs( final String exposedString ) {
    super( exposedString, true );
  }

  @Override
  public String getValue() {
    String value = super.getValue();
    return value.length()==0 ? "" : value.replace( '.', '/' ).concat( ".hs" );
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    String realValue = (value == null) ? "" : value;

    if (realValue.trim().equals( getValue().trim() )) {
      return;
    }

    if (!realValue.endsWith( ".hs" )) {
      super.setValue( realValue, blockNotification );
    } else {
      String newValue = realValue.substring( 0, realValue.length() - 3 ).replace( '/', '.' );
      super.setValue( newValue, blockNotification );
    }
  }

}
