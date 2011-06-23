package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntryMultiSelect;
import org.eclipse.jface.viewers.ITreeContentProvider;


public class MainIsFormEntry extends FormEntryMultiSelect {

  public MainIsFormEntry( final ITreeContentProvider contents ) {
    super( contents, true );
  }

  @Override
  public String getValue() {
    return super.getValue().replace( '.', '/' ).concat( ".hs" );
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    if (value == null || !value.endsWith( ".hs" )) {
      super.setValue( value, blockNotification );
    } else {
      String newValue = value.substring( 0, value.length() - 3 ).replace( '/', '.' );
      super.setValue( newValue, blockNotification );
    }
  }

}
