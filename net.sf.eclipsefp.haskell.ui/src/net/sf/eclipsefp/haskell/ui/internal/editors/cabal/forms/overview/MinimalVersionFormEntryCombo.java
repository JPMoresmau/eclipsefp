package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.Choice;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntryCombo;


public class MinimalVersionFormEntryCombo<T> extends FormEntryCombo<T> {

  public MinimalVersionFormEntryCombo( final Choice<T> choices ) {
    super(choices);
  }

  @Override
  public String getValue() {
    return ">= " + super.getValue();
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    if (value == null) {
      super.setValue( null, blockNotification );
    } else {
      if (value.startsWith( ">= " )) {
        super.setValue( value.substring( 3 ), blockNotification );
      } else {
        super.setValue( value, blockNotification );
      }
    }
  }
}
