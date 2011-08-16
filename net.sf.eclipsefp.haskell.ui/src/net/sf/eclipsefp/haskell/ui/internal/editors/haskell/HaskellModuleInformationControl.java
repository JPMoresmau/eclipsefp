package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import org.eclipse.swt.widgets.Shell;


public class HaskellModuleInformationControl extends HaskellInformationControl {

  public HaskellModuleInformationControl( final Shell parent ) {
    super( parent );
  }

  @Override
  public void setInput( final Object input ) {
    if( BrowserPlugin.getSharedInstance().isDatabaseLoaded() ) {
      Module m = BrowserPlugin.getSharedInstance().getCachedModule(
          ( String )input );
      if( m != null && m.getDoc().trim().length() > 0 ) {
        setDocumentation( HtmlUtil.generateDocument( null, m.getDoc() ) );
      } else {
        setDocumentation( "" );
      }
    } else {
      setDocumentation( "" );
    }
  }
}
