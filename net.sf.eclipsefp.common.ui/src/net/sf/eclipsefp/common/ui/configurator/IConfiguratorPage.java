// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.configurator;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/** <p>TODO</p>
  *
  * @author Leif Frenzel
  */
public interface IConfiguratorPage {

  /** <p>creates the actual ui of this <code>IConfiguratorPage</code>. The 
    * returned control will appear in the configurator wizard on its own
    * wizard page.</p> */
  Control createControl( Composite parent );
  /** <p>called by the wizard when the user finishes. Implementors can use 
    * this to write information into preferences etc.</p> */
  void performFinish();
  /** <p>If the configurator page has registered an {@link IProbe IProbe}
    * implementation in the <code>probeClass</code> attribute of the
    * extension, this method is called after the probe has been run by the
    * user.</p> */
  void probed( Object result );
}
