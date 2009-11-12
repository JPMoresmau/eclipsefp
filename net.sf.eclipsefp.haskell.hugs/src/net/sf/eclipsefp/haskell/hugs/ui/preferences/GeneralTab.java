// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.ui.preferences;

import static net.sf.eclipsefp.haskell.core.util.ResourceUtil.NL;
import java.io.*;
import net.sf.eclipsefp.common.ui.dialog.ExecutableDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.haskell.hugs.core.preferences.IHugsPreferenceNames;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;


/** <p>The tab on the HUGS preference page that displays general
  * information.</p>
  *
  * @author Leif Frenzel
  */
public class GeneralTab extends Tab implements IHugsPreferenceNames {

  public GeneralTab( final IPreferenceStore store ) {
    super( store );
  }

  // interface methods of Tab
  ///////////////////////////

  @Override
  public Control createControl( final Composite parent ) {
    String labelText = "HUGS executable";
    ExecutableDialogField dlgField = new ExecutableDialogField( parent,
                                                                labelText ){

      @Override
      protected String createDisplayContent( final String info ) {
    	  try {
    		  	File f=new File(info);
    		  	if (f.exists() && f.canRead()){
			        Process p=Runtime.getRuntime().exec(info);

			        InputStream is=p.getInputStream();
			        // read all text given by the console till the Hugs> prompt
			        // this rely on the > at the prompt
			        int r=is.read();
			        while (((char)r)!='>' && r>-1){
			        	r=is.read();
			        }
			        // read first space
			        is.read();
			        OutputStream os=p.getOutputStream();
			        // write version command
			        os.write((":version"+NL).getBytes());
			        os.flush();
			        StringBuffer sb=new StringBuffer();
			        r=is.read();
			        // read everything till the first line return
			        while (((char)r)!='\n' && ((char)r)!='\r' && r>-1){
			        	sb.append((char)r);
			        	r=is.read();
			        }
			        // read the rest
			        while (is.available()>0){
			        	is.read();
			        }
			        // quit
			        os.write((":quit"+NL).getBytes());
			        os.flush();
			        os.close();
			        is.close();
			        // the version, usually starting with --
			        String version=sb.toString();
			        if (version.startsWith("--")){
			        	version=version.substring(2).trim();
			        }
			        return version;
    		  	}
    		  	return "Not found";
    	  } catch (IOException ioe){
    		  ioe.printStackTrace();
    		  return ioe.getMessage();
    	  }
      }
    };

    dlgField.setInfo( getPreferenceStore().getString( EXECUTABLE_NAME ) );

    dlgField.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        getPreferenceStore().setValue( EXECUTABLE_NAME, ( String )newInfo );
      }
    } );
    return dlgField;
  }
}