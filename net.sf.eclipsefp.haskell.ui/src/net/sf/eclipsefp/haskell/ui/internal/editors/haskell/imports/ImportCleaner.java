/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.JobFacade;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportClean;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportCleanHandler;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.texteditor.ITextEditor;


/**
 * @author JP Moresmau
 *
 */
public class ImportCleaner {

  public void cleanFile(final ITextEditor editor){
    if (editor instanceof HaskellEditor) {
      HaskellEditor hEditor = (HaskellEditor) editor;
      final IFile f=hEditor.findFile();
      if (f!=null){
        JobFacade bwf=BuildWrapperPlugin.getJobFacade( f.getProject() );
        if (bwf!=null){
          final IDocument d= ( ( HaskellEditor )editor ).getDocument();
          final Display display=Display.findDisplay( Thread.currentThread() );
          bwf.cleanImport( f, new ImportCleanHandler() {

            @Override
            public void handleImportCleans( final List<ImportClean> cleans ) {
              if (cleans!=null){
                Collections.sort( cleans,new Comparator<ImportClean>() {
                  /* (non-Javadoc)
                   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
                   */
                  @Override
                  public int compare( final ImportClean o1, final ImportClean o2 ) {
                   return Integer.valueOf(o1.getLocation().getStartLine()).compareTo(Integer.valueOf(o2.getLocation().getStartLine()));
                  }
                } );
                display.asyncExec( new Runnable(){
                  /* (non-Javadoc)
                   * @see java.lang.Runnable#run()
                   */
                  @Override
                  public void run() {
                    for (ImportClean cl:cleans){
                      try {
                        int start=cl.getLocation().getStartOffset( d );
                        int length=cl.getLocation().getLength( d );
                        d.replace( start , length, cl.getText() );
                      } catch (BadLocationException ble){
                        HaskellUIPlugin.log( ble );
                      }
                    }

                  }
                } );

              }

            }
          } );
        }

      }
    }
  }



}
