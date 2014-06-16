/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.repl;

import java.util.Iterator;
import java.util.LinkedList;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.debug.ui.internal.HaskellDebugUI;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.console.TextConsolePage;
import org.eclipse.ui.console.TextConsoleViewer;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * the action writing the history command
 * @author JP Moresmau
 *
 */
public class HistoryAction extends Action  {
  private final StringBuilder current=new StringBuilder();
  private final LinkedList<String> commands=new LinkedList<String>();

  private int maxHistory=20;

  private int insertOffset=0;
  private int insertIndex=-1;
  /**
   * where the caret is in the line
   */
  private int caretPos = 0;

  public HistoryAction(final TextConsolePage p){
    super(UITexts.command_history,AbstractUIPlugin.imageDescriptorFromPlugin( HaskellDebugUI.getDefault().getBundle().getSymbolicName(), "icons/etool16/history16.gif" )); //$NON-NLS-1$
    IPreferencesService service = Platform.getPreferencesService();
    maxHistory=service.getInt(HaskellCorePlugin.getPluginId(), ICorePreferenceNames.RUN_COMMAND_HISTORY_MAX ,20,null);
    final TextConsoleViewer viewer=p.getViewer();

    final StyledText text=viewer.getTextWidget();

    text.addVerifyListener( new VerifyListener() {

      @Override
      public void verifyText( final VerifyEvent paramVerifyEvent ) {
        // more than one printable character: not a key press, append to current buffer
        if (paramVerifyEvent.text!=null && paramVerifyEvent.text.trim().length()>1){
          String t=paramVerifyEvent.text.trim();
          current.insert(caretPos,t);
          caretPos+=t.length();
          if (paramVerifyEvent.text.endsWith( "\r" ) || paramVerifyEvent.text.endsWith( "\n" )){ //$NON-NLS-1$ //$NON-NLS-2$

            String s=current.toString().trim();

            if (s.length()>0){
              commands.remove(s);
              commands.addFirst(s);
              if (commands.size()>maxHistory){
                commands.removeLast();
              }
              setEnabled( true );
            }
            insertIndex=-1;
            current.setLength( 0 );
            caretPos=0;
          }
        }
      }
    } );

    text.addKeyListener( new KeyAdapter() {

      /* (non-Javadoc)
       * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
       */
      @Override
      public void keyReleased( final KeyEvent e ) {
        if ((e.keyCode==SWT.ARROW_UP || e.keyCode==SWT.ARROW_DOWN) && (e.stateMask & SWT.CONTROL) == SWT.CONTROL && commands.size()>0){
          if (current.length()==0){
            insertOffset=text.getCharCount();
          } else {
            text.replaceTextRange( insertOffset, current.length(), "" ); //$NON-NLS-1$
            current.setLength( 0 );
            caretPos=0;
          }
          if (e.keyCode==SWT.ARROW_DOWN ){
            insertIndex--;
            if (insertIndex<0){
              insertIndex=commands.size()-1;
            }
          } else if (e.keyCode==SWT.ARROW_UP ){
            insertIndex++;
            if (insertIndex>=commands.size()){
              insertIndex=0;
            }
          }
          Iterator<String> it=commands.iterator();
          String toInsert=it.next();
          for (int a=0;a<insertIndex;a++){
            toInsert=it.next();
          }

          //current.append(toInsert); // done by verify listener
          text.append(toInsert);
          caretPos+=toInsert.length();
          text.setCaretOffset( text.getCharCount() );
        }
      }

      @Override
     public void keyPressed(final org.eclipse.swt.events.KeyEvent e) {
        char c=e.character;
        synchronized(current){
          // new line
          if (c=='\r'){
            String s=current.toString().trim();
            if (s.length()>0){
              commands.remove(s);
              commands.addFirst(s);
              if (commands.size()>maxHistory){
                commands.removeLast();
              }
              setEnabled( true );
            }
            insertIndex=-1;
            current.setLength( 0 );
            caretPos=0;
           // printable character
          } else if (!Character.isISOControl( c )){
            if (current.length()==0){
              insertOffset=text.getCharCount();
            }
            current.insert(caretPos,c);
            caretPos++;
            // backspace
          } else if (c=='\b' && current.length()>0){
            current.setLength( current.length()-1 );
            caretPos--;
          } else if (e.keyCode==SWT.ARROW_LEFT){
            caretPos--;
          } else if (e.keyCode==SWT.ARROW_RIGHT){
            if (caretPos<current.length()){
              caretPos++;
            }
          }
        }
      }

   } );



    setMenuCreator( new IMenuCreator() {

      @Override
      public Menu getMenu( final Menu arg0 ) {
        Menu m=new Menu(arg0);
        for (final String s:commands){
          MenuItem mi=new MenuItem( m, SWT.NONE );
          mi.setText( s );
          mi.addSelectionListener( new HistoryMenuSelectionListener( text, s ) );
        }
        return m;
      }

      @Override
      public Menu getMenu( final Control arg0 ) {
        Menu m=new Menu(arg0);
        for (final String s:commands){
          MenuItem mi=new MenuItem( m, SWT.NONE );
          mi.setText( s );
          mi.addSelectionListener( new HistoryMenuSelectionListener( text, s ) );
        }
        return m;
      }

      @Override
      public void dispose() {
        // NOOP

      }
    } );
    setEnabled( false );
  }



  public void dispose(){
   //NOOP
  }

  private class HistoryMenuSelectionListener extends SelectionAdapter {
    private final StyledText text;
    private final String command;

    public HistoryMenuSelectionListener( final StyledText text, final String command ) {
      super();
      this.text = text;
      this.command = command;
    }

    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    @Override
    public void widgetSelected( final SelectionEvent e ) {
      text.append( command);
      text.setCaretOffset( text.getCharCount() );

      Event event = new Event();
      event.widget = text;
      event.stateMask = 0;
      event.keyCode = 13;
      event.character = '\r';
      text.notifyListeners( SWT.KeyDown, event );

    }
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.action.Action#run()
   */
  @Override
  public void run() {
    // NOOP
  }
}