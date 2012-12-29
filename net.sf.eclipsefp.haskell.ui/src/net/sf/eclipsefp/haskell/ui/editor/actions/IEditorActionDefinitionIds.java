// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.editor.actions;


/** EclipseFP action definition identifiers; these should match the &lt;action&gt; definitionId, which then match
 * the &lt;command;&gt; definitions for the action.
  *
  * @author Leif Frenzel
  */
public interface IEditorActionDefinitionIds {
  final static String ActionPrefix = IEditorActionDefinitionIds.class.getPackage().getName() + ".";

  public final static String UNCOMMENT         = ActionPrefix.concat( "uncomment" ); //$NON-NLS-1$
  public final static String COMMENT           = ActionPrefix.concat( "comment" ); //$NON-NLS-1$
  public final static String COMMENT_PRAGMA    = ActionPrefix.concat( "comment_pragma" ); //$NON-NLS-1$
  public final static String FIRST_CHAR        = ActionPrefix.concat( "firstChar" ); //$NON-NLS-1$
  public final static String OPEN_DEFINITION   = ActionPrefix.concat( "openDefinition" ); //$NON-NLS-1$
  public final static String HADDOCK_FOLLOWING = ActionPrefix.concat( "haddock_following" ); //$NON-NLS-1$
  public final static String HADDOCK_PREVIOUS  = ActionPrefix.concat( "haddock_previous" ); //$NON-NLS-1$
  public final static String HADDOCK_BLOCK_FOLLOWING = ActionPrefix.concat( "haddock_block_following" ); //$NON-NLS-1$
  public final static String FORMAT           = ActionPrefix.concat( "format" ); //$NON-NLS-1$
  public final static String IMPORTS           = ActionPrefix.concat( "imports" ); //$NON-NLS-1$
}
