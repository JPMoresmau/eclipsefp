package net.sf.eclipsefp.haskell.ui.internal.preferences;

import java.util.StringTokenizer;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.ListEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * <p>Preferences page for search paths (for example open definition)</p>
  *
  * @author JP Moresmau
 */
public class SearchPathsPP extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
  public static final String SEP="|";

  private static final String[] defaults=new String[]{
    "file://${IMPL_BIN}/../doc/html/libraries/${PACKAGE_NAME}-${PACKAGE_VERSION}/${MODULE_HTML}#${ANCHOR}",
    "file://${IMPL_BIN}/../doc/html/libraries/${PACKAGE_NAME}-${PACKAGE_VERSION}/src/${MODULE_HTML}#${NAME}",
    "http://hackage.haskell.org/package/${PACKAGE_NAME}-${PACKAGE_VERSION}/docs/${MODULE_HTML}#${ANCHOR}",
    "http://hackage.haskell.org/packages/archive/${PACKAGE_NAME}/${PACKAGE_VERSION}/doc/html/${MODULE_HTML}#${ANCHOR}",
    "http://hackage.haskell.org/package/${PACKAGE_NAME}-${PACKAGE_VERSION}",
    "http://www.haskell.org/ghc/docs/${PACKAGE_VERSION}/html/libraries/${PACKAGE_NAME}/",
    "http://www.haskell.org/haskellwiki/Keywords#${NAME}"

} ;

  public static void initializeDefaults( final IPreferenceStore store ) {
    store.setDefault( IPreferenceConstants.HADDOCK_SEARCH_PATHS, createList(defaults ) );
  }

  public static String[] parseString( final String arg0 ) {
    StringTokenizer st=new StringTokenizer( arg0, SEP );
    int tokenCount = st.countTokens();
    String[] elements = new String[tokenCount];
    for (int i = 0; i < tokenCount; i++) {
      elements[i] = st.nextToken();
    }
    return elements;
  }

  public static String createList( final String[] arg0 ) {
    StringBuilder sb=new StringBuilder();
    for (String s:arg0){
      sb.append(s);
      sb.append(SEP);
    }
    return sb.toString();
  }



  public SearchPathsPP() {
    super();
    setPreferenceStore( HaskellUIPlugin.getDefault().getPreferenceStore() );
  }

  @Override
  public void init( final IWorkbench arg0 ) {
    // NOOP

  }

  @Override
  protected void createFieldEditors() {
     ListEditor leHaddock=new EntryModifiableListEditor(IPreferenceConstants.HADDOCK_SEARCH_PATHS,UITexts.preferences_search_haddock_description,getFieldEditorParent()) {

      @Override
      protected String[] parseString( final String arg0 ) {
        return SearchPathsPP.parseString(arg0);
      }

      @Override
      protected String getNewInputObject() {
        return getPath(defaults[1]);
      }

      private String getPath(final String original){
        InputDialog id=new InputDialog( getShell(), UITexts.preferences_search_haddock_new, UITexts.preferences_search_haddock_help, original, new IInputValidator() {

          @Override
          public String isValid( final String arg0 ) {
            if (arg0==null || arg0.length()==0){
              return UITexts.preferences_search_haddock_new_empty;
            }
            return null;
          }
        });
        int code=id.open();
        if (code==IDialogConstants.OK_ID){
          return id.getValue();
        }
        return null;
      }

      @Override
      protected String getModifiedEntry( final String original ) {
       return getPath(original);
      }

      @Override
      protected String createList( final String[] arg0 ) {
        return  SearchPathsPP.createList( arg0 );
      }
    };
    addField( leHaddock );
  }

}
