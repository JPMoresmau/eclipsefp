package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;


public class ImportsManager {

  private final IDocument doc;

  public ImportsManager(final IDocument doc) {
    this.doc = doc;
  }

  public ArrayList<AnImport> parseImports() {
    ArrayList<AnImport> r = new ArrayList<AnImport>();
    int lines = doc.getNumberOfLines();
    for (int line = 0; line < lines; line++) {
      try {
        IRegion reg = doc.getLineInformation( line );
        String contents = doc.get( reg.getOffset(), reg.getLength() );

        // Take blanks appart
        int realInit = reg.getOffset();
        int realLength = reg.getLength();
        for (int i = 0; i < contents.length(); i++) {
          if (Character.isWhitespace( i )) {
            realInit++;
            realLength--;
          } else {
            break;
          }
        }

        // Get contents another time
        reg = new Region( realInit, realLength );
        contents = doc.get( reg.getOffset(), reg.getLength() );
        if (contents.startsWith( "import" )) {
          // We are in an import declaration
          String[] words = contents.split( "[ \t\n\r]+" );
          if (words.length > 1) {
            // We are in an import with more than "import" on it
            int namePlace = 1;
            boolean isQualified = false;
            // See if we have a "qualified"
            if (words[1].equals("qualified")) {
              namePlace = 2;
              isQualified = true;
            }
            // Take the name of the import
            String name = words[namePlace];
            String items = null;
            String qualifiedName = null;
            boolean isHiding = false;
            // See if we have more things
            if (words.length > namePlace + 1) {
              int nextThings = namePlace + 1;
              // Maybe we have a hiding clause
              if (words[nextThings].equals("hiding")) {
                nextThings++;
                isHiding = true;
              }
              // Try to find '(' and ')'
              int beginPar = contents.indexOf( '(' );
              if (beginPar != -1) {
                int endPar = contents.indexOf( ')' );
                items = contents.substring( beginPar + 1, endPar );
              }
              // Try to find "as"
              int lastElement = words.length - 1;
              if (words[lastElement - 1].equals( "as" )) {
                qualifiedName = words[lastElement];
              }
            }

            // Create the element
            AnImport imp = new AnImport( name, reg, items != null, isHiding, isQualified, qualifiedName, items );
            r.add(imp);
          }
        }

      } catch (Exception ex) {
        // We continue with the next line
      }
    }
    return r;
  }

  public Map<String, Declaration> getDeclarations() {
    ArrayList<AnImport> imports = parseImports();
    boolean hasPrelude = false;
    for (AnImport i : imports) {
      if (i.getName().equals( "Prelude" )) {
        hasPrelude = true;
        break;
      }
    }
    if (!hasPrelude) {
      imports.add( new AnImport( "Prelude", null, true, false, null ) );
    }

    HashMap<String, Declaration> r = new HashMap<String, Declaration>();
    for (AnImport i : imports) {
      r.putAll( i.getDeclarations() );
    }

    return r;
  }
}
