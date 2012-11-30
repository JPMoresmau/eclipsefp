package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * Form entry for selecting modules from a project.
 * @author Alejandro Serrano
 *
 */
public class FormEntryModules extends FormEntry {

  static final int EXPOSED_COL = 0;
  static final int OTHER_COL = 1;
  static final int NAME_COL = 2;

  private final ArrayList<IOtherValueEntryListener> otherListeners = new ArrayList<IOtherValueEntryListener>();

  ArrayList<String> prevElements;
  boolean onlyOne;
  String exposedString;
  FormToolkit toolkit;
  Table table;
  ArrayList<String> exposed;
  ArrayList<String> other;
  boolean ignoreModify = false;
  HashMap<String, Button> exposedBoxes;
  HashMap<String, Button> otherBoxes;

  public FormEntryModules( final String exposedString ) {
    this( exposedString, false );
  }

  public FormEntryModules( final String exposedString, final boolean onlyOne ) {
    super();
    this.exposedString = exposedString;
    this.onlyOne = onlyOne;
    this.exposed = new ArrayList<String>();
    this.other = new ArrayList<String>();
    this.exposedBoxes = new HashMap<String, Button>();
    this.otherBoxes = new HashMap<String, Button>();
    this.prevElements = new ArrayList<String>();
  }


  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {
    this.toolkit = toolkit;

    table = toolkit.createTable( parent, SWT.SINGLE );
    table.setHeaderVisible( true );

    TableColumn exposedCol = new TableColumn( table, SWT.NULL );
    exposedCol.setText( exposedString );
    exposedCol.pack();
    TableColumn otherCol = new TableColumn( table, SWT.NULL );
    otherCol.setText( UITexts.cabalEditor_other_modules );
    otherCol.pack();
    TableColumn nameCol = new TableColumn( table, SWT.NULL );
    nameCol.setText( UITexts.cabalEditor_module );
    nameCol.pack();
  }

  public void changeExposedColumnName( final String newName ) {
    TableColumn exposedCol = table.getColumn( EXPOSED_COL );
    exposedCol.setText( newName );
  }

  @Override
  public Control getControl() {
    return table;
  }

  @Override
  public int heightHint() {
    return 170;
  }

  @SuppressWarnings ( "unchecked" )
  public synchronized void setSourceFolders( final FormEntryModulesRoot root,
      final boolean blockNotification ) {
    synchronized( this ) {
      if( root == null ) {
        // If we have no data
        table.clearAll();
        for( Button b: exposedBoxes.values() ) {
          b.dispose();
        }
        exposedBoxes.clear();
        for( Button b: otherBoxes.values() ) {
          b.dispose();
        }
        otherBoxes.clear();
        prevElements.clear();
        return;
      }

      Collection<String> sourceDirs = null;
      if( root.getStanza().getProperties()
          .containsKey( CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName() ) ) {
        //sourceDirs = root.getStanza().getProperties()
        //    .get( CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName() );
        sourceDirs=root.getStanza().getSourceDirs();
      } else {
        sourceDirs = Collections.emptyList();
      }

      ArrayList<String> modules = new ArrayList<String>();
      ModulesVisitor visitor = new ModulesVisitor( modules, sourceDirs );
      try {
        root.getProject().accept( visitor );
      } catch( CoreException e ) {
        HaskellUIPlugin.log( e );
      }

      PackageDescriptionStanza pkg = root.getDescription().getPackageStanza();
      if( pkg != null ) {
        if( pkg.getProperties().containsKey( CabalSyntax.FIELD_DATA_FILES.getCabalName() ) ) {
          // There are data files, so Paths_package is also provided
          modules.add( "Paths_"
              + pkg.getProperties().get( CabalSyntax.FIELD_NAME.getCabalName() ) );
        }
      }

      // Show the items in alphabetical order
      Collections.sort( modules );

      // Check if it is the same
      if( !getValueFrom( modules ).equals( getValueFrom( prevElements ) ) ) {
        prevElements = modules;
        // Set the new values for exposed and others
        boolean changeToExposed = false;
        boolean changeToOther = false;
        for( String s: ( ArrayList<String> )exposed.clone() ) {
          if( !modules.contains( s ) ) {
            changeToExposed = true;
            exposed.remove( s );
          }
        }
        for( String s: ( ArrayList<String> )other.clone() ) {
          if( !modules.contains( s ) ) {
            changeToOther = true;
            other.remove( s );
          }
        }

        // Add the items to table
        table.removeAll();

        for( Button b: exposedBoxes.values() ) {
          b.dispose();
        }
        exposedBoxes.clear();
        for( Button b: otherBoxes.values() ) {
          b.dispose();
        }
        otherBoxes.clear();

        for( String mod: modules ) {
          final TableItem item = new TableItem( table, SWT.NULL );
          item.setText( new String[] { "", "", mod } );
          // Add "exposed" check box
          Button exposedButton = toolkit.createButton( table, "", SWT.CHECK );
          exposedButton.setSelection( exposed.contains( mod ) );
          exposedButton.pack();
          exposedButton.addSelectionListener( new SelectionListener() {

            @Override
            public void widgetSelected( final SelectionEvent e ) {
              if( !ignoreModify ) {
                String mod = item.getText( NAME_COL );
                if( exposed.contains( mod ) ) {
                  exposed.remove( mod );
                } else {
                  if (onlyOne && exposed.size() > 0) {
                    // We have to deselect the other one
                    boolean previousIgnoreModify = ignoreModify;
                    ignoreModify = true;
                    for (String it : exposed) {
                      exposedBoxes.get( it ).setSelection( false );
                    }
                    ignoreModify = previousIgnoreModify;
                    exposed.clear();
                    exposed.add(mod);
                  } else {
                    exposed.add( mod );
                    Collections.sort( exposed );
                  }
                }
                FormEntryModules.this.notifyTextValueChanged();
              }
            }

            @Override
            public void widgetDefaultSelected( final SelectionEvent e ) {
              // Do nothing
            }
          } );
          exposedBoxes.put( mod, exposedButton );
          TableEditor exposedEditor = new TableEditor( table );
          exposedEditor.minimumWidth = exposedButton.getSize().x;
          exposedEditor.horizontalAlignment = SWT.CENTER;
          exposedEditor.setEditor( exposedButton, item, EXPOSED_COL );
          // Add "other" check box
          Button otherButton = toolkit.createButton( table, "", SWT.CHECK );
          otherButton.setSelection( other.contains( mod ) );
          otherButton.pack();
          otherButton.addSelectionListener( new SelectionListener() {

            @Override
            public void widgetSelected( final SelectionEvent e ) {
              if( !ignoreModify ) {
                String mod = item.getText( NAME_COL );
                if( other.contains( mod ) ) {
                  other.remove( mod );
                } else {
                  other.add( mod );
                  Collections.sort( other );
                }
                FormEntryModules.this.notifyOtherValueChanged();
              }
            }

            @Override
            public void widgetDefaultSelected( final SelectionEvent e ) {
              // Do nothing
            }
          } );
          otherBoxes.put( mod, otherButton );
          TableEditor otherEditor = new TableEditor( table );
          otherEditor.minimumWidth = otherButton.getSize().x;
          otherEditor.horizontalAlignment = SWT.CENTER;
          otherEditor.setEditor( otherButton, item, OTHER_COL );
        }
        // ensure module names are visible
        table.getColumn( 2 ).pack();
        // Tell modifications
        if( changeToExposed && !blockNotification ) {
          notifyTextValueChanged();
        }
        if( changeToOther && !blockNotification ) {
          notifyOtherValueChanged();
        }
      }
    }
  }

  public ArrayList<String> getOrderedValue( final String value ) {
    // Get ordered value
    String newValue = value == null ? "" : value;
    String[] elementsA = newValue.split( "," );
    ArrayList<String> elements = new ArrayList<String>();
    for( String e: elementsA ) {
      elements.add( e.trim() );
    }
    Collections.sort( elements );
    return elements;
  }

  @Override
  public synchronized void setValue( final String value,
      final boolean blockNotification ) {
    synchronized( this ) {
      List<String> newValue = getOrderedValue( value );
      if( getValueFrom( newValue ).equals( getValue() ) ) {
        // We have the same value
        return;
      }

      ignoreModify = true;
      exposed.clear();
      for( TableItem item: table.getItems() ) {
        String mod = item.getText( NAME_COL );
        if (exposedBoxes.containsKey( mod )) {
          exposedBoxes.get( mod ).setSelection( newValue.contains( mod ) );
        }
        if( newValue.contains( mod ) ) {
          exposed.add( mod );
        }
      }
      ignoreModify = false;

      if( !blockNotification ) {
        this.notifyTextValueChanged();
      }
    }
  }

  public synchronized void setOtherModulesValue( final String value,
      final boolean blockNotification ) {
    synchronized( this ) {
      List<String> newValue = getOrderedValue( value );
      if( getValueFrom( newValue ).equals( getOtherModulesValue() ) ) {
        // We have the same value
        return;
      }

      ignoreModify = true;
      other.clear();
      for( TableItem item: table.getItems() ) {
        String mod = item.getText( NAME_COL );
        if (otherBoxes.containsKey( mod )) {
          otherBoxes.get( mod ).setSelection( newValue.contains( mod ) );
        }
        if( newValue.contains( mod ) ) {
          other.add( mod );
        }
      }
      ignoreModify = false;

      if( !blockNotification ) {
        this.notifyOtherValueChanged();
      }
    }
  }

  String getValueFrom( final List<String> strings ) {
    StringBuilder builder = new StringBuilder();
    for( String s: strings ) {
      if( builder.length() > 0 ) {
        builder.append( ","+PlatformUtil.NL );
      }
      builder.append( s );
    }
    return builder.toString();
  }

  @Override
  public String getValue() {
    return getValueFrom( exposed );
  }

  public String getOtherModulesValue() {
    return getValueFrom( other );
  }

  @Override
  public void setEditable( final boolean editable ) {
    if (!editable) {
      setSourceFolders( null, true );
    }
    table.setEnabled( editable );
  }

  public void addOtherValueListener( final IOtherValueEntryListener listener ) {
    this.otherListeners.add( listener );
  }

  public void removeOtherValueListener( final IOtherValueEntryListener listener ) {
    this.otherListeners.remove( listener );
  }

  protected void notifyOtherValueChanged() {
    for( IOtherValueEntryListener listener: otherListeners ) {
      listener.otherTextValueChanged( this );
    }
  }


  public static class ModulesVisitor implements IResourceVisitor {

    public Collection<String> elts;
    public Vector<String> possiblePrefixes;

    public ModulesVisitor( final Collection<String> whereAdd,
        final Collection<String> dirs ) {
      this.elts = whereAdd;
      this.possiblePrefixes = new Vector<String>();
      for( String dir: dirs ) {
        this.possiblePrefixes.add( dir.trim() + "/" );
      }
    }

    @Override
    public boolean visit( final IResource resource ) {
      String path = resource.getProjectRelativePath().toString();
      if( resource instanceof IFile ) {
        for( String dir: possiblePrefixes ) {
          if( path.startsWith( dir ) ) {
            String filePath = path.substring( dir.length() );
            if( filePath.endsWith( ".hs" ) ) {
              String module = filePath.substring( 0, filePath.length() - 3 )
                  .replace( '/', '.' );
              this.elts.add( module );
            } else if( filePath.endsWith( ".lhs" ) ) {
              String module = filePath.substring( 0, filePath.length() - 4 )
                  .replace( '/', '.' );
              this.elts.add( module );
            }
          }
        }
      }
      return true;
    }
  }


}
