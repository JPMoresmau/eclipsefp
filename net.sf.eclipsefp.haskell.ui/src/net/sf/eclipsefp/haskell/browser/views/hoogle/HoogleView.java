package net.sf.eclipsefp.haskell.browser.views.hoogle;

import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultConstructor;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultDeclaration;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultModule;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultPackage;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.part.ViewPart;


public class HoogleView extends ViewPart implements SelectionListener,
    ISelectionChangedListener {

  Text text;
  TreeViewer viewer;
  Browser doc;
  HoogleContentProvider provider;

  @Override
  public void createPartControl( final Composite parent ) {
    GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = layout.horizontalSpacing = 0;
    layout.marginBottom = layout.marginHeight = layout.marginLeft = layout.marginRight = layout.marginTop = layout.marginWidth = 0;
    parent.setLayout( layout );

    text = new Text( parent, SWT.SINGLE | SWT.SEARCH | SWT.ICON_SEARCH
        | SWT.ICON_CANCEL );
    GridData textData = new GridData();
    textData.horizontalAlignment = SWT.FILL;
    textData.grabExcessHorizontalSpace = true;
    text.setLayoutData( textData );
    text.addSelectionListener( this );

    SashForm form = new SashForm( parent, SWT.VERTICAL );
    GridData formData = new GridData();
    formData.horizontalAlignment = SWT.FILL;
    formData.verticalAlignment = SWT.FILL;
    formData.grabExcessVerticalSpace = true;
    formData.grabExcessHorizontalSpace = true;
    form.setLayoutData( formData );
    viewer = new TreeViewer( form );
    provider = new HoogleContentProvider();
    viewer.setContentProvider( provider );
    viewer.setLabelProvider( new HoogleLabelProvider() );
    viewer.setInput( "" );
    doc = new Browser( form, SWT.NONE );
    form.setWeights( new int[] { 60, 40 } );

    // Hook for changes in selection
    viewer.addPostSelectionChangedListener( this );
  }

  @Override
  public void setFocus() {
    text.setFocus();
  }

  public void widgetSelected( final SelectionEvent e ) {
    // Do nothing
  }

  public void widgetDefaultSelected( final SelectionEvent e ) {
    if( e.detail == SWT.CANCEL ) {
      viewer.setInput( "" );
      viewer.refresh();
    } else {
      viewer.setInput( text.getText() );
      viewer.refresh();
      Object first = provider.getFirstElement();
      if( first != null ) {
        viewer.setSelection( new StructuredSelection( first ), true );
      }
    }
  }

  public void selectionChanged( final SelectionChangedEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();

    Object o = selection.getFirstElement();
    if( o == null ) {
      doc.setText( "" );
      return;
    }

    if( o instanceof HoogleResult ) {
      HoogleResult result = ( HoogleResult )o;
      String text = null;
      switch( result.getType() ) {
        case PACKAGE:
          HaskellPackage pkg = ( ( HoogleResultPackage )result ).getPackage();
          text = HtmlUtil.generateDocument( "package "
              + pkg.getIdentifier().toString(), pkg.getDoc() );
          break;
        case MODULE:
          HoogleResultModule mod = ( HoogleResultModule )result;
          text = HtmlUtil
              .generateDocument( "module " + mod.getName(), mod
                  .getPackageIdentifiers(), null, true, mod.getModule()
                  .getDoc() );
          break;
        case DECLARATION:
          HoogleResultDeclaration decl = ( HoogleResultDeclaration )result;
          text = HtmlUtil.generateDocument( decl.getDeclaration()
              .getCompleteDefinition(), decl.getPackageIdentifiers(), decl
              .getModule(), true, decl.getDeclaration().getDoc() );
          break;
        case CONSTRUCTOR:
          HoogleResultConstructor con = ( HoogleResultConstructor )result;
          text = HtmlUtil.generateDocument( con.getConstructor()
              .getCompleteDefinition(), con.getPackageIdentifiers(), con
              .getModule(), true, con.getDeclaration().getDoc() );
          break;
      }

      doc.setText( text );
    }
  }
}
