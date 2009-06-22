// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.common.ui.preferences.CheckboxPreferenceControl;
import net.sf.eclipsefp.common.ui.preferences.IValueChangedListener;
import net.sf.eclipsefp.common.ui.preferences.PreferenceControl;
import net.sf.eclipsefp.common.ui.util.DialogUtil;
import net.sf.eclipsefp.haskell.ui.internal.preferences.HaskellPreferencePage;

import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.List;
import org.eclipse.ui.forms.widgets.ColumnLayout;

/** <p>Preference page for the content assist preference settings.</p>
 *
 * @author Leif Frenzel
 *
 * TODO: None of these preferences have any effect yet, I think.
 * TODO: Preferences take effect immediately, instead of waiting for Apply.
 * TODO: Restore Defaults button does not work.
 */
public class ContentAssistPP extends HaskellPreferencePage implements IEditorPreferenceNames {

	private final ColorListEntry[] colorListModel = new ColorListEntry[] {
			new ColorListEntry( "Completion proposal background",
					CA_PROPOSALS_BACKGROUND ),
					new ColorListEntry( "Completion proposal foreground",
							CA_PROPOSALS_FOREGROUND ) };

	private List colorList;
	private ColorSelector colorSelector;
	private PreferenceControl txtAutoActDelay;
	private PreferenceControl txtActTriggers;

	@Override
	protected Control createContents( final Composite parent ) {
		Composite control = new Composite( parent, SWT.NONE );
		ColumnLayout layout = new ColumnLayout();
		layout.maxNumColumns = 1;
		control.setLayout(layout);

		createBehaviorGroup(control);
		createAppearanceGroup(control);

		initialize();
		return control;
	}

	// UI creation methods
	//////////////////////

	private void createBehaviorGroup(final Composite parent) {
		Group group = new Group(parent, SWT.SHADOW_ETCHED_IN);
		group.setText("Behavior");
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		group.setLayout(layout);

		createCheckboxControl(group, "Insert single &proposals automatically", CA_AUTOINSERT);
		createCheckboxControl(group, "Present proposals in &alphabetical order", CA_ORDER_PROPOSALS);

		CheckboxPreferenceControl autoActButton = createCheckboxControl(group, "&Enable auto activation", CA_AUTOACTIVATION);
		Composite aa = new Composite(group, SWT.NONE);
		GridLayout aal = new GridLayout();
		aal.numColumns = 1;
		aal.marginWidth = aal.marginHeight = 0;
		aal.marginLeft = 28;
		aa.setLayout(aal);
		txtAutoActDelay = createStringControl(aa, "Auto activation dela&y:", CA_AUTOACTIVATION_DELAY, 4);
		GridData gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		txtAutoActDelay.getControl().setLayoutData(gd);
		txtActTriggers = createStringControl(aa, "Auto activation &triggers:", CA_AUTOACTIVATION_TRIGGERS, 4);
		gd = new GridData();
		gd.horizontalAlignment = SWT.FILL;
		txtActTriggers.getControl().setLayoutData(gd);
		autoActButton.addValueChangedListener(new IValueChangedListener<Boolean>() {
			public void valueChanged(final PreferenceControl control, final Boolean value) {
				boolean enabled = value.booleanValue();
				txtAutoActDelay.setEnabled(enabled);
				txtActTriggers.setEnabled(enabled);
			}
		}, true);
	}

	private void createAppearanceGroup(final Composite parent) {
		Group group = new Group(parent, SWT.SHADOW_ETCHED_IN);
		group.setText("Appearance");
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		group.setLayout(layout);

		Composite editorComposite = createEditorComposite(group);
		createColorList(group, editorComposite );
		Composite stylesComposite = createStylesComposite( editorComposite );
		createLabel( stylesComposite, "C&olor:" );
		createColorSelector( stylesComposite );
	}

	private void createColorList( final Composite composite,
			final Composite editorComposite ) {
		int style = SWT.SINGLE | SWT.V_SCROLL | SWT.BORDER;
		colorList = new List( editorComposite, style );
		GridData gridData = new GridData(   GridData.VERTICAL_ALIGN_BEGINNING
				| GridData.FILL_HORIZONTAL );
		gridData.heightHint = DialogUtil.convertHeightInCharsToPixels( composite,
				8 );
		colorList.setLayoutData( gridData );
		colorList.addSelectionListener( new SelectionAdapter() {
			@Override
			public void widgetSelected( final SelectionEvent e ) {
				handleColorListSelection();
			}
		} );
	}

	private void createColorSelector( final Composite parent ) {
		colorSelector = new ColorSelector( parent );
		Button colorButton = colorSelector.getButton();
		GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
		gridData.horizontalAlignment = GridData.BEGINNING;
		colorButton.setLayoutData( gridData );
		colorButton.addSelectionListener( new SelectionAdapter() {
			@Override
			public void widgetSelected( final SelectionEvent e ) {
				int i = colorList.getSelectionIndex();
				String key = colorListModel[ i ].getColorKey();
				RGB colorValue = colorSelector.getColorValue();
				PreferenceConverter.setValue( getPreferenceStore(), key, colorValue );
			}
		} );
	}

	private Composite createStylesComposite( final Composite parent ) {
		GridLayout layout;
		Composite stylesComposite = new Composite( parent, SWT.NONE );
		layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.numColumns = 2;
		stylesComposite.setLayout( layout );
		stylesComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
		return stylesComposite;
	}

	private Composite createEditorComposite( final Composite parent ) {
		Composite editorComposite = new Composite( parent, SWT.NONE );
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		layout.horizontalSpacing = 10;
		editorComposite.setLayout( layout );
		GridData gridData = new GridData(   GridData.HORIZONTAL_ALIGN_FILL
				| GridData.FILL_VERTICAL );
		gridData.horizontalSpan = 2;
		editorComposite.setLayoutData( gridData );
		return editorComposite;
	}


	// helping methods
	//////////////////

	private void handleColorListSelection() {
		int i = colorList.getSelectionIndex();
		String key = colorListModel[ i ].getColorKey();
		RGB rgb = PreferenceConverter.getColor( getPreferenceStore(), key );
		colorSelector.setColorValue( rgb );
	}

	private void initialize() {
		for( int i = 0; i < colorListModel.length; i++ ) {
			colorList.add( colorListModel[ i ].getLabel() );
		}
		colorList.getDisplay().asyncExec( new Runnable() {
			public void run() {
				if( ( colorList != null ) && !colorList.isDisposed() ) {
					colorList.select( 0 );
					handleColorListSelection();
				}
			}
		} );
		loadControlValues();
	}

}