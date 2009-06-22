// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.common.ui.preferences.CheckboxPreferenceControl;
import net.sf.eclipsefp.common.ui.preferences.IValueChangedListener;
import net.sf.eclipsefp.common.ui.preferences.IntegerPreferenceControl;
import net.sf.eclipsefp.common.ui.preferences.PreferenceControl;
import net.sf.eclipsefp.common.ui.util.DialogUtil;
import net.sf.eclipsefp.haskell.ui.internal.preferences.HaskellPreferencePage;

import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.IPreferenceStore;
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
import org.eclipse.ui.editors.text.TextEditorPreferenceConstants;
import org.eclipse.ui.forms.widgets.ColumnLayout;

/** <p>the preference page for common editor preference settings.</p>
 *
 * @author Leif Frenzel
 *
 * TODO: Preferences take effect immediately, instead of waiting for Apply.
 * TODO: Restore Defaults button does not work.
 * TODO: Show line numbers does not seem to have any effect.
 * TODO: The layout of the preferences is strange.
 */
public class HaskellEditorPP extends HaskellPreferencePage implements IEditorPreferenceNames {

	private final ColorListEntry[] colorListModel = new ColorListEntry[] {
			new ColorListEntry( "Line number foreground",
					EDITOR_LINE_NUMBER_RULER_COLOR ),
					new ColorListEntry( "Matching brackets highlight",
							EDITOR_MATCHING_BRACKETS_COLOR ),
							new ColorListEntry( "Current line highlight", EDITOR_CURRENT_LINE_COLOR ),
							new ColorListEntry( "Print margin", EDITOR_PRINT_MARGIN_COLOR ) };

	private IntegerPreferenceControl printMarginColumn;
	private List colorList;
	private ColorSelector colorSelector;

	@Override
	protected Control createContents( final Composite parent ) {
		Composite control = new Composite( parent, SWT.NONE );
		ColumnLayout layout = new ColumnLayout();
		layout.maxNumColumns = 1;
		control.setLayout(layout);

		createEditingGroup(control);
		createAppearanceGroup(control);
		createColorsGroup(control);

		initialize();
		return control;
	}

	public static void initializeDefaultValues(final IPreferenceStore store) {
		TextEditorPreferenceConstants.initializeDefaultValues( store );
		DefaultEditorPreferenceInitializer.initializeDefaultValues( store );
	}


	// UI creation methods
	//////////////////////

	private Group createEditingGroup(final Composite parent) {
		Group group = new Group(parent, SWT.SHADOW_ETCHED_IN);
		group.setText("Editing");
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		group.setLayout(layout);

		createCheckboxControl(group, "Ins&ert space for tabs", IEditorPreferenceNames.EDITOR_SPACES_FOR_TABS);

		return group;
	}

	private Group createAppearanceGroup(final Composite parent) {
		Group group = new Group(parent, SWT.SHADOW_ETCHED_IN);
		group.setText("Appearance");
		GridLayout groupLayout = new GridLayout();
		groupLayout.numColumns = 2;
		group.setLayout(groupLayout);

		CheckboxPreferenceControl spm = createCheckboxControl(group, "Sho&w print margin", EDITOR_PRINT_MARGIN);
		printMarginColumn = createIntegerControl(group, "Print margin col&umn:", EDITOR_PRINT_MARGIN_COLUMN, 3);
		spm.addValueChangedListener(new IValueChangedListener<Boolean>() {
			public void valueChanged(final PreferenceControl<Boolean> control, final Boolean value) {
				boolean enabled = value.booleanValue();
				printMarginColumn.setEnabled(enabled);
			}
		}, true );

		createCheckboxControl(group, "Show lin&e numbers", EDITOR_LINE_NUMBER_RULER);
		createCheckboxControl(group, "Show overview &ruler", EDITOR_OVERVIEW_RULER);
		createCheckboxControl(group, "Highlight &matching brackets", EDITOR_MATCHING_BRACKETS);
		createCheckboxControl(group, "Hi&ghlight current line", EDITOR_CURRENT_LINE);

		return group;
	}

	private Group createColorsGroup(final Composite parent) {
		Group group = new Group(parent, SWT.SHADOW_ETCHED_IN);
		group.setText("Colors");
		GridLayout groupLayout = new GridLayout();
		groupLayout.numColumns = 1;
		group.setLayout(groupLayout);

		Composite editorComposite = createEditorComposite( group );
		createColorList( group, editorComposite );
		Composite stylesComposite = createStylesComposite( editorComposite );
		createLabel( stylesComposite, "C&olor:" );
		createColorSelector( stylesComposite );

		return group;
	}

	private void createColorSelector( final Composite parent ) {
		colorSelector = new ColorSelector( parent );
		Button foregroundColorButton = colorSelector.getButton();
		GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
		gridData.horizontalAlignment = GridData.BEGINNING;
		foregroundColorButton.setLayoutData( gridData );
		foregroundColorButton.addSelectionListener( new SelectionAdapter() {
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
		Composite stylesComposite = new Composite( parent, SWT.NONE );
		GridLayout layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.numColumns = 2;
		stylesComposite.setLayout( layout );
		stylesComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
		return stylesComposite;
	}

	private void createColorList( final Composite parent,
			final Composite editorComposite ) {
		int style = SWT.SINGLE | SWT.V_SCROLL | SWT.BORDER;
		colorList = new List( editorComposite, style );
		GridData gridData = new GridData(   GridData.VERTICAL_ALIGN_BEGINNING
				| GridData.FILL_HORIZONTAL );
		gridData.heightHint = DialogUtil.convertHeightInCharsToPixels( parent, 8 );
		colorList.setLayoutData( gridData );
		colorList.addSelectionListener( new SelectionAdapter() {
			@Override
			public void widgetSelected( final SelectionEvent e ) {
				handleColorListSelection();
			}
		} );
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