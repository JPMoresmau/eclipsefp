package net.sf.eclipsefp.haskell.profiler.internal.editors;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import net.sf.eclipsefp.haskell.profiler.model.Job;
import net.sf.eclipsefp.haskell.profiler.model.Sample;

import org.eclipse.birt.chart.model.Chart;
import org.eclipse.birt.chart.model.ChartWithAxes;
import org.eclipse.birt.chart.model.attribute.LegendItemType;
import org.eclipse.birt.chart.model.attribute.TickStyle;
import org.eclipse.birt.chart.model.component.Axis;
import org.eclipse.birt.chart.model.component.Series;
import org.eclipse.birt.chart.model.component.impl.SeriesImpl;
import org.eclipse.birt.chart.model.data.NumberDataSet;
import org.eclipse.birt.chart.model.data.SeriesDefinition;
import org.eclipse.birt.chart.model.data.impl.NumberDataSetImpl;
import org.eclipse.birt.chart.model.data.impl.SeriesDefinitionImpl;
import org.eclipse.birt.chart.model.impl.ChartWithAxesImpl;
import org.eclipse.birt.chart.model.type.AreaSeries;
import org.eclipse.birt.chart.model.type.impl.AreaSeriesImpl;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.dialogs.ContainerGenerator;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.eclipse.ui.part.EditorPart;

public class ProfilerViewer extends EditorPart {

	static int INITIAL_NUMBER_OF_ITEMS = 15;

	String fileContents;
	Job job = null;
	double[] samplePoints;
	List<Map.Entry<String, BigInteger>> entries;
	Scale slider;
	ChartCanvas canvas;

	public ProfilerViewer() {
		super();
	}

	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		setSite(site);
		setInput(input);

		try {
			IFileEditorInput fInput = (IFileEditorInput) input;
			IFile inputFile = fInput.getFile();
			setPartName(inputFile.getName());
			InputStream contents = inputFile.getContents();
			fileContents = new Scanner(contents).useDelimiter("\\Z").next();
			contents.close();
			job = Job.parse(new StringReader(fileContents));
			// Sort entries
			entries = job.sortEntriesByTotal();
			// Get sample points
			samplePoints = new double[job.getSamplesAndTimes().size()];
			int i = 0;
			for (Sample s : job.getSamples()) {
				samplePoints[i] = s.getTime();
				i++;
			}
		} catch (Exception e) {
			throw new PartInitException(Status.CANCEL_STATUS);
		}
	}

	@Override
	public void createPartControl(Composite parent) {
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		parent.setLayout(layout);

		Composite horiz = new Composite(parent, SWT.NONE);
		GridData hGridData = new GridData(GridData.FILL_HORIZONTAL);
		horiz.setLayoutData(hGridData);
		GridLayout hLayout = new GridLayout();
		hLayout.numColumns = 2;
		horiz.setLayout(hLayout);

		Label l = new Label(horiz, SWT.NONE);
		l.setText("Ungroup these elements");
		slider = new Scale(horiz, SWT.NONE);
		slider.setMinimum(0);
		slider.setMaximum(entries.size());
		slider.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		int n = entries.size() < INITIAL_NUMBER_OF_ITEMS ? entries.size() : INITIAL_NUMBER_OF_ITEMS;
		slider.setSelection(n);
		slider.addSelectionListener(new SelectionListener() {

			public void widgetSelected(SelectionEvent e) {
				Chart chart = createChart(slider.getSelection());
				canvas.setChart(chart);
				canvas.redraw();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
				Chart chart = createChart(slider.getSelection());
				canvas.setChart(chart);
				canvas.redraw();
			}
		});

		Chart chart = createChart(n);
		canvas = new ChartCanvas(parent, SWT.NONE);
		GridData cGridData = new GridData(GridData.FILL_BOTH);
		canvas.setLayoutData(cGridData);
		canvas.setChart(chart);
	}

	private Chart createChart(int numberApart) {
		int n = entries.size() < numberApart ? entries.size() : numberApart;
		List<Map.Entry<String, BigInteger>> entriesApart = new ArrayList<Map.Entry<String, BigInteger>>(
				entries.subList(0, n));

		Chart chart = ChartWithAxesImpl.create();
		// Title
		chart.getTitle().getLabel().getCaption().setValue(job.getName());
		// chart.getTitle().getLabel().getCaption().getFont().setSize(14);
		// chart.getTitle().getLabel().getCaption().getFont().setName("Arial");
		// Legend
		chart.getLegend().setItemType(LegendItemType.SERIES_LITERAL);
		chart.getLegend().setVisible(true);
		// X-Axis -> time
		Axis xAxis = ((ChartWithAxes) chart).getPrimaryBaseAxes()[0];
		// xAxis.setType(AxisType.LINEAR_LITERAL);
		xAxis.getMajorGrid().setTickStyle(TickStyle.BELOW_LITERAL);
		xAxis.getTitle().setVisible(true);
		xAxis.getTitle().getCaption().setValue(job.getSampleUnit());
		xAxis.getLabel().setVisible(true);
		// X-Axis data
		NumberDataSet xDataSet = NumberDataSetImpl.create(samplePoints);
		Series xCategory = SeriesImpl.create();
		xCategory.setDataSet(xDataSet);
		SeriesDefinition sdX = SeriesDefinitionImpl.create();
		sdX.getSeriesPalette().shift(0);
		xAxis.getSeriesDefinitions().add(sdX);
		sdX.getSeries().add(xCategory);
		// Y-Axis -> memory
		Axis yAxis = ((ChartWithAxes) chart).getPrimaryOrthogonalAxis(xAxis);
		yAxis.getMajorGrid().setTickStyle(TickStyle.LEFT_LITERAL);
		yAxis.getMajorGrid().getLineAttributes().setVisible(true);
		yAxis.getMinorGrid().getLineAttributes().setVisible(true);
		yAxis.setPercent(false);
		yAxis.getTitle().getCaption().setValue(job.getValueUnit());
		yAxis.getTitle().setVisible(true);
		yAxis.getTitle().getCaption().getFont().setRotation(90);
		yAxis.getLabel().setVisible(true);
		// Y-Axis data
		SeriesDefinition sdY = SeriesDefinitionImpl.create();
		sdY.getSeriesPalette().shift(1);
		yAxis.getSeriesDefinitions().add(sdY);
		// Get the numbers
		ProfileNumbers numbers = new ProfileNumbers(entriesApart, samplePoints.length);
		numbers.fillIn(job);
		// Add (rest) elements
		NumberDataSet restDataSet = NumberDataSetImpl.create(numbers.getRest());
		AreaSeries restSeries = (AreaSeries) AreaSeriesImpl.create();
		restSeries.setSeriesIdentifier("(rest)");
		restSeries.setDataSet(restDataSet);
		restSeries.getLineAttributes().setVisible(false);
		restSeries.getLabel().setVisible(false);
		restSeries.setStacked(true);
		sdY.getSeries().add(restSeries);
		// Add apart elements, in reverse order
		Collections.reverse(entriesApart);
		for (Map.Entry<String, BigInteger> entry : entriesApart) {
			double[] entryNumbers = numbers.getEntries().get(entry.getKey());
			NumberDataSet entryDataSet = NumberDataSetImpl.create(entryNumbers);
			AreaSeries entrySeries = (AreaSeries) AreaSeriesImpl.create();
			entrySeries.setSeriesIdentifier(entry.getKey());
			entrySeries.setDataSet(entryDataSet);
			entrySeries.getLineAttributes().setVisible(false);
			entrySeries.getLabel().setVisible(false);
			entrySeries.setStacked(true);
			sdY.getSeries().add(entrySeries);
		}

		return chart;
	}

	@Override
	public void setFocus() {
		// Do nothing
	}

	@Override
	public void doSave(IProgressMonitor monitor) {
		// Do nothing: the .hp files cannot be changed
	}

	@Override
	public void doSaveAs() {
		SaveAsDialog dialog = new SaveAsDialog(getSite().getShell());
		dialog.setOriginalName(getPartName());
		if (dialog.open() == Window.OK) {
			try {
				IPath path = dialog.getResult();
				IPath folder = path.uptoSegment(path.segmentCount() - 1);
				ContainerGenerator gen = new ContainerGenerator(folder);
				IContainer con = gen.generateContainer(null);
				IFile file = con.getFile(Path.fromPortableString(path.lastSegment()));
				byte[] bytes = fileContents.getBytes();
				InputStream source = new ByteArrayInputStream(bytes);
				if (!file.exists()) {
					file.create(source, IResource.NONE, null);
				} else {
					file.setContents(source, IResource.NONE, null);
				}
				source.close();
				setPartName(path.lastSegment());
			} catch (Exception e) {
				// Do nothing
			}
		}
	}

	@Override
	public boolean isDirty() {
		return false;
	}

	@Override
	public boolean isSaveAsAllowed() {
		return true;
	}

}
