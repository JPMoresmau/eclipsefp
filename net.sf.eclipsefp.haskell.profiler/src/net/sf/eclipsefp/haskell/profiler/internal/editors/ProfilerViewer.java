package net.sf.eclipsefp.haskell.profiler.internal.editors;

import java.io.InputStream;
import java.math.BigInteger;
import java.util.Collections;
import java.util.List;
import java.util.Map;

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
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

public class ProfilerViewer extends EditorPart {

	Job job = null;
	double[] samplePoints;
	List<Map.Entry<String, BigInteger>> entries;
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
			job = Job.parse(contents);
			contents.close();
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

		Label l = new Label(parent, SWT.NONE);
		l.setText("Placeholder");
		GridData lGridData = new GridData(GridData.FILL_HORIZONTAL);
		l.setLayoutData(lGridData);

		Chart chart = createChart(15);
		canvas = new ChartCanvas(parent, SWT.NONE);
		canvas.setChart(chart);
		GridData cGridData = new GridData(GridData.FILL_BOTH);
		canvas.setLayoutData(cGridData);
	}

	private Chart createChart(int numberApart) {
		int n = entries.size() < numberApart ? entries.size() : numberApart;
		List<Map.Entry<String, BigInteger>> entriesApart = entries.subList(0, n);

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
		// Do nothing: the .hp files cannot be changed
	}

	@Override
	public boolean isDirty() {
		return false;
	}

	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}

}
