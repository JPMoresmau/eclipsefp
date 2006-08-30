package net.sf.eclipsefp.common.core.util;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MultiplexedWriter extends Writer {

	private List<Writer> fOutputs = new ArrayList<Writer>();

	public MultiplexedWriter(Writer... outputs) {
		fOutputs.addAll(Arrays.asList(outputs));
	}
	
	public void addOutput(Writer out) {
		fOutputs.add(out);
	}

	@Override
	public void close() throws IOException {
		for (Writer output : fOutputs) {
			try {
				output.close();
			} catch (IOException ex) {
				//ignore error for this output and procede to next
			}
		}
	}

	@Override
	public void flush() throws IOException {
		for (Writer output : fOutputs) {
			try {
				output.flush();
			} catch (IOException ex) {
				//ignore error for this output and procede to next
			}
		}
	}

	@Override
	public void write(char[] cbuf, int off, int len) throws IOException {
		for (Writer output : fOutputs) {
			try {
				output.write(cbuf, off, len);
			} catch (IOException ex) {
				//ignore error for this output and procede to next
			}
		}
	}

}
