package net.sf.eclipsefp.haskell.util;

import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;

/**
 * ensure that only one job of a given type runs and only one job waits, intermediate jobs are discarded
 * @author JP Moresmau
 *
 */
public class SingleJobQueue {
	/**
	 * the currently running job
	 */
	private Job current;
	/**
	 * the next job to run
	 */
	private Job next;

	/**
	 * add a job to the queue
	 * if the queue is empty, schedule the job straight away
	 * otherwise queue the job (losing the previously waiting job if any)
	 * @param j
	 */
	public synchronized void addJob(Job j) {
		if (current==null){
			current=j;
			launchCurrent();
		} else {
			next=j;
		}
	}

	/**
	 * close the queue: cancel current job and remove next
	 */
	public synchronized void close(){
		next=null;
		if (current!=null){
			current.cancel();
		}
		current=null;
	}

	public synchronized boolean hasPendingJob() {
	  return next != null;
	}

	/**
	 * launch the current job with a listener that will launch the waiting job on completion
	 */
	private void launchCurrent(){
		if (current!=null){
			current.addJobChangeListener(new ScheduleNextListener());
			current.schedule();
		}
	}

	private class ScheduleNextListener extends JobChangeAdapter {
		@Override
		public void done(IJobChangeEvent event) {
			synchronized (SingleJobQueue.this) {
				// we're not running
				current=null;
				// we have a job waiting, let's run it
				if (next!=null){
					current=next;
					next=null;
					launchCurrent();
				}
			}
		}
	}

}
