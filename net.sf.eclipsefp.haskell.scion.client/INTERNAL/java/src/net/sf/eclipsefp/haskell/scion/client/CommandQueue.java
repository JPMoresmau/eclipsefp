package net.sf.eclipsefp.haskell.scion.client;

import java.util.concurrent.LinkedBlockingQueue;

import net.sf.eclipsefp.haskell.scion.commands.ScionCommand;

/**
 * A queue of Scion commands.
 * Because it implements {@link BlockingQueue}, it is thread-safe.
 * 
 * @author Thomas ten Cate
 */
public class CommandQueue extends LinkedBlockingQueue<ScionCommand> {

	private static final long serialVersionUID = 1L;
	
	/**
	 * The maximum number of commands that can be in the queue at any given time.
	 * It is used to prevent the queue from filling up indefinitely when the server cannot be run.
	 */
	private static final int MAX_QUEUE_LENGTH = 10;

	public CommandQueue() {
		super(MAX_QUEUE_LENGTH);
	}

}
