package net.sf.eclipsefp.haskell.scion.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import net.sf.eclipsefp.haskell.scion.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.lisp.LispExpr;

/**
 * A simple standalone program that allows for communication with a Scion server
 * by typing commands in the console.
 * 
 * @author Thomas ten Cate
 */
public class CommandLineClient {

	public static void main(String[] args) {
		ScionClient.initializeServer();
		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
		while (true) {
			String line;
			try {
				line = reader.readLine();
			} catch (IOException e) {
				e.printStackTrace();
				return;
			}
			if (line.length() == 0) {
				System.out.println("Exit");
				ScionClient.dispose();
				return;
			}
			RawCommand command = new RawCommand(line);
			ScionClient.syncRunCommand(command, 10000);
			if (command.isCompleted()) {
				System.out.println(command.getResponse().toString());
			} else {
				System.out.println("Timeout");
			}
		}
	}
	
	private static class RawCommand extends ScionCommand {

		private String lisp;
		private LispExpr response;
		
		public RawCommand(String lisp) {
			this.lisp = lisp;
		}
		
		@Override
		protected String internalLisp() {
			return lisp;
		}

		@Override
		protected void parseInternalResponse(LispExpr response) {
			this.response = response; 
		}
		
		public LispExpr getResponse() {
			return response;
		}
		
	}

}
