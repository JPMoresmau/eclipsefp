package net.sf.eclipsefp.haskell.hlint;

import java.util.List;

import org.codehaus.jparsec.Parser;
import org.codehaus.jparsec.Parsers;
import org.codehaus.jparsec.Scanners;
import org.codehaus.jparsec.Terminals;
import org.codehaus.jparsec.functors.Map;

public class OutputParser {

	static final Parser<Integer> NUMBER = Terminals.IntegerLiteral.PARSER
			.map(new Map<String, Integer>() {
				public Integer map(String s) {
					return Integer.valueOf(s);
				}
			});

	static final Parser<?> COLON = Scanners.isChar(':');
	static final Parser<Character> NOT_COLON = notChar(':');
	
	static final Parser<?> EOL = Scanners.isChar('\n');
	static final Parser<Character> NOT_EOL = notChar('\n');

	public static Parser<Character> notChar(char c) {
		return Terminals.CharLiteral.PARSER.next(new Map<Character, Parser<Character>>() {
			public Parser<Character> map(Character c) {
				return c == ':' ? Parsers.<Character> unexpected(":") : Parsers.constant(c);
			}
		});
	}
	
	public static Parser<String> upTo(char c) {
		return notChar(c).many().map(new Map<List<Character>, String>() {
			public String map(List<Character> chars) {
				StringBuilder builder = new StringBuilder();
				for (char c : chars)
					builder.append(c);
				return builder.toString();
			}
		});
	}
	
	
}
