package net.sf.eclipsefp.haskell.scion.lisp;

import java.util.LinkedList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.ScionParseException;

public class LispParser {

	public static LispExpr parse(String input) {
		return new LispParser(input).parse();
	}
	
	private static final String IDENT_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_:";
	
	private char[] input;
	private int pos = 0;
	private char cur;
	
	private LispParser(String input) {
		this.input = input.toCharArray();
		this.pos = 0;
		this.cur = this.input.length > 0 ? this.input[0] : '\0';
	}
	
	private LispExpr parse() {
		skipWhitespace();
		LispExpr expr = parseExpr();
		if (expr != null) {
			if (pos == input.length) {
				return expr;
			} else {
				parseError("Trailing garbage");
			}
		} else {
			parseError("Server response is not a Lisp expression"); // TODO improve
		}
		return null; // never reached
	}
	
	/**
	 * Parses an expression from the input.
	 * If parsing succeeds, returns the parsed expression and advances pos past the end of the expression
	 * and past any consequent whitespace.
	 * If parsing fails, throws an exception.
	 * This same specification holds for all parseXxx methods in this class.
	 */
	private LispExpr parseExpr() {
		if (isList())
			return parseList();
		if (isNumber())
			return parseNumber();
		if (isString())
			return parseString();
		if (isIdentifier())
			return parseIdentifier();
		return null;
	}
	
	private boolean isList() {
		return cur == '(';
	}
	
	private LispList parseList() {
		next();
		skipWhitespace();
		List<LispExpr> items = new LinkedList<LispExpr>();
		while (!eof() && cur != ')') {
			LispExpr expr = parseExpr();
			skipWhitespace();
			items.add(expr);
		}
		if (eof()) {
			parseError("Expected closing brace");
		}
		next();
		skipWhitespace();
		return new LispList(items);
	}
	
	private boolean isIdentifier() {
		return IDENT_CHARS.indexOf(cur) != -1;
	}
	
	private LispIdentifier parseIdentifier() {
		int start = pos;
		while (IDENT_CHARS.indexOf(cur) != -1) {
			next();
		}
		String name = new String(input, start, pos-start);
		skipWhitespace();
		return new LispIdentifier(name);
	}
	
	private boolean isString() {
		return cur == '"';
	}
	
	private LispString parseString() {
		next();
		StringBuffer value = new StringBuffer();
		while (!eof() && cur != '"') {
			if (cur == '\\') {
				next();
				if (eof()) {
					parseError("Incomplete escape sequence");
				}
				char c;
				switch (cur) {
					case 'n': c = '\n'; break;
					case 't': c = '\t'; break;
					// TODO ... etc
					default: c = cur; break;
				}
				value.append(c);
			} else {
				value.append(cur);
			}
			next();
		}
		if (eof()) {
			parseError("Expected closing quotes");
		}
		next();
		skipWhitespace();
		return new LispString(value.toString());
	}
	
	private boolean isNumber() {
		return Character.isDigit(cur) || cur == '-' || cur == '.';
	}
	
	private LispNumber parseNumber() {
		int start = pos;
		if (cur == '-')
			next();
		boolean haveDigit = false;
		while (Character.isDigit(cur)) {
			haveDigit = true;
			next();
		}
		if (!eof() && cur == '.') {
			next();
			while (Character.isDigit(cur)) {
				haveDigit = true;
				next();
			}
		}
		if (cur == 'e' || cur == 'E') {
			next();
			if (eof()) {
				parseError("Expected exponent");
			}
			if (cur == '-') {
				next();
			}
			if (eof() || !Character.isDigit(cur)) {
				parseError("Expected exponent");
			}
			do {
				next();
			} while (Character.isDigit(cur));
		}
		if (!haveDigit) {
			parseError("Numeric value with no digits");
		}
		String value = new String(input, start, pos-start);
		skipWhitespace();
		double val = Double.parseDouble(value);
		return new LispNumber(val);
	}
	
	private void skipWhitespace() {
		while (Character.isWhitespace(cur)) {
			next();
		}
	}
	
	private void next() {
		if (pos < input.length) {
			++pos;
			if (pos == input.length) {
				cur = '\0';
			} else {
				cur = input[pos];
			}
		}
	}
	
	private boolean eof() {
		return cur == '\0';
	}
	
	private void parseError(String message) {
		throw new ScionParseException(message); // TODO improve
	}
	
}
