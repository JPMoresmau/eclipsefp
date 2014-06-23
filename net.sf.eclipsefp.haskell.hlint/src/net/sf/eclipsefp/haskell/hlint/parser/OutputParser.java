/* Generated By:JavaCC: Do not edit this line. OutputParser.java */
package net.sf.eclipsefp.haskell.hlint.parser;

import java.util.*;
import net.sf.eclipsefp.haskell.hlint.*;

public class OutputParser implements OutputParserConstants {

  final public List<Suggestion> suggestions() throws ParseException {
  ArrayList<Suggestion > sugs= new ArrayList<>();
  Suggestion sug;
  SourceLocation loc;
    if (jj_2_4(3)) {
      label_1:
      while (true) {
        if (jj_2_1(3)) {
        } else {
          break label_1;
        }
        loc = location();
        if (jj_2_2(3)) {
          error();
        } else if (jj_2_3(3)) {
          sug = suggestion();
                                              sug.setLocation(loc);
                                              sugs.add(sug);
        } else {
          jj_consume_token(-1);
          throw new ParseException();
        }
      }
      jj_consume_token(INTEGER);
      string_until_eol();
    } else if (jj_2_5(3)) {
      jj_consume_token(7);
      jj_consume_token(EOL);
    } else {
      jj_consume_token(-1);
      throw new ParseException();
    }
    jj_consume_token(0);
    {if (true) return sugs;}
    throw new Error("Missing return statement in function");
  }

  final public void error() throws ParseException {
    jj_consume_token(8);
    jj_consume_token(EOL);
    jj_consume_token(9);
    jj_consume_token(EOL);
    jj_consume_token(SPACE);
    jj_consume_token(SPACE);
    jj_consume_token(8);
    string_until_eol();
    jj_consume_token(10);
    jj_consume_token(EOL);
    source_code();
    jj_consume_token(EOL);
  }

  final public SourceLocation location() throws ParseException {
  StringBuilder filename;
  Integer line;
  Integer column;
    filename = filename();
    line = number();
    column = number();
    jj_consume_token(SPACE);
    {if (true) return new SourceLocation(filename.toString().replace("\u005c\u005c\u005c\u005c","\u005c\u005c"), line, column);}
    throw new Error("Missing return statement in function");
  }

  final public Suggestion suggestion() throws ParseException {
  StringBuilder filename;
  Integer line;
  Severity sev;
  StringBuilder message;
  CodeModification pre;
  CodeModification post;
    sev = severity();
    jj_consume_token(SPACE);
    message = string_until_eol();
    pre = code_mod();
    post = code_mod();
    if (jj_2_6(3)) {
      jj_consume_token(11);
      string_until_eol();
    } else {
    }
    jj_consume_token(EOL);
    Suggestion s = new Suggestion();
    s.setSeverity(sev);
    s.setMessage(message.toString());
    s.setPre(pre);
    s.setPost(post);
    {if (true) return s;}
    throw new Error("Missing return statement in function");
  }

  final public StringBuilder filename() throws ParseException {
   Token c;
   StringBuilder s=new StringBuilder();
   StringBuilder rest=new StringBuilder();
    if (jj_2_7(3)) {
      c = jj_consume_token(COLON_SLASH);
              s.append(c.image);
    } else if (jj_2_8(3)) {
      c = jj_consume_token(OTHER_CHAR);
      s.append(c.image);
    } else if (jj_2_9(3)) {
      c = jj_consume_token(SPACE);
      s.append(c.image);
    } else {
      jj_consume_token(-1);
      throw new ParseException();
    }
    rest = string_until_colon();
        s.append(rest);
        {if (true) return s;}
    throw new Error("Missing return statement in function");
  }

  final public StringBuilder string_until_colon() throws ParseException {
  Token c;
  Token c1, c2;
  StringBuilder s=new StringBuilder();
    label_2:
    while (true) {
      if (jj_2_10(3)) {
      } else {
        break label_2;
      }
      if (jj_2_11(3)) {
        c = jj_consume_token(COLON_SLASH);
              s.append(c.image);
      } else if (jj_2_12(3)) {
        c = jj_consume_token(OTHER_CHAR);
      s.append(c.image);
      } else if (jj_2_13(3)) {
        c = jj_consume_token(SPACE);
      s.append(c.image);
      } else if (jj_2_14(3)) {
        c = jj_consume_token(INTEGER);
      s.append(c.image);
      } else {
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
    jj_consume_token(COLON);
        {if (true) return s;}
    throw new Error("Missing return statement in function");
  }

/*{
  c = < OTHER_CHAR >  s = string_until_colon()
  {    s.insert(0, c.image);
    return s;
  }
| c = < INTEGER >
  s = string_until_colon()
  {
    s.insert(0, c.image);
    return s;
  }
| c = < SPACE >
  s = string_until_colon()
  {
    s.insert(0, c.image);
    return s;
  }
| c = < COLON_SLASH >
  s = string_until_colon()
  {
    s.insert(0, c.image);
    return s;
  }
| < COLON >
  {    return new StringBuilder();
  }
}*/
  final public StringBuilder string_until_eol() throws ParseException {
  Token c;
  StringBuilder s=new StringBuilder();
    label_3:
    while (true) {
      if (jj_2_15(3)) {
      } else {
        break label_3;
      }
      if (jj_2_16(3)) {
        c = jj_consume_token(COLON_SLASH);
              s.append(c.image);
      } else if (jj_2_17(3)) {
        c = jj_consume_token(OTHER_CHAR);
      s.append(c.image);
      } else if (jj_2_18(3)) {
        c = jj_consume_token(COLON);
      s.append(c.image);
      } else if (jj_2_19(3)) {
        c = jj_consume_token(SPACE);
      s.append(c.image);
      } else if (jj_2_20(3)) {
        c = jj_consume_token(INTEGER);
      s.append(c.image);
      } else {
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
    jj_consume_token(EOL);
        {if (true) return s;}
    throw new Error("Missing return statement in function");
  }

/*{
  c = < OTHER_CHAR >
  s = string_until_eol()
  {
    s.insert(0, c.image);
    return s;
  }
| c = < SPACE >
  s = string_until_eol()
  {
    s.insert(0, c.image);
    return s;
  }
| c = < COLON_SLASH >
  s = string_until_eol()
  {
    s.insert(0, c.image);
    return s;
  }
| c = < COLON >
  s = string_until_eol()
  {
    s.insert(0, c.image);
    return s;
  }
| < EOL >
  {
    return new StringBuilder();
  }
}*/
  final public Integer number() throws ParseException {
  Token c;
    c = jj_consume_token(INTEGER);
    jj_consume_token(COLON);
    {if (true) return Integer.valueOf(c.image);}
    throw new Error("Missing return statement in function");
  }

  final public Severity severity() throws ParseException {
  StringBuilder sb;
    sb = string_until_colon();
    String s = sb.toString();
    if (s.equals("Ignore"))
      {if (true) return Severity.IGNORE;}
    else if (s.equals("Warning"))
      {if (true) return Severity.WARNING;}
    else if (s.equals("Error"))
      {if (true) return Severity.ERROR;}
    else
      {if (true) return null;}
    throw new Error("Missing return statement in function");
  }

  final public CodeModification code_mod() throws ParseException {
  StringBuilder s;
    string_until_eol();
    s = null;
    if (jj_2_21(3)) {
      s = source_code();
    } else {
    }
    if (s != null)
      {if (true) return new CodeModificationText(s.toString());}
    else
      {if (true) return new CodeModificationRemove();}
    throw new Error("Missing return statement in function");
  }

  final public StringBuilder source_code_line() throws ParseException {
  StringBuilder line;
    jj_consume_token(SPACE);
    jj_consume_token(SPACE);
    line = string_until_eol();
    {if (true) return line;}
    throw new Error("Missing return statement in function");
  }

  final public StringBuilder source_code() throws ParseException {
  StringBuilder line;
  StringBuilder all;
    all = source_code_line();
    label_4:
    while (true) {
      if (jj_2_22(3)) {
      } else {
        break label_4;
      }
      line = source_code_line();
      all.append('\u005cn');
      all.append(line.toString());
    }
    {if (true) return all;}
    throw new Error("Missing return statement in function");
  }

  private boolean jj_2_1(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_1(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(0, xla); }
  }

  private boolean jj_2_2(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_2(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(1, xla); }
  }

  private boolean jj_2_3(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_3(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(2, xla); }
  }

  private boolean jj_2_4(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_4(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(3, xla); }
  }

  private boolean jj_2_5(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_5(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(4, xla); }
  }

  private boolean jj_2_6(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_6(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(5, xla); }
  }

  private boolean jj_2_7(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_7(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(6, xla); }
  }

  private boolean jj_2_8(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_8(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(7, xla); }
  }

  private boolean jj_2_9(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_9(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(8, xla); }
  }

  private boolean jj_2_10(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_10(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(9, xla); }
  }

  private boolean jj_2_11(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_11(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(10, xla); }
  }

  private boolean jj_2_12(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_12(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(11, xla); }
  }

  private boolean jj_2_13(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_13(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(12, xla); }
  }

  private boolean jj_2_14(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_14(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(13, xla); }
  }

  private boolean jj_2_15(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_15(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(14, xla); }
  }

  private boolean jj_2_16(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_16(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(15, xla); }
  }

  private boolean jj_2_17(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_17(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(16, xla); }
  }

  private boolean jj_2_18(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_18(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(17, xla); }
  }

  private boolean jj_2_19(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_19(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(18, xla); }
  }

  private boolean jj_2_20(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_20(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(19, xla); }
  }

  private boolean jj_2_21(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_21(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(20, xla); }
  }

  private boolean jj_2_22(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_22(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(21, xla); }
  }

  private boolean jj_3R_13() {
    if (jj_3R_14()) return true;
    return false;
  }

  private boolean jj_3_10() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3_11()) {
    jj_scanpos = xsp;
    if (jj_3_12()) {
    jj_scanpos = xsp;
    if (jj_3_13()) {
    jj_scanpos = xsp;
    if (jj_3_14()) return true;
    }
    }
    }
    return false;
  }

  private boolean jj_3_11() {
    if (jj_scan_token(COLON_SLASH)) return true;
    return false;
  }

  private boolean jj_3R_14() {
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3_10()) { jj_scanpos = xsp; break; }
    }
    if (jj_scan_token(COLON)) return true;
    return false;
  }

  private boolean jj_3_20() {
    if (jj_scan_token(INTEGER)) return true;
    return false;
  }

  private boolean jj_3R_10() {
    if (jj_scan_token(SPACE)) return true;
    if (jj_scan_token(SPACE)) return true;
    if (jj_3R_8()) return true;
    return false;
  }

  private boolean jj_3_6() {
    if (jj_scan_token(11)) return true;
    if (jj_3R_8()) return true;
    return false;
  }

  private boolean jj_3R_6() {
    if (jj_scan_token(8)) return true;
    if (jj_scan_token(EOL)) return true;
    if (jj_scan_token(9)) return true;
    return false;
  }

  private boolean jj_3R_12() {
    if (jj_scan_token(INTEGER)) return true;
    return false;
  }

  private boolean jj_3_19() {
    if (jj_scan_token(SPACE)) return true;
    return false;
  }

  private boolean jj_3_3() {
    if (jj_3R_7()) return true;
    return false;
  }

  private boolean jj_3R_7() {
    if (jj_3R_13()) return true;
    if (jj_scan_token(SPACE)) return true;
    if (jj_3R_8()) return true;
    return false;
  }

  private boolean jj_3_9() {
    if (jj_scan_token(SPACE)) return true;
    return false;
  }

  private boolean jj_3_18() {
    if (jj_scan_token(COLON)) return true;
    return false;
  }

  private boolean jj_3_2() {
    if (jj_3R_6()) return true;
    return false;
  }

  private boolean jj_3_17() {
    if (jj_scan_token(OTHER_CHAR)) return true;
    return false;
  }

  private boolean jj_3_5() {
    if (jj_scan_token(7)) return true;
    if (jj_scan_token(EOL)) return true;
    return false;
  }

  private boolean jj_3_1() {
    if (jj_3R_5()) return true;
    return false;
  }

  private boolean jj_3_21() {
    if (jj_3R_9()) return true;
    return false;
  }

  private boolean jj_3_8() {
    if (jj_scan_token(OTHER_CHAR)) return true;
    return false;
  }

  private boolean jj_3_14() {
    if (jj_scan_token(INTEGER)) return true;
    return false;
  }

  private boolean jj_3_13() {
    if (jj_scan_token(SPACE)) return true;
    return false;
  }

  private boolean jj_3_15() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3_16()) {
    jj_scanpos = xsp;
    if (jj_3_17()) {
    jj_scanpos = xsp;
    if (jj_3_18()) {
    jj_scanpos = xsp;
    if (jj_3_19()) {
    jj_scanpos = xsp;
    if (jj_3_20()) return true;
    }
    }
    }
    }
    return false;
  }

  private boolean jj_3_16() {
    if (jj_scan_token(COLON_SLASH)) return true;
    return false;
  }

  private boolean jj_3R_8() {
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3_15()) { jj_scanpos = xsp; break; }
    }
    if (jj_scan_token(EOL)) return true;
    return false;
  }

  private boolean jj_3_7() {
    if (jj_scan_token(COLON_SLASH)) return true;
    return false;
  }

  private boolean jj_3R_5() {
    if (jj_3R_11()) return true;
    if (jj_3R_12()) return true;
    return false;
  }

  private boolean jj_3_4() {
    Token xsp;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3_1()) { jj_scanpos = xsp; break; }
    }
    if (jj_scan_token(INTEGER)) return true;
    if (jj_3R_8()) return true;
    return false;
  }

  private boolean jj_3_12() {
    if (jj_scan_token(OTHER_CHAR)) return true;
    return false;
  }

  private boolean jj_3R_11() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3_7()) {
    jj_scanpos = xsp;
    if (jj_3_8()) {
    jj_scanpos = xsp;
    if (jj_3_9()) return true;
    }
    }
    if (jj_3R_14()) return true;
    return false;
  }

  private boolean jj_3_22() {
    if (jj_3R_10()) return true;
    return false;
  }

  private boolean jj_3R_9() {
    if (jj_3R_10()) return true;
    return false;
  }

  /** Generated Token Manager. */
  public OutputParserTokenManager token_source;
  SimpleCharStream jj_input_stream;
  /** Current token. */
  public Token token;
  /** Next token. */
  public Token jj_nt;
  private int jj_ntk;
  private Token jj_scanpos, jj_lastpos;
  private int jj_la;
  private int jj_gen;
  final private int[] jj_la1 = new int[0];
  static private int[] jj_la1_0;
  static {
      jj_la1_init_0();
   }
   private static void jj_la1_init_0() {
      jj_la1_0 = new int[] {};
   }
  final private JJCalls[] jj_2_rtns = new JJCalls[22];
  private boolean jj_rescan = false;
  private int jj_gc = 0;

  /** Constructor with InputStream. */
  public OutputParser(java.io.InputStream stream) {
     this(stream, null);
  }
  /** Constructor with InputStream and supplied encoding */
  public OutputParser(java.io.InputStream stream, String encoding) {
    try { jj_input_stream = new SimpleCharStream(stream, encoding, 1, 1); } catch(java.io.UnsupportedEncodingException e) { throw new RuntimeException(e); }
    token_source = new OutputParserTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 0; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  /** Reinitialise. */
  public void ReInit(java.io.InputStream stream) {
     ReInit(stream, null);
  }
  /** Reinitialise. */
  public void ReInit(java.io.InputStream stream, String encoding) {
    try { jj_input_stream.ReInit(stream, encoding, 1, 1); } catch(java.io.UnsupportedEncodingException e) { throw new RuntimeException(e); }
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 0; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  /** Constructor. */
  public OutputParser(java.io.Reader stream) {
    jj_input_stream = new SimpleCharStream(stream, 1, 1);
    token_source = new OutputParserTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 0; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  /** Reinitialise. */
  public void ReInit(java.io.Reader stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 0; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  /** Constructor with generated Token Manager. */
  public OutputParser(OutputParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 0; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  /** Reinitialise. */
  public void ReInit(OutputParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 0; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  private Token jj_consume_token(int kind) throws ParseException {
    Token oldToken;
    if ((oldToken = token).next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    if (token.kind == kind) {
      jj_gen++;
      if (++jj_gc > 100) {
        jj_gc = 0;
        for (int i = 0; i < jj_2_rtns.length; i++) {
          JJCalls c = jj_2_rtns[i];
          while (c != null) {
            if (c.gen < jj_gen) c.first = null;
            c = c.next;
          }
        }
      }
      return token;
    }
    token = oldToken;
    jj_kind = kind;
    throw generateParseException();
  }

  static private final class LookaheadSuccess extends java.lang.Error { }
  final private LookaheadSuccess jj_ls = new LookaheadSuccess();
  private boolean jj_scan_token(int kind) {
    if (jj_scanpos == jj_lastpos) {
      jj_la--;
      if (jj_scanpos.next == null) {
        jj_lastpos = jj_scanpos = jj_scanpos.next = token_source.getNextToken();
      } else {
        jj_lastpos = jj_scanpos = jj_scanpos.next;
      }
    } else {
      jj_scanpos = jj_scanpos.next;
    }
    if (jj_rescan) {
      int i = 0; Token tok = token;
      while (tok != null && tok != jj_scanpos) { i++; tok = tok.next; }
      if (tok != null) jj_add_error_token(kind, i);
    }
    if (jj_scanpos.kind != kind) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) throw jj_ls;
    return false;
  }


/** Get the next Token. */
  final public Token getNextToken() {
    if (token.next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    jj_gen++;
    return token;
  }

/** Get the specific Token. */
  final public Token getToken(int index) {
    Token t = token;
    for (int i = 0; i < index; i++) {
      if (t.next != null) t = t.next;
      else t = t.next = token_source.getNextToken();
    }
    return t;
  }

  private int jj_ntk() {
    if ((jj_nt=token.next) == null)
      return (jj_ntk = (token.next=token_source.getNextToken()).kind);
    else
      return (jj_ntk = jj_nt.kind);
  }

  private java.util.List<int[]> jj_expentries = new java.util.ArrayList<>();
  private int[] jj_expentry;
  private int jj_kind = -1;
  private int[] jj_lasttokens = new int[100];
  private int jj_endpos;

  private void jj_add_error_token(int kind, int pos) {
    if (pos >= 100) return;
    if (pos == jj_endpos + 1) {
      jj_lasttokens[jj_endpos++] = kind;
    } else if (jj_endpos != 0) {
      jj_expentry = new int[jj_endpos];
      for (int i = 0; i < jj_endpos; i++) {
        jj_expentry[i] = jj_lasttokens[i];
      }
      jj_entries_loop: for (java.util.Iterator<?> it = jj_expentries.iterator(); it.hasNext();) {
        int[] oldentry = (int[])(it.next());
        if (oldentry.length == jj_expentry.length) {
          for (int i = 0; i < jj_expentry.length; i++) {
            if (oldentry[i] != jj_expentry[i]) {
              continue jj_entries_loop;
            }
          }
          jj_expentries.add(jj_expentry);
          break jj_entries_loop;
        }
      }
      if (pos != 0) jj_lasttokens[(jj_endpos = pos) - 1] = kind;
    }
  }

  /** Generate ParseException. */
  public ParseException generateParseException() {
    jj_expentries.clear();
    boolean[] la1tokens = new boolean[12];
    if (jj_kind >= 0) {
      la1tokens[jj_kind] = true;
      jj_kind = -1;
    }
    for (int i = 0; i < 0; i++) {
      if (jj_la1[i] == jj_gen) {
        for (int j = 0; j < 32; j++) {
          if ((jj_la1_0[i] & (1<<j)) != 0) {
            la1tokens[j] = true;
          }
        }
      }
    }
    for (int i = 0; i < 12; i++) {
      if (la1tokens[i]) {
        jj_expentry = new int[1];
        jj_expentry[0] = i;
        jj_expentries.add(jj_expentry);
      }
    }
    jj_endpos = 0;
    jj_rescan_token();
    jj_add_error_token(0, 0);
    int[][] exptokseq = new int[jj_expentries.size()][];
    for (int i = 0; i < jj_expentries.size(); i++) {
      exptokseq[i] = jj_expentries.get(i);
    }
    return new ParseException(token, exptokseq, tokenImage);
  }

  /** Enable tracing. */
  final public void enable_tracing() {
  }

  /** Disable tracing. */
  final public void disable_tracing() {
  }

  private void jj_rescan_token() {
    jj_rescan = true;
    for (int i = 0; i < 22; i++) {
    try {
      JJCalls p = jj_2_rtns[i];
      do {
        if (p.gen > jj_gen) {
          jj_la = p.arg; jj_lastpos = jj_scanpos = p.first;
          switch (i) {
            case 0: jj_3_1(); break;
            case 1: jj_3_2(); break;
            case 2: jj_3_3(); break;
            case 3: jj_3_4(); break;
            case 4: jj_3_5(); break;
            case 5: jj_3_6(); break;
            case 6: jj_3_7(); break;
            case 7: jj_3_8(); break;
            case 8: jj_3_9(); break;
            case 9: jj_3_10(); break;
            case 10: jj_3_11(); break;
            case 11: jj_3_12(); break;
            case 12: jj_3_13(); break;
            case 13: jj_3_14(); break;
            case 14: jj_3_15(); break;
            case 15: jj_3_16(); break;
            case 16: jj_3_17(); break;
            case 17: jj_3_18(); break;
            case 18: jj_3_19(); break;
            case 19: jj_3_20(); break;
            case 20: jj_3_21(); break;
            case 21: jj_3_22(); break;
          }
        }
        p = p.next;
      } while (p != null);
      } catch(LookaheadSuccess ls) { }
    }
    jj_rescan = false;
  }

  private void jj_save(int index, int xla) {
    JJCalls p = jj_2_rtns[index];
    while (p.gen > jj_gen) {
      if (p.next == null) { p = p.next = new JJCalls(); break; }
      p = p.next;
    }
    p.gen = jj_gen + xla - jj_la; p.first = token; p.arg = xla;
  }

  static final class JJCalls {
    int gen;
    Token first;
    int arg;
    JJCalls next;
  }

}
