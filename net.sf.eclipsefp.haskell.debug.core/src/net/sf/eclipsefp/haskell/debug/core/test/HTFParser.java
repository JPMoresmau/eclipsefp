/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.test;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IProject;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


/**
 * Parses the HTF JSON output file
 * @author JP Moresmau
 *
 */
public class HTFParser {
  private static final String TESTS="tests"; //$NON-NLS-1$
  private static final String LOCATION="location"; //$NON-NLS-1$
  private static final String PATH="path"; //$NON-NLS-1$
  private static final String LINE="line"; //$NON-NLS-1$
  private static final String FILE="file"; //$NON-NLS-1$
  private static final String TYPE="type"; //$NON-NLS-1$
  private static final String TEST_END="test-end"; //$NON-NLS-1$
  private static final String TEST_START="test-start"; //$NON-NLS-1$
  private static final String RESULT="result"; //$NON-NLS-1$
  private static final String RESULT_FAIL="fail"; //$NON-NLS-1$
  private static final String RESULT_PASS="pass"; //$NON-NLS-1$
  private static final String MESSAGE="message"; //$NON-NLS-1$
  private static final String TEST="test"; //$NON-NLS-1$
  private static final String WALL_TIME="wallTime"; //$NON-NLS-1$
  private static final String TEST_RESULTS="test-results"; //$NON-NLS-1$


  private static final String SEP="\n;;\n"; //$NON-NLS-1$

  /**
   * parse the test list (output of --list flag)
   * @param root the root test result
   * @param f the JSON file
   * @param p the project we're in, to calculate locations
   * @throws IOException
   * @throws JSONException
   */
  public static void parseTestList(final TestResult root,final File f,final IProject p) throws IOException, JSONException{
    String s=FileUtil.getContents( f,FileUtil.UTF8 );
    if (s.length()==0){
      return;
    }
    Map<String,TestResult> parents=new HashMap<>();
    parents.put( new JSONArray().toString(), root );
    int ix=s.indexOf(SEP);
    if (ix>-1){
      s=s.substring( 0,ix );
    }
    JSONObject obj=new JSONObject( s );
    JSONArray arr=obj.optJSONArray( TESTS );
    if (arr!=null){
      for (int a=0;a<arr.length();a++){
        JSONObject tcObj=arr.optJSONObject( a );
        newTestResult(tcObj,parents,p);
      }
    }

  }

  /**
   * create a new test result
   * @param tcObj
   * @param parents
   * @param p
   * @return
   * @throws JSONException
   */
  private static TestResult newTestResult(final JSONObject tcObj,final Map<String,TestResult> parents,final IProject p) throws JSONException{
    if (tcObj!=null){

      JSONArray qn=tcObj.optJSONArray( PATH );

      if (qn!=null){
          String name=qn.getString( qn.length()-1 );
          TestResult tr=new TestResult( name );
          parseLocation( tr,tcObj );
          tr.setProject( p );
          tr.setStatus( TestStatus.PENDING );
          parents.put(qn.toString() ,tr);
          getParent( qn, parents ).addChild( tr );
          return tr;
      }
    }
    return null;
  }

  /**
   * parse the location of the test
   * @param tr
   * @param tcObj
   * @throws JSONException
   */
  private static void parseLocation(final TestResult tr,final JSONObject tcObj) throws JSONException{
    JSONObject locObj=tcObj.optJSONObject( LOCATION );
    if (locObj!=null){
      int line=locObj.getInt( LINE );
      Location loc=new Location(locObj.getString(FILE),line,0,line,0);
      tr.setLocation( loc );
    }
  }

  /**
   * get the parent test result
   * @param qn
   * @param parents
   * @return
   * @throws JSONException
   */
  private static TestResult getParent(final JSONArray qn,final Map<String,TestResult> parents) throws JSONException{
    JSONArray qnParent=qn.removeLast();
    String key=qnParent.toString();
    TestResult parent=parents.get( key );
    if (parent==null){
      parent=new TestResult(qnParent.getString( qnParent.length()-1 ));
      parents.put(key,parent);
      getParent( qnParent, parents ).addChild(parent);
    }
    return parent;
  }

  /**
   * parse the test output file
   * @param root the root test result
   * @param f the output file
   * @param p the project, for resolving locations
   * @throws IOException
   * @throws JSONException
   */
  public static void parseTestOutput(final TestResult root,final File f,final IProject p) throws IOException, JSONException{
    String s=FileUtil.getContents( f,FileUtil.UTF8 );
    if (s.length()==0){
      return;
    }
    Map<String,TestResult> parents=new HashMap<>();
    fillParents(root, new JSONArray(),parents );
    //int ix=s.indexOf(SEP);
    //int start=0;
    //TestResult last=null;
    //while (ix>-1){
      //String obj=s.substring( start,ix ).trim();
     parse1TestOutput( s, parents, p );
     // start=ix+SEP.length();
     // ix=s.indexOf(SEP,start);

    //}
  }

  private static void fillParents(final TestResult tr,final JSONArray arr,final Map<String,TestResult> parents){
    parents.put( arr.toString(), tr );
    for (TestResult c:tr.getChildren()){
      JSONArray ca=new JSONArray();
      ca.putAll( arr );
      ca.put( c.getName() );
      fillParents( c, ca, parents );
    }
  }

  /**
   * parse 1 specific output
   * @param sobj the JSON string
   * @param parents the parents map
   * @param p the project for locations
   * @throws JSONException
   */
  private static void parse1TestOutput(final String sobj,final Map<String,TestResult> parents,final IProject p) throws JSONException{
    JSONObject rObj=new JSONObject(sobj);
    String tp=rObj.getString( TYPE );

    if (tp.equals( TEST_END )){
      JSONObject tcObj=rObj.getJSONObject( TEST );
      TestResult last=newTestResult( tcObj, parents, p );
      if (last!=null){
        String r=rObj.getString( RESULT );
        if (RESULT_PASS.equals(r)){
          last.setStatus( TestStatus.OK );
        } else if (RESULT_FAIL.equals(r)){
          last.setStatus( TestStatus.FAILURE );
        } else{
          last.setStatus( TestStatus.ERROR );
        }
        last.setText( rObj.optString(MESSAGE) );
        if (!TestStatus.OK.equals(last.getStatus())){
          parseLocation( last, rObj );
        }
        last.setWallTime( rObj.optLong( WALL_TIME, 0 ) );
      }
    } else if (tp.equals( TEST_START )){
      JSONObject tcObj=rObj.getJSONObject( TEST );
      TestResult tr=newTestResult( tcObj, parents, p );
      tr.setStatus( TestStatus.RUNNING ); // running until we get the corresponding test-end
      //return tr;
    } else if (tp.equals( TEST_RESULTS )){
      TestResult root=parents.get( new JSONArray().toString() );
      root.setWallTime( rObj.optLong( WALL_TIME, 0 ) );
    }
 //   return null;
  }

  /*

{"test":{"flatName":"Main:nonEmpty","location":{"file":"test\\Main.hs","line":12},"path":["Main","nonEmpty"],"sort":"unit-test"},"type":"test-start"}
;;
{"result":"fail","message":"assertEqual failed at test\\\\Main.hs:13\n* expected: [3, 2, 1]\n* but got:  [3]\n* diff:     \nC [3\nF , 2, 1\nC ]","test":{"flatName":"Main:nonEmpty","location":{"file":"test\\Main.hs","line":12},"path":["Main","nonEmpty"],"sort":"unit-test"},"wallTime":1,"type":"test-end","location":{"file":"test\\\\Main.hs","line":13}}
;;
{"test":{"flatName":"Main:empty","location":{"file":"test\\Main.hs","line":15},"path":["Main","empty"],"sort":"unit-test"},"type":"test-start"}
;;
{"result":"pass","message":"","test":{"flatName":"Main:empty","location":{"file":"test\\Main.hs","line":15},"path":["Main","empty"],"sort":"unit-test"},"wallTime":0,"type":"test-end","location":null}
;;
{"test":{"flatName":"Main:reverse","location":{"file":"test\\Main.hs","line":18},"path":["Main","reverse"],"sort":"quickcheck-property"},"type":"test-start"}
;;
{"result":"fail","message":"Falsifiable (after 3 tests and 1 shrink): \n[0,0]\nReplay argument: \"Just (275851486 2147483396,2)\"","test":{"flatName":"Main:reverse","location":{"file":"test\\Main.hs","line":18},"path":["Main","reverse"],"sort":"quickcheck-property"},"wallTime":0,"type":"test-end","location":null}
;;
{"failures":2,"passed":1,"pending":0,"wallTime":4,"errors":0,"type":"test-results"}
;;
*/



}
