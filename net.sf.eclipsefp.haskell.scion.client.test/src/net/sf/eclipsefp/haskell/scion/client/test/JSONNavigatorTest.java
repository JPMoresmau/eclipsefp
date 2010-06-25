package net.sf.eclipsefp.haskell.scion.client.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONNavigator;
import org.json.JSONObject;
import org.junit.Test;

public class JSONNavigatorTest {

	@Test
	public void objectobjectstring() throws JSONException {
		JSONObject obj2=new JSONObject();
		obj2.put("val2","final");
		JSONObject obj1=new JSONObject();
		obj1.put("val1", obj2);
		Object o=JSONNavigator.opt(obj1, "val1","val2");
		assertTrue(o instanceof String);
		assertEquals("final",o);
	}
	
	@Test
	public void objectarraystring() throws JSONException {
		JSONArray obj2=new JSONArray();
		obj2.put("final1");
		obj2.put("final2");
		JSONObject obj1=new JSONObject();
		obj1.put("val1", obj2);
		Object o=JSONNavigator.opt(obj1, "val1","1");
		assertTrue(o instanceof String);
		assertEquals("final2",o);
	}
	
	@Test
	public void objectarrayobjectstring() throws JSONException {
		JSONObject obj2=new JSONObject();
		obj2.put("val2","final");
		JSONArray arr1=new JSONArray();
		arr1.put("final1");
		arr1.put(obj2);
		JSONObject obj1=new JSONObject();
		obj1.put("val1", arr1);
		Object o=JSONNavigator.opt(obj1, "val1","1","val2");
		assertTrue(o instanceof String);
		assertEquals("final",o);
	}
	
	@Test
	public void objectarraylength() throws JSONException {
		JSONArray obj2=new JSONArray();
		obj2.put("final1");
		obj2.put("final2");
		JSONObject obj1=new JSONObject();
		obj1.put("val1", obj2);
		Object o=JSONNavigator.opt(obj1, "val1","length");
		assertTrue(o instanceof Integer);
		assertEquals(2,((Integer)o).intValue());
	}
}
