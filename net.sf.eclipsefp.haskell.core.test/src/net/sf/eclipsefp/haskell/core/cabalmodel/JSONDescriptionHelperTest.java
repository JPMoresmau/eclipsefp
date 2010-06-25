package net.sf.eclipsefp.haskell.core.cabalmodel;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.util.Set;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;

/**
 *
 * @author JP Moresmau
 *
 */
public class JSONDescriptionHelperTest {

	private JSONObject getJSONObject(final String file)throws IOException,JSONException{
		return new JSONObject(CabalModelTest.getContent( file ));

	}

	@Test
	public void executablesHaskell0() throws IOException,JSONException{
		JSONObject obj=getJSONObject("Haskell0.json");
		Set<String> exe=JSONDescriptionHelper.getNames(JSONDescriptionHelper.getExecutables( obj ));
		assertNotNull(exe);
		assertEquals(1,exe.size());
		assertTrue(exe.contains("main"));
	}

	@Test
  public void executablesScion() throws IOException,JSONException{
    JSONObject obj=getJSONObject("scion.json");
    Set<String> exe=JSONDescriptionHelper.getNames(JSONDescriptionHelper.getExecutables( obj ));
    assertNotNull(exe);
    assertEquals(1,exe.size());
    assertTrue(exe.contains("scion-server"));
  }

	@Test
  public void libraryHaskell0() throws IOException,JSONException{
    JSONObject obj=getJSONObject("Haskell0.json");
    assertFalse(JSONDescriptionHelper.hasLibrary( obj ));

  }

  @Test
  public void libraryScion() throws IOException,JSONException{
    JSONObject obj=getJSONObject("scion.json");
    assertTrue(JSONDescriptionHelper.hasLibrary( obj ));
  }

  @Test
  public void dependenciesHaskell0() throws IOException,JSONException{
    JSONObject obj=getJSONObject("Haskell0.json");
    Set<String> deps=JSONDescriptionHelper.getNames(JSONDescriptionHelper.getDependencies( obj ));
    assertNotNull(deps);
    assertEquals(2,deps.size());
    assertTrue(deps.contains(("containers")));
    assertTrue(deps.contains(("base")));
    deps=JSONDescriptionHelper.getNames(JSONDescriptionHelper.getDependencies( JSONDescriptionHelper.getExecutables( obj ).get( 0 )));
    assertNotNull(deps);
    assertEquals(2,deps.size());
    assertTrue(deps.contains(("containers")));
    assertTrue(deps.contains(("base")));
  }

  @Test
  public void dependenciesScion() throws IOException,JSONException{
    JSONObject obj=getJSONObject("scion.json");
    Set<String> deps=JSONDescriptionHelper.getNames(JSONDescriptionHelper.getDependencies( obj ));
    assertNotNull(deps);
    assertEquals(23,deps.size());
    assertTrue(deps.contains(("containers")));
    assertTrue(deps.contains(("base")));
    assertTrue(deps.contains(("directory")));
    assertTrue(deps.contains(("filepath")));
    assertTrue(deps.contains(("ghc")));
    assertTrue(deps.contains(("ghc-paths")));
    assertTrue(deps.contains(("ghc-syb")));
    assertTrue(deps.contains(("ghc-syb-utils")));
    assertTrue(deps.contains(("hslogger")));
    assertTrue(deps.contains(("json")));
    assertTrue(deps.contains(("multiset")));
    assertTrue(deps.contains(("time")));
    assertTrue(deps.contains(("uniplate")));
    assertTrue(deps.contains(("list-tries")));
    assertTrue(deps.contains(("binary")));
    assertTrue(deps.contains(("array")));
    assertTrue(deps.contains(("old-time")));
    assertTrue(deps.contains(("Cabal")));
    assertTrue(deps.contains(("bytestring")));
    assertTrue(deps.contains(("network")));
    assertTrue(deps.contains(("network-bytestring")));
    assertTrue(deps.contains(("utf8-string")));
    assertTrue(deps.contains(("QuickCheck")));

  }
}
