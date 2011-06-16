package net.sf.eclipsefp.haskell.browser.client;

import java.util.ArrayList;

import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Commands that can be sent to scion-browser.
 * 
 * @author serras
 */

public class Commands {

	public static JSONObject createLoadLocalDatabase(String path,
			boolean rebuild) throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "load-local-db");
		o.put("filepath", path);
		o.put("rebuild", rebuild);
		return o;
	}

	public static JSONObject createSetCurrentDatabase(CurrentDatabase current,
			PackageIdentifier id) throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "set-current-db");
		switch (current) {
		case ALL:
			o.put("new-db", "_all");
			break;
		case HACKAGE:
			o.put("new-db", "_hackage");
			break;
		case LOCAL:
			o.put("new-db", "_local");
			break;
		case PACKAGE:
			o.put("new-db", id.toJSON());
			break;
		}
		return o;
	}

	public static JSONObject createGetPackages() throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "get-packages");
		return o;
	}

	public static HaskellPackage[] responseGetPackages(String response)
			throws JSONException {
		JSONArray jPkgs = new JSONArray(response);
		ArrayList<HaskellPackage> aPkgs = new ArrayList<HaskellPackage>();
		for (int i = 0; i < jPkgs.length(); i++) {
			aPkgs.add(new HaskellPackage(jPkgs.getJSONObject(i)));
		}
		return aPkgs.toArray(new HaskellPackage[jPkgs.length()]);
	}

	public static JSONObject createGetAllModules() throws JSONException {
		return createGetModules("");
	}

	public static JSONObject createGetModules(String module)
			throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "get-modules");
		o.put("module", module);
		return o;
	}

	public static Module[] responseGetModules(String response)
			throws JSONException {
		JSONArray jMods = new JSONArray(response);
		ArrayList<Module> aMods = new ArrayList<Module>();
		for (int i = 0; i < jMods.length(); i++) {
			aMods.add(new Module(jMods.getJSONObject(i)));
		}
		return aMods.toArray(new Module[jMods.length()]);
	}

	public static JSONObject createGetDeclarations(String module)
			throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "get-declarations");
		o.put("module", module);
		return o;
	}

	public static Packaged<Declaration>[] responseGetDeclarations(
			String response) throws Exception {
		JSONArray jDecls = new JSONArray(response);
		ArrayList<Packaged<Declaration>> aDecls = new ArrayList<Packaged<Declaration>>();

		for (int i = 0; i < jDecls.length(); i++) {
			JSONArray pair = jDecls.getJSONArray(i);
			PackageIdentifier id = new PackageIdentifier(pair.getJSONObject(1));
			JSONArray declsInPkg = pair.getJSONArray(2);
			for (int j = 0; j < declsInPkg.length(); j++) {
				Declaration decl = Declaration.fromJSON(declsInPkg.getJSONObject(i));
				aDecls.add(new Packaged<Declaration>(id, decl));
			}
		}

		Packaged<Declaration>[] elts = (Packaged<Declaration>[]) new Packaged[aDecls.size()];
		return aDecls.toArray(elts);
	}
}
