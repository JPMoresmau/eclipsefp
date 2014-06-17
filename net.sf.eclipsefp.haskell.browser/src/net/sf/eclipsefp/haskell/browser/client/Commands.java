/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.client;

import java.util.ArrayList;

import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.DeclarationId;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Commands that can be sent to scion-browser.
 * 
 * @author Alejandro Serrano
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
	
	public static JSONObject createLoadHackageDatabase(String path,
			boolean rebuild) throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "load-hackage-db");
		o.put("filepath", path);
		o.put("rebuild", rebuild);
		return o;
	}
//
//	public static JSONObject createSetCurrentDatabase(DatabaseType current,
//			PackageIdentifier id) throws JSONException {
//		JSONObject o = new JSONObject();
//		o.put("command", "set-current-db");
//		switch (current) {
//		case ALL:
//			o.put("new-db", "_all");
//			break;
//		case HACKAGE:
//			o.put("new-db", "_hackage");
//			break;
//		case LOCAL:
//			o.put("new-db", "_local");
//			break;
//		case PACKAGE:
//			o.put("new-db", id.toJSON());
//			break;
//		}
//		return o;
//	}

	public static void setCurrentDatabase(Database db,JSONObject o) throws JSONException {
		
		switch (db.getType()) {
		case ALL:
			o.put("db", "_all");
			break;
		case HACKAGE:
			o.put("db", "_hackage");
			break;
		case LOCAL:
			o.put("db", "_local");
			break;
		case PACKAGE:
			o.put("db", db.getPkgId().toJSON());
			break;
		}
	}
	
	
	public static JSONObject createGetPackages(Database db) throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "get-packages");
		setCurrentDatabase(db,o);
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

	public static JSONObject createGetAllModules(Database db) throws JSONException {
		return createGetModules(db,"");
	}

	public static JSONObject createGetModules(Database db,String module)
			throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "get-modules");
		setCurrentDatabase(db,o);
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

	public static DeclarationId[] responseGetDeclarationId(String response)
			throws JSONException {
		JSONArray jMods = new JSONArray(response);
		ArrayList<DeclarationId> aMods = new ArrayList<DeclarationId>();
		for (int i = 0; i < jMods.length(); i++) {
			aMods.add(new DeclarationId(jMods.getJSONArray(i)));
		}
		return aMods.toArray(new DeclarationId[jMods.length()]);
	}
	
	public static JSONObject createGetDeclarations(Database db,String module)
			throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "get-declarations");
		setCurrentDatabase(db,o);
		o.put("module", module);
		return o;
	}

	public static JSONObject createGetDeclarationsFromPrefix(Database db,String prefix)
			throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "get-decl-prefix");
		setCurrentDatabase(db,o);
		o.put("prefix", prefix);
		return o;
	}
	
	public static Packaged<Declaration>[] responseGetDeclarations(
			String response) throws Exception {
		JSONArray jDecls = new JSONArray(response);
		ArrayList<Packaged<Declaration>> aDecls = new ArrayList<Packaged<Declaration>>();

		for (int i = 0; i < jDecls.length(); i++) {
			JSONArray pair = jDecls.getJSONArray(i);
			PackageIdentifier id = new PackageIdentifier(pair.getJSONObject(0));
			Declaration decl = Declaration.fromJSONSingleton(pair.getJSONObject(1));
			aDecls.add(new Packaged<Declaration>(id, decl));
		}

		@SuppressWarnings("unchecked")
		Packaged<Declaration>[] elts = new Packaged[aDecls.size()];
		return aDecls.toArray(elts);
	}
	
	public static Packaged<Declaration>[] responseGetDeclarationsFromPrefix(
			String response) throws Exception {
		JSONArray jDecls = new JSONArray(response);
		ArrayList<Packaged<Declaration>> aDecls = new ArrayList<Packaged<Declaration>>();

		for (int i = 0; i < jDecls.length(); i++) {
			JSONArray pair = jDecls.getJSONArray(i);
			PackageIdentifier id = new PackageIdentifier(pair.getJSONObject(0));
			Module m=new Module(pair.getJSONObject(1));
			Declaration decl = Declaration.fromJSONSingleton(pair.getJSONObject(2));
			decl.setModule(m);
			aDecls.add(new Packaged<Declaration>(id, decl));
		}

		@SuppressWarnings("unchecked")
		Packaged<Declaration>[] elts = new Packaged[aDecls.size()];
		return aDecls.toArray(elts);
	}
	
	public static JSONObject createSetExtraHooglePath(String newPath) throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "extra-hoogle-path");
		o.put("path", newPath);
		return o;
	}
	
	public static JSONObject createHoogleQuery(Database db,String query) throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "hoogle-query");
		setCurrentDatabase(db,o);
		o.put("query", query);
		return o;
	}
	
	public static HoogleResult[] responseHoogleQuery(String response) throws JSONException, Exception {
		JSONArray jResults = new JSONArray(response);
		ArrayList<HoogleResult> aResults = new ArrayList<HoogleResult>();
		
		for (int i = 0; i < jResults.length(); i++) {
			aResults.add(HoogleResult.fromJSON(jResults.getJSONObject(i)));
		}
		
		return aResults.toArray(new HoogleResult[jResults.length()]);
	}
	
	public static JSONObject createDownloadHoogleData() throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "hoogle-data");
		return o;
	}
	
	public static JSONObject createCheckHoogleData() throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "hoogle-check");
		return o;
	}
	
	public static JSONObject createFindModulesForDeclaration(Database db,String name) throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "get-decl-module");
		setCurrentDatabase(db,o);
		o.put("decl", name);
		return o;
	}
	
	public static JSONObject createQuit() throws JSONException {
		JSONObject o = new JSONObject();
		o.put("command", "quit");
		return o;
	}
}
