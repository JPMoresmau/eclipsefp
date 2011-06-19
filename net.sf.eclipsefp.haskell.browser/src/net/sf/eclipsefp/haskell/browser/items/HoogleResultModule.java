package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class HoogleResultModule extends HoogleResult {
	PackageIdentifier pkg;
	Module mod;

	public HoogleResultModule(PackageIdentifier pkg, Module mod) {
		setType(HoogleResultType.MODULE);
		this.pkg = pkg;
		this.mod = mod;
	}

	public HoogleResultModule(JSONObject o) throws JSONException {
		setType(HoogleResultType.MODULE);
		JSONArray result = o.getJSONArray("result");
		this.pkg = new PackageIdentifier(result.getJSONObject(0));
		this.mod = new Module(result.getJSONObject(1));
	}

	public PackageIdentifier getPackageIdentifier() {
		return this.pkg;
	}
	
	public Module getModule() {
		return this.mod;
	}

	@Override
	public String getName() {
		return this.mod.getName();
	}
}
