package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

public class HoogleResultPackage extends HoogleResult {
	HaskellPackage pkg;

	public HoogleResultPackage(HaskellPackage pkg) {
		setType(HoogleResultType.PACKAGE);
		this.pkg = pkg;
	}
	
	public HoogleResultPackage(JSONObject o) throws JSONException {
		setType(HoogleResultType.PACKAGE);
		this.pkg = new HaskellPackage(o.getJSONObject("result"));
	}
	
	public HaskellPackage getPackage() {
		return this.pkg;
	}

	@Override
	public String getName() {
		return this.pkg.getIdentifier().toString();
	}
}
