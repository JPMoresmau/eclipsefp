package net.sf.eclipsefp.haskell.util;

import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Map;

import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.net.proxy.IProxyService;
import org.osgi.util.tracker.ServiceTracker;

/**
 * network handling utilities
 * @author JP Moresmau
 *
 */
public class NetworkUtil {
	private static ServiceTracker proxyTracker;
	static {
		/**
		 * inspired from http://www.vogella.de/blog/2009/12/08/eclipse-rcp-proxy-preference/
		 */
		proxyTracker = new ServiceTracker(Activator.getContext(), IProxyService.class
                .getName(), null);
        proxyTracker.open();
	}
	
	/**
	 * adds a HTTP_PROXY environment entry if Eclipse has explicit proxy info
	 * @param pb
	 * @param urlS
	 */
	public static void addHTTP_PROXY_env(ProcessBuilder pb,String urlS) {

		String key="HTTP_PROXY";
		Map<String,String> m=pb.environment();
		if (!m.containsKey(key)){
			IProxyService proxyService =(IProxyService)proxyTracker.getService();
			IProxyData pd=null;
			String protocol="http";
			if (urlS!=null){
				try {
					URL url=new URL(urlS);
					protocol=url.getProtocol();
					IProxyData[] pds=proxyService.select(url.toURI());
					if (pds.length>0){
						pd=pds[0];
					}
				} catch (MalformedURLException mue){
					mue.printStackTrace();
				} catch (URISyntaxException use){
					use.printStackTrace();
				}
			} else {
				pd=proxyService.getProxyData(IProxyData.HTTP_PROXY_TYPE);
			}
			if (pd!=null){
				StringBuilder value=new StringBuilder(protocol);
				value.append("://");
				if (pd.getUserId()!=null && pd.getUserId().length()>0){
					value.append(pd.getUserId());
					value.append(":");
					value.append(pd.getPassword());
					value.append("@");
				}
				value.append(pd.getHost());
				value.append(":");
				value.append(pd.getPort());
				m.put(key, value.toString());
			}
		}
	}
	
}
