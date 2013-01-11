/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.List;

/**
 * handle import cleans
 * @author JP Moresmau
 *
 */
public interface ImportCleanHandler {

	void handleImportCleans(List<ImportClean> cleans);
}
