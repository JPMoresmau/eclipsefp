/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.actions;

import net.sf.eclipsefp.haskell.core.project.YesodNature;

/**
 * add yesod nature
 *
 * @author JP Moresmau
 *
 */
public class AddYesodNatureAction extends AddHaskellNatureAction {

    @Override
    protected String getNature() {
     return YesodNature.NATURE_ID;
    }
}
