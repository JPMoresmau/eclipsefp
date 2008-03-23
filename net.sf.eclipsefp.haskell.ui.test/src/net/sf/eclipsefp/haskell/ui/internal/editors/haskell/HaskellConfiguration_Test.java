/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import junit.framework.Assert;
import junit.framework.TestCase;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

public class HaskellConfiguration_Test extends TestCase {

	private static class MockContentAssistant extends ContentAssistant {

    private boolean fAutoInsert = false;

    @Override
    public void enableAutoInsert( final boolean enabled ) {
      fAutoInsert = enabled;
    }

    public void verify() {
      Assert.assertTrue( fAutoInsert );
    }
  }

	public void testProducesAutoInsertingAssitant() {
    IContentAssistantFactory factory = createMock( IContentAssistantFactory.class );
    MockContentAssistant assistant = new MockContentAssistant();
    expect( factory.createAssistant() ).andReturn( assistant ).once();

    replay( factory );

    SourceViewerConfiguration conf = new HaskellConfiguration( null, factory );
    IContentAssistant actualAssitant = conf.getContentAssistant( null );

    assertSame( assistant, actualAssitant );

    verify( factory );
    assistant.verify();
  }
}
