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
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import static org.easymock.EasyMock.anyInt;
import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import net.sf.eclipsefp.haskell.core.internal.util.CompletionProposalTestCase;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist.doubles.StubViewer;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

public class HaskellContentAssistProcessor_PDETest extends CompletionProposalTestCase {

	public void testCallsCompletionContext() {
		final String input = "module CompletionEngineTest where\n" +
                             "\n" +
                             "putStr str = str\n" +
                             "\n" +
                             "main = pu";
		final int inputOffset = 62;

		final ICompletionProposal proposalResult = createProposal("pu", "putStr", 62);

		IHaskellCompletionContext context = createMock(IHaskellCompletionContext.class);
		expect(context.computeProposals())
				.andReturn(new ICompletionProposal[] {proposalResult});
		replay(context);

		ICompletionContextFactory factory = createMock(ICompletionContextFactory.class);
		expect(factory.createContext((ITextViewer) anyObject(), anyInt()))
			.andReturn(context);
		replay(factory);

		HaskellCAProcessor processor = new HaskellCAProcessor(factory);
		ICompletionProposal[] props = processor.computeCompletionProposals(
				                          new StubViewer(input), inputOffset);

		assertNotNull(props);
		assertContains(proposalResult, props);

		verify(context, factory);
	}

}
