package net.sf.eclipsefp.haskell.core.test.codeassist;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

import net.sf.eclipsefp.haskell.core.codeassist.CompletionEngine;
import net.sf.eclipsefp.haskell.core.parser.test.util.Parser_PDETestCase;
import net.sf.eclipsefp.haskell.core.test.internal.doubles.StubHalamo;
import net.sf.eclipsefp.haskell.core.test.internal.doubles.StubModule;
import net.sf.eclipsefp.haskell.core.test.util.CompletionProposalTestCase;
import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;

public class CompletionEngine_PDETest extends Parser_PDETestCase {
	
	public void testDeclarationCompletion() throws CoreException {
		final String input = "module CompletionEngineTest where\n" +
				             "\n" +
				             "putStr str = str\n" +
				             "\n" +
				             "main = pu\n";
		final ICompilationUnit unit = parse(input);
		final CompletionEngine engine = new CompletionEngine();
		
		assertEquals('u', input.charAt(62 - 1));
		
		ICompletionProposal[] proposals = engine.complete(unit, 62);
		
		assertContains(createProposal("pu", "putStr", 62), proposals);
	}
	
	public void testPreludeClassCompletion() throws CoreException {
		final String input = "module CompletionEngineTest where\n" +
                             "\n" +
                             "fat :: N";
		final ICompilationUnit unit = parse(input);
		final CompletionEngine engine = new CompletionEngine();

		assertEquals('N', input.charAt(43 - 1));

		ICompletionProposal[] proposals = engine.complete(unit, 43);

		assertContains(createProposal("N", "Num", 43), proposals);
	}

	public void testKeywordCompletion() throws CoreException {
		final String input = "module CompletionEngineTest wh";
		//TODO avoid complaining about parsing error here
		final ICompilationUnit unit = parse(input);
		final CompletionEngine engine = new CompletionEngine();

		assertEquals('h', input.charAt(30 - 1));

		ICompletionProposal[] proposals = engine.complete(unit, 30);

		assertContains(createProposal("wh", "where", 30), proposals);
	}
	
	public void testDiscoverPreffixAfterLeftParen() throws CoreException {
		final String input = "module Factorial where\n" +
				             "\n" +
				             "fat 0 = 1\n" +
				             "fat 1 = n * (f";
		final ICompilationUnit unit = parse(input);
		final CompletionEngine engine = new CompletionEngine();
		
		assertEquals('f', input.charAt(48 - 1));

		ICompletionProposal[] proposals = engine.complete(unit, 48);

		assertContains(createProposal("f", "fat", 48), proposals);
	}
	
	public void testDoNotCompleteOnEmptyPrefix() throws CoreException {
		final String input = "module Factorial where\n" +
				             "\n" +
				             "fat 0 = 1\n" +
				             "fat 1 = n * (";
		final ICompilationUnit unit = parse(input);
		final CompletionEngine engine = new CompletionEngine();
		
		assertEquals('(', input.charAt(47 - 1));

		ICompletionProposal[] proposals = engine.complete(unit, 48);
		
		assertEquals(0, proposals.length);
	}
	
	public void testCompletePreffixWithUnderscore() throws CoreException {
		final String input = "module Underscore where\n" +
				             "\n" +
				             "_underscore = '_'\n" +
				             "prefixWithUnderscore str = _und";
		final ICompilationUnit unit = parse(input);
		final CompletionEngine engine = new CompletionEngine();
		
		assertEquals('d', input.charAt(74 - 1));

		ICompletionProposal[] proposals = engine.complete(unit, 74);

		assertContains(createProposal("_und", "_underscore", 74), proposals);
	}
	
	public void testSeeAcrossModules() throws CoreException {
		final String input = "module Main where\n" +
				             "\n" +
				             "main = putStr $ show $ f";
		final int offset = input.length();
		final ICompilationUnit unit = parse(input);
		final StubHalamo langModel = new StubHalamo();
		final CompletionEngine engine = new CompletionEngine(langModel);
		
		langModel.setModulesInScope(new StubModule("fat", "fib"));
		
		ICompletionProposal[] proposals = engine.complete(unit, offset);
		
		assertContains(createProposal("f", "fat", offset), proposals);
	}
	
	private void assertContains(ICompletionProposal proposal, ICompletionProposal[] proposals) {
		CompletionProposalTestCase.assertContains(proposal, proposals);
	}

	private ICompletionProposal createProposal(String replaced, String replacement, int offset) {
		return CompletionProposalTestCase.createProposal(replaced, replacement, offset);
	}
	
	//TODO test inter-module dependency fetching
	//create the Fibonacci module declaring the fibb function
	//create the Main module that imports the Fibonacci module and uses fibb
	
	//TODO seems like the functions from the module being completed are being listed twice
	//example: module Fibbonacci where
	//         fibb 0 = 1
	//         fi
	//when asking for code assistance here, we will get fibb listed twice
}
