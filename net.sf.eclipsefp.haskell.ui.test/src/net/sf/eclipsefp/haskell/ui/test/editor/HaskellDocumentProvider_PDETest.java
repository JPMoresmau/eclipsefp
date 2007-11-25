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
package net.sf.eclipsefp.haskell.ui.test.editor;

import java.io.ByteArrayInputStream;

import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.ui.editor.HaskellAnnotationModel;
import net.sf.eclipsefp.haskell.ui.editor.HaskellDocumentProvider;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.part.FileEditorInput;

/**
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class HaskellDocumentProvider_PDETest extends TestCase {
	
	private HaskellDocumentProvider fProvider;
	private IEditorInput fEditorInput;
	private IProject fProject;
	private IFile fFile;

	@Override
	protected void setUp() throws Exception {
		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		fProject = workspace.getRoot().getProject("myProject");
		fProject.create(null);
		fProject.open(null);
		fFile = fProject.getFile("myHaskellSourceCode.txt");
		final String code = "\n" +
				                  "module Main where\n" +
				                  "\n" +
				                  "main = putStrLn $ show $ fat 4\n";
		fFile.create(new ByteArrayInputStream( code.getBytes() ), true, null );
		//what platform ui would do for us...
		fProvider = new HaskellDocumentProvider();
		fEditorInput = new FileEditorInput(fFile);
		fProvider.connect(fEditorInput);
		HaskellAnnotationModel listener =
			(HaskellAnnotationModel) fProvider.getAnnotationModel(fEditorInput);
		listener.connect(fProvider.getDocument(fEditorInput));
	}

	public void testUsesResourceMarkerAnnotationModels() {
		IAnnotationModel model = fProvider.getAnnotationModel(fEditorInput);
		assertTrue(model instanceof HaskellAnnotationModel);
	}
	
	public void testAnnotationModelCreatesOffsetPositionFromMatrixCoords() throws CoreException {
		IMarker marker = fFile.createMarker(HaskellCorePlugin.ID_PROBLEM_MARKER);
		marker.setAttribute(IMarker.LINE_NUMBER, 4);
		marker.setAttribute(IMarker.CHAR_START, 25);
		marker.setAttribute(IMarker.CHAR_END, 27);
		
		HaskellAnnotationModel model = 
			(HaskellAnnotationModel) fProvider.getAnnotationModel(fEditorInput);
		Position pos = model.getMarkerPosition(marker);

		assertEquals(45, pos.getOffset());
		assertEquals(3, pos.getLength());
	}

	@Override
	protected void tearDown() throws Exception {
		fProject.delete(true, null);
	}
	
	//TODO test the partitioner (that was provided by the deleted code)
	
}
