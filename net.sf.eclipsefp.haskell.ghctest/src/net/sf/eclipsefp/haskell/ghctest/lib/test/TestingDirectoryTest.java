package net.sf.eclipsefp.haskell.ghctest.lib.test;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import static net.sf.eclipsefp.haskell.ghctest.lib.CommandRunner.consume;
import net.sf.eclipsefp.haskell.ghctest.lib.TestingDirectory;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TestingDirectoryTest {
	
	private TestingDirectory fDirectory;

	@Before public void createTempDirectory() {
		fDirectory = new TestingDirectory();
	}
	
	@After public void deleteTempDirectory() {
		fDirectory.destroy();
	}
	
	@Test public void shouldCreateDirectoryOnConstruction() {
		final File pathname = fDirectory.getPathname();
		
		assertTrue(pathname.exists());
		assertTrue(pathname.isDirectory());
	}

	@Test public void shouldCreateDifferentDirectories() {
		TestingDirectory fstDir = null;
		TestingDirectory sndDir = null;
		try {
			fstDir = new TestingDirectory();
			sndDir = new TestingDirectory();
			
			assertFalse(fstDir.getPathname().equals(sndDir.getPathname()));
		} finally {
			fstDir.destroy();
			sndDir.destroy();
		}
	}
	
	@Test public void destroyShouldDeleteTheDirectoryFromFileSystem() {
		fDirectory.destroy();
		
		assertFalse(fDirectory.getPathname().exists());
	}
	
	@Test public void createsFilesInsideDirectory() throws FileNotFoundException, IOException {
		File f = fDirectory.createFile("myFile.txt", "file contents");
		
		assertTrue(f.exists());
		assertEquals(f.getParentFile(), fDirectory.getPathname());
		assertContents("file contents", f);
	}

	@Test public void destroysNonEmptyDirectory() throws IOException {
		fDirectory.createFile("fileA.txt", "");
		
		fDirectory.destroy();
		
		assertFalse(fDirectory.getPathname().exists());
	}

	private void assertContents(String expectedContents, File file) throws FileNotFoundException, IOException {
		StringBuffer buf = new StringBuffer();
		consume(new FileReader(file), buf);
		assertEquals(expectedContents, buf.toString());
	}

}
