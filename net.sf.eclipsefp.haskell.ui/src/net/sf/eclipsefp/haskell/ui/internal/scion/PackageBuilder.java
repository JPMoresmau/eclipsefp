package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementation;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.NetworkUtil;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWTException;
import org.eclipse.ui.console.IOConsoleOutputStream;

/**
 * This class encapsulates the details of building a package for Scion, such as
 * unpacking the internal zip archive to the staging directory and running cabal to
 * produce the executable.
 *
 * Note: most of the contents of this class were previously in ScionBuilder,
 * but they were moved here to allow different packages to be built.
 *
 * @author B. Scott Michel (bscottm@ieee.org)
 * @author Alejandro Serrano (trupill@gmail.com)
 */
@Deprecated
public abstract class PackageBuilder {

  public static enum PackageBuilderMessages {
    UNPACK_ARCHIVE_TITLE,
    ARCHIVE_RESOURCE_NOT_FOUND,
    ARCHIVE_FILE_EXCEPTION,
    ARCHIVE_NON_SPECIFIC_EXTENSION,
    BUILD_JOB_TITLE,
    INSTALL_FAILED,
    INSTALL_ERROR;
  }

  public abstract InputStream getArchive();

  public abstract String getMessage(PackageBuilderMessages message);

  /** Unpack the package's archive to its final destination
  *
  * @param The final destination directory.
  * */
 public ScionBuildStatus unpackScionArchive(final File destdir) {
   ScionBuildStatus retval = new ScionBuildStatus();
   if( !destdir.exists() || destdir.list().length == 0 ) {
     // extract scion from bundled zip file
     InputStream is = this.getArchive();
     if( is == null ) {
       retval.buildFailed( getMessage( PackageBuilderMessages.UNPACK_ARCHIVE_TITLE ),
           getMessage( PackageBuilderMessages.ARCHIVE_RESOURCE_NOT_FOUND ) );
       return retval;
     }

     ZipInputStream zis = new ZipInputStream( is );
     ZipEntry ze = null;
     try {
       ze = zis.getNextEntry();
       while( ze != null ) {
         final int BUFFER_SIZE = 2048;
         byte[] data = new byte[ BUFFER_SIZE ];

         if( !ze.isDirectory() ) {
           File f = new File( destdir, ze.getName() );
           f.getParentFile().mkdirs();
           FileOutputStream fos = new FileOutputStream( f );
           BufferedOutputStream dest = new BufferedOutputStream( fos, BUFFER_SIZE );
           int count = 0;
           while( ( count = zis.read( data, 0, BUFFER_SIZE ) ) != -1 ) {
             dest.write( data, 0, count );
           }
           dest.close();
           fos.close();
         }
         ze = zis.getNextEntry();
       }
       zis.close();
     } catch( Exception e ) {
       String message;
       if (ze != null) {
         message = NLS.bind( getMessage( PackageBuilderMessages.ARCHIVE_FILE_EXCEPTION ), ze.getName(), e.toString() );
       } else {
         message = NLS.bind( getMessage( PackageBuilderMessages.ARCHIVE_NON_SPECIFIC_EXTENSION ), e.toString() );
       }
       // delete so we try to unzip next time
       FileUtil.deleteRecursively( destdir );
       retval.buildFailed( getMessage( PackageBuilderMessages.UNPACK_ARCHIVE_TITLE ), message );
     }
   }

   return retval;
 }

 public ScionBuildStatus update( final CabalImplementation cabalImpl, final IOConsoleOutputStream conout ) {
   final String cabalExecutable = cabalImpl.getCabalExecutableName().toOSString();
   ScionBuildStatus retval = new ScionBuildStatus();
   if( cabalExecutable.length() <= 0 ) {
     retval.buildFailed( UITexts.cabalUpdateJob_title, UITexts.zerolenCabalExecutable_message );
     return retval;
   }
   ArrayList<String> commands = new ArrayList<String>();
   commands.add( cabalExecutable );
   commands.add( "update" );
   commands.add( "-v" );
   ProcessBuilder pb = new ProcessBuilder( commands );
   pb.redirectErrorStream( true );
   NetworkUtil.addHTTP_PROXY_env( pb, NetworkUtil.HACKAGE_URL );
   String jobPrefix = getClass().getSimpleName();

   try {
     Process p = pb.start();
     BufferedReader is = new BufferedReader( new InputStreamReader( p.getInputStream(), "UTF8" ) );
     OutputWriter pbWriter = new OutputWriter( jobPrefix + "-OutputWriter", conout );
     InputReceiver  pbReader = new InputReceiver( jobPrefix + "-InputReader", is, pbWriter);

     pbReader.start();
     pbWriter.start();

     int code = p.waitFor();
     if( code != 0 ) {
       retval.buildFailed( UITexts.cabalUpdateJob_title, UITexts.cabalUpdateFailed );
     }

     pbWriter.setTerminate();
     pbWriter.interrupt();
     pbWriter.join();
     pbReader.setTerminate();
     pbReader.interrupt();
     pbReader.join();
   } catch( Exception e ) {
     retval.buildFailed( UITexts.cabalUpdateJob_title, UITexts.cabalUpdateError.concat( PlatformUtil.NL + e.toString() ) );
   }

   return retval;
 }

 /** Build the built-in Scion server using the Cabal.
  *
  * @param cabalImpl The Cabal implementation, which specifies the executable to run
  * @param destDir The destination directory, where the executable is built
  * @param conout The IOConsole output stream to which the build process' output is sent.
  */
 public ScionBuildStatus build( final CabalImplementation cabalImpl, final File destDir, final IOConsoleOutputStream conout ) {
   ArrayList<String> commands = new ArrayList<String>();
   ScionBuildStatus retval = new ScionBuildStatus();

   final String cabalExecutable = cabalImpl.getCabalExecutableName().toOSString();
   if( cabalExecutable.length() <= 0 ) {
     retval.buildFailed( getMessage( PackageBuilderMessages.BUILD_JOB_TITLE ), UITexts.zerolenCabalExecutable_message );
     return retval;
   }

   commands.add( cabalExecutable );
   HaskellUIPlugin.log( "cabal executable: ".concat( cabalExecutable )
       .concat(", cabal-install ")
       .concat( cabalImpl.getInstallVersion() )
       .concat(", Cabal library version ")
       .concat( cabalImpl.getLibraryVersion()),
       IStatus.INFO );

   String cabalLibVer = cabalImpl.getLibraryVersion();
   if( cabalLibVer.startsWith( "1.8." ) ) {
     commands.add( "-fcabal_1_8" );
   } else if( cabalLibVer.startsWith( "1.10." ) ) {
     commands.add( "-fcabal_1_10" );
   }

   commands.add( "install" );
   commands.add( "-v" );
   ProcessBuilder pb = new ProcessBuilder( commands );
   pb.directory( destDir );
   pb.redirectErrorStream( true );
   NetworkUtil.addHTTP_PROXY_env( pb, NetworkUtil.HACKAGE_URL );
   String jobPrefix = getClass().getSimpleName();

   try {
     Process p = pb.start();
     BufferedReader is = new BufferedReader( new InputStreamReader( p.getInputStream(), "UTF8" ) );
     OutputWriter pbWriter = new OutputWriter( jobPrefix + "-OutputWriter", conout );
     InputReceiver  pbReader = new InputReceiver( jobPrefix + "-InputReader", is, pbWriter);

     pbReader.start();
     pbWriter.start();

     int code = p.waitFor();
     if( code != 0 ) {
       retval.buildFailed( getMessage( PackageBuilderMessages.BUILD_JOB_TITLE ),
           getMessage( PackageBuilderMessages.INSTALL_FAILED ) );
     }

     pbWriter.setTerminate();
     pbWriter.interrupt();
     pbWriter.join();
     pbReader.setTerminate();
     pbReader.interrupt();
     pbReader.join();
   } catch( Exception e ) {
     retval.buildFailed( getMessage( PackageBuilderMessages.BUILD_JOB_TITLE ),
         getMessage( PackageBuilderMessages.INSTALL_ERROR ).concat( PlatformUtil.NL + e.toString() ) );
   }

   return retval;
 }

 /** Does the executable need building? (Note: this really only occurs if the
  * actual executable is not present in the staging directory.)
  */
 public abstract boolean needsBuilding();

 /**
  * A separate thread to write the communication with the server see
  * https://bugs.eclipse.org/bugs/show_bug.cgi?id=259107
  */
 public static class OutputWriter extends Thread {
   /** the message list **/
   private final LinkedList<String> messages;
   /** should we stop? **/
   private boolean                  terminateFlag;
   /** The output stream we'll write to. */
   private BufferedWriter           outStream;

   public OutputWriter(final String name, final OutputStream outStream) {
     super(name);
     messages = new LinkedList<String>();
     terminateFlag = false;
     this.outStream = null;
     try {
       this.outStream = new BufferedWriter( new OutputStreamWriter(outStream, "UTF8") );
     } catch (UnsupportedEncodingException exc) {
       // Keep Java happy
     }
   }

   public void setTerminate() {
     terminateFlag = true;
   }

   public void addMessage(final String msg) {
     synchronized (messages) {
       messages.add(msg + PlatformUtil.NL);
       messages.notify();
     }
   }

   public void addMessage(final char[] buf, final int start, final int length) {
     synchronized (messages) {
       messages.add(new String(buf, start, length));
       messages.notify();
     }
   }

   public void addMessage(final Exception e) {
     StringWriter sw = new StringWriter();
     PrintWriter pw = new PrintWriter(sw);
     e.printStackTrace(pw);
     pw.flush();
     synchronized (messages) {
       messages.add(sw.toString());
       messages.notify();
     }
   }

   @Override
   public void run() {
     while (!terminateFlag && outStream != null) {
       String m = null;
       synchronized (messages) {
         try {
           while (messages.isEmpty()) {
             messages.wait();
           }

         } catch (InterruptedException ignore) {
           // noop
         }
         if (!messages.isEmpty()) {
           m = messages.removeFirst();
         }
       }
       if (m != null) {
         try {
           outStream.write(m);
           outStream.flush();
         } catch (IOException ex) {
           // Nothing to do.
         } catch (SWTException se) {
           // probably device has been disposed
         }
       }
     }
   }
 }

 /**
  * The input receiver thread.
  */
 public static class InputReceiver extends Thread {

   private boolean terminateFlag;
   private BufferedReader inStream;
   private final OutputWriter outWriter;

   public InputReceiver( final String name, final BufferedReader stream,
       final OutputWriter outWriter ) {
     super( name );
     terminateFlag = false;
     this.inStream = stream;
     this.outWriter = outWriter;
   }

   public void setTerminate() {
     terminateFlag = true;
   }

   @Override
   public void run() {
     while( !terminateFlag && inStream != null ) {
       char[] buf = new char[1024];
       try {
         int nread = inStream.read( buf );
         if( nread > 0 ) {
           outWriter.addMessage( buf, 0, nread );
         } else if (nread < 0) {
           terminateFlag = true;
         }
       } catch( IOException ex ) {
         try {
           inStream.close();
         } catch( IOException ex1 ) {
           // Make Java happy.
         }
         inStream = null;
       }
     }
   }
 }
}
