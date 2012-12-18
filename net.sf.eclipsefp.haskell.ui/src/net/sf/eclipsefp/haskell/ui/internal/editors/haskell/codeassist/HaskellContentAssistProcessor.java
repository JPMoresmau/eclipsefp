// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.Documented;
import net.sf.eclipsefp.haskell.browser.items.Gadt;
import net.sf.eclipsefp.haskell.browser.items.Instance;
import net.sf.eclipsefp.haskell.browser.items.Local;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.Packaged;
import net.sf.eclipsefp.haskell.browser.items.TypeClass;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.buildwrapper.types.ImportDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef;
import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineResult;
import net.sf.eclipsefp.haskell.buildwrapper.types.ThingAtPoint;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.AnImport;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.AnImport.FileDocumented;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.ImportsManager;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.ProposalScope;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.HaskellText;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContentAssistEvent;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.ICompletionListener;
import org.eclipse.jface.text.contentassist.ICompletionListenerExtension;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.keys.IBindingService;

/**
 * Computes the content assist completion proposals and context information. This class is fairly stateful, since
 * what are presented as completion proposals depends on where the editor's point is located and the preceding token's
 * lexical type.
 *
 * @author Leif Frenzel (original author)
 * @author B. Scott Michel (bscottm@ieee.org)
 * @author Alejandro Serrano
 * @author JP Moresmau
 */
public class HaskellContentAssistProcessor implements IContentAssistProcessor {
  /** The associated content assistant, used to add/remove listeners */
  private final ContentAssistant assistant;
  /** Current completion prefix */
  private String prefix;

  /** The different context states that the completion processor needs to track */
  enum CompletionContext {
      NO_CONTEXT
    , DEFAULT_CONTEXT
    , IMPORT_STMT
    , IMPORT_LIST
    , TYCON_CONTEXT
    , CONID_CONTEXT
    , LANGUAGE_EXTENSIONS_CONTEXT
  }

  /** Default number of tokens to grab before point when determining completion context */
 // private final static int NUM_PRECEDING_TOKENS = 10;

  /** The current completion context state */
  private CompletionContext context;
  /** The original prefix offset */
  private int prefixOffsetAnchor;

  // Module context variables:
  /** Module names in the modules graph */
  private ArrayList<String> moduleGraphNames;
  /** Module names exposed by the cabal project file */
  private ArrayList<String> exposedModules;

  /** The module name for import list completion */
  private String moduleName;

  private char[] autoCompletionCharacters=null;

  private Boolean searchAll;

  /**
   * the currently showing scope of proposals
   */
  private ProposalScope scope=ProposalScope.IMPORTED;
  /**
   * the key binding for content assist
   */
  private String contentAssistBinding;

  /**
   * The constructor.
   *
   * @param assistant The associated content assistant
   */
	public HaskellContentAssistProcessor(final ContentAssistant assistant) {
	  super();
	  this.assistant = assistant;
	  internalReset();

	  String s= HaskellUIPlugin.getDefault().getPreferenceStore().getString( IEditorPreferenceNames.CA_AUTOACTIVATION_TRIGGERS ); //new char[] { '.' };
    if (s!=null){
      autoCompletionCharacters=s.toCharArray();
    }

	  // Add the listener, who modulates the completion context
	  this.assistant.addCompletionListener( new CAListener() );

	}

	// =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
  // interface methods of IContentAssistProcessor
  // =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

	/**
	 * {@inheritDoc}
	 */
	@Override
  public ICompletionProposal[] computeCompletionProposals(final ITextViewer viewer, final int offset)	{
	  // reset to default
	  assistant.setStatusLineVisible( false );
	  assistant.setShowEmptyList( false );
	  //ScionTokenScanner sts=((HaskellEditor)HaskellUIPlugin.getTextEditor( viewer )).getScanner();
	  IFile theFile = HaskellUIPlugin.getFile( viewer );
	  IDocument doc = viewer.getDocument();

	  // Figure out what we're doing...
    //ScionInstance scion = HaskellUIPlugin.getScionInstance( viewer );

    String prf = getCompletionPrefix( doc, offset );
    if (!isHaskell( theFile )){
      scope=ProposalScope.ALL;
    } else {
      if (prf!=null){ // && (!prf.startsWith( prefix ) || prf.length()==0)){
        if (prf.length()>0){
          if (prf.equals( prefix )){
            scope=scope.next();
          }
        }
        // || !prf.startsWith( prefix )
        if (prf.length()==0){
          scope=ProposalScope.IMPORTED;
        }
      }
    }
   // HaskellUIPlugin.log( "prefix:"+prefix+", prf:"+prf+",scope:"+scope,IStatus.INFO );
    prefix=prf;
    switch (context) {
      case NO_CONTEXT: {
        //if ( scion != null ) {
          int offsetPrefix = offset - prefix.length();
          searchAll=prefix.length()>0;
          try {
            Location lineBegin = new Location(theFile.toString(), doc, doc.getLineInformationOfOffset( offset ));

            if (offsetPrefix < 0) {
              offsetPrefix = 0;
            }

            //Region point = new Region( offsetPrefix, 0 );
            //HaskellLexerToken[] tokens = scion.tokensPrecedingPoint( NUM_PRECEDING_TOKENS, theFile, doc, point );
            IRegion lineR=doc.getLineInformationOfOffset( offsetPrefix );
            String line=doc.get(lineR.getOffset(),lineR.getLength());

            String lineContents = doc.get( lineBegin.getStartOffset( doc ), offset - lineBegin.getStartOffset( doc ) );
            if (lineContents.trim().startsWith( "import" ) && lineContents.contains( "(" )) {
              context = CompletionContext.IMPORT_LIST;
              // Get the module name
              String[] words = lineContents.trim().split( "[ ]+" );
              if (words.length > 1) {
                moduleName = words[1];
                if (moduleName.equals( "qualified" )) {
                  if (words.length > 2) {
                    moduleName = words[2];
                  } else {
                    moduleName = null;
                  }
                }
              } else {
                moduleName = null;
              }
              // Return imports list
              return importsList( viewer, theFile, doc, offset );
            }

              if (line.startsWith( "import" )) {
                return moduleNamesContext(theFile,offset);
              } else if (line.startsWith("{-# LANGUAGE")){
                return getLanguageExtensions( offset );
              }  else if (line.contains("::") || line.contains("->")) {

                return defaultCompletionContext( viewer, theFile, doc, offset, true );
              }

            //if (tokens != null) {
            //  if (LexerTokenCategories.hasImportContext( tokens, lineBegin )) {
            //    return moduleNamesContext(theFile,offset);
            //  } else if (LexerTokenCategories.hasTyConContext( tokens, lineBegin )) {
             //   return defaultCompletionContext( viewer, theFile, doc, offset, true );
           //   }
           // }
          } catch (BadLocationException ble) {
            // Ignore, pass through to default completion context.
          }

          return defaultCompletionContext( viewer, theFile, doc, offset, false );
//        }

//        break;
      }

      case DEFAULT_CONTEXT: {
        return defaultCompletionContext( viewer, theFile, doc, offset, false );
      }

      case IMPORT_STMT: {
        return filterModuleNames( offset );
      }

      case TYCON_CONTEXT: {
        return defaultCompletionContext( viewer, theFile, doc, offset, true );
      }

      case IMPORT_LIST: {
        return importsList( viewer, theFile, doc, offset );
      }

      case CONID_CONTEXT: {
        return null;
      }
      case LANGUAGE_EXTENSIONS_CONTEXT: {
        return getLanguageExtensions( offset );
      }
    }

    return null;
	}

  @Override
  public IContextInformation[] computeContextInformation(final ITextViewer viewer, final int documentOffset) {
 // unused
		return null;
	}

	@Override
  public char[] getCompletionProposalAutoActivationCharacters() {
		return autoCompletionCharacters;
	}

	@Override
  public char[] getContextInformationAutoActivationCharacters() {
	  // unused
		return null;
	}

	@Override
  public String getErrorMessage() {
		// return null to indicate we had no problems
		return null;
	}

	@Override
  public IContextInformationValidator getContextInformationValidator() {
	  // unused
		return null;
	}

	/** Hard reset internal state */
	private void internalReset() {
	  context = CompletionContext.NO_CONTEXT;
    prefixOffsetAnchor = -1;
	  prefix = new String();
	  moduleGraphNames = null;
	  exposedModules = null;
	  moduleName = null;
	  scope=ProposalScope.IMPORTED;
	}

	/** Get the completion prefix from the prefix offset, if non-zero, or reverse lex */
	private String getCompletionPrefix( final IDocument doc, final int offset ) {
	  if (prefixOffsetAnchor > 0) {
	    try {
        return doc.get( prefixOffsetAnchor, offset - prefixOffsetAnchor );
      } catch( BadLocationException ex ) {
        // Should not happen, but fall through to lexCompletionPrefix() call
      }
	  }

    // Prefix offset anchor isn't set, or fell through as the result of the exception
	  String retval = lexCompletionPrefix( doc, offset );
    prefixOffsetAnchor = offset - retval.length();

    return retval;
}

	private boolean isHaskell(final IFile theFile){
	  return theFile!=null && BuildWrapperPlugin.getFacade( theFile.getProject() )!=null;
	}

	/**
	 * Default completion context, if no other context can be determined.
	 */
	private ICompletionProposal[] defaultCompletionContext( final ITextViewer viewer, final IFile theFile, final IDocument doc,
	                                                        final int offset, final boolean typesHavePriority) {
	  boolean needSearch=prefix.length()>0 && Boolean.TRUE.equals(searchAll) ;
	  // we display the scope status bar if we can cycle
	  if (needSearch){
	    if (isHaskell( theFile )){
    	  assistant.setStatusLineVisible( true );
    	  // this doesn't work in the constructor, for some reason, so use lazy init
    	  if (contentAssistBinding==null){
    	    IBindingService bsvc=(IBindingService)PlatformUI.getWorkbench().getService( IBindingService.class );
    	    contentAssistBinding=bsvc.getBestActiveBindingFormattedFor( "org.eclipse.ui.edit.text.contentAssist.proposals" );
    	  }
    	  String msg=NLS.bind( UITexts.proposal_category_mode, contentAssistBinding, scope.next().getDescription());

    	  assistant.setStatusMessage( msg);
	    } else {
	      assistant.setStatusLineVisible( false );
	    }
  	  // we need to show the list so the user sees the message to cycle through scopes
  	  assistant.setShowEmptyList( true );
	  }
	  context = typesHavePriority ? CompletionContext.TYCON_CONTEXT : CompletionContext.DEFAULT_CONTEXT;


	  HaskellCompletionContext haskellCompletions = new HaskellCompletionContext( doc.get(), offset );
	  // ICompletionProposal[] haskellProposals = haskellCompletions.computeProposals();
    HSCodeTemplateAssistProcessor templates = new HSCodeTemplateAssistProcessor();
    ICompletionProposal[] templateProposals = templates.computeCompletionProposals( viewer, offset );

    // Get rest of proposals
    String prefix = haskellCompletions.getPointedQualifier();
    HaskellEditor editor=(HaskellEditor)HaskellUIPlugin.getTextEditor( viewer );
    ImportsManager mgr =editor!=null?editor.getImportsManager():null;
        //new ImportsManager( theFile, doc );
    //long t0=System.currentTimeMillis();
    Map<String, Documented> decls = new HashMap<String, Documented>();
    //long t1=System.currentTimeMillis();
    //HaskellUIPlugin.log( "getDeclarations:"+(t1-t0), IStatus.INFO );
    ArrayList<String> elts = new ArrayList<String>();
    ArrayList<String> typeElts = new ArrayList<String>();

    // declaration key to package
    Map<String,String> packages=new HashMap<String, String>();
    // constructor key to declaration name (if we use a constructor from a new module, we need to import to data type, not the constructor)
    Map<String,String> constructors=new HashMap<String, String>();

    //ProposalScope ps=ProposalScope.valueOf( HaskellUIPlugin.getDefault().getPreferenceStore().getString( IEditorPreferenceNames.CA_PROPOSALS_SCOPE ) );

    Map<String,Documented> importeds=mgr!=null?mgr.getDeclarations():new HashMap<String, Documented>();
    IProject project=theFile.getProject();
    if (needSearch && !scope.equals( ProposalScope.IMPORTED )){
      try {
        Set<String> pkgs=ResourceUtil.getImportPackages( new IFile[]{theFile});

//        List<SymbolDef> sds=BuildWrapperPlugin.getDefault().getUsageAPI().listDefinedSymbols( theFile.getProject() );
//        for (SymbolDef sd:sds){
//          String name=sd.getName();
//          if (name.startsWith( prefix ) && !importeds.containsKey( name )){
//            name=name+" ("+sd.getModule()+")";
//            Documented d=null;
//            String comm=sd.getComment();
//            if (comm==null){
//              comm="";
//            }
//            switch (sd.getType()){
//              case UsageQueryFlags.TYPE_CONSTRUCTOR:
//                d=new Constructor( comm, sd.getName(), "?",null );
//                break;
//              case UsageQueryFlags.TYPE_TYPE:
//                d=new DataType( comm, new String[0], sd.getName(), new String[0], "", new Constructor[0] );
//                break;
//              case UsageQueryFlags.TYPE_VAR:
//                d=new Function( comm, sd.getName(), "" );
//            }
//            decls.put(name, d );
//          }
//        }

        if (scope.equals( ProposalScope.ALL )){
          // search on everything
          Packaged<Declaration>[] browserDecls=BrowserPlugin.getSharedInstance().getDeclarationsFromPrefix(Database.LOCAL, prefix);
          if (browserDecls.length > 0) {
            // If the browser found the module
            for (Packaged<Declaration> browserDecl : browserDecls) {
              boolean newPackage=!pkgs.contains( browserDecl.getPackage().getName() );
              addBrowserDecl( browserDecl, decls, packages, constructors, newPackage );
            }
          }
        } else if (scope.equals( ProposalScope.PROJECT )){
          // things in project
          for (IContainer src:ResourceUtil.getAllSourceContainers( theFile )){
            Collection<IFile> fs=ResourceUtil.getSourceFiles(src);
            addFileSymbols(prefix,fs,importeds,decls,constructors);
          }
          // we reference ourselves, more exactly the library stanza
          // so we need to retrieve exposed modules from the library
          if (pkgs.contains( project.getName() )){
            IFile cf=BuildWrapperPlugin.getCabalFile( project );
            PackageDescription pd=PackageDescriptionLoader.load(cf);
            Map<String,List<PackageDescriptionStanza>> pds=pd.getStanzasBySourceDir();
            // retrieve all possible source containers for the library
            Set<IContainer> srcs=new HashSet<IContainer>();
            PackageDescriptionStanza pdLibrary=null;
            for (String src:pds.keySet()){
              for (PackageDescriptionStanza pd1:pds.get( src )){
                if (CabalSyntax.SECTION_LIBRARY.equals(pd1.getType())){
                  srcs.add( ResourceUtil.getContainer(project,src) );
                  pdLibrary=pd1;
                  break;
                }
              }
            }
            if (pdLibrary!=null){
                Collection<IFile> fs=new ArrayList<IFile>();
                // find the file for all exposed modules
                String ps=pdLibrary.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
                List<String> ls=PackageDescriptionLoader.parseList( ps );
                for (String m:ls){
                  String path=m.replace( '.', '/' );
                  outer:for (String ext:FileUtil.haskellExtensions){
                    for (IContainer fldr:srcs){
                      IFile file=fldr.getFile( new Path( path+"."+ext ) ); //$NON-NLS-1$
                      if (file.exists()){
                        fs.add(file);
                        break outer;
                      }
                    }
                  }
                }
                addFileSymbols(prefix,fs,importeds,decls,constructors);
            }

          }

          // search on dependent packages only
          BWFacade f=BuildWrapperPlugin.getFacade( project );
          if (f!=null){
            for (CabalPackage[] cps:f.getPackagesByDB().values()){
              for (CabalPackage cp:cps){
                if (pkgs.contains( cp.getName() )){
                  if (!f.getProject().getName().equals( cp.getName() )){
                    Database pkg=Database.Package( new PackageIdentifier( cp.getName(), cp.getVersion() ) );
                    Packaged<Declaration>[] browserDecls=BrowserPlugin.getSharedInstance().getDeclarationsFromPrefix(pkg, prefix);
                    if (browserDecls.length > 0) {
                      // If the browser found the module
                      for (Packaged<Declaration> browserDecl : browserDecls) {
                        addBrowserDecl( browserDecl, decls, packages, constructors, false );

                      }

                    }
                  }
                }
              }
            }

          }
        }

      } catch (Exception e){
        HaskellUIPlugin.log( e );
      }
    }
    if (scope.equals( ProposalScope.IMPORTED )){
      addLocals( editor, project, theFile, doc, offset, importeds );

      decls.putAll(importeds);
    }

    for ( Map.Entry<String, Documented> s : decls.entrySet() ) {
      if ( s.getKey().startsWith( prefix ) && s.getValue()!=null) {
        if (s.getValue().isType()) {
          typeElts.add( s.getKey() );
        } else {
          elts.add( s.getKey() );
        }
      }
    }



    Comparator<String> pointedComparator = new Comparator<String>() {

      @Override
      public int compare( final String a, final String b ) {
        boolean aPointed = isPointed(a);
        boolean bPointed = isPointed(b);
        if (aPointed && !bPointed) {
          return 1;
        } else if (!aPointed && bPointed) {
          return -1;
        } else {
          return a.compareToIgnoreCase( b );
        }
      }

    };
    Collections.sort( elts, pointedComparator );
    Collections.sort( typeElts, pointedComparator );

    // Merge the results together (templates precede generated proposals):
    int totalSize = templateProposals.length + elts.size() + typeElts.size();
    int endIndex = 0;
    ICompletionProposal[] result = new ICompletionProposal[ totalSize ];

    if ( templateProposals.length > 0 ) {
      System.arraycopy( templateProposals, 0, result, endIndex, templateProposals.length );
      endIndex += templateProposals.length;
    }

    final int plength = prefix.length();

    int i = 0;

    if (typesHavePriority) {
      i = 0;
      for ( String s : typeElts ) {
        Documented d = decls.get( s );
        result[endIndex + i] = getCompletionProposal( s, d, offset, plength, true,packages.get( s ) ,null);
        i++;
      }
      endIndex += typeElts.size();
    }

    i = 0;
    for ( String s : elts ) {
      Documented d = decls.get( s );
      String realImport=null;
      if (d instanceof Constructor){
        realImport=constructors.get( s )+"(..)";
      }
      result[endIndex + i] =getCompletionProposal( s, d, offset, plength, false,packages.get( s )  ,realImport);
      i++;
    }
    endIndex += elts.size();

    if (!typesHavePriority) {
      i = 0;
      for ( String s : typeElts ) {
        Documented d = decls.get( s );
        result[endIndex + i] = getCompletionProposal( s, d, offset, plength, true,packages.get( s )  ,null);
        i++;
      }
      endIndex += typeElts.size();
    }

    //scope=scope.next();
    //HaskellUIPlugin.log( "scope:"+scope,IStatus.INFO);
    return result;
   // return (totalSize > 0 ? result : null);
	}

	private static void addFileSymbols(final String prefix, final Collection<IFile> fs,final Map<String,Documented> importeds,final Map<String, Documented> decls,final Map<String,String> constructors){
	  for (IFile f:fs){
      String module=ResourceUtil.getModuleName( f );
      for (FileDocumented fd:AnImport.getDeclarationsFromFile( f )){
        String name=fd.getDocumented().getName();
        if (name.startsWith( prefix ) && !importeds.containsKey( name )){
          name=name+" ("+module+")";
          decls.put(name, fd.getDocumented() );
          if (fd.getDocumented() instanceof Constructor){
            Constructor c=(Constructor)fd.getDocumented();
            if (c.getTypeName()!=null){
              constructors.put(name, c.getTypeName() );
            }
          }
        }
      }
    }
	}

	/**
	 * add local definition from the method
	 * @param editor
	 * @param project
	 * @param theFile
	 * @param doc
	 * @param offset
	 * @param importeds
	 */
	private static void addLocals(final HaskellEditor editor,final IProject project,final IFile theFile,final IDocument doc,final int offset,final Map<String,Documented> importeds){
	  if (editor!=null){
  	  BWFacade f=BuildWrapperPlugin.getFacade(project) ;
      if (f!=null){
        try {
        int line=doc.getLineOfOffset( offset );
        int col=offset-doc.getLineOffset( line );
        OutlineResult or=editor.getLastOutlineResult();
        if (or!=null){
          Location odL=null;
          for (OutlineDef od:or.getOutlineDefs()){
            Location l=od.getLocation();
            if (l.contains( line, col )){
              odL=l;
              for (OutlineDef odc:od.getChildren()){
                Location lc=odc.getLocation();
                if (lc.contains( line, col )){
                  odL=lc;
                  break;
                }
              }
              break;
            }
          }
          if (odL!=null){
            List<ThingAtPoint> taps=f.getLocals( theFile,odL);
            for (ThingAtPoint t:taps){
              Local func=new Local("",t.getName(),t.getType());
              if (!importeds.containsKey( t.getName() )){
                importeds.put(t.getName(),func);
              }
            }
          }
        }
        } catch (BadLocationException ble){
          HaskellUIPlugin.log( ble );
        }
       }
	  }
	}

	private static boolean isPointed(final String name){
	  int ix=name.indexOf( " (" );
	  if (ix>-1){
	    return name.substring(0,ix).indexOf( '.' ) != -1;
	  }
	  return name.indexOf( '.' ) != -1;
	}


	private void addBrowserDecl(final Packaged<Declaration> browserDecl,final Map<String, Documented> decls,final Map<String,String> packages,final Map<String,String> constructors, final boolean newPackage){
	  String key= browserDecl.getElement().getName()+" (";
	  // prepend package name before module to indicate this will create a new reference
	  if (newPackage){
	    key+=browserDecl.getPackage().getName()+":";
	  }
	  key+=browserDecl.getElement().getModule().getName()+")";
    if (!decls.containsKey( key ) && !decls.containsKey( browserDecl.getElement().getModule().getName()+"."+key )){
      decls.put(key,browserDecl.getElement() );
      if (newPackage){
        packages.put( key, browserDecl.getPackage().getName() );
      }
      if (browserDecl.getElement() instanceof Gadt) {
        Gadt g = (Gadt)browserDecl.getElement();
        for (Constructor c : g.getConstructors()) {
          key=  c.getName()+" ("+browserDecl.getElement().getModule().getName()+")";
          decls.put(key,c );
          constructors.put( key, browserDecl.getElement().getName() );
          if (newPackage){
            packages.put( key, browserDecl.getPackage().getName() );
          }
        }
      }
    }
	}



	private ICompletionProposal getCompletionProposal(final String s,final Documented d,final int offset,final int length,final boolean isType,final String pkg,final String realImport){
	  Image i=isType?
	      ImageCache.getImageForDeclaration( ((Declaration)d).getType() ):
        ImageCache.getImageForBinding( d ) ;

	  // new modules are shown after the declaration name
	  String repl=s;
	  int ix=s.indexOf( " (" );
	  String module=null;
	  if (ix>-1){
	    repl=s.substring( 0,ix );
	    module=s.substring( ix+2,s.length()-1 );
	    ix=module.indexOf( ":" );
	    if (ix>-1){
	      module=module.substring( ix+1 );
	    }
	  }

	  return new FullHaskellCompletionProposal( repl, offset - length, length, s.length(),
        i, s,
         HtmlUtil.generateDocument( d.getCompleteDefinition(), d.getDoc() ),pkg,module,realImport);
	}

	private ICompletionProposal[] importsList( final ITextViewer viewer, final IFile theFile,
	    final IDocument doc, final int offset ) {
	  // Case when we don't know the module name yet
	  if (moduleName == null) {
      return new ICompletionProposal[0];
    }

	  // Get prefix
	  HaskellCompletionContext haskellCompletions = new HaskellCompletionContext( doc.get(), offset   );
	  String prefix = haskellCompletions.getPointedQualifier();
	  int plength = prefix.length();
	  // Reuse the general "imports" code, getting out the qualified names
	  AnImport imp = new AnImport( new ImportDef(moduleName, null, false, false, null ),false);
	  Map<String, FileDocumented> decls = imp.getDeclarations( theFile.getProject(), theFile, doc );

	  ArrayList<String> names = new ArrayList<String>();
	  for (Map.Entry<String, FileDocumented> decl : decls.entrySet()) {
	    String s = decl.getKey();
	    if (s.indexOf( '.' ) == -1 && s.startsWith( prefix )) {
        // Don't add qualified imports
        Documented d = decl.getValue().getDocumented();
        if (!(d instanceof Instance) && !(d instanceof TypeClass)) {
          names.add( s );
        }
      }
	  }
	  Collections.sort( names );

	  ICompletionProposal[] r = new ICompletionProposal[names.size()];
	  for (int i = 0; i < names.size(); i++) {
	    String s = names.get( i );
	    Documented d = decls.get( s ).getDocumented();
	    r[i] = new CompletionProposal( s, offset - plength, plength, s.length(),
          d instanceof Constructor ? ImageCache.CONSTRUCTOR : ImageCache.getImageForDeclaration( ((Declaration)d).getType() ),
          s, null, HtmlUtil.generateDocument( d.getCompleteDefinition(), d.getDoc() ) );
	  }

	  return r;
	}

	/**
	 * Initialize the state necessary for the 'import' statement's context.
	 *
	 * @param scion The scion-server instance for the document's file/project
	 * @param prefix The prefix used to filter matching module names
	 * @param offset Offset into the document where proposals could be inserted
	 *
	 * @return A ICompletionProposal array of matching module names, or null, if none.
	 */
//	private ICompletionProposal[] moduleNamesContext(final ScionInstance scion, final int offset) {
//	  // Grab all of the module names, keep them cached for the duration of the completion session
//
//
//	  moduleGraphNames = new ArrayList<String>();
//	  moduleGraphNames.addAll( scion.moduleGraph() );
//
//	  exposedModules = new ArrayList<String>();
//	  exposedModules.addAll( scion.listExposedModules() );
//	  context = CompletionContext.IMPORT_STMT;
//
//	  return filterModuleNames( offset );
//	}

	private ICompletionProposal[] moduleNamesContext(final IFile file, final int offset) {
	  moduleGraphNames = new ArrayList<String>();
	  for (PackageDescriptionStanza pds: ResourceUtil.getApplicableStanzas( new IFile[]{file} )){
	    moduleGraphNames.addAll(pds.listAllModules());
	  }
	  exposedModules = new ArrayList<String>();
	  return filterModuleNames( offset );
	}
	/**
	 * Filter module names given a matching prefix.
	 *
	 * @param prefix The module name prefix
	 * @param offset The offset into the document where the completions will be inserted.
	 * @return An ICompletionProposal array of matching completions, or null if none.
	 */
	private ICompletionProposal[] filterModuleNames( final int offset ) {
    // List<String> modules = new ArrayList<String>();
    final String normalizedPrefix = prefix.toLowerCase(Locale.ENGLISH);

    Set<String> modules = new HashSet<String>();

    for (String m : BrowserPlugin.getSharedInstance().getCachedModuleNames()) {
      if (prefix.length() == 0 || m.toLowerCase().startsWith( normalizedPrefix )) {
        modules.add(m);
      }
    }

    for (String m : moduleGraphNames ) {
      if (prefix.length() == 0 || m.toLowerCase().startsWith( normalizedPrefix )) {
        modules.add(m);
      }
    }

    for (String m : exposedModules ) {
      if (prefix.length() == 0 || m.toLowerCase().startsWith( normalizedPrefix )) {
        modules.add(m);
      }
    }

    if (modules.size() > 0) {
      String[] modulesA = modules.toArray( new String[modules.size()] );
      Arrays.sort( modulesA, String.CASE_INSENSITIVE_ORDER );

      ICompletionProposal[] result = new ICompletionProposal[modulesA.length];
      int i = 0;
      final int prefixLength = prefix.length();

      for (String m : modulesA) {
        Module realM = BrowserPlugin.getSharedInstance().getCachedModule( m );
        result[i] = new CompletionProposal( m, prefixOffsetAnchor, prefixLength, m.length(), ImageCache.MODULE, m, null,
            realM == null ? "" : HtmlUtil.generateDocument( null, realM.getDoc() ));
        ++i;
      }

      return result;
    }

    return null;
	}

	private ICompletionProposal[] getLanguageExtensions(final int offset ) {
	  String normalizedPrefix = prefix.toUpperCase(Locale.ENGLISH);
	  int ix=normalizedPrefix.lastIndexOf( ',' );
	  if (ix==-1){
	    ix=normalizedPrefix.lastIndexOf( ' ' );
	  }
	  if (ix>-1){
	    prefixOffsetAnchor+=ix+1;
	    normalizedPrefix=normalizedPrefix.substring( ix+1 ).trim();
	  }
	  List<String> extensions=CompilerManager.getExtensions();
	  if (extensions!=null){
	    List<String> ext = new ArrayList<String>();
	    for (String e:extensions){
	      if (normalizedPrefix.equals("") || e.toUpperCase( Locale.ENGLISH ).startsWith( normalizedPrefix )){
	        ext.add(e);
	      }
	    }
	    ICompletionProposal[] result = new ICompletionProposal[ext.size()];
	    int i=0;
	    final int prefixLength = normalizedPrefix.length();
	    for (String e:ext){
	      result[i]=new CompletionProposal( e, prefixOffsetAnchor, prefixLength, e.length());
	      i++;
	    }
	    return result;
	  }
	  return null;
	}

	/**
	 * Filter completion pairs
	 */
  /*private ICompletionProposal[] getTypeCompletions( final ScionInstance scion, final IFile file, final IDocument doc,
      final int offset ) {

    Map<String, String> completionPairs = scion.completionsForTypes( file, doc );
    ArrayList<CompletionProposal> proposals = new ArrayList<CompletionProposal>();

    if( completionPairs != null ) {
      final String normalizedPrefix = prefix.toLowerCase();
      TreeSet<String> sortedKeys = new TreeSet<String>(
          String.CASE_INSENSITIVE_ORDER );
      final int prefixLength = prefix.length();

      sortedKeys.addAll( completionPairs.keySet() );
      for( String k: sortedKeys ) {
        // Key may be a qualified name
        String name = k;
        int qualifier = k.lastIndexOf( "." );

        if( qualifier > 0 ) {
          name = k.substring( qualifier + 1 );
        }

        if( prefix.length() == 0
            || name.toLowerCase().startsWith( normalizedPrefix ) ) {
          // String fullProposal = name + " -- " + completionPairs.get( k );
          //
          // if( !name.equals( k ) ) {
          //   fullProposal = fullProposal + " as " + k;
          // }

          CompletionProposal proposal = new CompletionProposal( k,
              prefixOffsetAnchor, prefixLength, k.length(), ImageCache.TYPE, k,
              null, "" );
          proposals.add( proposal );
        }
      }
    }
    return proposals.toArray( new ICompletionProposal[ proposals.size() ] );
  }*/

	/**
	 * Get the completion prefix by reverse lexing from the offset. The reverse lexing process stops at the beginning of the
	 * line on which the editor point (offset) is located, unless a token has been otherwise collected.
	 *
	 * @param document The document from which to extract the completion prefix
	 * @param offset The current editor point in the document.
	 * @return The completion prefix string or an empty string if reverse lexing did not find anything useful.
	 */
	public static final String lexCompletionPrefix( final IDocument document, final int offset ) {
    // If we're beyond the document limit (how?), return an empty string
    if( offset > document.getLength() ) {
      return new String();
    }

    try {
      IRegion lineAt = document.getLineInformationOfOffset( offset );
      final int lineBegin = lineAt.getOffset();
      int i = offset - 1;
      final char ch = document.getChar( i );

      if (HaskellText.isHaskellIdentifierPart( ch ) || ch == '.') {
        // Scan backward until non-identifier character
        for (--i; i >= lineBegin; --i) {
          char innerC = document.getChar( i );
          if ( !HaskellText.isHaskellIdentifierPart(innerC) && innerC != '.' ) {
            break;
          }
        }

        ++i;
        String retval = document.get( i, offset - i );

        if ( !retval.startsWith( "_" ) ) {
          return retval;
        }
      } else if ( HaskellText.isCommentPart( ch ) ) {
        // Scan backward until a non-comment character:
        for (--i; i >= lineBegin && HaskellText.isCommentPart( document.getChar( i ) ); --i) {
          // NOP
        }

        ++i;
        String retval = document.get( i, offset - i );

        // Ensure that the prefix is really the start of a comment.
        if (retval.startsWith( "{-" ) || retval.startsWith( "--" ) ) {
          return retval;
        }
      } else if ( HaskellText.isSymbol( ch ) ) {
        // Scan backward until a non-comment character:
        for (--i; i >= lineBegin && HaskellText.isSymbol( document.getChar( i ) ); --i) {
          // NOP
        }

        ++i;
        return document.get( i, offset - i );
      } else if (ch == '(' || ch == ')') {
        // Don't include parentheses in a prefix, e.g., ":: (<point>".
        return new String();
      } else if (!Character.isWhitespace( ch )) {
        // Punt! Grab what we can until we hit whitespace
        for (--i; i >= lineBegin && !Character.isWhitespace( document.getChar(i) ); --i) {
          // NOP
        }

        if (++i < offset) {
          return document.get( i, offset - i);
        }
      }
    } catch( BadLocationException e ) {
      // Dunno how we'd generate this exception, but catch it anyway and fall through
    }

    return new String();
	}

	/** Content assistant listener: This initializes and manages the transitions between completion context states. */
	private class CAListener implements ICompletionListener, ICompletionListenerExtension {
	  public CAListener() {
	    // NOP
	  }

	  @Override
    public void assistSessionStarted( final ContentAssistEvent event ) {
	    // HaskellUIPlugin.log( "CA session starts, prefix = '" + (prefix != null ? prefix : "<null>") + "', context = " + context, null );

	    // Reset the context to force computeCompletionProposals to figure out what the context,
	    // clean out existing state:
	    HaskellContentAssistProcessor.this.internalReset();
    }

    @Override
    public void assistSessionEnded( final ContentAssistEvent event ) {
      // HaskellUIPlugin.log( "CA session ends.", null);

      // Reset internal state for completeness
      HaskellContentAssistProcessor.this.internalReset();
    }

    @Override
    public void assistSessionRestarted( final ContentAssistEvent event ) {
      // HaskellUIPlugin.log( "CA session restarts, prefix = '" + (prefix != null ? prefix : "<null>") + "', context = " + context, null );
    }

    @Override
    public void selectionChanged( final ICompletionProposal proposal, final boolean smartToggle ) {
      // HaskellUIPlugin.log( "CA session selection changed, prefix = '" + (prefix != null ? prefix : "<null>") + "', context = " + context, null );
      // NOP
    }

	}
}