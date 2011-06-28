/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;


public class CompilerExtensionContentProvider implements ITreeContentProvider {

  public static String[] extensions = new String[] { "OverlappingInstances",
      "UndecidableInstances", "IncoherentInstances", "DoRec", "RecursiveDo",
      "ParallelListComp", "MultiParamTypeClasses", "NoMonomorphismRestriction",
      "FunctionalDependencies", "Rank2Types", "RankNTypes",
      "PolymorphicComponents", "ExistentialQuantification",
      "ScopedTypeVariables", "ImplicitParams", "FlexibleContexts",
      "FlexibleInstances", "EmptyDataDecls", "CPP", "KindSignatures",
      "BangPatterns", "TypeSynonymInstances", "TemplateHaskell",
      "ForeignFunctionInterface", "Arrows", "Generics", "NoImplicitPrelude",
      "NamedFieldPuns", "PatternGuards", "GeneralizedNewtypeDeriving",
      "ExtensibleRecords", "RestrictedTypeSynonyms", "HereDocuments",
      "MagicHash", "TypeFamilies", "StandaloneDeriving", "UnicodeSyntax",
      "PatternSignatures", "UnliftedFFITypes", "LiberalTypeSynonyms",
      "TypeOperators", "RecordWildCards", "RecordPuns",
      "DisambiguateRecordFields", "OverloadedStrings", "GADTs",
      "NoMonoPatBinds", "RelaxedPolyRec", "ExtendedDefaultRules",
      "UnboxedTuples", "DeriveDataTypeable", "ConstrainedClassMethods",
      "PackageImports", "ImpredicativeTypes", "NewQualifiedOperators",
      "PostfixOperators", "QuasiQuotes", "TransformListComp", "ViewPatterns",
      "XmlSyntax", "RegularPatterns", "TupleSections", "GHCForeignImportPrim",
      "NPlusKPatterns", "DoAndIfThenElse", "RebindableSyntax",
      "ExplicitForAll", "DatatypeContexts", "MonoLocalBinds", "DeriveFunctor",
      "DeriveTraversable", "DeriveFoldable" };

  public void dispose() {
    // Do nothing
  }

  public void inputChanged( final Viewer viewer, final Object oldInput,
      final Object newInput ) {
    // Do nothing
  }

  public Object[] getElements( final Object inputElement ) {
    return extensions;
  }

  public Object[] getChildren( final Object parentElement ) {
    return new Object[ 0 ];
  }

  public Object getParent( final Object element ) {
    return null;
  }

  public boolean hasChildren( final Object element ) {
    return false;
  }

}
