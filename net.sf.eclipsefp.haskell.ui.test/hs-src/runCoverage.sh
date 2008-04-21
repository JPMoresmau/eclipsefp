#!/bin/bash
SRC_UI=../../net.sf.eclipsefp.haskell.ui/hs-src
SRC_CORE=../../net.sf.eclipsefp.haskell.core/hs-src
mkdir tmp coverage_report
ghc -i$SRC_CORE -i$SRC_UI -fhpc --make -odir tmp -hidir tmp AllTests_Suite.hs 
rm -r tmp
./AllTests_Suite 
hpc markup AllTests_Suite \
  --exclude=CodeFolding_Test \
  --srcdir=$SRC \
  --srcdir=. \
  --destdir=coverage_report
rm -r AllTests_Suite *.tix .hpc