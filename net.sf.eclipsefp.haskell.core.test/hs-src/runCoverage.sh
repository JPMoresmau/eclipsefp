#!/bin/bash
SRC=../../net.sf.eclipsefp.haskell.core/hs-src
mkdir tmp coverage_report
ghc -i$SRC -fhpc --make -odir tmp -hidir tmp AllTests_Suite.hs 
rm -r tmp
./AllTests_Suite 
hpc markup AllTests_Suite \
  --exclude=ValidateCabalFile_Test \
  --exclude=GHCOutputParser_Test \
  --srcdir=$SRC \
  --srcdir=. \
  --destdir=coverage_report
rm -r AllTests_Suite *.tix .hpc