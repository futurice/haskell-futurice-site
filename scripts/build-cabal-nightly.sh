#!/bin/sh

set -ex

die() {
  echo "ERROR:" $*
  exit 1
}

if [ "$#" -ne 3 ]; then
  die "Usage: sh build-cabal-nightly.sh sha1 indexstate outputdir"
fi

GITCOMMIT=$1
INDEXSTATE=$2
OUTPUTDIR=$3

HUMANINDEXSTATE=$(TZ=UTC date -r ${INDEXSTATE} +"%Y%m%d-%H%M%S")

# Check versions
if [ ! $(ghc --numeric-version) = "8.2.1" ]; then
  die "Requires GHC 8.2.1. In path: $(ghc --version)"
fi

# TODO: official release might have different tag. Use it when done.
if [ ! $(cabal-2.0 --numeric-version) = "2.0.0.0" ]; then
  die "Requires cabal-install 2.0. In path: $(cabal-2.0 --numeric-version)"
fi

echo "INFO: $(ghc --version)"
cabal-2.0 --version | sed 's/^/INFO: /'

# Update cabal index
cabal-2.0 update

# Make temp directory
BUILDDIR=$(mktemp -d /tmp/build-cabal-nightly.XXXXXX)
# trap 'rm -rf "$BUILDDIR"' EXIT

# Clone repository
cd $BUILDDIR
git clone --depth=50 https://github.com/haskell/cabal cabal
cd cabal
git fetch origin $GITCOMMIT
git checkout -qf FETCH_HEAD

# Modify cabal.project
echo "packages: Cabal cabal-install" > cabal.project

# Build
echo "INFO: commit $GITCOMMIT"
echo "INFO: index-state: @$INDEXSTATE ($HUMANINDEXSTATE)"

cabal-2.0 new-build --disable-tests --disable-benchmarks cabal-install:exe:cabal

# Packaging
EXE=$(find dist-newstyle -type f -name cabal)

echo "INFO: built cabal"
$EXE --version | sed 's/^/INFO: /'

cp $EXE $OUTPUTDIR/cabal-$GITCOMMIT-$HUMANINDEXSTATE

echo "INFO: DONE"
