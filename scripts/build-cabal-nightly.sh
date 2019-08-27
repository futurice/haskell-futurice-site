#!/bin/sh

# Example:
#
# sh build-cabal-nightly.sh 4942b1f5841cc2a59c5c3eb1b59c90f65983dba8 $(expr $(date +%s) - 300) /tmp
#

set -e

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
if [ ! $(ghc-8.6.5 --numeric-version) = "8.6.5" ]; then
  die "Requires GHC 8.6.5. In path: $(ghc --version)"
fi

# TODO: official release might have different tag. Use it when done.
if [ ! $(cabal --numeric-version) = "2.4.1.0" ]; then
  die "Requires cabal-install 2.4. In path: $(cabal --numeric-version)"
fi

echo "INFO: $(ghc --version)"
cabal --version | sed 's/^/INFO: /'

# Update cabal index
cabal update

# Make temp directory
BUILDDIR=$(mktemp -d /tmp/build-cabal-nightly.XXXXXX)
trap 'rm -rf "$BUILDDIR"' EXIT

# Clone repository
cd $BUILDDIR
git clone --depth=50 --no-single-branch https://github.com/haskell/cabal cabal
cd cabal
git fetch origin $GITCOMMIT
git checkout -qf FETCH_HEAD

# Modify cabal.project
echo "packages: Cabal cabal-install" > cabal.project

# Build
echo "INFO: commit $GITCOMMIT"
echo "INFO: index-state: @$INDEXSTATE ($HUMANINDEXSTATE)"

cabal new-build -w ghc-8.6.5 --disable-tests --disable-benchmarks --index-state=@$INDEXSTATE cabal-install:exe:cabal

# Packaging
EXE=$(find dist-newstyle -type f -name cabal)

echo "INFO: built cabal"
$EXE --version | sed 's/^/INFO: /'

cp $EXE $OUTPUTDIR/cabal-$GITCOMMIT-$HUMANINDEXSTATE

echo "INFO: DONE"
