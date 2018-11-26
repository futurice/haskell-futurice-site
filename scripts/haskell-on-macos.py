#!/usr/bin/env python3
#
# Program:            haskell-on-macos.py
# Description:        Install multiple GHC (and cabal-install) on macOS.
# Author:             Oleg Grenrus <oleg.grenrus@iki.fi>
# SPDX-Identifier-Id: BSD-3-Clause
#
# Copyright (c) 2017-2018 Futurice Oy
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
#
#     * Neither the name of Oleg Grenrus nor the names of other
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import argparse
import os
import os.path
import subprocess
import sys
import tempfile

class bcolors:
    ENDC = '\033[0m'
    RED = '\033[91m'
    YELLOW = '\033[93m'
    GREEN = '\033[92m'
    CYAN = '\033[96m'
    MAGENTA = '\033[95m'
    GRAY = '\033[37m'

# "rainbow"
colors = [
    bcolors.MAGENTA,
    bcolors.YELLOW,
    bcolors.GREEN,
    bcolors.CYAN,
]

class Fluffy:
    "This is not a shake, it's fluffy."

    def __init__(self):
        self.rules = {}
        self.ctx = Context(self)

    def build(self, target, **kwargs):
        self.ctx.build(target, **kwargs)

    def rule(self, target, phony=False):
        def rule_decorator(f):
            rule = Rule(f, phony)
            self.rules[target] = rule
            return rule

        return rule_decorator

    def set_dry(self, dry):
        self.ctx.set_dry(dry)

    def set_force(self, force):
        self.ctx.set_force(force)

    def set_verbose(self, verbose):
        self.ctx.set_verbose(verbose)

class Rule():
    def __init__(self, f, phony):
        self.phony = phony
        self.f = f

    def __call__(self, ctx):
        self.f(ctx)

class Context:
    "This is building context of Fluffy"

    def __init__(self, fluffy):
        self.fluffy = fluffy

        self.__done__  = set()
        self.__wait__  = set()

        self.__dry__ = False
        self.__force__ = False
        self.__verbose__ = False

    def set_dry(self, dry):
        self.__dry__ = dry == True

    def set_force(self, force):
        self.__force__ = force == True

    def set_verbose(self, verbose):
        self.__verbose__ = verbose == True

    def dry(self):
        return self.__dry__

    def build(self, target, force=False):
        # If we build this rule already, silently return
        if target in self.__done__:
            return

        # check if we know how to build the target
        if target not in self.fluffy.rules:
            # if we don't know, but target exists, it's ok.
            if not (self.__force__ or force) and os.path.isabs(target) and os.path.exists(target):
                self.info("DONE", target, "no rule, but exists in the file system")
                self.__done__.add(target)
                return

            # ... otherwise we error
            self.info("ERROR", target, "no rule to build")
            sys.exit(1)

        # Actual rule
        rule = self.fluffy.rules[target]

        # Check if the artifact is present for non-phony rules
        if not (self.__force__ or force) and not rule.phony and os.path.exists(target):
            self.info("DONE", target, "exists in the file system")
            self.__done__.add(target)
            return

        # Execute rule
        self.__wait__.add(target)
        rule(self)
        self.__wait__.remove(target)

        # Self-check: we have created the target
        if not self.__dry__ and not rule.phony and not os.path.exists(target):
            self.info("ERROR", target, "not created")
            sys.exit(1)

        self.info("DONE", target)
        self.__done__.add(target)

    def need(self, target):
        self.build(target)

    def info(self, pfx, *args):
        if pfx == "ERROR":
            c = bcolors.RED
        elif pfx == "DONE" or pfx == "RULE":
            c = colors[len(self.__wait__) % len(colors)]
        else:
            c = ""

        print("%s%8s%s %s" % (c, pfx, bcolors.ENDC, " ".join(args)))

    def curl(self, url, dest, checksum):
        self.info("CURL", url)
        destdir = os.path.dirname(dest)
        self.run(["curl", "-Lo", dest, url], check=True)
        checksum = "%s  ./%s\n" % (checksum, os.path.basename(dest))
        self.info("CHECKSUM", dest)
        self.run(["shasum", "-c", "-a", "256"], check=True, cwd=destdir, input=checksum.encode("utf-8"))

    def link(self, source, dest):
        self.info("LINK", dest)
        self.run(["ln", "-sf", source, dest], check=True)

    def mkdir(self, dir):
        self.info("MKDIR", dir)
        self.run(["mkdir", "-p", dir], check=True)

    def run(self, arg, **kwargs):
        if self.__verbose__:
            print("         %s%s%s" % (bcolors.GRAY, " ".join(arg), bcolors.ENDC))

        if not self.__dry__:
            subprocess.run(arg, **kwargs)

# GHC locations
ghcs = [
    {
        "version": "7.10.3",
        "url": "https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3b-x86_64-apple-darwin.tar.xz",
        "checksum": "b7cad2ea7badb7006621105fbf24b4bd364d2e51c1a75661978d9280d68e83a8",
    },
    {
        "version": "8.0.1",
        "url": "https://downloads.haskell.org/~ghc/8.0.1/ghc-8.0.1-x86_64-apple-darwin.tar.xz",
        "checksum": "06ec33056b927da5e68055147f165f873088f6812fe0c642c4c78c9a449fbc42",
    },
    {
        "version": "8.0.2",
        "url": "https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-x86_64-apple-darwin.tar.xz",
        "checksum": "ff50a2df9f002f33b9f09717ebf5ec5a47906b9b65cc57b1f9849f8b2e06788d",
    },
    {
        "version": "8.2.1",
        "url": "https://downloads.haskell.org/~ghc/8.2.1/ghc-8.2.1-x86_64-apple-darwin.tar.xz",
        "checksum": "900c802025fb630060dbd30f9738e5d107a4ca5a50d5c1262cd3e69fe4467188",
    },
    {
        "version": "8.2.2",
        "url": "https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-x86_64-apple-darwin.tar.xz",
        "checksum": "f90fcf62f7e0936a6dfc3601cf663729bfe9bbf85097d2d75f0a16f8c2e95c27",
    },
    {
        "version": "8.4.1",
        "url": "https://downloads.haskell.org/~ghc/8.4.1/ghc-8.4.1-x86_64-apple-darwin.tar.xz",
        "checksum": "d774e39f3a0105843efd06709b214ee332c30203e6c5902dd6ed45e36285f9b7",
    },
    {
        "version": "8.4.2",
        "url": "https://downloads.haskell.org/~ghc/8.4.2/ghc-8.4.2-x86_64-apple-darwin.tar.xz",
        "checksum": "87469222042b9ac23f9db216a8d4e5107297bdbbb99df71eb4d9e7208455def2",
    },
    {
        "version": "8.4.3",
        "url": "https://downloads.haskell.org/~ghc/8.4.3/ghc-8.4.3-x86_64-apple-darwin.tar.xz",
        "checksum": "af0b455f6c46b9802b4b48dad996619cfa27cc6e2bf2ce5532387b4a8c00aa64",
    },
    {
        "version": "8.4.4",
        "url": "https://downloads.haskell.org/~ghc/8.4.4/ghc-8.4.4-x86_64-apple-darwin.tar.xz",
        "checksum": "28dc89ebd231335337c656f4c5ead2ae2a1acc166aafe74a14f084393c5ef03a",
    },
    {
        "version": "8.6.2",
        "url": "https://downloads.haskell.org/~ghc/8.6.2/ghc-8.6.2-x86_64-apple-darwin.tar.xz",
        "checksum": "8ec46a25872226dd7e5cf7271e3f3450c05f32144b96e6b9cb44cc4079db50dc",
    },
]

# GHC aliases
aliases = ["ghc", "runghc", "ghci", "ghc-pkg", "haddock-ghc"]

cabals = [
    {
        "version": "head",
        "url": "https://haskell.futurice.com/files/cabal-ddec9b69e6a63c099bb550fa78ff3721ef3b4586-20181126-013621.xz",
        "checksum": "971f2d15f27a671af1b0cb767573516a87c251739aa955780122a9168ad83864",
    },
    {
        "version": "2.4.1.0",
        "url": "https://haskell.futurice.com/files/cabal-5e65672622d7f4edcc6e5ccecc50ce78c2786d22-20181126-011430.xz",
        "checksum": "663ead904afa4fa03e55064384467db40245c4329d4015a39b3d4ce2ac848a4b",
    },
    {
        "version": "2.4.0.0",
        "url": "https://haskell.futurice.com/files/cabal-0ba16f5c98ad612920f79e60d9105866096fb2e5-20180919-152709.xz",
        "checksum": "228585fd90773a7c133124395cac7e8ee29c079de481ae445078396bb6646334",
    },
]

def main():
    ghcIds = frozenset(map(lambda x: x["version"], ghcs))
    cabalIds = frozenset(map(lambda x: x["version"], cabals))

    def ghcPkg(i):
        return "ghc-" + i

    def cabalPkg(i):
        return "cabal-install-" + i

    pkgIds = frozenset(map(ghcPkg, ghcIds)).union(frozenset(map(cabalPkg, cabalIds)))

    def makeStrParser(name, xs):
        def strParser(s):
            if s in xs:
                return s
            else:
                raise argparse.ArgumentTypeError("Unknown %s: %s; try one of: %s" % (name, s, ", ".join(sorted(xs))))

        return strParser

    def dirParser(d):
        if os.path.isabs(d):
            return d
        else:
            raise argparse.ArgumentTypeError("Not an absolute directory path %s" % d)

    ghcParser = makeStrParser("ghc", ghcIds)
    cabalParser = makeStrParser("cabal", cabalIds)
    packageParser = makeStrParser("package", pkgIds)

    # Argument parser
    parser = argparse.ArgumentParser(description="Install Haskell tools on macOS")
    parser.add_argument("--make-dirs", dest="make_dirs", help="Create install directories (uses sudo)", action="store_true")
    parser.add_argument("--paths.d", dest="paths_d", help="Create /etc/paths.d/ghc (uses sudo)", action="store_true")
    parser.add_argument("--ghc-alias", type=ghcParser, dest="ghc_alias", action="store", metavar="GHC", help="Create versionless ghc aliases")
    parser.add_argument("--cabal-alias", type=cabalParser, dest="cabal_alias", action="store", metavar="CABAL", help="Create versionless cabal alias")
    parser.add_argument("--dry-run", dest="dry", action="store_true", help="Dry run: don't alter file-system")
    parser.add_argument("--force", dest="force", action="store_true", help="Execute rules even destination exists. Likely to fail.")
    parser.add_argument("--verbose", dest="verbose", action="store_true", help="Print commands executed")
    parser.add_argument("--install-dir", type=dirParser, metavar="DIR", default="/opt", dest="install_dir", action="store", help="Installation directory, default /opt")
    parser.add_argument("--download-dir", type=dirParser, metavar="DIR", default=os.path.join(os.environ["HOME"], "Downloads"), dest="download_dir", action="store", help="Downloads directory, default ~/Downloads")

    subparsers = parser.add_subparsers(metavar="command", dest="cmd", help="Command to execute: all, install")

    installP = subparsers.add_parser("install")
    installP.add_argument(dest="pkgs", metavar="PKG", type=packageParser, nargs='*', help="Which packages to install: %s" % ", ".join(sorted(pkgIds)))

    addP = subparsers.add_parser("all")

    res = parser.parse_args()

    # Installer
    installer = Fluffy()

    INSTALLDIR=res.install_dir
    DOWNLOADDIR=res.download_dir
    TEMPDIR=tempfile.mkdtemp()

    GHCDIR=os.path.join(INSTALLDIR, "ghc")
    CABALDIR=os.path.join(INSTALLDIR, "cabal")
    ALIASDIR=os.path.join(GHCDIR, "bin")

    CABALALIAS=os.path.join(ALIASDIR, "cabal")
    GHCALIAS=os.path.join(ALIASDIR, "ghc")

    def cabalPath(i):
        "Path to cabal binary of version i"
        return os.path.join(CABALDIR, i, "bin", "cabal")

    def ghcPath(i):
        "Path to ghc binary of version i"
        return os.path.join(GHCDIR, i, "bin", "ghc-" + i)

    @installer.rule(ALIASDIR)
    def aliasDirRule(ctx):
        ctx.need(GHCDIR)
        ctx.mkdir(ALIASDIR)

    def addGhc(ghc):
        installDir = os.path.join(GHCDIR, ghc["version"])
        ghcExe = ghcPath(ghc["version"])
        ghcAlias = os.path.join(ALIASDIR, "ghc-" + ghc["version"])
        pkgFile = os.path.basename(ghc["url"])
        downloadFile = os.path.join(DOWNLOADDIR, pkgFile)

        @installer.rule(ghcExe)
        def ghc_install(ctx):
            ctx.need(GHCDIR)
            ctx.need(downloadFile)

            ctx.info("UNTAR", downloadFile)
            ctx.run(["tar", "-xJf", downloadFile], check=True, cwd=TEMPDIR)

            ghcDir = os.path.join(TEMPDIR, "ghc-" + ghc["version"])
            ctx.info("CONFIGURE", "ghc", ghc["version"])
            ctx.run(["./configure", "--prefix=" + installDir], check=True, cwd=ghcDir)

            ctx.info("INSTALL", "ghc", ghc["version"])
            ctx.run(["make", "install"], check=True, cwd=ghcDir)

        @installer.rule(downloadFile)
        def ghc_download(ctx):
            ctx.curl(ghc["url"], downloadFile, ghc["checksum"])

        @installer.rule(ghcPkg(ghc["version"]), phony=True)
        def ghc_phony(ctx):
            ctx.need(ghcExe)
            ctx.need(ghcAlias)

        # The other aliases are side-effect of making ghc-version alias
        @installer.rule(ghcAlias)
        def programLink(ctx):
            ctx.need(ALIASDIR)
            ctx.need(ghcExe)

            for alias in aliases:
                exe = alias + "-" + ghc["version"]
                dest = os.path.join(ALIASDIR, exe)
                source = os.path.join(GHCDIR, ghc["version"], "bin", exe)
                ctx.link(source, dest)

    def addCabal(cabal):
        cabalExe = cabalPath(cabal["version"])
        cabalAlias = os.path.join(ALIASDIR, "cabal-" + cabal["version"])
        installDir = os.path.join(CABALDIR, cabal["version"])
        pkgFile = os.path.basename(cabal["url"])
        downloadFile = os.path.join(DOWNLOADDIR, pkgFile)

        binDir = os.path.dirname(cabalExe)

        @installer.rule(downloadFile)
        def cabal_download(ctx):
            ctx.curl(cabal["url"], downloadFile, cabal["checksum"])

        @installer.rule(cabalExe)
        def cabal_install(ctx):
            ctx.need(CABALDIR)
            ctx.need(downloadFile)
            ctx.mkdir(binDir)

            ctx.info("GUNZIP", downloadFile)
            # This is a ugly...
            if not ctx.dry():
                with open(cabalExe, "w") as f:
                    ctx.run(["gunzip", "-c", downloadFile], check=True, stdout=f)
            else:
                ctx.run(["gunzip", "-c", downloadFile], check=True)

            ctx.info("CHMOD", cabalExe)
            ctx.run(["chmod", "a+x", cabalExe], check=True)

        @installer.rule(cabalPkg(cabal["version"]), phony=True)
        def cabal_phony(ctx):
            ctx.need(cabalExe)
            ctx.need(cabalAlias)

        @installer.rule(cabalAlias)
        def cabal_alias(ctx):
            source = os.path.join(CABALDIR, cabal["version"], "bin", "cabal")
            ctx.need(ALIASDIR)
            ctx.need(source)
            ctx.link(source, cabalAlias)

    for ghc in ghcs:
        addGhc(ghc)
    for cabal in cabals:
        addCabal(cabal)

    # if --make-dirs is specified, create directories
    if res.make_dirs:
        @installer.rule(GHCDIR)
        def optGhcDir(ctx):
            ctx.info("MKDIR", GHCDIR)
            ctx.run(["sudo", "mkdir", "-p", GHCDIR], check=True)
            ctx.run(["sudo", "chown", os.environ["USER"], GHCDIR], check=True)

        @installer.rule(CABALDIR)
        def optcabalDir(ctx):
            ctx.info("MKDIR", CABALDIR)
            ctx.run(["sudo", "mkdir", "-p", CABALDIR], check=True)
            ctx.run(["sudo", "chown", os.environ["USER"], CABALDIR], check=True)

    # --cabal alias
    if res.cabal_alias is not None:
        @installer.rule(CABALALIAS)
        def cabalAlias(ctx):
            ctx.need(cabalPkg(res.cabal_alias))
            source = os.path.join(CABALDIR, res.cabal_alias, "bin", "cabal")
            ctx.link(source, CABALALIAS)

    if res.ghc_alias is not None:
        @installer.rule(GHCALIAS)
        def ghcAlias(ctx):
            ctx.need(ghcPkg(res.ghc_alias))

            for alias in aliases:
                exe = alias + "-" + res.ghc_alias
                dest = os.path.join(ALIASDIR, alias)
                source = os.path.join(GHCDIR, res.ghc_alias, "bin", exe)
                ctx.link(source, dest)

    if res.paths_d:
        @installer.rule("/etc/paths.d/ghc")
        def pathsDRule(ctx):
            ctx.run(["sudo", "sh", "-c", "echo " + ALIASDIR + " > /etc/paths.d/ghc"], check=True)

    def executeAliases():
        if res.cabal_alias is not None:
            installer.build(CABALALIAS, force=True)
        if res.ghc_alias is not None:
            installer.build(GHCALIAS, force=True)

    def executePathsD():
        if res.paths_d:
            installer.build("/etc/paths.d/ghc")

    # Behaviour flags
    installer.set_dry(res.dry)
    installer.set_force(res.force)
    installer.set_verbose(res.verbose)

    # Execute commands
    if res.cmd == "all":
        for r in pkgIds:
            installer.build(r)
        executeAliases()
        executePathsD()

    elif res.cmd == "install":
        for r in res.pkgs:
            installer.build(r)
        executeAliases()
        executePathsD()

    else:
        print("Command required")
        sys.exit(1)

if __name__ == "__main__":
    main()
