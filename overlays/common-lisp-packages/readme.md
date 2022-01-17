# common-lisp-packages

Nix library for the Common Lisp ecosystem

Warning: This library is __EXPERIMENTAL__ and everything can change

## purpose

The purpose of this library is leveraging the benefits of the [Nix](https://nixos.org/guides/how-nix-works.html) package manager to: 

- package ASDF systems
- build Lisp wrappers with systems in their closure

In addition, it implements a mechanism to import existing system definitions from Quicklisp.

## use cases

Use cases include:

- pinning down the exact commits of lisp libraries
- modifying libraries with your own patches
- using libraries not available in quicklisp
- automatic installation of libraries per project (nix-shell + direnv)
- automatic testing of lisp packages on multiple implementations

## usage

### packaging systems

Packages are declared using `build-asdf-system` - its API is documented in `default.nix`. Lots of examples are provided in `packages.nix`: for example, `cffi`, `cl-sqlite`, `hunchentoot` are packaged.

### building lisp wrappers

Lisp wrappers are built with `${lisp}WithPackages`. For example, to open a shell with SBCL + hunchentoot and cl-sqlite in PATH:
```
nix-shell -p 'sbclWithPackages (ps: [ ps.hunchentoot ps.cl-sqlite ])'
```

Then, inside `sbcl`, it's possible to load the libraries:
```
(require :asdf)
(asdf:load-system :hunchentoot)
(asdf:load-system :sqlite)
```

Supported `${lisp}`s are:

- `sbclWithPackages`
- `eclWithPackages`
- `abclWithPackages`
- `cclWithPackages`
- `claspWithPackages`

The closure of such a `${lisp}-with-packages` contains everything needed to run the specified lisp runtime and libraries. This includes:

- native libs (in this case, libcrypto and libsqlite)
- Java libs for ABCL (in this case, JNA for CFFI)
- compiler toolchain for ECL, etc.

### importing from Quicklisp

To generate Nix expressions from a Quicklisp release, run:

```
nix-shell   # if not using direnv
sbcl --script nix-quicklisp.lisp
```
This produces a `from-quicklisp.nix` containing Nix expressions for all packages in Quicklisp, which will be automatically picked up by `${lisp}WithPackages`. 

This is going to **take a while** because it needs to fetch the source code of each system to compute its sha256 hash (quicklisp provides a sha1 hash but Nix's `builtins.fetchTarball` requires a sha256.). During the first run, the sha256s are cached in `quicklisp.sqlite` and are reused in subsequent invocations. (Though feel free to save the earth some electricity and download a pre-filled database from https://galkowski.xyz/quicklisp.sqlite for free)

Quicklisp release url's are currently hard-coded and can be changed directly in the source code. 

#### nix <=2.3
To instantiate some of the imported expressions, you might need https://github.com/NixOS/nix/commit/cd44c0af71ace2eb8056c2b26b9249a5aa102b41 and a couple gigs of ram, as some of them are HUGE

So, if you experience stack overflow with nix <=2.3, try again with `ulimit -s 65536`

## other nix+CL projects

- https://github.com/SquircleSpace/ql2nix
- https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/lisp-modules
- https://github.com/jasom/ql2nix

## license

FreeBSD - see COPYING
