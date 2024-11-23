# Installing Unison

## Pre-built binaries

The easiest and quickest way is to install pre-built binaries available at many
package repositories. The caveat: some repositories are slow to update and
often include a very old version of Unison.

Alternatively, some pre-built binaries are made available at
https://github.com/bcpierce00/unison/releases for macOS, Linux and Windows.
No specific installation is needed, just unpack the files at a location of your
choosing.


## Building from source

### Packaging systems

Many packaging systems, including source-based systems like `pkgsrc`
and binary repositories like Debian GNU/Linux, make it easy to build
from source by handling all the dependencies for you and encoding the
unison build recipe.  Please refer to instructions provided by the
packaging system.  (Issues about packaging systems should be filed
with those systems, and not in the unison issue tracker.)

### Mostly-POSIX systems (GNU/Linux, BSDs, macOS, illumos-based OS, Solaris) and Cygwin

#### Build prerequisites

- A C99 compiler (e.g. gcc, clang)
- A recent version of OCaml compiler (version 4.08 at minimum)
  -- see https://ocaml.org/.  (Note that ocaml upstream says that 5.x is
  experimental.  The standard approach is the most recent 4.x ocaml
  release.)
- GNU make
- Basic POSIX tools: install, rm, sh

##### Optional, for the GUI only

- lablgtk3 and its prerequisites (GTK 3 and its dependencies)
- ocamlfind (there is backup code to operate without it in some circumstances)

##### Optional, on BSDs

- libinotify (optional, for building unison-fsmonitor)

##### Optional, for building the user manual

- sed
- LaTeX
- pdf2ps (optional, for PS output; included with Ghostscript, for example)
- HEVEA (https://hevea.inria.fr/) (optional, for HTML and text formats)
- Lynx (optional, for text format)
- (for developers only) HEVEA and Lynx are required to re-build the manual
  included in the Unison binary

#### Building

To build from source, first ensure that all prerequisites are
installed.  See each prerequisite's documentation for instructions, or
use a package manager.

Building from source is as simple as changing to the source directory
and executing:
```
gmake
```
where `gmake` is the command to run GNU make.  (Usually, GNU make is
available as "gmake", but on some systems it is only available as
"make".)  If you are using OPAM then `opam exec -- make` may work for
you, as opam needs to set up a specific environment.

Presence of lablgtk3 is detected automatically to build the GUI. If you want
to build only the GUI, type `make gui`. You can type `make tui` if you have
lablgtk3 installed but don't want the GUI built. Type `make fsmonitor` to build
only the filesystem monitor.

To install:
  - set `$PREFIX` in the environment if you don't want /usr/local
  - optionally set `$DESTDIR`
  - run `gmake install`.

The set of installed files (paths from the source directory) should be
```
src/unison              (the main executable for TUI/CLI)
src/unison-gui          (the main executable for GUI)
src/unison-fsmonitor    (optional, on some build platforms)
src/fsmonitor.py        (optional, if unison-fsmonitor is not built)
man/unison.1            (optional, manual page)
doc/unison-manual.*     (optional, user manual in different formats)
```

### Cross-compiling for a different target architecture or OS

To cross-compile for a different target, you need to have a cross-compilation
toolchain including both a cross-compiling C compiler and a cross-compiling
OCaml compiler. When you have cross-compilation toolchain in place, building
Unison from source works according to instructions above. You just have to add
a `TOOL_PREFIX` argument to `gmake` to indicate which toolchain to use (and
ensure the tools are in `$PATH`).

For example, to build a native Windows 64-bit executable using the MinGW
cross-compilation toolchain:
```
gmake TOOL_PREFIX=x86_64-w64-mingw32-
```

Building the manual page and documentation does not work when cross-compiling.
To build the documentation, first build Unison without cross-compilation.


### macOS

First, note that macOS is a mostly-POSIX system and see that section
above.

Second, note that macOS by default is missing a number of expected
tools, including POSIX-required system headers (e.g. all of
/usr/include).  The standard approach on macOS is to install either
Command Line Tools or Xcode, so that one has a working
C99/mostly-POSIX environment.  These also allow building against the
libraries required for the mac-native GUI.

#### Building

The mostly-POSIX instructions above will build the text user
interface, the GTK GUI and, if you have native GUI headers/libs, also
the macOS native GUI.

To build only the macOS native GUI, execute:
```
make macui
```

The built application will be located at `src/uimac/build/Default/Unison.app`.


### Windows

Building on Windows is currently somewhat complicated. All methods require
Cygwin as a POSIX-like layer for Windows. Cygwin is required for the build
process only; the build can produce fully native Windows binaries that don't
require Cygwin to run. To build Unison for usage within Cygwin environment,
follow build instructions for Unix-like OS above.

Builds are possible with MS Visual C++ (MSVC) (currently untested and likely
not working) and MinGW-w64 (currently the best option) compilers.

The build system automatically detects if the build is of MSVC, MinGW or Cygwin
GNU C (not native Windows) type based on the first OCaml compiler (ocamlc and
ocamlopt) found on PATH. Thus, if you have multiple compilers, you can easily
select between these methods by adjusting the PATH accordingly when running
`make`.

#### MinGW

Building with MinGW, you still need a Cygwin installation as the build
environment (if you are using OPAM, this can be installed automatically by
OPAM). Cygwin is not required to run the produced executables. You need to
have the following prerequisites:

- MinGW gcc and MinGW binutils (Cygwin package example mingw64-x86_64-gcc-core)
- A recent version of OCaml compiler (version 4.08 at minimum) which itself is
  built with MinGW gcc (it is possible to find pre-compiled binaries); do not
  use the Cygwin OCaml package as that is not compiled with MinGW. If using
  OPAM, be sure to include the system-mingw OPAM package when installing the
  OCaml compiler.
- GNU make
- A POSIX shell (available in Cygwin by default)

To build, change to directory where Unison source code is and execute:
```
make
```

If all goes well, the following files will be produced:
```
src/unison.exe              (the main executable for TUI/CLI)
src/unison-gui.exe          (the main executable for GUI, optional, see below)
src/unison-fsmonitor.exe    (filesystem monitor, optional)
```

You can check which DLLs are needed by the built binary by executing
`cygcheck src/unison.exe` or `ldd src/unison.exe`.

For building the GUI (optional), you also need the following:

- MinGW GTK 3 and its dependencies (Cygwin package example mingw64-x86_64-gtk3)
- Some MinGW GTK icon theme (optional, but recommended; Cygwin package example
  mingw64-x86_64-adwaita-icon-theme)
- lablgtk3 and its prerequisites (ocamlfind, dune build system)

Building lablgtk3 from source is complicated. It is recommended to use OPAM for
that (Cygwin package name opam). Make sure that OPAM uses the MinGW version of
gcc and binutils.

#### MSVC

Building with MSVC is in principle similar to building with MinGW, except that
the C compiler is now MSVC and the OCaml compiler must itself be built with
MSVC. A complete Visual Studio installation is not required, having Build Tools
installed is sufficient. It is possible to find pre-compiled OCaml compiler
binaries, but it may be easiest to use OPAM as it will also automatically set
up the correct environment for using MSVC and, if necessary, set up a Cygwin
environment behind the scenes. Just make sure to include the system-msvc OPAM
package when installing the OCaml compiler.

For building the GUI (optional) with MSVC, you also need the following:

- A native Windows GTK 3 installation (try https://github.com/wingtk/gvsbuild).
  Do not use GTK for Cygwin or MinGW.
  Make sure the installation location (typically C:\gtk\bin) is in the PATH
  environment variable.
- lablgtk3 and its prerequisites (ocamlfind, dune build system)


### Build options

There are some additional options that control the build process:

- NATIVE: If you can't compile a native binary for your platform then add
  `NATIVE=false` as argument to `make`. This will produce a single native
  executable with OCaml runtime and Unison bytecode embedded.
- CFLAGS, CPPFLAGS, LDFLAGS, LDLIBS control the build process as usual.
  OCaml compiler will pass these arguments on to the C compiler and linker.
- To produce a statically linked executable, add arguments suitable for your
  platform and toolchain in your LDFLAGS. Static linking mostly makes no
  difference because some widely-used C libraries, like glibc, do not allow
  static linking, GTK does not allow static linking and there is nothing to
  link statically for the textual user interface in Windows and Cygwin.
  Examples of LDFLAGS values to enable static linking:
    `-static` or `-Wl,-static` for gcc (should also work for clang);
    `-link -static` for gcc in Windows (MinGW, Cygwin, MSYS2) ("-link" is
    required due to flexlink being used in the toolchain).
