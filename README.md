# Lowlevel bindigs to the [GR](http://gr-framework.org) plotting framework

Based on version [0.37.0](https://github.com/sciapp/gr/tree/v0.37.0/lib/gks), the one installed by hombrew on osx (`brew install libgr`) at the time of creating the bindings. At some point I would like to add a high level interface, but I cannot predict when I will be able to put in thee time.

To install, first install `libGR` and set the `GRDIR` env variable, then run `opam install gr`.

The documentation is published here: [online documentation](http://www.mseri.me/ocaml-gr/gr/index.html).

Currently `libGr` is only looked through the default library paths of your system.
You can customise this by specifying the path to `libGR.so` or `libGR.dylib` with the environment variable `LIBGRPATH`.

Contributions are welcome.

## Warning

Very incomplete: I don't know how to deal with the ``meta thingy'' in ctypes, and I have yet to bind the GKS library.