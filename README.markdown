machines
========

[![Hackage](https://img.shields.io/hackage/v/machines.svg)](https://hackage.haskell.org/package/machines) [![Build Status](https://secure.travis-ci.org/ekmett/machines.png?branch=master)](http://travis-ci.org/ekmett/machines)

*Ceci n'est pas une pipe*

Machines are demand driven input sources like pipes or conduits, but can support multiple inputs.

You design a `Machine` by writing a `Plan`. You then `construct` the machine.

Simple machines that take one input are called a `Process` and processes form a `Category`. More generally you can attach a
`Process` to the output of any type of `Machine`, yielding a new `Machine`.

More complicated machines provide other ways of connecting to them.

Typically the use of machines proceeds by using simple plans into machine `Tee`s and `Wye`s, capping many of the inputs to
those with possibly monadic sources, feeding the rest input (possibly repeatedly) and calling `run` or `runT` to get the
answers out.

There is a lot of flexibility when building a machine in choosing between empowering the machine to run its own monadic effects
or delegating that responsibility to a custom driver.

A port of this design to scala is available from runarorama/scala-machines

Runar's slides are also available from https://dl.dropbox.com/u/4588997/Machines.pdf

Some worked examples are here https://github.com/alanz/machines-play

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
