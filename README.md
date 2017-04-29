# Chordemux
Chordemux is a MIDI filter/router utility which receives chords on its input channel, and routes chords' notes to different channels depending on chord type and inversion.

## Building instructions

Chordemux is written in Haskell and is compilable as usual Cabal project. Below are steps to build it from this repository.

* Install ghc, git, pkg-config, libasound2-dev and cabal-install (per the package manager used).
* Checkout the repository.

~~~
git clone https://github.com/dmgolubovsky/chordemux.git
~~~

* Change to the cloned repository directory, and update the Cabal packages database.

~~~
cabal update
~~~

* Install dependencies. The development files for the libasound2 library are required (in Debian/Ubuntu it is in the libasound2-dev package, in other Linux flavors the package name may differ).

~~~
cabal install --only-dependencies
~~~

* Build the program

~~~
cabal build
~~~

* Run the program in-place

~~~ 
cabal run
~~~

You will need to use a patchbay program like Claudia to connect the input and output ports of running chordemux with the MIDI source device (a keyboard or a sequencer), and with the recipients (QMidiArp or other sequencer/arpeggiator/drum program etc.)

These building instructions were tested on an Ubuntu 16.04 fresh Docker image.
