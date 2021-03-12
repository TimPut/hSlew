# hSlew

[![GPL-3.0-only license](https://img.shields.io/badge/license-GPL--3.0--only-blue.svg)](LICENSE)

hSlew is a tool to aid in the design of a novel differential planetary
gear system. 

# Basic Usage
hSlew take a path to a config file as it's only arguement. There is a
sample config in './test'. It outputs one solution to the design
constraints per line on stdout; I recommend piping the output into a
log file. Nota bene: no validation is currently done on the config
file, so if you enter unsatisfiable constraints such as minModule >=
maxModule, hSlew may enter an infinite loop; if this happens CTRL-C to
kill the process before reviewing the config and retrying.q

If you've compiled and have the binary on $PATH then:
``` $ hSlew ./path-to/config.toml ```
or if you've just checked out the repo:
``` $ cabal new-run hSlew ./path-to/config.toml ```

# Design Usage
TODO: give description of differential planetary gear set topology, and
discussion of design considerations.
