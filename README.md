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

# Annotated Example Config

```
# Sets the minimum diameter of the sun gear, this is useful because the sun gear,
# which is the output of the mechanism, may need to fit onto an existing shaft.
# Units are the same as those in the gear module parameter, typically mm.
minSunDiam = 12.0

# Set min and max number of teeth on all of the components. a minimum is useful
# since spur gears behave poorly with fewer than ~11 teeth. the maximum mostly
# reduces the search space for a solution.
maxNumTeeth = 100
minNumTeeth = 11

# Set range of gear module. For 3d printing, do some test prints to determine
# the lowest gear module you are able to print accurately. Currently min and max
# module must be distinct, so if you want a specific module, set the min to the
# desired module, and max to a value very slightly larger, e.g. 0.5 and 0.50001.
# this will produce near duplicate results in the output.
minModule = 0.5
maxModule = 0.7

# Allows you to enforce a minimum size for the planets to accomodate ball bearings.
# Units are the same as those in the gear module parameter, typically mm.
minPlanetDiam = 10.0

# Sets a maximum size for all components. useful to ensure clearance near other
# mechanisms and to ensure the design will fit on your print bed.
# Units are the same as those in the gear module parameter, typically mm.
maxDiam = 70.0

# These set the range for the absolute value of the desired gear reduction ratio.
# note that this mechanism is capable of producing negative (reversing) as well
# as positive (non-reversing) ratios. There is currently no way to specify to hSlew
# whether you want a reversing or non-reversing drive, but the output ratio is
# signed to indicate that.
minRatio = 5000.0
maxRatio = 6500.0

# Set minimum thickness for the floating ring gear.
# Units are the same as those in the gear module parameter, typically mm.
minDeltaRing = 12.0
```

# Design Usage
TODO: give description of differential planetary gear set topology, and
discussion of design considerations.

![](./example.jpeg)
