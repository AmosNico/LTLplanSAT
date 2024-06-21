# LTLplanSAT

## Introduction
TODO

## Prerequisites
* [Stack](https://docs.haskellstack.org/en/stable/) for compiling and running the project.
* Python should be installed and runnable from the command-line interface (for running Fast Downward).
* The SAT-solver [CryptoMiniSat](https://github.com/msoos/cryptominisat) version 5 needs to be downloaded in an executable called "crytominisat5" (this will usually be the default), and the location of the executables should be added to the path-variables.
* Optionally: The project automatically calls [VAL](https://github.com/KCL-Planning/VAL) to check wether the plan it finds is valid. For this the binaries of VAL need to be installed in a the directory "Val/bin" within this project. Running the project without VAL is possible, but you will probably get an error message after the plan has been returned.

## Structure of the project.
TODO

## Translation from PDDL to SAS
The translator of [Fast Downward](https://github.com/aibasel/downward) (copied into [src/translate](src/translate)) is used to translate the PDDL domain and problem files into a SAS file. The only changes to the files in this directory (with respect to ) are:
* The addition of ":constraints" and ":preferences" in the list of allowed requirements, see [src/translate/pddl/tasks.py](src/translate/pddl/tasks.py). 
* The verfication of ":metric" is removed (see lines 797-798 in [src/translate/pddl_parser/parsing_functions.py](src/translate/pddl_parser/parsing_functions.py)), since the addition of ":preferences" broke this test.

## Licence

Because Fast Downward is distributed under the GNU General Public License as published by the Free Software Foundation, this project falls under the same licence.