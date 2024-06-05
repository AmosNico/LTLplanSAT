# LTLplanSAT

## Translation from PDDL to SAS
The translator of [Fast Downward](https://github.com/aibasel/downward) (copied into [src/translate](src/translate)) is used to translate the PDDL domain and problem files into a SAS file. The only changes to the files in this directory (with respect to ) are:
* The addition of ":constraints" and ":preferences" in the list of allowed requirements, see [src/translate/pddl/tasks.py](src/translate/pddl/tasks.py). 
* The verfication of ":metric" is removed (see lines 797-798 in [src/translate/pddl_parser/parsing_functions.py](src/translate/pddl_parser/parsing_functions.py)), since the addition of ":preferences" broke this test.

## Licence

Because Fast Downward is distributed under the GNU General Public License as published by the Free Software Foundation, this project falls under the same licence.