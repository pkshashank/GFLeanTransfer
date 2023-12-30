This is a basic program that converts a natural language text to Lean 4 terms.
We use Grammatical Framework (GF) (https://www.grammaticalframework.org/) for parsing and linearisation.
The conversion is done via Haskell.
The natural language it accepts is from a controlled natural language called SimplifiedForTheL which is a simplified version of the ForTheL language (http://nevidal.org/download/forthel.pdf).
The input is given as example blocks written in SimplifiedForTheL.
This is still a work in progress, and thus the program cannot handle all example blocks written in SimplifiedForTheL.

To run the program, use stack run. The input should be put in the prompt.txt file.

