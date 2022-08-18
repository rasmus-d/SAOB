# SAOB
Program which i created to translate Swedish Academy Dictionary abbreviations, but the source code will work with any table and input.

Basically just a program which iterates through text and replaces substrings found to be keys in a given association list with the corresponding value. 

Main.hs reads a csv-file seperated by tabs as an association list, the Parser.hs function parse/3 will replace any keys found in a given text with the value in the association list, provided the key is surrounded by seperators. The seperators are defined in Parser.hs.


