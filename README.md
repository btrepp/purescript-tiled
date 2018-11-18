# purescript-tiled

A work in progress libarary for parsing tiled [https://www.mapeditor.org/]
maps into purescript records.

To begin with this will focus on parsing the json format only.

# Data.Tiled.File.*

The file module contains types and functions for dealing with
the data structures as defined in the tiled documents.
These are Map and Tileset

It attempts to keep the intent of various combinations in the file
and hopefully will eventually be two-way

The aim is provide primitives that can be used by higher levels.

# Compressed files

Zlib is supported, but is called via pako.
Pako must be available for compression to work.
Other wise the file will throw an error

#Testing

The default desert map (available in the tiled example directory)
Is included in json format and tested against.

If you have a different file that doesn't work/properties not in the example
it can be included in the tests.

Hopefully the tests are fairly straight forward so we can add plenty
more to cover all valid combinations of files

# NOTES

This is currently a massive work in progress. 
The api shapes may change.

