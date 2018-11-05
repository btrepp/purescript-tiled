#purescript-tiled

A work in progress libarary for parsing tiled [https://www.mapeditor.org/]
maps into purescript records.

To begin with this will focus on parsing the json format only.

# Raw

The raw module contains types and functions for dealing with
the data structures as defined in the tiled documents.
As these are serialized into json they can represent invalid states.

The aim is provide primitives that can be used by higher levels.

#NOTES

This is currently a massive work in progress. Nothing actually
works yet