{-

Welcome to a Spago project!

You can edit this file as you like.

-}

{ name =
    "my-project"
, dependencies =
    [ "effect"
    , "console"
    , "argonaut"
    , "maybe"
    , "profunctor-lenses"
    , "test-unit"
    , "node-fs"
    , "base64-codec"
    , "arraybuffer"
    , "pako"
    , "simple-json"
    , "filterable" ]
, packages =
    ./packages.dhall
}
