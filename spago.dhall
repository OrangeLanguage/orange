{ name = "my-project"
, dependencies =
  [ "bigints"
  , "console"
  , "effect"
  , "ordered-collections"
  , "parsing"
  , "psci-support"
  , "node-process"
  , "node-readline"
  , "node-fs"
  , "node-streams"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
