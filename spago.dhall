{ name = "my-project"
, dependencies =
  [ "bigints"
  , "console"
  , "effect"
  , "ordered-collections"
  , "parsing"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
