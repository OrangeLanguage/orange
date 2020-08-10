{ name = "my-project"
, dependencies = [ "bigints", "console", "effect", "parsing", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
