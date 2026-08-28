run: ## Start the application
	@cabal run

deps: ## Install the dependencies
	@cabal build --only-dependencies

build: ## Build the project
	@cabal build

watch: ## Automatically rebuild and start the application on code change
	@ghcid -c "cabal repl lib:ghcup-gtk" -T UI.main

clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl

test: ## Run the test suite
	@cabal test

lint: ## Run the code linter (HLint)
	@find app src test -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code styler (stylish-haskell)
	@cabal-gild *.cabal
	@fourmolu -q --mode inplace app src test

dist: build ## Assemble release tarball
	@rm -rf dist-tarball && mkdir -p dist-tarball/ghcup-gtk
	@cp $$(cabal list-bin ghcup-gtk) dist-tarball/ghcup-gtk/ghcup-gtk
	@cp data/org.haskell.GhcupGtk.desktop data/org.haskell.GhcupGtk.svg dist-tarball/ghcup-gtk/
	@tar czf ghcup-gtk-$$(git describe --tags --always).tar.gz -C dist-tarball ghcup-gtk
	@echo "Wrote ghcup-gtk-$$(git describe --tags --always).tar.gz"

package: ## Build native packages with fpm (deb/rpm/pacman on Linux, osxpkg on macOS)
	@./scripts/package.sh

help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
