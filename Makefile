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
	@find app src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code styler (stylish-haskell)
	@fourmolu -q --mode inplace app src
	@cabal-fmt -i *.cabal

help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
