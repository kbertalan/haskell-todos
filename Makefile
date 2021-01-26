@PHONY: ctest
ctest:
	ghcid \
		--command "stack ghci haskell-todos:lib haskell-todos:test:haskell-todos-test" \
		--test "main"

@PHONY: devel
devel:
	ghcid \
		--command "stack ghci haskell-todos:lib haskell-todos:exe:haskell-todos-exe --main-is haskell-todos:test:haskell-todos-test" \
		--test "main" \
		--test "DevelMain.update" \
		--clear \
		--lint

