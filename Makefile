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

@PHONY: lambda
lambda:
	mkdir -p build
	@rm -rf ./build/*
	docker build -t todo-lambda .
	id=$$(docker create todo-lambda); docker cp $$id:/root/output ./build; docker rm -v $$id
	cd build/output; zip -r todo-function.zip *

@PHONY: lambda-test
lambda-test:
	docker run --rm \
		-v `pwd`/build/output:/var/task:ro,delegated \
		lambci/lambda:provided.al2 \
		todos

