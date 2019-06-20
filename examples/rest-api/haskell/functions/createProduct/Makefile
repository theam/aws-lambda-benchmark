all:
	@mkdir -p build
	@rm -rf ./build/*
	@stack clean --docker
	@stack build --docker
	@cp `stack --docker path --local-install-root`/bin/{bootstrap,haskell_lambda} build
	@cd build && zip createProduct.zip haskell_lambda bootstrap && rm haskell_lambda bootstrap && cd ..