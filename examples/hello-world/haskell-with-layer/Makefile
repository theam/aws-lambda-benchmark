all:
	@mkdir -p build
	@rm -rf ./build/*
	@stack clean --docker
	@stack build --docker
	@cp `stack --docker path --local-install-root`/bin/haskell_lambda build
	@cd build && zip function.zip haskell_lambda && rm haskell_lambda && cd ..