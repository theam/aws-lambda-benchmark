all:
	@mkdir -p build
	@rm -rf ./build/*
	@stack clean --docker
	@stack build --docker
	@cp `stack --docker path --local-install-root`/bin/bootstrap build
	@cd build && zip getProduct.zip bootstrap && rm bootstrap && cd ..