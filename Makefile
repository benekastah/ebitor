
.PHONY: build eb eb-profile eb-server eb-server-profile test

# build:
# 	stack build --executable-profiling
build:
	stack build

eb: build
	stack exec eb

eb-profile: build
	stack exec eb-profile

eb-server: build
	stack exec eb-server

eb-server-profile: build
	stack exec eb-server-profile
