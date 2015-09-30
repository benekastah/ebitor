
# Bring this back if we need to use yi-rope
# build:
# 	stack build \
# 		--extra-include-dirs=/usr/local/opt/icu4c/include \
# 		--extra-lib-dirs=/usr/local/opt/icu4c/lib

build:
	stack build

eb: build
	stack exec eb

build-profile:
	stack build --executable-profiling

eb-profile: build-profile
	stack exec eb-profile

eb-server: build
	stack exec eb-server
