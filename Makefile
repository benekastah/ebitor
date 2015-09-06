
# Bring this back if we need to use yi-rope
# build:
# 	stack build \
# 		--extra-include-dirs=/usr/local/opt/icu4c/include \
# 		--extra-lib-dirs=/usr/local/opt/icu4c/lib

build:
	stack build

eb: build
	stack exec eb

eb-server: build
	stack exec eb-server
