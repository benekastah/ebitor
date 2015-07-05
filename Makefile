
# Bring this back if we need to use yi-rope
# build:
# 	stack build \
# 		--extra-include-dirs=/usr/local/opt/icu4c/include \
# 		--extra-lib-dirs=/usr/local/opt/icu4c/lib

build:
	stack build

run: build
	stack exec eb
