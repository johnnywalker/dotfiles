.PHONY: switch

# https://gist.github.com/sighingnow/deee806603ec9274fd47
OS_NAME := $(shell uname -s | tr A-Z a-z)

switch:
ifeq ($(OS_NAME), linux)
	nixos-rebuild switch --flake ./
endif
ifeq ($(OS_NAME), darwin)
	darwin-rebuild switch --option extra-builtins-file $$PWD/extra-builtins.nix --flake ./
endif
