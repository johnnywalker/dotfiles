.PHONY: boot switch

# https://gist.github.com/sighingnow/deee806603ec9274fd47
OS_NAME := $(shell uname -s | tr A-Z a-z)

boot:
ifeq ($(OS_NAME), linux)
	nixos-rebuild boot --flake ./
endif
ifeq ($(OS_NAME), darwin)
	darwin-rebuild boot --option extra-builtins-file $$PWD/extra-builtins.nix --flake ./
endif

switch:
ifeq ($(OS_NAME), linux)
	nixos-rebuild switch --flake ./
endif
ifeq ($(OS_NAME), darwin)
	darwin-rebuild switch --option extra-builtins-file $$PWD/extra-builtins.nix --flake ./
endif
