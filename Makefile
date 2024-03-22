.PHONY: switch

switch:
	darwin-rebuild switch --option extra-builtins-file $$PWD/extra-builtins.nix --flake ./
