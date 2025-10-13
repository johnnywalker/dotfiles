.PHONY: boot switch

# https://gist.github.com/sighingnow/deee806603ec9274fd47
OS_NAME := $(shell uname -s | tr A-Z a-z)

boot: rcup
ifeq ($(OS_NAME), linux)
	nixos-rebuild boot --flake ./
endif
ifeq ($(OS_NAME), darwin)
	darwin-rebuild boot --option extra-builtins-file $$PWD/extra-builtins.nix --flake ./
endif

switch: rcup
ifeq ($(OS_NAME), linux)
	# use impure for builtins.fetchGit with working directory
	nixos-rebuild switch --impure --keep-failed --flake ./
endif
ifeq ($(OS_NAME), darwin)
	darwin-rebuild switch --option extra-builtins-file $$PWD/extra-builtins.nix --flake ./
endif

rcup:
	nix shell nixpkgs#rcm --command rcup -- -d public -S emacs.d emacs.d

boot-petey:
	nixos-rebuild boot --flake .#petey --target-host petey --use-remote-sudo --show-trace

switch-petey:
	nixos-rebuild switch --flake .#petey --target-host petey --use-remote-sudo --show-trace
