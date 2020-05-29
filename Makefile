build:
	nix-build release.nix

run: build
	result/bin/topoi
