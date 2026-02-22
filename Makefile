.PHONY: dev build test install-ghcid

# Hot-reload REPL for backend development.
# Requires ghcid: run `make install-ghcid` once to install it.
dev:
	cd backend && ghcid

install-ghcid:
	cabal install ghcid --overwrite-policy=always

# Build the backend (and frontend when it exists).
build:
	cd backend && cabal build all

# Run backend tests.
test:
	cd backend && cabal test all --test-show-details=direct
