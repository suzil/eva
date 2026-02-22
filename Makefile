.PHONY: dev build test install install-ghcid

# Start backend (ghcid hot-reload) and frontend (Vite) concurrently.
# Ctrl+C kills both.
dev:
	@trap 'kill 0' INT; \
	(cd backend && ghcid) & \
	(cd frontend && npm run dev) & \
	wait

install-ghcid:
	cabal install ghcid --overwrite-policy=always

install:
	cd frontend && npm install

# Build backend and frontend.
build:
	cd backend && cabal build all
	cd frontend && npm run build

# Run backend (cabal) and frontend (Vitest) test suites.
test:
	cd backend && cabal test all --test-show-details=direct
	cd frontend && npm test

# Seed the Weekly Project Summarizer demo program (backend must be running).
# Usage: make seed  OR  make seed BASE_URL=http://localhost:8080
seed:
	@./scripts/seed-demo.sh $(BASE_URL)
