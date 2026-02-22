.PHONY: dev build test install install-ghcid docker-build docker-run

# Start backend (ghcid hot-reload) and frontend (Vite) concurrently.
# Ctrl+C kills both.
# Sources .env automatically if present (sets EVA_* vars for the backend).
dev:
	@set -a; [ -f .env ] && . ./.env || true; set +a; \
	lsof -ti :$${EVA_PORT:-8080} | xargs kill -9 2>/dev/null || true; \
	trap 'kill 0' INT; \
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

# Build the Docker image (multi-stage: GHC + Node + runtime).
docker-build:
	docker build -t eva .

# Start Eva via Docker Compose (requires EVA_CREDENTIAL_KEY to be set).
# Example: EVA_CREDENTIAL_KEY=$(openssl rand -hex 32) make docker-run
docker-run:
	docker compose up
