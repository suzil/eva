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

# Reset the database and seed the demo program.
# Kills any server on EVA_PORT, wipes EVA_DB_PATH, starts a fresh backend,
# waits for it to be healthy, then runs the seed script.
seed:
	@set -a; [ -f .env ] && . ./.env || true; set +a; \
	PORT=$${EVA_PORT:-8080}; DB=$${EVA_DB_PATH:-./eva.db}; \
	echo "--- Resetting $$DB ---"; \
	lsof -ti :$$PORT 2>/dev/null | xargs kill -9 2>/dev/null || true; \
	sleep 0.3; \
	rm -f "$$DB"; \
	(cd backend && cabal run exe:eva >>/tmp/eva-backend.log 2>&1) & \
	printf "Waiting for backend on :$$PORT"; \
	for i in $$(seq 1 60); do \
		sleep 1; printf "."; \
		curl -sf "http://localhost:$$PORT/api/health" >/dev/null 2>&1 && break; \
		if [ "$$i" -eq 60 ]; then printf "\nBackend did not start.\n"; exit 1; fi; \
	done; \
	printf "\n"; \
	./scripts/seed-demo.sh "http://localhost:$$PORT"; \
	echo ""; \
	echo "Backend running in background (PID $$(lsof -ti :$$PORT 2>/dev/null))."
	@echo "Restart 'make dev' to reattach ghcid."

# Build the Docker image (multi-stage: GHC + Node + runtime).
docker-build:
	docker build -t eva .

# Start Eva via Docker Compose (requires EVA_CREDENTIAL_KEY to be set).
# Example: EVA_CREDENTIAL_KEY=$(openssl rand -hex 32) make docker-run
docker-run:
	docker compose up
