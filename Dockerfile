# =============================================================================
# Stage 1: Build Haskell backend
# =============================================================================
FROM haskell:9.10.1 AS backend-builder

RUN apt-get update && apt-get install -y --no-install-recommends \
      libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build/backend

COPY backend/eva.cabal backend/cabal.project ./

# Download and cache the package index and all dependencies before copying
# source — this layer is reused as long as eva.cabal and cabal.project are
# unchanged.
RUN cabal update && cabal build --only-dependencies -j$(nproc) all

COPY backend/ .

# Enable optimizations for the release binary (cabal.project sets
# optimization: False for fast incremental dev builds).
RUN echo 'optimization: True' > cabal.project.local && \
    cabal install --installdir=/usr/local/bin exe:eva \
      --overwrite-policy=always -j$(nproc) && \
    strip /usr/local/bin/eva

# =============================================================================
# Stage 2: Build React frontend
# =============================================================================
FROM node:22-slim AS frontend-builder

WORKDIR /build/frontend

COPY frontend/package.json frontend/package-lock.json ./
RUN npm ci

COPY frontend/ .
RUN npm run build

# =============================================================================
# Stage 3: Runtime image
# =============================================================================
FROM debian:bookworm-slim AS runtime

RUN apt-get update && apt-get install -y --no-install-recommends \
      libgmp10 \
      libsqlite3-0 \
      ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=backend-builder /usr/local/bin/eva ./eva
COPY --from=frontend-builder /build/frontend/dist ./frontend-dist

VOLUME /data

EXPOSE 8080

ENV EVA_DB_PATH=/data/eva.db
ENV EVA_STATIC_DIR=/app/frontend-dist
ENV EVA_LOG_LEVEL=info

CMD ["/app/eva"]
