# Build stage
FROM haskell:9.6.6 as builder

WORKDIR /app

# Install system dependencies
RUN apt-get update && \
    apt-get install -y libgmp-dev && \
    rm -rf /var/lib/apt/lists/*

# Copy only the files needed to download dependencies
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack setup
RUN stack build --only-dependencies

# Copy the rest of the project files
COPY . .

# Build the project
RUN stack build --copy-bins

# Runtime stage
FROM debian:bullseye-slim

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && \
    apt-get install -y libgmp10 && \
    rm -rf /var/lib/apt/lists/*

# Copy the binary from the builder stage
COPY --from=builder /root/.local/bin/glados-exe .

ENTRYPOINT ["./glados-exe"]
