# Multi-stage build for Erlang/OTP application using erlang.mk
FROM erlang:28-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    git \
    make \
    gcc \
    musl-dev \
    wget \
    curl

WORKDIR /app

# Copy everything needed for build
COPY . ./

# Build the application and release
RUN make deps app rel


# Runtime stage
FROM erlang:28-alpine AS runtime

# Install runtime dependencies
RUN apk add --no-cache \
    bash \
    openssl \
    ncurses-libs \
    libstdc++

# Create erlskat user
RUN addgroup -g 1000 erlskat && \
    adduser -D -s /bin/sh -u 1000 -G erlskat erlskat

WORKDIR /opt/erlskat

# Copy the release from builder stage
COPY --from=builder /app/_rel/erlskat/ .

# Fix ownership and permissions
RUN chown -R erlskat:erlskat /opt/erlskat && \
    chmod +x bin/erlskat

# Switch to non-root user
USER erlskat

# Expose the websocket port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=10s --retries=3 \
    CMD ./bin/erlskat ping || exit 1

# Run the application in foreground mode
CMD ["./bin/erlskat", "foreground"]
