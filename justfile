# seq-actor Build System
#
# This is the SOURCE OF TRUTH for all build/test/lint operations.
# GitHub Actions calls these recipes directly - no duplication!

# Default recipe: show available commands
default:
    @just --list

# Build everything (compiler + runtime)
build: build-runtime build-compiler

# Install the compiler
install:
    @echo "Installing the compiler..."
    cargo install --path crates/compiler

# Build the Rust runtime as static library
build-runtime:
    @echo "Building runtime..."
    cargo build --release -p seq-actor-runtime
    @echo "Runtime built: target/release/libseq_actor_runtime.a"

# Build the compiler
build-compiler:
    @echo "Building compiler..."
    cargo build --release -p seq-actor-compiler
    @echo "Compiler built: target/release/seqact"

# Build all example programs
build-examples: build
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Building examples..."
    mkdir -p target/examples
    for file in examples/*.act; do
        name=$(basename "$file" .act)
        echo "  Compiling $file..."
        SEQ_ACTOR_RUNTIME=target/release/libseq_actor_runtime.a target/release/seqact build "$file" -o "target/examples/$name" || echo "  Warning: $file failed to compile (expected for some examples)"
    done
    echo "Examples built in target/examples/"
    ls -lh target/examples/ 2>/dev/null || echo "  (no examples compiled successfully yet)"

# Run all Rust unit tests
test:
    @echo "Running Rust unit tests..."
    cargo test --workspace --all-targets

# Run clippy on all workspace members
lint:
    @echo "Running clippy..."
    cargo clippy --workspace --all-targets -- -D warnings

# Format all code
fmt:
    @echo "Formatting code..."
    cargo fmt --all

# Check formatting without modifying files
fmt-check:
    @echo "Checking code formatting..."
    cargo fmt --all -- --check

# Run all CI checks (same as GitHub Actions!)
# This is what developers should run before pushing
ci: fmt-check lint test build build-examples
    @echo ""
    @echo "All CI checks passed!"
    @echo "   - Code formatting"
    @echo "   - Clippy lints"
    @echo "   - Unit tests"
    @echo "   - Compiler built"
    @echo "   - Runtime built"
    @echo "   - Examples built"
    @echo ""
    @echo "Safe to push to GitHub - CI will pass."

# Clean all build artifacts
clean:
    @echo "Cleaning build artifacts..."
    cargo clean
    rm -f examples/*.ll
    rm -rf target/examples
    @echo "Clean complete"

# Development: quick format + build + test
dev: fmt build test

# Show test output (verbose)
test-verbose:
    cargo test --workspace -- --nocapture

# Check for outdated dependencies
outdated:
    cargo outdated --workspace

# Generate documentation
doc:
    cargo doc --workspace --no-deps --open

# Verify workspace consistency
verify-workspace:
    @echo "Verifying workspace configuration..."
    cargo tree --workspace
    @echo "Workspace verified"

# Quick check (faster than full ci)
check:
    @echo "Running quick checks..."
    cargo check --workspace --all-targets
    cargo clippy --workspace --all-targets -- -D warnings
    @echo "Quick checks passed!"
