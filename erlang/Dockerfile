# Build stage 0
FROM erlang:latest AS build
# Set the working directory inside the container
# WORKDIR /app

# Copy only the necessary files for building the application
# This ensures we use Docker's build cache effectively
COPY rebar.config rebar.lock ./
COPY src/ ./src/
COPY test/ ./test/
COPY relx.config ./

# Install rebar3 for building the application
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/


RUN rebar3 get-deps
RUN rebar3 compile

# Build the release
# FROM erlang:latest

# # Set the working directory for the application runtime
# # WORKDIR /app

# # Copy the compiled application from the build stage
# COPY --from=build /_build/default/lib/erlang /_build/default/lib/erlang
# COPY relx.config ./

# # Expose the application's HTTP port
# EXPOSE 8080

# Command to start the application
# CMD ["erl", "-pa", "_build/default/lib/*/ebin", "-s", "erlang"]
CMD ["rebar3", "shell"]