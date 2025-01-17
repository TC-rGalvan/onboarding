
# Makefile for Erlang Application with Docker and Rebar3

# Variables
DOCKER_COMPOSE = docker-compose
DOCKER_FILE = Dockerfile
DOCKER_IMAGE_NAME = erlang-app
DOCKER_CONTAINER_NAME = erlang-container
BUILD_ARG_ENV = development
PORT = 8080
REDIS_PORT = 6379

# Targets

# Build the Docker image
build:
	@echo "Building Docker image..."
	@docker build --build-arg ENV=$(BUILD_ARG_ENV) -t $(DOCKER_IMAGE_NAME) -f $(DOCKER_FILE) .

# Start Docker services (app and redis) using docker-compose
up:
	@echo "Starting Docker services..."
	@$(DOCKER_COMPOSE) up --build

# Start only the application container (without Redis)
up-app:
	@echo "Starting only the app container..."
	@$(DOCKER_COMPOSE) up --build --no-deps app

# Stop Docker services
down:
	@echo "Stopping Docker services..."
	@$(DOCKER_COMPOSE) down

# Run tests using rebar3 (assuming test folder exists)
test:
	@echo "Running tests..."
	@docker run --rm $(DOCKER_IMAGE_NAME) rebar3 eunit

# Open a shell in the running Docker container
shell:
	@echo "Starting a shell in the container..."
	@docker run -it --rm $(DOCKER_IMAGE_NAME) rebar3 shell

# Clean up Docker images, containers, and volumes
clean:
	@echo "Cleaning up Docker resources..."
	@docker-compose down --volumes --remove-orphans

# Run Redis container only (for development purposes)
up-redis:
	@echo "Starting Redis container..."
	@$(DOCKER_COMPOSE) up redis

.PHONY: build up up-app down test shell clean up-redis
