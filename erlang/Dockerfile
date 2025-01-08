# Use the official Erlang image as the base image
FROM erlang:latest

# Install rebar3
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Copy the current directory contents into the container at /app
COPY . .

# Compile the project
RUN rebar3 get-deps
RUN rebar3 compile

# Expose port 8080
EXPOSE 8080

# Command to run the application
CMD ["rebar3", "shell"]