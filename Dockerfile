# Multi-stage build for WoWChat
# Stage 1: Build the application
FROM maven:3.8-openjdk-11 AS builder

# Set working directory
WORKDIR /build

# Copy pom.xml and download dependencies (for better caching)
COPY pom.xml .
RUN mvn dependency:go-offline -B

# Copy source code
COPY src ./src

# Build the application
RUN mvn clean package -DskipTests

# Stage 2: Runtime image
FROM eclipse-temurin:11-jre

# Set working directory
WORKDIR /app

# Copy the built JAR and resources from builder stage
COPY --from=builder /build/target/wowchat.jar ./wowchat.jar
COPY --from=builder /build/target/classes/*.csv ./
COPY --from=builder /build/target/classes/logback.xml ./

# Copy configuration file (can be overridden with volume mount in Coolify)
COPY src/main/resources/wowchat.conf ./wowchat.conf

# Environment variables for configuration
# These should be set in Coolify's environment variables section

# Discord Configuration
ENV DISCORD_TOKEN=""
ENV GUILD_CHAT_CHANNEL_ID=""
ENV OFFICER_CHAT_CHANNEL_ID=""
ENV GUILD_ROLE_SYNC_ENABLED=""
ENV GUILD_ROLE_ID=""

ENV WOW_PLATFORM=""
ENV ITEM_DATABASE=""
# WoW Server Configuration
ENV WOW_VERSION=""
ENV WOW_REALMLIST=""
ENV WOW_REALM=""

# WoW Account Configuration
ENV WOW_ACCOUNT=""
ENV WOW_PASSWORD=""
ENV WOW_CHARACTER=""

# Optional: Add a healthcheck (adjust if your app exposes any endpoint)
# Since this is a chat bot without HTTP endpoints, we'll check if the process is running
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
  CMD ps aux | grep -v grep | grep wowchat.jar || exit 1

# Create a non-root user for security
RUN groupadd -r wowchat && useradd -r -g wowchat wowchat && \
    chown -R wowchat:wowchat /app
USER wowchat

# Run the application
CMD ["java", "-Dlogback.configurationFile=logback.xml", "-jar", "wowchat.jar", "wowchat.conf"]

