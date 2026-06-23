FROM registry.redhat.io/ubi10/ubi:latest as builder

RUN dnf install -y dotnet-sdk-9.0 && \
    dnf clean all && \
    rm -rf /var/cache/dnf

WORKDIR /build
COPY src/CSharpProvider.csproj .
RUN dotnet restore

COPY src/ .
RUN dotnet publish -c Release -o /app

FROM registry.redhat.io/ubi10/ubi:latest

# Install .NET SDK (required at runtime for dotnet restore on analyzed projects)
RUN dnf install -y dotnet-sdk-9.0 && \
    dnf clean all && \
    rm -rf /var/cache/dnf

# Create directories with proper permissions for OpenShift compatibility
# Group 0 (root group) needs rwx for OpenShift arbitrary UIDs
RUN mkdir -p /analyzer-lsp /projects && \
    chgrp -R 0 /home /analyzer-lsp /projects && \
    chmod -R g=u /home /analyzer-lsp /projects

USER 1001

ENV HOME=/home
ENV DOTNET_ROOT=/usr/lib64/dotnet
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1
ENV DOTNET_RUNNING_IN_CONTAINER=true

WORKDIR /analyzer-lsp

# Copy published application
COPY --from=builder /app /usr/local/lib/csharp-provider
COPY LICENSE /licenses/

ENTRYPOINT ["dotnet", "/usr/local/lib/csharp-provider/CSharpProvider.dll"]
CMD ["--name", "c-sharp", "--port", "14651"]

LABEL \
        description="Migration Toolkit for Applications - Dotnet External Provider" \
        io.k8s.description="Migration Toolkit for Applications - Dotnet External Provider" \
        io.k8s.display-name="MTA - Dotnet External Provider" \
        io.openshift.maintainer.project="MTA" \
        io.openshift.tags="migration,modernization,mta,tackle,konveyor" \
        summary="Migration Toolkit for Applications - Dotnet External Provider"
