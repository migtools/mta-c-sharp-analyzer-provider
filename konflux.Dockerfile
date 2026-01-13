FROM registry.redhat.io/ubi9/ubi:latest as builder
COPY --chown=1001:0 . /workspace
RUN dnf install -y rust-toolset unzip

WORKDIR /workspace

RUN --mount=type=cache,id=cargohome,uid=1001,gid=0,mode=0777,target=/root/.cargo cargo build --release

FROM registry.redhat.io/ubi9/ubi:latest

RUN dnf install -y dotnet-sdk-8.0 dotnet-runtime-8.0 tar gzip findutils && \
    dnf clean all && \
    rm -rf /var/cache/dnf
RUN dotnet tool install --tool-path=/usr/local/bin Paket
RUN dotnet tool install --tool-path=/usr/local/bin ilspycmd
RUN chgrp -R 0 /home && chmod -R g=u /home
USER 1001

ENV HOME=/home
ENV RUST_LOG=INFO,c_sharp_analyzer_provider_cli=DEBUG,

COPY --from=builder /workspace/target/release/c-sharp-analyzer-provider-cli /usr/local/bin/c-sharp-provider
COPY --from=builder --chmod=0755 /workspace/scripts/dotnet-install.sh /usr/local/bin/scripts/dotnet-install.sh
COPY --from=builder --chmod=0755 /workspace/scripts/dotnet-install.ps1 /usr/local/bin/scripts/dotnet-install.ps1
COPY --from=builder /workspace/LICENSE /licenses/

WORKDIR /analyzer-lsp
RUN chgrp -R 0 /analyzer-lsp && chmod -R g=u /analyzer-lsp

ENTRYPOINT ["/usr/local/bin/c-sharp-provider"]
CMD ["--name", "c-sharp", "--port", "14651"]

LABEL \
        description="Migration Toolkit for Applications - Dotnet External Provider" \
        io.k8s.description="Migration Toolkit for Applications - Dotnet External Provider" \
        io.k8s.display-name="MTA - Dotnet External Provider" \
        io.openshift.maintainer.project="MTA" \
        io.openshift.tags="migration,modernization,mta,tackle,konveyor" \
        summary="Migration Toolkit for Applications - Dotnet External Provider"
