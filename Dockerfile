FROM ubuntu:16.04
RUN mkdir -p /opt/ninety-nine-test
WORKDIR /opt/ninety-nine-test
COPY .stack-work/install/ffe95053/bin/ninety-nine-tdd-exe.exe /opt/ninety-nine-test
