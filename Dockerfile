FROM ubuntu:24.04

RUN apt-get update && \
   apt-get install -y fpc build-essential && \
   rm -rf /var/lib/apt/lists/*

# Set working directory inside the container
WORKDIR /src

CMD ["bash"]
