# ============================================================
# Pollería la Infantería — Imagen Docker
# Python 3.13 + R 4.x + Flask webapp + modelos ML
# ============================================================
FROM python:3.13-slim

# Evitar prompts interactivos durante instalación
ENV DEBIAN_FRONTEND=noninteractive
ENV MONGO_URL=mongodb://mongodb:27017

# ─── Dependencias del sistema ───
RUN apt-get update && apt-get install -y --no-install-recommends \
    # R base
    r-base r-base-dev \
    # Paquetes R que necesitan compilación
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libsodium-dev libsasl2-dev \
    # Utilidades
    curl procps \
    # Limpiar caché
    && rm -rf /var/lib/apt/lists/*

# ─── Paquetes de Python ───
RUN pip install --no-cache-dir flask pymongo

# ─── Instalar paquetes de R en una sola capa ───
# (ordenados: primero los que tienen más dependencias)
RUN R -e "install.packages(c( \
    'Rcpp', 'rlang', 'curl', 'openssl', 'jsonlite', \
    'dplyr', 'readxl', 'ggplot2', 'lubridate', \
    'mongolite', 'randomForest', 'arules', 'gridExtra' \
  ), repos='https://cloud.r-project.org', Ncpus=2)" \
  && R -e "install.packages('prophet', repos='https://cloud.r-project.org', Ncpus=2)"

# ─── Copiar el proyecto ───
WORKDIR /app
COPY . .

# ─── Crear directorios necesarios ───
RUN mkdir -p /app/data/raw /app/data/output /app/data/processed

# ─── Entrypoint ───
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

EXPOSE 5000
CMD ["/entrypoint.sh"]
