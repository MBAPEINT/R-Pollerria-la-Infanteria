#!/bin/bash
# ============================================================
# Entrypoint — Espera a que MongoDB esté listo, luego
# ejecuta el pipeline inicial (si hay Excel) y arranca Flask
# ============================================================
set -e

echo "============================================"
echo "  Pollería la Infantería — Iniciando..."
echo "============================================"

# Esperar a que MongoDB esté disponible
echo "⏳ Esperando a MongoDB (mongodb:27017)..."

# Usar un script Python separado para evitar problemas de escaping
python3 -c '
from pymongo import MongoClient
import os, sys, time

url = os.environ.get("MONGO_URL", "mongodb://mongodb:27017")
max_retries = 30

for i in range(max_retries):
    try:
        client = MongoClient(url, serverSelectionTimeoutMS=3000)
        client.admin.command("ping")
        print("✅ MongoDB conectado")
        sys.exit(0)
    except Exception as e:
        if i < max_retries - 1:
            time.sleep(2)
        else:
            print(f"❌ No se pudo conectar a MongoDB tras {max_retries} intentos: {e}")
            sys.exit(1)
'

# Si existe el Excel en data/raw, ejecutar pipeline inicial
EXCEL="/app/data/raw/data_2023.xlsx"
if [ -f "$EXCEL" ]; then
    echo ""
    echo "📊 Excel detectado: data_2023.xlsx"
    echo "⏳ Ejecutando pipeline inicial (poblar MongoDB)..."
    Rscript /app/main.R
    echo "✅ Pipeline inicial completado"
else
    echo ""
    echo "⚠️  No se encontró data_2023.xlsx en data/raw/"
    echo "   Súbalo desde la webapp en 'Actualizar Datos'"
fi

echo ""
echo "🌐 Iniciando webapp en http://localhost:5000"
echo "============================================"
echo ""

# Iniciar Flask
cd /app/webapp
exec python3 app.py
