#!/bin/bash
# ============================================================
# Entrypoint para Render.com
# MongoDB ya está en Atlas → no corremos ETL, no esperamos
# a un contenedor local. Solo iniciamos Flask.
# ============================================================
set -e

echo "============================================"
echo "  Pollería la Infantería — Render"
echo "  MongoDB: ${MONGO_URL}"
echo "============================================"

# Verificar conexión a MongoDB (Atlas)
echo "⏳ Verificando conexión a MongoDB Atlas..."
python3 -c '
from pymongo import MongoClient
import os, sys, time

url = os.environ.get("MONGO_URL", "mongodb://localhost:27017")
max_retries = 10

for i in range(max_retries):
    try:
        client = MongoClient(url, serverSelectionTimeoutMS=10000)
        client.admin.command("ping")
        print("✅ MongoDB Atlas conectado")
        # Verificar que la BD tenga datos
        db = client["Polleria"]
        count = db["Ventas"].count_documents({})
        print(f"   Colección Ventas: {count} documentos")
        sys.exit(0)
    except Exception as e:
        if i < max_retries - 1:
            print(f"   Intento {i+1}/{max_retries} fallido, reintentando...")
            time.sleep(3)
        else:
            print(f"⚠️  No se pudo conectar a MongoDB: {e}")
            print("   La app iniciará pero puede fallar sin BD.")
            sys.exit(0)
'

echo ""
echo "🌐 Iniciando webapp en puerto 5000"
echo "============================================"
echo ""

cd /app/webapp
exec python3 app.py
