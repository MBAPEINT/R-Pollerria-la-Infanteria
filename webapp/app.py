"""
Pollería la Infantería — Dashboard Interactivo
Flask + MongoDB + Chart.js
Incluye: upload de Excel + pipeline con barra de progreso
"""
import json
import csv
import os
import re
import subprocess
import threading
from datetime import datetime, timedelta
from collections import defaultdict

from flask import Flask, render_template, jsonify, request
from pymongo import MongoClient
from bson.objectid import ObjectId

app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = 50 * 1024 * 1024  # 50 MB máximo

# Conexión MongoDB (usa variable de entorno si está en Docker)
MONGO_URL = os.environ.get("MONGO_URL", "mongodb://localhost:27017")
client = MongoClient(MONGO_URL)
db = client["Polleria"]

BASE = os.path.dirname(os.path.abspath(__file__))
PROJECT = os.path.dirname(BASE)

# Ruta a Rscript (variable de entorno en Docker, detección automática en Windows)
RSCRIPT = os.environ.get("R_SCRIPT_PATH", "")
if not RSCRIPT or not os.path.exists(RSCRIPT):
    # Intentar detectar en Windows
    for candidate in [
        r"C:\Program Files\R\R-4.6.1\bin\Rscript.exe",
        r"C:\Program Files\R\R-4.4.1\bin\Rscript.exe",
        r"C:\Program Files\R\R-4.3.1\bin\Rscript.exe",
        "/usr/bin/Rscript",  # Linux / Docker
    ]:
        if os.path.exists(candidate):
            RSCRIPT = candidate
            break

print(f"MongoDB: {MONGO_URL}")
print(f"Rscript: {RSCRIPT}")

# Estado global del pipeline (compartido entre threads)
pipeline_status = {
    "running": False,
    "step": "",
    "percent": 0,
    "error": None
}
pipeline_lock = threading.Lock()


# ═══════════════════════════════════════════════════════════
# HELPERS
# ═══════════════════════════════════════════════════════════

def read_csv(filename):
    """Leer CSV de predicciones/proyectos"""
    paths = [
        os.path.join(PROJECT, "data", "output", filename),
    ]
    for p in paths:
        if os.path.exists(p):
            with open(p, encoding="utf-8") as f:
                return list(csv.DictReader(f))
    return []


def mongo_find(collection, query=None, fields=None):
    """Wrapper para consultas MongoDB"""
    if query is None:
        query = {}
    if fields is None:
        fields = {"_id": 0}
    return list(db[collection].find(query, fields))


# ═══════════════════════════════════════════════════════════
# RUTAS
# ═══════════════════════════════════════════════════════════

@app.route("/")
def index():
    return render_template("index.html")


# --- API: KPIs en tiempo real ---
@app.route("/api/kpis")
def api_kpis():
    ventas = list(db["Ventas"].find({}, {"FechaVenta": 1, "Total": 1, "MetodoPago": 1, "Cliente": 1, "_id": 0}))

    # Parsear fechas
    for v in ventas:
        if isinstance(v["FechaVenta"], str):
            v["FechaVenta"] = datetime.fromisoformat(v["FechaVenta"].replace("Z", ""))
        v["fecha_str"] = v["FechaVenta"].strftime("%Y-%m-%d")

    hoy = max(v["fecha_str"] for v in ventas)
    ayer = (datetime.strptime(hoy, "%Y-%m-%d") - timedelta(days=1)).strftime("%Y-%m-%d")
    inicio_semana = (datetime.strptime(hoy, "%Y-%m-%d") - timedelta(days=6)).strftime("%Y-%m-%d")
    inicio_mes = hoy[:8] + "01"
    inicio_30d = (datetime.strptime(hoy, "%Y-%m-%d") - timedelta(days=30)).strftime("%Y-%m-%d")

    hoy_data = [v for v in ventas if v["fecha_str"] == hoy]
    ayer_data = [v for v in ventas if v["fecha_str"] == ayer]
    semana_data = [v for v in ventas if v["fecha_str"] >= inicio_semana]

    total_hoy = sum(v["Total"] for v in hoy_data)
    total_ayer = sum(v["Total"] for v in ayer_data)
    n_hoy = len(hoy_data)

    var = ((total_hoy - total_ayer) / total_ayer * 100) if total_ayer > 0 else 0

    # Ticket promedio semana
    tickets = [v["Total"] for v in semana_data]
    ticket_semana = round(sum(tickets) / len(tickets), 2) if tickets else 0

    # Ventas por día de la semana
    dias_data = defaultdict(lambda: {"total": 0, "n": 0})
    for v in semana_data:
        d = v["fecha_str"]
        dias_data[d]["total"] += v["Total"]
        dias_data[d]["n"] += 1

    ventas_diarias = []
    for d in sorted(dias_data.keys()):
        ventas_diarias.append({
            "fecha": d,
            "total": round(dias_data[d]["total"], 2),
            "ventas": dias_data[d]["n"]
        })

    # Clientes activos (30d)
    clientes_30d = len(set(v["Cliente"] for v in ventas if v["fecha_str"] >= inicio_30d))

    # Métodos de pago (semana)
    metodos = defaultdict(lambda: {"n": 0, "total": 0})
    for v in semana_data:
        mp = v.get("MetodoPago", "Desconocido")
        metodos[mp]["n"] += 1
        metodos[mp]["total"] += v["Total"]

    # Insumos bajo mínimo
    insumos = list(db["Insumos"].find({}, {"_id": 0}))
    insumos_bajo = [i for i in insumos if i["StockActual"] <= i["StockMinimo"]]

    return jsonify({
        "fecha_actual": hoy,
        "total_hoy": round(total_hoy, 2),
        "n_ventas_hoy": n_hoy,
        "variacion_diaria": round(var, 1),
        "ticket_semana": ticket_semana,
        "n_ventas_semana": len(semana_data),
        "clientes_activos_30d": clientes_30d,
        "insumos_bajo": len(insumos_bajo),
        "total_insumos": len(insumos),
        "ventas_diarias": ventas_diarias,
        "metodos_pago": {mp: {"n": d["n"], "pct": round(d["n"]/max(len(semana_data),1)*100, 1)}
                        for mp, d in sorted(metodos.items(), key=lambda x: -x[1]["n"])},
        "insumos_bajo_detalle": [{"nombre": i["Nombre"], "stock": i["StockActual"],
                                  "minimo": i["StockMinimo"], "maximo": i["StockMaximo"]}
                                for i in insumos_bajo]
    })


# --- API: Predicción ventas ---
@app.route("/api/prediccion_ventas")
def api_prediccion_ventas():
    data = read_csv("ventas_prediccion_anual.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/ml/01_ventas_semanales.R"})

    # Próximos 30 días
    proximos = data[-30:]
    semanal = []
    for i in range(0, 30, 7):
        chunk = proximos[i:i+7]
        semanal.append({
            "semana": f"Semana {i//7 + 1}",
            "total": round(sum(float(d["yhat"]) for d in chunk), 0)
        })

    # Próximo año por mes
    anual = data[-365:]
    meses = defaultdict(float)
    for d in anual:
        mes = d["ds"][:7]
        meses[mes] += float(d["yhat"])

    return jsonify({
        "proxima_semana": [{"fecha": d["ds"], "ventas": round(float(d["yhat"]), 0),
                            "min": round(float(d["yhat_lower"]), 0),
                            "max": round(float(d["yhat_upper"]), 0)}
                          for d in data[-7:]],
        "resumen_semanal": semanal,
        "resumen_mensual": [{"mes": m, "total": round(t, 0)} for m, t in sorted(meses.items())],
        "total_anio": round(sum(float(d["yhat"]) for d in anual), 0)
    })


# --- API: Segmentación clientes ---
@app.route("/api/segmentacion")
def api_segmentacion():
    data = read_csv("segmentacion_clientes.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/proyectos/04_segmentacion_clientes.R"})

    segmentos = defaultdict(lambda: {"n": 0, "gasto": 0, "frecuencia": 0, "recencia": 0})
    for c in data:
        seg = c.get("segmento", "Sin segmento")
        segmentos[seg]["n"] += 1
        segmentos[seg]["gasto"] += float(c.get("gasto_total", 0))
        segmentos[seg]["frecuencia"] += float(c.get("frecuencia", 0))
        segmentos[seg]["recencia"] += float(c.get("recencia", 0))

    total = sum(s["n"] for s in segmentos.values())
    resultado = []
    for nombre, s in segmentos.items():
        resultado.append({
            "nombre": nombre,
            "clientes": s["n"],
            "porcentaje": round(s["n"] / max(total, 1) * 100, 1),
            "gasto_medio": round(s["gasto"] / max(s["n"], 1), 0),
            "frecuencia_media": round(s["frecuencia"] / max(s["n"], 1), 1),
            "recencia_media": round(s["recencia"] / max(s["n"], 1), 0)
        })

    return jsonify(sorted(resultado, key=lambda x: -x["gasto_medio"]))


# --- API: Combos ---
@app.route("/api/combos")
def api_combos():
    data = read_csv("reglas_asociacion.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/ml/04_combos_productos.R"})

    return jsonify([{
        "antecedente": d.get("Antecedente", "").replace("{", "").replace("}", ""),
        "consecuente": d.get("Consecuente", "").replace("{", "").replace("}", ""),
        "soporte": float(d.get("Soporte", 0)),
        "confianza": float(d.get("Confianza", 0)),
        "lift": float(d.get("Lift", 0))
    } for d in data])


# --- API: Riesgo agotamiento ---
@app.route("/api/agotamiento")
def api_agotamiento():
    data = read_csv("riesgo_agotamiento.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/ml/02_agotamiento_insumos.R"})

    return jsonify([{
        "nombre": d.get("Nombre", ""),
        "stock": float(d.get("StockActual", 0)),
        "minimo": float(d.get("StockMinimo", 0)),
        "dias": float(d.get("dias_hasta_agotar", 0)),
        "riesgo": d.get("riesgo", "DESCONOCIDO"),
        "probabilidad": float(d.get("riesgo_prob", 0))
    } for d in sorted(data, key=lambda x: -float(x.get("riesgo_prob", 0)))])


# --- API: Anomalías ---
@app.route("/api/anomalias")
def api_anomalias():
    data = read_csv("anomalias_detectadas.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/proyectos/07_deteccion_anomalias.R"})

    return jsonify({
        "total_detectadas": len(data),
        "porcentaje": 0.36,
        "muestra": [{
            "id": d.get("IdVenta", ""),
            "total": float(d.get("Total", 0)),
            "productos": int(float(d.get("cant_productos", 1))),
            "score": int(float(d.get("score_anomalia", 0)))
        } for d in data[:20]]
    })


# --- API: Top productos semanal (ventas reales) ---
@app.route("/api/top_productos")
def api_top_productos():
    detalle = mongo_find("Detalles_Venta", fields={"Producto": 1, "Cantidad": 1, "FechaVenta": 1})
    ventas_det = mongo_find("Ventas", fields={"FechaVenta": 1})

    # Última fecha
    fechas = []
    for v in ventas_det:
        f = v.get("FechaVenta")
        if isinstance(f, str):
            f = datetime.fromisoformat(f.replace("Z", ""))
        fechas.append(f.strftime("%Y-%m-%d"))

    hoy = max(fechas)
    inicio_semana = (datetime.strptime(hoy, "%Y-%m-%d") - timedelta(days=6)).strftime("%Y-%m-%d")

    top = defaultdict(float)
    for d in detalle:
        f = d.get("FechaVenta")
        if isinstance(f, str):
            f = datetime.fromisoformat(f.replace("Z", ""))
        if f.strftime("%Y-%m-%d") >= inicio_semana:
            top[d.get("Producto", "Desconocido")] += float(d.get("Cantidad", 0))

    resultado = sorted([{"producto": k, "cantidad": round(v, 0)} for k, v in top.items()],
                      key=lambda x: -x["cantidad"])[:10]

    return jsonify(resultado)


# --- API: Evolución mensual ---
@app.route("/api/evolucion_mensual")
def api_evolucion_mensual():
    ventas = mongo_find("Ventas", fields={"FechaVenta": 1, "Total": 1})
    meses = defaultdict(float)

    for v in ventas:
        f = v.get("FechaVenta")
        if isinstance(f, str):
            f = datetime.fromisoformat(f.replace("Z", ""))
        mes = f.strftime("%Y-%m")
        meses[mes] += float(v.get("Total", 0))

    return jsonify([{"mes": m, "total": round(t, 2)} for m, t in sorted(meses.items())[-12:]])


# ═══════════════════════════════════════════════════════════
# API: COMPARACIÓN DE MODELOS
# ═══════════════════════════════════════════════════════════

@app.route("/api/comparacion/ventas")
def api_comparacion_ventas():
    """Resultados comparación Prophet vs ARIMA vs XGBoost"""
    data = read_csv("comparacion_ventas.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/ml/comparacion/01_comparar_ventas.R"})
    return jsonify([{
        "modelo": d.get("Modelo", ""),
        "mape": float(d.get("MAPE", 0)),
        "rmse": float(d.get("RMSE", 0)),
        "mae": float(d.get("MAE", 0)),
        "r2": float(d.get("R2", 0)),
        "tiempo": float(d.get("Tiempo_Segundos", 0))
    } for d in data])


@app.route("/api/comparacion/segmentacion")
def api_comparacion_segmentacion():
    """Resultados comparación K-Means vs DBSCAN vs GMM"""
    data = read_csv("comparacion_segmentacion.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/ml/comparacion/02_comparar_segmentacion.R"})
    return jsonify([{
        "modelo": d.get("Modelo", ""),
        "silhouette": float(d.get("Silhouette", 0)),
        "silhouette_sd": float(d.get("Silhouette_SD", 0)) if d.get("Silhouette_SD") else None,
        "n_clusters": int(float(d.get("N_Clusters", 0))),
        "pct_outliers": float(d.get("Pct_Outliers", 0)),
        "tiempo": float(d.get("Tiempo_Segundos", 0))
    } for d in data])


@app.route("/api/comparacion/combos")
def api_comparacion_combos():
    """Resultados comparación Apriori vs FP-Growth vs Eclat"""
    data = read_csv("comparacion_combos.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/ml/comparacion/03_comparar_combos.R"})
    return jsonify([{
        "modelo": d.get("Modelo", ""),
        "n_reglas": int(float(d.get("N_Reglas", 0))),
        "lift_maximo": float(d.get("Lift_Maximo", 0)),
        "n_reglas_utiles": int(float(d.get("N_Reglas_Utiles", 0))),
        "tiempo": float(d.get("Tiempo_Segundos", 0))
    } for d in data])


@app.route("/api/comparacion/agotamiento")
def api_comparacion_agotamiento():
    """Resultados comparación RF vs XGBoost vs Regresión Logística"""
    data = read_csv("comparacion_agotamiento.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/ml/comparacion/04_comparar_agotamiento.R"})
    return jsonify([{
        "modelo": d.get("Modelo", ""),
        "f1_test": float(d.get("F1_Test", 0)),
        "f1_cv_mean": float(d.get("F1_CV_Mean", 0)),
        "f1_cv_sd": float(d.get("F1_CV_SD", 0)),
        "precision": float(d.get("Precision_Test", 0)),
        "recall": float(d.get("Recall_Test", 0)),
        "accuracy": float(d.get("Accuracy_Test", 0)),
        "tiempo": float(d.get("Tiempo_Segundos", 0))
    } for d in data])


@app.route("/api/comparacion/anomalias")
def api_comparacion_anomalias():
    """Resultados comparación Isolation Forest vs LOF vs One-Class SVM"""
    data = read_csv("comparacion_anomalias.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/ml/comparacion/05_comparar_anomalias.R"})
    return jsonify([{
        "modelo": d.get("Modelo", ""),
        "n_anomalias": int(float(d.get("N_Anomalias", 0))),
        "pct_anomalias": float(d.get("Pct_Anomalias", 0)),
        "consenso": float(d.get("Consenso_2Modelos", 0)) if d.get("Consenso_2Modelos") else None,
        "tiempo": float(d.get("Tiempo_Segundos", 0)) if d.get("Tiempo_Segundos") else None
    } for d in data])


@app.route("/api/comparacion/resumen")
def api_comparacion_resumen():
    """Tabla resumen final con ganadores por predicción"""
    data = read_csv("tabla_resumen_final.csv")
    if not data:
        return jsonify({"error": "Ejecuta primero src/ml/comparacion/06_tabla_resumen.R"})
    return jsonify([{
        "prediccion": d.get("Prediccion", ""),
        "modelo_1": d.get("Modelo_1", ""),
        "score_1": float(d.get("Score_1", 0)),
        "modelo_2": d.get("Modelo_2", ""),
        "score_2": float(d.get("Score_2", 0)),
        "modelo_3": d.get("Modelo_3", ""),
        "score_3": float(d.get("Score_3", 0)),
        "ganador": d.get("Ganador", ""),
        "metrica_clave": d.get("Metrica_Clave", "")
    } for d in data])


# ═══════════════════════════════════════════════════════════
# CRUD GENÉRICO — MongoDB
# ═══════════════════════════════════════════════════════════

# Colecciones permitidas para CRUD (seguridad: evita acceso a cualquier colección)
CRUD_COLLECTIONS = {
    "Clientes", "Productos", "Ventas", "Personal",
    "Insumos", "Proveedores", "Metodos_Pago", "Ordenes_Compra"
}

# Mapeo de nombres de campos de fecha conocidos por colección
DATE_FIELDS = {
    "Ventas": {"FechaVenta"},
    "Personal": {"FechaIngreso"},
    "Ordenes_Compra": {"FechaPedido", "FechaEntrega"},
    "Movimientos_Inventario": {"FechaMovimiento"},
}

# Campos de auditoría del ETL que se excluyen del CRUD
AUDIT_SUFFIXES = [
    "_es_nulo_imputado", "_imputada", "_es_outlier", "_limpia",
    "_estandarizada", "_es_nulo"
]


def _sanitize_doc(doc):
    """Convierte ObjectId → str y datetime → ISO string para JSON."""
    if doc is None:
        return None
    out = {}
    for k, v in doc.items():
        if k == "_id":
            out[k] = str(v)
        elif isinstance(v, datetime):
            out[k] = v.isoformat()
        elif isinstance(v, (ObjectId,)):
            out[k] = str(v)
        else:
            out[k] = v
    return out


def _parse_body_for_mongo(body, collection):
    """Convierte strings ISO a datetime para los campos de fecha conocidos."""
    date_fields = DATE_FIELDS.get(collection, set())
    for k, v in body.items():
        if k in date_fields and isinstance(v, str):
            try:
                body[k] = datetime.fromisoformat(v.replace("Z", ""))
            except (ValueError, AttributeError):
                pass
    return body


def _is_audit_field(field_name):
    """Determina si un campo es de auditoría del pipeline ETL."""
    for suffix in AUDIT_SUFFIXES:
        if field_name.endswith(suffix):
            return True
    return False


@app.route("/api/collections")
def api_collections():
    """Lista las colecciones disponibles para CRUD con conteo de documentos."""
    result = []
    for name in sorted(CRUD_COLLECTIONS):
        if name in db.list_collection_names():
            count = db[name].count_documents({})
            result.append({"name": name, "count": count})
    return jsonify(result)


@app.route("/api/crud/<collection>/schema")
def api_crud_schema(collection):
    """Infiera el schema desde un documento de muestra."""
    if collection not in CRUD_COLLECTIONS:
        return jsonify({"error": "Colección no permitida"}), 403
    if collection not in db.list_collection_names():
        return jsonify({"error": "Colección no encontrada"}), 404

    sample = db[collection].find_one({}) or {}
    schema = []
    for key, value in sample.items():
        if _is_audit_field(key):
            schema.append({"key": key, "type": "text", "audit": True})
        elif key == "_id":
            schema.append({"key": "_id", "type": "objectid"})
        elif isinstance(value, datetime):
            schema.append({"key": key, "type": "date"})
        elif isinstance(value, bool):
            schema.append({"key": key, "type": "boolean"})
        elif isinstance(value, int):
            schema.append({"key": key, "type": "integer"})
        elif isinstance(value, float):
            schema.append({"key": key, "type": "number"})
        else:
            schema.append({"key": key, "type": "text"})
    return jsonify(schema)


@app.route("/api/crud/<collection>")
def api_crud_list(collection):
    """Lista paginada de documentos."""
    if collection not in CRUD_COLLECTIONS:
        return jsonify({"error": "Colección no permitida"}), 403
    if collection not in db.list_collection_names():
        return jsonify({"error": "Colección no encontrada"}), 404

    page = int(request.args.get("page", 1))
    per_page = min(int(request.args.get("per_page", 25)), 100)
    search = request.args.get("search", "").strip()
    sort_field = request.args.get("sort", "_id")
    sort_order = int(request.args.get("order", -1))

    # Construir query con búsqueda
    query = {}
    if search:
        sample = db[collection].find_one({}) or {}
        text_fields = [
            k for k, v in sample.items()
            if isinstance(v, str) and not _is_audit_field(k) and k != "_id"
        ]
        # También buscar en campos numéricos convertidos a string (teléfono, DNI, RUC)
        number_fields = [
            k for k, v in sample.items()
            if isinstance(v, (int, float)) and not _is_audit_field(k)
        ]
        or_clauses = []

        # Regex en campos de texto
        regex = re.compile(re.escape(search), re.IGNORECASE)
        for field in text_fields:
            or_clauses.append({field: {"$regex": regex}})

        # Coincidencia exacta parcial en campos numéricos (ej: teléfono, DNI)
        search_num = None
        try:
            search_num = float(search)
        except ValueError:
            pass
        if search_num is not None:
            for field in number_fields:
                or_clauses.append({field: search_num})

        if or_clauses:
            query["$or"] = or_clauses

    total = db[collection].count_documents(query)
    total_pages = max(1, (total + per_page - 1) // per_page)

    docs = list(
        db[collection]
        .find(query)
        .sort(sort_field, sort_order)
        .skip((page - 1) * per_page)
        .limit(per_page)
    )

    docs = [_sanitize_doc(d) for d in docs]

    return jsonify({
        "data": docs,
        "total": total,
        "page": page,
        "per_page": per_page,
        "total_pages": total_pages
    })


@app.route("/api/crud/<collection>/<doc_id>")
def api_crud_get(collection, doc_id):
    """Obtener un documento por _id."""
    if collection not in CRUD_COLLECTIONS:
        return jsonify({"error": "Colección no permitida"}), 403
    if collection not in db.list_collection_names():
        return jsonify({"error": "Colección no encontrada"}), 404

    try:
        obj_id = ObjectId(doc_id)
    except Exception:
        return jsonify({"error": "ID inválido"}), 400

    doc = db[collection].find_one({"_id": obj_id})
    if doc is None:
        return jsonify({"error": "Documento no encontrado"}), 404

    return jsonify(_sanitize_doc(doc))


@app.route("/api/crud/<collection>", methods=["POST"])
def api_crud_create(collection):
    """Crear un nuevo documento."""
    if collection not in CRUD_COLLECTIONS:
        return jsonify({"error": "Colección no permitida"}), 403
    if collection not in db.list_collection_names():
        return jsonify({"error": "Colección no encontrada"}), 404

    body = request.get_json(silent=True)
    if not body or not isinstance(body, dict):
        return jsonify({"error": "Body JSON requerido"}), 400

    # Eliminar _id si viene (MongoDB genera uno nuevo)
    body.pop("_id", None)

    body = _parse_body_for_mongo(body, collection)

    try:
        # ── Hook: Ventas → auto-registrar cliente ──
        if collection == "Ventas":
            _auto_registrar_cliente(body)

        # ── Hook: Ventas → descontar insumos ──
        if collection == "Ventas":
            _descontar_insumos_venta(body)

        result = db[collection].insert_one(body)
        new_doc = db[collection].find_one({"_id": result.inserted_id})
        return jsonify(_sanitize_doc(new_doc)), 201
    except Exception as e:
        return jsonify({"error": str(e)}), 500


def _auto_registrar_cliente(venta_body):
    """Si el cliente de la venta no existe en Clientes, lo crea automáticamente."""
    nombre_completo = venta_body.get("Cliente", "").strip()
    if not nombre_completo:
        return

    # Verificar si ya existe (búsqueda exacta)
    existente = db["Clientes"].find_one({
        "$or": [
            {"PrimerNombre": {"$regex": f"^{re.escape(nombre_completo.split()[0])}$", "$options": "i"}},
        ]
    }) if len(nombre_completo.split()) > 0 else None

    if existente:
        return  # Ya existe, no duplicar

    # Parsear nombre: "Ana Valeria Vargas Suarez" → PrimerNombre, ApellidoPaterno, etc.
    partes = nombre_completo.split()
    if len(partes) >= 4:
        primer_nombre = partes[0]
        segundo_nombre = partes[1]
        apellido_paterno = partes[2]
        apellido_materno = " ".join(partes[3:])
    elif len(partes) == 3:
        primer_nombre = partes[0]
        segundo_nombre = ""
        apellido_paterno = partes[1]
        apellido_materno = partes[2]
    elif len(partes) == 2:
        primer_nombre = partes[0]
        segundo_nombre = ""
        apellido_paterno = partes[1]
        apellido_materno = ""
    else:
        primer_nombre = nombre_completo
        segundo_nombre = ""
        apellido_paterno = ""
        apellido_materno = ""

    nuevo_cliente = {
        "PrimerNombre": primer_nombre,
        "SegundoNombre": segundo_nombre,
        "ApellidoPaterno": apellido_paterno,
        "ApellidoMaterno": apellido_materno,
        "Telefono": 0.0,
        "Correo": "",
    }
    try:
        db["Clientes"].insert_one(nuevo_cliente)
        print(f"  [Auto-Cliente] Creado: {nombre_completo}")
    except Exception as e:
        print(f"  [Auto-Cliente] Error: {e}")


def _descontar_insumos_venta(venta_body):
    """Descuenta insumos del inventario según los productos vendidos.
    Busca productos en formato Producto1_Nombre/Cantidad y consulta Detalle_Insumo."""
    for i in range(1, 8):  # Hasta 7 productos por venta (formato ancho)
        nom_key = f"Producto{i}_Nombre"
        cant_key = f"Producto{i}_Cantidad"
        producto_nombre = venta_body.get(nom_key, "").strip()
        cantidad_vendida = float(venta_body.get(cant_key, 0) or 0)

        if not producto_nombre or cantidad_vendida <= 0:
            continue

        # Buscar el producto en Productos para obtener sus insumos
        producto = db["Productos"].find_one({"NombreProducto": producto_nombre})
        if not producto:
            continue

        # Recorrer insumos del producto (Insumo1_Nombre/Cantidad ... Insumo6_Nombre/Cantidad)
        for j in range(1, 7):
            ins_key = f"Insumo{j}_Nombre"
            ins_cant_key = f"Insumo{j}_Cantidad"
            insumo_nombre = producto.get(ins_key, "").strip() if isinstance(producto.get(ins_key), str) else ""
            insumo_por_unidad = float(producto.get(ins_cant_key, 0) or 0)

            if not insumo_nombre or insumo_por_unidad <= 0:
                continue

            # Calcular consumo total: cantidad_vendida * insumo_por_unidad
            consumo_total = cantidad_vendida * insumo_por_unidad

            # Descontar del stock
            resultado = db["Insumos"].update_one(
                {"Nombre": insumo_nombre},
                {"$inc": {"StockActual": -consumo_total}}
            )
            if resultado.matched_count > 0:
                print(f"  [Descuento] {insumo_nombre}: -{consumo_total:.2f} (por {cantidad_vendida}x {producto_nombre})")


@app.route("/api/crud/<collection>/<doc_id>", methods=["PUT"])
def api_crud_update(collection, doc_id):
    """Actualizar un documento existente."""
    if collection not in CRUD_COLLECTIONS:
        return jsonify({"error": "Colección no permitida"}), 403
    if collection not in db.list_collection_names():
        return jsonify({"error": "Colección no encontrada"}), 404

    try:
        obj_id = ObjectId(doc_id)
    except Exception:
        return jsonify({"error": "ID inválido"}), 400

    body = request.get_json(silent=True)
    if not body or not isinstance(body, dict):
        return jsonify({"error": "Body JSON requerido"}), 400

    # No permitir modificar _id
    body.pop("_id", None)

    body = _parse_body_for_mongo(body, collection)

    try:
        result = db[collection].update_one({"_id": obj_id}, {"$set": body})
        if result.matched_count == 0:
            return jsonify({"error": "Documento no encontrado"}), 404
        updated_doc = db[collection].find_one({"_id": obj_id})
        return jsonify(_sanitize_doc(updated_doc))
    except Exception as e:
        return jsonify({"error": str(e)}), 500


@app.route("/api/crud/<collection>/<doc_id>", methods=["DELETE"])
def api_crud_delete(collection, doc_id):
    """Eliminar un documento."""
    if collection not in CRUD_COLLECTIONS:
        return jsonify({"error": "Colección no permitida"}), 403
    if collection not in db.list_collection_names():
        return jsonify({"error": "Colección no encontrada"}), 404

    try:
        obj_id = ObjectId(doc_id)
    except Exception:
        return jsonify({"error": "ID inválido"}), 400

    result = db[collection].delete_one({"_id": obj_id})
    if result.deleted_count == 0:
        return jsonify({"error": "Documento no encontrado"}), 404

    return jsonify({"ok": True, "deleted_id": doc_id})


# ═══════════════════════════════════════════════════════════
# ACTUALIZACIÓN: Pipeline de predicciones con progreso
# ═══════════════════════════════════════════════════════════

@app.route("/api/ejecutar-predicciones", methods=["POST"])
def api_ejecutar_predicciones():
    """Inicia el pipeline de predicciones (8 scripts R) en background.
    Todos los scripts leen directamente de MongoDB."""
    global pipeline_status

    with pipeline_lock:
        if pipeline_status["running"]:
            return jsonify({"error": "Ya hay una actualización en curso"}), 409
        pipeline_status = {
            "running": True,
            "step": "Iniciando predicciones...",
            "percent": 0,
            "error": None
        }

    # Lanzar pipeline en background
    thread = threading.Thread(target=_ejecutar_pipeline, daemon=True)
    thread.start()

    return jsonify({"ok": True, "mensaje": "Pipeline iniciado. Monitoree el progreso en /api/progreso"})


@app.route("/api/progreso")
def api_progreso():
    """Devuelve el estado actual del pipeline"""
    with pipeline_lock:
        return jsonify(dict(pipeline_status))


def _ejecutar_pipeline():
    """Ejecuta 8 scripts: 4 ML + 4 proyectos. Todos leen de MongoDB."""
    global pipeline_status

    scripts = [
        # (ruta, nombre_visible, rango_porcentaje)
        ("src/ml/01_ventas_semanales.R", "ML 1/4: Ventas semanales (Prophet)", (0, 25)),
        ("src/ml/02_agotamiento_insumos.R", "ML 2/4: Agotamiento insumos (Random Forest)", (25, 50)),
        ("src/ml/04_combos_productos.R", "ML 3/4: Combos productos (Apriori)", (50, 62)),
        ("src/ml/06_cantidad_productos.R", "ML 4/4: Unidades por producto (Random Forest)", (62, 75)),
        ("src/proyectos/01_dashboard_ejecutivo.R", "Proyecto 1/4: Dashboard ejecutivo", (75, 81)),
        ("src/proyectos/02_alerta_stock.R", "Proyecto 2/4: Alerta de stock", (81, 87)),
        ("src/proyectos/04_segmentacion_clientes.R", "Proyecto 3/4: Segmentación clientes", (87, 93)),
        ("src/proyectos/07_deteccion_anomalias.R", "Proyecto 4/4: Detección anomalías", (93, 100)),
    ]

    try:
        for i, (script, nombre, (pct_inicio, pct_fin)) in enumerate(scripts):
            with pipeline_lock:
                pipeline_status["step"] = f"Paso {i+1}/8: {nombre}"
                pipeline_status["percent"] = pct_inicio

            script_path = os.path.join(PROJECT, script)
            if not os.path.exists(script_path):
                raise FileNotFoundError(f"No se encontró: {script_path}")

            result = subprocess.run(
                [RSCRIPT, script_path],
                cwd=PROJECT,
                capture_output=True,
                text=True,
                timeout=600
            )

            if result.returncode != 0:
                error_msg = result.stderr.strip().split("\n")[-1] if result.stderr else f"Código de salida: {result.returncode}"
                raise RuntimeError(f"Error en {script}: {error_msg}")

            with pipeline_lock:
                pipeline_status["percent"] = pct_fin

        # Éxito
        with pipeline_lock:
            pipeline_status["step"] = "¡Predicciones actualizadas! Todos los reportes están listos."
            pipeline_status["percent"] = 100
            pipeline_status["running"] = False

    except Exception as e:
        with pipeline_lock:
            pipeline_status["error"] = str(e)[:500]
            pipeline_status["step"] = f"Error: {str(e)[:200]}"
            pipeline_status["running"] = False


if __name__ == "__main__":
    print(r"""
    ╔══════════════════════════════════════════════╗
    ║   Pollería la Infantería — Dashboard        ║
    ║   http://localhost:5000                     ║
    ║                                              ║
    ║   CRUD MongoDB + Predicciones R → Listo     ║
    ╚══════════════════════════════════════════════╝
    """)
    port = int(os.environ.get("PORT", 5000))
    debug = os.environ.get("FLASK_DEBUG", "false").lower() == "true"
    app.run(debug=debug, host="0.0.0.0", port=port)
