import pandas as pd
import numpy as np

xlsx = 'data/raw/data_2023.xlsx'

# Cargar las hojas necesarias
df_ventas = pd.read_excel(xlsx, sheet_name='Ventas')

# Convertir FechaVenta a datetime
df_ventas['FechaVenta'] = pd.to_datetime(df_ventas['FechaVenta'], format='mixed', utc=True)

print('=' * 70)
print('PATRON 1: DIA DE LA SEMANA CON MAYOR VOLUMEN DE VENTAS')
print('=' * 70)

dias_semana = ['Lunes', 'Martes', 'Miercoles', 'Jueves', 'Viernes', 'Sabado', 'Domingo']
df_ventas['dia_semana'] = df_ventas['FechaVenta'].dt.dayofweek
df_ventas['nombre_dia'] = df_ventas['dia_semana'].map(lambda x: dias_semana[x])

ventas_por_dia = df_ventas.groupby('nombre_dia').agg(
    monto_total=('Total', 'sum'),
    num_ventas=('Total', 'count'),
    ticket_promedio=('Total', 'mean')
).round(2).sort_values('monto_total', ascending=False)

print(ventas_por_dia.to_string())
print()

mejor_dia = ventas_por_dia['monto_total'].idxmax()
peor_dia = ventas_por_dia['monto_total'].idxmin()
mejor_monto = ventas_por_dia.loc[mejor_dia, 'monto_total']
mejor_ventas = ventas_por_dia.loc[mejor_dia, 'num_ventas']
peor_monto = ventas_por_dia.loc[peor_dia, 'monto_total']

print(f'>>> CONCLUSION: {mejor_dia} es el dia con mayor volumen de ventas')
print(f'    (S/ {mejor_monto:,.2f} en total, {mejor_ventas:,} ventas)')
print(f'    Recomendacion: reforzar personal y stock los {mejor_dia},')
print(f'    y evaluar promociones para {peor_dia} (S/ {peor_monto:,.2f})')
print()

print()
print('=' * 70)
print('PATRON 2: METODO DE PAGO CON MENOR TICKET PROMEDIO')
print('=' * 70)

# El nuevo Excel ya tiene MetodoPago como texto directo ("Efectivo", "Plin", etc.)
ticket_por_metodo = df_ventas.groupby('MetodoPago').agg(
    ticket_promedio=('Total', 'mean'),
    num_ventas=('Total', 'count'),
    monto_total=('Total', 'sum')
).round(2).sort_values('ticket_promedio')

# Ya no se necesita mapeo de IDs a nombres

print(ticket_por_metodo.to_string())
print()

menor_metodo = ticket_por_metodo['ticket_promedio'].idxmin()
mayor_metodo = ticket_por_metodo['ticket_promedio'].idxmax()
menor_ticket = ticket_por_metodo.loc[menor_metodo, 'ticket_promedio']
mayor_ticket = ticket_por_metodo.loc[mayor_metodo, 'ticket_promedio']

print(f'>>> CONCLUSION: {menor_metodo} tiene el ticket promedio mas bajo (S/ {menor_ticket:,.2f})')
print(f'    mientras {mayor_metodo} tiene el mas alto (S/ {mayor_ticket:,.2f})')
print(f'    Diferencia: S/ {mayor_ticket - menor_ticket:,.2f} por transaccion')
print(f'    Recomendacion: evaluar incentivos para migrar clientes hacia')
print(f'    metodos con mayor ticket promedio')
print()

print()
print('=' * 70)
print('PATRON 3: CONCENTRACION DE INGRESOS - REGLA 80/20')
print('=' * 70)

gasto_por_cliente = df_ventas.groupby('Cliente').agg(
    gasto_total=('Total', 'sum'),
    num_compras=('Total', 'count')
).sort_values('gasto_total', ascending=False)

total_clientes = len(gasto_por_cliente)
ingreso_total = gasto_por_cliente['gasto_total'].sum()

top20_n = int(np.ceil(total_clientes * 0.2))
top20_clientes = gasto_por_cliente.head(top20_n)
ingreso_top20 = top20_clientes['gasto_total'].sum()
porcentaje_top20 = round(100 * ingreso_top20 / ingreso_total, 1)

top10_n = int(np.ceil(total_clientes * 0.1))
top10_clientes = gasto_por_cliente.head(top10_n)
ingreso_top10 = top10_clientes['gasto_total'].sum()
porcentaje_top10 = round(100 * ingreso_top10 / ingreso_total, 1)

top5_n = int(np.ceil(total_clientes * 0.05))
top5_clientes = gasto_por_cliente.head(top5_n)
ingreso_top5 = top5_clientes['gasto_total'].sum()
porcentaje_top5 = round(100 * ingreso_top5 / ingreso_total, 1)

bottom80 = gasto_por_cliente.iloc[top20_n:]
ingreso_bottom80 = bottom80['gasto_total'].sum()

print(f'Total de clientes con compras: {total_clientes:,}')
print(f'Ingreso total: S/ {ingreso_total:,.2f}')
print()
print(f'Top 5%  ({top5_n:4d} clientes): S/ {ingreso_top5:>12,.2f}  ({porcentaje_top5:5.1f}%)')
print(f'Top 10% ({top10_n:4d} clientes): S/ {ingreso_top10:>12,.2f}  ({porcentaje_top10:5.1f}%)')
print(f'Top 20% ({top20_n:4d} clientes): S/ {ingreso_top20:>12,.2f}  ({porcentaje_top20:5.1f}%)')
print(f'Resto 80% ({total_clientes - top20_n:4d} clientes): S/ {ingreso_bottom80:>12,.2f}  ({100 - porcentaje_top20:5.1f}%)')
print()

if porcentaje_top20 >= 80:
    print(f'>>> CONCLUSION: Se CUMPLE la regla 80/20.')
else:
    print(f'>>> CONCLUSION: NO se cumple estrictamente la regla 80/20.')

print(f'    El {porcentaje_top20}% de los ingresos lo genera el 20% de clientes.')
print(f'    Recomendacion: priorizar un programa de fidelizacion para ese')
print(f'    segmento top en lugar de campanas masivas.')

print(f'')
print(f'    Ticket promedio cliente top 20%:    S/ {top20_clientes["gasto_total"].mean():,.2f}')
print(f'    Ticket promedio cliente bottom 80%: S/ {bottom80["gasto_total"].mean():,.2f}')
print(f'    Compras promedio top 20%:           {top20_clientes["num_compras"].mean():.1f}')
print(f'    Compras promedio bottom 80%:        {bottom80["num_compras"].mean():.1f}')
print(f'    Gasto maximo (cliente #1):          S/ {gasto_por_cliente.iloc[0]["gasto_total"]:,.2f}')
print(f'    Gasto minimo:                       S/ {gasto_por_cliente.iloc[-1]["gasto_total"]:,.2f}')
