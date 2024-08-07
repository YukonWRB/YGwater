from flask import Flask, request, jsonify, abort
import psycopg2
from psycopg2 import sql
from functools import wraps
import ssl

app = Flask(__name__)

# Simple API key validation
API_KEYS = {'your_api_key': 'user_role'}

def require_api_key(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        api_key = request.headers.get('X-API-KEY')
        if api_key and API_KEYS.get(api_key):
            return f(*args, **kwargs)
        else:
            abort(401)
    return decorated_function

# Database connection
def get_db_connection():
    conn = psycopg2.connect(
        dbname='your_dbname',
        user='your_dbuser',
        password='your_dbpassword',
        host='your_dbhost',
        port='your_dbport'
    )
    return conn

# Helper function to build SQL query with filters
def build_query(base_query, filters):
    query = sql.SQL(base_query)
    if filters:
        conditions = [sql.SQL("{} = {}").format(sql.Identifier(k), sql.Placeholder(k)) for k in filters.keys()]
        query += sql.SQL(" WHERE ") + sql.SQL(" AND ").join(conditions)
    return query

# Example endpoint with filters and authentication
@app.route('/data', methods=['GET'])
@require_api_key
def get_data():
    table_name = request.args.get('table', default='default_view')
    filters = {k: v for k, v in request.args.items() if k != 'table'}

    # Input validation
    allowed_tables = ['default_view', 'another_view']
    if table_name not in allowed_tables:
        abort(400, description="Invalid table name")

    base_query = f"SELECT * FROM {table_name}"
    query = build_query(base_query, filters)
    
    conn = get_db_connection()
    cursor = conn.cursor()
    cursor.execute(query, filters)
    data = cursor.fetchall()
    cursor.close()
    conn.close()

    return jsonify(data)

if __name__ == '__main__':
    context = ssl.SSLContext(ssl.PROTOCOL_TLS)
    context.load_cert_chain('path/to/cert.pem', 'path/to/key.pem')
    app.run(host='0.0.0.0', port=5000, ssl_context=context)
