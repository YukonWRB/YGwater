import sqlite3
import pandas as pd


TABLE_NAMES = ['freeze_up', 'snow_free', 'table_stations', 'dc_constants']

def get_drought_codes_table(table_name, db_path='.data/drought_codes.sqlite'):

    if table_name not in TABLE_NAMES:
        raise ValueError(f"Table name {table_name} not found in database")

    # Connect to the SQLite database
    connection = sqlite3.connect(db_path)

    # Create a cursor object to interact with the database
    cursor = connection.cursor()

    # Example query to fetch data from a table
    query = f"SELECT * FROM {table_name}"

    try:
        cursor.execute(query)
        rows = cursor.fetchall()
        columns = [description[0] for description in cursor.description]
        df = pd.DataFrame(rows, columns=columns)
    except sqlite3.Error as e:
        print(f"An error occurred: {e}")

    # Close the connection
    connection.close()
    df.set_index("id", inplace=True)
    return df