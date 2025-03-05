import os



XML_FILE_URL = "https://www.weather.gov/source/aprfc/toCanada/YukonFlows.xml"

PEM_FILE = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "certificates/weather-gov-chain.pem")