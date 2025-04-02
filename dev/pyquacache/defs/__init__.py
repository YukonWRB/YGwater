from pathlib import Path



def load_rconfig(renviron_path = Path.home() / "Documents" / ".Renviron"):
    # Define the path to the .Renviron file in the "My Documents" directory
    # Check if the file exists
    if renviron_path.exists():
        # Read the .Renviron file
        with open(renviron_path, 'r') as file:
            renviron_content = [line.strip() for line in file.readlines()]
    env_config = {v[0]: v[1].strip('"') for v in [line.split('=') for line in renviron_content if '=' in line]}
    # Convert numeric values to their appropriate types
    for key, value in env_config.items():
        if value.isdigit():
            env_config[key] = int(value)
        else:
            try:
                env_config[key] = float(value)
            except ValueError:
                pass
    return env_config


# Load database configuration from .Renviron
db_config = load_rconfig()
db_url = f"postgresql://{db_config['aquacacheAdminUser']}:{db_config['aquacacheAdminPass']}@{db_config['aquacacheHost']}:{db_config['aquacachePort']}/aquacache"
