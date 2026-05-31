import os
from pathlib import Path


def load_rconfig(renviron_path=None):
    candidates = []

    if renviron_path is not None:
        candidates.append(Path(renviron_path))
    else:
        candidates.extend([
            Path.home() / ".Renviron",
            Path.home() / "Documents" / ".Renviron",
        ])

    env_config = {}

    for candidate in candidates:
        if candidate.exists():
            with open(candidate, "r", encoding="utf-8") as file:
                renviron_content = [line.strip() for line in file.readlines()]
            env_config.update(
                {
                    key: value.strip('"')
                    for key, value in [line.split("=", 1) for line in renviron_content if "=" in line]
                }
            )
            break

    if not env_config:
        env_config = {
            key: value
            for key, value in os.environ.items()
            if key in {
                "snowAdminUser",
                "snowAdminPass",
                "snowHost",
                "snowPort",
                "aquacacheAdminUser",
                "aquacacheAdminPass",
                "aquacacheHost",
                "aquacachePort",
            }
        }

    # Convert numeric values to their appropriate types
    for key, value in env_config.items():
        if isinstance(value, str) and value.isdigit():
            env_config[key] = int(value)
        else:
            try:
                env_config[key] = float(value)
            except (TypeError, ValueError):
                pass

    return env_config


def _require_config_value(config, key):
    if key not in config or config[key] in (None, ""):
        raise RuntimeError(
            f"Missing required database configuration value '{key}'. Set it in ~/.Renviron or as an environment variable."
        )
    return config[key]


def _build_db_url(config, user_key, pass_key, host_key, port_key):
    return (
        f"postgresql://{_require_config_value(config, user_key)}:{_require_config_value(config, pass_key)}"
        f"@{_require_config_value(config, host_key)}:{_require_config_value(config, port_key)}/aquacache"
    )


# Load database configuration from .Renviron
db_config = load_rconfig()

db_url_prod = _build_db_url(
    db_config,
    "snowAdminUser",
    "snowAdminPass",
    "snowHost",
    "snowPort",
)
db_url_dev = _build_db_url(
    db_config,
    "aquacacheAdminUser",
    "aquacacheAdminPass",
    "aquacacheHost",
    "aquacachePort",
)