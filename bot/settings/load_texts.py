"""
This module provides a function to load localized texts from TOML files.
"""

import toml


def load_texts(locale="ru"):
    """
    Load localized texts from a TOML file based on the given locale.

    Args:
        locale (str): The locale code (e.g., "ru" or "en").

    Returns:
        dict: The loaded texts from the TOML file.
    """
    locale = locale if locale in ["ru", "en"] else "ru"
    with open(f"res/locales/{locale}.toml", encoding="utf-8") as f:
        return toml.load(f)
