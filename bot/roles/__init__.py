"""
This module initializes role associations for the bot.
"""

from bot.roles.admin import Admin
from bot.roles.barman import Barman
from bot.roles.customer import Customer

__all__ = ["Admin", "Barman", "Customer", "role_associations"]

role_associations = {"admin": Admin, "customer": Customer, "barman": Barman}
