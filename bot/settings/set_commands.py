"""
This module provides a function to set bot commands based on the given locale.
"""

from telegram import BotCommand
from bot.settings import load_texts


async def set_commands(bot, locale="ru"):
    """
    Set bot commands based on the given locale.
    """
    texts = load_texts(locale)["texts"]
    commands = [
        BotCommand("start", texts["start_description"]),
        BotCommand("help", texts["help_description"]),
        BotCommand("menu", texts["menu_description"]),
    ]
    await bot.set_my_commands(commands=commands)
