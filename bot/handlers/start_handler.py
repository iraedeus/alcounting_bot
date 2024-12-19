"""
This module defines the start handler for the bot.
"""

import logging
from telegram import Update
from telegram.ext import ContextTypes

from bot.database import Database, User
from bot.settings import load_texts


async def start_handler(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    """Send a message when the command /start is issued."""
    logging.getLogger(__name__).info(
        "%s use %s", update.message.from_user.id, update.message.text
    )
    user = update.effective_user

    database = Database()
    new_user = User(user.id, user.name)
    database.insert_user(new_user)
    await update.message.reply_text(
        load_texts(user.language_code)["texts"]["start"],
    )
