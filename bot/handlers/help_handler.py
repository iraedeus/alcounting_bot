"""
This module defines the help handler for the bot.
"""

import logging
from telegram import Update
from telegram.ext import ContextTypes

from bot.settings import load_texts


async def help_handler(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    """Send a message when the command /help is issued."""
    logging.getLogger(__name__).info(
        "%s use %s", update.message.from_user.id, update.message.text
    )
    await update.message.reply_text(
        load_texts(update.effective_user.language_code)["texts"]["help"]
    )
