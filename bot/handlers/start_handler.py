import logging
from telegram import Update
from telegram.ext import ContextTypes

from bot.database import Database, User


async def start_handler(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    """Send a message when the command /start is issued."""
    logging.getLogger(__name__).info(f'{update.message.from_user.id} use {update.message.text}')
    user = update.effective_user
    """await update.message.reply_html(
        rf"Hi {user.mention_html()}!",
        reply_markup=ForceReply(selective=True),
    )"""
    database = Database()
    new_user = User(user.id, user.name, "customer")
    database.insert_user(new_user)
    await update.message.reply_text('Привет! Я бот. Нажимай на кнопку "/help" для получения подсказок по командам.')