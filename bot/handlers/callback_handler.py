"""
This module contains the callback handler for the bot.
"""

import logging
from telegram import Update
from telegram.ext import CallbackContext
from telegram.constants import ParseMode

from bot.database import Database
from bot.roles import role_associations
from bot.settings import load_texts


async def callback_handler(update: Update, context: CallbackContext) -> None:
    """
    This handler processes the inline buttons on the menu.
    """
    db = Database()
    tg_user = update.effective_user
    current_user = db.get_user_by_id(tg_user.id)

    data = update.callback_query.data
    text = ""
    markup = None

    role_class = role_associations.get(current_user.type)
    texts = load_texts(tg_user.language_code)["texts"]
    role_obj = role_class(db, tg_user.id, texts)
    if role_class:
        text, markup = role_obj.on_button_tap(data)
        if not text or markup is None:
            text, markup = "Callback, Err0r", role_obj.build_menu()
    else:
        logging.error("incorrect user type")

    logging.info("Callbackdata: %s", data)

    await update.callback_query.answer()

    await update.callback_query.edit_message_text(
        text, ParseMode.HTML, reply_markup=markup
    )
