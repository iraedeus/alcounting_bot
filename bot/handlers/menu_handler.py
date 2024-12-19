"""
This module contains the menu handler for the bot.
"""

import logging
from telegram import Update
from telegram.ext import CallbackContext
from telegram.constants import ParseMode

from bot.database import Database
from bot.roles import role_associations
from bot.settings import load_texts


async def menu_handler(update: Update, context: CallbackContext) -> None:
    """
    This handler sends a menu with the inline buttons we pre-assigned above
    """

    logging.getLogger(__name__).info(
        "%s use %s", update.message.from_user.id, update.message.text
    )

    db = Database()
    tg_user = update.effective_user
    current_user = db.get_user_by_id(tg_user.id)
    role_class = role_associations.get(current_user.type)
    texts = load_texts(tg_user.language_code)["texts"]
    role_object = role_class(db, tg_user.id, texts)

    await context.bot.send_message(
        update.message.from_user.id,
        texts["menu"],
        parse_mode=ParseMode.HTML,
        reply_markup=role_object.build_menu(),
    )
