import logging
from telegram import Update
from telegram.ext import CallbackContext
from telegram.constants import ParseMode

from bot.database import Database
from bot.roles.admin import Admin
from bot.roles.barman import Barman
from bot.roles.customer import Customer



async def callback_handler(update: Update, context: CallbackContext) -> None:
    """
    This handler processes the inline buttons on the menu
    """
    database = Database()
    tg_user = update.effective_user
    current_user = database.get_user_by_id(tg_user.id)

    data = update.callback_query.data
    text = ''
    markup = None

    if current_user.type == "customer":
        text, markup = Customer.on_button_tap(Customer, data, tg_user.id)
        if markup is None:
            text, markup = Customer.back_to_customer_menu(Customer, data, tg_user.id)
    elif current_user.type == "barman":
        text, markup = Barman.on_button_tap(Barman, data, tg_user.id)
        if markup is None:
            text, markup = Barman.back_to_barman_menu(Barman, data, tg_user.id)
    elif current_user.type == "admin":
        pass
    else:
        logging.getLogger(__name__).error("incorrect user type")

    logging.getLogger(__name__).info(f'{update.effective_user.id} Callbackdata: {data}')

    # Close the query to end the client-side loading animation
    await update.callback_query.answer()

    # Update message content with corresponding menu section
    await update.callback_query.edit_message_text(
        text,
        ParseMode.HTML,
        reply_markup=markup
    )