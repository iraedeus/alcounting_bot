from telegram.ext import CallbackQueryHandler, CommandHandler

from bot.handlers.callback_handler import callback_handler
from bot.handlers.help_handler import help_handler
from bot.handlers.menu_handler import menu_handler
from bot.handlers.start_handler import start_handler

def register_handlers(application):
    application.add_handler(CallbackQueryHandler(callback_handler))
    application.add_handler(CommandHandler('help', help_handler))
    application.add_handler(CommandHandler('menu', menu_handler))
    application.add_handler(CommandHandler('start', start_handler))

__all__ = [
    'register_handlers',
]