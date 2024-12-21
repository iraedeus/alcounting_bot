import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from telegram import User as TelegramUser, Update, Message
from telegram.constants import ParseMode

from bot.database import Database
from bot.handlers.menu_handler import menu_handler
from bot.settings import load_texts


@pytest.mark.asyncio
async def test_menu_handler_customer():
    telegram_user = TelegramUser(
        id=1, first_name="Test", is_bot=False, language_code="ru"
    )
    message = MagicMock(spec=Message)
    message.from_user = telegram_user
    message.text = "/menu"
    update = MagicMock(spec=Update)
    update.effective_user = telegram_user
    update.message = message

    context = MagicMock()
    context.bot.send_message = AsyncMock()

    database = MagicMock(spec=Database)
    user = MagicMock()
    user.type = "customer"

    with patch("bot.handlers.menu_handler.Database", return_value=database):
        with patch.object(database, "get_user_by_id", return_value=user):
            with patch("bot.roles.Customer.build_menu", return_value="customer_menu"):
                await menu_handler(update, context)

                context.bot.send_message.assert_called_once_with(
                    telegram_user.id,
                    load_texts(telegram_user.language_code)["texts"]["menu"],
                    parse_mode=ParseMode.HTML,
                    reply_markup="customer_menu",
                )


@pytest.mark.asyncio
async def test_menu_handler_barman():
    telegram_user = TelegramUser(
        id=1, first_name="Test", is_bot=False, language_code="ru"
    )
    message = MagicMock(spec=Message)
    message.from_user = telegram_user
    message.text = "/menu"
    update = MagicMock(spec=Update)
    update.effective_user = telegram_user
    update.message = message

    context = MagicMock()
    context.bot.send_message = AsyncMock()

    database = MagicMock(spec=Database())
    user = MagicMock()
    user.type = "barman"

    with patch("bot.handlers.menu_handler.Database", return_value=database):
        with patch.object(database, "get_user_by_id", return_value=user):
            with patch("bot.roles.Barman.build_menu", return_value="barman_menu"):
                await menu_handler(update, context)

                context.bot.send_message.assert_called_once_with(
                    telegram_user.id,
                    load_texts(telegram_user.language_code)["texts"]["menu"],
                    parse_mode=ParseMode.HTML,
                    reply_markup="barman_menu",
                )
