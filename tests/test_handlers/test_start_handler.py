import pytest
from unittest.mock import MagicMock, AsyncMock, patch
from telegram import User as TelegramUser, Update, Message
from bot.handlers.start_handler import start_handler
from bot.database import Database, User


@pytest.mark.asyncio
async def test_start_handler():

    telegram_user = TelegramUser(id=1, first_name="Test", is_bot=False)

    message = MagicMock(spec=Message)
    message.from_user = telegram_user
    message.text = "/start"
    update = MagicMock(spec=Update)
    update.effective_user = telegram_user
    update.message = message

    context = MagicMock()
    context.bot = AsyncMock()

    database = MagicMock(spec=Database)
    user = User(id=1, name="Test", type="customer")
    user.language_code = "ru"

    with patch("bot.handlers.start_handler.Database", return_value=database):
        with patch("bot.handlers.start_handler.User", return_value=user):
            await start_handler(update, context)

            database.insert_user.assert_called_once_with(user)
            message.reply_text.assert_called_once_with(
                'Привет! Я бот. Нажимай на кнопку "/help" для получения подсказок по командам.'
            )
