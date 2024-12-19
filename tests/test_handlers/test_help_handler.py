import pytest
from unittest.mock import MagicMock
from telegram import User as TelegramUser, Update, Message
from bot.handlers.help_handler import help_handler
from bot.settings import load_texts


@pytest.mark.asyncio
async def test_help_handler():
    telegram_user = TelegramUser(id=1, first_name="Test", is_bot=False, language_code="ru")

    message = MagicMock(spec=Message)
    message.from_user = telegram_user
    message.text = "/help"
    update = MagicMock(spec=Update)
    update.effective_user = telegram_user
    update.message = message

    context = MagicMock()

    await help_handler(update, context)
    texts = load_texts(telegram_user.language_code)["texts"]
    message.reply_text.assert_called_once_with(texts["help"])
