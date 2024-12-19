# # tests/test_callback_handler.py
#
# import pytest
# from unittest.mock import AsyncMock, MagicMock
# from telegram import Update, User, CallbackQuery
# from bot.handlers.callback_handler import callback_handler
# from bot.database import Database, User as UserDB
# from bot.roles import role_associations
# from bot.settings import load_texts
#
#
# @pytest.mark.asyncio
# async def test_callback_handler():
#     db = MagicMock(Database)
#     role_class = MagicMock()
#     role_obj = MagicMock()
#     role_associations["customer"] = role_class
#     role_class.return_value = role_obj
#
#     tg_user = User(id=123, first_name="Test", is_bot=False)
#     callback_query = MagicMock(CallbackQuery)
#     callback_query.data = "test_data"
#     callback_query.answer = AsyncMock()
#     callback_query.edit_message_text = AsyncMock()
#     update = MagicMock(Update)
#     update.effective_user = tg_user
#     update.callback_query = callback_query
#     db.insert_user(MagicMock(UserDB(123, "Test", "customer")))
#     db.get_user_by_id.return_value = MagicMock(UserDB(123, "Test", "customer"))
#     load_texts.return_value = {"texts": {}}
#
#     await callback_handler(update, MagicMock())
#
#     role_class.assert_called_once_with(db, tg_user.id, {})
#     role_obj.on_button_tap.assert_called_once_with("test_data")
#     callback_query.answer.assert_called_once()
#     callback_query.edit_message_text.assert_called_once()
