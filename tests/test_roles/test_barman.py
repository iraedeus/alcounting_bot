import pytest
from unittest.mock import MagicMock
from telegram import InlineKeyboardMarkup
from bot.roles.barman import Barman


@pytest.fixture
def barman():
    db = MagicMock()
    tg_user_id = 12345
    texts = {
        "queue_button": "Queue",
        "queue_text": "Queue Text",
        "complete_button": "Complete",
        "back_to_menu_button": "Back to Menu",
        "back_button": "Back",
        "menu": "Menu",
        "show_products_button": "Show Products",
        "make_order_button": "Make Order",
        "show_orders_button": "Show Orders",
    }
    return Barman(db, tg_user_id, texts)


def test_build_menu(barman):
    menu = barman.build_menu()
    assert isinstance(menu, InlineKeyboardMarkup)


def test_handle_queue_button(barman):
    barman.db.get_orders_queue.return_value = []
    text, markup = barman._Barman__handle_queue_button()
    assert text == "Queue Text"
    assert isinstance(markup, InlineKeyboardMarkup)


def test_handle_pre_complete_order(barman):
    order = MagicMock()
    order.date = "2023-10-10 10:10:10"
    order.product = "Product1"
    order.customer_id = 12345
    order.barman_id = 54321
    order.status = "размещён"
    barman.db.get_order_by_date.return_value = order

    text, markup = barman._Barman__handle_pre_complete_order(
        "complete_2023-10-10 10:10:10"
    )
    assert "Product1" in text
    # assert "2023-10-10 10:10" in text
    assert isinstance(markup, InlineKeyboardMarkup)


def test_handle_complete_order(barman):
    order = MagicMock()
    order.date = "2023-10-10 10:10:10"
    order.product = "Product1"
    order.customer_id = 12345
    order.barman_id = 54321
    order.status = "размещён"
    barman.db.get_order_by_date.return_value = order

    text, markup = barman._Barman__handle_complete_order(
        "ccomplete_2023-10-10 10:10:10"
    )
    assert "Заказ завершён!!!" in text
    assert "Product1" in text
    assert isinstance(markup, InlineKeyboardMarkup)


def test_on_button_tap_queue(barman):
    barman.db.get_orders_queue.return_value = []
    text, markup = barman.on_button_tap("Queue")
    assert text == "Queue Text"
    assert isinstance(markup, InlineKeyboardMarkup)


def test_on_button_tap_complete(barman):
    order = MagicMock()
    order.date = "2023-10-10 10:10:10"
    order.product = "Product1"
    order.customer_id = 12345
    order.barman_id = 54321
    order.status = "размещён"
    barman.db.get_order_by_date.return_value = order

    text, markup = barman.on_button_tap("complete_2023-10-10 10:10:10")
    assert "Product1" in text
    assert isinstance(markup, InlineKeyboardMarkup)
