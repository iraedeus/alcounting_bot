import pytest
from unittest.mock import MagicMock
from telegram import InlineKeyboardMarkup
from bot.roles.customer import Customer


@pytest.fixture
def customer():
    db = MagicMock()
    tg_user_id = 12345
    texts = {
        "menu": "Menu",
        "show_products_text": "Show Products",
        "make_order_text": "Make Order",
        "show_orders_text": "Show Orders",
        "show_orders_button": "Show Orders",
        "show_products_button": "Show Products",
        "make_order_button": "Make Order",
        "approve_orders_button": "Approve Order",
        "back_to_menu_button": "Back to Menu",
        "back_button": "Back",
    }
    return Customer(db, tg_user_id, texts)


def test_build_menu(customer):
    menu = customer.build_menu()
    assert isinstance(menu, InlineKeyboardMarkup)


def test_handle_show_products_button(customer):
    text, markup = customer._Customer__handle_show_products_button()
    assert text == "Show Products"
    assert isinstance(markup, InlineKeyboardMarkup)


def test_handle_make_order_button(customer):
    text, markup = customer._Customer__handle_make_order_button()
    assert text == "Make Order"
    assert isinstance(markup, InlineKeyboardMarkup)


def test_handle_show_orders_button(customer):
    text, markup = customer._Customer__handle_show_orders_button()
    assert text == "Show Orders"
    assert isinstance(markup, InlineKeyboardMarkup)


def test_handle_product_info(customer):
    product = MagicMock()
    product.name = "Product1"
    product.description = "Description1"
    product.price = 100
    customer.db.get_product_by_name.return_value = product

    text, markup = customer._Customer__handle_product_info("shown_Product1")
    assert "<b>Product1</b>" in text
    assert "Description1" in text
    assert "100руб." in text
    assert isinstance(markup, InlineKeyboardMarkup)


def test_handle_pre_approve_order(customer):
    text, markup = customer._Customer__handle_pre_approve_order("chose_Product1")
    assert "Product1" in text
    assert isinstance(markup, InlineKeyboardMarkup)


def test_handle_approve_order(customer):
    order = MagicMock()
    order.date = "2023-10-10 10:10:10"
    order.product = "Product1"
    order.customer_id = customer.tg_user_id
    order.status = "размещён"
    customer.db.get_order_by_date.return_value = order

    text, markup = customer._Customer__handle_approve_order("nextchose_Product1")
    assert "Заказ оформлен!" in text
    assert "Product1" in text
    assert isinstance(markup, InlineKeyboardMarkup)


def test_handle_order_info(customer):
    order = MagicMock()
    order.date = "2023-10-10 10:10:10"
    order.product = "Product1"
    order.barman_id = 54321
    order.status = "размещён"
    barman = MagicMock()
    barman.name = "Barman1"
    customer.db.get_order_by_date.return_value = order
    customer.db.get_user_by_id.return_value = barman

    text, markup = customer._Customer__handle_order_info("2023-10-10 10:10:10")
    assert "Product1" in text
    assert "Barman1" in text
    assert isinstance(markup, InlineKeyboardMarkup)


def test_back_to_menu(customer):
    text, markup = customer._Customer__back_to_menu()
    assert text == "Menu"
    assert isinstance(markup, InlineKeyboardMarkup)


def test_on_button_tap_top_menu(customer):
    customer.db.get_all_orders.return_value = []
    text, markup = customer.on_button_tap("Show Products")
    assert text == "Show Products"
    assert isinstance(markup, InlineKeyboardMarkup)


def test_on_button_tap_invalid(customer):
    customer.db.get_all_orders.return_value = []
    text, markup = customer.on_button_tap("Invalid Button")
    assert text == "Err0r"
    assert isinstance(markup, InlineKeyboardMarkup)
