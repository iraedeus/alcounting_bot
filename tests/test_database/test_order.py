import pytest
from bot.database.order import Order


@pytest.fixture
def order():
    return Order(
        date="2023-01-01",
        product="Coffee",
        customer_id=1,
        barman_id=2,
        status="pending",
    )


def test_get_order_date(order):
    assert order.get_order_date() == "2023-01-01"


def test_get_order_product(order):
    assert order.get_order_product() == "Coffee"


def test_get_order_customer_id(order):
    assert order.get_order_customer_id() == 1


def test_get_order_barman_id(order):
    assert order.get_order_barman_id() == 2


def test_get_order_status(order):
    assert order.get_order_status() == "pending"


def test_set_order_date(order):
    order.set_order_date("2023-02-01")
    assert order.get_order_date() == "2023-02-01"


def test_set_order_product(order):
    order.set_order_product("Tea")
    assert order.get_order_product() == "Tea"


def test_set_order_customer_id(order):
    order.set_order_customer_id(3)
    assert order.get_order_customer_id() == 3


def test_set_order_barman_id(order):
    order.set_order_barman_id(4)
    assert order.get_order_barman_id() == 4


def test_set_order_status(order):
    order.set_order_status("completed")
    assert order.get_order_status() == "completed"
