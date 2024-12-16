import pytest
from bot.database.product import Product


@pytest.fixture
def product():
    return Product(
        name="Test Product", description="A product for testing", price=100.0
    )


def test_create_product(product):
    assert product.get_name() == "Test Product"
    assert product.get_description() == "A product for testing"
    assert product.get_price() == 100.0


def test_update_product_name(product):
    product.set_name("Updated Product")
    assert product.get_name() == "Updated Product"


def test_update_product_description(product):
    product.set_description("An updated product description")
    assert product.get_description() == "An updated product description"


def test_update_product_price(product):
    product.set_price(150.0)
    assert product.get_price() == 150.0
