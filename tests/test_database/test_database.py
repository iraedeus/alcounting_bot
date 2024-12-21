import pytest
import os

from bot.database.database import Database
from bot.database.user import User
from bot.database.product import Product
from bot.database.order import Order


@pytest.fixture
def db_fixture():
    return Database(":memory:")


def test_fix_path_creates_directory_and_file(tmp_path, db_fixture):
    test_path = tmp_path / "test_dir" / "test_file.db"
    db_fixture.fix_path(str(test_path))
    assert os.path.exists(test_path)
    assert os.path.isfile(test_path)


def test_insert_user(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    db_fixture.insert_user(user)
    assert db_fixture.get_user_by_id(1) == user


def test_insert_product(db_fixture):
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    db_fixture.insert_product(product)
    retrieved_product = db_fixture.get_product_by_name("Test Product")
    assert retrieved_product.name == product.name
    assert retrieved_product.description == product.description
    assert retrieved_product.price == product.price


def test_insert_order(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    order = Order(
        date="2023-01-01T00:00:00",
        product="Test Product",
        customer_id=1,
        barman_id=2,
        status="placed",
    )
    db_fixture.insert_user(user)
    db_fixture.insert_product(product)
    db_fixture.insert_order(order)
    assert db_fixture.get_order_by_date("2023-01-01T00:00:00") is not None


def test_delete_user(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    db_fixture.insert_user(user)
    db_fixture.delete_user(user)
    assert db_fixture.get_user_by_id(1) is None


def test_delete_product(db_fixture):
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    db_fixture.insert_product(product)
    db_fixture.delete_product(product)
    assert db_fixture.get_product_by_name("Test Product") is None


def test_delete_order(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    order = Order(
        date="2023-01-02T00:00:00",
        product="Test Product",
        customer_id=1,
        barman_id=2,
        status="placed",
    )
    db_fixture.insert_user(user)
    db_fixture.insert_product(product)
    db_fixture.insert_order(order)
    db_fixture.delete_order(order)
    assert db_fixture.get_order_by_date("2023-01-02T00:00:00") is None


def test_update_order(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    order = Order(
        date="2023-01-03T00:00:00",
        product="Test Product",
        customer_id=1,
        barman_id=2,
        status="placed",
    )
    db_fixture.insert_user(user)
    db_fixture.insert_product(product)
    db_fixture.insert_order(order)
    order.status = "completed"
    db_fixture.update_order(order)
    updated_order = db_fixture.get_order_by_date("2023-01-03T00:00:00")
    assert updated_order.status == "completed"


def test_insert_user_with_invalid_data(db_fixture):
    with pytest.raises(ValueError):
        db_fixture.insert_user(None)


def test_insert_product_with_invalid_data(db_fixture):
    with pytest.raises(ValueError):
        db_fixture.insert_product(None)


def test_insert_order_with_invalid_data(db_fixture):
    with pytest.raises(ValueError):
        db_fixture.insert_order(None)


def test_update_user_with_invalid_data(db_fixture):
    with pytest.raises(ValueError):
        db_fixture.update_user(None)


def test_update_product_with_invalid_data(db_fixture):
    with pytest.raises(ValueError):
        db_fixture.update_product(None)


def test_update_order_with_invalid_data(db_fixture):
    with pytest.raises(ValueError):
        db_fixture.update_order(None)


def test_delete_user_with_invalid_data(db_fixture):
    with pytest.raises(ValueError):
        db_fixture.delete_user(None)


def test_delete_product_with_invalid_data(db_fixture):
    with pytest.raises(ValueError):
        db_fixture.delete_product(None)


def test_delete_order_with_invalid_data(db_fixture):
    with pytest.raises(ValueError):
        db_fixture.delete_order(None)


def test_get_all_products(db_fixture):
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    db_fixture.insert_product(product)
    products = db_fixture.get_all_products()
    assert len(products) == 1
    assert products[0].name == "Test Product"


def test_get_all_users(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    db_fixture.insert_user(user)
    users = db_fixture.get_all_users()
    assert len(users) == 1
    assert users[0].name == "Test User"


def test_get_all_orders(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    order = Order(
        date="2023-01-01T00:00:00",
        product="Test Product",
        customer_id=1,
        barman_id=2,
        status="placed",
    )
    db_fixture.insert_user(user)
    db_fixture.insert_product(product)
    db_fixture.insert_order(order)
    orders = db_fixture.get_all_orders()
    assert len(orders) == 1
    assert orders[0].date == "2023-01-01T00:00:00"


def test_get_user_by_id(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    db_fixture.insert_user(user)
    retrieved_user = db_fixture.get_user_by_id(1)
    assert retrieved_user is not None
    assert retrieved_user.name == "Test User"


def test_get_product_by_name(db_fixture):
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    db_fixture.insert_product(product)
    retrieved_product = db_fixture.get_product_by_name("Test Product")
    assert retrieved_product is not None
    assert retrieved_product.name == "Test Product"


def test_get_order_by_date(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    order = Order(
        date="2023-01-01T00:00:00",
        product="Test Product",
        customer_id=1,
        barman_id=2,
        status="placed",
    )
    db_fixture.insert_user(user)
    db_fixture.insert_product(product)
    db_fixture.insert_order(order)
    retrieved_order = db_fixture.get_order_by_date("2023-01-01T00:00:00")
    assert retrieved_order is not None
    assert retrieved_order.date == "2023-01-01T00:00:00"


def test_get_orders_by_customer_id(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    order = Order(
        date="2023-01-01T00:00:00",
        product="Test Product",
        customer_id=1,
        barman_id=2,
        status="placed",
    )
    db_fixture.insert_user(user)
    db_fixture.insert_product(product)
    db_fixture.insert_order(order)
    orders = db_fixture.get_orders_by_customer_id(1)
    assert len(orders) == 1
    assert orders[0].customer_id == 1


def test_get_orders_queue(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    order = Order(
        date="2023-01-01T00:00:00",
        product="Test Product",
        customer_id=1,
        barman_id=2,
        status="размещён",
    )
    db_fixture.insert_user(user)
    db_fixture.insert_product(product)
    db_fixture.insert_order(order)
    orders = db_fixture.get_orders_queue()
    assert len(orders) == 1
    assert orders[0].status == "размещён"


def test_update_user_with_valid_data(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    db_fixture.insert_user(user)
    user.name = "Updated User"
    db_fixture.update_user(user)
    updated_user = db_fixture.get_user_by_id(1)
    assert updated_user.name == "Updated User"


def test_update_product_with_valid_data(db_fixture):
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    db_fixture.insert_product(product)
    product.description = "Updated description"
    db_fixture.update_product(product)
    updated_product = db_fixture.get_product_by_name("Test Product")
    assert updated_product.description == "Updated description"


def test_update_order_with_valid_data(db_fixture):
    user = User(id=1, name="Test User", type="customer")
    product = Product(
        name="Test Product", description="A product for testing", price=100
    )
    order = Order(
        date="2023-01-01T00:00:00",
        product="Test Product",
        customer_id=1,
        barman_id=2,
        status="placed",
    )
    db_fixture.insert_user(user)
    db_fixture.insert_product(product)
    db_fixture.insert_order(order)
    order.status = "completed"
    db_fixture.update_order(order)
    updated_order = db_fixture.get_order_by_date("2023-01-01T00:00:00")
    assert updated_order.status == "completed"
