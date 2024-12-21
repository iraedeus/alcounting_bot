import pytest
from bot.database.user import User


@pytest.fixture
def user():
    return User(id=1, name="Test User", type="customer")


def test_create_user(user):
    assert user.get_user_id() == 1
    assert user.get_user_name() == "Test User"
    assert user.get_user_type() == "customer"


def test_update_user_name(user):
    user.set_user_name("Updated User")
    assert user.get_user_name() == "Updated User"


def test_update_user_id(user):
    user.set_user_id(2)
    assert user.get_user_id() == 2


def test_update_user_type(user):
    user.set_user_type("admin")
    assert user.get_user_type() == "admin"


def test_invalid_user_type():
    with pytest.raises(ValueError):
        User(id=1, name="Invalid User", type="invalid")


def test_set_invalid_user_type(user):
    with pytest.raises(ValueError):
        user.set_user_type("invalid")
