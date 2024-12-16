import pytest
from unittest.mock import MagicMock
from telegram.ext import Application

@pytest.fixture
def mock_application():
    return MagicMock(spec=Application)
