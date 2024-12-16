"""Module for the User class."""

from dataclasses import dataclass


@dataclass
class User:
    """Class representing a user in the system."""

    id: int
    name: str
    type: str

    def __post_init__(self):
        """Post-initialization processing."""
        if self.type not in ["barman", "admin", "customer"]:
            raise ValueError("Invalid user type")

    def get_user_name(self) -> str:
        """Get the name of the user."""
        return self.name

    def get_user_id(self) -> int:
        """Get the ID of the user."""
        return self.id

    def get_user_type(self) -> str:
        """Get the type of the user."""
        return self.type

    def set_user_name(self, name: str) -> None:
        """Set the name of the user."""
        self.name = name

    def set_user_id(self, user_id: int) -> None:
        """Set the ID of the user."""
        self.id = user_id

    def set_user_type(self, user_type: str) -> None:
        """Set the type of the user."""
        if user_type not in ["barman", "admin", "customer"]:
            raise ValueError("Invalid user type")
        self.type = user_type
