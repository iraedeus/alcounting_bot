"""Module for the Product class."""

from dataclasses import dataclass


@dataclass
class Product:
    """Class representing a product in the system."""

    name: str
    description: str
    price: float

    def get_name(self) -> str:
        """Get the name of the product."""
        return self.name

    def get_price(self) -> float:
        """Get the price of the product."""
        return self.price

    def get_description(self) -> str:
        """Get the description of the product."""
        return self.description

    def set_name(self, val: str) -> None:
        """Set the name of the product."""
        self.name = val

    def set_price(self, val: float) -> None:
        """Set the price of the product."""
        self.price = val

    def set_description(self, val: str) -> None:
        """Set the description of the product."""
        self.description = val
