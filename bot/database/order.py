"""Module for the Order class."""

from dataclasses import dataclass


@dataclass
class Order:
    """Class representing an order in the system."""

    date: str
    product: str
    customer_id: int
    barman_id: int
    status: str

    def get_order_date(self) -> str:
        """Get the date of the order."""
        return self.date

    def get_order_product(self) -> str:
        """Get the product of the order."""
        return self.product

    def get_order_customer_id(self) -> int:
        """Get the customer ID of the order."""
        return self.customer_id

    def get_order_barman_id(self) -> int:
        """Get the barman ID of the order."""
        return self.barman_id

    def get_order_status(self) -> str:
        """Get the status of the order."""
        return self.status

    def set_order_date(self, date: str):
        """Set the date of the order."""
        self.date = date

    def set_order_product(self, product: str):
        """Set the product of the order."""
        self.product = product

    def set_order_customer_id(self, customer_id: int):
        """Set the customer ID of the order."""
        self.customer_id = customer_id

    def set_order_barman_id(self, barman_id: int):
        """Set the barman ID of the order."""
        self.barman_id = barman_id

    def set_order_status(self, status: str):
        """Set the status of the order."""
        self.status = status
