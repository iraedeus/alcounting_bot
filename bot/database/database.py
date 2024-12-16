"""Module for working with the database"""

import logging
import os
import sqlite3

from bot.database.order import Order
from bot.database.product import Product
from bot.database.user import User

DATABASE_DEFAULT_PATH = "data/database.db"


class Database:
    """Class for working with sqlite3 database"""

    def __init__(self, path: str = DATABASE_DEFAULT_PATH):
        self.fix_path(path)

        self.conn = sqlite3.connect(path)
        self.cur = self.conn.cursor()

        self.cur.execute(
            """
            CREATE TABLE IF NOT EXISTS Products (
                name TEXT NOT NULL PRIMARY KEY,
                description TEXT NOT NULL,
                price INTEGER
            );
        """
        )
        self.cur.execute(
            """
            CREATE TABLE IF NOT EXISTS Users (
                id INTEGER PRIMARY KEY,
                name TEXT NOT NULL,
                type TEXT NOT NULL
            );
        """
        )
        self.cur.execute(
            """
            CREATE TABLE IF NOT EXISTS Orders (
                date TEXT NOT NULL PRIMARY KEY,
                product TEXT NOT NULL,
                customer_id INTEGER,
                barman_id INTEGER,
                status TEXT NOT NULL,
                FOREIGN KEY (product) REFERENCES Products(name),
                FOREIGN KEY (customer_id) REFERENCES Users(id),
                FOREIGN KEY (barman_id) REFERENCES Users(id)
            );
        """
        )
        self.conn.commit()
        logging.getLogger(__name__).debug("Database created")

    def __del__(self):
        self.conn.close()
        logging.getLogger(__name__).debug("Database closed")

    def fix_path(self, path: str) -> None:
        """Create a directory and file if they do not exist"""
        if not os.path.exists(path) and path != ":memory:":
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w", encoding="utf-8") as file:
                file.close()

    def insert_user(self, user: User) -> None:
        """Insert a new user into the Users table."""
        if user is None:
            raise ValueError("User cannot be None")
        self.cur.execute(
            """
            INSERT OR IGNORE INTO Users (id, name, type)
            VALUES (?, ?, ?)""",
            (user.id, user.name, user.type),
        )

    def insert_product(self, product: Product) -> None:
        """Insert a new product into the Products table."""
        if product is None:
            raise ValueError("Product cannot be None")
        self.cur.execute(
            """
            INSERT OR IGNORE INTO Products (name, description, price)
            VALUES (?, ?, ?)""",
            (product.name, product.description, product.price),
        )

    def insert_order(self, order: Order) -> None:
        """Insert a new order into the Orders table."""
        if order is None:
            raise ValueError("Order cannot be None")
        self.cur.execute(
            """
            INSERT INTO Orders (date, product, customer_id, barman_id, status)
            VALUES (?, ?, ?, ?, ?)""",
            (
                order.date,
                order.product,
                order.customer_id,
                order.barman_id,
                order.status,
            ),
        )

    def delete_user(self, user: User) -> None:
        """Delete a user from the Users table."""
        if user is None:
            raise ValueError("User cannot be None")
        self.cur.execute("DELETE FROM Users WHERE id = ?", (user.id,))

    def delete_product(self, product: Product) -> None:
        """Delete a product from the Products table."""
        if product is None:
            raise ValueError("Product cannot be None")
        self.cur.execute("DELETE FROM Products WHERE name = ?", (product.name,))

    def delete_order(self, order: Order) -> None:
        """Delete an order from the Orders table."""
        if order is None:
            raise ValueError("Order cannot be None")
        self.cur.execute("DELETE FROM Orders WHERE date = ?", (order.date,))

    def get_all_products(self) -> list[Product]:
        """Retrieve all products from the Products table."""
        self.cur.execute("SELECT * FROM Products")
        rows = self.cur.fetchall()
        return [Product(row[0], row[1], row[2]) for row in rows]

    def get_all_users(self) -> list[User]:
        """Retrieve all users from the Users table."""
        self.cur.execute("SELECT * FROM Users")
        rows = self.cur.fetchall()
        return [User(row[0], row[1], row[2]) for row in rows]

    def get_all_orders(self) -> list[Order]:
        """Retrieve all orders from the Orders table."""
        self.cur.execute("SELECT * FROM Orders")
        rows = self.cur.fetchall()
        return [Order(row[0], row[1], row[2], row[3], row[4]) for row in rows]

    def get_user_by_id(self, user_id: int) -> User:
        """Retrieve a user by their ID from the Users table."""
        self.cur.execute("SELECT * FROM Users WHERE id = ?", (user_id,))
        found = self.cur.fetchone()
        return User(found[0], found[1], found[2]) if found else None

    def get_product_by_name(self, name: str) -> Product:
        """Retrieve a product by its name from the Products table."""
        self.cur.execute("SELECT * FROM Products WHERE name = ?", (name,))
        found = self.cur.fetchone()
        return Product(found[0], found[1], found[2]) if found else None

    def get_order_by_date(self, order_date: str) -> Order:
        """Retrieve an order by its date from the Orders table."""
        self.cur.execute("SELECT * FROM Orders WHERE date = ?", (order_date,))
        found = self.cur.fetchone()
        return (
            Order(found[0], found[1], found[2], found[3], found[4]) if found else None
        )

    def get_orders_by_customer_id(self, customer_id: int) -> list[Order]:
        """Retrieve orders by customer ID from the Orders table."""
        self.cur.execute("SELECT * FROM Orders WHERE customer_id = ?", (customer_id,))
        rows = self.cur.fetchall()
        return [Order(row[0], row[1], row[2], row[3], row[4]) for row in rows]

    def get_orders_queue(self) -> list[Order]:
        """Retrieve orders with status 'размещён' from the Orders table."""
        self.cur.execute("SELECT * FROM Orders WHERE status = ?", ("размещён",))
        rows = self.cur.fetchall()
        return [Order(row[0], row[1], row[2], row[3], row[4]) for row in rows]

    def update_user(self, user: User) -> None:
        """Update a user in the Users table."""
        if user is None:
            raise ValueError("User cannot be None")
        self.cur.execute(
            """
            UPDATE Users SET name = ?, type = ? WHERE id = ?""",
            (user.name, user.type, user.id),
        )

    def update_product(self, product: Product) -> None:
        """Update a product in the Products table."""
        if product is None:
            raise ValueError("Product cannot be None")
        self.cur.execute(
            """
            UPDATE Products SET description = ?, price = ? WHERE name = ?""",
            (product.description, product.price, product.name),
        )

    def update_order(self, order: Order) -> None:
        """Update an order in the Orders table."""
        if order is None:
            raise ValueError("Order cannot be None")
        self.cur.execute(
            """
            UPDATE Orders SET barman_id = ?, status = ? WHERE date = ?""",
            (order.barman_id, order.status, order.date),
        )
