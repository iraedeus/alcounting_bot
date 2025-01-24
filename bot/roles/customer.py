"""
This module contains the Customer class which represents a customer interacting with the bot.
"""

import logging
import datetime
from telegram import InlineKeyboardMarkup, InlineKeyboardButton
from bot.database import Product, Order


class Customer:
    """
    A class to represent a customer interacting with the bot.
    """

    def __init__(self, db, tg_user_id, texts):
        """
        Initialize the Customer with database, Telegram user ID, and texts.
        """
        self.db = db
        self.tg_user_id = tg_user_id
        self.texts = texts
        self.back_to_menu_button = [
            InlineKeyboardButton(
                self.texts["back_to_menu_button"],
                callback_data=self.texts["back_to_menu_button"],
            )
        ]

    def create_customer_menu_buttons(self):
        """Create buttons for the customer menu."""
        return [
            [
                InlineKeyboardButton(
                    self.texts["show_products_button"],
                    callback_data=self.texts["show_products_button"],
                )
            ],
            [
                InlineKeyboardButton(
                    self.texts["make_order_button"],
                    callback_data=self.texts["make_order_button"],
                )
            ],
            [
                InlineKeyboardButton(
                    self.texts["show_orders_button"],
                    callback_data=self.texts["show_orders_button"],
                )
            ],
        ]

    def build_menu(self) -> InlineKeyboardMarkup:
        """
        Build the customer menu with inline buttons.
        """
        buttons = self.create_customer_menu_buttons()
        return InlineKeyboardMarkup(buttons)

    def __build_products_menu(self, prefix) -> InlineKeyboardMarkup:
        """
        Build the menu to show products to make an order or show product information.
        """
        my_products = self.db.get_all_products()
        buttons = []
        if my_products is not None:
            for product in my_products:
                button = InlineKeyboardButton(
                    product.name, callback_data=prefix + product.name
                )
                buttons.append([button])
        else:
            logging.getLogger(__name__).info("No products in database")
        buttons.append(self.back_to_menu_button)
        return InlineKeyboardMarkup(buttons)

    def __build_show_product_info_menu(self, data) -> InlineKeyboardMarkup:
        """
        Build the menu to show product information.
        """
        buttons = [
            [InlineKeyboardButton("Сделать заказ", callback_data="chose_" + data[6:])],
            [
                InlineKeyboardButton(
                    self.texts["back_button"],
                    callback_data=self.texts["show_products_button"],
                )
            ],
            self.back_to_menu_button,
        ]
        return InlineKeyboardMarkup(buttons)

    def __back_to_menu(self):
        """
        Return to the main menu.
        """
        return self.texts["menu"], self.build_menu()

    def __build_pre_approve_order_menu(self, data) -> InlineKeyboardMarkup:
        """
        Build the menu to pre-approve an order.
        """
        buttons = [
            [
                InlineKeyboardButton(
                    self.texts["approve_orders_button"], callback_data=data
                )
            ],
            [
                InlineKeyboardButton(
                    self.texts["back_button"],
                    callback_data=self.texts["make_order_button"],
                )
            ],
            self.back_to_menu_button,
        ]
        return InlineKeyboardMarkup(buttons)

    def __build_approve_order_menu(self, pre_data) -> InlineKeyboardMarkup:
        """
        Build the menu to approve an order.
        """
        buttons = [
            [InlineKeyboardButton(self.texts["back_button"], callback_data=pre_data)],
            self.back_to_menu_button,
        ]
        return InlineKeyboardMarkup(buttons)

    def __build_show_orders_menu(self) -> InlineKeyboardMarkup:
        """
        Build the menu to show orders.
        """
        my_orders = self.db.get_orders_by_customer_id(self.tg_user_id)
        buttons = []
        if my_orders is not None:
            for my_order in my_orders:
                button = InlineKeyboardButton(
                    my_order.date[:-7], callback_data=my_order.date
                )
                buttons.append([button])
        else:
            logging.getLogger(__name__).info(
                "No orders found for user %s", self.tg_user_id
            )
        buttons.append(self.back_to_menu_button)
        return InlineKeyboardMarkup(buttons)

    def __build_show_order_info_menu(self) -> InlineKeyboardMarkup:
        """
        Build the menu to show order information.
        """
        buttons = [
            [
                InlineKeyboardButton(
                    self.texts["back_button"],
                    callback_data=self.texts["show_orders_button"],
                )
            ],
            self.back_to_menu_button,
        ]
        return InlineKeyboardMarkup(buttons)

    def __handle_show_products_button(self):
        """
        Handle the show products button press.
        """
        logging.getLogger(__name__).info(
            "%s press the SHOW_PRODUCTS_BUTTON or return to SHOW_PRODUCTS menu",
            self.tg_user_id,
        )
        text = self.texts["show_products_text"]
        markup = self.__build_products_menu("shown_")
        return text, markup

    def __handle_make_order_button(self):
        """
        Handle the make order button press.
        """
        logging.getLogger(__name__).info(
            "%s press the MAKE_ORDER_BUTTON or return to MAKE_ORDER menu",
            self.tg_user_id,
        )
        text = self.texts["make_order_text"]
        markup = self.__build_products_menu("chose_")
        return text, markup

    def __handle_show_orders_button(self):
        """
        Handle the show orders button press.
        """
        logging.getLogger(__name__).info(
            "%s press the SHOW_ORDERS_BUTTON or return to SHOW_ORDERS menu",
            self.tg_user_id,
        )
        text = self.texts["show_orders_text"]
        markup = self.__build_show_orders_menu()
        return text, markup

    def __handle_product_info(self, data):
        """
        Handle the product info button press.
        """
        logging.getLogger(__name__).info(
            "%s watch for the %s", self.tg_user_id, data[6:]
        )
        product: Product = self.db.get_product_by_name(data[6:])
        text = (
            f"<b>{product.name}</b>"
            f"{self.texts['product_description']} {product.description}"
            f"{self.texts['product_price']} {product.price}"
        )
        markup = self.__build_show_product_info_menu(data)
        return text, markup

    def __handle_pre_approve_order(self, data):
        """
        Handle the pre-approve order button press.
        """
        logging.getLogger(__name__).info("%s chosen the %s", self.tg_user_id, data)
        text = (
            f"{self.texts['chosen_product']} <b>{data[6:]}</b>"
            f'{self.texts["confirm_order"]}'
        )
        markup = self.__build_pre_approve_order_menu(f"next{data}")
        return text, markup

    def __handle_approve_order(self, data):
        """
        Handle the approve order button press.
        """
        logging.getLogger(__name__).info("%s approved the %s", self.tg_user_id, data)
        order = Order(
            str(datetime.datetime.now()), data[10:], self.tg_user_id, None, "размещён"
        )
        self.db.insert_order(order)
        text = (
            f'{self.texts["order_created"]}'
            f'{self.texts["order_time"]} {order.date[:-7]}'
            f'{self.texts["order_product"]} \n{order.product}'
            f'{self.texts["order_status"]} {order.status}'
        )
        markup = self.__build_approve_order_menu(data[4:])
        return text, markup

    def __handle_order_info(self, data):
        """
        Handle the order info button press.
        """
        logging.getLogger(__name__).info("%s watch for the %s", self.tg_user_id, data)
        order = self.db.get_order_by_date(data)
        barman = self.db.get_user_by_id(order.barman_id)
        text = (
            f'{self.texts["order_date"]} {order.date[:-7]}'
            f'{self.texts["order_product"]} {order.product}'
            f'{self.texts["order_barman"]} {barman if barman is None else barman.name}'
            f'{self.texts["order_status"]} {order.status}'
        )
        markup = self.__build_show_order_info_menu()
        return text, markup

    def on_button_tap(self, data) -> (str, InlineKeyboardMarkup):
        """
        Handle button tap events.
        """
        orders_dates = [order.date for order in self.db.get_all_orders()]

        top_menus_associations = {
            self.texts["show_products_button"]: self.__handle_show_products_button,
            self.texts["make_order_button"]: self.__handle_make_order_button,
            self.texts["show_orders_button"]: self.__handle_show_orders_button,
            self.texts["back_to_menu_button"]: self.__back_to_menu,
        }

        for callback_prefix, action in [
            ("shown_", self.__handle_product_info),
            ("chose_", self.__handle_pre_approve_order),
            ("nextchose_", self.__handle_approve_order),
        ]:
            if data.startswith(callback_prefix):
                return action(data)

        if data in orders_dates:
            return self.__handle_order_info(data)

        if data in top_menus_associations:
            return top_menus_associations[data]()

        logging.getLogger(__name__).error("Incorrect button pressed")
        return self.texts["error"], self.build_menu()
