"""
This module contains the Barman class which represents a barman interacting with the bot.
"""

import logging
from telegram import InlineKeyboardMarkup, InlineKeyboardButton
from bot.roles.customer import Customer
from bot.database import Database


class Barman(Customer):
    """
    A class to represent a barman interacting with the bot.
    """

    def __init__(self, db: Database, tg_user_id: int, texts: dict):
        super().__init__(db, tg_user_id, texts)
        self.texts = texts

    def create_barman_buttons_menu(self):
        """Create buttons for the barman menu"""
        buttons = self.create_customer_menu_buttons()
        buttons.append(
            [
                InlineKeyboardButton(
                    self.texts["queue_button"], callback_data=self.texts["queue_button"]
                )
            ]
        )
        return buttons

    def build_menu(self) -> InlineKeyboardMarkup:
        return InlineKeyboardMarkup(self.create_barman_buttons_menu())

    def __build_queue_menu(self) -> InlineKeyboardMarkup:
        my_orders = self.db.get_orders_queue()
        buttons = []
        if my_orders is not None:
            for my_order in my_orders:
                button = InlineKeyboardButton(
                    f"{my_order.date[:-7]} {my_order.product}",
                    callback_data=f"complete_{my_order.date}",
                )
                buttons.append([button])
        else:
            logging.getLogger(__name__).info("No orders in database")
        buttons.append(self.back_to_menu_button)
        return InlineKeyboardMarkup(buttons)

    def __build_pre_complete_order_menu(self, data) -> InlineKeyboardMarkup:
        buttons = [
            [
                InlineKeyboardButton(
                    self.texts["complete_button"], callback_data="c" + data
                )
            ],
            [
                InlineKeyboardButton(
                    self.texts["back_button"], callback_data=self.texts["queue_button"]
                )
            ],
            self.back_to_menu_button,
        ]
        return InlineKeyboardMarkup(buttons)

    def __build_complete_order_menu(self) -> InlineKeyboardMarkup:
        buttons = [
            [
                InlineKeyboardButton(
                    self.texts["back_button"], callback_data=self.texts["queue_button"]
                )
            ],
            self.back_to_menu_button,
        ]
        return InlineKeyboardMarkup(buttons)

    def __handle_queue_button(self):
        logging.getLogger(__name__).info(
            "%s press the QUEUE_BUTTON or return to the QUEUE menu", self.tg_user_id
        )
        text = self.texts["queue_text"]
        markup = self.__build_queue_menu()
        return text, markup

    def __pre_complete_order_text(self, order, customer_name, barman_name):
        return (
            self.texts["order_time"]
            + str(order.date[:-7])
            + self.texts["order_product"]
            + str(order.product)
            + self.texts["order_customer"]
            + str(customer_name)
            + self.texts["order_barman"]
            + str(barman_name)
            + self.texts["order_status"]
            + str(order.status)
        )

    def __handle_pre_complete_order(self, data):
        logging.getLogger(__name__).info("%s watch for the %s", self.tg_user_id, data)
        order = self.db.get_order_by_date(data[9:])
        customer_name = self.db.get_user_by_id(order.customer_id).name
        barman = self.db.get_user_by_id(order.barman_id)
        text = self.__pre_complete_order_text(order, customer_name, barman)
        markup = self.__build_pre_complete_order_menu(data)
        return text, markup

    def __handle_complete_order(self, data):
        logging.getLogger(__name__).info("%s approved the %s", self.tg_user_id, data)
        order = self.db.get_order_by_date(data[10:])
        order.set_order_barman_id(self.tg_user_id)
        order.set_order_status("завершён")
        self.db.update_order(order)
        customer_name = self.db.get_user_by_id(order.customer_id).name
        barman_name = self.db.get_user_by_id(order.barman_id).name
        text = self.texts["order_completed"] + self.__pre_complete_order_text(
            order, customer_name, barman_name
        )
        markup = self.__build_complete_order_menu()
        return text, markup

    def on_button_tap(self, data) -> (str, InlineKeyboardMarkup):
        text, markup = super().on_button_tap(data)

        if text != "Err0r":
            return text, markup
        if data == self.texts["queue_button"]:
            return self.__handle_queue_button()
        if data.startswith("complete_"):
            return self.__handle_pre_complete_order(data)
        if data.startswith("ccomplete_"):
            return self.__handle_complete_order(data)
        return "Err0r", self.build_menu()
