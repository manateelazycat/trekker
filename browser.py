#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2024 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: <lazycat.manatee@gmail.com> <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import queue
import threading
import traceback
import sys
import json
import abc

from core.utils import *
from PyQt6.QtCore import pyqtSignal
from PyQt6.QtWidgets import QGraphicsScene

from utils import parse_json_content

webengine_profile = QWebEngineProfile('trekker')
webengine_profile.setPersistentCookiesPolicy(QWebEngineProfile.PersistentCookiesPolicy.ForcePersistentCookies)
webengine_profile.setHttpCacheType(QWebEngineProfile.HttpCacheType.DiskHttpCache)

class Browser(object):
    def __init__(self, args):
        (buffer_id, url) = args

        self.buffer_id = buffer_id
        self.url = url

        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        self.communication_thread = threading.Thread(target=self.communicate_with_main_process)
        self.communication_thread.start()

        self.eval_in_emacs("message", "hello from trekker subprocess")

        self.event_loop.join()

    def communicate_with_main_process(self):
        while True:
            try:
                data = sys.stdin.readline().strip()
                message = parse_json_content(data)
                self.print_log("Got message: {}".format(message))
            except:
                self.print_log(traceback.format_exc())

    def event_dispatcher(self):
        while True:
            try:
                message = self.event_queue.get(True)
                self.print_log("**** ", message)
                self.event_queue.task_done()
            except:
                self.print_log(traceback.format_exc())

    def send_message_to_main_process(self, message_type, message):
        print(json.dumps({
            "buffer_id": self.buffer_id,
            "type": message_type,
            "content": message
        }))
        sys.stdout.flush()

    def print_log(self, log):
        self.send_message_to_main_process("log", log)

    def message_emacs(self, message):
        self.send_message_to_main_process("message", message)

    def eval_in_emacs(self, method_name, *args):
        message = {
            "method_name": method_name,
            "args": list(args)
        }
        self.send_message_to_main_process("eval_in_emacs", message)

class BrowserBuffer(QGraphicsScene):
    __metaclass__ = abc.ABCMeta

    aspect_ratio_change = pyqtSignal()
    enter_fullscreen_request = pyqtSignal()
    exit_fullscreen_request = pyqtSignal()

    def __init__(self, buffer_id, url):
        super(QGraphicsScene, self).__init__()

        self.buffer_id = buffer_id
        self.url = url

        self.profile = webengine_profile

class BrowserView(QWidget):

    def __init__(self, buffer, view_info):
        super(View, self).__init__()

        self.buffer = buffer


class BrowserWidget(QWebEngineView):

    translate_selected_text = QtCore.pyqtSignal(str)

    def __init__(self, profile, buffer_id):
        super(QWebEngineView, self).__init__(profile)

        self.installEventFilter(self)
        self.buffer_id = buffer_id
        self.is_button_press = False

        self.web_page = BrowserPage(profile)
        self.setPage(self.web_page)

class BrowserPage(QWebEnginePage):
    def __init__(self, profile):
        QWebEnginePage.__init__(self, profile)

    def execute_javascript(self, script_src):
        ''' Execute JavaScript.'''
        try:
            if hasattr(self, "loop") and self.loop.isRunning():
                # NOTE:
                #
                # Just return None is QEventLoop is busy, such as press 'j' key not release on webpage.
                # Otherwise will got error 'RecursionError: maximum recursion depth exceeded while calling a Python object'.
                #
                # And don't warry, API 'execute_javascript' is works well for programming purpse since we just call this interface occasionally.
                return None
            else:
                # Build event loop.
                self.loop = QEventLoop()

                # Run JavaScript code.
                self.runJavaScript(script_src, self.callback_js)

                # Execute event loop, and wait event loop quit.
                self.loop.exec()

                # Return JavaScript function result.
                return self.result
        except:
            import traceback
            traceback.print_exc()

            return None

    def callback_js(self, result):
        ''' Callback of JavaScript, call loop.quit to jump code after loop.exec.'''
        self.result = result
        self.loop.quit()

    def javaScriptConsoleMessage(self, level, message, line_number, source_id):
        # Only print JavaScript console message for EAF application.
        # don't print any console message from browser, print too much message will slow down Emacs.
        if self.url().toString() == "file:///":
            print("[JavaScript console]: " + message)

if __name__ == "__main__":
    Browser(sys.argv[1:])
