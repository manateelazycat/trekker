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

class Browser(object):
    def __init__(self, args):
        (buffer_id, url) = args

        self.buffer_id = buffer_id
        self.url = url

        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
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

if __name__ == "__main__":
    Browser(sys.argv[1:])
