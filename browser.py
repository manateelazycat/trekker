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

class Browser(object):
    def __init__(self, args):
        (buffer_id, url) = args

        self.buffer_id = buffer_id
        self.url = url

        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        print("***** ", buffer_id, url)

        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
                message = self.event_queue.get(True)
                print("**** ", message)
                self.event_queue.task_done()
        except:
            print(traceback.format_exc())

if __name__ == "__main__":
    Browser(sys.argv[1:])
