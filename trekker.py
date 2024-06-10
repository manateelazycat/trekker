#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
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
import sys
import threading
import traceback
import subprocess
import os
import datetime
import json

from epc.server import ThreadingEPCServer
from utils import *


class Trekker:
    def __init__(self, args):
        global emacs_width, emacs_height, proxy_string

        # Parse init arguments.
        (emacs_width, emacs_height, emacs_server_port) = args
        emacs_width = int(emacs_width)
        emacs_height = int(emacs_height)

        # Init variables.
        self.views_data = None
        self.buffer_dict = {}
        self.view_dict = {}

        self.browser_subprocess_dict = {}
        self.browser_subprocess_communication_threads = {}

        # Init EPC client port.
        init_epc_client(int(emacs_server_port))

        # Build EPC server.
        self.server = ThreadingEPCServer(('127.0.0.1', 0), log_traceback=True)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.join(trekker_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)
        # self.server.logger = logger

        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()
        
        # All Emacs request running in event_loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        # Pass epc port and webengine codec information to Emacs when first start trekker.
        eval_in_emacs('trekker--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def event_dispatcher(self):
        while True:
            try:
                message = self.event_queue.get(True)
                print("**** ", message)
                self.event_queue.task_done()
            except:
                logger.error(traceback.format_exc())

    def create_buffer(self, buffer_id, url):
        if buffer_id not in self.browser_subprocess_dict:
            browser_subprocess = subprocess.Popen(
                ["python3", os.path.join(os.path.dirname(__file__), "browser.py"), str(buffer_id), str(url)],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)

            self.browser_subprocess_dict[buffer_id] = browser_subprocess

            browser_subprocess_communication_thread = threading.Thread(target=self.browser_subprocess_message_handler, args=(buffer_id,))
            self.browser_subprocess_communication_threads[buffer_id] = browser_subprocess_communication_thread
            browser_subprocess_communication_thread.start()

    def browser_subprocess_message_handler(self, buffer_id):
        while True:
            try:
                output = self.browser_subprocess_dict[buffer_id].stdout.readline().strip()
                message = parse_json_content(output)
                if "type" in message:
                    if message["type"] == "log":
                        print("[{}] {}: {}".format(message["buffer_id"], datetime.datetime.now().time(), message["content"]))
                    elif message["type"] == "message":
                        message_emacs(message["content"])
                    elif message["type"] == "eval_in_emacs":
                        content = message["content"]
                        method_name = content["method_name"]
                        args = content["args"]
                        eval_in_emacs(method_name, *args)
            except:
                print(traceback.format_exc())


    def send_message_to_subprocess(self, buffer_id, message):
        if buffer_id in self.browser_subprocess_dict:
            # Note: need add b'\n' at end, otherwise subprocess will block at sys.stdin.readline()
            self.browser_subprocess_dict[buffer_id].stdin.write(json.dumps(message).encode("utf-8") + b'\n')
            self.browser_subprocess_dict[buffer_id].stdin.flush()

    def kill_buffer(self, buffer_id):
        pass

    def update_views(self, args):
        ''' Update views.'''
        if args != self.views_data:
            self.views_data = args

            view_infos = args.split(",")

            # Do something if buffer's all view hide after update_views operation.
            old_view_buffer_ids = list(set(map(lambda v: v.buffer_id, self.view_dict.values())))
            new_view_buffer_ids = list(set(map(lambda v: v.split(":")[0], view_infos)))

            # Call all_views_hide interface when buffer's all views will hide.
            # We do something in app's buffer interface, such as videoplayer will pause video when all views hide.
            # Note, we must call this function before last view destroy,
            # such as QGraphicsVideoItem will report "Internal data stream error" error.
            for old_view_buffer_id in old_view_buffer_ids:
                if old_view_buffer_id not in new_view_buffer_ids:
                    if old_view_buffer_id in self.buffer_dict:
                        # self.buffer_dict[old_view_buffer_id].all_views_hide()
                        print("Hide all views of buffer id: {}".format(old_view_buffer_id))

            # Remove old key from view dict and destroy old view.
            for key in list(self.view_dict):
                if key not in view_infos:
                    self.destroy_view_later(key)

            # NOTE:
            # Create new view and REPARENT view to Emacs window.
            if view_infos != ['']:
                for view_info in view_infos:
                    if view_info not in self.view_dict:
                        (buffer_id, _, _, _, _, _) = view_info.split(":")
                        try:
                            # view = View(self.buffer_dict[buffer_id], view_info)
                            print("Create view {} of buffer {}".format(view_info, buffer_id))
                            # self.view_dict[view_info] = view
                        except KeyError:
                            # Hide all view, to switch *eaf* buffer.
                            for key in self.view_dict:
                                self.view_dict[key].hide()

                            # eval_in_emacs('eaf--rebuild-buffer', [])
                            print("Buffer id '{}' not exists".format(buffer_id))
                            return

            # Call some_view_show interface when buffer's view switch back.
            # Note, this must call after new view create, otherwise some buffer,
            # such as QGraphicsVideoItem will report "Internal data stream error" error.
            if view_infos != ['']:
                for new_view_buffer_id in new_view_buffer_ids:
                    if new_view_buffer_id not in old_view_buffer_ids:
                        if new_view_buffer_id in self.buffer_dict:
                            # self.buffer_dict[new_view_buffer_id].some_view_show()
                            print("Call some_view_show of buffer id: {}".format(new_view_buffer_id))

            # Adjust buffer size along with views change.
            # Note: just buffer that option `fit_to_view' is False need to adjust,
            # if buffer option fit_to_view is True, buffer render adjust by view.resizeEvent()
            for buffer in list(self.buffer_dict.values()):
                if not buffer.fit_to_view:
                    buffer_views = list(filter(lambda v: v.buffer_id == buffer.buffer_id, list(self.view_dict.values())))

                    # Adjust buffer size to max view's size.
                    if len(buffer_views) > 0:
                        max_view = max(buffer_views, key=lambda v: v.width * v.height)

                        # buffer.buffer_widget.width, buffer.buffer_widget.height = lambda: max_view.width, lambda: max_view.height
                        # buffer.buffer_widget.resize(max_view.width, max_view.height)
                        print("Resize buffer {} with {} and {}".format(buffer, max_view.width, max_view.height))
                    # Adjust buffer size to emacs window size if not match view found.
                    else:
                        # buffer.buffer_widget.width, buffer.buffer_widget.height = lambda: emacs_width, lambda: emacs_height
                        # buffer.buffer_widget.resize(emacs_width, emacs_height)
                        print("Resize buffer {} with {} and {}".format(buffer, emacs_width, emacs_height))

                    # Send resize signal to buffer.
                    # buffer.resize_view()
                    print("Call buffer {} resize_view interface".format(buffer))

            # NOTE:
            # When you do switch buffer or kill buffer in Emacs, will call Python function 'update_views.
            # Screen will flick if destroy old view BEFORE reparent new view.
            #
            # So we call function 'destroy_view_now' at last to make sure destroy old view AFTER reparent new view.
            # Then screen won't flick.
            self.destroy_view_now()

    def destroy_view_later(self, key):
        '''Just record view id in global list 'destroy_view_list', and not destroy old view immediately.'''
        global destroy_view_list

        destroy_view_list.append(key)

    def destroy_view_now(self):
        '''Destroy all old view immediately.'''
        global destroy_view_list

        for key in destroy_view_list:
            if key in self.view_dict:
                # self.view_dict[key].destroy_view()
                print("Call destroy_view of view {}".format(self.view_dict[key]))
            self.view_dict.pop(key, None)

        destroy_view_list = []

    def kill_emacs(self):
        pass

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

if __name__ == "__main__":
    proxy_string = ""
    emacs_width = emacs_height = 0
    destroy_view_list = []

    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("Trekker(sys.argv[1:])")
    else:
        Trekker(sys.argv[1:])
