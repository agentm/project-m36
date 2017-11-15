from ipykernel.kernelbase import Kernel
import subprocess
from websocket import create_connection
import time
import select
import json
import socket
import errno

class ITutorialDKernel(Kernel):
    implementation = 'ITutorialD'
    implementation_version = '0.1'

    banner = 'TutorialD kernel using Project:M36 https://github.com/agentm/project-m36'

    language = 'TutorialD'
    language_version = '0.3'
    language_info = {
        'name': 'TutorialD',
        'mimetype': 'text/x-tutd',
        'file_extension': '.tutd',
        }
    dbserver = None
    ws = None

    def runtutd(self, tutd):
        #setup server
        port = 63000
        dbname = 'jupyter'
        if not self.dbserver:
            try:
                self.dbserver = subprocess.Popen(['project-m36-websocket-server',
                                                  '--database', dbname,
                                                  '--port', str(port)], 
                                                 )
            except OSError as err:
                if err.errno == errno.ENOENT:
                    return ({'displayerror':{'html':'"project-m36-websocket-server" is not in the PATH environment variable and could not be started.'}}, None)
                else:
                    raise
        if not self.ws:
            attempts = 0
            while 1:
                try:
                    ws = create_connection('ws://127.0.0.1:{}'.format(port))
                except socket.error as err:
                    if err.errno in (errno.ECONNREFUSED, ):
                        time.sleep(0.3)
                        attempts += 1
                        if attempts > 10:
                            assert False, "websocket timeout"
                    else:
                        raise
                else:
                    break
            self.ws = ws
            self.ws.send('connectdb:{}'.format(dbname))
            #receive and discard prompt info
            self.ws.recv()
            self.ws.recv()
        self.ws.send('executetutd/html+text:' + tutd)
        jsonresult = self.discard_prompts()
        res = json.loads(jsonresult)
        if 'displayerror' in jsonresult:
            return (res, None)
        elif 'acknowledged' in jsonresult:
            return (None, {'html':'ok', 'text':'ok'})
        else:
            return (None, res['displayrelation'])

    def discard_prompts(self):
        #read from the websocket until a non-prompt message is received
        while 1:
            sel = select.select([self.ws.sock],[],[],0)
            if len(sel[0]) > 0:
                #there is something to read
                msg = self.ws.recv()
                if 'promptInfo' not in msg:
                    return msg

    def send_error(self, msg):
        stream_content = {'ename':'TutorialDError',
                          'traceback':[msg],
                          'evalue':''}
        self.send_response(self.iopub_socket, 'error', stream_content)
        

    def do_execute(self, code, silent, 
                   store_history=True,
                   user_expressions=False,
                   allow_stdin=False):
        (err, relresult) = self.runtutd(code)
        if not silent:
            if err:
                status = 'error'
                self.send_error(err['displayerror']['html'])
            else:
                status = 'ok'
                display_content = {
                    'data':{'text/html':relresult['html'],
                            'text/plain':relresult['text']},
                    'metadata':{},
                    'transient':{}
                    }
                self.send_response(self.iopub_socket, 'display_data', display_content)
            return {'status': status,
                        'execution_count': self.execution_count,
                        'payload':[],
                        'user_expressions':{}}

    def do_shutdown(self, restart):
        if self.dbserver:
            self.dbserver.terminate()
            self.dbserver.kill()

if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=ITutorialDKernel)
