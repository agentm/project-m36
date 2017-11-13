from ipykernel.kernelbase import Kernel
import subprocess
from websocket import create_connection
import time
import select
import json

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
        dbname = 'spam'
        '''
        if not self.dbserver:
            self.dbserver = subprocess.Popen(['project-m36-websocket-server',
                                              '--database', 'itutd',
                                              '--port', str(port)], 
                                         )
                                         '''
        if not self.ws:
            attempts = 0
            while 1:
                ws = create_connection('ws://127.0.0.1:{}'.format(port))
                if ws:
                    break
                time.sleep(0.3)
                attempt += 1
                if attempt > 10:
                    assert False, "timeout"
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

    def do_execute(self, code, silent, 
                   store_history=True,
                   user_expressions=False,
                   allow_stdin=False):
        (err, relresult) = self.runtutd(code)
        if not silent:
            if err:
                status = 'error'
                stream_content = {'ename':'TutorialDError',
                                  'traceback':[err['displayerror']['html']],
                                  'ename':'TutorialDError',
                                  'evalue':''}
                self.send_response(self.iopub_socket, 'error', stream_content)
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

if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=ITutorialDKernel)
