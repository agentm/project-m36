from ipykernel.kernelbase import Kernel
import subprocess
from websocket import create_connection, WebSocketConnectionClosedException
import time
import select
import json
import socket
import errno
import urlparse

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

# todo: implement wrong database name handling
    def runtutd(self, tutd):
        #setup server
        dbname = 'jupyter'
        port = 63000
        if not self.ws and not self.dbserver:
            try:
                self.dbserver = subprocess.Popen(['project-m36-websocket-server',
                                                  '--database', dbname,
                                                  '--port', str(port)], 
                                                 )
            except OSError as err:
                if err.errno == errno.ENOENT:
                    return ('"project-m36-websocket-server" is not in the PATH environment variable and could not be started.', None)
                else:
                    raise
            res = self.connect_ws('ws://127.0.0.1:63000', dbname, retries=10)
            if res:
                return ('Connection attempt failed.', None)

        self.ws.send('executetutd/html+text:' + tutd)
        try:
            jsonresult = self.discard_prompts()
        except WebSocketConnectionClosedException:
            self.ws = None
            return ('Websocket server closed the connection.',None)
        res = json.loads(jsonresult)
        if 'displayerror' in res:
            return (res['displayerror']['text'], None)
        elif 'acknowledged' in res:
            return (None, {'html':'ok', 'text':'ok'})
        else:
            return (None, res['displayrelation'])

    def connect_ws(self, url, dbname, retries=0):
        while 1:
            try:
                ws = create_connection(url)
            except socket.error as err:
                if err.errno in (errno.ECONNREFUSED, ):
                    time.sleep(1.0)
                    if retries == 0:
                        return 'Failed to connect to websocket.'
                    retries -= 1
                else:
                    raise
            else:
                break
        self.ws = ws
        self.ws.send('connectdb:{}'.format(dbname))
        msg = json.loads(self.ws.recv())
        if 'displayerror' in msg:
            return msg['displayerror']['tag']
        return None

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
        

    def process_magic(self, code):
        #handle "magic" processing of commands which are not sent to the tutd interpreter
        cmds = code.split(' ')
        ok = {'text':'ok', 'html':'ok'}

        if cmds[0] == '%connect' and len(cmds) == 3:
            #connect to alternate database
            url = urlparse.urlparse(cmds[1])
            dbname = cmds[2]
            if not url.scheme.startswith('ws'):
                return ('Rejected attempt to connect to non-websocket. Use ws:// or wss:// scheme',None)
            res = self.connect_ws(url.geturl(), dbname)
            if res: #error from connection
                return (res, None, None)
            else: #connection successful
                return (None, ok, None)
        elif cmds[0] == '%help' and len(cmds) == 1:
            #display some help
            return (None, ok, self.show_help())
        elif cmds[0].startswith('%'):
            return ('Invalid magic command.', None)

    def do_execute(self, code, silent, 
                   store_history=True,
                   user_expressions=False,
                   allow_stdin=False):
        magic = self.process_magic(code)
        payload = None
        if magic:
            (err, result, payload) = magic
        else:
            (err, result) = self.runtutd(code)
        if not silent:
            if err:
                status = 'error'
                self.send_error(err)
            else:
                status = 'ok'
                display_content = {
                    'data':{'text/html':result['html'],
                            'text/plain':result['text']},
                    'metadata':{},
                    'transient':{}
                    }
                self.send_response(self.iopub_socket, 'display_data', display_content)
            ret ={'status': status,
                        'execution_count': self.execution_count,
                        'payload':[payload],
                        'user_expressions':{}}
            if payload:
                ret['payload'] = [payload]
            return ret

    def do_shutdown(self, restart):
        if self.dbserver:
            self.dbserver.terminate()
            self.dbserver.kill()

    def show_help(self):
        return {'source': 'page',
                'start':0,
                'data':{'text/html':'''<h1>Project:M36 TutorialD Jupyter Kernel Help</h1>
<p>TutorialD is an interactive language for the relational algebra.</p>

<p><a href="https://github.com/agentm/project-m36/blob/master/docs/15_minute_tutorial.markdown">15 Minute TutorialD Introduction</a></p>
<p><a href="https://github.com/agentm/project-m36/blob/master/docs/tutd_tutorial.markdown">TutorialD Documentation</a></p>
''',
                        'text/plain':
'''Project:M36 TutorialD Jupyter Kernel Help

TutorialD is an interactive language for the relational algebra.

15 Minute TutorialD Introduction: https://github.com/agentm/project-m36/blob/master/docs/15_minute_tutorial.markdown
TutorialD Documentation: https://github.com/agentm/project-m36/blob/master/docs/tutd_tutorial.markdown
'''}}

if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=ITutorialDKernel)
