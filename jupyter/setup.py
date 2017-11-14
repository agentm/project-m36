from distutils.core import setup

setup(
    name='itutd',
    version='0.1',
    packages=['itutd'],
    description='TutorialD Jupyter kernel for Project:M36',
    author='AgentM',
    author_email='agentm@themactionfaction.com',
    url='https://github.com/agentm/project-m36/jupyter/',
    install_requires=[
        'jupyter_client', 'IPython', 'ipykernel', 'websocket-client'
    ],
    classifiers=[
        'Intended Audience :: Developers',
        'License :: OSI Approved :: Public Domain',
        'Programming Language :: Python :: 2',
    ],
)
