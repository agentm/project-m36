# TutorialD Jupyter Kernel for Project:M36

## Introduction

[The Jupyter Notebook](https://jupyter-notebook.readthedocs.io/en/stable/notebook.html) is web-based tool for experimenting with various languages. Project:M36 offers a Jupyter kernel for the TutorialD interpreter.

## Installation

To install the `tutd` kernel:

* install the `itutd` module into your jupyter virtual environment:
```
$ cd project-m36/jupyter/itutd
$ python setup.py install    
```

* install `kernel.json`:

   ```$ mkdir ~/Library/Jupyter/kernels/itutd && cp project-m36/jupyter/kernel.json ~/Library/Jupyter/kernels/itutd``` (macOS)

   ```$ mkdir ~/.local/share/jupyter/kernels/tutd && cp project-m36/jupyter/kernel.json ~/.local/share/jupyter/kernels/tutd``` (Linux)

   ```$ mkdir %APPDATA%\jupyter\kernels\tutd && copy project-m36\jupyter\kernel.json %APPDATA%\jupyter\kernels\tutd``` (Windows)

* check that the kernel is detected:
```
$ jupyter kernelspec list
Available kernels:
  python2    ...
  tutd       ...
```

## Usage

With the kernel installed, run `jupyter notebook` for the web-based interface and click "New" at the top right of the page. The TutorialD kernel should be in the list.

Once a new TutorialD notebook is opened, the interpreter is identical to the `tutd` command line interpreter and accepts the same commands. If you are new to Project:M36-flavored TutorialD, please [read the tutorial](/docs/tutd_tutorial.markdown).
