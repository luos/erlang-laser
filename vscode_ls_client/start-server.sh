#!/bin/bash
echo $HOME
echo "$@" > $HOME/vscode-test.txt
echo `pwd` > $HOME/vscode-test.txt

sleep 500