#!/bin/bash

nohup mex-server 2012 passwd.real +RTS -M1G &

# demo client name and password must be last line of password file
nohup mex-client $(tail -1 passwd.real) http://localhost:3000 http://localhost:2012/rpc +RTS -M100m &

