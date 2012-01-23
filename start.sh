#!/bin/bash

nohup mex-server 2012 passwd.real +RTS -M100m &
nohup mex-client $(tail -1 passwd.real) http://localhost:3000 http://localhost:2012/rpc +RTS -M100m &
