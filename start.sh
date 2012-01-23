#!/bin/bash

nohup mex-server 2012 passwd +RTS -M100m &
nohup mex-client L L http://localhost:3000 http://localhost:2012/rpc +RTS -M100m &

