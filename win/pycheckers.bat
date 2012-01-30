@echo off
pep8 --ignore=E221,E701,E202,W191 --repeat %1
pyflakes %1
true
