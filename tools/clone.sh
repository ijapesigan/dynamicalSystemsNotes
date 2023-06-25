#!/bin/bash

git clone git@github.com:ijapesigan/dynamicalSystemsNotes.git
rm -rf "$PWD.git"
mv dynamicalSystemsNotes/.git "$PWD"
rm -rf dynamicalSystemsNotes
