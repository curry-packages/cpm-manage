# cpm-manage - Tools to manage the main repository of the Curry Package Manager

This package contains tools to manage the central repository
of the Curry Package Manager CPM.
Currently, it provides support to test all packages of the
central repository and to add a new package to it.

## Installing the tool

After checking out the tool by the command

    > cpm checkout cpm-manage

go into the root directory of the package and run

    > cpm install

This installs the executable `cpm-manage` in the bin directory
of CPM.


## Using the tool

This tool provides two commands:

    > cpm-manage testall

This tests all packages of the central repository by installing
each package and testing it with `cpm test`.

    > cpm-manage add package.json

This package checks out the package specifed by the given JSON file,
tests is with `cpm test`, and, if everything is successful,
adds to the local copy of central repository.
Thus, one can push a new version of this central repository
in order to publish the package.
