cpm-manage - Tools to manage the main repository of the Curry Package Manager
=============================================================================

This package contains tools to manage the central repository
of the Curry Package Manager CPM.
For instance, it provides support to test all packages of the
central repository or to generate web pages for all packages.

## Installing the tool

After checking out the tool by the command

    > cypm checkout cpm-manage

go into the root directory of the package and run

    > cypm install

Alternatively, one can install the latest version directly by

    > cypm install cpm-manage

This installs the executable `cpm-manage` in the bin directory
of CPM.


## Using the tool

This tool provides various commands. For instance,

    > cpm-manage testall

tests all packages of the central repository by installing
each package and testing it with `cypm test`.

The command

    > cpm-manage genhtml

generates HTML pages for all packages of the central repository.

The command

    > cpm-manage showgraph PKG

visualizes the dependencies of package `PGK` as a dot graph.
Thus, it shows packages that directly and indirectly uses `PGK`
and it shows all directly and indirectly packages used by `PKG`.
