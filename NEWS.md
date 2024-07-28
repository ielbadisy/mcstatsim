# mcstatsim 0.5.0

* Added a `NEWS.md` file to track changes to the package.

* Introduced parallel computing support via the `future` package implemented in the `pbapply` package.

* The unique dependency is now the `pbapply` package.

* This update addresses the limitation of the previous version, which only supported multicore parallel computing on Unix-based operating systems. The new version supports parallel computing across different operating systems, including Windows.

* Improved documentation and examples to reflect the new parallel computing capabilities.

