`EXTERNAL-PROGRAM` is an attempt to make the `RUN-PROGRAM` functionality in implementations like SBCL and CCL as portable as possible without sacrificing much in the way of power.

**Note**: This library is available via [Quicklisp](https://quicklisp.org/).

Here are some of the differences:

* splits `START` (async) and `RUN` (sync) into two separate functions, rather than using a single function with a `WAIT` parameter that changes the function's specification;
* offers a `REPLACE-ENVIRONMENT-P` parameter that indicates whether provided env vars should build on or replace the current environment.

Not all functionality is available on all platforms. `EXTERNAL-PROGRAM` provides warnings and errors when these limitations are encountered. But I'll try my best to work around them.
