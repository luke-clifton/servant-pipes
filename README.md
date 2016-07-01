servant-pipes
=============

This library allows you to stream data using the
[Pipes](https://hackage.haskell.org/package/pipes) library.

This library adds a new combinator to the servant ecosystem, `Stream`
which is usually used through one of it's aliases, such as `GetStream`.

When writing a service, this endpoint will require you to return a
`Producer a IO ()` from your handler. When generating a client, the
endpoint will give you a `Producer a (SafeT IO) ()`.

See the `yes-serve` subdirectory for a small example project.
