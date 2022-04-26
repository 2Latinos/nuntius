# nuntius [![Erlang CI][ci-img]][ci]

[ci]: https://github.com/2Latinos/nuntius
[ci-img]: https://github.com/2Latinos/nuntius/workflows/erlang/badge.svg

`nuntius` is an Erlang/OTP library to mock registered processes. Its main use case is to intercept
messages sent to specific processes and to allow the consumer to act upon them.

## Usage

`nuntius` is best used via [rebar3](https://rebar3.org/)'s `test` profile and using Erlang/OTP's
[Common Test framework](https://www.erlang.org/doc/man/common_test.html):

1\. change your `rebar.config` to include:

```erlang
{profiles, [{test, [{deps, [nuntius]}]}]}.
```

2\. run your `nuntius`-enabled tests with:

```shell
rebar3 ct
```

## Features

* places mock processes in front of previously registered processes; these mock processes will
intercept (and optionally handle) every message that was supposed to go to the latter ones, then
  * allows mock processes to decide on letting the messages pass through, or not,
  * allows mock processes to run one or many pre-processing functions on each received message,
  * allows mock processes to discard intercepted messages entirely,
  * allows history collection of messages received by the mock processes for further analysis.

## Options for the mock process

The following parameters allow you to configure the interaction between the mock and mocked
processes, as well as other elements for debugging:

* `passthrough`: when `true` (default: `true`) all messages received by the mock process are
passed through to the mocked process,
* `history`: when `true` (default: `true`) all messages received by the mock process are
classified as per [Understanding the message history](#understanding-the-message-history).

## Understanding the message history

History elements are classified with 4 keys:

* `timestamp`: an integer representing Erlang system time in native time unit,
* `message`: the message that was received and/or potentially handled by expectations
(or passed through),
* `mocked`: an indication of whether or not any of the expecations you declared handled
the message,
* `passed_through`: an indication of whether or not the received message was passed through to
the mocked process.

## Caveats

`nuntius` tries to execute your expectations by simply calling their declarations inside a
`try-catch` expression. Because of this, non-matching expectations will return a `function_clause`,
that is caught.
Since it's not possible (at this moment) to distinguish a `function_clause` provoked by `nuntius`'
internal code or your own, we propose you to make sure your functions don't fail with a 
`function_clause`.
You can also check the message history to understand if a given message was mocked and/or
passed through.

## Documentation

Documentation is generated with:

```shell
rebar3 edoc
```

after which you can use your favorite Web browser to open `doc/index.html`.

It is also available, online, at [hexdocs.pm/nuntius](https://hexdocs.pm/nuntius/).

## Examples

Examples are found at [examples](examples).

## Versioning

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Readme

We get inspiration for our README's format/content from
[Make a README](https://www.makeareadme.com/).

## Changelog

All notable changes to this project will be referenced from the [CHANGELOG](CHANGELOG.md).

## Contributing

Though this project is maintained by [2Latinos](https://github.com/2Latinos) contributions are
accepted and welcome. Check [CONTRIBUTING.md](CONTRIBUTING.md) for more.

## License

Check [LICENSE](LICENSE).
