# nuntius [![Erlang CI][ci-img]][ci]

[ci]: https://github.com/2Latinos/nuntius
[ci-img]: https://github.com/2Latinos/nuntius/workflows/erlang/badge.svg

`nuntius` is an Erlang/OTP library to mock registered processes. Its main use case is to intercept
messages sent to specific processes and to allow the consumer to act upon them.

## Usage

`nuntius` is best used via [rebar3](https://rebar3.org/)'s `test` profile and using Erlang/OTP's
[Common Test framework](https://www.erlang.org/doc/man/common_test.html):

1. change your `rebar.config` to include:

```erlang
{profiles, [{test, [{deps, [nuntius]}]}]}.
```

1. run your `nuntius`-enabled tests with:

```shell
rebar3 ct
```

## Features

* places mock processes in front of previously registered processes; these mock processes will
intercept (and handle) every message that was supposed to go to the latter ones, then
  * allows mock processes to decide on letting the messages pass through, or not,
  * allows mock processes to run one or many pre-processing functions on each received message,
  * allows mock processes to discard intercepted messages entirely,
  * allows history collection of messages received by the mock processes for further analysis.

## Documentation

Documentation is generated with:

```shell
rebar3 edoc
```

## Examples

Examples are found at [examples](examples).

## Versioning

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Readme

We get inspiration for our README's format/content from
[Make a README](https://www.makeareadme.com/).

## Changelog

All notable changes to this project will be referenced from [this file](CHANGELOG.md).

## Contributing

Though this project is maintained by [2Latinos][https://github.com/2Latinos] contributions are
accepted and welcome. Check [CONTRIBUTING.md](CONTRIBUTING.md) for more.

It is also available online at [hexdocs.pm/nuntius](https://hexdocs.pm/nuntius/).

## License

Check [LICENSE](LICENSE).
