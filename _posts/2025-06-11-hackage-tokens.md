---
title: You should be using Hackage tokens
---

If you're still using `cabal upload --username USERNAME --password PASSWORD` to
publish your Haskell packages, it's time for an upgrade. Hackage tokens provide
a more secure and flexible alternative that every Haskell developer should
adopt.

Instead of using your password directly, use:

```sh
cabal upload --token TOKEN
```

<aside>

Update 2025-06-12:
If you use Stack, set [the `HACKAGE_TOKEN` environment variable][1] when running `stack upload`.

[1]: https://docs.haskellstack.org/en/v3.5.1/commands/upload_command/#the-hackage_key-environment-variable

</aside>

This simple change brings several important benefits:

1.  **Enhanced Security**: You no longer need to store your Hackage password in
    plain text, reducing the risk of credential exposure.

2.  **Better CI/CD Integration**: For automated deployments, tokens are the
    safer choice. They can be stored as environment variables or secrets in
    your CI system.

3.  **Fine-grained Control**: Tokens can be generated and revoked at will. If a
    token is compromised, you can simply invalidate it without changing your
    main password.

Hackage tokens have been available for a while now:

-   API tokens were implemented in the Hackage server in 2018 by
    [PR #534](https://github.com/haskell/hackage-server/pull/534).

-   The `--token` option was added to Cabal's upload command in 2023 by
    [PR #9058](https://github.com/haskell/cabal/pull/9058).

To create your own tokens:

1.  Log in to your Hackage account

2.  Visit your user management page at: `hackage.haskell.org/user/USERNAME/manage`

3.  Scroll down to "Register new token"

4.  Generate a new token

Hackage tokens represent a small change in your workflow that brings
significant security benefits. If you're serious about Haskell development,
especially in a team or production context, making the switch from
password-based to token-based authentication should be a priority.

Take a few minutes today to set up your tokens and improve your package
publishing security posture.
