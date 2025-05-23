---
title: You Should Be Using Hackage Tokens
author: Taylor Fausak
tags: [haskell, hackage, security]
---

## Why Hackage Tokens Matter

If you're still using `cabal upload --username foo --password bar` to publish your Haskell packages, it's time for an upgrade. Hackage tokens provide a more secure and flexible alternative that every Haskell developer should adopt.

## The Better Way: Token-Based Authentication

Instead of using your password directly, use:

```haskell
cabal upload --token qux
```

This simple change brings several important benefits:

1. **Enhanced Security**: You no longer need to store your Hackage password in plaintext, reducing the risk of credential exposure.

2. **Better CI/CD Integration**: For automated deployments, tokens are the safer choice. They can be stored as environment variables or secrets in your CI system.

3. **Fine-grained Control**: Tokens can be generated and revoked at will. If a token is compromised, you can simply invalidate it without changing your main password.

4. **Reduced Attack Surface**: Using dedicated tokens for different purposes or projects limits the potential damage if any single token is compromised.

## How to Get Started

Hackage tokens have been available for a while now:

- API tokens were implemented in the Hackage server in 2018 ([PR #534](https://github.com/haskell/hackage-server/pull/534))
- The `--token` option was added to Cabal's upload command in 2023 ([PR #9058](https://github.com/haskell/cabal/pull/9058))

To create your own tokens:

1. Log in to your Hackage account
2. Visit your user management page at: [https://hackage.haskell.org/user/YOUR_USERNAME/manage](https://hackage.haskell.org/user/YOUR_USERNAME/manage) (replace YOUR_USERNAME with your actual username)
3. Look for the token management section
4. Create a new token with an appropriate description (e.g., "CI uploads" or "Personal laptop")

## Best Practices

When using Hackage tokens, consider these recommendations:

- Create different tokens for different environments or purposes
- Store tokens securely using environment variables or a secrets manager
- Regularly rotate tokens, especially for sensitive environments
- Include token revocation in your security incident response plan

## Conclusion

Hackage tokens represent a small change in your workflow that brings significant security benefits. If you're serious about Haskell development, especially in a team or production context, making the switch from password-based to token-based authentication should be a priority.

Take a few minutes today to set up your tokens and improve your package publishing security posture.
