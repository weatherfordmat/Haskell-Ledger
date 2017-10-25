# Haskell-Ledger

## Overview 

It allows for record keeping, budgeting, and financial planning.

<img width="1130" alt="demo" src="https://user-images.githubusercontent.com/13956201/31250168-a2b50440-a9df-11e7-9e41-2bbd7a1fac2a.png">

## Why Haskell

Haskell forces you to follow a functional paradigm and to think entirely differently than you would in an object-orientated language.

This is one of my passion projects. It's purpose is primarily learning, experimenting with functional programming, but I would also like to test the limits of 
Haskell's strongly typed system, its structure, tooling, etc.

## How To Run

The project uses cabal/stack for dependency management. To run the project:
``` haskell
stack setup
stack build
stack exec transaction-exe
```

## Task List

- [ ] Encryption/Description of Transaction File
- [ ] Web Client for viewing data
- [ ] Currency Exchange, Stock Information, Etc.
- [ ] Exchange plaintext for optional AcidState DB or NoSQL DB
- [ ] Allow importing from api, .csv, .qbo, etc.

## Contributors

@weatherfordmat

Contributions are welcome. Feature discussion is also encouraged.