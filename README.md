# Human Language Model

Test your language model!

[![](screenshot.png)](https://torvaney.github.io/projects/human-language-model.html)

[Try it out](https://torvaney.github.io/projects/human-language-model.html)

## What is it?

Test the accuracy of your "language model" on a given corpus (sort of).

### What is a "language model"?

> A statistical language model is a probability distribution over sequences of words. Given such a sequence, say of length m, it assigns a probability to the whole sequence.

(from [Wikipedia](https://en.wikipedia.org/wiki/Language_model))

## How does it work

The programme takes a plain text file, and splits it into non-overlapping chunks.
You then have to guess which character came next, after the chunk shown. Chunks are
show in random order.

## How to build

The app is written in Elm, and can be compiled to Html (and javascript) like so:

<!-- TODO: Automate with Make or something -->
1. `elm make src/elm/Main.elm`
1. `open index.html`
