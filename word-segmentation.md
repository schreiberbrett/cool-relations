# Word Segmentation in miniKanren

I took Latin in high school, but that did not prepare me for reading papyrus or Roman inscriptions. Many examples of Latin "in the wild" do not put spaces in between their words.

Here's some miniKanren code that can attempt to segment a text into its individual words.

- `text` 
- `lexicon` A list of words, e.g. `'((A P P L E) (B A N A N A) (C H E R R Y))`
- `sentence` The text, segmented into a sentence.

```scheme
(defrel (segmento text lexicon words)
  (conde ((== text '()) (== words '()))
         ((fresh (first-word rest-words rest-text)
                 (== words `(,first-word . ,rest-words))
                 (pairo first-word) ; words should be nonempty
                 (membero first-word lexicon)
                 (appendo first-word rest-text text)
                 (segmento rest-text lexicon rest-words)))))
```

```
> (run* (q) (segmento '(S E N A T U S P O P U L U S Q U E R O M A N U S) '((P O P U L U S) (Q U E) (R O M A N U S) (S E N A T U S))
 q))
'(((S E N A T U S) (P O P U L U S) (Q U E) (R O M A N U S)))
```

"The Senate and the Roman people".

## Further work

Haven't tried this relation in different modes.

This code must find an exact match. A better approach would be to maximize the number of words found, so that in the case of misspellings or work cutoffs, the code can still recognize some words in the text, rather than failing outright.

`lexicon` could be represented as a trie, rather than a list. But then I'd want to restrict the alphabet to 0/1.
