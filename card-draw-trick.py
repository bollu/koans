#!/usr/bin/env python3
import random
random.seed(12357)
deck = list(range(13*4))
random.shuffle(deck)

suits = ["♠", "♡", "♣" , "♢"]
numbers = [str(s) for s in range(1, 11)] + ["J(5)", "Q(5)", "K(5)", "A(5)"] 
for card in deck:
    print("%s %s" % (suits[card//13], numbers[card % 13]))
    input()
