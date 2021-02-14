#!/usr/bin/env python3
import random
import time
random.seed(253)
deck = list(range(13*4))
random.shuffle(deck)

suits = ["♠", "♡", "♣" , "♢"]
numbers = [str(s) for s in range(1, 11)] + ["J(5)", "Q(5)", "K(5)", "A(5)"] 
for card in deck:
    print("%s %s" % (suits[card//13], numbers[card % 13]))
    time.sleep(3)
